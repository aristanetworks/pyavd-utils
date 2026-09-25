// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Depth-first traversal of effective schemas and their source occurrences.
//!
//! Compilation answers what an effective schema node contains. Generation also
//! needs to know where that node was used, which reference introduced it, and
//! whether a consumer wants to inspect its descendants. This module combines
//! those two perspectives while walking the schema. It deliberately does not
//! retain a second graph: occurrence state exists only for the duration of a
//! visitor call, and children are resolved only when the visitor requests them.

#![allow(
    clippy::as_conversions,
    clippy::indexing_slicing,
    reason = "SchemaId table indices are produced and validated by CompiledStore"
)]

use indexmap::IndexMap;

use crate::CompileError;
use crate::SchemaDiagnostic;
use crate::StoreSource;
use crate::any::SourceSchema;
use crate::compiled::Common;
use crate::compiled::CompiledStore;
use crate::compiled::DictSchema;
use crate::compiled::ListSchema;
use crate::compiled::SchemaId;
use crate::resolve::resolve_ref::resolve_ref;

/// Identifies how an occurrence is connected to its parent schema.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum SchemaRelation<'a> {
    /// The selected named schema.
    Root,
    /// A statically named dictionary key.
    Key(&'a str),
    /// A dictionary key selected through a dynamic-key path.
    DynamicKey(&'a str),
    /// The item schema of a list.
    Items,
}

/// Controls whether traversal continues below the current occurrence.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum TraversalControl {
    /// Visit the occurrence's statically known children in schema order.
    Descend,
    /// Return to the parent without resolving or visiting any children.
    SkipChildren,
}

/// Effective schema data and source context for one visited occurrence.
///
/// The effective data comes from the deduplicated compiled store. The path and
/// leading reference layers describe this particular use-site and therefore
/// cannot be stored on the compiled node itself.
#[derive(Debug)]
pub(crate) struct SchemaOccurrence<'a> {
    compiled: &'a CompiledStore,
    path: &'a [String],
    relation: SchemaRelation<'a>,
    schema_id: SchemaId,
    pure_references: Vec<&'a str>,
}

impl<'a> SchemaOccurrence<'a> {
    /// Absolute source-schema path of this use-site.
    pub(crate) fn path(&self) -> &[String] {
        self.path
    }

    /// Relationship between this occurrence and its parent.
    pub(crate) fn relation(&self) -> SchemaRelation<'a> {
        self.relation
    }

    /// Identity of the effective node in the compiled schema tables.
    pub(crate) fn schema_id(&self) -> SchemaId {
        self.schema_id
    }

    /// Leading reference-only layers contributing to this occurrence.
    ///
    /// Consumers decide whether a reference has an existing representation or
    /// whether traversal should continue into its effective children.
    pub(crate) fn pure_references(&self) -> &[&str] {
        &self.pure_references
    }

    /// Properties shared by every effective schema type.
    pub(crate) fn common(&self) -> &Common {
        common(self.compiled, self.schema_id)
    }

    /// Effective list schema when this occurrence is a list.
    pub(crate) fn list(&self) -> Option<&ListSchema> {
        let SchemaId::List(index) = self.schema_id else {
            return None;
        };
        Some(&self.compiled.lists[index as usize])
    }

    /// Effective dictionary schema when this occurrence is a dictionary.
    pub(crate) fn dict(&self) -> Option<&DictSchema> {
        let SchemaId::Dict(index) = self.schema_id else {
            return None;
        };
        Some(&self.compiled.dicts[index as usize])
    }
}

/// Consumer of a depth-first schema traversal.
///
/// `enter` is called before children and decides whether they are needed.
/// `leave` is called after a successful `enter` when the requested descendant
/// traversal also succeeds. It is still called when children are skipped. This
/// pairing lets generators maintain a stack of purpose-specific output plans
/// without retaining a generic occurrence graph.
pub(crate) trait SchemaVisitor {
    /// Error returned by consumer-specific validation or planning.
    type Error: From<CompileError>;

    /// Inspect an occurrence and decide whether its children should be visited.
    fn enter(&mut self, occurrence: &SchemaOccurrence<'_>)
    -> Result<TraversalControl, Self::Error>;

    /// Finish processing an occurrence after any requested children.
    fn leave(&mut self, occurrence: &SchemaOccurrence<'_>) -> Result<(), Self::Error>;
}

/// Compiled schema paired with a lazy, occurrence-aware traversal strategy.
///
/// Constructing a traverser performs complete schema compilation, including
/// reference and type validation. Walking is separate: it recreates only the
/// source-layer context needed for visited occurrences and never stores that
/// generation-only provenance in the runtime archive.
#[derive(Debug)]
pub(crate) struct SchemaTraverser<'a> {
    source: &'a StoreSource,
    compiled: CompiledStore,
    schema_name: &'a str,
    root_source: &'a SourceSchema,
    root_id: SchemaId,
}

impl<'a> SchemaTraverser<'a> {
    /// Compile the selected schema and prepare it for one or more traversals.
    pub(crate) fn compile(
        source: &'a StoreSource,
        schema_name: &'a str,
    ) -> Result<Self, CompileError> {
        let compiled = CompiledStore::compile_schema(source, schema_name)?;
        let root_source = source
            .get(schema_name)
            .map_err(|error| SchemaDiagnostic::Reference {
                schema_path: vec![schema_name.to_owned()],
                reference: format!("{schema_name}#"),
                error: error.into(),
            })?;
        let root_id =
            *compiled
                .roots
                .get(schema_name)
                .ok_or_else(|| SchemaDiagnostic::StructuralCycle {
                    schema_path: vec![schema_name.to_owned()],
                })?;
        Ok(Self {
            source,
            compiled,
            schema_name,
            root_source,
            root_id,
        })
    }

    /// Effective compiled tables used by the selected schema.
    pub(crate) fn compiled(&self) -> &CompiledStore {
        &self.compiled
    }

    /// Visit the root and all descendants requested by `visitor`.
    pub(crate) fn traverse<V: SchemaVisitor>(&self, visitor: &mut V) -> Result<(), V::Error> {
        self.visit(
            &[self.root_source],
            self.root_id,
            vec![self.schema_name.to_owned()],
            SchemaRelation::Root,
            visitor,
        )
    }

    fn visit<V: SchemaVisitor>(
        &self,
        declared_layers: &[&'a SourceSchema],
        schema_id: SchemaId,
        path: Vec<String>,
        relation: SchemaRelation<'_>,
        visitor: &mut V,
    ) -> Result<(), V::Error> {
        let layers = expand_layers(self.source, declared_layers, &path)?;
        let pure_references = layers
            .iter()
            .copied()
            .take_while(|schema| is_pure_reference(schema))
            .filter_map(schema_ref)
            .collect();
        let occurrence = SchemaOccurrence {
            compiled: &self.compiled,
            path: &path,
            relation,
            schema_id,
            pure_references,
        };

        if visitor.enter(&occurrence)? == TraversalControl::Descend {
            self.visit_children(&layers, schema_id, &path, visitor)?;
        }
        visitor.leave(&occurrence)
    }

    fn visit_children<V: SchemaVisitor>(
        &self,
        layers: &[&'a SourceSchema],
        schema_id: SchemaId,
        path: &[String],
        visitor: &mut V,
    ) -> Result<(), V::Error> {
        match schema_id {
            SchemaId::Bool(_) | SchemaId::Int(_) | SchemaId::Str(_) => Ok(()),
            SchemaId::List(index) => {
                let list = &self.compiled.lists[index as usize];
                let item_layers = layers
                    .iter()
                    .filter_map(|schema| match schema {
                        SourceSchema::List(schema) => schema.items.as_deref(),
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                if let Some(item_id) = list.items
                    && !item_layers.is_empty()
                {
                    self.visit(
                        &item_layers,
                        item_id,
                        path_with(path, "items"),
                        SchemaRelation::Items,
                        visitor,
                    )?;
                }
                Ok(())
            }
            SchemaId::Dict(index) => {
                let dict = &self.compiled.dicts[index as usize];
                let key_layers = dict_child_layers(layers, false);
                for (name, child_id) in &dict.keys {
                    let declared = key_layers.get(name.as_str()).ok_or_else(|| {
                        CompileError::from(SchemaDiagnostic::StructuralCycle {
                            schema_path: path_with(path, name),
                        })
                    })?;
                    self.visit(
                        declared,
                        *child_id,
                        path_with(&path_with(path, "keys"), name),
                        SchemaRelation::Key(name),
                        visitor,
                    )?;
                }
                let dynamic_key_layers = dict_child_layers(layers, true);
                for (name, child_id) in &dict.dynamic_keys {
                    let declared = dynamic_key_layers.get(name.as_str()).ok_or_else(|| {
                        CompileError::from(SchemaDiagnostic::StructuralCycle {
                            schema_path: path_with(path, name),
                        })
                    })?;
                    self.visit(
                        declared,
                        *child_id,
                        path_with(&path_with(path, "dynamic_keys"), name),
                        SchemaRelation::DynamicKey(name),
                        visitor,
                    )?;
                }
                Ok(())
            }
        }
    }
}

fn dict_child_layers<'a>(
    layers: &[&'a SourceSchema],
    dynamic: bool,
) -> IndexMap<&'a str, Vec<&'a SourceSchema>> {
    let mut children = IndexMap::new();
    for schema in layers {
        let SourceSchema::Dict(schema) = schema else {
            continue;
        };
        let map = if dynamic {
            schema.dynamic_keys.as_ref()
        } else {
            schema.keys.as_ref()
        };
        if let Some(map) = map {
            for (name, child) in map {
                children
                    .entry(name.as_str())
                    .or_insert_with(Vec::new)
                    .push(child);
            }
        }
    }
    children
}

fn expand_layers<'a>(
    store: &'a StoreSource,
    declared_layers: &[&'a SourceSchema],
    path: &[String],
) -> Result<Vec<&'a SourceSchema>, CompileError> {
    let Some(first) = declared_layers.first().copied() else {
        return Err(SchemaDiagnostic::StructuralCycle {
            schema_path: path.to_vec(),
        }
        .into());
    };
    let mut result = Vec::new();
    for declared in declared_layers {
        let mut layer = *declared;
        let mut chain = Vec::new();
        loop {
            if schema_type(first) != schema_type(layer) {
                return Err(SchemaDiagnostic::TypeMismatch {
                    expected: schema_type(first),
                    found: schema_type(layer),
                }
                .into());
            }
            chain.push(std::ptr::from_ref(layer));
            result.push(layer);
            let Some(reference) = schema_ref(layer) else {
                break;
            };
            layer = resolve_ref(reference, store).map_err(|error| SchemaDiagnostic::Reference {
                schema_path: path.to_vec(),
                reference: reference.to_owned(),
                error,
            })?;
            if chain.contains(&std::ptr::from_ref(layer)) {
                return Err(SchemaDiagnostic::ReferenceCycle {
                    schema_path: path.to_vec(),
                    reference: reference.to_owned(),
                }
                .into());
            }
        }
    }
    Ok(result)
}

fn is_pure_reference(schema: &SourceSchema) -> bool {
    match schema {
        SourceSchema::Bool(schema) => base_is_pure(&schema.base),
        SourceSchema::Int(schema) => {
            base_is_pure(&schema.base)
                && schema.min.is_none()
                && schema.max.is_none()
                && schema.valid_values.valid_values.is_none()
                && schema.valid_values.dynamic_valid_values.is_none()
                && schema.convert_types.convert_types.is_none()
        }
        SourceSchema::Str(schema) => {
            base_is_pure(&schema.base)
                && schema.convert_to_lower_case.is_none()
                && schema.format.is_none()
                && schema.min_length.is_none()
                && schema.max_length.is_none()
                && schema.pattern.is_none()
                && schema.valid_values.valid_values.is_none()
                && schema.valid_values.dynamic_valid_values.is_none()
                && schema.convert_types.convert_types.is_none()
        }
        SourceSchema::List(schema) => {
            base_is_pure(&schema.base)
                && schema.items.is_none()
                && schema.min_length.is_none()
                && schema.max_length.is_none()
                && schema.primary_key.is_none()
                && schema.unique_keys.is_none()
                && schema.allow_duplicate_primary_key.is_none()
        }
        SourceSchema::Dict(schema) => {
            base_is_pure(&schema.base)
                && schema.keys.is_none()
                && schema.dynamic_keys.is_none()
                && schema.allow_other_keys.is_none()
                && schema.schema_defs.is_none()
                && schema.schema_id.is_none()
                && schema.schema_schema.is_none()
        }
    }
}

fn base_is_pure<T: crate::base::DataValue>(base: &crate::base::Base<T>) -> bool {
    base.default.is_none()
        && base.display_name.is_none()
        && base.required.is_none()
        && base.schema_ref.is_some()
}

fn schema_ref(schema: &SourceSchema) -> Option<&str> {
    match schema {
        SourceSchema::Bool(schema) => schema.base.schema_ref.as_deref(),
        SourceSchema::Int(schema) => schema.base.schema_ref.as_deref(),
        SourceSchema::Str(schema) => schema.base.schema_ref.as_deref(),
        SourceSchema::List(schema) => schema.base.schema_ref.as_deref(),
        SourceSchema::Dict(schema) => schema.base.schema_ref.as_deref(),
    }
}

fn schema_type(schema: &SourceSchema) -> &'static str {
    match schema {
        SourceSchema::Bool(_) => "bool",
        SourceSchema::Int(_) => "int",
        SourceSchema::Str(_) => "str",
        SourceSchema::List(_) => "list",
        SourceSchema::Dict(_) => "dict",
    }
}

fn common(store: &CompiledStore, schema_id: SchemaId) -> &Common {
    match schema_id {
        SchemaId::Bool(index) => &store.bools[index as usize].common,
        SchemaId::Int(index) => &store.ints[index as usize].common,
        SchemaId::Str(index) => &store.strings[index as usize].common,
        SchemaId::List(index) => &store.lists[index as usize].common,
        SchemaId::Dict(index) => &store.dicts[index as usize].common,
    }
}

fn path_with(path: &[String], element: &str) -> Vec<String> {
    let mut result = path.to_vec();
    result.push(element.to_owned());
    result
}

#[cfg(test)]
mod tests {
    use super::SchemaOccurrence;
    use super::SchemaRelation;
    use super::SchemaTraverser;
    use super::SchemaVisitor;
    use super::TraversalControl;
    use crate::Load as _;
    use crate::StoreSource;

    #[derive(Default)]
    struct RecordingVisitor {
        visits: Vec<(Vec<String>, Vec<String>)>,
    }

    impl SchemaVisitor for RecordingVisitor {
        type Error = crate::CompileError;

        fn enter(
            &mut self,
            occurrence: &SchemaOccurrence<'_>,
        ) -> Result<TraversalControl, Self::Error> {
            self.visits.push((
                occurrence.path().to_vec(),
                occurrence
                    .pure_references()
                    .iter()
                    .map(ToString::to_string)
                    .collect(),
            ));
            if occurrence.relation() == SchemaRelation::Key("first") {
                Ok(TraversalControl::SkipChildren)
            } else {
                Ok(TraversalControl::Descend)
            }
        }

        fn leave(&mut self, _occurrence: &SchemaOccurrence<'_>) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    #[test]
    fn traversal_preserves_occurrence_context_and_honors_skips() {
        let store = StoreSource::from_json(
            r#"{
                "shared": {
                    "type": "dict",
                    "keys": {"value": {"type": "str"}}
                },
                "model": {
                    "type": "dict",
                    "keys": {
                        "first": {"type": "dict", "$ref": "shared#"},
                        "second": {"type": "dict", "$ref": "shared#"}
                    }
                }
            }"#,
        )
        .unwrap();
        let traverser = SchemaTraverser::compile(&store, "model").unwrap();
        let mut visitor = RecordingVisitor::default();

        traverser.traverse(&mut visitor).unwrap();

        assert_eq!(
            visitor.visits,
            vec![
                (vec!["model".to_owned()], Vec::new()),
                (
                    vec!["model".to_owned(), "keys".to_owned(), "first".to_owned()],
                    vec!["shared#".to_owned()],
                ),
                (
                    vec!["model".to_owned(), "keys".to_owned(), "second".to_owned()],
                    vec!["shared#".to_owned()],
                ),
                (
                    vec![
                        "model".to_owned(),
                        "keys".to_owned(),
                        "second".to_owned(),
                        "keys".to_owned(),
                        "value".to_owned(),
                    ],
                    Vec::new(),
                ),
            ]
        );
    }
}
