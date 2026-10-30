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
//! visitor call. The selected schema is compiled completely before traversal;
//! pruning avoids descendant occurrence expansion, not schema compilation.
//! Generation policies do not participate in runtime validation or change the
//! runtime archive layout.

#![allow(
    clippy::as_conversions,
    clippy::indexing_slicing,
    reason = "SchemaId table indices are produced and validated by CompiledStore"
)]

use indexmap::IndexMap;

use crate::CompileError;
use crate::DynamicKeySource;
use crate::SchemaDiagnostic;
use crate::SchemaPath;
use crate::SchemaPathElement;
use crate::StoreSource;
use crate::any::SourceLayer;
use crate::any::SourceSchema;
use crate::compiled::Common;
use crate::compiled::CompiledStore;
use crate::compiled::DictSchema;
use crate::compiled::ListSchema;
use crate::compiled::SchemaId;
use crate::dict::SourceRootDict;
use crate::source_layers::expand_layers;

/// Controls whether traversal continues below the current occurrence.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum TraversalControl {
    /// Visit the occurrence's statically known children in schema order.
    Descend,
    /// Return to the parent without resolving or visiting any children.
    SkipChildren,
}

/// Policy for dictionary documentation boundaries during generation.
#[derive(Clone, Copy, Debug)]
pub(crate) enum HiddenKeysPolicy {
    /// Visit children regardless of dictionary documentation settings.
    Expand,
    /// Visit the dictionary itself, but not children when `hide_keys` is set.
    Prune,
    /// Preserve Markdown's direct rendering of nonempty dictionary item keys.
    PruneExceptDictionaryItems,
}

/// Shared traversal policies configured by each generator.
///
/// Visitors can additionally prune occurrences based on consumer-specific
/// knowledge, such as whether a Python model already exists for a reference.
#[derive(Clone, Copy, Debug)]
pub(crate) struct TraversalConfig {
    /// Whether dictionary documentation boundaries suppress descent.
    pub hidden_keys: HiddenKeysPolicy,
    /// Whether removed fields are visited at all. The root is always visited.
    pub include_removed: bool,
}

impl TraversalConfig {
    /// Decide whether policy permits descent after the visitor enters an occurrence.
    ///
    /// Hidden boundaries still receive `enter` and `leave` callbacks. The
    /// dictionary-item exception preserves Markdown's direct rendering of
    /// nonempty item keys independently of the item's `hide_keys` setting.
    fn should_descend(self, occurrence: &SchemaOccurrence<'_>) -> bool {
        let hidden = occurrence
            .common()
            .documentation_options
            .as_ref()
            .is_some_and(|options| options.hide_keys);
        if !hidden {
            return true;
        }
        match self.hidden_keys {
            HiddenKeysPolicy::Expand => true,
            HiddenKeysPolicy::Prune => false,
            HiddenKeysPolicy::PruneExceptDictionaryItems => {
                occurrence.relation() == Some(&SchemaPathElement::Items)
                    && occurrence.dict().is_some_and(|dict| !dict.keys.is_empty())
            }
        }
    }
}

/// Effective schema data and source context for one visited occurrence.
///
/// The effective data comes from the deduplicated compiled store. The path and
/// leading reference layers describe this particular use-site and therefore
/// cannot be stored on the compiled node itself.
#[derive(Debug)]
pub(crate) struct SchemaOccurrence<'a> {
    compiled: &'a CompiledStore,
    path: &'a SchemaPath,
    schema_id: SchemaId,
    pure_reference_count: usize,
    references: Vec<SchemaPath>,
}

impl SchemaOccurrence<'_> {
    /// Absolute source-schema path of this use-site.
    pub(crate) fn path(&self) -> &SchemaPath {
        self.path
    }

    /// Edge from the parent to this occurrence, borrowed from its schema path.
    /// The selected root has no parent edge and returns `None`.
    /// Definitions are resolved as reference targets, not visited as child occurrences.
    pub(crate) fn relation(&self) -> Option<&SchemaPathElement> {
        self.path.elements().last()
    }

    /// Identity of the effective node in the compiled schema tables.
    pub(crate) fn schema_id(&self) -> SchemaId {
        self.schema_id
    }

    /// Leading reference-only layers contributing to this occurrence.
    ///
    /// Consumers decide whether a reference has an existing representation or
    /// whether traversal should continue into its effective children.
    pub(crate) fn pure_references(&self) -> &[SchemaPath] {
        &self.references[..self.pure_reference_count]
    }

    /// References contributing to this occurrence, in layer precedence order.
    ///
    /// Unlike `pure_references`, this includes layers with local overrides.
    /// References belong to a use-site, not to an interned runtime schema ID.
    pub(crate) fn references(&self) -> &[SchemaPath] {
        &self.references
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

/// Compiled schema paired with an occurrence-aware generation traversal.
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
    root_source: &'a SourceRootDict,
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
                schema_path: SchemaPath::root(schema_name),
                reference: format!("{schema_name}#"),
                error: error.into(),
            })?;
        let root_id =
            *compiled
                .roots
                .get(schema_name)
                .ok_or_else(|| SchemaDiagnostic::StructuralCycle {
                    schema_path: SchemaPath::root(schema_name),
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

    /// Visit occurrences allowed by both the generation policy and the visitor.
    pub(crate) fn traverse<V: SchemaVisitor>(
        &self,
        visitor: &mut V,
        config: TraversalConfig,
    ) -> Result<(), V::Error> {
        self.visit(
            &[SourceLayer::root(self.root_source)],
            self.root_id,
            SchemaPath::root(self.schema_name),
            visitor,
            config,
        )
    }

    /// Visit one occurrence and any descendants permitted by both policies.
    ///
    /// Removed non-root occurrences are excluded before either visitor callback.
    /// Hidden-key boundaries instead suppress only descendants: a successful
    /// `enter` is paired with `leave` when traversal completes without error.
    fn visit<V: SchemaVisitor>(
        &self,
        declared_layers: &[SourceLayer<'a>],
        schema_id: SchemaId,
        path: SchemaPath,
        visitor: &mut V,
        config: TraversalConfig,
    ) -> Result<(), V::Error> {
        if !config.include_removed
            && !path.elements().is_empty()
            && common(&self.compiled, schema_id)
                .deprecation
                .as_ref()
                .is_some_and(|deprecation| deprecation.removed)
        {
            return Ok(());
        }
        let expanded = expand_layers(self.source, declared_layers, &path)?;
        let layers = expanded.layers;
        let pure_reference_count = layers
            .iter()
            .copied()
            .take_while(is_pure_reference)
            .filter(|schema| schema.schema_ref().is_some())
            .count();
        let occurrence = SchemaOccurrence {
            compiled: &self.compiled,
            path: &path,
            schema_id,
            pure_reference_count,
            references: expanded.references,
        };

        if visitor.enter(&occurrence)? == TraversalControl::Descend
            && config.should_descend(&occurrence)
        {
            self.visit_children(&layers, schema_id, &path, visitor, config)?;
        }
        visitor.leave(&occurrence)
    }

    /// Visit children in compiled schema order with their declared source layers.
    ///
    /// Source context is reconstructed only for visited occurrences; anonymous
    /// list items and static or dynamic dictionary keys retain distinct relations.
    fn visit_children<V: SchemaVisitor>(
        &self,
        layers: &[SourceLayer<'a>],
        schema_id: SchemaId,
        path: &SchemaPath,
        visitor: &mut V,
        config: TraversalConfig,
    ) -> Result<(), V::Error> {
        match schema_id {
            SchemaId::Bool(_) | SchemaId::Int(_) | SchemaId::Str(_) => Ok(()),
            SchemaId::List(index) => {
                let list = &self.compiled.lists[index as usize];
                let item_layers = layers
                    .iter()
                    .filter_map(|layer| match layer {
                        SourceLayer::Schema(SourceSchema::List(schema)) => schema.items.as_deref(),
                        _ => None,
                    })
                    .map(SourceLayer::schema)
                    .collect::<Vec<_>>();
                if let Some(item_id) = list.items
                    && !item_layers.is_empty()
                {
                    self.visit(
                        &item_layers,
                        item_id,
                        path.child(SchemaPathElement::Items),
                        visitor,
                        config,
                    )?;
                }
                Ok(())
            }
            SchemaId::Dict(index) => {
                let dict = &self.compiled.dicts[index as usize];
                let key_layers = dict_child_layers(layers);
                for (name, child_id) in &dict.keys {
                    let declared = key_layers.get(name.as_str()).ok_or_else(|| {
                        CompileError::from(SchemaDiagnostic::StructuralCycle {
                            schema_path: path.child(SchemaPathElement::Key { key: name.clone() }),
                        })
                    })?;
                    self.visit(
                        declared,
                        *child_id,
                        path.child(SchemaPathElement::Key { key: name.clone() }),
                        visitor,
                        config,
                    )?;
                }
                let dynamic_key_layers = dynamic_child_layers(layers);
                for (name, child_id) in &dict.dynamic_keys {
                    let (source, declared) =
                        dynamic_key_layers.get(name.as_str()).ok_or_else(|| {
                            CompileError::from(SchemaDiagnostic::StructuralCycle {
                                schema_path: path.clone(),
                            })
                        })?;
                    self.visit(
                        declared,
                        *child_id,
                        path.child(SchemaPathElement::DynamicKey { source: *source }),
                        visitor,
                        config,
                    )?;
                }
                Ok(())
            }
        }
    }
}

fn dict_child_layers<'a>(layers: &[SourceLayer<'a>]) -> IndexMap<&'a str, Vec<SourceLayer<'a>>> {
    let mut children = IndexMap::new();
    for layer in layers {
        let map = match layer {
            SourceLayer::Root(schema) => schema.keys.as_ref(),
            SourceLayer::Schema(SourceSchema::Dict(schema)) => schema.keys.as_ref(),
            SourceLayer::Schema(_) => None,
        };
        for (name, child) in map.into_iter().flat_map(|map| map.iter()) {
            children
                .entry(name.as_str())
                .or_insert_with(Vec::new)
                .push(SourceLayer::schema(child));
        }
    }
    children
}

fn dynamic_child_layers<'a>(
    layers: &[SourceLayer<'a>],
) -> IndexMap<&'a str, (DynamicKeySource, Vec<SourceLayer<'a>>)> {
    let mut children = IndexMap::new();
    for layer in layers {
        if let SourceLayer::Root(schema) = layer {
            for (source, child) in schema.dynamic_keys.iter().flat_map(|map| map.iter()) {
                children
                    .entry(source.path())
                    .or_insert_with(|| (*source, Vec::new()))
                    .1
                    .push(SourceLayer::schema(child));
            }
        }
    }
    children
}

fn is_pure_reference(layer: &SourceLayer<'_>) -> bool {
    match layer {
        SourceLayer::Root(schema) => {
            base_is_pure(&schema.base)
                && schema.keys.is_none()
                && schema.dynamic_keys.is_none()
                && schema.allow_other_keys.is_none()
                && schema.schema_defs.is_none()
                && schema.schema_id.is_none()
                && schema.schema_schema.is_none()
        }
        SourceLayer::Schema(SourceSchema::Bool(schema)) => base_is_pure(&schema.base),
        SourceLayer::Schema(SourceSchema::Int(schema)) => {
            base_is_pure(&schema.base)
                && schema.min.is_none()
                && schema.max.is_none()
                && schema.valid_values.valid_values.is_none()
                && schema.valid_values.dynamic_valid_values.is_none()
                && schema.convert_types.convert_types.is_none()
        }
        SourceLayer::Schema(SourceSchema::Str(schema)) => {
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
        SourceLayer::Schema(SourceSchema::List(schema)) => {
            base_is_pure(&schema.base)
                && schema.items.is_none()
                && schema.min_length.is_none()
                && schema.max_length.is_none()
                && schema.primary_key.is_none()
                && schema.unique_keys.is_none()
                && schema.allow_duplicate_primary_key.is_none()
        }
        SourceLayer::Schema(SourceSchema::Dict(schema)) => {
            base_is_pure(&schema.base) && schema.keys.is_none() && schema.allow_other_keys.is_none()
        }
    }
}

fn base_is_pure<T: crate::base::DataValue>(base: &crate::base::Base<T>) -> bool {
    base.default.is_none()
        && base.display_name.is_none()
        && base.required.is_none()
        && base.schema_ref.is_some()
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

#[cfg(test)]
mod tests {
    use super::HiddenKeysPolicy;
    use super::SchemaOccurrence;
    use super::SchemaTraverser;
    use super::SchemaVisitor;
    use super::TraversalConfig;
    use super::TraversalControl;
    use crate::Load as _;
    use crate::SchemaPath;
    use crate::SchemaPathElement;
    use crate::StoreSource;

    #[derive(Default)]
    struct RecordingVisitor {
        visits: Vec<(SchemaPath, Vec<String>)>,
    }

    impl SchemaVisitor for RecordingVisitor {
        type Error = crate::CompileError;

        fn enter(
            &mut self,
            occurrence: &SchemaOccurrence<'_>,
        ) -> Result<TraversalControl, Self::Error> {
            self.visits.push((
                occurrence.path().clone(),
                occurrence
                    .pure_references()
                    .iter()
                    .map(SchemaPath::to_reference)
                    .collect(),
            ));
            if matches!(occurrence.relation(), Some(SchemaPathElement::Key { key }) if key == "first")
            {
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

        traverser
            .traverse(
                &mut visitor,
                TraversalConfig {
                    hidden_keys: HiddenKeysPolicy::Expand,
                    include_removed: true,
                },
            )
            .unwrap();

        assert_eq!(
            visitor.visits,
            vec![
                (SchemaPath::root("model"), Vec::new()),
                (
                    SchemaPath::root("model").child(SchemaPathElement::Key {
                        key: "first".to_owned(),
                    }),
                    vec!["shared#".to_owned()],
                ),
                (
                    SchemaPath::root("model").child(SchemaPathElement::Key {
                        key: "second".to_owned(),
                    }),
                    vec!["shared#".to_owned()],
                ),
                (
                    SchemaPath::root("model")
                        .child(SchemaPathElement::Key {
                            key: "second".to_owned(),
                        })
                        .child(SchemaPathElement::Key {
                            key: "value".to_owned(),
                        }),
                    Vec::new(),
                ),
            ]
        );
    }
}
