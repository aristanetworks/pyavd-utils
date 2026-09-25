// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Python source generation compatible with pyAVD's nested schema models.
//!
//! A traversal first validates the subset supported by this generator and
//! builds a Python-specific projection. Rendering then converts that projection
//! into explicit class, field, literal, and import plans. Keeping rendering
//! plans separate from schema traversal makes output conventions independent of
//! how effective schema occurrences are discovered.

#![allow(
    clippy::as_conversions,
    clippy::indexing_slicing,
    clippy::manual_let_else,
    clippy::struct_excessive_bools,
    clippy::too_many_lines,
    clippy::unreachable,
    reason = "The renderer consumes compiler-validated schema IDs and uses explicit plans"
)]

use std::fmt::Write as _;

use indexmap::IndexMap;

use crate::CompileError;
use crate::StoreSource;
use crate::compiled::CompiledStore;
use crate::compiled::CompiledValue;
use crate::compiled::SchemaId;
use crate::generation::traversal::SchemaOccurrence;
use crate::generation::traversal::SchemaRelation;
use crate::generation::traversal::SchemaTraverser;
use crate::generation::traversal::SchemaVisitor;
use crate::generation::traversal::TraversalControl;

const HEADER: &str = "# Copyright (c) 2026 Arista Networks, Inc.\n\
# Use of this source code is governed by the Apache License 2.0\n\
# that can be found in the LICENSE file.\n\n\
from __future__ import annotations\n";

/// Error raised while planning or rendering a generated artifact.
#[derive(Debug, derive_more::Display)]
pub enum GenerationError {
    /// Schema compilation failed.
    #[display("{_0}")]
    Compile(CompileError),
    /// The selected generator requires a dictionary root.
    #[display(
        "Schema '{schema_name}' has type '{found}', but Python model generation requires a dictionary root"
    )]
    RootType {
        /// Name of the schema selected for generation.
        schema_name: String,
        /// Model type found at the schema root.
        found: &'static str,
    },
    /// A requested root key is not present in the selected schema.
    #[display("Root key '{root_key}' was not found in schema '{schema_name}'")]
    UnknownRootKey {
        /// Name of the schema selected for generation.
        schema_name: String,
        /// Root key requested by the caller.
        root_key: String,
    },
    /// The selected occurrence uses a feature outside the current experiment.
    #[display("Python model generation does not yet support {feature} at '{schema_path}'")]
    Unsupported {
        /// Path of the schema occurrence using the unsupported feature.
        schema_path: String,
        /// Name of the unsupported schema feature.
        feature: &'static str,
    },
    /// Generation metadata required by an artifact convention is absent.
    #[display("Python model generation requires '{metadata}' at '{schema_path}'")]
    MissingMetadata {
        /// Path of the schema occurrence missing required metadata.
        schema_path: String,
        /// Name of the missing metadata field.
        metadata: &'static str,
    },
}

impl From<CompileError> for GenerationError {
    fn from(value: CompileError) -> Self {
        Self::Compile(value)
    }
}

/// Generate the current nested Python model source for one named schema.
pub fn generate_python_models(
    store: &StoreSource,
    schema_name: &str,
) -> Result<String, GenerationError> {
    generate_python_models_projection(store, schema_name, &class_name(schema_name), &[])
}

/// Generate a current Python model containing only selected root keys.
///
/// An empty `root_keys` slice includes every root key.
pub fn generate_python_models_projection(
    store: &StoreSource,
    schema_name: &str,
    generated_class_name: &str,
    root_keys: &[String],
) -> Result<String, GenerationError> {
    let traverser = SchemaTraverser::compile(store, schema_name)?;
    let mut projection = PythonProjection::new(schema_name, generated_class_name, root_keys);
    traverser.traverse(&mut projection)?;
    let root_node = projection.finish();
    let mut root = build_node(
        traverser.compiled(),
        &root_node,
        generated_class_name,
        Some(root_base(generated_class_name)),
    );
    if generated_class_name == "EosDesigns" {
        augment_eos_designs_root(traverser.compiled(), &root_node, &mut root)?;
    }
    let mut imports = ImportSet::default();
    root.collect_imports(&mut imports);
    let mut output = String::from(HEADER);
    render_imports(&mut output, &imports);
    output.push_str("\n\n");
    root.render(&mut output, 0);
    while output.ends_with('\n') {
        output.pop();
    }
    output.push('\n');
    Ok(output)
}

/// Generator-specific projection of one visited schema occurrence.
///
/// The projection owns only nodes that Python model generation consumes.
/// Removed fields, unselected root keys, and descendants represented by an
/// existing referenced model are pruned while traversal is in progress.
#[derive(Clone, Debug)]
struct PythonNode {
    relation: OwnedRelation,
    path: Vec<String>,
    schema_id: SchemaId,
    retained_model_reference: Option<String>,
    keys: IndexMap<String, PythonNode>,
    dynamic_keys: IndexMap<String, PythonNode>,
    items: Option<Box<PythonNode>>,
}

impl PythonNode {
    fn from_occurrence(
        occurrence: &SchemaOccurrence<'_>,
        retained_model_reference: Option<String>,
    ) -> Self {
        Self {
            relation: occurrence.relation().into(),
            path: occurrence.path().to_vec(),
            schema_id: occurrence.schema_id(),
            retained_model_reference,
            keys: IndexMap::new(),
            dynamic_keys: IndexMap::new(),
            items: None,
        }
    }

    fn attach(&mut self, child: Self) {
        match &child.relation {
            OwnedRelation::Root => unreachable!(),
            OwnedRelation::Key(name) => {
                self.keys.insert(name.clone(), child);
            }
            OwnedRelation::DynamicKey(name) => {
                self.dynamic_keys.insert(name.clone(), child);
            }
            OwnedRelation::Items => self.items = Some(Box::new(child)),
        }
    }
}

/// Owned form of [`SchemaRelation`] retained in the Python output plan.
#[derive(Clone, Debug)]
enum OwnedRelation {
    Root,
    Key(String),
    DynamicKey(String),
    Items,
}

impl From<SchemaRelation<'_>> for OwnedRelation {
    fn from(value: SchemaRelation<'_>) -> Self {
        match value {
            SchemaRelation::Root => Self::Root,
            SchemaRelation::Key(name) => Self::Key(name.to_owned()),
            SchemaRelation::DynamicKey(name) => Self::DynamicKey(name.to_owned()),
            SchemaRelation::Items => Self::Items,
        }
    }
}

/// Builds and validates the minimal occurrence tree needed by Python rendering.
struct PythonProjection<'a> {
    schema_name: &'a str,
    generated_class_name: &'a str,
    root_keys: &'a [String],
    stack: Vec<Option<PythonNode>>,
    root: Option<PythonNode>,
}

impl<'a> PythonProjection<'a> {
    fn new(schema_name: &'a str, generated_class_name: &'a str, root_keys: &'a [String]) -> Self {
        Self {
            schema_name,
            generated_class_name,
            root_keys,
            stack: Vec::new(),
            root: None,
        }
    }

    fn finish(self) -> PythonNode {
        match self.root {
            Some(root) => root,
            None => unreachable!("traversal always visits and completes the compiled root"),
        }
    }

    fn unsupported(occurrence: &SchemaOccurrence<'_>, feature: &'static str) -> GenerationError {
        GenerationError::Unsupported {
            schema_path: occurrence.path().join("/"),
            feature,
        }
    }
}

impl SchemaVisitor for PythonProjection<'_> {
    type Error = GenerationError;

    fn enter(
        &mut self,
        occurrence: &SchemaOccurrence<'_>,
    ) -> Result<TraversalControl, Self::Error> {
        if occurrence.relation() == SchemaRelation::Root {
            let Some(dict) = occurrence.dict() else {
                return Err(GenerationError::RootType {
                    schema_name: self.schema_name.to_owned(),
                    found: runtime_type(occurrence.schema_id()),
                });
            };
            if let Some(root_key) = self
                .root_keys
                .iter()
                .find(|root_key| !dict.keys.contains_key(root_key.as_str()))
            {
                return Err(GenerationError::UnknownRootKey {
                    schema_name: self.schema_name.to_owned(),
                    root_key: root_key.clone(),
                });
            }
        } else if self.stack.len() == 1
            && let SchemaRelation::Key(name) = occurrence.relation()
            && !self.root_keys.is_empty()
            && !self.root_keys.iter().any(|selected| selected == name)
        {
            self.stack.push(None);
            return Ok(TraversalControl::SkipChildren);
        }

        if occurrence.relation() != SchemaRelation::Root && is_removed_common(occurrence.common()) {
            self.stack.push(None);
            return Ok(TraversalControl::SkipChildren);
        }

        let retained_model_reference = retained_model_reference(occurrence, self.schema_name);
        let control = if retained_model_reference.is_some() {
            TraversalControl::SkipChildren
        } else {
            match occurrence.schema_id() {
                SchemaId::Bool(_) | SchemaId::Int(_) | SchemaId::Str(_) => {}
                SchemaId::List(_) => {
                    let Some(list) = occurrence.list() else {
                        unreachable!("SchemaId identifies a list");
                    };
                    let Some(items) = list.items else {
                        return Err(Self::unsupported(
                            occurrence,
                            "lists without an item schema",
                        ));
                    };
                    if matches!(items, SchemaId::List(_)) {
                        return Err(Self::unsupported(occurrence, "nested lists"));
                    }
                }
                SchemaId::Dict(_) => {
                    let Some(dict) = occurrence.dict() else {
                        unreachable!("SchemaId identifies a dictionary");
                    };
                    let root_dynamic_keys = occurrence.relation() == SchemaRelation::Root
                        && self.generated_class_name == "EosDesigns";
                    if !dict.dynamic_keys.is_empty() && !root_dynamic_keys {
                        return Err(Self::unsupported(occurrence, "dynamic keys"));
                    }
                }
            }
            TraversalControl::Descend
        };
        self.stack.push(Some(PythonNode::from_occurrence(
            occurrence,
            retained_model_reference,
        )));
        Ok(control)
    }

    fn leave(&mut self, _occurrence: &SchemaOccurrence<'_>) -> Result<(), Self::Error> {
        let completed = match self.stack.pop() {
            Some(completed) => completed,
            None => unreachable!("leave is paired with every successful enter"),
        };
        let Some(completed) = completed else {
            return Ok(());
        };
        if let Some(parent) = self.stack.last_mut() {
            let Some(parent) = parent.as_mut() else {
                unreachable!("traversal never descends below a pruned occurrence");
            };
            parent.attach(completed);
        } else {
            self.root = Some(completed);
        }
        Ok(())
    }
}

/// Select a pure cross-schema reference represented by an existing Python model.
fn retained_model_reference(
    occurrence: &SchemaOccurrence<'_>,
    model_schema_name: &str,
) -> Option<String> {
    let retain_reference = match occurrence.schema_id() {
        SchemaId::Dict(_) => true,
        SchemaId::List(_) => occurrence
            .list()
            .is_some_and(|list| list.primary_key.is_some() && !list.allow_duplicate_primary_key),
        SchemaId::Bool(_) | SchemaId::Int(_) | SchemaId::Str(_) => false,
    };
    retain_reference.then(|| {
        occurrence
            .pure_references()
            .iter()
            .copied()
            .find(|reference| {
                reference
                    .split_once('#')
                    .is_some_and(|(schema_name, _)| schema_name != model_schema_name)
                    && !reference.contains("/$defs/")
            })
            .map(ToOwned::to_owned)
    })?
}

#[derive(Clone, Debug)]
/// Renderable plan for one nested Python class declaration.
enum ClassPlan {
    Model(ModelPlan),
    List(ListPlan),
    Literal(LiteralPlan),
}

impl ClassPlan {
    fn render(&self, output: &mut String, level: usize) {
        match self {
            Self::Model(plan) => plan.render(output, level),
            Self::List(plan) => plan.render(output, level),
            Self::Literal(plan) => plan.render(output, level),
        }
    }

    fn collect_imports(&self, imports: &mut ImportSet) {
        match self {
            Self::Model(plan) => plan.collect_imports(imports),
            Self::List(plan) => plan.collect_imports(imports),
            Self::Literal(_) => imports.literal = true,
        }
    }
}

#[derive(Clone, Debug)]
/// Python model class with its nested declarations and schema-backed fields.
struct ModelPlan {
    name: String,
    base: String,
    description: Option<String>,
    classes: Vec<ClassPlan>,
    fields: Vec<FieldPlan>,
    class_vars: Vec<ClassVarPlan>,
    allow_other_keys: bool,
}

impl ModelPlan {
    fn render(&self, output: &mut String, level: usize) {
        line(
            output,
            level,
            &format!("class {}({}):", self.name, self.base),
        );
        if let Some(description) = &self.description {
            render_docstring(output, level + 1, description);
        }
        if !self.classes.is_empty() {
            for (index, class) in self.classes.iter().enumerate() {
                if index > 0 && !matches!(self.classes.get(index - 1), Some(ClassPlan::Literal(_)))
                {
                    output.push('\n');
                }
                class.render(output, level + 1);
            }
            if !matches!(self.classes.last(), Some(ClassPlan::Literal(_))) {
                output.push('\n');
            }
        }
        if !self.class_vars.is_empty() {
            for class_var in &self.class_vars {
                line(
                    output,
                    level + 1,
                    &format!(
                        "{}: ClassVar[{}] = {}",
                        class_var.name, class_var.type_hint, class_var.value
                    ),
                );
            }
        }
        if !self.fields.is_empty() {
            self.render_fields(output, level + 1);
            output.push_str("\n\n");
            self.render_init(output, level + 1);
            output.push('\n');
        }
    }

    fn render_fields(&self, output: &mut String, level: usize) {
        line(output, level, "_fields: ClassVar[dict] = {");
        for (index, field) in self.fields.iter().enumerate() {
            let comma = if index + 1 == self.fields.len() {
                ""
            } else {
                ","
            };
            line(
                output,
                level + 1,
                &format!(
                    "\"{}\": {{\"type\": {}{}}}{}",
                    field.name,
                    field.runtime_type,
                    field
                        .default
                        .as_ref()
                        .map(|default| format!(", \"default\": {default}"))
                        .unwrap_or_default(),
                    comma
                ),
            );
        }
        line(output, level, "}");
        let remapped_fields = self
            .fields
            .iter()
            .filter(|field| field.name != field.key)
            .collect::<Vec<_>>();
        if !remapped_fields.is_empty() {
            line(
                output,
                level,
                &format!(
                    "_field_to_key_map: ClassVar[dict] = {{{}}}",
                    remapped_fields
                        .iter()
                        .map(|field| format!("'{}': '{}'", field.name, field.key))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            );
            line(
                output,
                level,
                &format!(
                    "_key_to_field_map: ClassVar[dict] = {{{}}}",
                    remapped_fields
                        .iter()
                        .map(|field| format!("'{}': '{}'", field.key, field.name))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            );
        }
        if self.allow_other_keys {
            line(output, level, "_allow_other_keys: ClassVar[bool] = True");
        }
        for field in &self.fields {
            line(
                output,
                level,
                &format!("{}: {}", field.name, field.annotation(false)),
            );
            if let Some(docstring) = field.docstring() {
                render_preformatted_docstring(output, level, &docstring);
            }
        }
    }

    fn render_init(&self, output: &mut String, level: usize) {
        line(output, level, "if TYPE_CHECKING:");
        line(output, level + 1, "def __init__(");
        line(output, level + 2, "self,");
        line(output, level + 2, "*,");
        for (index, field) in self.fields.iter().enumerate() {
            let comma = if index + 1 == self.fields.len() {
                ""
            } else {
                ","
            };
            line(
                output,
                level + 2,
                &format!(
                    "{}: {} = Undefined{}",
                    field.name,
                    field.annotation(true),
                    comma
                ),
            );
        }
        line(output, level + 1, ")-> None:");
        line(output, level + 2, "\"\"\"");
        line(output, level + 2, &format!("{}.", self.name));
        output.push_str("\n\n");
        if let Some(description) = &self.description {
            for wrapped in wrap_description(description, 100) {
                line(output, level + 2, &wrapped);
            }
            output.push('\n');
        }
        line(output, level + 2, "Args:");
        for field in &self.fields {
            match field.description.as_deref() {
                Some(description) => {
                    let formatted = wrap_description(description, 100)
                        .join("\n")
                        .replace("Example:\n", "Example:  # fmt: skip\n")
                        .replace("Examples:\n", "Examples:  # fmt: skip\n")
                        .replace("Note:\n", "Note:  # fmt: skip\n")
                        .replace("Notes:\n", "Notes:  # fmt: skip\n");
                    if formatted.contains('\n') {
                        line(output, level + 3, &format!("{}:", field.name));
                        for physical_line in formatted.split('\n') {
                            if physical_line.is_empty() {
                                output.push('\n');
                            } else {
                                let _ = writeln!(
                                    output,
                                    "{}   {physical_line}",
                                    "    ".repeat(level + 3)
                                );
                            }
                        }
                    } else {
                        line(output, level + 3, &format!("{}: {formatted}", field.name));
                    }
                }
                None => line(
                    output,
                    level + 3,
                    &format!("{}: {}", field.name, field.name),
                ),
            }
        }
        output.push('\n');
        line(output, level + 2, "\"\"\"");
    }

    fn collect_imports(&self, imports: &mut ImportSet) {
        imports.class_var =
            !self.fields.is_empty() || !self.class_vars.is_empty() || self.allow_other_keys;
        imports.avd_model = true;
        match self.base.as_str() {
            "EosCliConfigGenRootModel" => imports.eos_cli_root = true,
            "EosDesignsRootModel" => imports.eos_designs_root = true,
            "Protocol" => imports.protocol = true,
            _ => {}
        }
        for class in &self.classes {
            class.collect_imports(imports);
        }
        for field in &self.fields {
            imports.coerce_type |= field
                .default
                .as_deref()
                .is_some_and(|default| default.contains("coerce_type"));
            if let Some(reference) = &field.external_reference {
                if reference == "EosDesigns" || reference.starts_with("EosDesigns.") {
                    imports.eos_designs = true;
                } else if reference == "EosCliConfigGen"
                    || reference.starts_with("EosCliConfigGen.")
                {
                    imports.eos_cli = true;
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
/// Class-level variable emitted as part of a generated model.
struct ClassVarPlan {
    name: String,
    type_hint: String,
    value: String,
}

#[derive(Clone, Debug)]
/// Generated list model and its item-type contract.
struct ListPlan {
    name: String,
    base: String,
    item_type: String,
    primary_key: Option<String>,
    description: Option<String>,
}

impl ListPlan {
    fn render(&self, output: &mut String, level: usize) {
        line(
            output,
            level,
            &format!("class {}({}):", self.name, self.base),
        );
        if let Some(description) = &self.description {
            render_docstring(output, level + 1, description);
        }
        if let Some(primary_key) = &self.primary_key {
            line(
                output,
                level + 1,
                &format!("_primary_key: ClassVar[str] = \"{primary_key}\""),
            );
        }
        output.push('\n');
        line(
            output,
            level,
            &format!("{}._item_type = {}", self.name, self.item_type),
        );
    }

    fn collect_imports(&self, imports: &mut ImportSet) {
        imports.class_var |= self.primary_key.is_some();
        if self.primary_key.is_some() {
            imports.avd_indexed_list = true;
        } else {
            imports.avd_list = true;
        }
    }
}

#[derive(Clone, Debug)]
/// Type alias constraining a scalar field to literal schema values.
struct LiteralPlan {
    name: String,
    values: Vec<String>,
}

impl LiteralPlan {
    fn render(&self, output: &mut String, level: usize) {
        line(
            output,
            level,
            &format!(
                "{}: TypeAlias = Literal[{}]",
                self.name,
                self.values.join(", ")
            ),
        );
    }
}

#[derive(Clone, Debug)]
/// Python field metadata used for annotations, defaults, and docstrings.
struct FieldPlan {
    name: String,
    key: String,
    runtime_type: String,
    type_hint: String,
    optional: bool,
    default: Option<String>,
    description: Option<String>,
    external_reference: Option<String>,
}

impl FieldPlan {
    fn annotation(&self, include_undefined: bool) -> String {
        let mut types = vec![self.type_hint.clone()];
        if include_undefined {
            types.push("UndefinedType".to_owned());
        }
        if self.optional
            && self.default.is_none()
            && matches!(self.runtime_type.as_str(), "str" | "int" | "bool")
        {
            types.push("None".to_owned());
        }
        types.join(" | ")
    }

    fn docstring(&self) -> Option<String> {
        let description = self
            .description
            .as_deref()
            .map(|description| wrap_description(description, 100).join("\n"));
        match (description, &self.default) {
            (Some(description), Some(default)) => {
                Some(format!("{description}\n\nDefault value: `{default}`"))
            }
            (Some(description), None) => Some(description),
            (None, Some(default)) => Some(format!("Default value: `{default}`")),
            (None, None) => None,
        }
    }
}

fn build_node(
    compiled: &CompiledStore,
    node: &PythonNode,
    name: &str,
    base: Option<&str>,
) -> ClassPlan {
    match node.schema_id {
        SchemaId::Dict(index) => {
            let schema = &compiled.dicts[index as usize];
            let mut classes = Vec::new();
            let mut fields = Vec::new();
            for (key, child) in &node.keys {
                let child_name = class_name(key);
                let is_primary_key = false;
                let (mut child_classes, field) =
                    build_field(compiled, child, key, &child_name, is_primary_key);
                classes.append(&mut child_classes);
                fields.push(field);
            }
            ClassPlan::Model(ModelPlan {
                name: name.to_owned(),
                base: base.unwrap_or("AvdModel").to_owned(),
                description: Some(format!("Subclass of {}.", base.unwrap_or("AvdModel"))),
                classes,
                fields,
                class_vars: Vec::new(),
                allow_other_keys: schema.allow_other_keys,
            })
        }
        _ => unreachable!(),
    }
}

fn augment_eos_designs_root(
    compiled: &CompiledStore,
    root_node: &PythonNode,
    root: &mut ClassPlan,
) -> Result<(), GenerationError> {
    let ClassPlan::Model(root) = root else {
        return Ok(());
    };
    let custom_item_name = "_CustomStructuredConfigurationsItem";
    root.classes.push(ClassPlan::Model(ModelPlan {
        name: custom_item_name.to_owned(),
        base: "AvdModel".to_owned(),
        description: None,
        classes: Vec::new(),
        fields: vec![
            FieldPlan {
                name: "key".to_owned(),
                key: "key".to_owned(),
                runtime_type: "str".to_owned(),
                type_hint: "str".to_owned(),
                optional: false,
                default: None,
                description: Some("Complete key including prefix".to_owned()),
                external_reference: None,
            },
            FieldPlan {
                name: "value".to_owned(),
                key: "value".to_owned(),
                runtime_type: "EosCliConfigGen".to_owned(),
                type_hint: "EosCliConfigGen".to_owned(),
                optional: false,
                default: None,
                description: Some(
                    "Structured config including the suffix part of the key.".to_owned(),
                ),
                external_reference: Some("EosCliConfigGen".to_owned()),
            },
        ],
        class_vars: Vec::new(),
        allow_other_keys: false,
    }));
    root.classes.push(ClassPlan::List(ListPlan {
        name: "_CustomStructuredConfigurations".to_owned(),
        base: format!("AvdIndexedList[str, {custom_item_name}]"),
        item_type: custom_item_name.to_owned(),
        primary_key: Some("key".to_owned()),
        description: None,
    }));
    root.fields.push(FieldPlan {
        name: "_custom_structured_configurations".to_owned(),
        key: "_custom_structured_configurations".to_owned(),
        runtime_type: "_CustomStructuredConfigurations".to_owned(),
        type_hint: "_CustomStructuredConfigurations".to_owned(),
        optional: true,
        default: None,
        description: None,
        external_reference: None,
    });

    if root_node.dynamic_keys.is_empty() {
        return Ok(());
    }
    let mut dynamic_classes = Vec::new();
    let mut dynamic_fields = Vec::new();
    let mut dynamic_key_maps = Vec::new();
    for (dynamic_path, node) in &root_node.dynamic_keys {
        let display_name = common(compiled, node.schema_id)
            .display_name
            .as_deref()
            .ok_or_else(|| GenerationError::MissingMetadata {
                schema_path: node.path.join("/"),
                metadata: "display_name",
            })?;
        let dynamic_type = display_name.replace(' ', "_").to_lowercase();
        let model_name = class_name(&format!("dynamic_{dynamic_type}"));
        dynamic_key_maps.push(format!(
            "{{'dynamic_keys_path': '{dynamic_path}', 'model_key': '{dynamic_type}'}}"
        ));
        let (child_classes, mut value_field) = build_field(
            compiled,
            node,
            dynamic_path,
            &class_name(&dynamic_type),
            false,
        );
        "value".clone_into(&mut value_field.name);
        value_field.description = Some("Value of dynamic key".to_owned());
        dynamic_classes.push(ClassPlan::Model(ModelPlan {
            name: format!("{model_name}Item"),
            base: "AvdModel".to_owned(),
            description: None,
            classes: child_classes,
            fields: vec![
                FieldPlan {
                    name: "key".to_owned(),
                    key: "key".to_owned(),
                    runtime_type: "str".to_owned(),
                    type_hint: "str".to_owned(),
                    optional: false,
                    default: None,
                    description: Some("Key used as dynamic key".to_owned()),
                    external_reference: None,
                },
                value_field,
            ],
            class_vars: Vec::new(),
            allow_other_keys: false,
        }));
        dynamic_classes.push(ClassPlan::List(ListPlan {
            name: model_name.clone(),
            base: format!("AvdIndexedList[str, {model_name}Item]"),
            item_type: format!("{model_name}Item"),
            primary_key: Some("key".to_owned()),
            description: None,
        }));
        dynamic_fields.push(FieldPlan {
            name: dynamic_type.clone(),
            key: dynamic_type.clone(),
            runtime_type: model_name.clone(),
            type_hint: model_name,
            optional: false,
            default: None,
            description: Some(format!("Collection of dynamic '{dynamic_type}'.")),
            external_reference: None,
        });
    }
    let tuple_suffix = if dynamic_key_maps.len() == 1 { "," } else { "" };
    root.classes.push(ClassPlan::Model(ModelPlan {
        name: "_DynamicKeys".to_owned(),
        base: "AvdModel".to_owned(),
        description: Some("Data models for dynamic keys.".to_owned()),
        classes: dynamic_classes,
        fields: dynamic_fields,
        class_vars: vec![ClassVarPlan {
            name: "_dynamic_key_maps".to_owned(),
            type_hint: "tuple[dict, ...]".to_owned(),
            value: format!("({}{tuple_suffix})", dynamic_key_maps.join(", ")),
        }],
        allow_other_keys: false,
    }));
    root.fields.push(FieldPlan {
        name: "_dynamic_keys".to_owned(),
        key: "_dynamic_keys".to_owned(),
        runtime_type: "_DynamicKeys".to_owned(),
        type_hint: "_DynamicKeys".to_owned(),
        optional: false,
        default: None,
        description: Some("Dynamic keys".to_owned()),
        external_reference: None,
    });
    Ok(())
}

fn build_field(
    compiled: &CompiledStore,
    node: &PythonNode,
    key: &str,
    generated_name: &str,
    primary_key: bool,
) -> (Vec<ClassPlan>, FieldPlan) {
    if let Some(reference) = &node.retained_model_reference {
        let reference_name = class_name_from_ref(reference);
        let mut field = field_plan(
            compiled,
            node.schema_id,
            key,
            &reference_name,
            &reference_name,
            primary_key,
            Some(reference_name.clone()),
        );
        field
            .description
            .clone_from(&common(compiled, node.schema_id).description);
        return (Vec::new(), field);
    }
    match node.schema_id {
        SchemaId::Bool(_) => (
            Vec::new(),
            field_plan(
                compiled,
                node.schema_id,
                key,
                "bool",
                "bool",
                primary_key,
                None,
            ),
        ),
        SchemaId::Int(index) => {
            let schema = &compiled.ints[index as usize];
            let classes = schema
                .valid_values
                .as_ref()
                .map_or_else(Vec::new, |values| {
                    vec![ClassPlan::Literal(LiteralPlan {
                        name: generated_name.to_owned(),
                        values: values.iter().map(ToString::to_string).collect(),
                    })]
                });
            let type_hint = if classes.is_empty() {
                "int"
            } else {
                generated_name
            };
            (
                classes,
                field_plan(
                    compiled,
                    node.schema_id,
                    key,
                    "int",
                    type_hint,
                    primary_key,
                    None,
                ),
            )
        }
        SchemaId::Str(index) => {
            let schema = &compiled.strings[index as usize];
            let classes = schema
                .valid_values
                .as_ref()
                .map_or_else(Vec::new, |values| {
                    vec![ClassPlan::Literal(LiteralPlan {
                        name: generated_name.to_owned(),
                        values: values.iter().map(|value| format!("\"{value}\"")).collect(),
                    })]
                });
            let type_hint = if classes.is_empty() {
                "str"
            } else {
                generated_name
            };
            (
                classes,
                field_plan(
                    compiled,
                    node.schema_id,
                    key,
                    "str",
                    type_hint,
                    primary_key,
                    None,
                ),
            )
        }
        SchemaId::Dict(index) => {
            let schema = &compiled.dicts[index as usize];
            if schema.keys.is_empty() {
                return (
                    Vec::new(),
                    field_plan(
                        compiled,
                        node.schema_id,
                        key,
                        "dict",
                        "dict",
                        primary_key,
                        None,
                    ),
                );
            }
            let class = build_node(compiled, node, generated_name, None);
            (
                vec![class],
                field_plan(
                    compiled,
                    node.schema_id,
                    key,
                    generated_name,
                    generated_name,
                    primary_key,
                    None,
                ),
            )
        }
        SchemaId::List(index) => {
            let schema = &compiled.lists[index as usize];
            let item_name = match (schema.items, node.items.as_deref()) {
                (Some(SchemaId::Dict(_)), Some(item_node)) => {
                    let item_name = format!("{generated_name}Item");
                    let item = build_dict_item(
                        compiled,
                        item_node,
                        &item_name,
                        schema.primary_key.as_deref(),
                    );
                    let indexed_primary_key = if schema.allow_duplicate_primary_key {
                        None
                    } else {
                        schema.primary_key.as_deref()
                    };
                    let description = list_description(indexed_primary_key, &item_name, item_node);
                    let list = list_plan(
                        generated_name,
                        &item_name,
                        indexed_primary_key,
                        description.clone(),
                        item_node,
                    );
                    let mut field = field_plan(
                        compiled,
                        node.schema_id,
                        key,
                        generated_name,
                        generated_name,
                        primary_key,
                        None,
                    );
                    field.description = Some(combine_descriptions(
                        common(compiled, node.schema_id).description.as_deref(),
                        &description,
                    ));
                    return (vec![item, ClassPlan::List(list)], field);
                }
                (Some(SchemaId::Str(_)), _) => "str".to_owned(),
                (Some(SchemaId::Int(_)), _) => "int".to_owned(),
                (Some(SchemaId::Bool(_)), _) => "bool".to_owned(),
                _ => "Any".to_owned(),
            };
            let description = format!("Subclass of AvdList with `{item_name}` items.");
            let list = ListPlan {
                name: generated_name.to_owned(),
                base: format!("AvdList[{item_name}]"),
                item_type: item_name,
                primary_key: None,
                description: Some(description.clone()),
            };
            let mut field = field_plan(
                compiled,
                node.schema_id,
                key,
                generated_name,
                generated_name,
                primary_key,
                None,
            );
            field.description = Some(combine_descriptions(
                common(compiled, node.schema_id).description.as_deref(),
                &description,
            ));
            (vec![ClassPlan::List(list)], field)
        }
    }
}

fn build_dict_item(
    compiled: &CompiledStore,
    node: &PythonNode,
    name: &str,
    primary_key: Option<&str>,
) -> ClassPlan {
    let SchemaId::Dict(index) = node.schema_id else {
        unreachable!();
    };
    let schema = &compiled.dicts[index as usize];
    let keys = &node.keys;
    let mut classes = Vec::new();
    let mut fields = Vec::new();
    let mut ordered_keys = Vec::with_capacity(keys.len());
    if let Some(primary_key) = primary_key
        && let Some(primary_key_occurrence) = keys.get(primary_key)
    {
        ordered_keys.push((primary_key, primary_key_occurrence));
    }
    ordered_keys.extend(
        keys.iter()
            .filter(|(key, _)| Some(key.as_str()) != primary_key)
            .map(|(key, child_occurrence)| (key.as_str(), child_occurrence)),
    );
    for (key, child) in ordered_keys {
        let child_name = class_name(key);
        let (mut child_classes, field) =
            build_field(compiled, child, key, &child_name, primary_key == Some(key));
        classes.append(&mut child_classes);
        fields.push(field);
    }
    ClassPlan::Model(ModelPlan {
        name: name.to_owned(),
        base: "AvdModel".to_owned(),
        description: Some("Subclass of AvdModel.".to_owned()),
        classes,
        fields,
        class_vars: Vec::new(),
        allow_other_keys: schema.allow_other_keys,
    })
}

fn list_plan(
    name: &str,
    item_name: &str,
    primary_key: Option<&str>,
    description: String,
    item_node: &PythonNode,
) -> ListPlan {
    let primary_key_type = primary_key
        .and_then(|primary_key| item_node.keys.get(primary_key))
        .map_or("str", |node| runtime_type(node.schema_id));
    ListPlan {
        name: name.to_owned(),
        base: primary_key.map_or_else(
            || format!("AvdList[{item_name}]"),
            |_| format!("AvdIndexedList[{primary_key_type}, {item_name}]"),
        ),
        item_type: item_name.to_owned(),
        primary_key: primary_key.map(field_name),
        description: Some(description),
    }
}

fn list_description(primary_key: Option<&str>, item_name: &str, item_node: &PythonNode) -> String {
    match primary_key {
        Some(primary_key) => {
            let primary_key_type = item_node
                .keys
                .get(primary_key)
                .map_or("str", |node| runtime_type(node.schema_id));
            format!(
                "Subclass of AvdIndexedList with `{item_name}` items. Primary key is `{}` (`{primary_key_type}`).",
                field_name(primary_key)
            )
        }
        None => format!("Subclass of AvdList with `{item_name}` items."),
    }
}

fn field_plan(
    compiled: &CompiledStore,
    schema_id: SchemaId,
    key: &str,
    runtime_type: &str,
    type_hint: &str,
    primary_key: bool,
    external_reference: Option<String>,
) -> FieldPlan {
    let common = common(compiled, schema_id);
    let default = common.default.as_ref().map(|value| {
        let rendered = render_default(value);
        match schema_id {
            SchemaId::List(_) => format!("lambda cls: coerce_type({rendered}, target_type=cls)"),
            SchemaId::Dict(_)
                if type_hint
                    .chars()
                    .next()
                    .is_some_and(|character| character.is_ascii_uppercase()) =>
            {
                format!("lambda cls: coerce_type({rendered}, target_type=cls)")
            }
            _ => rendered,
        }
    });
    let description = match schema_id {
        SchemaId::Dict(_) | SchemaId::List(_) => Some(combine_descriptions(
            common.description.as_deref(),
            &auto_description(compiled, schema_id, type_hint),
        )),
        _ => common.description.clone(),
    };
    FieldPlan {
        name: field_name(key),
        key: schema_key(key),
        runtime_type: runtime_type.to_owned(),
        type_hint: type_hint.to_owned(),
        optional: !common.required && !primary_key,
        default,
        description,
        external_reference,
    }
}

fn combine_descriptions(schema_description: Option<&str>, model_description: &str) -> String {
    match schema_description {
        Some(description) => format!("{description}\n\n{model_description}"),
        None => model_description.to_owned(),
    }
}

fn auto_description(compiled: &CompiledStore, schema_id: SchemaId, type_name: &str) -> String {
    match schema_id {
        SchemaId::Dict(_) => "Subclass of AvdModel.".to_owned(),
        SchemaId::List(index) => {
            let schema = &compiled.lists[index as usize];
            if let Some(primary_key) = &schema.primary_key {
                format!(
                    "Subclass of AvdIndexedList with `{type_name}Item` items. Primary key is `{primary_key}` (`str`).",
                )
            } else {
                let item = schema.items.map_or("Any", runtime_type);
                format!("Subclass of AvdList with `{item}` items.")
            }
        }
        _ => String::new(),
    }
}

fn common(store: &CompiledStore, schema_id: SchemaId) -> &crate::compiled::Common {
    match schema_id {
        SchemaId::Bool(index) => &store.bools[index as usize].common,
        SchemaId::Int(index) => &store.ints[index as usize].common,
        SchemaId::Str(index) => &store.strings[index as usize].common,
        SchemaId::List(index) => &store.lists[index as usize].common,
        SchemaId::Dict(index) => &store.dicts[index as usize].common,
    }
}

fn is_removed_common(common: &crate::compiled::Common) -> bool {
    common
        .deprecation
        .as_ref()
        .is_some_and(|deprecation| deprecation.removed)
}

fn runtime_type(schema_id: SchemaId) -> &'static str {
    match schema_id {
        SchemaId::Bool(_) => "bool",
        SchemaId::Int(_) => "int",
        SchemaId::Str(_) => "str",
        SchemaId::List(_) => "list",
        SchemaId::Dict(_) => "dict",
    }
}

fn render_default(value: &CompiledValue) -> String {
    match value {
        CompiledValue::Null => "None".to_owned(),
        CompiledValue::Bool(value) => if *value { "True" } else { "False" }.to_owned(),
        CompiledValue::I64(value) => value.to_string(),
        CompiledValue::U64(value) => value.to_string(),
        CompiledValue::String(value) => format!("\"{value}\""),
        CompiledValue::List(values) => format!(
            "[{}]",
            values
                .iter()
                .map(render_default)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        CompiledValue::Object(values) => format!(
            "{{{}}}",
            values
                .iter()
                .map(|(key, child)| format!("\"{key}\": {}", render_default(child)))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

#[derive(Default)]
/// Imports selected from the completed rendering plan.
struct ImportSet {
    class_var: bool,
    literal: bool,
    avd_model: bool,
    avd_list: bool,
    avd_indexed_list: bool,
    eos_designs: bool,
    eos_cli: bool,
    eos_designs_root: bool,
    eos_cli_root: bool,
    protocol: bool,
    coerce_type: bool,
}

fn render_imports(output: &mut String, imports: &ImportSet) {
    let mut regular_imports = Vec::new();
    if imports.eos_cli {
        regular_imports.push("from pyavd._eos_cli_config_gen.schema import EosCliConfigGen");
    }
    if imports.eos_designs {
        regular_imports.push("from pyavd._eos_designs.schema import EosDesigns");
    }
    if imports.coerce_type {
        regular_imports.push("from pyavd._schema.coerce_type import coerce_type");
    }
    if imports.eos_cli_root {
        regular_imports.push(
            "from pyavd._schema.models.eos_cli_config_gen_root_model import EosCliConfigGenRootModel",
        );
    }
    if imports.eos_designs_root {
        regular_imports
            .push("from pyavd._schema.models.eos_designs_root_model import EosDesignsRootModel");
    }
    if imports.class_var {
        regular_imports.push("from typing import ClassVar");
    }
    if imports.literal {
        regular_imports.push("from typing import Literal, TypeAlias");
    }
    if imports.protocol {
        regular_imports.push("from typing import Protocol");
    }
    regular_imports.push("from typing import TYPE_CHECKING");
    regular_imports.sort_unstable();
    for import in regular_imports {
        output.push_str(import);
        output.push('\n');
    }
    output.push_str("\n\n");
    if imports.avd_indexed_list {
        output.push_str("from pyavd._schema.models.avd_indexed_list import AvdIndexedList\n");
    }
    if imports.avd_list {
        output.push_str("from pyavd._schema.models.avd_list import AvdList\n");
    }
    if imports.avd_model {
        output.push_str("from pyavd._schema.models.avd_model import AvdModel\n");
    }
    output.push_str("\nif TYPE_CHECKING:\n");
    output.push_str("    from pyavd._utils import Undefined, UndefinedType\n");
}

fn root_base(class_name: &str) -> &str {
    match class_name {
        "EosCliConfigGen" => "EosCliConfigGenRootModel",
        "EosDesigns" => "EosDesignsRootModel",
        name if name.ends_with("Protocol") => "Protocol",
        _ => "AvdModel",
    }
}

fn class_name(value: &str) -> String {
    value
        .split('_')
        .map(|element| {
            let mut chars = element.chars();
            chars
                .next()
                .map(|first| first.to_uppercase().collect::<String>() + chars.as_str())
                .unwrap_or_default()
        })
        .collect()
}

fn schema_key(value: &str) -> String {
    value.replace(['<', '>'], "").replace('.', "_")
}

fn field_name(value: &str) -> String {
    let key = schema_key(value);
    if is_python_identifier(value) {
        key
    } else {
        format!("field_{key}")
    }
}

fn class_name_from_ref(reference: &str) -> String {
    let (schema_name, path) = reference.split_once('#').unwrap_or((reference, ""));
    let mut parts = vec![class_name(schema_name)];
    let elements = path.split('/').collect::<Vec<_>>();
    for (index, element) in elements.iter().enumerate() {
        if element.is_empty() || matches!(*element, "keys" | "items") {
            continue;
        }
        let suffix = (elements.get(index + 1) == Some(&"items")).then_some("_item");
        parts.push(class_name(&format!(
            "{element}{}",
            suffix.unwrap_or_default()
        )));
    }
    parts.join(".")
}

fn is_python_identifier(value: &str) -> bool {
    !matches!(
        value,
        "False"
            | "None"
            | "True"
            | "and"
            | "as"
            | "assert"
            | "async"
            | "await"
            | "break"
            | "class"
            | "continue"
            | "def"
            | "del"
            | "elif"
            | "else"
            | "except"
            | "finally"
            | "for"
            | "from"
            | "global"
            | "if"
            | "import"
            | "in"
            | "is"
            | "lambda"
            | "nonlocal"
            | "not"
            | "or"
            | "pass"
            | "raise"
            | "return"
            | "try"
            | "while"
            | "with"
            | "yield"
    ) && value.chars().all(|character| {
        character == '_' || character.is_ascii_lowercase() || character.is_ascii_digit()
    })
}

fn render_docstring(output: &mut String, level: usize, value: &str) {
    let formatted = wrap_description(value, 100).join("\n");
    render_preformatted_docstring(output, level, &formatted);
}

fn render_preformatted_docstring(output: &mut String, level: usize, value: &str) {
    if value.contains('\n') {
        line(output, level, "\"\"\"");
        for physical_line in value.split('\n') {
            if physical_line.is_empty() {
                output.push('\n');
            } else {
                line(output, level, physical_line);
            }
        }
        line(output, level, "\"\"\"");
    } else {
        line(output, level, &format!("\"\"\"{value}\"\"\""));
    }
}

fn wrap_description(value: &str, width: usize) -> Vec<String> {
    let mut chunks = Vec::<String>::new();
    for character in value.chars() {
        let whitespace = matches!(
            character,
            '\t' | '\n' | '\u{000b}' | '\u{000c}' | '\r' | ' '
        );
        match chunks.last_mut() {
            Some(chunk)
                if chunk.chars().next().is_some_and(|first| {
                    matches!(first, '\t' | '\n' | '\u{000b}' | '\u{000c}' | '\r' | ' ')
                        == whitespace
                }) =>
            {
                chunk.push(character);
            }
            _ => chunks.push(character.to_string()),
        }
    }

    let mut split_chunks = Vec::new();
    for chunk in chunks {
        if chunk.trim().is_empty() {
            split_chunks.push(chunk);
        } else {
            split_chunks.extend(split_hyphenated_chunk(&chunk));
        }
    }
    let mut remaining_chunks = split_chunks;
    let mut result = Vec::new();
    let mut position = 0;
    while position < remaining_chunks.len() {
        if !result.is_empty() && remaining_chunks[position].trim().is_empty() {
            position += 1;
        }
        let mut line_chunks = Vec::new();
        let mut line_length = 0;
        while position < remaining_chunks.len() {
            let chunk_length = remaining_chunks[position].chars().count();
            if line_length + chunk_length > width {
                break;
            }
            line_length += chunk_length;
            line_chunks.push(remaining_chunks[position].clone());
            position += 1;
        }
        if position < remaining_chunks.len() && remaining_chunks[position].chars().count() > width {
            let available = width.saturating_sub(line_length).max(1);
            let remainder = remaining_chunks[position]
                .chars()
                .skip(available)
                .collect::<String>();
            let prefix = remaining_chunks[position]
                .chars()
                .take(available)
                .collect::<String>();
            line_chunks.push(prefix);
            remaining_chunks[position] = remainder;
        }
        if line_chunks
            .last()
            .is_some_and(|chunk| chunk.trim().is_empty())
        {
            line_chunks.pop();
        }
        if !line_chunks.is_empty() {
            result.push(line_chunks.concat());
        }
    }
    result
}

fn split_hyphenated_chunk(chunk: &str) -> Vec<String> {
    let characters = chunk.char_indices().collect::<Vec<_>>();
    let mut result = Vec::new();
    let mut start = 0;
    for index in 0..characters.len() {
        if characters[index].1 != '-' {
            continue;
        }
        let valid_prefix = (index >= 2
            && characters[index - 2].1.is_alphabetic()
            && characters[index - 1].1.is_alphabetic())
            || (index >= 3
                && characters[index - 3].1.is_alphabetic()
                && characters[index - 2].1 == '-'
                && characters[index - 1].1.is_alphabetic());
        let valid_suffix = index + 2 < characters.len()
            && characters[index + 1].1.is_alphabetic()
            && (characters[index + 2].1.is_alphabetic()
                || (characters[index + 2].1 == '-'
                    && index + 3 < characters.len()
                    && characters[index + 3].1.is_alphabetic()));
        if valid_prefix && valid_suffix {
            let next_index = characters[index + 1].0;
            result.push(chunk.get(start..next_index).unwrap_or_default().to_owned());
            start = next_index;
        }
    }
    if start < chunk.len() {
        result.push(chunk.get(start..).unwrap_or_default().to_owned());
    }
    result
}

fn line(output: &mut String, level: usize, value: &str) {
    let _ = writeln!(output, "{}{value}", "    ".repeat(level));
}
