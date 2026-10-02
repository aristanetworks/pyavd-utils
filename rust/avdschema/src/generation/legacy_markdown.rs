// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Markdown and YAML-example generation for AVD schema documentation.
//!
//! Traversal builds a documentation-specific tree containing only occurrences
//! that can contribute to rendered output. Documentation boundaries are applied
//! during traversal, so schemas below `hide_keys` are never expanded merely to
//! be discarded by the renderer.

#![allow(
    clippy::as_conversions,
    clippy::indexing_slicing,
    clippy::too_many_lines,
    clippy::unreachable,
    reason = "The renderer consumes compiler-validated schema IDs and mirrors the legacy output contract"
)]

use std::collections::BTreeSet;
use std::fmt::Write as _;

use crate::CompileError;
use crate::StoreSource;
use crate::compiled::Common;
use crate::compiled::CompiledDeprecation;
use crate::compiled::CompiledStore;
use crate::compiled::CompiledStringFormat;
use crate::compiled::CompiledValue;
use crate::compiled::SchemaId;
use crate::generation::traversal::SchemaOccurrence;
use crate::generation::traversal::SchemaRelation;
use crate::generation::traversal::SchemaTraverser;
use crate::generation::traversal::SchemaVisitor;
use crate::generation::traversal::TraversalControl;

const LICENSE_HEADER: &str = "Copyright (c) 2026 Arista Networks, Inc.\n\
Use of this source code is governed by the Apache License 2.0\n\
that can be found in the LICENSE file.";

/// Error raised while generating Markdown schema documentation.
#[derive(Debug, derive_more::Display)]
pub enum DocumentationGenerationError {
    /// Schema compilation failed.
    #[display("{_0}")]
    Compile(CompileError),
    /// Documentation generation requires a dictionary root.
    #[display(
        "Schema '{schema_name}' has type '{found}', but documentation generation requires a dictionary root"
    )]
    RootType {
        /// Name of the schema selected for documentation generation.
        schema_name: String,
        /// Model type found at the schema root.
        found: &'static str,
    },
}

impl From<CompileError> for DocumentationGenerationError {
    fn from(value: CompileError) -> Self {
        Self::Compile(value)
    }
}

/// Generate all legacy Markdown schema tables for one named schema.
pub fn generate_markdown_documentation(
    store: &StoreSource,
    schema_name: &str,
) -> Result<Vec<(String, String)>, DocumentationGenerationError> {
    let traverser = SchemaTraverser::compile(store, schema_name)?;
    let mut projection = DocumentationProjection::new(traverser.compiled(), schema_name);
    traverser.traverse(&mut projection)?;
    let root = projection.finish();
    Ok(root
        .descendant_tables(traverser.compiled())
        .into_iter()
        .map(|table| {
            let contents = render_markdown(traverser.compiled(), &root, &table);
            (table, contents)
        })
        .collect())
}

/// Documentation properties retained for one rendered schema occurrence.
#[derive(Debug)]
struct DocNode {
    schema_id: SchemaId,
    key: Option<String>,
    path: Vec<String>,
    table: Option<String>,
    is_primary_key: bool,
    is_unique: bool,
    is_first_list_key: bool,
    children: Vec<DocNode>,
}

impl DocNode {
    fn descendant_tables(&self, compiled: &CompiledStore) -> BTreeSet<String> {
        if self.hide_keys(compiled) {
            return BTreeSet::new();
        }
        let mut tables = BTreeSet::new();
        for child in &self.children {
            if let Some(table) = &child.table {
                tables.insert(table.clone());
            }
            tables.extend(child.descendant_tables(compiled));
        }
        tables
    }

    fn should_render(&self, compiled: &CompiledStore, target_table: &str) -> bool {
        self.path.is_empty()
            || self.table.as_deref() == Some(target_table)
            || self.is_primary_key
            || self.descendant_tables(compiled).contains(target_table)
    }

    fn hide_keys(&self, compiled: &CompiledStore) -> bool {
        common(compiled, self.schema_id)
            .documentation_options
            .as_ref()
            .is_some_and(|options| options.hide_keys)
    }
}

/// Partially built documentation node held while descendants are visited.
#[derive(Debug)]
struct DocDraft {
    schema_id: SchemaId,
    key: Option<String>,
    path: Vec<String>,
    table: Option<String>,
    is_primary_key: bool,
    is_unique: bool,
    is_first_list_key: bool,
}

/// Stack entry used to assemble documentation children in legacy output order.
#[derive(Debug)]
struct DocFrame {
    kind: DocFrameKind,
    children: Vec<DocNode>,
    dynamic_children: Vec<DocNode>,
}

/// Determines whether a visited occurrence is rendered or only supplies list context.
#[derive(Debug)]
enum DocFrameKind {
    Node(DocDraft),
    TransparentListItem {
        path: Vec<String>,
        table: Option<String>,
        primary_key: Option<String>,
        unique_primary_key: bool,
        visited_keys: usize,
    },
    Ignored,
}

impl DocFrame {
    fn node(draft: DocDraft) -> Self {
        Self {
            kind: DocFrameKind::Node(draft),
            children: Vec::new(),
            dynamic_children: Vec::new(),
        }
    }

    fn ignored() -> Self {
        Self {
            kind: DocFrameKind::Ignored,
            children: Vec::new(),
            dynamic_children: Vec::new(),
        }
    }

    fn attach(&mut self, child: DocNode, relation: SchemaRelation<'_>) {
        if matches!(relation, SchemaRelation::DynamicKey(_)) {
            self.dynamic_children.push(child);
        } else {
            self.children.push(child);
        }
    }
}

/// Builds the documentation-specific tree from traversal callbacks.
struct DocumentationProjection<'a> {
    compiled: &'a CompiledStore,
    schema_name: &'a str,
    stack: Vec<DocFrame>,
    root: Option<DocNode>,
}

impl<'a> DocumentationProjection<'a> {
    fn new(compiled: &'a CompiledStore, schema_name: &'a str) -> Self {
        Self {
            compiled,
            schema_name,
            stack: Vec::new(),
            root: None,
        }
    }

    fn finish(self) -> DocNode {
        match self.root {
            Some(root) => root,
            None => unreachable!("traversal always visits and completes the documentation root"),
        }
    }

    fn draft(&mut self, occurrence: &SchemaOccurrence<'_>) -> Option<DocDraft> {
        match occurrence.relation() {
            SchemaRelation::Root => Some(Self::draft_with_context(
                occurrence,
                None,
                Vec::new(),
                None,
                false,
                false,
                false,
            )),
            SchemaRelation::Items => {
                let parent = self.stack.last()?;
                let DocFrameKind::Node(parent) = &parent.kind else {
                    return None;
                };
                Some(Self::draft_with_context(
                    occurrence,
                    None,
                    path_with(&parent.path, "[]"),
                    parent.table.as_deref(),
                    false,
                    false,
                    true,
                ))
            }
            SchemaRelation::Key(name) => {
                let parent = self.stack.last_mut()?;
                match &mut parent.kind {
                    DocFrameKind::Node(parent) => Some(Self::draft_with_context(
                        occurrence,
                        Some(name.to_owned()),
                        path_with(&parent.path, name),
                        parent.table.as_deref(),
                        false,
                        false,
                        false,
                    )),
                    DocFrameKind::TransparentListItem {
                        path,
                        table,
                        primary_key,
                        unique_primary_key,
                        visited_keys,
                    } => {
                        let is_primary_key = primary_key.as_deref() == Some(name);
                        let draft = Self::draft_with_context(
                            occurrence,
                            Some(name.to_owned()),
                            path_with(path, name),
                            table.as_deref(),
                            is_primary_key,
                            is_primary_key && *unique_primary_key,
                            *visited_keys == 0,
                        );
                        *visited_keys += 1;
                        Some(draft)
                    }
                    DocFrameKind::Ignored => None,
                }
            }
            SchemaRelation::DynamicKey(name) => {
                let parent = self.stack.last()?;
                let DocFrameKind::Node(parent) = &parent.kind else {
                    return None;
                };
                let key = format!("<{name}>");
                Some(Self::draft_with_context(
                    occurrence,
                    Some(key.clone()),
                    path_with(&parent.path, &key),
                    parent.table.as_deref(),
                    false,
                    false,
                    false,
                ))
            }
        }
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "Documentation flags are derived independently from list context"
    )]
    fn draft_with_context(
        occurrence: &SchemaOccurrence<'_>,
        key: Option<String>,
        path: Vec<String>,
        parent_table: Option<&str>,
        is_primary_key: bool,
        is_unique: bool,
        is_first_list_key: bool,
    ) -> DocDraft {
        let table = occurrence
            .common()
            .documentation_options
            .as_ref()
            .and_then(|options| options.table.clone())
            .or_else(|| parent_table.map(ToOwned::to_owned))
            .or_else(|| {
                (path.len() == 1).then(|| {
                    key.as_deref()
                        .unwrap_or_default()
                        .replace(['<', '>'], "")
                        .replace('_', "-")
                })
            });
        DocDraft {
            schema_id: occurrence.schema_id(),
            key,
            path,
            table,
            is_primary_key,
            is_unique,
            is_first_list_key,
        }
    }
}

impl SchemaVisitor for DocumentationProjection<'_> {
    type Error = DocumentationGenerationError;

    fn enter(
        &mut self,
        occurrence: &SchemaOccurrence<'_>,
    ) -> Result<TraversalControl, Self::Error> {
        if occurrence.relation() == SchemaRelation::Root && occurrence.dict().is_none() {
            return Err(DocumentationGenerationError::RootType {
                schema_name: self.schema_name.to_owned(),
                found: runtime_type(occurrence.schema_id()),
            });
        }

        if occurrence.relation() == SchemaRelation::Items
            && occurrence.dict().is_some_and(|dict| !dict.keys.is_empty())
        {
            let Some(parent) = self.stack.last() else {
                unreachable!("list items always have a parent occurrence");
            };
            let DocFrameKind::Node(parent) = &parent.kind else {
                unreachable!("a traversed list item always has a rendered list parent");
            };
            let list = &self.compiled.lists[schema_index(parent.schema_id)];
            self.stack.push(DocFrame {
                kind: DocFrameKind::TransparentListItem {
                    path: path_with(&parent.path, "[]"),
                    table: parent.table.clone(),
                    primary_key: list.primary_key.clone(),
                    unique_primary_key: !list.allow_duplicate_primary_key,
                    visited_keys: 0,
                },
                children: Vec::new(),
                dynamic_children: Vec::new(),
            });
        } else if let Some(draft) = self.draft(occurrence) {
            self.stack.push(DocFrame::node(draft));
        } else {
            self.stack.push(DocFrame::ignored());
            return Ok(TraversalControl::SkipChildren);
        }

        let hide_keys = occurrence
            .common()
            .documentation_options
            .as_ref()
            .is_some_and(|options| options.hide_keys);
        Ok(if hide_keys {
            TraversalControl::SkipChildren
        } else {
            TraversalControl::Descend
        })
    }

    fn leave(&mut self, occurrence: &SchemaOccurrence<'_>) -> Result<(), Self::Error> {
        let Some(mut frame) = self.stack.pop() else {
            unreachable!("leave is paired with every successful enter");
        };
        match frame.kind {
            DocFrameKind::Ignored => {}
            DocFrameKind::TransparentListItem { .. } => {
                let Some(parent) = self.stack.last_mut() else {
                    unreachable!("transparent list items always have a parent");
                };
                parent.children.append(&mut frame.children);
            }
            DocFrameKind::Node(draft) => {
                frame.dynamic_children.append(&mut frame.children);
                let node = DocNode {
                    schema_id: draft.schema_id,
                    key: draft.key,
                    path: draft.path,
                    table: draft.table,
                    is_primary_key: draft.is_primary_key,
                    is_unique: draft.is_unique,
                    is_first_list_key: draft.is_first_list_key,
                    children: frame.dynamic_children,
                };
                if let Some(parent) = self.stack.last_mut() {
                    parent.attach(node, occurrence.relation());
                } else {
                    self.root = Some(node);
                }
            }
        }
        Ok(())
    }
}

fn render_markdown(compiled: &CompiledStore, root: &DocNode, table: &str) -> String {
    let mut output = String::from("<!--\n");
    for line in LICENSE_HEADER.lines() {
        let _ = writeln!(output, "  ~ {line}");
    }
    output.push_str("  -->\n=== \"Table\"\n\n");
    output.push_str(&indent(&render_table(compiled, root, table), "    "));
    output.push_str("\n=== \"YAML\"\n\n");
    output.push_str(&indent(&render_yaml(compiled, root, table), "    "));
    output
}

fn render_table(compiled: &CompiledStore, root: &DocNode, table: &str) -> String {
    let mut rows = vec![
        "| Variable | Type | Required | Default | Value Restrictions | Description |".to_owned(),
        "| -------- | ---- | -------- | ------- | ------------------ | ----------- |".to_owned(),
    ];
    render_table_rows(compiled, root, table, &mut rows);
    rows.push(String::new());
    rows.join("\n")
}

fn render_table_rows(
    compiled: &CompiledStore,
    node: &DocNode,
    table: &str,
    rows: &mut Vec<String>,
) {
    if !node.should_render(compiled, table) {
        return;
    }
    if !node.path.is_empty() {
        rows.push(format!(
            "| {} | {} | {} | {} | {} | {} |",
            render_table_key(compiled, node),
            render_table_type(compiled, node),
            render_required(compiled, node),
            render_table_default(compiled, node),
            render_table_restrictions(compiled, node),
            render_table_description(compiled, node),
        ));
    }
    if node.hide_keys(compiled) {
        return;
    }
    for child in &node.children {
        render_table_rows(compiled, child, table, rows);
    }
}

fn render_table_key(compiled: &CompiledStore, node: &DocNode) -> String {
    let indentation_count = node.path.len() * 2 - 2 + usize::from(node.key.is_none()) * 2;
    let indentation = if node.is_first_list_key {
        format!("{}-&nbsp;", "&nbsp;".repeat(indentation_count - 2))
    } else {
        "&nbsp;".repeat(indentation_count)
    };
    let key = node.key.as_deref().map_or_else(
        || format!("&lt;{}&gt;", runtime_type(node.schema_id)),
        |key| key.replace('<', "&lt;").replace('>', "&gt;"),
    );
    format!(
        "[<samp>{indentation}{key}</samp>](## \"{}\"){}",
        node.path.join("."),
        render_deprecation_label(compiled, node)
    )
}

fn render_table_type(compiled: &CompiledStore, node: &DocNode) -> String {
    let schema_id = node.schema_id;
    let mut result = display_type(schema_id).to_owned();
    if let SchemaId::List(index) = schema_id
        && let Some(items) = compiled.lists[index as usize].items
    {
        let _ = write!(result, ", items: {}", display_type(items));
    }
    result
}

fn render_required(compiled: &CompiledStore, node: &DocNode) -> &'static str {
    if node.is_primary_key || common(compiled, node.schema_id).required {
        if node.is_unique {
            "Required, Unique"
        } else {
            "Required"
        }
    } else {
        ""
    }
}

fn render_table_default(compiled: &CompiledStore, node: &DocNode) -> String {
    let Some(default) = common(compiled, node.schema_id).default.as_ref() else {
        return String::new();
    };
    let rendered = render_python_value(default);
    if matches!(default, CompiledValue::List(values) if values.len() > 1 || rendered.len() > 40)
        || matches!(default, CompiledValue::Object(values) if values.len() > 1 || rendered.len() > 40)
    {
        "See (+) on YAML tab".to_owned()
    } else {
        match default {
            CompiledValue::String(value) => format!("`{value}`"),
            _ => format!("`{rendered}`"),
        }
    }
}

fn render_table_restrictions(compiled: &CompiledStore, node: &DocNode) -> String {
    restrictions(compiled, node.schema_id, true).join("<br>")
}

fn render_table_description(compiled: &CompiledStore, node: &DocNode) -> String {
    let common = common(compiled, node.schema_id);
    let mut output = common
        .description
        .as_deref()
        .map(|description| description.replace('\n', "<br>"))
        .unwrap_or_default();
    if let Some(deprecation) = &common.deprecation {
        output.push_str(&render_deprecation_description(deprecation));
    }
    output
}

fn render_deprecation_label(compiled: &CompiledStore, node: &DocNode) -> String {
    common(compiled, node.schema_id)
        .deprecation
        .as_ref()
        .map(|deprecation| {
            let label = if deprecation.removed {
                "removed"
            } else {
                "deprecated"
            };
            format!(" <span style=\"color:red\">{label}</span>")
        })
        .unwrap_or_default()
}

fn render_deprecation_description(deprecation: &CompiledDeprecation) -> String {
    let mut descriptions = Vec::new();
    if deprecation.removed {
        descriptions.push("This key was removed.".to_owned());
    } else {
        descriptions.push("This key is deprecated.".to_owned());
    }
    if let Some(version) = &deprecation.remove_in_version {
        descriptions.push(format!(
            "Support {} removed in AVD version {version}.",
            if deprecation.removed {
                "was"
            } else {
                "will be"
            }
        ));
    } else if let Some(date) = &deprecation.remove_after_date {
        descriptions.push(format!(
            "Support {} removed in the first major AVD version released after {date}.",
            if deprecation.removed {
                "was"
            } else {
                "will be"
            }
        ));
    } else if deprecation.removed {
        descriptions.push("Support was removed in AVD.".to_owned());
    }
    if let Some(new_key) = &deprecation.new_key {
        descriptions.push(format!("Use <samp>{new_key}</samp> instead."));
    }
    if let Some(url) = &deprecation.url {
        descriptions.push(format!("See [here]({url}) for details."));
    }
    format!(
        "<span style=\"color:red\">{}</span>",
        descriptions.join(" ")
    )
}

fn render_yaml(compiled: &CompiledStore, root: &DocNode, table: &str) -> String {
    let mut lines = Vec::new();
    let mut annotations = Vec::new();
    render_yaml_lines(compiled, root, table, &mut lines, &mut annotations);
    let yaml = lines.join("\n").trim().to_owned();
    let mut output = format!("```yaml\n{yaml}\n```\n");
    for (index, annotation) in annotations.iter().enumerate() {
        let _ = write!(
            output,
            "\n{}. Default Value\n\n{}\n",
            index + 1,
            indent(&format!("```yaml\n{annotation}```"), "    ")
        );
    }
    output
}

fn render_yaml_lines(
    compiled: &CompiledStore,
    node: &DocNode,
    table: &str,
    lines: &mut Vec<String>,
    annotations: &mut Vec<String>,
) {
    if is_removed(compiled, node) || !node.should_render(compiled, table) {
        return;
    }
    if !node.path.is_empty() {
        if let Some(description) = common(compiled, node.schema_id).description.as_deref() {
            lines.push(format!("\n{}", render_yaml_description(node, description)));
        }
        if let Some(deprecation) = common(compiled, node.schema_id).deprecation.as_ref() {
            lines.push(render_yaml_deprecation(node, deprecation));
        }
        let annotation_number = default_annotation(compiled, node).map(|annotation| {
            annotations.push(annotation);
            annotations.len()
        });
        lines.push(render_yaml_field(compiled, node, annotation_number));
    }
    if node.hide_keys(compiled) {
        return;
    }
    for child in &node.children {
        render_yaml_lines(compiled, child, table, lines, annotations);
    }
}

fn render_yaml_description(node: &DocNode, description: &str) -> String {
    let indentation = yaml_indentation(node, false);
    description
        .trim()
        .lines()
        .map(|line| {
            if line.is_empty() {
                format!("{indentation}#")
            } else {
                format!("{indentation}# {line}")
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn render_yaml_deprecation(node: &DocNode, deprecation: &CompiledDeprecation) -> String {
    let mut descriptions = vec!["This key is deprecated.".to_owned()];
    if let Some(version) = &deprecation.remove_in_version {
        descriptions.push(format!("Support will be removed in AVD version {version}."));
    } else if let Some(date) = &deprecation.remove_after_date {
        descriptions.push(format!(
            "Support will be removed in the first major AVD version released after {date}."
        ));
    }
    if let Some(new_key) = &deprecation.new_key {
        descriptions.push(format!(
            "Use {} instead.",
            new_key
                .split(" or ")
                .map(|key| format!("`{key}`"))
                .collect::<Vec<_>>()
                .join(" or ")
        ));
    }
    if let Some(url) = &deprecation.url {
        descriptions.push(format!("See [here]({url}) for details."));
    }
    let indentation = yaml_indentation(node, false);
    descriptions
        .into_iter()
        .map(|line| format!("{indentation}# {line}"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn render_yaml_field(
    compiled: &CompiledStore,
    node: &DocNode,
    annotation_number: Option<usize>,
) -> String {
    let schema_id = node.schema_id;
    let indentation = yaml_indentation(node, true);
    let annotation = annotation_number.map_or_else(String::new, |number| format!(" # ({number})!"));
    match schema_id {
        SchemaId::List(index) => {
            let schema = &compiled.lists[index as usize];
            let mut properties = restrictions(compiled, schema_id, false);
            if let Some(default) = inline_default(&schema.common, false) {
                properties.push(default);
            }
            if schema.common.required || node.is_primary_key {
                properties.push(
                    if node.is_unique {
                        "required; unique"
                    } else {
                        "required"
                    }
                    .to_owned(),
                );
            }
            let properties = if properties.is_empty() {
                String::new()
            } else {
                format!(" # {}", properties.join("; "))
            };
            let item_marker = if schema.items.is_none() {
                " <list>"
            } else {
                ""
            };
            format!(
                "{indentation}{}:{item_marker}{properties}{annotation}",
                node.key.as_deref().unwrap_or_default()
            )
        }
        SchemaId::Dict(index) => {
            let schema = &compiled.dicts[index as usize];
            let mut properties = Vec::new();
            if let Some(default) = inline_default(&schema.common, false) {
                properties.push(default);
            }
            if schema.common.required || node.is_primary_key {
                properties.push(
                    if node.is_unique {
                        "required; unique"
                    } else {
                        "required"
                    }
                    .to_owned(),
                );
            }
            let properties = if properties.is_empty() {
                String::new()
            } else {
                format!(" # {}", properties.join("; "))
            };
            let dict_marker = if schema.keys.is_empty() || node.hide_keys(compiled) {
                " <dict>"
            } else {
                ""
            };
            let key = node
                .key
                .as_deref()
                .map_or_else(String::new, |key| format!("{key}:"));
            format!("{indentation}{key}{dict_marker}{properties}{annotation}")
        }
        _ => {
            let mut properties = vec![runtime_type(schema_id).to_owned()];
            properties.extend(restrictions(compiled, schema_id, false));
            if let Some(default) = inline_default(
                common(compiled, schema_id),
                matches!(schema_id, SchemaId::Str(_)),
            ) {
                properties.push(default);
            }
            if common(compiled, schema_id).required || node.is_primary_key {
                properties.push(
                    if node.is_unique {
                        "required; unique"
                    } else {
                        "required"
                    }
                    .to_owned(),
                );
            }
            let key = node
                .key
                .as_deref()
                .map_or_else(String::new, |key| format!("{key}: "));
            format!("{indentation}{key}<{}>{annotation}", properties.join("; "))
        }
    }
}

fn default_annotation(compiled: &CompiledStore, node: &DocNode) -> Option<String> {
    let default = common(compiled, node.schema_id).default.as_ref()?;
    if !needs_annotation(default) {
        return None;
    }
    let key = node.key.as_deref().unwrap_or("null");
    let yaml = render_yaml_mapping(key, default);
    Some(yaml)
}

fn needs_annotation(value: &CompiledValue) -> bool {
    match value {
        CompiledValue::List(values) => values.len() > 1 || render_python_value(value).len() > 40,
        CompiledValue::Object(values) => values.len() > 1 || render_python_value(value).len() > 40,
        _ => false,
    }
}

fn inline_default(common: &Common, quote_string: bool) -> Option<String> {
    let default = common.default.as_ref()?;
    if needs_annotation(default) {
        return None;
    }
    let rendered = match default {
        CompiledValue::String(value) if quote_string => format!("\"{value}\""),
        _ => render_python_value(default),
    };
    Some(format!("default={rendered}"))
}

fn restrictions(store: &CompiledStore, schema_id: SchemaId, markdown: bool) -> Vec<String> {
    let mut result = Vec::new();
    match schema_id {
        SchemaId::Int(index) => {
            let schema = &store.ints[index as usize];
            if markdown {
                if let Some(min) = schema.min {
                    result.push(format!("Min: {min}"));
                }
                if let Some(max) = schema.max {
                    result.push(format!("Max: {max}"));
                }
            } else if let (Some(min), Some(max)) = (schema.min, schema.max) {
                result.push(format!("{min}-{max}"));
            } else if let Some(min) = schema.min {
                result.push(format!(">={min}"));
            } else if let Some(max) = schema.max {
                result.push(format!("<={max}"));
            }
            append_valid_values(
                &mut result,
                schema.dynamic_valid_values.as_deref(),
                schema.valid_values.as_deref(),
                false,
                markdown,
            );
        }
        SchemaId::Str(index) => {
            let schema = &store.strings[index as usize];
            if markdown {
                if let Some(min) = schema.min_length {
                    result.push(format!("Min Length: {min}"));
                }
                if let Some(max) = schema.max_length {
                    result.push(format!("Max Length: {max}"));
                }
                if let Some(format) = schema.format {
                    result.push(format!("Format: {}", display_format(format)));
                }
                if schema.convert_to_lower_case {
                    result.push("Value is converted to lower case.".to_owned());
                }
            } else if let (Some(min), Some(max)) = (schema.min_length, schema.max_length) {
                result.push(format!("length {min}-{max}"));
            } else if let Some(min) = schema.min_length {
                result.push(format!("length >={min}"));
            } else if let Some(max) = schema.max_length {
                result.push(format!("length <={max}"));
            }
            append_valid_values(
                &mut result,
                schema.dynamic_valid_values.as_deref(),
                schema.valid_values.as_deref(),
                true,
                markdown,
            );
            if markdown && let Some(pattern) = &schema.pattern {
                result.push(format!("Pattern: `{pattern}`"));
            }
        }
        SchemaId::List(index) => {
            let schema = &store.lists[index as usize];
            if markdown {
                if let Some(min) = schema.min_length {
                    result.push(format!("Min Length: {min}"));
                }
                if let Some(max) = schema.max_length {
                    result.push(format!("Max Length: {max}"));
                }
            } else if let (Some(min), Some(max)) = (schema.min_length, schema.max_length) {
                result.push(format!("{min}-{max} items"));
            } else if let Some(min) = schema.min_length {
                result.push(format!(">={min} items"));
            } else if let Some(max) = schema.max_length {
                result.push(format!("<={max} items"));
            }
        }
        SchemaId::Bool(_) | SchemaId::Dict(_) => {}
    }
    result
}

fn append_valid_values<T: ToString>(
    output: &mut Vec<String>,
    dynamic: Option<&[String]>,
    values: Option<&[T]>,
    string_values: bool,
    markdown: bool,
) {
    let mut rendered = dynamic
        .unwrap_or_default()
        .iter()
        .map(|value| format!("<value(s) of {value}>"))
        .collect::<Vec<_>>();
    rendered.extend(values.unwrap_or_default().iter().map(ToString::to_string));
    if rendered.is_empty() {
        return;
    }
    if markdown {
        output.push("Valid Values:".to_owned());
        output.extend(
            rendered
                .into_iter()
                .map(|value| format!("- <code>{value}</code>")),
        );
    } else {
        if string_values {
            rendered = rendered
                .into_iter()
                .map(|value| format!("\"{value}\""))
                .collect();
        }
        output.push(rendered.join(" | "));
    }
}

fn render_python_value(value: &CompiledValue) -> String {
    match value {
        CompiledValue::Null => "None".to_owned(),
        CompiledValue::Bool(value) => if *value { "True" } else { "False" }.to_owned(),
        CompiledValue::I64(value) => value.to_string(),
        CompiledValue::U64(value) => value.to_string(),
        CompiledValue::String(value) => format!("'{value}'"),
        CompiledValue::List(values) => format!(
            "[{}]",
            values
                .iter()
                .map(render_python_value)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        CompiledValue::Object(values) => format!(
            "{{{}}}",
            values
                .iter()
                .map(|(key, nested_value)| format!(
                    "'{key}': {}",
                    render_python_value(nested_value)
                ))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

fn render_yaml_mapping(key: &str, value: &CompiledValue) -> String {
    let mut mapping = serde_yaml::Mapping::new();
    mapping.insert(serde_yaml::Value::String(key.to_owned()), yaml_value(value));
    wrap_yaml_plain_scalars(&serde_yaml::to_string(&mapping).unwrap_or_default())
}

fn wrap_yaml_plain_scalars(yaml: &str) -> String {
    let mut output = String::new();
    for line in yaml.split_inclusive('\n') {
        let physical_line = line.trim_end_matches('\n');
        let Some((prefix, value)) = physical_line.split_once(": ") else {
            output.push_str(line);
            continue;
        };
        if physical_line.len() <= 80
            || value.starts_with(['\'', '"', '[', '{', '|', '>'])
            || !value.contains(' ')
        {
            output.push_str(line);
            continue;
        }
        let indentation = physical_line.len() - physical_line.trim_start().len();
        let continuation = " ".repeat(indentation + 2);
        let mut current = format!("{prefix}: ");
        for word in value.split(' ') {
            if current.trim_end().len() > 80 {
                let _ = writeln!(output, "{}", current.trim_end());
                current.clone_from(&continuation);
            }
            current.push_str(word);
            current.push(' ');
        }
        let _ = writeln!(output, "{}", current.trim_end());
    }
    output
}

fn yaml_value(value: &CompiledValue) -> serde_yaml::Value {
    match value {
        CompiledValue::Null => serde_yaml::Value::Null,
        CompiledValue::Bool(value) => serde_yaml::Value::Bool(*value),
        CompiledValue::I64(value) => serde_yaml::Value::Number((*value).into()),
        CompiledValue::U64(value) => serde_yaml::Value::Number((*value).into()),
        CompiledValue::String(value) => serde_yaml::Value::String(value.clone()),
        CompiledValue::List(values) => {
            serde_yaml::Value::Sequence(values.iter().map(yaml_value).collect())
        }
        CompiledValue::Object(values) => serde_yaml::Value::Mapping(
            values
                .iter()
                .map(|(key, nested_value)| {
                    (
                        serde_yaml::Value::String(key.clone()),
                        yaml_value(nested_value),
                    )
                })
                .collect(),
        ),
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

fn is_removed(compiled: &CompiledStore, node: &DocNode) -> bool {
    common(compiled, node.schema_id)
        .deprecation
        .as_ref()
        .is_some_and(|deprecation| deprecation.removed)
}

fn yaml_indentation(node: &DocNode, honor_first_list_key: bool) -> String {
    let count = node.path.len() * 2 - 2 + usize::from(node.key.is_none()) * 2;
    if node.is_first_list_key && honor_first_list_key {
        format!("{}- ", " ".repeat(count - 2))
    } else {
        " ".repeat(count)
    }
}

fn display_type(schema_id: SchemaId) -> &'static str {
    match schema_id {
        SchemaId::Bool(_) => "Boolean",
        SchemaId::Int(_) => "Integer",
        SchemaId::Str(_) => "String",
        SchemaId::List(_) => "List",
        SchemaId::Dict(_) => "Dictionary",
    }
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

fn display_format(format: CompiledStringFormat) -> &'static str {
    match format {
        CompiledStringFormat::Cidr => "cidr",
        CompiledStringFormat::Ip => "ip",
        CompiledStringFormat::IpPool => "ip_pool",
        CompiledStringFormat::Ipv4 => "ipv4",
        CompiledStringFormat::Ipv4Cidr => "ipv4_cidr",
        CompiledStringFormat::Ipv4Pool => "ipv4_pool",
        CompiledStringFormat::Ipv6 => "ipv6",
        CompiledStringFormat::Ipv6Cidr => "ipv6_cidr",
        CompiledStringFormat::Ipv6Pool => "ipv6_pool",
        CompiledStringFormat::Mac => "mac",
    }
}

fn schema_index(schema_id: SchemaId) -> usize {
    match schema_id {
        SchemaId::Bool(index)
        | SchemaId::Int(index)
        | SchemaId::Str(index)
        | SchemaId::List(index)
        | SchemaId::Dict(index) => index as usize,
    }
}

fn path_with(path: &[String], element: &str) -> Vec<String> {
    let mut result = path.to_vec();
    result.push(element.to_owned());
    result
}

fn indent(value: &str, prefix: &str) -> String {
    value
        .split_inclusive('\n')
        .map(|line| {
            if line.trim().is_empty() {
                line.to_owned()
            } else {
                format!("{prefix}{line}")
            }
        })
        .collect()
}
