// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Field-occurrence projection for the SQLite-backed Schema Explorer.
//!
//! Each use-site receives its own row, even when effective schemas are interned
//! together. Dictionary list items supply path, primary-key, and table context
//! without producing rows themselves. Hidden dictionaries remain visible as
//! leaves, with cross-schema reference links retained from source provenance.
//! Removed fields remain discoverable; consumers can exclude them from examples.
//! Consumers own persistence, including `SQLite` layout and indexes. The Python
//! generation bindings export these rows as JSON Lines without a `SQLite` dependency.

#![allow(
    clippy::as_conversions,
    clippy::indexing_slicing,
    reason = "Table indices are validated by schema compilation"
)]

use serde::Serialize;
use serde_json::Map;
use serde_json::Value;
use serde_json::json;

use crate::CompileError;
use crate::SchemaPathElement;
use crate::StoreSource;
use crate::compiled::CompiledStore;
use crate::compiled::CompiledStringFormat;
use crate::compiled::CompiledValue;
use crate::compiled::SchemaId;
use crate::generation::documentation_context::documentation_table;
use crate::generation::traversal::HiddenKeysPolicy;
use crate::generation::traversal::SchemaOccurrence;
use crate::generation::traversal::SchemaTraverser;
use crate::generation::traversal::SchemaVisitor;
use crate::generation::traversal::TraversalConfig;
use crate::generation::traversal::TraversalControl;

/// One field occurrence in the Schema Explorer's database contract.
///
/// JSON defaults and constraints are values here; the `SQLite` writer encodes
/// them as text. Rows are returned in depth-first schema order, which determines
/// their database IDs and the browser's schema-order presentation.
#[allow(
    clippy::struct_excessive_bools,
    reason = "The database contract exposes independent boolean columns for each field state"
)]
#[derive(Debug, Serialize)]
pub struct SchemaExplorerRow {
    /// Selected schema name, used as the Explorer module identifier.
    pub module: String,
    /// Field path with `[]` for list items and `<path>` for dynamic keys.
    pub key_path: String,
    /// Model type, with the presentation suffix `(deprecated)` when applicable.
    pub var_type: String,
    /// Effective description with surrounding whitespace removed.
    pub description: String,
    /// Effective default; absent defaults become SQL NULL.
    pub default_value: Option<Value>,
    /// Explicit requirement or implicit primary-key requirement.
    pub required: bool,
    /// Whether this is the immediate primary-key field of a list item.
    pub primary_key: bool,
    /// Whether duplicate values of this primary key are prohibited.
    pub unique_key: bool,
    /// Nearest parent field, excluding anonymous list-item schemas.
    pub parent_path: String,
    /// Number of named field segments; placeholders count as one segment.
    pub depth: usize,
    /// Effective documentation-table grouping for this occurrence.
    pub doc_table: String,
    /// Whether a deprecation warning applies to a field that is not removed.
    pub deprecated: bool,
    /// Whether support for this field has been removed.
    pub removed: bool,
    /// Cross-schema reference behind a hidden dictionary boundary.
    pub cross_ref: Option<String>,
    /// Supported restrictions, including item type and restrictions for lists.
    pub constraints: Option<Map<String, Value>>,
}

/// Project named schemas into Explorer rows without writing runtime artifacts.
///
/// Schema names and field insertion order are preserved. Each selected schema
/// is compiled completely before its occurrences are traversed. Hidden-key
/// pruning saves occurrence expansion, not the initial compilation work.
pub fn generate_schema_explorer_rows(
    source: &StoreSource,
    schema_names: &[String],
) -> Result<Vec<SchemaExplorerRow>, CompileError> {
    let mut rows = Vec::new();
    for schema_name in schema_names {
        let traverser = SchemaTraverser::compile(source, schema_name)?;
        let mut projection = ExplorerProjection {
            compiled: traverser.compiled(),
            module: schema_name,
            stack: Vec::new(),
            rows: &mut rows,
        };
        traverser.traverse(
            &mut projection,
            TraversalConfig {
                hidden_keys: HiddenKeysPolicy::Prune,
                include_removed: true,
            },
        )?;
    }
    Ok(rows)
}

/// Context for the children of a visited occurrence, including anonymous items.
#[derive(Debug)]
struct ExplorerFrame {
    path: String,
    parent_field: String,
    depth: usize,
    table: Option<String>,
    schema_id: SchemaId,
    primary_key: Option<String>,
    unique_primary_key: bool,
}

/// Streaming row collector; only ancestor context is retained during traversal.
#[derive(Debug)]
struct ExplorerProjection<'a> {
    compiled: &'a CompiledStore,
    module: &'a str,
    stack: Vec<ExplorerFrame>,
    rows: &'a mut Vec<SchemaExplorerRow>,
}

impl SchemaVisitor for ExplorerProjection<'_> {
    type Error = CompileError;

    #[allow(
        clippy::unreachable,
        reason = "Traversal visits the root first, items only below lists, and static or dynamic keys as children"
    )]
    fn enter(
        &mut self,
        occurrence: &SchemaOccurrence<'_>,
    ) -> Result<TraversalControl, Self::Error> {
        if occurrence.relation().is_none() {
            self.stack.push(ExplorerFrame {
                path: String::new(),
                parent_field: String::new(),
                depth: 0,
                table: documentation_table(occurrence.common(), None, None),
                schema_id: occurrence.schema_id(),
                primary_key: None,
                unique_primary_key: false,
            });
            return Ok(TraversalControl::Descend);
        }
        let Some(parent) = self.stack.last() else {
            unreachable!("a non-root occurrence has an ancestor frame");
        };
        if occurrence.relation() == Some(&SchemaPathElement::Items) {
            let list = match parent.schema_id {
                SchemaId::List(index) => &self.compiled.lists[index as usize],
                _ => unreachable!("items are visited only below a list"),
            };
            self.stack.push(ExplorerFrame {
                path: format!("{}[]", parent.path),
                parent_field: parent.parent_field.clone(),
                depth: parent.depth,
                table: documentation_table(occurrence.common(), parent.table.as_deref(), None),
                schema_id: occurrence.schema_id(),
                primary_key: list.primary_key.clone(),
                unique_primary_key: !list.allow_duplicate_primary_key,
            });
            return Ok(TraversalControl::Descend);
        }
        let name = match occurrence.relation() {
            Some(SchemaPathElement::Key { key }) => key.clone(),
            Some(SchemaPathElement::DynamicKey { source }) => format!("<{}>", source.path()),
            None | Some(SchemaPathElement::Items) => unreachable!("handled above"),
            Some(SchemaPathElement::Definition { .. }) => {
                unreachable!("generation traverses effective children, not definition declarations")
            }
        };
        let path = if parent.path.is_empty() {
            name.clone()
        } else {
            format!("{}.{name}", parent.path)
        };
        let table = documentation_table(
            occurrence.common(),
            parent.table.as_deref(),
            (parent.depth == 0).then_some(name.as_str()),
        );
        self.rows.push(field_row(
            self.compiled,
            self.module,
            parent,
            occurrence,
            &name,
            &path,
            table.as_deref().unwrap_or_default(),
        ));
        self.stack.push(ExplorerFrame {
            path: path.clone(),
            parent_field: path,
            depth: parent.depth + 1,
            table,
            schema_id: occurrence.schema_id(),
            primary_key: None,
            unique_primary_key: false,
        });
        Ok(TraversalControl::Descend)
    }

    fn leave(&mut self, _occurrence: &SchemaOccurrence<'_>) -> Result<(), Self::Error> {
        self.stack.pop();
        Ok(())
    }
}

/// Project effective properties and ancestor context into one named field row.
fn field_row(
    compiled: &CompiledStore,
    module: &str,
    parent: &ExplorerFrame,
    occurrence: &SchemaOccurrence<'_>,
    name: &str,
    path: &str,
    table: &str,
) -> SchemaExplorerRow {
    let common = occurrence.common();
    let primary_key = parent.primary_key.as_deref() == Some(name);
    let removed = common
        .deprecation
        .as_ref()
        .is_some_and(|deprecation| deprecation.removed);
    let deprecated = !removed
        && common
            .deprecation
            .as_ref()
            .is_some_and(|deprecation| deprecation.warning);
    let mut var_type = type_name(occurrence.schema_id()).to_owned();
    if common.deprecation.is_some() {
        var_type.push_str("(deprecated)");
    }
    let hidden = common
        .documentation_options
        .as_ref()
        .is_some_and(|options| options.hide_keys);
    let cross_ref = if hidden {
        occurrence
            .references()
            .iter()
            .find(|reference| reference.schema_name() != module)
            .map(crate::SchemaPath::to_reference)
    } else {
        None
    };
    let mut constraints = restrictions(compiled, occurrence.schema_id());
    if let Some(items) = occurrence.list().and_then(|list| list.items) {
        let mut item_constraints = Map::new();
        item_constraints.insert("type".to_owned(), json!(type_name(items)));
        item_constraints.extend(restrictions(compiled, items));
        constraints.insert("items".to_owned(), Value::Object(item_constraints));
    }
    SchemaExplorerRow {
        module: module.to_owned(),
        key_path: path.to_owned(),
        var_type,
        description: common
            .description
            .as_deref()
            .unwrap_or_default()
            .trim()
            .to_owned(),
        default_value: common.default.as_ref().map(json_value),
        required: common.required || primary_key,
        primary_key,
        unique_key: primary_key && parent.unique_primary_key,
        parent_path: parent.parent_field.clone(),
        depth: parent.depth + 1,
        doc_table: table.to_owned(),
        deprecated,
        removed,
        cross_ref,
        constraints: (!constraints.is_empty()).then_some(constraints),
    }
}

/// Select the restrictions supported by the Explorer database format.
fn restrictions(compiled: &CompiledStore, schema_id: SchemaId) -> Map<String, Value> {
    let mut result = Map::new();
    macro_rules! property {
        ($schema:ident, $field:ident) => {
            if let Some(value) = &$schema.$field {
                result.insert(stringify!($field).to_owned(), json!(value));
            }
        };
    }
    match schema_id {
        SchemaId::Int(index) => {
            let schema = &compiled.ints[index as usize];
            property!(schema, valid_values);
            property!(schema, min);
            property!(schema, max);
            property!(schema, convert_types);
        }
        SchemaId::Str(index) => {
            let schema = &compiled.strings[index as usize];
            property!(schema, valid_values);
            property!(schema, min_length);
            property!(schema, max_length);
            property!(schema, pattern);
            if let Some(format) = schema.format {
                result.insert("format".to_owned(), json!(format_name(format)));
            }
            property!(schema, convert_types);
        }
        SchemaId::List(index) => {
            let schema = &compiled.lists[index as usize];
            property!(schema, min_length);
            property!(schema, max_length);
        }
        SchemaId::Bool(_) | SchemaId::Dict(_) => {}
    }
    result
}

fn type_name(schema_id: SchemaId) -> &'static str {
    match schema_id {
        SchemaId::Bool(_) => "bool",
        SchemaId::Int(_) => "int",
        SchemaId::Str(_) => "str",
        SchemaId::List(_) => "list",
        SchemaId::Dict(_) => "dict",
    }
}

fn format_name(format: CompiledStringFormat) -> &'static str {
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

fn json_value(value: &CompiledValue) -> Value {
    match value {
        CompiledValue::Null => Value::Null,
        CompiledValue::Bool(value) => json!(value),
        CompiledValue::I64(value) => json!(value),
        CompiledValue::U64(value) => json!(value),
        CompiledValue::String(value) => json!(value),
        CompiledValue::List(values) => Value::Array(values.iter().map(json_value).collect()),
        CompiledValue::Object(values) => Value::Object(
            values
                .iter()
                .map(|(name, field_value)| (name.clone(), json_value(field_value)))
                .collect(),
        ),
    }
}
