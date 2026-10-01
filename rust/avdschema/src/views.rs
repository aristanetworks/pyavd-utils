// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Typed borrowed views over the immutable compiled schema store.

use fancy_regex::Regex;

use crate::Store;
use crate::compiled::ArchivedBoolSchema;
use crate::compiled::ArchivedCompiledDeprecation;
use crate::compiled::ArchivedCompiledDocumentationOptions;
use crate::compiled::ArchivedCompiledStringFormat;
use crate::compiled::ArchivedCompiledValue;
use crate::compiled::ArchivedDictSchema;
use crate::compiled::ArchivedIntSchema;
use crate::compiled::ArchivedListSchema;
use crate::compiled::ArchivedSchemaId;
use crate::compiled::ArchivedStrSchema;
use crate::compiled::SchemaId;

/// Internal identifier of one schema node inside an archive.
#[derive(Clone, Copy, Debug)]
struct SchemaCursor<'a> {
    store: &'a Store,
    id: SchemaId,
}

pub(crate) fn schema_view(store: &Store, id: ArchivedSchemaId) -> SchemaView<'_> {
    SchemaCursor {
        store,
        id: native_schema_id(id),
    }
    .view()
}

impl<'a> SchemaCursor<'a> {
    fn view(self) -> SchemaView<'a> {
        match self.id {
            SchemaId::Bool(index) => SchemaView::Bool(BoolView {
                schema: table_get(&self.store.archived().bools, index),
            }),
            SchemaId::Int(index) => SchemaView::Int(IntView {
                schema: table_get(&self.store.archived().ints, index),
            }),
            SchemaId::Str(index) => SchemaView::Str(StrView {
                cursor: self,
                schema: table_get(&self.store.archived().strings, index),
            }),
            SchemaId::List(index) => SchemaView::List(ListView {
                cursor: self,
                schema: table_get(&self.store.archived().lists, index),
            }),
            SchemaId::Dict(index) => SchemaView::Dict(DictView {
                cursor: self,
                schema: table_get(&self.store.archived().dicts, index),
            }),
        }
    }
}

/// Typed borrowed view of one schema node in a compiled store.
#[derive(Clone, Copy, Debug)]
pub enum SchemaView<'a> {
    /// Boolean schema.
    Bool(BoolView<'a>),
    /// Integer schema.
    Int(IntView<'a>),
    /// String schema.
    Str(StrView<'a>),
    /// List schema.
    List(ListView<'a>),
    /// Dictionary schema.
    Dict(DictView<'a>),
}

impl<'a> SchemaView<'a> {
    /// Return properties shared by every schema type.
    pub fn common(self) -> CommonView<'a> {
        match self {
            Self::Bool(view) => view.common(),
            Self::Int(view) => view.common(),
            Self::Str(view) => view.common(),
            Self::List(view) => view.common(),
            Self::Dict(view) => view.common(),
        }
    }

    /// Return whether a value is required at this schema position.
    pub fn required(self) -> bool {
        self.common().required()
    }

    /// Return the effective deprecation metadata, if any.
    pub fn deprecation(self) -> Option<DeprecationView<'a>> {
        self.common().deprecation()
    }

    /// Return a borrowed view of the schema default without materializing it into input data.
    pub fn default(self) -> Option<SchemaValueView<'a>> {
        self.common().default()
    }

    /// Return the display name, if configured.
    pub fn display_name(self) -> Option<&'a str> {
        self.common().display_name()
    }

    /// Return the description, if configured.
    pub fn description(self) -> Option<&'a str> {
        self.common().description()
    }

    /// Return documentation-generation metadata, if configured.
    pub fn documentation_options(self) -> Option<DocumentationOptionsView<'a>> {
        self.common().documentation_options()
    }
}

/// Borrowed view of properties shared by every effective schema node.
#[derive(Clone, Copy, Debug)]
pub struct CommonView<'a>(&'a crate::compiled::ArchivedCommon);

impl<'a> CommonView<'a> {
    /// Return whether a value is required at this schema position.
    pub fn required(self) -> bool {
        self.0.required
    }

    /// Return the effective deprecation metadata, if any.
    pub fn deprecation(self) -> Option<DeprecationView<'a>> {
        self.0.deprecation.as_ref().map(DeprecationView)
    }

    /// Return a borrowed view of the schema default.
    pub fn default(self) -> Option<SchemaValueView<'a>> {
        self.0.default.as_ref().map(SchemaValueView::from)
    }

    /// Return the display name, if configured.
    pub fn display_name(self) -> Option<&'a str> {
        self.0.display_name.as_ref().map(AsRef::as_ref)
    }

    /// Return the description, if configured.
    pub fn description(self) -> Option<&'a str> {
        self.0.description.as_ref().map(AsRef::as_ref)
    }

    /// Return documentation-generation metadata, if configured.
    pub fn documentation_options(self) -> Option<DocumentationOptionsView<'a>> {
        self.0
            .documentation_options
            .as_ref()
            .map(DocumentationOptionsView)
    }
}

/// Borrowed view of documentation-generation controls.
#[derive(Clone, Copy, Debug)]
pub struct DocumentationOptionsView<'a>(&'a ArchivedCompiledDocumentationOptions);

impl<'a> DocumentationOptionsView<'a> {
    /// Return the requested documentation table style, if configured.
    pub fn table(self) -> Option<&'a str> {
        self.0.table.as_ref().map(AsRef::as_ref)
    }

    /// Return whether dictionary keys should be hidden from generated documentation.
    pub fn hide_keys(self) -> bool {
        self.0.hide_keys
    }
}

/// Borrowed JSON-compatible schema value, primarily used for defaults.
#[derive(Clone, Copy, Debug)]
pub enum SchemaValueView<'a> {
    /// JSON null.
    Null,
    /// Boolean value.
    Bool(bool),
    /// Signed integer value.
    I64(i64),
    /// Unsigned integer value.
    U64(u64),
    /// Borrowed string value.
    String(&'a str),
    /// Borrowed list value.
    List(SchemaListValueView<'a>),
    /// Borrowed object value.
    Object(SchemaObjectValueView<'a>),
}

/// Borrowed view of a list-valued schema default.
#[derive(Clone, Copy, Debug)]
pub struct SchemaListValueView<'a>(&'a rkyv::vec::ArchivedVec<ArchivedCompiledValue>);

impl<'a> SchemaListValueView<'a> {
    /// Iterate over the list values without allocating owned values.
    pub fn iter(self) -> impl Iterator<Item = SchemaValueView<'a>> {
        self.0.iter().map(SchemaValueView::from)
    }
}

/// Borrowed view of an object-valued schema default.
#[derive(Clone, Copy, Debug)]
pub struct SchemaObjectValueView<'a>(
    &'a rkyv::vec::ArchivedVec<
        rkyv::tuple::ArchivedTuple2<rkyv::string::ArchivedString, ArchivedCompiledValue>,
    >,
);

impl<'a> SchemaObjectValueView<'a> {
    /// Iterate over borrowed key-value pairs without allocating owned values.
    pub fn iter(self) -> impl Iterator<Item = (&'a str, SchemaValueView<'a>)> {
        self.0
            .iter()
            .map(|entry| (entry.0.as_ref(), SchemaValueView::from(&entry.1)))
    }
}

impl<'a> From<&'a ArchivedCompiledValue> for SchemaValueView<'a> {
    fn from(value: &'a ArchivedCompiledValue) -> Self {
        match value {
            ArchivedCompiledValue::Null => Self::Null,
            ArchivedCompiledValue::Bool(value) => Self::Bool(*value),
            ArchivedCompiledValue::I64(value) => Self::I64(value.to_native()),
            ArchivedCompiledValue::U64(value) => Self::U64(value.to_native()),
            ArchivedCompiledValue::String(value) => Self::String(value.as_ref()),
            ArchivedCompiledValue::List(values) => Self::List(SchemaListValueView(values)),
            ArchivedCompiledValue::Object(values) => Self::Object(SchemaObjectValueView(values)),
        }
    }
}

/// Semantic formats supported by string-schema validation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StringFormatView {
    /// IP network in either address family.
    Cidr,
    /// IP address in either address family.
    Ip,
    /// IP pool in either address family.
    IpPool,
    /// IPv4 address.
    Ipv4,
    /// IPv4 network.
    Ipv4Cidr,
    /// IPv4 pool.
    Ipv4Pool,
    /// IPv6 address.
    Ipv6,
    /// IPv6 network.
    Ipv6Cidr,
    /// IPv6 pool.
    Ipv6Pool,
    /// MAC address.
    Mac,
}

impl From<ArchivedCompiledStringFormat> for StringFormatView {
    fn from(value: ArchivedCompiledStringFormat) -> Self {
        match value {
            ArchivedCompiledStringFormat::Cidr => Self::Cidr,
            ArchivedCompiledStringFormat::Ip => Self::Ip,
            ArchivedCompiledStringFormat::IpPool => Self::IpPool,
            ArchivedCompiledStringFormat::Ipv4 => Self::Ipv4,
            ArchivedCompiledStringFormat::Ipv4Cidr => Self::Ipv4Cidr,
            ArchivedCompiledStringFormat::Ipv4Pool => Self::Ipv4Pool,
            ArchivedCompiledStringFormat::Ipv6 => Self::Ipv6,
            ArchivedCompiledStringFormat::Ipv6Cidr => Self::Ipv6Cidr,
            ArchivedCompiledStringFormat::Ipv6Pool => Self::Ipv6Pool,
            ArchivedCompiledStringFormat::Mac => Self::Mac,
        }
    }
}

/// Borrowed view of effective schema deprecation metadata.
#[derive(Clone, Copy, Debug)]
pub struct DeprecationView<'a>(&'a ArchivedCompiledDeprecation);

impl<'a> DeprecationView<'a> {
    /// Return whether use of this schema position should emit a warning.
    pub fn warning(self) -> bool {
        self.0.warning
    }
    /// Return whether this schema position has been removed.
    pub fn removed(self) -> bool {
        self.0.removed
    }
    /// Return whether the deprecated and replacement keys may coexist.
    pub fn allow_with_new_key(self) -> bool {
        self.0.allow_with_new_key
    }
    /// Return the replacement key, if configured.
    pub fn new_key(self) -> Option<&'a str> {
        self.0.new_key.as_ref().map(AsRef::as_ref)
    }
    /// Return the planned removal version, if configured.
    pub fn remove_in_version(self) -> Option<&'a str> {
        self.0.remove_in_version.as_ref().map(AsRef::as_ref)
    }
    /// Return the planned removal date, if configured.
    pub fn remove_after_date(self) -> Option<&'a str> {
        self.0.remove_after_date.as_ref().map(AsRef::as_ref)
    }
    /// Return the migration documentation URL, if configured.
    pub fn url(self) -> Option<&'a str> {
        self.0.url.as_ref().map(AsRef::as_ref)
    }
    /// Return the upgrade handler name, if configured.
    pub fn upgrade_handler(self) -> Option<&'a str> {
        self.0.upgrade_handler.as_ref().map(AsRef::as_ref)
    }
}

macro_rules! scalar_view {
    ($documentation:literal, $name:ident, $schema:ty) => {
        #[doc = $documentation]
        #[derive(Clone, Copy, Debug)]
        pub struct $name<'a> {
            schema: &'a $schema,
        }

        impl<'a> $name<'a> {
            /// Return properties shared by every schema type.
            pub fn common(self) -> CommonView<'a> {
                CommonView(&self.schema.common)
            }
        }
    };
}

scalar_view!(
    "Borrowed view of a boolean schema.",
    BoolView,
    ArchivedBoolSchema
);
scalar_view!(
    "Borrowed view of an integer schema.",
    IntView,
    ArchivedIntSchema
);

/// Borrowed view of a string schema.
#[derive(Clone, Copy, Debug)]
pub struct StrView<'a> {
    cursor: SchemaCursor<'a>,
    schema: &'a ArchivedStrSchema,
}

impl<'a> StrView<'a> {
    /// Return properties shared by every schema type.
    pub fn common(self) -> CommonView<'a> {
        CommonView(&self.schema.common)
    }
}

impl<'a> IntView<'a> {
    /// Return the inclusive minimum value, if constrained.
    pub fn min(self) -> Option<i64> {
        self.schema.min.as_ref().map(|value| value.to_native())
    }
    /// Return the inclusive maximum value, if constrained.
    pub fn max(self) -> Option<i64> {
        self.schema.max.as_ref().map(|value| value.to_native())
    }
    /// Iterate over statically permitted values, if constrained.
    pub fn valid_values(self) -> Option<impl Iterator<Item = i64> + 'a> {
        self.schema
            .valid_values
            .as_ref()
            .map(|values| values.iter().map(|value| value.to_native()))
    }
    /// Iterate over data paths supplying dynamic permitted values.
    pub fn dynamic_valid_values(self) -> Option<impl Iterator<Item = &'a str> + 'a> {
        self.schema
            .dynamic_valid_values
            .as_ref()
            .map(|values| values.iter().map(AsRef::as_ref))
    }
    /// Iterate over source types accepted for coercion.
    pub fn convert_types(self) -> Option<impl Iterator<Item = &'a str> + 'a> {
        self.schema
            .convert_types
            .as_ref()
            .map(|values| values.iter().map(AsRef::as_ref))
    }
}

impl<'a> StrView<'a> {
    /// Return whether accepted values are converted to lowercase.
    pub fn convert_to_lower_case(self) -> bool {
        self.schema.convert_to_lower_case
    }
    /// Return the minimum string length, if constrained.
    pub fn min_length(self) -> Option<u64> {
        self.schema
            .min_length
            .as_ref()
            .map(|value| value.to_native())
    }
    /// Return the maximum string length, if constrained.
    pub fn max_length(self) -> Option<u64> {
        self.schema
            .max_length
            .as_ref()
            .map(|value| value.to_native())
    }
    /// Return the source regular-expression pattern, if constrained.
    pub fn pattern(self) -> Option<&'a str> {
        self.schema.pattern.as_ref().map(AsRef::as_ref)
    }
    /// Iterate over statically permitted values, if constrained.
    pub fn valid_values(self) -> Option<impl Iterator<Item = &'a str> + 'a> {
        self.schema
            .valid_values
            .as_ref()
            .map(|values| values.iter().map(AsRef::as_ref))
    }
    /// Iterate over data paths supplying dynamic permitted values.
    pub fn dynamic_valid_values(self) -> Option<impl Iterator<Item = &'a str> + 'a> {
        self.schema
            .dynamic_valid_values
            .as_ref()
            .map(|values| values.iter().map(AsRef::as_ref))
    }
    /// Iterate over source types accepted for coercion.
    pub fn convert_types(self) -> Option<impl Iterator<Item = &'a str> + 'a> {
        self.schema
            .convert_types
            .as_ref()
            .map(|values| values.iter().map(AsRef::as_ref))
    }
    /// Return the semantic string format, if constrained.
    pub fn format(self) -> Option<StringFormatView> {
        self.schema.format.as_ref().map(|format| (*format).into())
    }
    /// Return the lazily compiled, fully anchored regular expression.
    ///
    /// Compilation failures are cached in the owning [`Store`].
    pub fn compiled_pattern(self) -> Option<Result<&'a Regex, &'a str>> {
        let pattern = self.pattern()?;
        let SchemaId::Str(index) = self.cursor.id else {
            return None;
        };
        Some(self.cursor.store.pattern(index, pattern))
    }
}

/// Borrowed view of a list schema.
#[derive(Clone, Copy, Debug)]
pub struct ListView<'a> {
    cursor: SchemaCursor<'a>,
    schema: &'a ArchivedListSchema,
}

impl<'a> ListView<'a> {
    /// Return properties shared by every schema type.
    pub fn common(self) -> CommonView<'a> {
        CommonView(&self.schema.common)
    }
    /// Return the schema for each list item, if configured.
    pub fn items(self) -> Option<SchemaView<'a>> {
        self.schema.items.as_ref().map(|id| {
            SchemaCursor {
                store: self.cursor.store,
                id: native_schema_id(*id),
            }
            .view()
        })
    }
    /// Return the minimum list length, if constrained.
    pub fn min_length(self) -> Option<u64> {
        self.schema
            .min_length
            .as_ref()
            .map(|value| value.to_native())
    }
    /// Return the maximum list length, if constrained.
    pub fn max_length(self) -> Option<u64> {
        self.schema
            .max_length
            .as_ref()
            .map(|value| value.to_native())
    }
    /// Return the item key that identifies indexed-list entries, if configured.
    pub fn primary_key(self) -> Option<&'a str> {
        self.schema.primary_key.as_ref().map(AsRef::as_ref)
    }
    /// Iterate over item keys whose values must be unique, if configured.
    pub fn unique_keys(self) -> Option<impl Iterator<Item = &'a str> + 'a> {
        self.schema
            .unique_keys
            .as_ref()
            .map(|values| values.iter().map(AsRef::as_ref))
    }
    /// Return whether duplicate primary-key values are permitted.
    pub fn allow_duplicate_primary_key(self) -> bool {
        self.schema.allow_duplicate_primary_key
    }
}

/// Borrowed view of a dictionary schema.
#[derive(Clone, Copy, Debug)]
pub struct DictView<'a> {
    cursor: SchemaCursor<'a>,
    schema: &'a ArchivedDictSchema,
}

impl<'a> DictView<'a> {
    /// Return properties shared by every schema type.
    pub fn common(self) -> CommonView<'a> {
        CommonView(&self.schema.common)
    }
    /// Return the schema for a statically declared key.
    pub fn key(self, key: &str) -> Option<SchemaView<'a>> {
        self.schema.keys.get(key).map(|id| {
            SchemaCursor {
                store: self.cursor.store,
                id: native_schema_id(*id),
            }
            .view()
        })
    }
    /// Return the schema associated with a dynamic-key source path.
    pub fn dynamic_key(self, path: &str) -> Option<SchemaView<'a>> {
        self.schema.dynamic_keys.get(path).map(|id| {
            SchemaCursor {
                store: self.cursor.store,
                id: native_schema_id(*id),
            }
            .view()
        })
    }
    /// Iterate over statically declared keys in schema order.
    pub fn keys(self) -> impl Iterator<Item = (&'a str, SchemaView<'a>)> + 'a {
        self.schema.keys.iter().map(|(key, id)| {
            let view = SchemaCursor {
                store: self.cursor.store,
                id: native_schema_id(*id),
            }
            .view();
            (key.as_ref(), view)
        })
    }
    /// Iterate over dynamic-key source paths in schema order.
    pub fn dynamic_keys(self) -> impl Iterator<Item = (&'a str, SchemaView<'a>)> + 'a {
        self.schema.dynamic_keys.iter().map(|(key, id)| {
            let view = SchemaCursor {
                store: self.cursor.store,
                id: native_schema_id(*id),
            }
            .view();
            (key.as_ref(), view)
        })
    }
    /// Iterate over default concrete keys for a dynamic-key source path.
    pub fn default_dynamic_keys(self, path: &str) -> Option<impl Iterator<Item = &'a str> + 'a> {
        self.schema
            .default_dynamic_keys
            .get(path)
            .map(|values| values.iter().map(AsRef::as_ref))
    }
    /// Return whether keys without a matching static or dynamic schema are permitted.
    pub fn allow_other_keys(self) -> bool {
        self.schema.allow_other_keys
    }
    /// Return whether this node begins relaxed validation for its descendants.
    pub fn begin_relaxed_validation(self) -> bool {
        self.schema.begin_relaxed_validation
    }
    /// Return whether the dictionary declares any static or dynamic keys.
    pub fn has_schema_keys(self) -> bool {
        !self.schema.keys.is_empty() || !self.schema.dynamic_keys.is_empty()
    }
}

fn native_schema_id(id: ArchivedSchemaId) -> SchemaId {
    match id {
        ArchivedSchemaId::Bool(index) => SchemaId::Bool(index.to_native()),
        ArchivedSchemaId::Int(index) => SchemaId::Int(index.to_native()),
        ArchivedSchemaId::Str(index) => SchemaId::Str(index.to_native()),
        ArchivedSchemaId::List(index) => SchemaId::List(index.to_native()),
        ArchivedSchemaId::Dict(index) => SchemaId::Dict(index.to_native()),
    }
}

fn table_get<T>(table: &[T], index: u32) -> &T {
    match usize::try_from(index)
        .ok()
        .and_then(|index| table.get(index))
    {
        Some(value) => value,
        None => invalid_schema_id(),
    }
}

#[allow(
    clippy::panic,
    reason = "archive integrity is validated before any SchemaView can be created"
)]
fn invalid_schema_id() -> ! {
    panic!("validated schema archive contains an invalid schema id")
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use serde_json::json;

    use super::*;
    fn metadata_store() -> Store {
        Store::from_json(
            &json!({
                "base_str": {
                    "type": "str",
                    "default": "base",
                    "description": "base description",
                    "required": true,
                    "min_length": 2,
                    "max_length": 20,
                    "pattern": "[a-z]+",
                    "format": "mac",
                    "valid_values": ["base", "local"],
                    "dynamic_valid_values": ["choices.names"],
                    "convert_types": ["int"],
                    "documentation_options": {"table": "base table"},
                    "deprecation": {
                        "warning": true,
                        "new_key": "replacement",
                        "allow_with_new_key": true,
                        "removed": false,
                        "remove_in_version": "6.0.0",
                        "remove_after_date": "2030-01-01",
                        "url": "https://example.test",
                        "upgrade_handler": "rename"
                    }
                },
                "base_dict": {
                    "type": "dict",
                    "allow_other_keys": true,
                    "keys": {"inherited": {"type": "bool"}}
                },
                "test": {
                    "type": "dict",
                    "default": {
                        "enabled": true,
                        "names": ["one", "two"],
                        "nothing": null,
                        "negative": -1,
                        "large": 9_223_372_036_854_775_808_u64
                    },
                    "display_name": "Test schema",
                    "documentation_options": {"table": "root", "hide_keys": true},
                    "keys": {
                        "name": {
                            "type": "str",
                            "$ref": "base_str#",
                            "default": "local",
                            "display_name": "Name"
                        },
                        "count": {
                            "type": "int",
                            "min": 1,
                            "max": 10,
                            "valid_values": [1, 2],
                            "dynamic_valid_values": ["counts"],
                            "convert_types": ["str"]
                        },
                        "items": {
                            "type": "list",
                            "min_length": 1,
                            "max_length": 4,
                            "primary_key": "id",
                            "unique_keys": ["name"],
                            "allow_duplicate_primary_key": true,
                            "items": {"type": "dict", "keys": {"id": {"type": "str"}}}
                        },
                        "relaxed": {
                            "type": "dict",
                            "$ref": "base_dict#",
                            "relaxed_validation": true,
                            "keys": {"local": {"type": "bool"}}
                        }
                    },
                    "dynamic_keys": {"names": {"type": "bool"}}
                }
            })
            .to_string(),
        )
        .expect("metadata schema should compile")
    }

    fn assert_default_views(default: SchemaObjectValueView<'_>) {
        let defaults = default.iter().collect::<HashMap<_, _>>();
        assert!(matches!(defaults["enabled"], SchemaValueView::Bool(true)));
        assert!(matches!(defaults["nothing"], SchemaValueView::Null));
        assert!(matches!(defaults["negative"], SchemaValueView::I64(-1)));
        assert!(matches!(
            defaults["large"],
            SchemaValueView::U64(9_223_372_036_854_775_808)
        ));
        let SchemaValueView::List(names) = defaults["names"] else {
            panic!("names default should be a list")
        };
        assert!(matches!(
            names.iter().collect::<Vec<_>>().as_slice(),
            [
                SchemaValueView::String("one"),
                SchemaValueView::String("two")
            ]
        ));
    }

    #[test]
    fn compiled_patterns_preserve_full_match_and_ascii_contract() {
        let store = Store::from_json(
            &json!({
                "alternation": {"type": "str", "pattern": "foo|bar"},
                "perl_classes": {"type": "str", "pattern": r"\d+\s+\d+"},
                "lookahead": {"type": "str", "pattern": "(?=[a-z])(?=.*[0-9])[a-z0-9]+"},
                "variable_lookbehind": {"type": "str", "pattern": "(?<=a+)b"},
                "unicode_property": {"type": "str", "pattern": r"\p{Greek}+"}
            })
            .to_string(),
        )
        .expect("pattern schemas should compile");

        let Some(SchemaView::Str(alternation)) = store.get("alternation") else {
            panic!("alternation schema should compile as a string")
        };
        let alternation = alternation.compiled_pattern().unwrap().unwrap();
        assert!(alternation.is_match("foo").unwrap());
        assert!(alternation.is_match("bar").unwrap());
        assert!(!alternation.is_match("foobar").unwrap());

        for name in ["perl_classes", "lookahead", "variable_lookbehind"] {
            let Some(SchemaView::Str(schema)) = store.get(name) else {
                panic!("{name} schema should compile as a string")
            };
            assert!(schema.compiled_pattern().unwrap().is_ok());
        }

        let Some(SchemaView::Str(unicode_property)) = store.get("unicode_property") else {
            panic!("unicode_property schema should compile as a string")
        };
        assert!(unicode_property.compiled_pattern().unwrap().is_err());
    }

    #[test]
    fn views_expose_effective_metadata() {
        let store = metadata_store();
        let root_view = store.get("test").expect("test root should exist");
        assert_eq!(root_view.display_name(), Some("Test schema"));
        assert_eq!(root_view.description(), None);
        assert!(!root_view.required());
        assert!(root_view.deprecation().is_none());
        assert!(root_view.default().is_some());
        assert_eq!(
            root_view
                .documentation_options()
                .and_then(DocumentationOptionsView::table),
            Some("root")
        );
        let SchemaView::Dict(root) = root_view else {
            panic!("test root should be a dict")
        };
        assert_eq!(root.common().display_name(), Some("Test schema"));
        let docs = root
            .common()
            .documentation_options()
            .expect("root documentation options should exist");
        assert_eq!(docs.table(), Some("root"));
        assert!(docs.hide_keys());
        let SchemaValueView::Object(default) =
            root.common().default().expect("default should exist")
        else {
            panic!("root default should be an object")
        };
        assert_default_views(default);

        let SchemaView::Str(name) = root.key("name").expect("name should exist") else {
            panic!("name should be a string")
        };
        assert!(matches!(
            name.common().default(),
            Some(SchemaValueView::String("local"))
        ));
        assert_eq!(name.common().display_name(), Some("Name"));
        assert_eq!(name.common().description(), Some("base description"));
        assert!(name.common().required());
        assert_eq!(name.min_length(), Some(2));
        assert_eq!(name.max_length(), Some(20));
        assert_eq!(name.pattern(), Some("[a-z]+"));
        assert_eq!(name.format(), Some(StringFormatView::Mac));
        assert_eq!(
            name.valid_values().unwrap().collect::<Vec<_>>(),
            ["base", "local"]
        );
        assert_eq!(
            name.dynamic_valid_values().unwrap().collect::<Vec<_>>(),
            ["choices.names"]
        );
        assert_eq!(name.convert_types().unwrap().collect::<Vec<_>>(), ["int"]);
        let deprecation = name
            .common()
            .deprecation()
            .expect("deprecation should exist");
        assert!(deprecation.warning());
        assert!(deprecation.allow_with_new_key());
        assert!(!deprecation.removed());
        assert_eq!(deprecation.new_key(), Some("replacement"));
        assert_eq!(deprecation.remove_in_version(), Some("6.0.0"));
        assert_eq!(deprecation.remove_after_date(), Some("2030-01-01"));
        assert_eq!(deprecation.url(), Some("https://example.test"));
        assert_eq!(deprecation.upgrade_handler(), Some("rename"));

        let SchemaView::Int(count) = root.key("count").expect("count should exist") else {
            panic!("count should be an integer")
        };
        assert_eq!(count.min(), Some(1));
        assert_eq!(count.max(), Some(10));
        assert_eq!(count.valid_values().unwrap().collect::<Vec<_>>(), [1, 2]);
        assert_eq!(
            count.dynamic_valid_values().unwrap().collect::<Vec<_>>(),
            ["counts"]
        );
        assert_eq!(count.convert_types().unwrap().collect::<Vec<_>>(), ["str"]);

        let SchemaView::List(items) = root.key("items").expect("items should exist") else {
            panic!("items should be a list")
        };
        assert_eq!(items.min_length(), Some(1));
        assert_eq!(items.max_length(), Some(4));
        assert_eq!(items.primary_key(), Some("id"));
        assert_eq!(items.unique_keys().unwrap().collect::<Vec<_>>(), ["name"]);
        assert!(items.allow_duplicate_primary_key());
        assert!(matches!(items.items(), Some(SchemaView::Dict(_))));

        let SchemaView::Dict(relaxed) = root.key("relaxed").expect("relaxed should exist") else {
            panic!("relaxed should be a dict")
        };
        assert!(relaxed.allow_other_keys());
        assert!(relaxed.begin_relaxed_validation());
        assert!(relaxed.key("inherited").is_some());
        assert!(relaxed.key("local").is_some());
    }

    #[test]
    fn string_format_views_cover_the_source_format_contract() {
        let formats = [
            ("cidr", StringFormatView::Cidr),
            ("ip", StringFormatView::Ip),
            ("ip_pool", StringFormatView::IpPool),
            ("ipv4", StringFormatView::Ipv4),
            ("ipv4_cidr", StringFormatView::Ipv4Cidr),
            ("ipv4_pool", StringFormatView::Ipv4Pool),
            ("ipv6", StringFormatView::Ipv6),
            ("ipv6_cidr", StringFormatView::Ipv6Cidr),
            ("ipv6_pool", StringFormatView::Ipv6Pool),
            ("mac", StringFormatView::Mac),
        ];
        let keys = formats
            .iter()
            .map(|(format, _expected)| {
                (
                    (*format).to_owned(),
                    json!({"type": "str", "format": format}),
                )
            })
            .collect::<serde_json::Map<_, _>>();
        let store =
            Store::from_json(&json!({"test": {"type": "dict", "keys": keys}}).to_string()).unwrap();
        let Some(SchemaView::Dict(root)) = store.get("test") else {
            panic!("test root should be a dict")
        };

        for (format, expected) in formats {
            let Some(SchemaView::Str(schema)) = root.key(format) else {
                panic!("format schema should be a string")
            };
            assert_eq!(schema.format(), Some(expected));
        }
    }
}
