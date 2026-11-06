// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Typed addresses of schema declarations and effective schema occurrences.
//!
//! These addresses describe schema edges, not paths through input data. A declaration
//! reference and an inherited use-site can share the representation without identifying
//! the same location. Paths are not model identities: multiple paths may resolve to one
//! interned schema ID. Parsing checks reference syntax; resolution checks node existence
//! and whether each edge is valid for the current schema type.

#![deny(missing_docs)]

use std::fmt;
use std::str::FromStr;

use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;
use serde::de::Visitor;

use crate::DynamicKeySource;
use crate::UnsupportedDynamicKeySource;

/// One edge in a schema address. Dictionary container names are structural, not keys.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SchemaPathElement {
    /// A statically named dictionary child, including keys literally named `keys` or `items`.
    Key {
        /// Name of the child.
        key: String,
    },
    /// A root dynamic-key declaration, not one of its concrete input key names.
    DynamicKey {
        /// Declaration supplying the dynamic schema.
        source: DynamicKeySource,
    },
    /// The item schema of a list, without an input list index.
    Items,
    /// A reusable definition on a named source-schema root.
    Definition {
        /// Name under `$defs`.
        name: String,
    },
}

/// Address of a schema declaration or effective occurrence in a named schema.
///
/// Display and Serde use the absolute AVD `$ref` string syntax (`schema#/keys/name`).
/// Deserialization checks the path grammar without resolving a schema store. Existence,
/// node-type compatibility, and reference cycles are checked when compiling source schemas.
/// Names are literal strings and are not checked against metaschema naming conventions.
/// Parsed names must be nonempty. The wire format does not escape `/` or `#` within key
/// or definition names.
///
/// ```
/// use avdschema::{SchemaPath, SchemaPathElement};
///
/// let path: SchemaPath = "avd_design#/keys/devices/items".parse()?;
/// assert_eq!(path.schema_name(), "avd_design");
/// assert_eq!(path.elements().last(), Some(&SchemaPathElement::Items));
/// assert_eq!(path.to_string(), "avd_design#/keys/devices/items");
/// # Ok::<(), avdschema::SchemaPathParseError>(())
/// ```
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SchemaPath {
    schema: String,
    elements: Vec<SchemaPathElement>,
}

impl SchemaPath {
    /// Address the root of a named schema without parsing a reference string.
    pub fn root(schema: impl Into<String>) -> Self {
        Self {
            schema: schema.into(),
            elements: Vec::new(),
        }
    }

    /// Name identifying the source schema, before store alias resolution.
    #[cfg_attr(
        feature = "metaschema",
        allow(
            clippy::same_name_method,
            reason = "JsonSchema::schema_name names the Rust type; this accessor names the addressed AVD schema"
        )
    )]
    pub fn schema_name(&self) -> &str {
        &self.schema
    }

    /// Structural edges from the named root to this location.
    pub fn elements(&self) -> &[SchemaPathElement] {
        &self.elements
    }

    /// Return a child address without changing its parent.
    #[must_use]
    pub fn child(&self, element: SchemaPathElement) -> Self {
        let mut child = self.clone();
        child.elements.push(element);
        child
    }
}

impl fmt::Display for SchemaPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}#{}", self.schema, PathSuffix(&self.elements))
    }
}

impl Serialize for SchemaPath {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

impl<'de> Deserialize<'de> for SchemaPath {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_str(SchemaPathVisitor)
    }
}

/// Decode the wire string directly, without retaining a second unparsed representation.
struct SchemaPathVisitor;

impl Visitor<'_> for SchemaPathVisitor {
    type Value = SchemaPath;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("an absolute AVD schema reference string")
    }

    fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
        let reference = v;
        reference.parse().map_err(|error| match error {
            SchemaPathParseError::InvalidSyntax { .. } => E::custom(error),
            _ => E::custom(format_args!("Invalid schema $ref '{reference}': {error}")),
        })
    }
}

#[cfg(feature = "metaschema")]
impl schemars::JsonSchema for SchemaPath {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        "SchemaPath".into()
    }

    fn inline_schema() -> bool {
        true
    }

    fn json_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // Authoring tools see the wire string, never the internal path fields. This naming
        // pattern is authoring policy; runtime parsing checks delimiters and structural edges.
        schemars::json_schema!({
            "type": "string",
            "pattern": r"^[a-z][a-z_]*#(/[a-z$][\.a-z0-9_]*)*$",
        })
    }
}

struct PathSuffix<'a>(&'a [SchemaPathElement]);

impl fmt::Display for PathSuffix<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for element in self.0 {
            match element {
                SchemaPathElement::Key { key } => write!(f, "/keys/{key}")?,
                SchemaPathElement::DynamicKey { source } => {
                    write!(f, "/dynamic_keys/{}", source.path())?;
                }
                SchemaPathElement::Items => f.write_str("/items")?,
                SchemaPathElement::Definition { name } => write!(f, "/$defs/{name}")?,
            }
        }
        Ok(())
    }
}

/// Failure to decode the wire syntax of a schema reference into structural steps.
#[derive(Clone, Debug, derive_more::Display)]
pub enum SchemaPathParseError {
    /// The reference does not follow the absolute AVD reference grammar.
    #[display("Invalid syntax for schema $ref '{reference}'.")]
    InvalidSyntax {
        /// Unmodified authored reference.
        reference: String,
    },
    /// A component is not a supported structural edge.
    #[display("Invalid schema path element '{element}'.")]
    InvalidElement {
        /// Unexpected structural component.
        element: String,
    },
    /// A container was selected without naming a schema within it.
    #[display("A schema path cannot end at the '{mapping}' mapping.")]
    IncompleteMapping {
        /// Container missing its child name.
        mapping: String,
    },
    /// A dynamic declaration names an unsupported source.
    #[display("{_0}")]
    UnsupportedDynamicSource(UnsupportedDynamicKeySource),
}

impl std::error::Error for SchemaPathParseError {}

impl FromStr for SchemaPath {
    type Err = SchemaPathParseError;

    fn from_str(reference: &str) -> Result<Self, Self::Err> {
        let invalid = || SchemaPathParseError::InvalidSyntax {
            reference: reference.to_owned(),
        };
        let (schema, suffix) = reference.split_once('#').ok_or_else(invalid)?;
        // Validate delimiters, not naming conventions. Source dictionary keys are ordinary
        // strings, including historical names excluded from the authoring metaschema.
        if schema.is_empty()
            || suffix.contains('#')
            || (!suffix.is_empty() && !suffix.starts_with('/'))
        {
            return Err(invalid());
        }
        let mut components = suffix.split('/').skip(1);
        if components.clone().any(str::is_empty) {
            return Err(invalid());
        }
        let mut path = Self::root(schema);
        while let Some(component) = components.next() {
            let element = match component {
                "items" => SchemaPathElement::Items,
                "keys" | "dynamic_keys" | "$defs" => {
                    let name = components.next().ok_or_else(|| {
                        SchemaPathParseError::IncompleteMapping {
                            mapping: component.to_owned(),
                        }
                    })?;
                    match component {
                        "keys" => SchemaPathElement::Key {
                            key: name.to_owned(),
                        },
                        "$defs" => SchemaPathElement::Definition {
                            name: name.to_owned(),
                        },
                        _ => SchemaPathElement::DynamicKey {
                            source: DynamicKeySource::try_from(name)
                                .map_err(SchemaPathParseError::UnsupportedDynamicSource)?,
                        },
                    }
                }
                _ => {
                    return Err(SchemaPathParseError::InvalidElement {
                        element: component.to_owned(),
                    });
                }
            };
            path.elements.push(element);
        }
        Ok(path)
    }
}

#[cfg(test)]
mod tests {
    use super::SchemaPath;
    use super::SchemaPathElement;
    use super::SchemaPathParseError;
    use crate::DynamicKeySource;
    use crate::Load as _;
    use crate::StoreSource;

    #[test]
    fn schema_references_serialize_as_strings_and_deserialize_as_typed_paths() {
        for reference in [
            "schema#",
            "schema#/keys/keys/keys/items/items/keys/value",
            "schema#/$defs/definition/items",
            "schema#/dynamic_keys/node_type_keys.key/keys/name",
        ] {
            let json = serde_json::to_string(reference).unwrap();
            let path: SchemaPath = serde_json::from_str(&json).unwrap();
            assert_eq!(path.to_string(), reference);
            assert_eq!(serde_json::to_string(&path).unwrap(), json);
        }

        let json = serde_json::json!({
            "model": {"type": "dict", "$ref": "shared#/keys/settings"},
            "shared": {"type": "dict", "keys": {"settings": {"type": "dict"}}}
        });
        let source = StoreSource::from_json(&json.to_string()).unwrap();
        let reference = source
            .get("model")
            .unwrap()
            .base
            .schema_ref
            .as_ref()
            .unwrap();
        assert_eq!(reference.schema_name(), "shared");
        assert_eq!(
            reference.elements(),
            &[SchemaPathElement::Key {
                key: "settings".into()
            }]
        );
        assert_eq!(serde_json::to_value(source).unwrap(), json);
    }

    #[test]
    fn source_loading_reports_invalid_references_even_in_unused_definitions() {
        for (reference, reason) in [
            (
                "#/keys/key2",
                "Invalid syntax for schema $ref '#/keys/key2'.",
            ),
            (
                "eos_cli_config_gen",
                "Invalid syntax for schema $ref 'eos_cli_config_gen'.",
            ),
            (
                "model/keys/name",
                "Invalid syntax for schema $ref 'model/keys/name'.",
            ),
            (
                "model#/keys",
                "A schema path cannot end at the 'keys' mapping.",
            ),
            (
                "model#/unknown/value",
                "Invalid schema path element 'unknown'.",
            ),
            (
                "model#/dynamic_keys/unknown.key",
                "Unsupported dynamic-key source 'unknown.key'",
            ),
        ] {
            let json = serde_json::json!({
                "model": {"type": "dict", "$defs": {"unused": {"type": "str", "$ref": reference}}}
            });
            let error = StoreSource::from_json(&json.to_string())
                .unwrap_err()
                .to_string();
            assert!(error.contains(reference), "{error}");
            assert!(error.contains(reason), "{error}");
        }
    }

    #[test]
    fn source_loading_rejects_non_string_reference_values() {
        let error = StoreSource::from_json(r#"{"model":{"type":"dict","$ref":42}}"#)
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("expected an absolute AVD schema reference string"),
            "{error}"
        );
    }

    #[cfg(feature = "metaschema")]
    #[test]
    fn reference_metaschema_describes_the_wire_string_not_path_fields() {
        let schema = schemars::schema_for!(SchemaPath);
        let value = serde_json::to_value(schema).unwrap();
        assert_eq!(value["type"], "string");
        assert_eq!(value["pattern"], r"^[a-z][a-z_]*#(/[a-z$][\.a-z0-9_]*)*$");
        assert!(value.get("properties").is_none());
    }

    #[cfg(feature = "yaml")]
    #[test]
    fn yaml_reference_loading_uses_the_same_string_contract() {
        let yaml = "model:\n  type: dict\n  $ref: shared#/keys/settings\n";
        let source: StoreSource = yaml_parser::serde::from_str(yaml).unwrap();
        let serialized = yaml_parser::serde::to_string(&source).unwrap();
        let reloaded: StoreSource = yaml_parser::serde::from_str(&serialized).unwrap();
        assert_eq!(source, reloaded);
        let error = yaml_parser::serde::from_str::<StoreSource>(
            &yaml.replace("shared#/keys/settings", "shared#/keys"),
        )
        .unwrap_err()
        .to_string();
        assert!(error.contains("shared#/keys"), "{error}");
        assert!(
            error.contains("cannot end at the 'keys' mapping"),
            "{error}"
        );
    }

    #[test]
    fn authored_references_round_trip_without_confusing_literal_and_structural_names() {
        for reference in [
            "schema#",
            "schema#/keys/keys/keys/items/items/keys/value",
            "schema#/$defs/definition/items",
            "schema#/dynamic_keys/node_type_keys.key/keys/name",
        ] {
            let path: SchemaPath = reference.parse().unwrap();
            assert_eq!(path.to_string(), reference);
        }
        let path: SchemaPath = "schema#/keys/keys/keys/items/items".parse().unwrap();
        assert_eq!(path.schema_name(), "schema");
        assert_eq!(
            path.elements(),
            &[
                SchemaPathElement::Key { key: "keys".into() },
                SchemaPathElement::Key {
                    key: "items".into()
                },
                SchemaPathElement::Items,
            ]
        );
    }

    #[test]
    fn programmatic_children_keep_their_parent_and_dynamic_source_identity() {
        let root = SchemaPath::root("schema");
        let declaration = root.child(SchemaPathElement::DynamicKey {
            source: DynamicKeySource::NetworkServices,
        });
        assert!(root.elements().is_empty());
        assert_eq!(
            declaration.to_string(),
            "schema#/dynamic_keys/network_services_keys.name"
        );
        assert_eq!(
            declaration.to_string().parse::<SchemaPath>().unwrap(),
            declaration
        );
        assert_ne!(
            declaration,
            root.child(SchemaPathElement::Key {
                key: "network_services_keys.name".into()
            })
        );
    }

    #[test]
    fn programmatic_reference_names_round_trip_without_authoring_spelling_checks() {
        for schema in ["Schema", "schema1", "shared-schema", "共有"] {
            for name in [
                "Vxlan1",
                "MIB_family_name",
                "1name",
                "na-me",
                "a$b",
                "name\n",
                "na~1me",
                "é",
            ] {
                for element in [
                    SchemaPathElement::Key { key: name.into() },
                    SchemaPathElement::Definition { name: name.into() },
                ] {
                    let path = SchemaPath::root(schema).child(element);
                    assert_eq!(path.to_string().parse::<SchemaPath>().unwrap(), path);
                    let path_json = serde_json::to_string(&path).unwrap();
                    assert_eq!(
                        serde_json::from_str::<SchemaPath>(&path_json).unwrap(),
                        path
                    );

                    let mut dict = crate::dict::SourceDict::default();
                    dict.base.schema_ref = Some(path);
                    let source = crate::any::SourceSchema::Dict(dict);
                    let source_json = serde_json::to_string(&source).unwrap();
                    assert_eq!(
                        serde_json::from_str::<crate::any::SourceSchema>(&source_json).unwrap(),
                        source
                    );
                }
            }
        }
    }

    #[test]
    fn source_loading_and_compilation_accept_legacy_reference_names() {
        for reference in [
            "SharedSchema1#/keys/Vxlan1",
            "SharedSchema1#/$defs/SharedType",
        ] {
            let json = serde_json::json!({
                "Model1": {"type": "dict", "keys": {"copy": {"type": "str", "$ref": reference}}},
                "SharedSchema1": {"type": "dict", "keys": {"Vxlan1": {"type": "str"}}, "$defs": {"SharedType": {"type": "str"}}}
            });
            let source = StoreSource::from_json(&json.to_string()).unwrap();
            crate::Store::compile(&source).unwrap();
        }
    }

    #[test]
    fn malformed_reference_syntax_is_rejected_at_the_boundary() {
        for reference in [
            "schema",
            "#",
            "schema#keys/name",
            "schema#/",
            "schema#/keys/",
            "schema##",
            "schema#/keys/name#suffix",
            "schema#/keys//items",
            "schema#/items/",
        ] {
            assert!(
                matches!(
                    reference.parse::<SchemaPath>(),
                    Err(SchemaPathParseError::InvalidSyntax { .. })
                ),
                "{reference:?}"
            );
        }
    }

    #[test]
    fn structural_parse_errors_keep_the_offending_component() {
        for mapping in ["keys", "dynamic_keys", "$defs"] {
            assert!(matches!(format!("schema#/{mapping}").parse::<SchemaPath>(),
                Err(SchemaPathParseError::IncompleteMapping { mapping: found }) if found == mapping));
        }
        assert!(matches!("schema#/unknown".parse::<SchemaPath>(),
            Err(SchemaPathParseError::InvalidElement { element }) if element == "unknown"));
        assert!(
            matches!("schema#/dynamic_keys/unsupported.key".parse::<SchemaPath>(),
            Err(SchemaPathParseError::UnsupportedDynamicSource(error)) if error.path == "unsupported.key")
        );
    }
}
