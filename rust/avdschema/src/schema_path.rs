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
/// `Display` renders diagnostic locations (`schema/keys/name`). Use
/// [`Self::to_reference`] for the authored `$ref` representation (`schema#/keys/name`).
///
/// ```
/// use avdschema::{SchemaPath, SchemaPathElement};
///
/// let path: SchemaPath = "avd_design#/keys/devices/items".parse()?;
/// assert_eq!(path.schema_name(), "avd_design");
/// assert_eq!(path.elements().last(), Some(&SchemaPathElement::Items));
/// assert_eq!(path.to_reference(), "avd_design#/keys/devices/items");
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

    /// Render the absolute AVD `$ref` syntax.
    pub fn to_reference(&self) -> String {
        format!("{}#{}", self.schema, PathSuffix(&self.elements))
    }
}

impl fmt::Display for SchemaPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.schema, PathSuffix(&self.elements))
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
        // AVD references use a restricted grammar, not general JSON Pointer syntax.
        if !schema
            .as_bytes()
            .first()
            .is_some_and(u8::is_ascii_lowercase)
            || !schema
                .bytes()
                .all(|byte| byte.is_ascii_lowercase() || byte == b'_')
            || (!suffix.is_empty() && !suffix.starts_with('/'))
        {
            return Err(invalid());
        }
        let mut components = suffix.split('/').skip(1);
        for component in components.clone() {
            if !component
                .as_bytes()
                .first()
                .is_some_and(|byte| byte.is_ascii_lowercase() || *byte == b'$')
                || !component.bytes().all(|byte| {
                    byte.is_ascii_lowercase()
                        || byte.is_ascii_digit()
                        || matches!(byte, b'_' | b'.' | b'$')
                })
                || component.bytes().skip(1).any(|byte| byte == b'$')
            {
                return Err(invalid());
            }
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

    #[test]
    fn authored_references_round_trip_without_confusing_literal_and_structural_names() {
        for reference in [
            "schema#",
            "schema#/keys/keys/keys/items/items/keys/value",
            "schema#/$defs/definition/items",
            "schema#/dynamic_keys/node_type_keys.key/keys/name",
        ] {
            let path: SchemaPath = reference.parse().unwrap();
            assert_eq!(path.to_reference(), reference);
            assert_eq!(path.to_string(), reference.replace('#', ""));
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
            declaration.to_reference(),
            "schema#/dynamic_keys/network_services_keys.name"
        );
        assert_eq!(
            declaration.to_reference().parse::<SchemaPath>().unwrap(),
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
    fn malformed_reference_syntax_is_rejected_at_the_boundary() {
        for reference in [
            "schema",
            "#",
            "Schema#",
            "schema1#",
            "schema#keys/name",
            "schema#/",
            "schema#/keys/",
            "schema#/keys/na-me",
            "schema#/keys/a$b",
            "schema##",
            "schema#/keys/name\n",
            "schema#/keys/na~1me",
            "schema#/keys/é",
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
