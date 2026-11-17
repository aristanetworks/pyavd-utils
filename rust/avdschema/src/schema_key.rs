// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Validated names of statically declared dictionary keys and reusable definitions.
//!
//! This is the source-model compatibility rule, not the stricter spelling policy in
//! the generated authoring metaschema. Runtime archive tables keep ordinary strings.

#![deny(missing_docs)]

use std::borrow::Borrow;
use std::fmt;
use std::str::FromStr;

/// A nonempty schema key containing only ASCII letters, digits and underscores.
///
/// The compatibility grammar permits uppercase and digit-leading names. The private
/// representation ensures that construction and deserialization use the same checks.
#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(try_from = "String")]
pub struct SchemaKey(String);

impl SchemaKey {
    /// Validate a source dictionary key or definition name using `[A-Za-z0-9_]+`.
    pub fn new(value: impl Into<String>) -> Result<Self, InvalidSchemaKey> {
        let value = value.into();
        if value.is_empty()
            || !value
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
        {
            return Err(InvalidSchemaKey { value });
        }
        Ok(Self(value))
    }

    /// Borrow the validated name without allocating.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for SchemaKey {
    type Error = InvalidSchemaKey;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl FromStr for SchemaKey {
    type Err = InvalidSchemaKey;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Self::new(value)
    }
}

impl Borrow<str> for SchemaKey {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

// Build-time traversal indexes borrowed source keys without cloning their strings.
impl Borrow<str> for &SchemaKey {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<str> for SchemaKey {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for SchemaKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// A source key or definition name outside the supported compatibility grammar.
#[derive(Clone, Debug, PartialEq, Eq, derive_more::Display)]
#[display("Invalid schema key '{value}': expected [A-Za-z0-9_]+")]
pub struct InvalidSchemaKey {
    /// Original name supplied by the caller or source document.
    pub value: String,
}

impl std::error::Error for InvalidSchemaKey {}

#[cfg(test)]
mod tests {
    use super::SchemaKey;
    use crate::Load as _;
    use crate::StoreSource;

    #[test]
    fn constructors_and_serde_share_the_key_grammar() {
        for name in [
            "a",
            "Vxlan1",
            "MIB_family_name",
            "_",
            "123",
            "EVPN_OVERLAY_PEERS",
        ] {
            let key = SchemaKey::new(name).unwrap();
            assert_eq!(key.as_str(), name);
            assert_eq!(name.parse::<SchemaKey>().unwrap(), key);
            let json = serde_json::to_string(&key).unwrap();
            assert_eq!(serde_json::from_str::<SchemaKey>(&json).unwrap(), key);
        }
        for name in [
            "",
            "with-dash",
            "with.dot",
            "space key",
            "with/slash",
            "#",
            "é",
            "name\n",
        ] {
            let error = SchemaKey::new(name).unwrap_err();
            assert_eq!(error.value, name);
            let json = serde_json::to_string(name).unwrap();
            assert!(serde_json::from_str::<SchemaKey>(&json).is_err());
        }
    }

    #[test]
    fn source_dictionaries_and_definitions_reject_invalid_names_during_loading() {
        for name in ["", "with-dash", "with.dot", "space key", "é"] {
            for document in [
                serde_json::json!({"root": {"type": "dict", "keys": {name: {"type": "str"}}}}),
                serde_json::json!({"root": {"type": "dict", "$defs": {name: {"type": "str"}}}}),
                serde_json::json!({"root": {"type": "dict", "keys": {"outer": {"type": "dict", "keys": {name: {"type": "str"}}}}}}),
            ] {
                let error = StoreSource::from_json(&document.to_string())
                    .unwrap_err()
                    .to_string();
                assert!(
                    error.contains(&format!("Invalid schema key '{name}'")),
                    "{error}"
                );
                assert!(error.contains("[A-Za-z0-9_]+"), "{error}");
            }
        }
    }

    #[cfg(feature = "yaml")]
    #[test]
    fn yaml_source_keys_use_the_same_checked_type() {
        let valid = "root:\n  type: dict\n  keys:\n    MIB_family_name:\n      type: str\n";
        let source: StoreSource = yaml_parser::serde::from_str(valid).unwrap();
        let serialized = yaml_parser::serde::to_string(&source).unwrap();
        assert_eq!(
            yaml_parser::serde::from_str::<StoreSource>(&serialized).unwrap(),
            source
        );
        let error = yaml_parser::serde::from_str::<StoreSource>(
            &valid.replace("MIB_family_name", "invalid-key"),
        )
        .unwrap_err()
        .to_string();
        assert!(
            error.contains("Invalid schema key 'invalid-key'"),
            "{error}"
        );
    }
}
