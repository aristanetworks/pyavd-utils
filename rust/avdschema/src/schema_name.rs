// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Validated names identifying root documents in schema stores and schema paths.
//!
//! Schema names additionally allow dashes; static dictionary keys use [`crate::SchemaKey`].

#![deny(missing_docs)]

use std::borrow::Borrow;
use std::fmt;
use std::str::FromStr;

/// A nonempty schema name containing only ASCII letters, digits, underscores and dashes.
///
/// The compatibility grammar permits uppercase and digit-leading names. The private
/// representation ensures that construction and deserialization use the same checks.
#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(try_from = "String")]
pub struct SchemaName(String);

impl SchemaName {
    /// Validate a schema root name using `[A-Za-z0-9_-]+`.
    pub fn new(value: impl Into<String>) -> Result<Self, InvalidSchemaName> {
        let value = value.into();
        if value.is_empty()
            || !value
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
        {
            return Err(InvalidSchemaName { value });
        }
        Ok(Self(value))
    }

    /// Borrow the validated name without allocating.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for SchemaName {
    type Error = InvalidSchemaName;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl FromStr for SchemaName {
    type Err = InvalidSchemaName;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Self::new(value)
    }
}

impl Borrow<str> for SchemaName {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<str> for SchemaName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for SchemaName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// A schema root name outside the supported compatibility grammar.
#[derive(Clone, Debug, PartialEq, Eq, derive_more::Display)]
#[display("Invalid schema name '{value}': expected [A-Za-z0-9_-]+")]
pub struct InvalidSchemaName {
    /// Original name supplied by the caller or source document.
    pub value: String,
}

impl std::error::Error for InvalidSchemaName {}

#[cfg(test)]
mod tests {
    use super::SchemaName;
    use crate::Load as _;
    use crate::StoreSource;

    #[test]
    fn schema_names_allow_dashes_without_relaxing_dictionary_keys() {
        for name in ["schema", "Schema1", "_root", "123", "shared-schema"] {
            let schema_name = SchemaName::new(name).unwrap();
            let serialized = serde_json::to_string(&schema_name).unwrap();
            assert_eq!(
                serde_json::from_str::<SchemaName>(&serialized).unwrap(),
                schema_name
            );
            let source_json = serde_json::json!({name: {"type": "dict"}}).to_string();
            let source = StoreSource::from_json(&source_json).unwrap();
            assert!(source.get(name).is_ok());
        }
        assert!(crate::SchemaKey::new("shared-schema").is_err());
        for name in ["", "with.dot", "space name", "name#", "é"] {
            let error = SchemaName::new(name).unwrap_err();
            assert_eq!(error.value, name);
            let source_json = serde_json::json!({name: {"type": "dict"}}).to_string();
            let load_error = StoreSource::from_json(&source_json)
                .unwrap_err()
                .to_string();
            assert!(load_error.contains("Invalid schema name"), "{load_error}");
            let document_error = StoreSource::from_schema_json(name, r#"{"type":"dict"}"#)
                .unwrap_err()
                .to_string();
            assert!(
                document_error.contains("Invalid schema name"),
                "{document_error}"
            );
        }
    }

    #[test]
    fn string_entrypoints_report_invalid_root_names_before_lookup_or_io() {
        let source = StoreSource::from_json("{}").unwrap();
        assert!(matches!(
            crate::Store::compile_schema(&source, "invalid.name"),
            Err(crate::StoreError::Compile(crate::CompileError::InvalidSchemaName(error)))
                if error.value == "invalid.name"
        ));
        #[cfg(feature = "dump_load_files")]
        {
            let paths = std::collections::HashMap::from([(
                "invalid.name".to_owned(),
                std::path::PathBuf::from("nonexistent-schema.json"),
            )]);
            assert!(matches!(
                StoreSource::new_from_paths(paths),
                Err(crate::LoadError::InvalidSchemaName(error)) if error.value == "invalid.name"
            ));
        }
    }
}
