// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde::Deserialize;
use serde::Serialize;
use serde_with::skip_serializing_none;

use super::any::SourceSchema;
use super::base::Base;
use super::base::convert_types::ConvertTypes;
use super::base::documentation_options::DocumentationOptions;
use super::base::valid_values::ValidValues;

/// Enum for string formats allowed by the [`SourceStr`] schema.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Format {
    Cidr,
    Ip,
    IpPool,
    Ipv4,
    Ipv4Cidr,
    Ipv4Pool,
    Ipv6,
    Ipv6Cidr,
    Ipv6Pool,
    Mac,
}

/// AVD Schema for string data.
#[skip_serializing_none]
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceStr {
    /// Convert string value to lower case before performing validation
    pub convert_to_lower_case: Option<bool>,
    pub format: Option<Format>,
    pub max_length: Option<u64>,
    pub min_length: Option<u64>,
    /// A regular expression which will be matched on the variable value.
    /// The regular expression should be valid according to the ECMA 262 dialect
    /// Remember to use double escapes
    pub pattern: Option<Pattern>,
    #[serde(flatten)]
    pub base: Base<String>,
    #[serde(flatten)]
    pub convert_types: ConvertTypes,
    #[serde(flatten)]
    pub valid_values: ValidValues<String>,
    pub documentation_options: Option<DocumentationOptions>,
}

impl<'x> TryFrom<&'x SourceSchema> for &'x SourceStr {
    type Error = &'static str;

    fn try_from(value: &'x SourceSchema) -> Result<Self, Self::Error> {
        match value {
            SourceSchema::Str(str) => Ok(str),
            _ => Err("Unable to convert from SourceSchema to SourceStr. Invalid Schema type."),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_more::Display)]
#[display("{pattern}")]
#[serde(transparent)]
pub struct Pattern {
    pub pattern: String,
}
impl From<&str> for Pattern {
    fn from(value: &str) -> Self {
        Self {
            pattern: value.to_owned(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::SourceStr;
    use crate::any::SourceSchema;
    use crate::boolean::SourceBool;

    #[test]
    fn try_from_anyschema_ok() {
        let anyschema = &SourceSchema::Str(SourceStr::default());
        let result: Result<&SourceStr, _> = anyschema.try_into();
        assert!(result.is_ok());
    }
    #[test]
    fn try_from_anyschema_err() {
        let anyschema = &SourceSchema::Bool(SourceBool::default());
        let result: Result<&SourceStr, _> = anyschema.try_into();
        assert!(result.is_err());
    }
}
