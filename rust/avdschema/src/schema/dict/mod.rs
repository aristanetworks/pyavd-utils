// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

pub mod dynamic_keys;
mod prefix_keys;

pub use dynamic_keys::DynamicKeyOverrides;
use ordermap::OrderMap;
pub use prefix_keys::SourcePrefixKey;
use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;
use serde_with::skip_serializing_none;

use super::any::SourceSchema;
use super::base::Base;
use super::base::documentation_options::DocumentationOptionsDict;

/// Source model for an AVD dictionary schema.
#[skip_serializing_none]
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceDict {
    /// Dictionary of dictionary keys in the format `{<keyname>: {<schema>}}`.
    ///
    /// `keyname` must use snake case. `schema` is recursive and must conform
    /// to the AVD schema format.
    pub keys: Option<OrderMap<String, SourceSchema>>,
    /// Dictionary of dynamic dictionary keys in the format
    /// `{<variable.path>: {<schema>}}`.
    ///
    /// `variable.path` uses dot notation and points to data under the parent
    /// dictionary containing the concrete key names. Lists in the path are
    /// expanded across all items.
    pub dynamic_keys: Option<OrderMap<String, SourceSchema>>,
    /// Prefix-based dictionary keys evaluated from static prefixes or sibling input data.
    pub prefix_keys: Option<Vec<SourcePrefixKey>>,
    pub allow_other_keys: Option<bool>,
    pub relaxed_validation: Option<bool>,
    #[serde(rename = "$id")]
    pub schema_id: Option<String>,
    #[serde(rename = "$schema")]
    pub schema_schema: Option<String>,
    #[serde(rename = "$defs")]
    pub schema_defs: Option<OrderMap<String, SourceSchema>>,
    #[serde(flatten)]
    pub base: Base<OrderMap<String, Value>>,
    pub documentation_options: Option<DocumentationOptionsDict>,
}

impl<'a> TryFrom<&'a SourceSchema> for &'a SourceDict {
    type Error = &'static str;

    fn try_from(value: &'a SourceSchema) -> Result<Self, Self::Error> {
        match value {
            SourceSchema::Dict(dict) => Ok(dict),
            _ => Err("Unable to convert from SourceSchema to SourceDict. Invalid Schema type."),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::SourceDict;
    use crate::any::SourceSchema;
    use crate::boolean::SourceBool;

    #[test]
    fn try_from_source_schema_ok() {
        let schema = SourceSchema::Dict(SourceDict::default());
        let result: Result<&SourceDict, _> = (&schema).try_into();
        assert!(result.is_ok());
    }

    #[test]
    fn try_from_source_schema_err() {
        let schema = SourceSchema::Bool(SourceBool::default());
        let result: Result<&SourceDict, _> = (&schema).try_into();
        assert!(result.is_err());
    }
}
