// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

pub mod dynamic_keys;

pub use dynamic_keys::DynamicKeyOverrides;
use ordermap::OrderMap;
use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;
use serde_with::skip_serializing_none;

use super::any::SourceSchema;
use super::base::Base;
#[cfg(feature = "metaschema")]
use super::base::DataValue;
use super::base::documentation_options::DocumentationOptionsDict;

/// Schema-only replacement for `OrderMap<String, Value>`, which cannot implement
/// `JsonSchema` because both the trait and type are external.
#[cfg(feature = "metaschema")]
#[derive(schemars::JsonSchema)]
#[schemars(transparent)]
#[allow(
    dead_code,
    reason = "This type only supplies the JSON Schema representation for OrderMap"
)]
struct MetaObject(std::collections::BTreeMap<String, Value>);

#[cfg(feature = "metaschema")]
impl DataValue for MetaObject {}

/// Schema-only key type for statically named schema keys.
///
/// Static keys use snake case. The two explicitly named exceptions are deprecated keys retained
/// in supported AVD schemas for backwards compatibility.
#[cfg(feature = "metaschema")]
#[derive(schemars::JsonSchema)]
#[schemars(transparent)]
#[allow(
    dead_code,
    reason = "This type supplies the property-name pattern for the generated metaschema"
)]
struct MetaStaticSchemaKey(
    #[schemars(regex(pattern = "^(?:[a-z][a-z0-9_]*|MIB_family_name|Vxlan1)$"))] String,
);

/// Schema-only key type for reusable schema definitions.
#[cfg(feature = "metaschema")]
#[derive(schemars::JsonSchema)]
#[schemars(transparent)]
#[allow(
    dead_code,
    reason = "This type supplies the property-name pattern for the generated metaschema"
)]
struct MetaDefinitionKey(#[schemars(regex(pattern = "^[a-z][a-z0-9_]*$"))] String);

/// Schema-only key type for data paths declaring dynamic schema keys.
#[cfg(feature = "metaschema")]
#[derive(schemars::JsonSchema)]
#[schemars(transparent)]
#[allow(
    dead_code,
    reason = "This type supplies the property-name pattern for the generated metaschema"
)]
struct MetaDynamicSchemaKey(#[schemars(regex(pattern = "^[a-z][a-z0-9_.]*$"))] String);

/// Source model for an AVD dictionary schema.
#[skip_serializing_none]
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct SourceDict {
    /// Dictionary of dictionary keys in the format `{<keyname>: {<schema>}}`.
    ///
    /// `keyname` must use snake case. `schema` is recursive and must conform
    /// to the AVD schema format.
    #[cfg_attr(
        feature = "metaschema",
        schemars(with = "Option<std::collections::BTreeMap<MetaStaticSchemaKey, SourceSchema>>")
    )]
    pub keys: Option<OrderMap<String, SourceSchema>>,
    /// Dictionary of dynamic dictionary keys in the format
    /// `{<variable.path>: {<schema>}}`.
    ///
    /// `variable.path` uses dot notation and points to data under the parent
    /// dictionary containing the concrete key names. Lists in the path are
    /// expanded across all items.
    #[cfg_attr(
        feature = "metaschema",
        schemars(with = "Option<std::collections::BTreeMap<MetaDynamicSchemaKey, SourceSchema>>")
    )]
    pub dynamic_keys: Option<OrderMap<String, SourceSchema>>,
    pub allow_other_keys: Option<bool>,
    pub relaxed_validation: Option<bool>,
    #[serde(rename = "$id")]
    pub schema_id: Option<String>,
    #[serde(rename = "$schema")]
    pub schema_schema: Option<String>,
    #[serde(rename = "$defs")]
    #[cfg_attr(
        feature = "metaschema",
        schemars(with = "Option<std::collections::BTreeMap<MetaDefinitionKey, SourceSchema>>")
    )]
    pub schema_defs: Option<OrderMap<String, SourceSchema>>,
    #[serde(flatten)]
    #[cfg_attr(feature = "metaschema", schemars(with = "Base<MetaObject>"))]
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
