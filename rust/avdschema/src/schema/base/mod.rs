// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

pub mod convert_types;
pub mod documentation_options;
pub mod valid_values;

use ordermap::OrderMap;
use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;
use serde_with::skip_serializing_none;

/// `DataValue` marks the allowed types to be used as generics in Base, `ValidValues` and default values.
pub trait DataValue {}
impl DataValue for bool {}
impl DataValue for i64 {}
impl DataValue for String {}
// OrderMap<String, Value> corresponds to a dict
impl DataValue for OrderMap<String, Value> {}
// Vec<Value> corresponds to a list
impl DataValue for Vec<Value> {}

/// Schema properties shared by all schema types.
#[skip_serializing_none]
#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct Base<T>
where
    T: DataValue,
{
    /// Default value
    pub default: Option<T>,
    /// Free text display name for forms and documentation (single line)
    #[cfg_attr(feature = "metaschema", schemars(regex(pattern = r"^[^\n]+$")))]
    pub display_name: Option<String>,
    /// Free text description for forms and documentation (multi line)
    #[cfg_attr(feature = "metaschema", schemars(length(min = 1)))]
    pub description: Option<String>,
    /// Key is required
    /// Default is false.
    pub required: Option<bool>,
    pub deprecation: Option<Deprecation>,
    /// Reference to another AVD schema or schema fragment, inherited by this schema.
    /// Use a named schema followed by a JSON Pointer, for example
    /// `eos_cli_config_gen#/keys/hostname` or `eos_cli_config_gen#/$defs/shared`.
    /// A reference ending in `#` imports the complete root dictionary. Local properties override
    /// inherited properties, and the referenced schema must have the same type.
    #[serde(rename = "$ref")]
    #[cfg_attr(
        feature = "metaschema",
        schemars(regex(pattern = r"^[a-z][a-z_]*#(/[a-z$][\.a-z0-9_]*)*$"))
    )]
    pub schema_ref: Option<String>,
}

/// Deprecation settings
#[skip_serializing_none]
#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct Deprecation {
    /// Emit deprecation warning if key is set
    pub warning: bool,
    /// Relative path to new key
    pub new_key: Option<String>,
    /// Allow the deprecated key to be configured simultaneously with the new key
    /// Default is false.
    pub allow_with_new_key: Option<bool>,
    /// Support for this key has been removed
    /// Default is false.
    pub removed: Option<bool>,
    /// Version in which the key will be removed
    pub remove_in_version: Option<String>,
    /// Date after which the key will be removed in the next major version
    pub remove_after_date: Option<String>,
    /// URL detailing the deprecation and migration guidelines
    pub url: Option<String>,
    /// Upgrade handler used to migrate deprecated data
    pub upgrade_handler: Option<String>,
}
