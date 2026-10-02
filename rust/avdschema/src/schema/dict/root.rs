// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Source model for the root dictionary of an AVD schema.

#![deny(missing_docs)]

use ordermap::OrderMap;
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::de::Error as _;
use serde_json::Value;
use serde_with::skip_serializing_none;

#[cfg(feature = "metaschema")]
use super::MetaDefinitionKey;
#[cfg(feature = "metaschema")]
use super::MetaDynamicSchemaKey;
#[cfg(feature = "metaschema")]
use super::MetaObject;
#[cfg(feature = "metaschema")]
use super::MetaStaticSchemaKey;
use crate::Inherit;
use crate::any::SourceSchema;
use crate::base::Base;
use crate::base::documentation_options::DocumentationOptionsDict;
use crate::utils::dump::Dump;
use crate::utils::load::Load;
#[cfg(feature = "dump_load_files")]
use crate::utils::load::LoadFromFragments;

/// Root dictionary of one named AVD schema.
///
/// A schema root supports reusable definitions and data-driven keys in addition to the
/// properties available on recursive dictionary schemas. These properties describe the named
/// schema document and are therefore not accepted on nested [`super::SourceDict`] values.
#[skip_serializing_none]
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct SourceRootDict {
    /// Statically named dictionary keys.
    #[cfg_attr(
        feature = "metaschema",
        schemars(with = "Option<std::collections::BTreeMap<MetaStaticSchemaKey, SourceSchema>>")
    )]
    pub keys: Option<OrderMap<String, SourceSchema>>,
    /// Data paths whose values provide additional root key names.
    #[cfg_attr(
        feature = "metaschema",
        schemars(with = "Option<std::collections::BTreeMap<MetaDynamicSchemaKey, SourceSchema>>")
    )]
    pub dynamic_keys: Option<OrderMap<String, SourceSchema>>,
    /// Whether input keys absent from `keys` and resolved `dynamic_keys` are accepted.
    pub allow_other_keys: Option<bool>,
    /// Whether required-key validation is disabled for descendants.
    pub relaxed_validation: Option<bool>,
    /// Identifier carried by the authored schema document.
    #[serde(rename = "$id")]
    pub schema_id: Option<String>,
    /// Metaschema identifier carried by the authored schema document.
    #[serde(rename = "$schema")]
    pub schema_schema: Option<String>,
    /// Reusable schema definitions addressable through `$ref`.
    #[serde(rename = "$defs")]
    #[cfg_attr(
        feature = "metaschema",
        schemars(with = "Option<std::collections::BTreeMap<MetaDefinitionKey, SourceSchema>>")
    )]
    pub schema_defs: Option<OrderMap<String, SourceSchema>>,
    /// Properties shared with every schema value.
    #[serde(flatten)]
    #[cfg_attr(feature = "metaschema", schemars(with = "Base<MetaObject>"))]
    pub base: Base<OrderMap<String, Value>>,
    /// Documentation-generation settings for this dictionary.
    pub documentation_options: Option<DocumentationOptionsDict>,
}

/// Root discriminator used while loading and serializing schema documents.
///
/// Keeping the discriminator outside [`SourceRootDict`] gives callers the same ergonomic data
/// model as the recursive schema structs while still requiring `type: dict` on the wire.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[serde(tag = "type", rename_all = "lowercase")]
pub(crate) enum SourceRootSchema {
    Dict(SourceRootDict),
}

/// Deserialization-only representation used to report recursive root types precisely.
///
/// Deserializing all supported discriminators first avoids exposing Serde's `unknown variant`
/// message when a valid recursive type is used where a named schema root is expected.
#[derive(Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
#[allow(
    clippy::large_enum_variant,
    reason = "Root deserialization avoids a heap allocation for each valid schema"
)]
enum SourceRootSchemaWire {
    Bool,
    Int,
    Str,
    List,
    Dict(SourceRootDict),
}

impl<'de> Deserialize<'de> for SourceRootSchema {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        match SourceRootSchemaWire::deserialize(deserializer)? {
            SourceRootSchemaWire::Dict(dict) => Ok(Self::Dict(dict)),
            SourceRootSchemaWire::Bool => Err(D::Error::custom(
                "Schema source has type 'bool', but requires a dictionary root",
            )),
            SourceRootSchemaWire::Int => Err(D::Error::custom(
                "Schema source has type 'int', but requires a dictionary root",
            )),
            SourceRootSchemaWire::Str => Err(D::Error::custom(
                "Schema source has type 'str', but requires a dictionary root",
            )),
            SourceRootSchemaWire::List => Err(D::Error::custom(
                "Schema source has type 'list', but requires a dictionary root",
            )),
        }
    }
}

impl SourceRootSchema {
    /// Return the dictionary carried by the root discriminator.
    pub(crate) fn as_dict(&self) -> &SourceRootDict {
        match self {
            Self::Dict(dict) => dict,
        }
    }
}

impl Dump for SourceRootSchema {}
impl Load for SourceRootSchema {}
#[cfg(feature = "dump_load_files")]
impl LoadFromFragments for SourceRootSchema {}

impl Inherit for SourceRootSchema {
    fn inherit(&mut self, other: &Self) {
        match (self, other) {
            (Self::Dict(dict), Self::Dict(other_dict)) => dict.inherit(other_dict),
        }
    }
}
