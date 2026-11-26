// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde::Deserialize;
use serde::Serialize;
use serde_with::skip_serializing_none;

/// Source configuration for dictionary keys selected by a string prefix.
///
/// Prefixes are declared directly with `prefixes` or read from the sibling input key named
/// by `prefixes_key`. Static prefixes take precedence when both are present. At least one
/// source must be configured. Input values override schema defaults, including empty lists.
#[skip_serializing_none]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "metaschema", schemars(extend("anyOf" = [{"required": ["prefixes"]}, {"required": ["prefixes_key"]}])))]
#[serde(deny_unknown_fields)]
pub struct SourcePrefixKey {
    /// Sibling key containing the list of prefixes in input data or in its schema default.
    #[cfg_attr(
        feature = "metaschema",
        schemars(with = "Option<super::MetaStaticSchemaKey>")
    )]
    pub prefixes_key: Option<crate::SchemaKey>,
    /// Static list of prefixes, taking precedence over `prefixes_key`.
    pub prefixes: Option<Vec<String>>,
    /// Whether the suffix after the prefix selects a key in the referenced dictionary schema.
    pub include_suffix_in_data: bool,
    /// Disable required-key checks below matching values, for partial configuration patches.
    #[serde(default)]
    pub relaxed_validation: bool,
    /// Reference to the schema applied to matching input keys.
    pub schema_ref: crate::SchemaPath,
}
