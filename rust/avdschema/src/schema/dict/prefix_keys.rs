// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde::Deserialize;
use serde::Serialize;
use serde_with::skip_serializing_none;

/// Source configuration for dictionary keys selected by a string prefix.
///
/// Prefixes may either be declared directly with [`Self::prefixes`] or read from a sibling input
/// key named by [`Self::prefixes_key`]. Directly declared prefixes take precedence when both are
/// present.
#[skip_serializing_none]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourcePrefixKey {
    /// Sibling key containing the list of prefixes in input data or in its schema default.
    pub prefixes_key: Option<String>,
    /// Static list of prefixes, taking precedence over [`Self::prefixes_key`].
    pub prefixes: Option<Vec<String>>,
    /// Whether the suffix after the prefix selects a key in the referenced dictionary schema.
    pub include_suffix_in_data: bool,
    /// Reference to the schema applied to matching input keys.
    pub schema_ref: String,
}
