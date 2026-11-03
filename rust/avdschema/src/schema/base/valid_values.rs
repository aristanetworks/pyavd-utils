// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde::Deserialize;
use serde::Serialize;
use serde_with::skip_serializing_none;

// Valid values allowed by schema. Used for Int and Str schemas.
#[skip_serializing_none]
#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct ValidValues<T> {
    /// List of valid values
    pub valid_values: Option<Vec<T>>,
    /// Dot-notation paths to variables under the parent input dictionary containing valid values.
    /// For example, `interfaces.name` collects interface names from a list of dictionaries.
    /// When a path traverses a list, every list item is unpacked.
    /// Valid values are derived from the input data, not from the schema document.
    pub dynamic_valid_values: Option<Vec<String>>,
}
