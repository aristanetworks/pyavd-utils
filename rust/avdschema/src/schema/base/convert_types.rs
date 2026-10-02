// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

#![cfg_attr(
    feature = "metaschema",
    allow(
        dead_code,
        reason = "Schema-only types model type-specific conversion values"
    )
)]

use serde::Deserialize;
use serde::Serialize;
use serde_with::skip_serializing_none;

/// List of types to convert from. Used by Str and Int schemas.
#[skip_serializing_none]
#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ConvertTypes {
    pub convert_types: Option<Vec<String>>,
}

/// Schema representation of the values accepted by integer conversion.
#[cfg(feature = "metaschema")]
#[derive(schemars::JsonSchema)]
#[schemars(rename_all = "lowercase")]
pub(crate) enum IntConvertType {
    Bool,
    Str,
    Float,
}

/// Schema representation of the `convert_types` property for integer schemas.
#[cfg(feature = "metaschema")]
#[derive(schemars::JsonSchema)]
pub(crate) struct IntConvertTypes {
    convert_types: Option<Vec<IntConvertType>>,
}

/// Schema representation of the values accepted by string conversion.
#[cfg(feature = "metaschema")]
#[derive(schemars::JsonSchema)]
#[schemars(rename_all = "lowercase")]
pub(crate) enum StrConvertType {
    Bool,
    Int,
    Float,
}

/// Schema representation of the `convert_types` property for string schemas.
#[cfg(feature = "metaschema")]
#[derive(schemars::JsonSchema)]
pub(crate) struct StrConvertTypes {
    convert_types: Option<Vec<StrConvertType>>,
}
