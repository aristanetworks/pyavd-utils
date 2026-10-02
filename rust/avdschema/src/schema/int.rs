// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde::Deserialize;
use serde::Serialize;
use serde_with::skip_serializing_none;

use super::any::SourceSchema;
use super::base::convert_types::ConvertTypes;
use super::base::documentation_options::DocumentationOptions;
use super::base::valid_values::ValidValues;
use crate::schema::base::Base;

/// AVD Schema for integer data.
#[skip_serializing_none]
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct SourceInt {
    pub min: Option<i64>,
    pub max: Option<i64>,
    #[serde(flatten)]
    pub base: Base<i64>,
    #[serde(flatten)]
    #[cfg_attr(
        feature = "metaschema",
        schemars(with = "super::base::convert_types::IntConvertTypes")
    )]
    pub convert_types: ConvertTypes,
    #[serde(flatten)]
    pub valid_values: ValidValues<i64>,
    pub documentation_options: Option<DocumentationOptions>,
}
impl<'x> TryFrom<&'x SourceSchema> for &'x SourceInt {
    type Error = &'static str;

    fn try_from(value: &'x SourceSchema) -> Result<Self, Self::Error> {
        match value {
            SourceSchema::Int(int) => Ok(int),
            _ => Err("Unable to convert from SourceSchema to SourceInt. Invalid Schema type."),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::SourceInt;
    use crate::any::SourceSchema;
    use crate::str::SourceStr;

    #[test]
    fn try_from_anyschema_ok() {
        let anyschema = &SourceSchema::Int(SourceInt::default());
        let result: Result<&SourceInt, _> = anyschema.try_into();
        assert!(result.is_ok());
    }
    #[test]
    fn try_from_anyschema_err() {
        let anyschema = &SourceSchema::Str(SourceStr::default());
        let result: Result<&SourceInt, _> = anyschema.try_into();
        assert!(result.is_err());
    }
}
