// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde::Deserialize;
use serde::Serialize;
use serde_with::skip_serializing_none;

use super::any::SourceSchema;
use super::base::Base;
use super::base::documentation_options::DocumentationOptions;

/// AVD Schema for boolean data.
#[skip_serializing_none]
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct SourceBool {
    #[serde(flatten)]
    pub base: Base<bool>,
    pub documentation_options: Option<DocumentationOptions>,
}

impl<'x> TryFrom<&'x SourceSchema> for &'x SourceBool {
    type Error = &'static str;

    fn try_from(value: &'x SourceSchema) -> Result<Self, Self::Error> {
        match value {
            SourceSchema::Bool(bool) => Ok(bool),
            _ => Err("Unable to convert from SourceSchema to SourceBool. Invalid Schema type."),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::SourceBool;
    use crate::any::SourceSchema;
    use crate::str::SourceStr;

    #[test]
    fn try_from_anyschema_ok() {
        let anyschema = &SourceSchema::Bool(SourceBool::default());
        let result: Result<&SourceBool, _> = anyschema.try_into();
        assert!(result.is_ok());
    }
    #[test]
    fn try_from_anyschema_err() {
        let anyschema = &SourceSchema::Str(SourceStr::default());
        let result: Result<&SourceBool, _> = anyschema.try_into();
        assert!(result.is_err());
    }
}
