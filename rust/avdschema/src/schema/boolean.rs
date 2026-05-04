// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;
use serde_with::skip_serializing_none;

use super::any::AnySchema;
use super::base::Base;
use super::base::documentation_options::DocumentationOptions;
use crate::any::Shortcuts;
use crate::base::Deprecation;

/// AVD Schema for boolean data.
#[skip_serializing_none]
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Bool {
    #[serde(flatten)]
    pub base: Base<bool>,
    pub documentation_options: Option<DocumentationOptions>,
}

impl Shortcuts for Bool {
    fn is_required(&self) -> bool {
        self.base.required.unwrap_or_default()
    }
    fn deprecation(&self) -> &Option<Deprecation> {
        &self.base.deprecation
    }

    fn default_(&self) -> Option<Value> {
        self.base.default.as_ref().map(|value| Value::Bool(*value))
    }
}

impl<'x> TryFrom<&'x AnySchema> for &'x Bool {
    type Error = &'static str;

    fn try_from(value: &'x AnySchema) -> Result<Self, Self::Error> {
        match value {
            AnySchema::Bool(bool) => Ok(bool),
            _ => Err("Unable to convert from AnySchema to Bool. Invalid Schema type."),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Bool;
    use crate::any::AnySchema;
    use crate::str::Str;

    #[test]
    fn try_from_anyschema_ok() {
        let anyschema = &AnySchema::Bool(Bool::default());
        let result: Result<&Bool, _> = anyschema.try_into();
        assert!(result.is_ok());
    }
    #[test]
    fn try_from_anyschema_err() {
        let anyschema = &AnySchema::Str(Str::default());
        let result: Result<&Bool, _> = anyschema.try_into();
        assert!(result.is_err());
    }
}
