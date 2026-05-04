// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::Store;
use log::debug;
use serde_json::Value;

use super::Validation;
use crate::context::Configuration;
use crate::context::Context;
use crate::context::ValidationResult;
use crate::feedback::InputDiagnostic;
use crate::feedback::ParseDiagnostic;
use crate::validatable::ValidatableValue;

#[derive(Debug, Default)]
/// Result of validation for a single parsed value or YAML document.
pub struct ValidationOutput<T> {
    /// The validation result with errors, warnings, and infos.
    pub result: ValidationResult,
    /// The coerced data with types adjusted according to the schema.
    /// Only populated when `Configuration::return_coerced_data` is true.
    pub coerced: Option<T>,
}

#[derive(Debug)]
/// Result of validating a single-document input source.
pub struct InputValidationResult<T> {
    pub input_diagnostics: Vec<InputDiagnostic>,
    pub document: ValidationOutput<T>,
}

/// Validate already-parsed values against a schema.
pub trait StoreValidate<V>
where
    V: ValidatableValue,
{
    /// Entrypoint for validating a value implementing ValidatableValue against the given schema name.
    fn validate_value(
        &self,
        value: &V,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<V::Coerced>, StoreValidateError>;
}

/// Parse JSON input and validate the resulting value against a schema.
pub trait StoreValidateInput {
    fn validate_json(
        &self,
        json: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<InputValidationResult<Value>, StoreValidateError>;
}

impl<V> StoreValidate<V> for Store
where
    V: ValidatableValue,
{
    fn validate_value(
        &self,
        value: &V,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<V::Coerced>, StoreValidateError> {
        debug!("Validating value");
        let mut ctx = Context::new(self, configuration);
        let schema = self.get(schema_name)?;
        let coerced = schema.validate(value, &mut ctx);
        debug!("Validating value done");
        Ok(ValidationOutput {
            result: ctx.result,
            coerced,
        })
    }
}

impl StoreValidateInput for Store {
    fn validate_json(
        &self,
        json: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<InputValidationResult<Value>, StoreValidateError> {
        debug!("Validating JSON");
        let _ = self.get(schema_name)?;
        let value: Value = match serde_json::from_str(json) {
            Ok(value) => value,
            Err(parse_error) => {
                return Ok(InputValidationResult {
                    input_diagnostics: vec![InputDiagnostic::ParseDiagnostic(
                        ParseDiagnostic::from_json_error(&parse_error, json),
                    )],
                    document: Default::default(),
                });
            }
        };
        debug!("Deserialization of JSON done");
        let document = <Store as StoreValidate<Value>>::validate_value(
            self,
            &value,
            schema_name,
            configuration,
        )?;
        debug!("Validating JSON done");
        Ok(InputValidationResult {
            input_diagnostics: Vec::new(),
            document,
        })
    }
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum StoreValidateError {
    SchemaStore(avdschema::SchemaStoreError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::feedback::ParseDiagnosticKind;
    use crate::validation::test_utils::get_test_store;

    #[test]
    fn validate_json_invalid_schema() {
        let input = "{}";
        let store = get_test_store();
        let result = store.validate_json(input, "invalid_schema", None);
        assert!(matches!(
            result,
            Err(StoreValidateError::SchemaStore(
                avdschema::SchemaStoreError::InvalidSchemaName(schema)
            ))
                if schema == "invalid_schema"
        ));
    }

    #[test]
    fn validate_json_parse_error_is_returned_as_feedback() {
        let input = "{\"foo\":";
        let store = get_test_store();
        let result = store.validate_json(input, "avd_design", None).unwrap();
        assert!(result.document.result.errors.is_empty());
        assert!(result.document.result.warnings.is_empty());
        assert!(result.document.result.infos.is_empty());
        assert!(matches!(
            result.input_diagnostics.as_slice(),
            [InputDiagnostic::ParseDiagnostic(parse_diagnostic)]
                if parse_diagnostic.kind == ParseDiagnosticKind::JsonSyntax
                    && parse_diagnostic.span.start <= input.len()
                    && parse_diagnostic.span.end <= input.len()
        ));
    }

    #[test]
    fn validate_value_invalid_schema() {
        let input = serde_json::json!({});
        let store = get_test_store();
        let result = store.validate_value(&input, "invalid_schema", None);
        assert!(matches!(
            result,
            Err(StoreValidateError::SchemaStore(
                avdschema::SchemaStoreError::InvalidSchemaName(schema)
            ))
                if schema == "invalid_schema"
        ));
    }
}
