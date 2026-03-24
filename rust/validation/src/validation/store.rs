// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::{Schema, Store};
use log::debug;
use serde_json::Value;
use yaml_parser::{Node, parse};

use crate::{
    context::{Configuration, Context, ValidationResult},
    feedback::{InputDiagnostic, ParseDiagnostic},
    validatable::ValidatableValue,
};

use super::Validation;

/// Result of validation for a single parsed value or YAML document.
pub struct ValidationOutput<T> {
    /// The validation result with errors, warnings, and infos.
    pub result: ValidationResult,
    /// The coerced data with types adjusted according to the schema.
    /// Only populated when `Configuration::return_coerced_data` is true.
    pub coerced: Option<T>,
}

/// Result of validating a single-document input source.
pub struct InputValidationResult<T> {
    pub input_diagnostics: Vec<InputDiagnostic>,
    pub document: ValidationOutput<T>,
}

/// Result of validating a YAML input source that may contain multiple documents.
pub struct YamlValidationResult<T> {
    pub input_diagnostics: Vec<InputDiagnostic>,
    pub documents: Vec<ValidationOutput<T>>,
}

/// Validate already-parsed values against a schema.
pub trait StoreValidate<S, V>
where
    V: ValidatableValue,
{
    /// Entrypoint for validating a value implementing ValidatableValue against the given schema name.
    fn validate_value(
        &self,
        value: &V,
        schema_name: S,
        configuration: Option<&Configuration>,
    ) -> ValidationOutput<V::Coerced>;
}

/// Parse JSON or YAML input and validate the resulting value against a schema.
pub trait StoreValidateInput<S> {
    fn validate_json(
        &self,
        json: &str,
        schema_name: S,
        configuration: Option<&Configuration>,
    ) -> Result<InputValidationResult<Value>, StoreValidateError>;

    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: S,
        configuration: Option<&Configuration>,
    ) -> Result<YamlValidationResult<Node<'static>>, StoreValidateError>;
}

impl<V> StoreValidate<Schema, V> for Store
where
    V: ValidatableValue,
{
    fn validate_value(
        &self,
        value: &V,
        schema_name: Schema,
        configuration: Option<&Configuration>,
    ) -> ValidationOutput<V::Coerced> {
        debug!("Validating value");
        let mut ctx = Context::new(self, configuration);
        let schema = self.get(schema_name);
        let coerced = schema.validate(value, &mut ctx);
        debug!("Validating value done");
        ValidationOutput {
            result: ctx.result,
            coerced,
        }
    }
}

impl<V> StoreValidate<&str, V> for Store
where
    V: ValidatableValue,
{
    fn validate_value(
        &self,
        value: &V,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> ValidationOutput<V::Coerced> {
        match Schema::try_from(schema_name) {
            Ok(schema_type) => <Store as StoreValidate<Schema, V>>::validate_value(
                self,
                value,
                schema_type,
                configuration,
            ),
            Err(_) => empty_validation_output(),
        }
    }
}

impl StoreValidateInput<Schema> for Store {
    fn validate_json(
        &self,
        json: &str,
        schema_name: Schema,
        configuration: Option<&Configuration>,
    ) -> Result<InputValidationResult<Value>, StoreValidateError> {
        debug!("Validating JSON");
        let value: Value = serde_json::from_str(json)?;
        debug!("Deserialization of JSON done");
        let document = <Store as StoreValidate<Schema, Value>>::validate_value(
            self,
            &value,
            schema_name,
            configuration,
        );
        debug!("Validating JSON done");
        Ok(InputValidationResult {
            input_diagnostics: Vec::new(),
            document,
        })
    }

    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: Schema,
        configuration: Option<&Configuration>,
    ) -> Result<YamlValidationResult<Node<'static>>, StoreValidateError> {
        debug!("Validating YAML");
        let (yaml_docs, parse_errors) = parse(yaml);
        debug!("Deserialization of YAML done");

        let input_diagnostics = parse_errors
            .into_iter()
            .map(|parse_error| InputDiagnostic::YamlParse(ParseDiagnostic::from(parse_error)))
            .collect();

        let documents = yaml_docs
            .iter()
            .map(|document| {
                <Store as StoreValidate<Schema, Node<'static>>>::validate_value(
                    self,
                    document,
                    schema_name,
                    configuration,
                )
            })
            .collect();

        debug!("Validating YAML done");
        Ok(YamlValidationResult {
            input_diagnostics,
            documents,
        })
    }
}

impl StoreValidateInput<&str> for Store {
    fn validate_json(
        &self,
        json: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<InputValidationResult<Value>, StoreValidateError> {
        match Schema::try_from(schema_name) {
            Ok(schema_type) => <Store as StoreValidateInput<Schema>>::validate_json(
                self,
                json,
                schema_type,
                configuration,
            ),
            Err(_) => Ok(InputValidationResult {
                input_diagnostics: vec![InputDiagnostic::InvalidSchema {
                    schema: schema_name.to_string(),
                }],
                document: empty_validation_output(),
            }),
        }
    }

    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<YamlValidationResult<Node<'static>>, StoreValidateError> {
        match Schema::try_from(schema_name) {
            Ok(schema_type) => <Store as StoreValidateInput<Schema>>::validate_yaml(
                self,
                yaml,
                schema_type,
                configuration,
            ),
            Err(_) => Ok(YamlValidationResult {
                input_diagnostics: vec![InputDiagnostic::InvalidSchema {
                    schema: schema_name.to_string(),
                }],
                documents: Vec::new(),
            }),
        }
    }
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum StoreValidateError {
    JsonError(serde_json::Error),
}

fn empty_validation_output<T>() -> ValidationOutput<T> {
    ValidationOutput {
        result: ValidationResult::default(),
        coerced: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        feedback::{
            Feedback, InputDiagnostic, ParseDiagnosticKind, SourceSpan, Type, ValidationDiagnostic,
            Violation,
        },
        validation::test_utils::get_test_store,
    };

    #[test]
    fn validate_yaml_err() {
        let input = "key3:\n  some_key: some_value\n";
        let store = get_test_store();
        let result = store.validate_yaml(input, "avd_design", None);
        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.input_diagnostics.is_empty());
        assert_eq!(output.documents.len(), 1);
        assert!(output.documents[0].result.infos.is_empty());
        assert_eq!(
            output.documents[0].result.errors,
            vec![Feedback {
                path: vec!["key3".into()].into(),
                span: Some(SourceSpan { start: 8, end: 28 }),
                issue: Violation::InvalidType {
                    expected: Type::Str,
                    found: Type::Dict
                }
                .into()
            },]
        )
    }

    #[test]
    fn validate_yaml_invalid_schema() {
        let input = "";
        let store = get_test_store();
        let result = store.validate_yaml(input, "invalid_schema", None);
        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.documents.is_empty());
        assert_eq!(
            output.input_diagnostics,
            vec![InputDiagnostic::InvalidSchema {
                schema: "invalid_schema".to_string()
            },]
        )
    }

    #[test]
    fn validate_yaml_parse_error_is_returned_as_feedback() {
        let input = "[\n---\nfoo: bar\n";
        let store = get_test_store();
        let result = store.validate_yaml(input, "avd_design", None);
        assert!(result.is_ok());
        let output = result.unwrap();

        assert!(output.input_diagnostics.iter().any(|diagnostic| {
            matches!(
                diagnostic,
                InputDiagnostic::YamlParse(parse_diagnostic)
                    if parse_diagnostic.kind == ParseDiagnosticKind::YamlSyntax
                        && parse_diagnostic.span.end >= parse_diagnostic.span.start
            )
        }));
        assert!(!output.documents.is_empty());
    }

    #[test]
    fn validate_yaml_multiple_documents() {
        let input = "foo: bar\n---\nkey3:\n  some_key: some_value\n";
        let store = get_test_store();
        let result = store.validate_yaml(input, "avd_design", None);
        assert!(result.is_ok());
        let output = result.unwrap();
        assert_eq!(output.documents.len(), 2);
        assert!(output.documents[0].result.errors.is_empty());
        assert_eq!(
            output.documents[1].result.errors,
            vec![Feedback {
                path: vec!["key3".into()].into(),
                span: Some(SourceSpan { start: 21, end: 41 }),
                issue: Violation::InvalidType {
                    expected: Type::Str,
                    found: Type::Dict
                }
                .into()
            },]
        );
    }

    #[test]
    fn validate_json_invalid_schema() {
        let input = "{}";
        let store = get_test_store();
        let result = store.validate_json(input, "invalid_schema", None);
        assert!(result.is_ok());
        let output = result.unwrap();
        assert_eq!(
            output.input_diagnostics,
            vec![InputDiagnostic::InvalidSchema {
                schema: "invalid_schema".to_string()
            },]
        );
        assert!(output.document.result.errors.is_empty());
    }

    #[test]
    fn validate_value_invalid_schema() {
        let input = serde_json::json!({});
        let store = get_test_store();
        let output = store.validate_value(&input, "invalid_schema", None);
        assert!(output.result.warnings.is_empty());
        assert!(output.result.errors.is_empty());
    }

    #[test]
    fn yaml_feedback_span_is_populated() {
        let input = "key3:\n  some_key: some_value\n";
        let store = get_test_store();
        let result = store.validate_yaml(input, "avd_design", None).unwrap();
        let Some(feedback) = result.documents[0].result.errors.first() else {
            panic!("expected validation feedback")
        };
        assert!(matches!(
            &feedback.issue,
            ValidationDiagnostic::Violation(_)
        ));
        assert!(feedback.span.is_some());
    }
}
