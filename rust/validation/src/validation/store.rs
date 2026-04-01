// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::Store;
use log::debug;
use serde_json::Value;
use yaml_parser::{Node, parse};

use crate::{
    context::{Configuration, Context, ValidationResult},
    feedback::{InputDiagnostic, ParseDiagnostic},
    validatable::ValidatableValue,
};

use super::Validation;

#[derive(Debug)]
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

#[derive(Debug)]
/// Result of validating a YAML input source that may contain multiple documents.
pub struct YamlValidationResult<T> {
    pub input_diagnostics: Vec<InputDiagnostic>,
    pub documents: Vec<ValidationOutput<T>>,
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

/// Parse JSON or YAML input and validate the resulting value against a schema.
pub trait StoreValidateInput {
    fn validate_json(
        &self,
        json: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<InputValidationResult<Value>, StoreValidateError>;

    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<YamlValidationResult<Node<'static>>, StoreValidateError>;
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
                    document: empty_validation_output(),
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

    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<YamlValidationResult<Node<'static>>, StoreValidateError> {
        debug!("Validating YAML");
        let _ = self.get(schema_name)?;
        let (yaml_docs, parse_errors) = parse(yaml);
        debug!("Deserialization of YAML done");

        let input_diagnostics = parse_errors
            .into_iter()
            .map(|parse_error| InputDiagnostic::ParseDiagnostic(ParseDiagnostic::from(parse_error)))
            .collect();

        let mut documents = Vec::with_capacity(yaml_docs.len());
        for document in &yaml_docs {
            documents.push(<Store as StoreValidate<Node<'static>>>::validate_value(
                self,
                document,
                schema_name,
                configuration,
            )?);
        }

        debug!("Validating YAML done");
        Ok(YamlValidationResult {
            input_diagnostics,
            documents,
        })
    }
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum StoreValidateError {
    SchemaStore(avdschema::SchemaStoreError),
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
        feedback::{Feedback, ParseDiagnosticKind, SourceSpan, Type, ValidationIssue, Violation},
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
        assert!(matches!(
            result,
            Err(StoreValidateError::SchemaStore(
                avdschema::SchemaStoreError::InvalidSchemaName(schema)
            ))
                if schema == "invalid_schema"
        ));
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
                InputDiagnostic::ParseDiagnostic(parse_diagnostic)
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

    #[test]
    fn yaml_feedback_span_is_populated() {
        let input = "key3:\n  some_key: some_value\n";
        let store = get_test_store();
        let result = store.validate_yaml(input, "avd_design", None).unwrap();
        let Some(feedback) = result.documents[0].result.errors.first() else {
            panic!("expected validation feedback")
        };
        assert!(matches!(&feedback.issue, ValidationIssue::Violation(_)));
        assert!(feedback.span.is_some());
    }
}
