// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::Store;
use log::debug;
use serde_json::Value;
use yaml_parser::Node;
use yaml_parser::parse;

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
                        ParseDiagnostic::from_source(&parse_error),
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
            .map(|parse_error| {
                InputDiagnostic::ParseDiagnostic(ParseDiagnostic::from_source(&parse_error))
            })
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::feedback::Feedback;
    use crate::feedback::ParseDiagnosticKind;
    use crate::feedback::SourceSpan;
    use crate::feedback::Type;
    use crate::feedback::Violation;
    use crate::validation::test_utils::get_test_store;

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
                        && parse_diagnostic
                            .to_source_span(input)
                            .end
                            >= parse_diagnostic.to_source_span(input).start
            )
        }));
        assert!(!output.documents.is_empty());
    }

    #[test]
    fn validate_yaml_parse_error_without_document_is_returned_as_feedback() {
        let input = "*undefined_alias";
        let store = get_test_store();
        let result = store.validate_yaml(input, "avd_design", None);
        assert!(result.is_ok());
        let output = result.unwrap();

        assert!(output.input_diagnostics.iter().any(|diagnostic| {
            matches!(
                diagnostic,
                InputDiagnostic::ParseDiagnostic(parse_diagnostic)
                    if parse_diagnostic.kind == ParseDiagnosticKind::YamlSyntax
                        && parse_diagnostic
                            .to_source_span(input)
                            .end
                            >= parse_diagnostic.to_source_span(input).start
            )
        }));
        assert!(output.documents.is_empty());
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
    fn validate_yaml_ok_with_coerced_data() {
        let input = "key3: 123\n---\nkey3: 456\n";
        let store = get_test_store();
        let configuration = Configuration {
            return_coerced_data: true,
            return_coercion_infos: true,
            ..Default::default()
        };
        let result = store
            .validate_yaml(input, "avd_design", Some(&configuration))
            .unwrap();

        assert!(result.input_diagnostics.is_empty());
        assert_eq!(result.documents.len(), 2);
        assert!(result.documents[0].result.errors.is_empty());
        assert_eq!(
            result.documents[0]
                .coerced
                .as_ref()
                .and_then(|node| node.get("key3")),
            Some(&Node::new(
                yaml_parser::Value::String("123".to_owned().into()),
                yaml_parser::Span::new(6..9)
            ))
        );
        assert!(result.documents[1].result.errors.is_empty());
        assert_eq!(
            result.documents[1]
                .coerced
                .as_ref()
                .and_then(|node| node.get("key3")),
            Some(&Node::new(
                yaml_parser::Value::String("456".to_owned().into()),
                yaml_parser::Span::new(20..23)
            ))
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
    fn validate_json_ok_with_coerced_data() {
        let input = r#"{"key3":123}"#;
        let store = get_test_store();
        let configuration = Configuration {
            return_coerced_data: true,
            return_coercion_infos: true,
            ..Default::default()
        };
        let result = store
            .validate_json(input, "avd_design", Some(&configuration))
            .unwrap();

        assert!(result.input_diagnostics.is_empty());
        assert!(result.document.result.errors.is_empty());
        assert_eq!(
            result.document.coerced,
            Some(serde_json::json!({ "key3": "123" }))
        );
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
                    && parse_diagnostic.to_source_span(input).start <= input.len()
                    && parse_diagnostic.to_source_span(input).end <= input.len()
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
    fn validate_value_ok_with_coerced_data() {
        let input = serde_json::json!({ "key3": 123 });
        let store = get_test_store();
        let configuration = Configuration {
            return_coerced_data: true,
            return_coercion_infos: true,
            ..Default::default()
        };
        let result = store
            .validate_value(&input, "avd_design", Some(&configuration))
            .unwrap();

        assert!(result.result.errors.is_empty());
        assert_eq!(result.coerced, Some(serde_json::json!({ "key3": "123" })));
    }

    #[test]
    fn yaml_feedback_span_is_populated() {
        let input = "key3:\n  some_key: some_value\n";
        let store = get_test_store();
        let result = store.validate_yaml(input, "avd_design", None).unwrap();
        let Some(feedback) = result.documents[0].result.errors.first() else {
            panic!("expected validation feedback")
        };
        assert!(feedback.span.is_some());
    }
}
