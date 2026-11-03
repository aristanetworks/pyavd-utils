// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::Store;
use log::debug;
use serde_json::Value;
use yaml_parser::Node;
use yaml_parser::parse_with_source;

use crate::context::Configuration;
use crate::context::Context;
use crate::context::ValidationResult;
use crate::feedback::InputDiagnostic;
use crate::feedback::ParseDiagnostic;
use crate::validatable::ValidatableValue;
use crate::walker::Validator;

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
    /// Entrypoint for validating a value implementing `ValidatableValue` against the given schema name.
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

    /// Parse and validate YAML while preserving the caller-provided source identifier.
    fn validate_yaml_with_source(
        &self,
        yaml: &str,
        source: Option<std::sync::Arc<str>>,
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
        let mut ctx = Context::new(configuration);
        let schema = self.get(schema_name).ok_or_else(|| {
            avdschema::SchemaStoreError::InvalidSchemaName(schema_name.to_owned())
        })?;
        let coerced = Validator::new(self, &mut ctx).validate(schema, value);
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
        let _ = self.get(schema_name).ok_or_else(|| {
            avdschema::SchemaStoreError::InvalidSchemaName(schema_name.to_owned())
        })?;
        let value: Value = match serde_json::from_str(json) {
            Ok(value) => value,
            Err(parse_error) => {
                return Ok(InputValidationResult {
                    input_diagnostics: vec![InputDiagnostic::ParseDiagnostic(
                        ParseDiagnostic::from_source(&parse_error),
                    )],
                    document: ValidationOutput::default(),
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
        self.validate_yaml_with_source(yaml, None, schema_name, configuration)
    }

    fn validate_yaml_with_source(
        &self,
        yaml: &str,
        source: Option<std::sync::Arc<str>>,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<YamlValidationResult<Node<'static>>, StoreValidateError> {
        debug!("Validating YAML");
        let _ = self.get(schema_name).ok_or_else(|| {
            avdschema::SchemaStoreError::InvalidSchemaName(schema_name.to_owned())
        })?;
        let (yaml_docs, parse_errors) = parse_with_source(yaml, source);
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
    use std::collections::HashMap;

    use super::*;
    use crate::feedback::Feedback;
    use crate::feedback::ParseDiagnosticKind;
    use crate::feedback::SourceSpan;
    use crate::feedback::Type;
    use crate::feedback::Violation;
    use crate::validation::test_utils::get_test_store;

    fn get_test_archive() -> Store {
        Store::compile(&get_test_store()).expect("test schema should compile")
    }

    fn get_null_contract_archive() -> Store {
        Store::from_json(
            r#"{
                "test": {
                    "type": "dict",
                    "keys": {
                        "boolean": {"type": "bool"},
                        "integer": {"type": "int"},
                        "string": {"type": "str"},
                        "list": {"type": "list", "items": {"type": "str"}},
                        "dict": {"type": "dict", "keys": {}}
                    }
                }
            }"#,
        )
        .expect("null contract schema should compile")
    }

    #[test]
    fn validate_yaml_err() {
        let input = "key3:\n  some_key: some_value\n";
        let store = get_test_archive();
        let result = store.validate_yaml(input, "avd_design", None);
        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.input_diagnostics.is_empty());
        assert_eq!(output.documents.len(), 1);
        assert!(output.documents[0].result.infos.is_empty());
        assert_eq!(
            output.documents[0].result.errors,
            vec![Feedback {
                source: None,
                path: vec!["key3".into()].into(),
                span: Some(SourceSpan { start: 8, end: 28 }),
                issue: Violation::InvalidType {
                    expected: Type::Str,
                    found: Type::Dict
                }
                .into()
            },]
        );
    }

    #[test]
    fn validate_yaml_invalid_schema() {
        let input = "";
        let store = get_test_archive();
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
        let store = get_test_archive();
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
        let store = get_test_archive();
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
        let store = get_test_archive();
        let result = store.validate_yaml(input, "avd_design", None);
        assert!(result.is_ok());
        let output = result.unwrap();
        assert_eq!(output.documents.len(), 2);
        assert!(output.documents[0].result.errors.is_empty());
        assert_eq!(
            output.documents[1].result.errors,
            vec![Feedback {
                source: None,
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
        let store = get_test_archive();
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
                yaml_parser::Span::new(6..9),
                None
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
                yaml_parser::Span::new(20..23),
                None
            ))
        );
    }

    #[test]
    fn validate_json_invalid_schema() {
        let input = "{}";
        let store = get_test_archive();
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
        let store = get_test_archive();
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
        let store = get_test_archive();
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
        let store = get_test_archive();
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
        let store = get_test_archive();
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
    fn validate_null_contract_for_all_schema_types() {
        let input = serde_json::json!({
            "boolean": null,
            "integer": null,
            "string": null,
            "list": null,
            "dict": null
        });
        let store = get_null_contract_archive();
        let allow_nulls = Configuration {
            return_coerced_data: true,
            ..Default::default()
        };

        let allowed = store
            .validate_value(&input, "test", Some(&allow_nulls))
            .unwrap();
        assert!(allowed.result.errors.is_empty());
        assert_eq!(allowed.coerced, Some(input.clone()));

        let restrict_nulls = Configuration {
            restrict_null_values: true,
            return_coerced_data: true,
            ..Default::default()
        };
        let restricted = store
            .validate_value(&input, "test", Some(&restrict_nulls))
            .unwrap();
        let errors_by_path = restricted
            .result
            .errors
            .iter()
            .map(|feedback| (feedback.path.to_string(), &feedback.issue))
            .collect::<HashMap<_, _>>();
        assert_eq!(errors_by_path.len(), 5);
        for (key, expected) in [
            ("boolean", Type::Bool),
            ("integer", Type::Int),
            ("string", Type::Str),
            ("list", Type::List),
            ("dict", Type::Dict),
        ] {
            assert!(matches!(
                errors_by_path.get(key),
                Some(issue)
                    if **issue
                        == Violation::InvalidType {
                            expected,
                            found: Type::Null,
                        }
                        .into()
            ));
        }
        assert_eq!(restricted.coerced, Some(input));
    }

    #[test]
    fn relaxed_reference_suppresses_nested_required_key_errors() {
        let store = Store::from_json(
            r#"{
                "base": {
                    "type": "dict",
                    "keys": {"required": {"type": "str", "required": true}}
                },
                "test": {
                    "type": "dict",
                    "keys": {
                        "strict": {"type": "dict", "$ref": "base#"},
                        "relaxed": {
                            "type": "dict",
                            "$ref": "base#",
                            "relaxed_validation": true
                        }
                    }
                }
            }"#,
        )
        .unwrap();
        let output = store
            .validate_value(
                &serde_json::json!({"strict": {}, "relaxed": {}}),
                "test",
                None,
            )
            .unwrap();

        assert_eq!(
            output.result.errors,
            vec![Feedback {
                source: None,
                path: "strict".into(),
                span: None,
                issue: Violation::MissingRequiredKey {
                    key: "required".to_owned(),
                }
                .into(),
            }]
        );
    }

    #[test]
    fn yaml_feedback_span_is_populated() {
        let input = "key3:\n  some_key: some_value\n";
        let store = get_test_archive();
        let result = store.validate_yaml(input, "avd_design", None).unwrap();
        let Some(feedback) = result.documents[0].result.errors.first() else {
            panic!("expected validation feedback")
        };
        assert!(feedback.span.is_some());
    }

    #[test]
    fn yaml_feedback_distinguishes_key_value_and_container_sources() {
        let store = Store::from_json(
            r#"{"test": {"type": "dict", "keys": {
            "number": {"type": "int"},
            "old": {"type": "str", "deprecation": {"warning": true}},
            "gone": {"type": "str", "deprecation": {"warning": true, "removed": true}},
            "required": {"type": "str", "required": true}
        }}}"#,
        )
        .unwrap();
        let (mut documents, errors) = parse_with_source(
            "number: nope\nold: value\ngone: value\nunexpected: value\n",
            Some("container-fixture".into()),
        );
        assert!(errors.is_empty());
        let document = &mut documents[0];
        let yaml_parser::Value::Mapping(pairs) = &mut document.value else {
            panic!("expected mapping");
        };
        // Model a merged entry whose key and value came from different fragments.
        for pair in pairs {
            pair.key.source = Some("key-fixture".into());
            pair.value.source = Some("value-fixture".into());
        }
        let output = store.validate_value(document, "test", None).unwrap();
        assert_eq!(output.result.errors.len(), 4);
        for error in output.result.errors {
            let expected_source = match error.issue {
                crate::feedback::ErrorIssue::Violation(Violation::MissingRequiredKey {
                    ..
                }) => "container-fixture",
                crate::feedback::ErrorIssue::Violation(
                    Violation::UnexpectedKey() | Violation::Removed(_),
                ) => "key-fixture",
                _ => "value-fixture",
            };
            assert_eq!(error.source.as_deref(), Some(expected_source));
            assert!(error.span.is_some());
        }
        assert_eq!(output.result.warnings.len(), 1);
        assert_eq!(
            output.result.warnings[0].source.as_deref(),
            Some("key-fixture")
        );
        assert!(output.result.warnings[0].span.is_some());
    }

    #[test]
    fn source_aware_yaml_validation_preserves_sources_in_feedback_and_coerced_output() {
        let source: std::sync::Arc<str> = "fixture-a".into();
        let config = Configuration {
            return_coerced_data: true,
            return_coercion_infos: true,
            ..Default::default()
        };
        let store = get_test_archive();
        let result = store
            .validate_yaml_with_source(
                "key3: 123",
                Some(source.clone()),
                "avd_design",
                Some(&config),
            )
            .unwrap();
        let document = &result.documents[0];
        assert_eq!(document.result.infos.len(), 1);
        assert_eq!(
            document.result.infos[0].source.as_deref(),
            Some("fixture-a")
        );
        assert!(document.result.infos[0].span.is_some());
        let coerced = document.coerced.as_ref().unwrap();
        assert!(std::sync::Arc::ptr_eq(
            coerced.source.as_ref().unwrap(),
            &source
        ));
        let yaml_parser::Value::Mapping(pairs) = &coerced.value else {
            panic!("expected mapping");
        };
        assert!(std::sync::Arc::ptr_eq(
            pairs[0].key.source.as_ref().unwrap(),
            &source
        ));
        assert!(std::sync::Arc::ptr_eq(
            pairs[0].value.source.as_ref().unwrap(),
            &source
        ));

        let json = store
            .validate_json(r#"{"key3": {}}"#, "avd_design", None)
            .unwrap();
        assert!(!json.document.result.errors.is_empty());
        for feedback in json.document.result.errors {
            assert!(feedback.source.is_none());
            assert!(feedback.span.is_none());
        }
    }

    #[test]
    fn duplicate_keys_keep_each_key_source() {
        let store =
            Store::from_json(r#"{"test": {"type": "dict", "keys": {"number": {"type": "int"}}}}"#)
                .unwrap();
        let (mut documents, errors) = yaml_parser::parse("number: 1\nnumber: 2\n");
        assert!(errors.is_empty());
        let document = &mut documents[0];
        let yaml_parser::Value::Mapping(pairs) = &mut document.value else {
            panic!("expected mapping");
        };
        pairs[0].key.source = Some("fixture-a".into());
        pairs[1].key.source = Some("fixture-b".into());
        let output = store.validate_value(document, "test", None).unwrap();
        assert_eq!(output.result.errors.len(), 2);
        let sources: Vec<_> = output
            .result
            .errors
            .iter()
            .map(|feedback| feedback.source.as_deref())
            .collect();
        assert_eq!(sources, vec![Some("fixture-a"), Some("fixture-b")]);
    }

    #[test]
    fn uniqueness_feedback_preserves_both_sources() {
        let store = Store::from_json(
            r#"{"test": {"type": "dict", "keys": {
            "items": {"type": "list", "primary_key": "name", "items": {
                "type": "dict", "keys": {"name": {"type": "str"}}
            }}
        }}}"#,
        )
        .unwrap();
        let (mut documents, errors) = parse_with_source(
            "items: [{name: same}, {name: same}]",
            Some("fixture-a".into()),
        );
        assert!(errors.is_empty());
        let document = &mut documents[0];
        let yaml_parser::Value::Mapping(root) = &mut document.value else {
            panic!("expected mapping");
        };
        let yaml_parser::Value::Sequence(items) = &mut root[0].value.value else {
            panic!("expected sequence");
        };
        let yaml_parser::Value::Mapping(second) = &mut items[1].node.value else {
            panic!("expected mapping");
        };
        second[0].value.source = Some("fixture-b".into());
        let output = store.validate_value(document, "test", None).unwrap();
        assert_eq!(output.result.errors.len(), 2);
        for (feedback, (source, other)) in output
            .result
            .errors
            .iter()
            .zip([("fixture-a", "fixture-b"), ("fixture-b", "fixture-a")])
        {
            assert_eq!(feedback.source.as_deref(), Some(source));
            let crate::feedback::ErrorIssue::Violation(Violation::ValueNotUnique {
                other_source,
                other_span,
                ..
            }) = &feedback.issue
            else {
                panic!("expected uniqueness violation");
            };
            assert_eq!(other_source.as_deref(), Some(other));
            assert!(feedback.span.is_some());
            assert!(other_span.is_some());
        }
    }
}
