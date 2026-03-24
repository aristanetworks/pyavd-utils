// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::{Schema, Store};
use log::debug;
use serde_json::Value;
use yaml_parser::{Node, Value as YamlValue, parse};

use crate::{
    context::{Configuration, Context, ValidationResult},
    feedback::{ErrorIssue, Feedback, ParseDiagnostic, Violation},
    validatable::ValidatableValue,
};

use super::Validation;

/// Result of validation including optionally coerced data.
pub struct ValidationOutput<T> {
    /// The validation result with errors, warnings, and infos.
    pub result: ValidationResult,
    /// The coerced data with types adjusted according to the schema.
    /// Only populated when `Configuration::return_coerced_data` is true.
    pub coerced: Option<T>,
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
    ) -> Result<ValidationOutput<Value>, StoreValidateError>;

    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: S,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<Node<'static>>, StoreValidateError>;
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
            Err(_) => invalid_schema_output(schema_name, self),
        }
    }
}

impl StoreValidateInput<Schema> for Store {
    fn validate_json(
        &self,
        json: &str,
        schema_name: Schema,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<Value>, StoreValidateError> {
        debug!("Validating JSON");
        let value: Value = serde_json::from_str(json)?;
        debug!("Deserialization of JSON done");
        let output = <Store as StoreValidate<Schema, Value>>::validate_value(
            self,
            &value,
            schema_name,
            configuration,
        );
        debug!("Validating JSON done");
        Ok(output)
    }

    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: Schema,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<Node<'static>>, StoreValidateError> {
        debug!("Validating YAML");
        let (yaml_docs, parse_errors) = parse(yaml);
        debug!("Deserialization of YAML done");

        let parse_feedback: Vec<Feedback<ErrorIssue>> = parse_errors
            .into_iter()
            .map(|parse_error| Feedback {
                path: Default::default(),
                issue: ParseDiagnostic::from(parse_error).into(),
            })
            .collect();

        let mut output = if let Some(value) = yaml_docs.into_iter().next() {
            <Store as StoreValidate<Schema, Node<'static>>>::validate_value(
                self,
                &value,
                schema_name,
                configuration,
            )
        } else if parse_feedback.is_empty() {
            // Preserve the old "empty input becomes null" behavior from serde_yaml.
            let value = Node::new(YamlValue::Null, Default::default());
            <Store as StoreValidate<Schema, Node<'static>>>::validate_value(
                self,
                &value,
                schema_name,
                configuration,
            )
        } else {
            ValidationOutput {
                result: ValidationResult::default(),
                coerced: None,
            }
        };

        if !parse_feedback.is_empty() {
            let mut errors = parse_feedback;
            errors.extend(output.result.errors);
            output.result.errors = errors;
        }
        debug!("Validating YAML done");
        Ok(output)
    }
}

impl StoreValidateInput<&str> for Store {
    fn validate_json(
        &self,
        json: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<Value>, StoreValidateError> {
        match Schema::try_from(schema_name) {
            Ok(schema_type) => <Store as StoreValidateInput<Schema>>::validate_json(
                self,
                json,
                schema_type,
                configuration,
            ),
            Err(_) => Ok(invalid_schema_output(schema_name, self)),
        }
    }

    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<Node<'static>>, StoreValidateError> {
        match Schema::try_from(schema_name) {
            Ok(schema_type) => <Store as StoreValidateInput<Schema>>::validate_yaml(
                self,
                yaml,
                schema_type,
                configuration,
            ),
            Err(_) => Ok(invalid_schema_output(schema_name, self)),
        }
    }
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum StoreValidateError {
    JsonError(serde_json::Error),
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum SchemaConversionError {
    InvalidSchemaName(String),
}

impl SchemaConversionError {
    pub fn get_invalid_schema_name(&self) -> String {
        match self {
            SchemaConversionError::InvalidSchemaName(s) => s.clone(),
        }
    }

    pub fn to_validation_result(&self, store: &Store) -> ValidationResult {
        let mut ctx = Context::new(store, None);
        ctx.add_error(Violation::InvalidSchema {
            schema: self.get_invalid_schema_name(),
        });
        ctx.result
    }
}

fn invalid_schema_output<T>(schema_name: &str, store: &Store) -> ValidationOutput<T> {
    ValidationOutput {
        result: SchemaConversionError::InvalidSchemaName(schema_name.to_string())
            .to_validation_result(store),
        coerced: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        feedback::{ErrorIssue, Feedback, ParseDiagnosticKind, Type},
        validation::test_utils::get_test_store,
    };

    #[test]
    fn validate_yaml_err() {
        let input = "key3:\n  some_key: some_value\n";
        let store = get_test_store();
        let result = store.validate_yaml(input, "avd_design", None);
        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.result.infos.is_empty());
        assert_eq!(
            output.result.errors,
            vec![Feedback {
                path: vec!["key3".into()].into(),
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
        assert!(output.result.warnings.is_empty());
        assert_eq!(
            output.result.errors,
            vec![Feedback {
                path: Default::default(),
                issue: Violation::InvalidSchema {
                    schema: "invalid_schema".to_string()
                }
                .into()
            },]
        )
    }

    #[test]
    fn validate_yaml_parse_error_is_returned_as_feedback() {
        let input = "[";
        let store = get_test_store();
        let result = store.validate_yaml(input, "avd_design", None);
        assert!(result.is_ok());
        let output = result.unwrap();

        assert!(output.result.errors.iter().any(|feedback| {
            matches!(
                &feedback.issue,
                ErrorIssue::Parse(parse_diagnostic)
                    if parse_diagnostic.kind == ParseDiagnosticKind::YamlSyntax
                        && parse_diagnostic.span.end >= parse_diagnostic.span.start
            )
        }));
    }

    #[test]
    fn validate_value_invalid_schema() {
        let input = serde_json::json!({});
        let store = get_test_store();
        let output = store.validate_value(&input, "invalid_schema", None);
        assert!(output.result.warnings.is_empty());
        assert_eq!(
            output.result.errors,
            vec![Feedback {
                path: Default::default(),
                issue: Violation::InvalidSchema {
                    schema: "invalid_schema".to_string()
                }
                .into()
            },]
        )
    }
}
