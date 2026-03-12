// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::{Schema, Store};
use serde_json::Value;
use yaml_parser::{Node, parse};

use crate::{
    context::{Configuration, Context, ValidationResult},
    feedback::Violation,
};
use log::debug;

use super::Validation;

/// Result of validation including optionally coerced data.
pub struct ValidationOutput<T> {
    /// The validation result with errors, warnings, and infos.
    pub result: ValidationResult,
    /// The coerced data with types adjusted according to the schema.
    /// Only populated when `Configuration::return_coerced_data` is true.
    pub coerced: Option<T>,
}

pub trait StoreValidate<T> {
    /// Entrypoint for validating a JSON document against the given schema name.
    /// Returns both validation results and the coerced JSON Value.
    fn validate_json(
        &self,
        json: &str,
        schema_name: T,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<Value>, StoreValidateError>;

    /// Entrypoint for validating a YAML document against the given schema name.
    /// Returns both validation results and the coerced YAML Node.
    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: T,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<Node<'static>>, StoreValidateError>;

    /// Entrypoint for validating a serde_json::Value against the given schema name.
    /// Returns both validation results and the coerced Value.
    fn validate_value(
        &self,
        value: &Value,
        schema_name: T,
        configuration: Option<&Configuration>,
    ) -> ValidationOutput<Value>;
}

impl StoreValidate<Schema> for Store {
    fn validate_json(
        &self,
        json: &str,
        schema_name: Schema,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<Value>, StoreValidateError> {
        debug!("Validating JSON");
        let value: Value = serde_json::from_str(json)?;
        debug!("Deserialization of JSON Done");
        let output = self.validate_value(&value, schema_name, configuration);
        debug!("Validating JSON Done");
        Ok(output)
    }

    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: Schema,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<Node<'static>>, StoreValidateError> {
        debug!("Validating YAML");
        let mut value = yaml_parser::Value::from_str(yaml)?;
        debug!("Deserialization of YAML Done");
        let result = self.validate_value(&mut value, schema_name, configuration);
        debug!("Validating YAML Done");
        Ok(ValidationOutput {
            result: ctx.result,
            coerced,
        })
    }

    fn validate_value(
        &self,
        value: &Value,
        schema_name: Schema,
        configuration: Option<&Configuration>,
    ) -> ValidationOutput<Value> {
        debug!("Validating serde_json::Value");
        let mut ctx = Context::new(self, configuration);
        let schema = self.get(schema_name);
        let coerced = schema.validate(value, &mut ctx);
        debug!("Validating serde_json::Value Done");
        ValidationOutput {
            result: ctx.result,
            coerced,
        }
    }
}

impl StoreValidate<&str> for Store {
    fn validate_json(
        &self,
        json: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<Value>, StoreValidateError> {
        with_resolved_schema_json_output(schema_name, self, |schema_type| {
            self.validate_json(json, schema_type, configuration)
        })
    }

    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationOutput<Node<'static>>, StoreValidateError> {
        with_resolved_schema_yaml_output(schema_name, self, |schema_type| {
            self.validate_yaml(yaml, schema_type, configuration)
        })
    }

    fn validate_value(
        &self,
        value: &Value,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> ValidationOutput<Value> {
        with_resolved_schema_value_output(schema_name, self, |schema_type| {
            self.validate_value(value, schema_type, configuration)
        })
    }
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum StoreValidateError {
    JsonError(serde_json::Error),
    YamlParseError(yaml_parser::ParseError),
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

impl From<ValidationResult> for Result<ValidationResult, StoreValidateError> {
    fn from(result: ValidationResult) -> Self {
        Ok(result)
    }
}

fn with_resolved_schema_json_output<
    F: FnOnce(Schema) -> Result<ValidationOutput<Value>, StoreValidateError>,
>(
    schema_name: &str,
    store: &Store,
    func: F,
) -> Result<ValidationOutput<Value>, StoreValidateError> {
    match Schema::try_from(schema_name) {
        Ok(schema_type) => func(schema_type),
        Err(_) => Ok(ValidationOutput {
            result: SchemaConversionError::InvalidSchemaName(schema_name.to_string())
                .to_validation_result(store),
            coerced: None,
        }),
    }
}

fn with_resolved_schema_yaml_output<
    F: FnOnce(Schema) -> Result<ValidationOutput<Node<'static>>, StoreValidateError>,
>(
    schema_name: &str,
    store: &Store,
    func: F,
) -> Result<ValidationOutput<Node<'static>>, StoreValidateError> {
    match Schema::try_from(schema_name) {
        Ok(schema_type) => func(schema_type),
        Err(_) => Ok(ValidationOutput {
            result: SchemaConversionError::InvalidSchemaName(schema_name.to_string())
                .to_validation_result(store),
            coerced: None,
        }),
    }
}

fn with_resolved_schema_value_output<F: FnOnce(Schema) -> ValidationOutput<Value>>(
    schema_name: &str,
    store: &Store,
    func: F,
) -> ValidationOutput<Value> {
    match Schema::try_from(schema_name) {
        Ok(schema_type) => func(schema_type),
        Err(_) => ValidationOutput {
            result: SchemaConversionError::InvalidSchemaName(schema_name.to_string())
                .to_validation_result(store),
            coerced: None,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        feedback::{Feedback, Type},
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
