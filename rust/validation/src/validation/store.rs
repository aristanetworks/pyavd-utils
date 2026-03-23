// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::Store;
use serde_json::Value;

use crate::{
    coercion::Coercion,
    context::{Configuration, Context, ValidationResult},
};
use log::debug;

use super::Validation;

pub trait StoreValidate {
    /// Entrypoint for validating a JSON document against the given schema name.
    fn validate_json(
        &self,
        json: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationResult, StoreValidateError>;

    /// Entrypoint for validating a YAML document against the given schema name.
    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationResult, StoreValidateError>;

    /// Entrypoint for validating a serde_json::Value against the given schema name.
    fn validate_value(
        &self,
        value: &mut Value,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationResult, StoreValidateError>;

    /// Coerce the given value recursively to match the types of the schema.
    /// Returns a ValidationResult where only coercions have been populated.
    ///
    /// Used by external tools to coerce the data and inserting default values
    /// before trying to resolve refs based on data paths.
    fn coerce_value(
        &self,
        value: &mut Value,
        schema_name: &str,
    ) -> Result<ValidationResult, StoreValidateError>;
}

impl StoreValidate for Store {
    fn validate_json(
        &self,
        json: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationResult, StoreValidateError> {
        debug!("Validating JSON");
        let mut value = serde_json::from_str(json)?;
        debug!("Deserialization of JSON Done");
        let result = self.validate_value(&mut value, schema_name, configuration)?;
        debug!("Validating JSON Done");
        Ok(result)
    }
    fn validate_yaml(
        &self,
        yaml: &str,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationResult, StoreValidateError> {
        debug!("Validating YAML");
        let mut value = serde_yaml::from_str::<Value>(yaml)?;
        debug!("Deserialization of YAML Done");
        let result = self.validate_value(&mut value, schema_name, configuration)?;
        debug!("Validating YAML Done");
        Ok(result)
    }
    fn validate_value(
        &self,
        value: &mut Value,
        schema_name: &str,
        configuration: Option<&Configuration>,
    ) -> Result<ValidationResult, StoreValidateError> {
        debug!("Validating serde_json::Value");
        let mut ctx = Context::new(self, configuration);
        let schema = self.get(schema_name)?;
        schema.coerce(value, &mut ctx);
        debug!("Validating serde_json::Value Coercion Done");
        schema.validate_value(value, &mut ctx);
        debug!("Validating serde_json::Value Done");
        Ok(ctx.result)
    }
    fn coerce_value(
        &self,
        value: &mut Value,
        schema_name: &str,
    ) -> Result<ValidationResult, StoreValidateError> {
        debug!("Coercing serde_json::Value");
        let mut ctx = Context::new(self, None);
        let schema = self.get(schema_name)?;
        schema.coerce(value, &mut ctx);
        debug!("Coercing serde_json::Value Done");
        Ok(ctx.result)
    }
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum StoreValidateError {
    SchemaStore(avdschema::SchemaStoreError),
    Json(serde_json::Error),
    Yaml(serde_yaml::Error),
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        feedback::{Feedback, Type, Violation},
        validation::test_utils::get_test_store,
    };

    #[test]
    fn validate_yaml_err() {
        let input = "key3:\n  some_key: some_value\n";
        let store = get_test_store();
        let result = store.validate_yaml(input, "avd_design", None);
        assert!(result.is_ok());
        let validation_result = result.unwrap();
        assert!(validation_result.infos.is_empty());
        assert_eq!(
            validation_result.errors,
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
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("invalid_schema"));
    }
    #[test]
    fn validate_value_invalid_schema() {
        let mut input = serde_json::json!({});
        let store = get_test_store();
        let result = store.validate_value(&mut input, "invalid_schema", None);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("invalid_schema"));
    }
}
