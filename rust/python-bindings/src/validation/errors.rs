// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::PyErr;

use super::exceptions;

#[derive(Debug)]
pub(crate) enum ValidationPyError {
    StoreNotInitialized,
    StoreValidate(::validation::StoreValidateError),
    InvalidJsonData(String),
    InvalidAdhocSchema(String),
    InvalidCoercedDataJson(serde_json::Error),
    ValidationResult(PyErr),
}

impl From<::validation::StoreValidateError> for ValidationPyError {
    fn from(error: ::validation::StoreValidateError) -> Self {
        Self::StoreValidate(error)
    }
}

impl From<PyErr> for ValidationPyError {
    fn from(error: PyErr) -> Self {
        Self::ValidationResult(error)
    }
}

impl From<crate::schema_store::errors::SchemaStorePyError> for ValidationPyError {
    fn from(error: crate::schema_store::errors::SchemaStorePyError) -> Self {
        match error {
            crate::schema_store::errors::SchemaStorePyError::NotInitialized => {
                Self::StoreNotInitialized
            }
            error => Self::ValidationResult(error.into()),
        }
    }
}

impl From<ValidationPyError> for PyErr {
    fn from(error: ValidationPyError) -> Self {
        match error {
            ValidationPyError::StoreNotInitialized => {
                exceptions::ValidationStoreNotInitializedError::new_err(
                    "The schema store was not initialized. \
                     Initialization can only happen once, and must be done before running any validations.",
                )
            }
            ValidationPyError::StoreValidate(error) => match error {
                ::validation::StoreValidateError::SchemaStore(error) => match error {
                    avdschema::SchemaStoreError::InvalidSchemaName(name) => {
                        exceptions::ValidationInvalidSchemaNameError::new_err(format!(
                            "Schema name '{name}' not found in the schema store."
                        ))
                    }
                },
            },
            ValidationPyError::InvalidJsonData(message) => {
                exceptions::ValidationInvalidJsonDataError::new_err(format!(
                    "Invalid JSON in data: {message}"
                ))
            }
            ValidationPyError::InvalidAdhocSchema(error) => {
                exceptions::ValidationInvalidAdhocSchemaJsonError::new_err(error)
            }
            ValidationPyError::InvalidCoercedDataJson(error) => {
                exceptions::ValidationInvalidCoercedDataJsonError::new_err(format!(
                    "Coerced validation output could not be serialized as JSON: {error}"
                ))
            }
            ValidationPyError::ValidationResult(error) => error,
        }
    }
}
