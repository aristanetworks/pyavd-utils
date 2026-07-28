// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::PyErr;

use crate::exceptions;

#[derive(Debug)]
pub enum ValidationPyError {
    Load(avdschema::LoadError),
    SchemaResolver(avdschema::SchemaResolverError),
    StoreValidate(validation::StoreValidateError),
    ValidationResult(PyErr),
    StoreAlreadyInitialized,
    StoreNotInitialized,
    InvalidJsonData(String),
    InvalidAdhocSchemaJson(serde_json::Error),
    InvalidCoercedDataJson(serde_json::Error),
}

impl From<PyErr> for ValidationPyError {
    fn from(err: PyErr) -> Self {
        Self::ValidationResult(err)
    }
}

impl From<avdschema::LoadError> for ValidationPyError {
    fn from(err: avdschema::LoadError) -> Self {
        Self::Load(err)
    }
}

impl From<avdschema::SchemaResolverError> for ValidationPyError {
    fn from(err: avdschema::SchemaResolverError) -> Self {
        Self::SchemaResolver(err)
    }
}

impl From<validation::StoreValidateError> for ValidationPyError {
    fn from(err: validation::StoreValidateError) -> Self {
        Self::StoreValidate(err)
    }
}

impl From<ValidationPyError> for PyErr {
    fn from(err: ValidationPyError) -> Self {
        match err {
            ValidationPyError::Load(err) => load_error_to_pyerr(err),
            ValidationPyError::SchemaResolver(err) => schema_resolver_error_to_pyerr(err),
            ValidationPyError::StoreValidate(err) => store_validate_error_to_pyerr(err),
            ValidationPyError::ValidationResult(err) => err,
            ValidationPyError::StoreAlreadyInitialized => {
                exceptions::ValidationStoreAlreadyInitializedError::new_err(
                    "Unable to initialize the schema store. \
                     Initialization can only happen once, and must be done before running any validations."
                        .to_owned(),
                )
            }
            ValidationPyError::StoreNotInitialized => store_not_initialized_error_to_pyerr(),
            ValidationPyError::InvalidJsonData(message) => invalid_json_in_data_to_pyerr(message),
            ValidationPyError::InvalidAdhocSchemaJson(err) => {
                invalid_adhoc_schema_json_to_pyerr(err)
            }
            ValidationPyError::InvalidCoercedDataJson(err) => {
                invalid_coerced_data_json_to_pyerr(err)
            }
        }
    }
}

fn load_error_to_pyerr(err: avdschema::LoadError) -> PyErr {
    match err {
        avdschema::LoadError::JsonError(err) => exceptions::ValidationStoreLoadJsonError::new_err(
            format!("Error while loading the Schema Store from file: {err}."),
        ),
        avdschema::LoadError::YamlError(err) => exceptions::ValidationStoreLoadYamlError::new_err(
            format!("Error while loading the Schema Store from file: {err}."),
        ),
        avdschema::LoadError::IoError(err) => exceptions::ValidationStoreLoadIoError::new_err(
            format!("Error while loading the Schema Store from file: {err}."),
        ),
        avdschema::LoadError::InvalidExtension {} => {
            exceptions::ValidationStoreInvalidExtensionError::new_err(
                "Error while loading the Schema Store from file: Invalid extension for input file.",
            )
        }
        avdschema::LoadError::NoFilesFound {} => {
            exceptions::ValidationStoreNoFilesFoundError::new_err(
                "Error while loading the Schema Store from file: No files found.",
            )
        }
    }
}

fn schema_store_error_to_pyerr(err: avdschema::SchemaStoreError) -> PyErr {
    let message = err.to_string();
    match err {
        avdschema::SchemaStoreError::InvalidSchemaName(_) => {
            exceptions::ValidationInvalidSchemaNameError::new_err(message)
        }
    }
}

fn schema_resolver_error_to_pyerr(err: avdschema::SchemaResolverError) -> PyErr {
    let message = format!("Error while resolving the Schema Store: {err}");
    match err {
        avdschema::SchemaResolverError::SchemaType(_) => {
            exceptions::ValidationSchemaTypeError::new_err(message)
        }
        avdschema::SchemaResolverError::RefSyntax(_) => {
            exceptions::ValidationRefSyntaxError::new_err(message)
        }
        avdschema::SchemaResolverError::SchemaPath(_) => {
            exceptions::ValidationSchemaPathError::new_err(message)
        }
        avdschema::SchemaResolverError::SchemaStoreError(err) => schema_store_error_to_pyerr(err),
        avdschema::SchemaResolverError::SchemaWalkError(_) => {
            exceptions::ValidationSchemaWalkError::new_err(message)
        }
    }
}

fn store_validate_error_to_pyerr(err: validation::StoreValidateError) -> PyErr {
    match err {
        validation::StoreValidateError::SchemaStore(err) => schema_store_error_to_pyerr(err),
    }
}

fn invalid_json_in_data_to_pyerr(message: impl std::fmt::Display) -> PyErr {
    exceptions::ValidationInvalidJsonDataError::new_err(format!("Invalid JSON in data: {message}."))
}

fn invalid_adhoc_schema_json_to_pyerr(err: serde_json::Error) -> PyErr {
    exceptions::ValidationInvalidAdhocSchemaJsonError::new_err(format!(
        "Invalid JSON in adhoc schema: {err}."
    ))
}

fn invalid_coerced_data_json_to_pyerr(err: serde_json::Error) -> PyErr {
    exceptions::ValidationInvalidCoercedDataJsonError::new_err(format!(
        "Coerced validation output could not be serialized as JSON: {err}."
    ))
}

fn store_not_initialized_error_to_pyerr() -> PyErr {
    exceptions::ValidationStoreNotInitializedError::new_err(
        "The schema store was not initialized. \
         Initialization can only happen once, and must be done before running any validations."
            .to_owned(),
    )
}
