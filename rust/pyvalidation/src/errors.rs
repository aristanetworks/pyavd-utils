// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::PyErr;

use crate::exceptions;

#[derive(Debug, derive_more::From)]
pub(crate) enum InitStoreFromFilePyError {
    Load(avdschema::LoadError),
    SchemaResolver(avdschema::SchemaResolverError),
    StoreAlreadyInitialized,
}

impl From<InitStoreFromFilePyError> for PyErr {
    fn from(err: InitStoreFromFilePyError) -> Self {
        match err {
            InitStoreFromFilePyError::Load(err) => load_error_to_pyerr(err),
            InitStoreFromFilePyError::SchemaResolver(err) => schema_resolver_error_to_pyerr(err),
            InitStoreFromFilePyError::StoreAlreadyInitialized => {
                exceptions::ValidationStoreAlreadyInitializedError::new_err(
                    "Unable to initialize the schema store. \
                     Initialization can only happen once, and must be done before running any validations."
                        .to_owned(),
                )
            }
        }
    }
}

#[derive(Debug, derive_more::From)]
pub(crate) enum ValidateJsonPyError {
    StoreNotInitialized,
    StoreValidate(validation::StoreValidateError),
    InvalidJsonData(String),
    ValidationResult(PyErr),
}

impl From<ValidateJsonPyError> for PyErr {
    fn from(err: ValidateJsonPyError) -> Self {
        match err {
            ValidateJsonPyError::StoreNotInitialized => store_not_initialized_error_to_pyerr(),
            ValidateJsonPyError::StoreValidate(err) => store_validate_error_to_pyerr(err),
            ValidateJsonPyError::InvalidJsonData(message) => invalid_json_in_data_to_pyerr(message),
            ValidateJsonPyError::ValidationResult(err) => err,
        }
    }
}

#[derive(Debug, derive_more::From)]
pub(crate) enum GetValidatedDataPyError {
    StoreNotInitialized,
    StoreValidate(validation::StoreValidateError),
    InvalidJsonData(String),
    InvalidCoercedDataJson(serde_json::Error),
    ValidationResult(PyErr),
}

impl From<GetValidatedDataPyError> for PyErr {
    fn from(err: GetValidatedDataPyError) -> Self {
        match err {
            GetValidatedDataPyError::StoreNotInitialized => store_not_initialized_error_to_pyerr(),
            GetValidatedDataPyError::StoreValidate(err) => store_validate_error_to_pyerr(err),
            GetValidatedDataPyError::InvalidJsonData(message) => {
                invalid_json_in_data_to_pyerr(message)
            }
            GetValidatedDataPyError::InvalidCoercedDataJson(err) => {
                exceptions::ValidationInvalidCoercedDataJsonError::new_err(format!(
                    "Coerced validation output could not be serialized as JSON: {err}."
                ))
            }
            GetValidatedDataPyError::ValidationResult(err) => err,
        }
    }
}

#[derive(Debug)]
pub(crate) enum ValidateJsonWithAdhocSchemaPyError {
    StoreNotInitialized,
    InvalidJsonData(serde_json::Error),
    InvalidAdhocSchemaJson(serde_json::Error),
    ValidationResult(PyErr),
}

impl From<PyErr> for ValidateJsonWithAdhocSchemaPyError {
    fn from(err: PyErr) -> Self {
        Self::ValidationResult(err)
    }
}

impl From<ValidateJsonWithAdhocSchemaPyError> for PyErr {
    fn from(err: ValidateJsonWithAdhocSchemaPyError) -> Self {
        match err {
            ValidateJsonWithAdhocSchemaPyError::StoreNotInitialized => {
                store_not_initialized_error_to_pyerr()
            }
            ValidateJsonWithAdhocSchemaPyError::InvalidJsonData(err) => {
                invalid_json_in_data_to_pyerr(err)
            }
            ValidateJsonWithAdhocSchemaPyError::InvalidAdhocSchemaJson(err) => {
                exceptions::ValidationInvalidAdhocSchemaJsonError::new_err(format!(
                    "Invalid JSON in adhoc schema: {err}."
                ))
            }
            ValidateJsonWithAdhocSchemaPyError::ValidationResult(err) => err,
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

fn store_not_initialized_error_to_pyerr() -> PyErr {
    exceptions::ValidationStoreNotInitializedError::new_err(
        "The schema store was not initialized. \
         Initialization can only happen once, and must be done before running any validations."
            .to_owned(),
    )
}
