// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::PyErr;

use crate::validation::exceptions;

#[derive(Debug)]
pub(crate) enum SchemaStorePyError {
    NotInitialized,
    AlreadyInitialized,
    Store(avdschema::StoreError),
    InvalidSchemaName(String),
    SchemaPath(avdschema::SchemaPathError),
}

impl From<avdschema::StoreError> for SchemaStorePyError {
    fn from(error: avdschema::StoreError) -> Self {
        Self::Store(error)
    }
}

impl From<avdschema::SchemaPathError> for SchemaStorePyError {
    fn from(error: avdschema::SchemaPathError) -> Self {
        Self::SchemaPath(error)
    }
}

impl From<SchemaStorePyError> for PyErr {
    fn from(error: SchemaStorePyError) -> Self {
        match error {
            SchemaStorePyError::NotInitialized => {
                exceptions::ValidationStoreNotInitializedError::new_err(
                    "The schema store was not initialized. \
                     Initialization can only happen once, and must be done before running any validations.",
                )
            }
            SchemaStorePyError::AlreadyInitialized => {
                exceptions::ValidationStoreAlreadyInitializedError::new_err(
                    "Unable to initialize the schema store. \
                     Initialization can only happen once, and must be done before running any validations.",
                )
            }
            SchemaStorePyError::Store(error) => match error {
                avdschema::StoreError::Io(error) => {
                    exceptions::ValidationStoreLoadIoError::new_err(format!(
                        "Error while loading the schema store: {error}"
                    ))
                }
                error => exceptions::ValidationStoreLoadError::new_err(format!(
                    "Error while loading the schema store: {error}"
                )),
            },
            SchemaStorePyError::InvalidSchemaName(name) => {
                exceptions::ValidationInvalidSchemaNameError::new_err(format!(
                    "Schema name '{name}' is not supported."
                ))
            }
            SchemaStorePyError::SchemaPath(error) => {
                exceptions::ValidationSchemaPathError::new_err(error.to_string())
            }
        }
    }
}
