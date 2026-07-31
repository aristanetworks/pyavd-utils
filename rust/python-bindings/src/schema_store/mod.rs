// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::path::PathBuf;
use std::sync::OnceLock;

use avdschema::GetSchemaFromPathError;
use avdschema::Load as _;
use avdschema::SchemaStoreError;
use avdschema::Store;
use avdschema::get_list_primary_key as get_avdschema_list_primary_key;
use log::info;
use pyo3::PyResult;
use pyo3::exceptions::PyRuntimeError;
use pyo3::pyfunction;

pub(crate) static STORE: OnceLock<Store> = OnceLock::new();

pub(crate) fn get_store() -> PyResult<&'static Store> {
    STORE.get().ok_or_else(|| {
        PyRuntimeError::new_err(
            "The schema store was not initialized. \
             Initialization can only happen once, and must be done before running any validations."
                .to_owned(),
        )
    })
}

/// Shared schema store helpers.
#[pyo3::pymodule]
pub(crate) mod _schema_store {
    use super::*;

    #[pyfunction]
    pub(crate) fn init_store_from_file(file: PathBuf) -> PyResult<()> {
        info!("Initialize the schema store from file.");

        let store = {
            let store = Store::from_file(Some(&file)).map_err(|err| {
                PyRuntimeError::new_err(format!(
                    "Error while loading the Schema Store from file: {err}",
                ))
            })?;
            store.as_resolved().map_err(|err| {
                PyRuntimeError::new_err(format!("Error while resolving the Schema Store: {err}"))
            })
        }?;

        STORE.set(store).map_err(|_store| {
            PyRuntimeError::new_err(
                "Unable to initialize the schema store. \
                     Initialization can only happen once, and must be done before running any validations."
                    .to_owned(),
            )
        }).inspect(|()| info!("Initialized the schema store from file."))
    }

    #[pyfunction]
    /// Return the primary key for a list schema at the given data path.
    ///
    /// This helper only supports the EOS config schema for now, since other AVD
    /// schemas can use dynamic keys which are not supported here.
    pub(crate) fn get_list_primary_key(
        schema_name: &str,
        data_path: Vec<String>,
    ) -> PyResult<Option<String>> {
        get_avdschema_list_primary_key(schema_name, get_store()?, &data_path).map_err(
            |err| match err {
                GetSchemaFromPathError::StoreError(SchemaStoreError::InvalidSchemaName(name))
                    if name == schema_name =>
                {
                    PyRuntimeError::new_err(format!(
                        "Schema name '{name}' is not supported by get_list_primary_key. Supported schema names are 'eos_config'."
                    ))
                }
                err => PyRuntimeError::new_err(format!("Error while resolving schema path: {err:?}")),
            },
        )
    }
}
