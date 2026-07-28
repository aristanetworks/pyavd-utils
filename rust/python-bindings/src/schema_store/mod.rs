// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::path::PathBuf;
use std::sync::OnceLock;

use avdschema::Load as _;
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
                PyRuntimeError::new_err(format!("Error while resolving the Schema Store: {err}",))
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
    /// Limitation: this only resolves static schema paths and dynamic root keys
    /// that can be inferred from schema defaults. User-defined dynamic root keys
    /// are not resolved because this helper does not accept input data or
    /// dynamic-key overrides.
    pub(crate) fn get_list_primary_key(
        schema_name: &str,
        data_path: Vec<String>,
    ) -> PyResult<Option<String>> {
        get_avdschema_list_primary_key(schema_name, get_store()?, &data_path).map_err(|err| {
            PyRuntimeError::new_err(format!("Error while resolving schema path: {err:?}"))
        })
    }
}
