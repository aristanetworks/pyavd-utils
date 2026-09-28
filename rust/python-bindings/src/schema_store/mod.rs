// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::path::PathBuf;
use std::sync::OnceLock;

use avdschema::Load as _;
use avdschema::Store;
use avdschema::StoreSource;
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

fn already_initialized_error() -> pyo3::PyErr {
    PyRuntimeError::new_err(
        "Unable to initialize the schema store. \
         Initialization can only happen once, and must be done before running any validations."
            .to_owned(),
    )
}

/// Shared schema store helpers.
#[pyo3::pymodule]
pub(crate) mod _schema_store {
    use super::*;

    #[pyfunction]
    /// Validate and memory-map the process-wide compiled schema store.
    ///
    /// Initialization can happen only once per process and must happen before validation.
    pub(crate) fn init_store_from_file(file: PathBuf) -> PyResult<()> {
        info!("Initialize the schema store from file.");
        if STORE.get().is_some() {
            return Err(already_initialized_error());
        }

        let store = Store::from_file(&file).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while loading the Schema Store from file: {err}"
            ))
        })?;

        STORE
            .set(store)
            .map_err(|_store| already_initialized_error())
            .inspect(|()| info!("Initialized the schema store from file."))
    }

    #[pyfunction]
    /// Compile a source schema-store file into an archived runtime store.
    ///
    /// The destination is written atomically and may subsequently be memory-mapped with
    /// [`init_store_from_file`].
    pub(crate) fn compile_schema_archive(source: PathBuf, destination: PathBuf) -> PyResult<()> {
        let store = StoreSource::from_file(Some(&source)).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while loading the Schema Store from file: {err}"
            ))
        })?;
        Store::compile_to_file(&store, &destination).map_err(|err| {
            PyRuntimeError::new_err(format!("Error while compiling the Schema Store: {err}"))
        })
    }

    #[pyfunction]
    /// Return the primary key for a list schema at the given data path.
    ///
    /// Dynamic keys in the AVD design schema are not supported today; only
    /// static schema paths can be inspected.
    pub(crate) fn get_list_primary_key(
        schema_name: &str,
        data_path: Vec<String>,
    ) -> PyResult<Option<String>> {
        if !matches!(schema_name, "eos_config" | "avd_design") {
            return Err(PyRuntimeError::new_err(format!(
                "Schema name '{schema_name}' is not supported by get_list_primary_key. Supported schema names are 'eos_config' and 'avd_design'."
            )));
        }
        get_store()?
            .get_list_primary_key(schema_name, &data_path)
            .map(|primary_key| primary_key.map(ToOwned::to_owned))
            .map_err(|err| {
                PyRuntimeError::new_err(format!("Error while resolving schema path: {err}"))
            })
    }
}
