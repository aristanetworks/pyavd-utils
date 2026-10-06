// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::path::PathBuf;
use std::sync::OnceLock;

use avdschema::Store;
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
    use pyo3::pyclass;

    use super::PathBuf;
    use super::PyResult;
    use super::PyRuntimeError;
    use super::STORE;
    use super::Store;
    use super::already_initialized_error;
    use super::get_store;
    use super::info;
    use super::pyfunction;

    /// Minimal metadata for a resolved schema node.
    #[pyclass(frozen, get_all)]
    pub(crate) struct SchemaInfo {
        pub schema_type: &'static str,
        pub primary_key: Option<String>,
    }

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
    /// Return minimal schema metadata at the given data path.
    ///
    /// General data-aware dynamic-key resolution is not supported. Lookup retains
    /// the existing empty-input behavior and its schema-default resolution rules.
    pub(crate) fn get_schema_info(
        schema_name: &str,
        data_path: Vec<String>,
    ) -> PyResult<Option<SchemaInfo>> {
        if !matches!(schema_name, "eos_config" | "avd_design") {
            return Err(PyRuntimeError::new_err(format!(
                "Schema name '{schema_name}' is not supported by get_schema_info. Supported schema names are 'eos_config' and 'avd_design'."
            )));
        }
        get_store()?
            .get_schema_info(schema_name, &data_path)
            .map(|info| {
                info.map(|info| SchemaInfo {
                    schema_type: info.schema_type,
                    primary_key: info.primary_key.map(ToOwned::to_owned),
                })
            })
            .map_err(|err| {
                PyRuntimeError::new_err(format!("Error while resolving schema path: {err}"))
            })
    }
}
