// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

pub(crate) mod errors;

use std::path::PathBuf;
use std::sync::OnceLock;

use avdschema::Store;
use log::info;
use pyo3::pyfunction;

use self::errors::SchemaStorePyError;

pub(crate) static STORE: OnceLock<Store> = OnceLock::new();

pub(crate) fn get_store() -> Result<&'static Store, SchemaStorePyError> {
    STORE.get().ok_or(SchemaStorePyError::NotInitialized)
}

/// Shared schema store helpers.
#[pyo3::pymodule]
pub(crate) mod _schema_store {
    use super::PathBuf;
    use super::STORE;
    use super::Store;
    use super::errors::SchemaStorePyError;
    use super::get_store;
    use super::info;
    use super::pyfunction;
    #[rustfmt::skip]
    #[pymodule_export]
    pub(crate) use crate::validation::exceptions::{
        ValidationError,
        ValidationInvalidSchemaNameError,
        ValidationSchemaPathError,
        ValidationStoreAlreadyInitializedError,
        ValidationStoreLoadError,
        ValidationStoreLoadIoError,
        ValidationStoreNotInitializedError,
    };

    #[pyfunction]
    /// Validate and memory-map the process-wide compiled schema store.
    ///
    /// Initialization can happen only once per process and must happen before validation.
    pub(crate) fn init_store_from_file(file: PathBuf) -> Result<(), SchemaStorePyError> {
        info!("Initialize the schema store from file.");
        if STORE.get().is_some() {
            return Err(SchemaStorePyError::AlreadyInitialized);
        }

        let store = Store::from_file(&file)?;

        STORE
            .set(store)
            .map_err(|_store| SchemaStorePyError::AlreadyInitialized)
            .inspect(|()| info!("Initialized the schema store from file."))
    }

    #[pyfunction]
    /// Return the primary key for a list schema at the given data path.
    ///
    /// Dynamic keys in the AVD design schema are not supported today; only
    /// static schema paths can be inspected.
    pub(crate) fn get_list_primary_key(
        schema_name: &str,
        data_path: Vec<String>,
    ) -> Result<Option<String>, SchemaStorePyError> {
        if !matches!(schema_name, "eos_config" | "avd_design") {
            return Err(SchemaStorePyError::InvalidSchemaName(
                schema_name.to_owned(),
            ));
        }
        get_store()?
            .get_list_primary_key(schema_name, &data_path)
            .map(|primary_key| primary_key.map(ToOwned::to_owned))
            .map_err(Into::into)
    }
}
