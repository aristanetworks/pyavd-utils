// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::path::PathBuf;

use avdschema::Load as _;
use avdschema::Store;
use avdschema::StoreSource;
use pyo3::PyResult;
use pyo3::exceptions::PyRuntimeError;
use pyo3::pyfunction;

/// Schema-store compilation helpers.
#[pyo3::pymodule]
pub(crate) mod _schema_store {
    use super::*;

    #[pyfunction]
    /// Compile a source schema-store file into the archived runtime store.
    ///
    /// The destination is written atomically and can subsequently be memory-mapped by
    /// `pyavd_utils.schema_store.init_store_from_file`.
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
}
