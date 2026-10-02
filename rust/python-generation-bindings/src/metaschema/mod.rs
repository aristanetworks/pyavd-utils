// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::path::PathBuf;

use avdschema::generate_metaschema_json;
use pyo3::PyResult;
use pyo3::exceptions::PyRuntimeError;
use pyo3::pyfunction;

/// AVD source-schema metaschema generation helpers.
#[pyo3::pymodule]
pub(crate) mod _metaschema {
    use super::PathBuf;
    use super::PyResult;
    use super::PyRuntimeError;
    use super::generate_metaschema_json;
    use super::pyfunction;

    #[pyfunction]
    /// Write the AVD source-schema metaschema as formatted JSON.
    pub(crate) fn generate_metaschema(destination: PathBuf) -> PyResult<()> {
        let json = generate_metaschema_json().map_err(|error| {
            PyRuntimeError::new_err(format!(
                "Error while generating the AVD metaschema: {error}"
            ))
        })?;
        std::fs::write(&destination, json).map_err(|error| {
            PyRuntimeError::new_err(format!(
                "Error while writing the AVD metaschema to '{}': {error}",
                destination.display()
            ))
        })
    }
}
