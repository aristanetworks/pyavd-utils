// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
//! Python bindings for pyavd-utils generation tooling.

#![allow(
    missing_docs,
    missing_debug_implementations,
    clippy::needless_pass_by_value,
    clippy::unnecessary_wraps,
    reason = "PyO3-facing API names mirror the exported Python module contract"
)]

mod metaschema;
mod schema_store;

#[pyo3::pymodule]
pub mod _bindings {
    use log::debug;
    use pyo3::Bound;
    use pyo3::PyResult;
    use pyo3::types::PyModule;

    #[pymodule_init]
    fn init(_module: &Bound<'_, PyModule>) -> PyResult<()> {
        pyo3_log::init();
        debug!("initialized pyavd_utils_gen._bindings");
        Ok(())
    }

    #[pymodule_export]
    use crate::metaschema::_metaschema;
    #[pymodule_export]
    use crate::schema_store::_schema_store;
}
