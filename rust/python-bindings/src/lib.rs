// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
//! Python bindings for pyavd-utils.

#![allow(
    missing_docs,
    missing_debug_implementations,
    clippy::fn_params_excessive_bools,
    clippy::manual_let_else,
    clippy::module_name_repetitions,
    clippy::needless_pass_by_value,
    clippy::struct_excessive_bools,
    clippy::unnecessary_trailing_comma,
    clippy::unnecessary_wraps,
    reason = "PyO3-facing API names and test assertions mirror the exported Python module contract"
)]

mod passwords;
mod schema_store;
mod validation;

#[pyo3::pymodule]
pub mod _bindings {
    use log::debug;
    use pyo3::Bound;
    use pyo3::PyResult;
    use pyo3::types::PyModule;

    #[pymodule_init]
    fn init(_module: &Bound<'_, PyModule>) -> PyResult<()> {
        pyo3_log::init();
        debug!("initialized pyavd_utils._bindings");
        Ok(())
    }

    #[pymodule_export]
    use crate::passwords::_passwords;
    #[pymodule_export]
    use crate::schema_store::_schema_store;
    #[pymodule_export]
    use crate::validation::_validation;
}

#[cfg(test)]
mod tests;
