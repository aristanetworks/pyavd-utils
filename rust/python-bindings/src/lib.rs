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

    /// Shared schema store helpers.
    #[pyo3::pymodule]
    mod schema_store {
        #[pymodule_export]
        use crate::schema_store::init_store_from_file;
    }

    /// Schema validation helpers.
    #[pyo3::pymodule]
    mod validation {
        #[pymodule_export]
        use crate::validation::Configuration;
        #[pymodule_export]
        use crate::validation::Deprecation;
        #[pymodule_export]
        use crate::validation::IgnoredEosConfigKey;
        #[pymodule_export]
        use crate::validation::ValidatedDataResult;
        #[pymodule_export]
        use crate::validation::ValidationResult;
        #[pymodule_export]
        use crate::validation::Violation;
        #[pymodule_export]
        use crate::validation::get_validated_data;
        #[pymodule_export]
        use crate::validation::validate_json;
        #[pymodule_export]
        use crate::validation::validate_json_with_adhoc_schema;
    }

    /// Password hashing and encryption helpers.
    #[pyo3::pymodule]
    mod passwords {
        #[cfg(feature = "cbc")]
        #[pymodule_export]
        use crate::passwords::cbc_decrypt;
        #[cfg(feature = "cbc")]
        #[pymodule_export]
        use crate::passwords::cbc_encrypt;
        #[cfg(feature = "cbc")]
        #[pymodule_export]
        use crate::passwords::cbc_verify;
        #[cfg(feature = "sha512")]
        #[pymodule_export]
        use crate::passwords::sha512_crypt;
        #[cfg(feature = "simple-7")]
        #[pymodule_export]
        use crate::passwords::simple_7_decrypt;
        #[cfg(feature = "simple-7")]
        #[pymodule_export]
        use crate::passwords::simple_7_encrypt;
    }
}

#[cfg(test)]
mod tests;
