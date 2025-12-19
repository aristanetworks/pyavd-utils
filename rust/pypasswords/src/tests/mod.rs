// Copyright (c) 2025 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::passwords;

use pyo3::types::PyAnyMethods as _;

// Initializing python only once. Otherwise things may crash when running in multiple threads.
static INIT_PY: std::sync::Once = std::sync::Once::new();

fn setup() {
    INIT_PY.call_once(|| {
        pyo3::append_to_inittab!(passwords);
        pyo3::Python::initialize();
    })
}

fn with_passwords_module<F: FnOnce(pyo3::Python<'_>, pyo3::Bound<'_, pyo3::types::PyModule>)>(
    test_logic: F,
) {
    setup();

    pyo3::Python::attach(|py| {
        let module = py
            .import("passwords")
            .expect("Failed to import embedded passwords module");

        test_logic(py, module);
    });
}

#[cfg(feature = "sha512")]
mod test_sha512;

#[cfg(feature = "cbc")]
mod test_cbc;

#[cfg(feature = "simple-7")]
mod test_simple_7;
