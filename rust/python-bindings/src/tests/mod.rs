// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::sync::OnceLock;

use pyo3::types::PyAnyMethods as _;
use pyo3::types::PyDict;

use crate::_bindings;

mod passwords;
mod validation;

static INIT_PY: OnceLock<()> = OnceLock::new();
static INIT_STORE: OnceLock<()> = OnceLock::new();

fn setup_python() {
    INIT_PY.get_or_init(|| {
        pyo3::append_to_inittab!(_bindings);
        pyo3::Python::initialize();
    });
}

fn setup() {
    setup_python();
    INIT_STORE.get_or_init(|| {
        pyo3::Python::attach(|py| {
            let module = py
                .import("_bindings")
                .unwrap()
                .getattr("_schema_store")
                .unwrap();
            let kwargs = PyDict::new(py);
            let file = py.detach(test_schema_store::get_store_gz_path);
            kwargs.set_item("file", file).unwrap();
            module
                .call_method("init_store_from_file", (), Some(&kwargs))
                .unwrap();
        });
    });
}

fn get_path_and_message_from_py_violation(
    violation: pyo3::Bound<'_, pyo3::PyAny>,
) -> (Vec<String>, String) {
    let path: Vec<String> = violation
        .getattr("path")
        .unwrap()
        .cast_into_exact::<pyo3::types::PyList>()
        .unwrap()
        .into_iter()
        .map(|item| {
            item.cast_into_exact::<pyo3::types::PyString>()
                .unwrap()
                .to_string()
        })
        .collect();
    let message = violation
        .getattr("message")
        .unwrap()
        .cast_into_exact::<pyo3::types::PyString>()
        .unwrap()
        .to_string();
    (path, message)
}
