// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::types::PyAnyMethods as _;

use crate::tests::setup_python;

#[test]
fn sha512_crypt_valid_hash_with_salt_ok() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let result = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("password", "arista").unwrap();
            kwargs.set_item("salt", "1234567890ABCDEF").unwrap();
            module
                .call_method("sha512_crypt", args, Some(&kwargs))
                .unwrap()
        };
        let expected_hash = pyo3::types::PyString::new(
            py,
            "$6$1234567890ABCDEF$5h/.K2RuwSPqXTncNaqmw./4HduYZNE4RHDfivjrQ8nrYX3AcB8gKSsKFC1VSVOl3E46/QFZ85uHZWhxQGTeS0",
        );
        assert!(result.eq(&expected_hash).unwrap());
    });
}

#[test]
fn sha512_crypt_empty_salt_err() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let err = module
            .call_method1("sha512_crypt", ("arista", ""))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Invalid Salt: Salt cannot be empty."
        );
    });
}

#[test]
fn sha512_crypt_invalid_character_in_salt_err() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let err = module
            .call_method1("sha512_crypt", ("arista", "#"))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Invalid Salt: Salt contains an invalid character: '#'"
        );
    });
}
