// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::types::PyAnyMethods as _;

use crate::tests::setup_python;

#[test]
fn cbc_decrypt_invalid_base64_err() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let err = module
            .call_method1("cbc_decrypt", ("passwd", "ThisIsNotBase64!!!"))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert_eq!(err.value(py).to_string(), "Invalid Base64 encoding");
    });
}

#[test]
fn cbc_decrypt_failed_err() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let err = module
            .call_method1("cbc_decrypt", ("any_key", "YWJjZA=="))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyRuntimeError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Decryption failed (check password)"
        );
    });
}

#[test]
fn cbc_decrypt_invalid_signature_err() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let err = module
            .call_method1("cbc_decrypt", ("some_key", "YWFhYWFhYWFhYWFhYWFhYQ=="))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyRuntimeError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Invalid Arista signature in decrypted data"
        );
    });
}

#[test]
fn cbc_verify_returns_bool() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let key = "42.42.42.42";
        let data = "arista";

        let encrypted: String = module
            .call_method1("cbc_encrypt", (key, data))
            .unwrap()
            .extract()
            .unwrap();
        let is_valid: bool = module
            .call_method1("cbc_verify", (key, encrypted.clone()))
            .unwrap()
            .extract()
            .unwrap();
        let is_invalid: bool = module
            .call_method1("cbc_verify", ("wrong_key", encrypted))
            .unwrap()
            .extract()
            .unwrap();

        assert!(is_valid);
        assert!(!is_invalid);
    });
}
