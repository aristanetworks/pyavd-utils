// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::*;

#[test]
fn cbc_decrypt_invalid_base64_err() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py.import("passwords").unwrap();
        let args = ("passwd", "ThisIsNotBase64!!!");

        let err = module.call_method1("cbc_decrypt", args).unwrap_err();

        // Maps CbcError::InvalidBase64 -> PyValueError
        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert_eq!(err.value(py).to_string(), "Invalid Base64 encoding");
    });
}

#[test]
fn cbc_decrypt_failed_err() {
    with_passwords_module(|py, module| {
        // "YWJjZA==" is "abcd" (4 bytes).
        // Triple DES requires multiples of 8.
        // This will trigger the .map_err(|_| CbcError::DecryptionFailed) branch.
        let args = ("any_key", "YWJjZA==");

        let err = module.call_method1("cbc_decrypt", args).unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyRuntimeError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Decryption failed (check password)"
        );
    });
}

#[test]
fn cbc_decrypt_invalid_signature_err() {
    with_passwords_module(|py, module| {
        // Provide valid base64, but no Arista signature at the beginning
        let args = ("some_key", "YWFhYWFhYWFhYWFhYWFhYQ==");
        let err = module.call_method1("cbc_decrypt", args).unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyRuntimeError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Invalid Arista signature in decrypted data"
        );
    });
}

#[test]
fn cbc_verify_returns_bool() {
    with_passwords_module(|_py, module| {
        let key = "42.42.42.42";
        let data = "arista";

        let encrypted: String = module
            .call_method1("cbc_encrypt", (key, data))
            .unwrap()
            .extract()
            .unwrap();

        // Success case
        let is_valid: bool = module
            .call_method1("cbc_verify", (key, encrypted.clone()))
            .unwrap()
            .extract()
            .unwrap();
        assert!(is_valid);

        // Failure case
        let is_invalid: bool = module
            .call_method1("cbc_verify", ("wrong_key", encrypted))
            .unwrap()
            .extract()
            .unwrap();
        assert!(!is_invalid);
    });
}
