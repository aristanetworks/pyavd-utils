// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::types::PyAnyMethods as _;

use crate::passwords::errors::CbcDecryptPyError;
use crate::passwords::errors::CbcEncryptPyError;
use crate::passwords::exceptions;
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

        assert!(err.is_instance_of::<exceptions::CBCInvalidBase64Error>(py));
        assert!(err.is_instance_of::<exceptions::PasswordError>(py));
        assert_eq!(err.value(py).to_string(), "Invalid Base64 encoding.");
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

        assert!(err.is_instance_of::<exceptions::CBCDecryptionFailedError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Decryption failed (check password)."
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

        assert!(err.is_instance_of::<exceptions::CBCInvalidSignatureError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Invalid Arista signature in decrypted data."
        );
    });
}

#[test]
fn cbc_invalid_base64_error_uses_public_module_path() {
    setup_python();
    pyo3::Python::attach(|py| {
        let error_type = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap()
            .getattr("CBCInvalidBase64Error")
            .unwrap();
        let module_name: String = error_type.getattr("__module__").unwrap().extract().unwrap();

        assert_eq!(module_name, "pyavd_utils.passwords");
    });
}

#[test]
fn cbc_wrapper_errors_map_to_specific_pyerrs() {
    setup_python();
    pyo3::Python::attach(|py| {
        let decrypt_error = pyo3::PyErr::from(CbcDecryptPyError::InvalidUtf8(
            String::from_utf8(vec![0xff]).unwrap_err(),
        ));
        assert!(decrypt_error.is_instance_of::<exceptions::CBCInvalidUtf8Error>(py));
        assert!(decrypt_error.is_instance_of::<exceptions::PasswordError>(py));
        assert_eq!(
            decrypt_error.value(py).to_string(),
            "Decrypted data is not valid UTF-8."
        );

        let encrypt_error = pyo3::PyErr::from(CbcEncryptPyError::InvalidBase64Utf8(
            String::from_utf8(vec![0xff]).unwrap_err(),
        ));
        assert!(encrypt_error.is_instance_of::<exceptions::CBCInvalidBase64Utf8Error>(py));
        assert!(encrypt_error.is_instance_of::<exceptions::PasswordError>(py));
        assert_eq!(
            encrypt_error.value(py).to_string(),
            "CBC Base64 output is not valid UTF-8."
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
