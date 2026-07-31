// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::types::PyAnyMethods as _;

use crate::tests::setup_python;

#[test]
fn simple_7_encrypt_py_ok() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let encrypted = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("data", "arista").unwrap();
            kwargs.set_item("salt", 4).unwrap();
            module
                .call_method("simple_7_encrypt", args, Some(&kwargs))
                .unwrap()
        };
        assert_eq!(encrypted.to_string(), "045A190F1C354D");
    });
}

#[test]
fn simple_7_encrypt_decrypt_roundtrip() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let password = "test_password";
        let encrypted: String = module
            .call_method1("simple_7_encrypt", (password, Some(5_u8)))
            .unwrap()
            .extract()
            .unwrap();
        let decrypted: String = module
            .call_method1("simple_7_decrypt", (encrypted,))
            .unwrap()
            .extract()
            .unwrap();

        assert_eq!(decrypted, password);
    });
}

#[test]
fn simple_7_encrypt_with_random_salt() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let password = "test_password";
        let encrypted: String = module
            .call_method1("simple_7_encrypt", (password, py.None()))
            .unwrap()
            .extract()
            .unwrap();
        let decrypted: String = module
            .call_method1("simple_7_decrypt", (encrypted,))
            .unwrap()
            .extract()
            .unwrap();

        assert_eq!(decrypted, password);
    });
}

#[test]
fn simple_7_encrypt_empty_password_err() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let err = module
            .call_method1("simple_7_encrypt", ("", Some(5_u8)))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert_eq!(err.value(py).to_string(), "Password must not be empty");
    });
}

#[test]
fn simple_7_encrypt_invalid_salt_err() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let err = module
            .call_method1("simple_7_encrypt", ("test_password", Some(16_u8)))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Salt must be in the range 0-15, got 16"
        );
    });
}

#[test]
fn simple_7_decrypt_data_too_short_err() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let err = module.call_method1("simple_7_decrypt", ("0",)).unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Encrypted data too short (minimum 2 characters required for salt)"
        );
    });
}

#[test]
fn simple_7_decrypt_invalid_hex_err() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let err = module
            .call_method1("simple_7_decrypt", ("01GGGG",))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert!(err.value(py).to_string().contains("Invalid hex encoding"));
    });
}

#[test]
fn simple_7_decrypt_invalid_salt_format_err() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let err = module
            .call_method1("simple_7_decrypt", ("XX1234",))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert!(err.value(py).to_string().contains("Invalid salt format"));
    });
}

#[test]
fn simple_7_decrypt_salt_out_of_range_err() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let err = module
            .call_method1("simple_7_decrypt", ("161234",))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Salt must be in the range 0-15, got 16"
        );
    });
}

#[test]
fn simple_7_known_values() {
    setup_python();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_passwords")
            .unwrap();
        let test_cases: [(u8, &str, &str); 4] = [
            (1, "foo", "0115090B"),
            (6, "foo", "0600002E"),
            (9, "foo", "094A4106"),
            (15, "foo", "15140403"),
        ];

        for (salt, password, expected_encrypted) in test_cases {
            let encrypted: String = module
                .call_method1("simple_7_encrypt", (password, Some(salt)))
                .unwrap()
                .extract()
                .unwrap();
            let decrypted: String = module
                .call_method1("simple_7_decrypt", (expected_encrypted,))
                .unwrap()
                .extract()
                .unwrap();

            assert_eq!(encrypted, expected_encrypted);
            assert_eq!(decrypted, password);
        }
    });
}
