// Copyright (c) 2025 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::*;

#[test]
fn simple_7_encrypt_decrypt_roundtrip() {
    with_passwords_module(|_py, module| {
        let password = "test_password";
        let salt = 5u8;

        let encrypted: String = module
            .call_method1("simple_7_encrypt", (password, Some(salt)))
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
    with_passwords_module(|py, module| {
        let password = "test_password";

        // Call with None for salt
        let encrypted: String = module
            .call_method1("simple_7_encrypt", (password, py.None()))
            .unwrap()
            .extract()
            .unwrap();

        // Should be able to decrypt it
        let decrypted: String = module
            .call_method1("simple_7_decrypt", (encrypted,))
            .unwrap()
            .extract()
            .unwrap();

        assert_eq!(decrypted, password);
    });
}

#[test]
fn simple_7_encrypt_invalid_salt_err() {
    with_passwords_module(|py, module| {
        let password = "test_password";
        let invalid_salt = 16u8; // Out of range (0-15)

        let err = module
            .call_method1("simple_7_encrypt", (password, Some(invalid_salt)))
            .unwrap_err();

        // Maps Simple7Error::InvalidSaltValue -> PyValueError
        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert_eq!(err.value(py).to_string(), "Salt must be in the range 0-15, got 16");
    });
}

#[test]
fn simple_7_decrypt_data_too_short_err() {
    with_passwords_module(|py, module| {
        let err = module
            .call_method1("simple_7_decrypt", ("0",))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert_eq!(
            err.value(py).to_string(),
            "Encrypted data too short (minimum 2 characters required for salt)"
        );
    });
}

#[test]
fn simple_7_decrypt_invalid_hex_err() {
    with_passwords_module(|py, module| {
        let err = module
            .call_method1("simple_7_decrypt", ("01GGGG",))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert!(err.value(py).to_string().contains("Invalid hex encoding"));
    });
}

#[test]
fn simple_7_decrypt_invalid_salt_format_err() {
    with_passwords_module(|py, module| {
        let err = module
            .call_method1("simple_7_decrypt", ("XX1234",))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert!(err.value(py).to_string().contains("Invalid salt format"));
    });
}

#[test]
fn simple_7_decrypt_salt_out_of_range_err() {
    with_passwords_module(|py, module| {
        let err = module
            .call_method1("simple_7_decrypt", ("161234",))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert_eq!(err.value(py).to_string(), "Salt must be in the range 0-15, got 16");
    });
}

#[test]
fn simple_7_known_values() {
    with_passwords_module(|_py, module| {
        // Test known encryption values
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
            assert_eq!(encrypted, expected_encrypted);

            let decrypted: String = module
                .call_method1("simple_7_decrypt", (expected_encrypted,))
                .unwrap()
                .extract()
                .unwrap();
            assert_eq!(decrypted, password);
        }
    });
}
