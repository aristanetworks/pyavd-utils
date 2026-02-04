// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::*;
use pyo3::types::PyBytesMethods as _;

#[test]
fn generate_encryption_key_returns_32_bytes() {
    with_passwords_module(|_py, module| {
        let key: Vec<u8> = module
            .call_method0("generate_encryption_key")
            .unwrap()
            .extract()
            .unwrap();

        assert_eq!(key.len(), 32);
    });
}

#[test]
fn generate_encryption_key_produces_unique_keys() {
    with_passwords_module(|_py, module| {
        let key1: Vec<u8> = module
            .call_method0("generate_encryption_key")
            .unwrap()
            .extract()
            .unwrap();

        let key2: Vec<u8> = module
            .call_method0("generate_encryption_key")
            .unwrap()
            .extract()
            .unwrap();

        assert_ne!(key1, key2);
    });
}

#[test]
fn aes_encrypt_decrypt_roundtrip() {
    with_passwords_module(|py, module| {
        let key: Vec<u8> = module
            .call_method0("generate_encryption_key")
            .unwrap()
            .extract()
            .unwrap();

        let plaintext = b"Hello, World!";
        let plaintext_bytes = pyo3::types::PyBytes::new(py, plaintext);
        let key_bytes = pyo3::types::PyBytes::new(py, &key);

        let encrypted = module
            .call_method1("aes_encrypt", (plaintext_bytes, key_bytes.clone()))
            .unwrap();

        // Encrypted data should be different from plaintext
        let encrypted_bytes: &[u8] = encrypted.downcast::<pyo3::types::PyBytes>().unwrap().as_bytes();
        assert_ne!(encrypted_bytes, plaintext);

        let decrypted = module
            .call_method1("aes_decrypt", (encrypted, key_bytes))
            .unwrap();

        let decrypted_bytes: Vec<u8> = decrypted.extract().unwrap();
        assert_eq!(decrypted_bytes, plaintext);
    });
}

#[test]
fn aes_encrypt_invalid_key_length_err() {
    with_passwords_module(|py, module| {
        let plaintext = b"Hello, World!";
        let plaintext_bytes = pyo3::types::PyBytes::new(py, plaintext);
        let short_key = pyo3::types::PyBytes::new(py, &[0u8; 16]); // Only 16 bytes instead of 32

        let err = module
            .call_method1("aes_encrypt", (plaintext_bytes, short_key))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert!(err.value(py).to_string().contains("32 bytes"));
    });
}

#[test]
fn aes_decrypt_invalid_key_length_err() {
    with_passwords_module(|py, module| {
        let encrypted = pyo3::types::PyBytes::new(py, &[0u8; 50]); // Dummy encrypted data
        let short_key = pyo3::types::PyBytes::new(py, &[0u8; 16]); // Only 16 bytes

        let err = module
            .call_method1("aes_decrypt", (encrypted, short_key))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyValueError>(py));
        assert!(err.value(py).to_string().contains("32 bytes"));
    });
}

#[test]
fn aes_decrypt_with_wrong_key_fails() {
    with_passwords_module(|py, module| {
        let key1: Vec<u8> = module
            .call_method0("generate_encryption_key")
            .unwrap()
            .extract()
            .unwrap();

        let key2: Vec<u8> = module
            .call_method0("generate_encryption_key")
            .unwrap()
            .extract()
            .unwrap();

        let plaintext = b"Hello, World!";
        let plaintext_bytes = pyo3::types::PyBytes::new(py, plaintext);
        let key1_bytes = pyo3::types::PyBytes::new(py, &key1);
        let key2_bytes = pyo3::types::PyBytes::new(py, &key2);

        let encrypted = module
            .call_method1("aes_encrypt", (plaintext_bytes, key1_bytes))
            .unwrap();

        // Decrypting with wrong key should fail
        let err = module
            .call_method1("aes_decrypt", (encrypted, key2_bytes))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyRuntimeError>(py));
    });
}

#[test]
fn aes_decrypt_corrupted_data_fails() {
    with_passwords_module(|py, module| {
        let key: Vec<u8> = module
            .call_method0("generate_encryption_key")
            .unwrap()
            .extract()
            .unwrap();

        // Data too short to be valid (minimum is 29 bytes: 12 nonce + 16 tag + 1 ciphertext)
        let corrupted_data = pyo3::types::PyBytes::new(py, &[0u8; 20]);
        let key_bytes = pyo3::types::PyBytes::new(py, &key);

        let err = module
            .call_method1("aes_decrypt", (corrupted_data, key_bytes))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyRuntimeError>(py));
    });
}

#[test]
fn vault_encrypt_decrypt_roundtrip_with_vault_id() {
    with_passwords_module(|py, module| {
        let plaintext = b"Hello, Ansible Vault!";
        let plaintext_bytes = pyo3::types::PyBytes::new(py, plaintext);
        let password = "secret_password";
        let vault_id = "my_vault";

        let encrypted: String = module
            .call_method1("vault_encrypt", (plaintext_bytes, password, Some(vault_id)))
            .unwrap()
            .extract()
            .unwrap();

        // Verify v1.2 header format
        assert!(encrypted.starts_with("$ANSIBLE_VAULT;1.2;AES256;my_vault\n"));

        let result = module
            .call_method1("vault_decrypt", (&encrypted, password))
            .unwrap();

        let (decrypted, returned_vault_id): (Vec<u8>, Option<String>) = result.extract().unwrap();
        assert_eq!(decrypted, plaintext);
        assert_eq!(returned_vault_id, Some(vault_id.to_string()));
    });
}

#[test]
fn vault_encrypt_decrypt_roundtrip_without_vault_id() {
    with_passwords_module(|py, module| {
        let plaintext = b"Hello, Ansible Vault!";
        let plaintext_bytes = pyo3::types::PyBytes::new(py, plaintext);
        let password = "secret_password";
        let none: Option<&str> = None;

        let encrypted: String = module
            .call_method1("vault_encrypt", (plaintext_bytes, password, none))
            .unwrap()
            .extract()
            .unwrap();

        // Verify v1.1 header format
        assert!(encrypted.starts_with("$ANSIBLE_VAULT;1.1;AES256\n"));

        let result = module
            .call_method1("vault_decrypt", (&encrypted, password))
            .unwrap();

        let (decrypted, returned_vault_id): (Vec<u8>, Option<String>) = result.extract().unwrap();
        assert_eq!(decrypted, plaintext);
        assert_eq!(returned_vault_id, None);
    });
}

#[test]
fn vault_decrypt_wrong_password_fails() {
    with_passwords_module(|py, module| {
        let plaintext = b"Secret data";
        let plaintext_bytes = pyo3::types::PyBytes::new(py, plaintext);

        let encrypted: String = module
            .call_method1("vault_encrypt", (plaintext_bytes, "correct_password", Some("test")))
            .unwrap()
            .extract()
            .unwrap();

        let err = module
            .call_method1("vault_decrypt", (&encrypted, "wrong_password"))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyRuntimeError>(py));
        assert!(err.value(py).to_string().contains("HMAC"));
    });
}

#[test]
fn vault_invalid_format_fails() {
    with_passwords_module(|py, module| {
        let err = module
            .call_method1("vault_decrypt", ("invalid vault data", "password"))
            .unwrap_err();

        assert!(err.is_instance_of::<pyo3::exceptions::PyRuntimeError>(py));
    });
}
