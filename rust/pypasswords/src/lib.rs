// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
#![deny(unused_crate_dependencies)]

use pyo3::pymodule;

#[pymodule]
#[pyo3(name = "passwords")]
mod passwords {

    use pyo3::{
        PyResult,
        exceptions::{PyRuntimeError, PyValueError},
        pyfunction,
    };

    #[cfg(feature = "sha512")]
    #[pyfunction]
    /// Computes the SHA512 crypt value for the password given the salt
    pub fn sha512_crypt(password: String, salt: String) -> PyResult<String> {
        passwords::sha512_crypt(&password, &salt).map_err(|err| {
            // Mapping our crates error to Python errors.
            match err {
                passwords::Sha512CryptError::InvalidSalt(_) => {
                    PyValueError::new_err(err.to_string())
                }
                passwords::Sha512CryptError::ShaCrypt(_) => {
                    PyRuntimeError::new_err(err.to_string())
                }
            }
        })
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Encrypt the data with CBC TripleDES
    pub fn cbc_encrypt(password: String, data: String) -> PyResult<String> {
        let result_bytes = passwords::cbc_encrypt(password.as_bytes(), data.as_bytes())
            .map_err(|err| PyRuntimeError::new_err(err.to_string()))?;
        String::from_utf8(result_bytes)
            .map_err(|_| PyRuntimeError::new_err("Base64 output contained invalid UTF-8"))
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Decrypt the encrypted_data with CBC TripleDES
    pub fn cbc_decrypt(password: String, encrypted_data: String) -> PyResult<String> {
        let decrypted_bytes =
            passwords::cbc_decrypt(password.as_bytes(), encrypted_data.as_bytes()).map_err(
                |err| match err {
                    passwords::CbcError::InvalidBase64 => PyValueError::new_err(err.to_string()),
                    _ => PyRuntimeError::new_err(err.to_string()),
                },
            )?;

        String::from_utf8(decrypted_bytes)
            .map_err(|_| PyValueError::new_err(passwords::CbcError::InvalidUtf8.to_string()))
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Verify if the encrypted data matches the given password
    pub fn cbc_verify(password: String, encrypted_data: String) -> bool {
        passwords::cbc_check_password(password.as_bytes(), encrypted_data.as_bytes())
    }

    #[cfg(feature = "simple-7")]
    #[pyfunction]
    /// Encrypt (obfuscate) a password with insecure type-7.
    ///
    /// If salt is None, a random salt in the range 0-15 will be used.
    pub fn simple_7_encrypt(data: String, salt: Option<u8>) -> PyResult<String> {
        passwords::simple_7_encrypt(&data, salt).map_err(|err| match err {
            passwords::Simple7Error::InvalidSaltValue(_) => PyValueError::new_err(err.to_string()),
            _ => PyRuntimeError::new_err(err.to_string()),
        })
    }

    #[cfg(feature = "simple-7")]
    #[pyfunction]
    /// Decrypt (deobfuscate) a password from insecure type-7.
    pub fn simple_7_decrypt(data: String) -> PyResult<String> {
        passwords::simple_7_decrypt(&data).map_err(|err| match err {
            passwords::Simple7Error::InvalidUtf8(_) => PyRuntimeError::new_err(err.to_string()),
            _ => PyValueError::new_err(err.to_string()),
        })
    }

    #[cfg(feature = "encryption")]
    #[pyfunction]
    /// Generate a random 32-byte AES-256 encryption key.
    pub fn generate_encryption_key() -> Vec<u8> {
        encrypt::generate_key().to_vec()
    }

    #[cfg(feature = "encryption")]
    #[pyfunction]
    /// Encrypt data using AES-256-GCM.
    ///
    /// Args:
    ///     data: The plaintext data to encrypt.
    ///     key: A 32-byte AES-256 encryption key.
    ///
    /// Returns:
    ///     The encrypted data (nonce + ciphertext + auth tag).
    pub fn aes_encrypt(data: &[u8], key: &[u8]) -> PyResult<Vec<u8>> {
        let key: &[u8; 32] = key
            .try_into()
            .map_err(|_| PyValueError::new_err(format!("Key must be exactly 32 bytes, got {}", key.len())))?;
        encrypt::encrypt(data, key).map_err(|err| PyRuntimeError::new_err(err.to_string()))
    }

    #[cfg(feature = "encryption")]
    #[pyfunction]
    /// Decrypt data using AES-256-GCM.
    ///
    /// Args:
    ///     data: The encrypted data (nonce + ciphertext + auth tag).
    ///     key: A 32-byte AES-256 encryption key.
    ///
    /// Returns:
    ///     The decrypted plaintext data.
    pub fn aes_decrypt(data: &[u8], key: &[u8]) -> PyResult<Vec<u8>> {
        let key: &[u8; 32] = key
            .try_into()
            .map_err(|_| PyValueError::new_err(format!("Key must be exactly 32 bytes, got {}", key.len())))?;
        encrypt::decrypt(data, key).map_err(|err| PyRuntimeError::new_err(err.to_string()))
    }

    #[cfg(feature = "encryption")]
    #[pyfunction]
    /// Encrypt data using Ansible Vault v1.2 format.
    ///
    /// Args:
    ///     data: The plaintext data to encrypt.
    ///     password: The password to use for encryption.
    ///     vault_id: The vault ID label to include in the header.
    ///
    /// Returns:
    ///     The encrypted data as a string in Ansible Vault format.
    pub fn vault_encrypt(data: &[u8], password: &str, vault_id: &str) -> PyResult<String> {
        encrypt::vault_encrypt(data, password, vault_id)
            .map_err(|err| PyRuntimeError::new_err(err.to_string()))
    }

    #[cfg(feature = "encryption")]
    #[pyfunction]
    /// Decrypt data from Ansible Vault v1.2 format.
    ///
    /// Args:
    ///     vault_data: The encrypted vault data as a string.
    ///     password: The password to use for decryption.
    ///
    /// Returns:
    ///     A tuple of (decrypted_data, vault_id).
    pub fn vault_decrypt(vault_data: &str, password: &str) -> PyResult<(Vec<u8>, String)> {
        encrypt::vault_decrypt(vault_data, password)
            .map_err(|err| PyRuntimeError::new_err(err.to_string()))
    }
}

// Implementation of the pytests but here using pyo3 wrappers in Rust, to ensure we get coverage data
// and that we can catch issues in Rust without building the Python first.
#[cfg(test)]
mod tests;
