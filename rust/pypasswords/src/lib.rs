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
}

// Implementation of the pytests but here using pyo3 wrappers in Rust, to ensure we get coverage data
// and that we can catch issues in Rust without building the Python first.
#[cfg(test)]
mod tests;
