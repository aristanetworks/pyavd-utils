// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
#![deny(unused_crate_dependencies)]

use pyo3::pymodule;

#[pymodule]
#[pyo3(name = "passwords")]
mod passwords {

    use pyo3::{
        Bound, PyAny, PyResult,
        exceptions::{PyRuntimeError, PyValueError},
        prelude::PyAnyMethods,
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
    /// Raises ValueError if password is not a non-empty string or salt is not an integer in 0-15.
    pub fn simple_7_encrypt(data: Bound<'_, PyAny>, salt: Bound<'_, PyAny>) -> PyResult<String> {
        let data_str: String = data
            .extract()
            .map_err(|_| PyValueError::new_err("Password MUST be a string with at least 1 character."))?;
        if data_str.is_empty() {
            return Err(PyValueError::new_err("Password MUST be a string with at least 1 character."));
        }

        let salt_val: i64 = salt
            .extract()
            .map_err(|_| PyValueError::new_err("Salt MUST be an integer within the range 0-15."))?;
        if !(0..=15).contains(&salt_val) {
            return Err(PyValueError::new_err("Salt MUST be an integer within the range 0-15."));
        }

        passwords::simple_7_encrypt(&data_str, Some(salt_val as u8)).map_err(|err| match err {
            passwords::Simple7Error::InvalidSaltValue(_) => PyValueError::new_err(err.to_string()),
            _ => PyRuntimeError::new_err(err.to_string()),
        })
    }

    #[cfg(feature = "simple-7")]
    #[pyfunction]
    /// Decrypt (deobfuscate) a password from insecure type-7.
    ///
    /// Raises ValueError if password is not a non-empty string or decryption fails.
    pub fn simple_7_decrypt(data: Bound<'_, PyAny>) -> PyResult<String> {
        let data_str: String = data
            .extract()
            .map_err(|_| PyValueError::new_err("Password MUST be a string with at least 1 character."))?;
        if data_str.is_empty() {
            return Err(PyValueError::new_err("Password MUST be a string with at least 1 character."));
        }

        passwords::simple_7_decrypt(&data_str).map_err(|err| match err {
            passwords::Simple7Error::InvalidUtf8(_) => PyRuntimeError::new_err(err.to_string()),
            _ => PyValueError::new_err(err.to_string()),
        })
    }
}

// Implementation of the pytests but here using pyo3 wrappers in Rust, to ensure we get coverage data
// and that we can catch issues in Rust without building the Python first.
#[cfg(test)]
mod tests;
