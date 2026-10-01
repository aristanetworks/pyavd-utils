// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

/// Password hashing and encryption helpers.
#[pyo3::pymodule]
pub(crate) mod _passwords {
    use pyo3::PyResult;
    #[cfg(any(feature = "cbc", feature = "sha512", feature = "simple-7"))]
    use pyo3::exceptions::PyRuntimeError;
    #[cfg(any(feature = "cbc", feature = "sha512", feature = "simple-7"))]
    use pyo3::exceptions::PyValueError;
    #[cfg(any(feature = "cbc", feature = "sha512", feature = "simple-7"))]
    use pyo3::pyfunction;

    #[cfg(feature = "sha512")]
    #[pyfunction]
    /// Computes the SHA512 crypt value for the password given the salt.
    pub(crate) fn sha512_crypt(password: &str, salt: &str) -> PyResult<String> {
        ::passwords::sha512_crypt(password, salt).map_err(|err| match err {
            ::passwords::Sha512CryptError::InvalidSalt(_)
            | ::passwords::Sha512CryptError::Base64InvalidLength(_) => {
                PyValueError::new_err(err.to_string())
            }
            ::passwords::Sha512CryptError::ShaCrypt(_) => PyRuntimeError::new_err(err.to_string()),
        })
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Encrypt the data with CBC `TripleDES`.
    pub(crate) fn cbc_encrypt(password: &str, data: &str) -> PyResult<String> {
        let result_bytes = ::passwords::cbc_encrypt(password.as_bytes(), data.as_bytes())
            .map_err(|err| PyRuntimeError::new_err(err.to_string()))?;
        String::from_utf8(result_bytes)
            .map_err(|_err| PyRuntimeError::new_err("Base64 output contained invalid UTF-8"))
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Decrypt the `encrypted_data` with CBC `TripleDES`.
    pub(crate) fn cbc_decrypt(password: &str, encrypted_data: &str) -> PyResult<String> {
        let decrypted_bytes =
            ::passwords::cbc_decrypt(password.as_bytes(), encrypted_data.as_bytes()).map_err(
                |err| match err {
                    ::passwords::CbcError::InvalidBase64 => PyValueError::new_err(err.to_string()),
                    _ => PyRuntimeError::new_err(err.to_string()),
                },
            )?;

        String::from_utf8(decrypted_bytes)
            .map_err(|_err| PyValueError::new_err(::passwords::CbcError::InvalidUtf8.to_string()))
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Verify if the encrypted data matches the given password.
    pub(crate) fn cbc_verify(password: &str, encrypted_data: &str) -> bool {
        ::passwords::cbc_check_password(password.as_bytes(), encrypted_data.as_bytes())
    }

    #[cfg(feature = "simple-7")]
    #[pyfunction]
    /// Encrypt (obfuscate) a password with insecure type-7.
    pub(crate) fn simple_7_encrypt(data: &str, salt: Option<u8>) -> PyResult<String> {
        ::passwords::simple_7_encrypt(data, salt).map_err(|err| match err {
            ::passwords::Simple7Error::InvalidSaltValue(_)
            | ::passwords::Simple7Error::EmptyPassword => PyValueError::new_err(err.to_string()),
            _ => PyRuntimeError::new_err(err.to_string()),
        })
    }

    #[cfg(feature = "simple-7")]
    #[pyfunction]
    /// Decrypt (deobfuscate) a password from insecure type-7.
    pub(crate) fn simple_7_decrypt(data: &str) -> PyResult<String> {
        ::passwords::simple_7_decrypt(data).map_err(|err| match err {
            ::passwords::Simple7Error::InvalidUtf8(_) => PyRuntimeError::new_err(err.to_string()),
            _ => PyValueError::new_err(err.to_string()),
        })
    }
}
