// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
//! Python bindings for password helpers.

#![deny(unused_crate_dependencies)]

use pyo3::pymodule;

mod errors;
mod exceptions;

#[pymodule]
#[pyo3(name = "passwords")]
mod passwords {

    use pyo3::PyResult;
    use pyo3::pyfunction;

    #[cfg(feature = "cbc")]
    use crate::errors::CbcDecryptPyError;
    #[cfg(feature = "cbc")]
    use crate::errors::CbcEncryptPyError;
    #[cfg(feature = "sha512")]
    use crate::errors::Sha512CryptPyError;
    #[cfg(feature = "simple-7")]
    use crate::errors::Simple7DecryptPyError;
    #[cfg(feature = "simple-7")]
    use crate::errors::Simple7EncryptPyError;
    #[rustfmt::skip]
    #[pymodule_export]
    pub(crate) use crate::exceptions::{
        CBCDecryptionFailedError,
        CBCEncryptionFailedError,
        CBCInvalidBase64Error,
        CBCInvalidBase64Utf8Error,
        CBCInvalidSignatureError,
        CBCInvalidUtf8Error,
        PasswordError,
        Sha512CryptBase64Error,
        Sha512CryptInvalidSaltCharacterError,
        Sha512CryptInvalidSaltEmptyError,
        Sha512CryptLibraryError,
        Simple7DataTooShortError,
        Simple7EmptyPasswordError,
        Simple7InvalidHexEncodingError,
        Simple7InvalidSaltFormatError,
        Simple7InvalidSaltValueError,
        Simple7InvalidUtf8Error,
        Simple7RandomSourceUnavailableError,
    };

    #[cfg(feature = "sha512")]
    #[pyfunction]
    /// Computes the SHA512 crypt value for the password given the salt
    pub(crate) fn sha512_crypt(password: &str, salt: &str) -> PyResult<String> {
        Ok(passwords::sha512_crypt(password, salt).map_err(Into::<Sha512CryptPyError>::into)?)
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Encrypt the data with CBC `TripleDES`
    pub(crate) fn cbc_encrypt(password: &str, data: &str) -> PyResult<String> {
        let result_bytes = passwords::cbc_encrypt(password.as_bytes(), data.as_bytes())
            .map_err(Into::<CbcEncryptPyError>::into)?;
        Ok(String::from_utf8(result_bytes).map_err(|_err| CbcEncryptPyError::InvalidBase64Utf8)?)
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Decrypt the `encrypted_data` with CBC `TripleDES`
    pub(crate) fn cbc_decrypt(password: &str, encrypted_data: &str) -> PyResult<String> {
        let decrypted_bytes =
            passwords::cbc_decrypt(password.as_bytes(), encrypted_data.as_bytes())
                .map_err(Into::<CbcDecryptPyError>::into)?;

        Ok(String::from_utf8(decrypted_bytes).map_err(|_err| CbcDecryptPyError::InvalidUtf8)?)
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Verify if the encrypted data matches the given password
    pub(crate) fn cbc_verify(password: &str, encrypted_data: &str) -> bool {
        passwords::cbc_check_password(password.as_bytes(), encrypted_data.as_bytes())
    }

    #[cfg(feature = "simple-7")]
    #[pyfunction]
    /// Encrypt (obfuscate) a password with insecure type-7.
    ///
    /// If salt is None, a random salt in the range 0-15 will be used.
    /// Raises a specific `PasswordError` subclass if the password is empty or the salt is out of range.
    pub(crate) fn simple_7_encrypt(data: &str, salt: Option<u8>) -> PyResult<String> {
        Ok(passwords::simple_7_encrypt(data, salt).map_err(Into::<Simple7EncryptPyError>::into)?)
    }

    #[cfg(feature = "simple-7")]
    #[pyfunction]
    /// Decrypt (deobfuscate) a password from insecure type-7.
    ///
    /// Raises a specific `PasswordError` subclass if decryption fails.
    pub(crate) fn simple_7_decrypt(data: &str) -> PyResult<String> {
        Ok(passwords::simple_7_decrypt(data).map_err(Into::<Simple7DecryptPyError>::into)?)
    }
}

// Implementation of the pytests but here using pyo3 wrappers in Rust, to ensure we get coverage data
// and that we can catch issues in Rust without building the Python first.
#[cfg(test)]
mod tests;
