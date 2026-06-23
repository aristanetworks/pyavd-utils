// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
//! Python bindings for password helpers.

#![deny(unused_crate_dependencies)]

use pyo3::pymodule;

mod exceptions;

#[pymodule]
#[pyo3(name = "passwords")]
mod passwords {

    use pyo3::PyResult;
    use pyo3::pyfunction;

    #[pymodule_export]
    pub(crate) use crate::exceptions::CBCDecryptionFailedError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::CBCEncryptionFailedError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::CBCInvalidBase64Error;
    #[pymodule_export]
    pub(crate) use crate::exceptions::CBCInvalidBase64Utf8Error;
    #[pymodule_export]
    pub(crate) use crate::exceptions::CBCInvalidSignatureError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::CBCInvalidUtf8Error;
    #[pymodule_export]
    pub(crate) use crate::exceptions::PasswordError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::Sha512CryptBase64Error;
    #[pymodule_export]
    pub(crate) use crate::exceptions::Sha512CryptInvalidSaltCharacterError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::Sha512CryptInvalidSaltEmptyError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::Sha512CryptLibraryError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::Simple7DataTooShortError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::Simple7EmptyPasswordError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::Simple7InvalidHexEncodingError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::Simple7InvalidSaltFormatError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::Simple7InvalidSaltValueError;
    #[pymodule_export]
    pub(crate) use crate::exceptions::Simple7InvalidUtf8Error;
    #[pymodule_export]
    pub(crate) use crate::exceptions::Simple7RandomSourceUnavailableError;

    pub(crate) trait ToPythonError {
        fn to_python_error(self) -> pyo3::PyErr;
    }

    #[cfg(feature = "sha512")]
    impl ToPythonError for passwords::Sha512CryptError {
        fn to_python_error(self) -> pyo3::PyErr {
            let message = self.to_string();
            match self {
                passwords::Sha512CryptError::InvalidSalt(passwords::InvalidSaltError::IsEmpty) => {
                    Sha512CryptInvalidSaltEmptyError::new_err(message)
                }
                passwords::Sha512CryptError::InvalidSalt(
                    passwords::InvalidSaltError::InvalidCharacter(_),
                ) => Sha512CryptInvalidSaltCharacterError::new_err(message),
                passwords::Sha512CryptError::ShaCrypt(_) => {
                    Sha512CryptLibraryError::new_err(message)
                }
                passwords::Sha512CryptError::Base64InvalidLength(_) => {
                    Sha512CryptBase64Error::new_err(message)
                }
            }
        }
    }

    #[cfg(feature = "cbc")]
    impl ToPythonError for passwords::CbcError {
        fn to_python_error(self) -> pyo3::PyErr {
            let message = self.to_string();
            match self {
                passwords::CbcError::InvalidBase64 => CBCInvalidBase64Error::new_err(message),
                passwords::CbcError::DecryptionFailed => CBCDecryptionFailedError::new_err(message),
                passwords::CbcError::InvalidSignature => CBCInvalidSignatureError::new_err(message),
                passwords::CbcError::InvalidUtf8 => CBCInvalidUtf8Error::new_err(message),
                passwords::CbcError::EncryptionFailed => CBCEncryptionFailedError::new_err(message),
            }
        }
    }

    #[cfg(feature = "simple-7")]
    impl ToPythonError for passwords::Simple7Error {
        fn to_python_error(self) -> pyo3::PyErr {
            let message = self.to_string();
            match self {
                passwords::Simple7Error::InvalidSaltFormat(_) => {
                    Simple7InvalidSaltFormatError::new_err(message)
                }
                passwords::Simple7Error::InvalidHexEncoding(_) => {
                    Simple7InvalidHexEncodingError::new_err(message)
                }
                passwords::Simple7Error::RandomSourceUnavailable(_) => {
                    Simple7RandomSourceUnavailableError::new_err(message)
                }
                passwords::Simple7Error::InvalidUtf8(_) => {
                    Simple7InvalidUtf8Error::new_err(message)
                }
                passwords::Simple7Error::InvalidSaltValue(_) => {
                    Simple7InvalidSaltValueError::new_err(message)
                }
                passwords::Simple7Error::DataTooShort => Simple7DataTooShortError::new_err(message),
                passwords::Simple7Error::EmptyPassword => {
                    Simple7EmptyPasswordError::new_err(message)
                }
            }
        }
    }

    #[cfg(feature = "sha512")]
    #[pyfunction]
    /// Computes the SHA512 crypt value for the password given the salt
    pub(crate) fn sha512_crypt(password: &str, salt: &str) -> PyResult<String> {
        passwords::sha512_crypt(password, salt).map_err(ToPythonError::to_python_error)
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Encrypt the data with CBC `TripleDES`
    pub(crate) fn cbc_encrypt(password: &str, data: &str) -> PyResult<String> {
        let result_bytes = passwords::cbc_encrypt(password.as_bytes(), data.as_bytes())
            .map_err(ToPythonError::to_python_error)?;
        String::from_utf8(result_bytes).map_err(|_err| {
            CBCInvalidBase64Utf8Error::new_err("Base64 output contained invalid UTF-8")
        })
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Decrypt the `encrypted_data` with CBC `TripleDES`
    pub(crate) fn cbc_decrypt(password: &str, encrypted_data: &str) -> PyResult<String> {
        let decrypted_bytes =
            passwords::cbc_decrypt(password.as_bytes(), encrypted_data.as_bytes())
                .map_err(ToPythonError::to_python_error)?;

        String::from_utf8(decrypted_bytes)
            .map_err(|_err| passwords::CbcError::InvalidUtf8.to_python_error())
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
        passwords::simple_7_encrypt(data, salt).map_err(ToPythonError::to_python_error)
    }

    #[cfg(feature = "simple-7")]
    #[pyfunction]
    /// Decrypt (deobfuscate) a password from insecure type-7.
    ///
    /// Raises a specific `PasswordError` subclass if decryption fails.
    pub(crate) fn simple_7_decrypt(data: &str) -> PyResult<String> {
        passwords::simple_7_decrypt(data).map_err(ToPythonError::to_python_error)
    }
}

// Implementation of the pytests but here using pyo3 wrappers in Rust, to ensure we get coverage data
// and that we can catch issues in Rust without building the Python first.
#[cfg(test)]
mod tests;
