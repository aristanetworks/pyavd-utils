// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
//! Python bindings for password helpers.

#![deny(unused_crate_dependencies)]

use pyo3::create_exception;
use pyo3::exceptions::PyException;
use pyo3::pymodule;

create_exception!(
    passwords,
    PasswordError,
    PyException,
    "Base exception for pyavd_utils.passwords."
);
create_exception!(
    passwords,
    Sha512CryptInvalidSaltEmptyError,
    PasswordError,
    "SHA512 crypt salt is empty."
);
create_exception!(
    passwords,
    Sha512CryptInvalidSaltCharacterError,
    PasswordError,
    "SHA512 crypt salt contains an invalid character."
);
create_exception!(
    passwords,
    Sha512CryptLibraryError,
    PasswordError,
    "SHA512 crypt library error."
);
create_exception!(
    passwords,
    CBCInvalidBase64Error,
    PasswordError,
    "CBC encrypted data is not valid Base64."
);
create_exception!(
    passwords,
    CBCDecryptionFailedError,
    PasswordError,
    "CBC decryption failed."
);
create_exception!(
    passwords,
    CBCInvalidSignatureError,
    PasswordError,
    "CBC decrypted data has an invalid Arista signature."
);
create_exception!(
    passwords,
    CBCInvalidUtf8Error,
    PasswordError,
    "CBC decrypted data is not valid UTF-8."
);
create_exception!(
    passwords,
    CBCEncryptionFailedError,
    PasswordError,
    "CBC encryption failed."
);
create_exception!(
    passwords,
    CBCInvalidBase64Utf8Error,
    PasswordError,
    "CBC Base64 output is not valid UTF-8."
);
create_exception!(
    passwords,
    Simple7InvalidSaltFormatError,
    PasswordError,
    "Type-7 encrypted data has an invalid salt format."
);
create_exception!(
    passwords,
    Simple7InvalidHexEncodingError,
    PasswordError,
    "Type-7 encrypted data has invalid hex encoding."
);
create_exception!(
    passwords,
    Simple7RandomSourceUnavailableError,
    PasswordError,
    "Type-7 random salt source is unavailable."
);
create_exception!(
    passwords,
    Simple7InvalidUtf8Error,
    PasswordError,
    "Type-7 decrypted data is not valid UTF-8."
);
create_exception!(
    passwords,
    Simple7InvalidSaltValueError,
    PasswordError,
    "Type-7 salt value is outside the supported range."
);
create_exception!(
    passwords,
    Simple7DataTooShortError,
    PasswordError,
    "Type-7 encrypted data is too short."
);
create_exception!(
    passwords,
    Simple7EmptyPasswordError,
    PasswordError,
    "Type-7 password is empty."
);

#[pymodule]
#[pyo3(name = "passwords")]
mod passwords {

    use pyo3::PyResult;
    use pyo3::pyfunction;

    #[pymodule_export]
    pub(crate) use super::CBCDecryptionFailedError;
    #[pymodule_export]
    pub(crate) use super::CBCEncryptionFailedError;
    #[pymodule_export]
    pub(crate) use super::CBCInvalidBase64Error;
    #[pymodule_export]
    pub(crate) use super::CBCInvalidBase64Utf8Error;
    #[pymodule_export]
    pub(crate) use super::CBCInvalidSignatureError;
    #[pymodule_export]
    pub(crate) use super::CBCInvalidUtf8Error;
    #[pymodule_export]
    pub(crate) use super::PasswordError;
    #[pymodule_export]
    pub(crate) use super::Sha512CryptInvalidSaltCharacterError;
    #[pymodule_export]
    pub(crate) use super::Sha512CryptInvalidSaltEmptyError;
    #[pymodule_export]
    pub(crate) use super::Sha512CryptLibraryError;
    #[pymodule_export]
    pub(crate) use super::Simple7DataTooShortError;
    #[pymodule_export]
    pub(crate) use super::Simple7EmptyPasswordError;
    #[pymodule_export]
    pub(crate) use super::Simple7InvalidHexEncodingError;
    #[pymodule_export]
    pub(crate) use super::Simple7InvalidSaltFormatError;
    #[pymodule_export]
    pub(crate) use super::Simple7InvalidSaltValueError;
    #[pymodule_export]
    pub(crate) use super::Simple7InvalidUtf8Error;
    #[pymodule_export]
    pub(crate) use super::Simple7RandomSourceUnavailableError;

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
        Ok(String::from_utf8(result_bytes).expect("Base64 output should only contain ASCII"))
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Decrypt the `encrypted_data` with CBC `TripleDES`
    pub(crate) fn cbc_decrypt(password: &str, encrypted_data: &str) -> PyResult<String> {
        let decrypted_bytes =
            passwords::cbc_decrypt(password.as_bytes(), encrypted_data.as_bytes())
                .map_err(ToPythonError::to_python_error)?;

        String::from_utf8(decrypted_bytes)
            .map_err(|_| passwords::CbcError::InvalidUtf8.to_python_error())
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
    /// Raises `ValueError` if the password is empty or the salt is out of range.
    pub(crate) fn simple_7_encrypt(data: &str, salt: Option<u8>) -> PyResult<String> {
        passwords::simple_7_encrypt(data, salt).map_err(ToPythonError::to_python_error)
    }

    #[cfg(feature = "simple-7")]
    #[pyfunction]
    /// Decrypt (deobfuscate) a password from insecure type-7.
    ///
    /// Raises `ValueError` if the password is empty or decryption fails.
    pub(crate) fn simple_7_decrypt(data: &str) -> PyResult<String> {
        passwords::simple_7_decrypt(data).map_err(ToPythonError::to_python_error)
    }
}

// Implementation of the pytests but here using pyo3 wrappers in Rust, to ensure we get coverage data
// and that we can catch issues in Rust without building the Python first.
#[cfg(test)]
mod tests;
