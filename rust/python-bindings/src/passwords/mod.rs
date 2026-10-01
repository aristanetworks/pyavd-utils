// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

pub(crate) mod errors;
pub(crate) mod exceptions;

/// Password hashing and encryption helpers.
#[pyo3::pymodule]
pub(crate) mod _passwords {
    #[cfg(feature = "cbc")]
    use super::errors::CbcDecryptPyError;
    #[cfg(feature = "cbc")]
    use super::errors::CbcEncryptPyError;
    #[cfg(feature = "sha512")]
    use super::errors::Sha512CryptPyError;
    #[cfg(feature = "simple-7")]
    use super::errors::Simple7PyError;
    #[rustfmt::skip]
    #[pymodule_export]
    pub(crate) use super::exceptions::{
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
    #[cfg(any(feature = "cbc", feature = "sha512", feature = "simple-7"))]
    use pyo3::pyfunction;

    #[cfg(feature = "sha512")]
    #[pyfunction]
    /// Computes the SHA512 crypt value for the password given the salt.
    pub(crate) fn sha512_crypt(password: &str, salt: &str) -> Result<String, Sha512CryptPyError> {
        Ok(::passwords::sha512_crypt(password, salt)?)
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Encrypt the data with CBC `TripleDES`.
    pub(crate) fn cbc_encrypt(password: &str, data: &str) -> Result<String, CbcEncryptPyError> {
        let result_bytes = ::passwords::cbc_encrypt(password.as_bytes(), data.as_bytes())?;
        Ok(String::from_utf8(result_bytes)?)
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Decrypt the `encrypted_data` with CBC `TripleDES`.
    pub(crate) fn cbc_decrypt(
        password: &str,
        encrypted_data: &str,
    ) -> Result<String, CbcDecryptPyError> {
        let decrypted_bytes =
            ::passwords::cbc_decrypt(password.as_bytes(), encrypted_data.as_bytes())?;

        Ok(String::from_utf8(decrypted_bytes)?)
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
    pub(crate) fn simple_7_encrypt(data: &str, salt: Option<u8>) -> Result<String, Simple7PyError> {
        Ok(::passwords::simple_7_encrypt(data, salt)?)
    }

    #[cfg(feature = "simple-7")]
    #[pyfunction]
    /// Decrypt (deobfuscate) a password from insecure type-7.
    pub(crate) fn simple_7_decrypt(data: &str) -> Result<String, Simple7PyError> {
        Ok(::passwords::simple_7_decrypt(data)?)
    }
}
