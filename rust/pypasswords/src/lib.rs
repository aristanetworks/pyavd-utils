// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
#![deny(unused_crate_dependencies)]

use pyo3::pymodule;

#[pymodule]
#[pyo3(name = "passwords")]
mod passwords {

    use pyo3::{
        Bound, PyResult, create_exception,
        exceptions::PyException,
        pyfunction,
        types::{PyModule, PyModuleMethods},
    };

    pub(crate) trait ToPythonError {
        fn to_python_error(self) -> pyo3::PyErr;
    }

    macro_rules! define_python_exceptions {
        ($module:ident, [$(($name:ident, $base:ty, $doc:literal)),+ $(,)?]) => {
            $(
                create_exception!($module, $name, $base, $doc);
            )+

            fn register_python_exceptions(m: &Bound<'_, PyModule>) -> PyResult<()> {
                $(
                    m.add(stringify!($name), m.py().get_type::<$name>())?;
                )+
                Ok(())
            }
        };
    }

    define_python_exceptions!(
        passwords,
        [
            (
                PyAVDUtilsPasswordError,
                PyException,
                "Base exception for pyavd_utils.passwords."
            ),
            (
                PyAVDUtilsSha512CryptError,
                PyAVDUtilsPasswordError,
                "Base exception for SHA512 crypt errors."
            ),
            (
                PyAVDUtilsSha512CryptInvalidSaltError,
                PyAVDUtilsSha512CryptError,
                "Invalid SHA512 crypt salt."
            ),
            (
                PyAVDUtilsSha512CryptInvalidSaltEmptyError,
                PyAVDUtilsSha512CryptInvalidSaltError,
                "SHA512 crypt salt is empty."
            ),
            (
                PyAVDUtilsSha512CryptInvalidSaltCharacterError,
                PyAVDUtilsSha512CryptInvalidSaltError,
                "SHA512 crypt salt contains an invalid character."
            ),
            (
                PyAVDUtilsSha512CryptLibraryError,
                PyAVDUtilsSha512CryptError,
                "SHA512 crypt library error."
            ),
            (
                PyAVDUtilsCBCError,
                PyAVDUtilsPasswordError,
                "Base exception for CBC password errors."
            ),
            (
                PyAVDUtilsCBCInvalidBase64Error,
                PyAVDUtilsCBCError,
                "CBC encrypted data is not valid Base64."
            ),
            (
                PyAVDUtilsCBCDecryptionFailedError,
                PyAVDUtilsCBCError,
                "CBC decryption failed."
            ),
            (
                PyAVDUtilsCBCInvalidSignatureError,
                PyAVDUtilsCBCError,
                "CBC decrypted data has an invalid Arista signature."
            ),
            (
                PyAVDUtilsCBCInvalidUtf8Error,
                PyAVDUtilsCBCError,
                "CBC decrypted data is not valid UTF-8."
            ),
            (
                PyAVDUtilsCBCEncryptionFailedError,
                PyAVDUtilsCBCError,
                "CBC encryption failed."
            ),
            (
                PyAVDUtilsCBCInvalidBase64Utf8Error,
                PyAVDUtilsCBCError,
                "CBC Base64 output is not valid UTF-8."
            ),
            (
                PyAVDUtilsSimple7Error,
                PyAVDUtilsPasswordError,
                "Base exception for Type-7 password errors."
            ),
            (
                PyAVDUtilsSimple7InvalidSaltFormatError,
                PyAVDUtilsSimple7Error,
                "Type-7 encrypted data has an invalid salt format."
            ),
            (
                PyAVDUtilsSimple7InvalidHexEncodingError,
                PyAVDUtilsSimple7Error,
                "Type-7 encrypted data has invalid hex encoding."
            ),
            (
                PyAVDUtilsSimple7RandomSourceUnavailableError,
                PyAVDUtilsSimple7Error,
                "Type-7 random salt source is unavailable."
            ),
            (
                PyAVDUtilsSimple7InvalidUtf8Error,
                PyAVDUtilsSimple7Error,
                "Type-7 decrypted data is not valid UTF-8."
            ),
            (
                PyAVDUtilsSimple7InvalidSaltValueError,
                PyAVDUtilsSimple7Error,
                "Type-7 salt value is outside the supported range."
            ),
            (
                PyAVDUtilsSimple7DataTooShortError,
                PyAVDUtilsSimple7Error,
                "Type-7 encrypted data is too short."
            ),
        ]
    );

    #[cfg(feature = "sha512")]
    impl ToPythonError for passwords::Sha512CryptError {
        fn to_python_error(self) -> pyo3::PyErr {
            let message = self.to_string();
            match self {
                passwords::Sha512CryptError::InvalidSalt(passwords::InvalidSaltError::IsEmpty) => {
                    PyAVDUtilsSha512CryptInvalidSaltEmptyError::new_err(message)
                }
                passwords::Sha512CryptError::InvalidSalt(
                    passwords::InvalidSaltError::InvalidCharacter(_),
                ) => PyAVDUtilsSha512CryptInvalidSaltCharacterError::new_err(message),
                passwords::Sha512CryptError::ShaCrypt(_) => {
                    PyAVDUtilsSha512CryptLibraryError::new_err(message)
                }
            }
        }
    }

    #[cfg(feature = "cbc")]
    impl ToPythonError for passwords::CbcError {
        fn to_python_error(self) -> pyo3::PyErr {
            let message = self.to_string();
            match self {
                passwords::CbcError::InvalidBase64 => {
                    PyAVDUtilsCBCInvalidBase64Error::new_err(message)
                }
                passwords::CbcError::DecryptionFailed => {
                    PyAVDUtilsCBCDecryptionFailedError::new_err(message)
                }
                passwords::CbcError::InvalidSignature => {
                    PyAVDUtilsCBCInvalidSignatureError::new_err(message)
                }
                passwords::CbcError::InvalidUtf8 => PyAVDUtilsCBCInvalidUtf8Error::new_err(message),
                passwords::CbcError::EncryptionFailed => {
                    PyAVDUtilsCBCEncryptionFailedError::new_err(message)
                }
            }
        }
    }

    #[cfg(feature = "simple-7")]
    impl ToPythonError for passwords::Simple7Error {
        fn to_python_error(self) -> pyo3::PyErr {
            let message = self.to_string();
            match self {
                passwords::Simple7Error::InvalidSaltFormat(_) => {
                    PyAVDUtilsSimple7InvalidSaltFormatError::new_err(message)
                }
                passwords::Simple7Error::InvalidHexEncoding(_) => {
                    PyAVDUtilsSimple7InvalidHexEncodingError::new_err(message)
                }
                passwords::Simple7Error::RandomSourceUnavailable(_) => {
                    PyAVDUtilsSimple7RandomSourceUnavailableError::new_err(message)
                }
                passwords::Simple7Error::InvalidUtf8(_) => {
                    PyAVDUtilsSimple7InvalidUtf8Error::new_err(message)
                }
                passwords::Simple7Error::InvalidSaltValue(_) => {
                    PyAVDUtilsSimple7InvalidSaltValueError::new_err(message)
                }
                passwords::Simple7Error::DataTooShort => {
                    PyAVDUtilsSimple7DataTooShortError::new_err(message)
                }
            }
        }
    }

    #[pymodule_init]
    fn init(m: &Bound<'_, PyModule>) -> PyResult<()> {
        register_python_exceptions(m)
    }

    #[cfg(feature = "sha512")]
    #[pyfunction]
    /// Computes the SHA512 crypt value for the password given the salt
    pub fn sha512_crypt(password: String, salt: String) -> PyResult<String> {
        passwords::sha512_crypt(&password, &salt).map_err(ToPythonError::to_python_error)
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Encrypt the data with CBC TripleDES
    pub fn cbc_encrypt(password: String, data: String) -> PyResult<String> {
        let result_bytes = passwords::cbc_encrypt(password.as_bytes(), data.as_bytes())
            .map_err(ToPythonError::to_python_error)?;
        Ok(String::from_utf8(result_bytes).expect("Base64 output should only contain ASCII"))
    }

    #[cfg(feature = "cbc")]
    #[pyfunction]
    /// Decrypt the encrypted_data with CBC TripleDES
    pub fn cbc_decrypt(password: String, encrypted_data: String) -> PyResult<String> {
        let decrypted_bytes =
            passwords::cbc_decrypt(password.as_bytes(), encrypted_data.as_bytes())
                .map_err(ToPythonError::to_python_error)?;

        String::from_utf8(decrypted_bytes)
            .map_err(|_| passwords::CbcError::InvalidUtf8.to_python_error())
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
    /// Raises ValueError if the password is empty or the salt is out of range.
    pub fn simple_7_encrypt(data: String, salt: Option<u8>) -> PyResult<String> {
        passwords::simple_7_encrypt(&data, salt).map_err(ToPythonError::to_python_error)
    }

    #[cfg(feature = "simple-7")]
    #[pyfunction]
    /// Decrypt (deobfuscate) a password from insecure type-7.
    ///
    /// Raises ValueError if the password is empty or decryption fails.
    pub fn simple_7_decrypt(data: String) -> PyResult<String> {
        passwords::simple_7_decrypt(&data).map_err(ToPythonError::to_python_error)
    }
}

// Implementation of the pytests but here using pyo3 wrappers in Rust, to ensure we get coverage data
// and that we can catch issues in Rust without building the Python first.
#[cfg(test)]
mod tests;
