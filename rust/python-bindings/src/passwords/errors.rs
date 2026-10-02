// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::PyErr;

use super::exceptions;

#[cfg(feature = "sha512")]
#[derive(Debug, derive_more::From)]
pub(crate) struct Sha512CryptPyError(::passwords::Sha512CryptError);

#[cfg(feature = "sha512")]
impl From<Sha512CryptPyError> for PyErr {
    fn from(Sha512CryptPyError(error): Sha512CryptPyError) -> Self {
        let message = error.to_string();
        match error {
            ::passwords::Sha512CryptError::InvalidSalt(::passwords::InvalidSaltError::IsEmpty) => {
                exceptions::Sha512CryptInvalidSaltEmptyError::new_err(message)
            }
            ::passwords::Sha512CryptError::InvalidSalt(
                ::passwords::InvalidSaltError::InvalidCharacter(_),
            ) => exceptions::Sha512CryptInvalidSaltCharacterError::new_err(message),
            ::passwords::Sha512CryptError::ShaCrypt(_) => {
                exceptions::Sha512CryptLibraryError::new_err(message)
            }
            ::passwords::Sha512CryptError::Base64InvalidLength(_) => {
                exceptions::Sha512CryptBase64Error::new_err(message)
            }
        }
    }
}

#[cfg(feature = "cbc")]
#[derive(Debug, derive_more::From)]
pub(crate) enum CbcEncryptPyError {
    Cbc(::passwords::CbcError),
    InvalidBase64Utf8(std::string::FromUtf8Error),
}

#[cfg(feature = "cbc")]
impl From<CbcEncryptPyError> for PyErr {
    fn from(error: CbcEncryptPyError) -> Self {
        match error {
            CbcEncryptPyError::Cbc(error) => cbc_error_to_pyerr(error),
            CbcEncryptPyError::InvalidBase64Utf8(_error) => {
                exceptions::CBCInvalidBase64Utf8Error::new_err(
                    "CBC Base64 output is not valid UTF-8.",
                )
            }
        }
    }
}

#[cfg(feature = "cbc")]
#[derive(Debug, derive_more::From)]
pub(crate) enum CbcDecryptPyError {
    Cbc(::passwords::CbcError),
    InvalidUtf8(std::string::FromUtf8Error),
}

#[cfg(feature = "cbc")]
impl From<CbcDecryptPyError> for PyErr {
    fn from(error: CbcDecryptPyError) -> Self {
        match error {
            CbcDecryptPyError::Cbc(error) => cbc_error_to_pyerr(error),
            CbcDecryptPyError::InvalidUtf8(_error) => {
                exceptions::CBCInvalidUtf8Error::new_err("Decrypted data is not valid UTF-8.")
            }
        }
    }
}

#[cfg(feature = "cbc")]
fn cbc_error_to_pyerr(error: ::passwords::CbcError) -> PyErr {
    let message = error.to_string();
    match error {
        ::passwords::CbcError::InvalidBase64 => exceptions::CBCInvalidBase64Error::new_err(message),
        ::passwords::CbcError::DecryptionFailed => {
            exceptions::CBCDecryptionFailedError::new_err(message)
        }
        ::passwords::CbcError::InvalidSignature => {
            exceptions::CBCInvalidSignatureError::new_err(message)
        }
        ::passwords::CbcError::InvalidUtf8 => exceptions::CBCInvalidUtf8Error::new_err(message),
        ::passwords::CbcError::EncryptionFailed => {
            exceptions::CBCEncryptionFailedError::new_err(message)
        }
    }
}

#[cfg(feature = "simple-7")]
#[derive(Debug, derive_more::From)]
pub(crate) struct Simple7PyError(::passwords::Simple7Error);

#[cfg(feature = "simple-7")]
impl From<Simple7PyError> for PyErr {
    fn from(Simple7PyError(error): Simple7PyError) -> Self {
        let message = error.to_string();
        match error {
            ::passwords::Simple7Error::InvalidSaltFormat(_) => {
                exceptions::Simple7InvalidSaltFormatError::new_err(message)
            }
            ::passwords::Simple7Error::InvalidHexEncoding(_) => {
                exceptions::Simple7InvalidHexEncodingError::new_err(message)
            }
            ::passwords::Simple7Error::RandomSourceUnavailable(_) => {
                exceptions::Simple7RandomSourceUnavailableError::new_err(message)
            }
            ::passwords::Simple7Error::InvalidUtf8(_) => {
                exceptions::Simple7InvalidUtf8Error::new_err(message)
            }
            ::passwords::Simple7Error::InvalidSaltValue(_) => {
                exceptions::Simple7InvalidSaltValueError::new_err(message)
            }
            ::passwords::Simple7Error::DataTooShort => {
                exceptions::Simple7DataTooShortError::new_err(message)
            }
            ::passwords::Simple7Error::EmptyPassword => {
                exceptions::Simple7EmptyPasswordError::new_err(message)
            }
        }
    }
}
