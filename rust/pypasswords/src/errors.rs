// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::PyErr;

use crate::exceptions;

#[cfg(feature = "sha512")]
#[derive(Debug, derive_more::From)]
pub(crate) struct Sha512CryptPyError(passwords::Sha512CryptError);

#[cfg(feature = "sha512")]
impl From<Sha512CryptPyError> for PyErr {
    fn from(err: Sha512CryptPyError) -> Self {
        let Sha512CryptPyError(err) = err;
        sha512_crypt_error_to_pyerr(&err)
    }
}

#[cfg(feature = "cbc")]
#[derive(Debug, derive_more::From)]
pub(crate) enum CbcEncryptPyError {
    Cbc(passwords::CbcError),
    InvalidBase64Utf8(std::string::FromUtf8Error),
}

#[cfg(feature = "cbc")]
impl From<CbcEncryptPyError> for PyErr {
    fn from(err: CbcEncryptPyError) -> Self {
        match err {
            CbcEncryptPyError::Cbc(err) => cbc_error_to_pyerr(&err),
            CbcEncryptPyError::InvalidBase64Utf8(err) => {
                exceptions::CBCInvalidBase64Utf8Error::new_err(format!(
                    "Base64 output contained invalid UTF-8: {err}"
                ))
            }
        }
    }
}

#[cfg(feature = "cbc")]
#[derive(Debug, derive_more::From)]
pub(crate) enum CbcDecryptPyError {
    Cbc(passwords::CbcError),
    InvalidUtf8(std::string::FromUtf8Error),
}

#[cfg(feature = "cbc")]
impl From<CbcDecryptPyError> for PyErr {
    fn from(err: CbcDecryptPyError) -> Self {
        match err {
            CbcDecryptPyError::Cbc(err) => cbc_error_to_pyerr(&err),
            CbcDecryptPyError::InvalidUtf8(err) => exceptions::CBCInvalidUtf8Error::new_err(
                format!("{}: {err}", passwords::CbcError::InvalidUtf8),
            ),
        }
    }
}

#[cfg(feature = "simple-7")]
#[derive(Debug, derive_more::From)]
pub(crate) struct Simple7EncryptPyError(passwords::Simple7Error);

#[cfg(feature = "simple-7")]
impl From<Simple7EncryptPyError> for PyErr {
    fn from(err: Simple7EncryptPyError) -> Self {
        let Simple7EncryptPyError(err) = err;
        simple_7_error_to_pyerr(&err)
    }
}

#[cfg(feature = "simple-7")]
#[derive(Debug, derive_more::From)]
pub(crate) struct Simple7DecryptPyError(passwords::Simple7Error);

#[cfg(feature = "simple-7")]
impl From<Simple7DecryptPyError> for PyErr {
    fn from(err: Simple7DecryptPyError) -> Self {
        let Simple7DecryptPyError(err) = err;
        simple_7_error_to_pyerr(&err)
    }
}

#[cfg(feature = "sha512")]
fn sha512_crypt_error_to_pyerr(err: &passwords::Sha512CryptError) -> PyErr {
    let message = err.to_string();
    match err {
        passwords::Sha512CryptError::InvalidSalt(passwords::InvalidSaltError::IsEmpty) => {
            exceptions::Sha512CryptInvalidSaltEmptyError::new_err(message)
        }
        passwords::Sha512CryptError::InvalidSalt(
            passwords::InvalidSaltError::InvalidCharacter(_),
        ) => exceptions::Sha512CryptInvalidSaltCharacterError::new_err(message),
        passwords::Sha512CryptError::ShaCrypt(_) => {
            exceptions::Sha512CryptLibraryError::new_err(message)
        }
        passwords::Sha512CryptError::Base64InvalidLength(_) => {
            exceptions::Sha512CryptBase64Error::new_err(message)
        }
    }
}

#[cfg(feature = "cbc")]
fn cbc_error_to_pyerr(err: &passwords::CbcError) -> PyErr {
    let message = err.to_string();
    match err {
        passwords::CbcError::InvalidBase64 => exceptions::CBCInvalidBase64Error::new_err(message),
        passwords::CbcError::DecryptionFailed => {
            exceptions::CBCDecryptionFailedError::new_err(message)
        }
        passwords::CbcError::InvalidSignature => {
            exceptions::CBCInvalidSignatureError::new_err(message)
        }
        passwords::CbcError::InvalidUtf8 => exceptions::CBCInvalidUtf8Error::new_err(message),
        passwords::CbcError::EncryptionFailed => {
            exceptions::CBCEncryptionFailedError::new_err(message)
        }
    }
}

#[cfg(feature = "simple-7")]
fn simple_7_error_to_pyerr(err: &passwords::Simple7Error) -> PyErr {
    let message = err.to_string();
    match err {
        passwords::Simple7Error::InvalidSaltFormat(_) => {
            exceptions::Simple7InvalidSaltFormatError::new_err(message)
        }
        passwords::Simple7Error::InvalidHexEncoding(_) => {
            exceptions::Simple7InvalidHexEncodingError::new_err(message)
        }
        passwords::Simple7Error::RandomSourceUnavailable(_) => {
            exceptions::Simple7RandomSourceUnavailableError::new_err(message)
        }
        passwords::Simple7Error::InvalidUtf8(_) => {
            exceptions::Simple7InvalidUtf8Error::new_err(message)
        }
        passwords::Simple7Error::InvalidSaltValue(_) => {
            exceptions::Simple7InvalidSaltValueError::new_err(message)
        }
        passwords::Simple7Error::DataTooShort => {
            exceptions::Simple7DataTooShortError::new_err(message)
        }
        passwords::Simple7Error::EmptyPassword => {
            exceptions::Simple7EmptyPasswordError::new_err(message)
        }
    }
}
