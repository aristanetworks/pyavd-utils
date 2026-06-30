// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::*;
use crate::errors::Sha512CryptPyError;
#[test]
fn sha512_crypt_valid_hash_with_salt_ok() {
    with_passwords_module(|py, module| {
        let args = ();
        let kwargs = pyo3::types::PyDict::new(py);
        kwargs
            .set_item("password", pyo3::types::PyString::new(py, "arista"))
            .unwrap();
        kwargs
            .set_item("salt", pyo3::types::PyString::new(py, "1234567890ABCDEF"))
            .unwrap();
        let result = module
            .call_method("sha512_crypt", args, Some(&kwargs))
            .unwrap();

        let expected_hash = pyo3::types::PyString::new(
            py,
            "$6$1234567890ABCDEF$5h/.K2RuwSPqXTncNaqmw./4HduYZNE4RHDfivjrQ8nrYX3AcB8gKSsKFC1VSVOl3E46/QFZ85uHZWhxQGTeS0",
        );
        assert!(result.eq(expected_hash).unwrap());
    });
}

#[test]
fn sha512_crypt_empty_salt_err() {
    with_passwords_module(|py, module| {
        let args = ();
        let kwargs = pyo3::types::PyDict::new(py);
        kwargs
            .set_item("password", pyo3::types::PyString::new(py, "arista"))
            .unwrap();
        kwargs
            .set_item("salt", pyo3::types::PyString::new(py, ""))
            .unwrap();
        let err = module
            .call_method("sha512_crypt", args, Some(&kwargs))
            .unwrap_err();

        assert_eq!(
            err.value(py).to_string(),
            "Invalid Salt: Salt cannot be empty."
        );
        assert!(err.is_instance_of::<passwords::Sha512CryptInvalidSaltEmptyError>(py));
        assert!(err.is_instance_of::<passwords::PasswordError>(py));
    });
}

#[test]
fn sha512_crypt_invalid_character_in_salt_err() {
    with_passwords_module(|py, module| {
        let args = ();
        let kwargs = pyo3::types::PyDict::new(py);
        kwargs
            .set_item("password", pyo3::types::PyString::new(py, "arista"))
            .unwrap();
        kwargs
            .set_item("salt", pyo3::types::PyString::new(py, "🐍"))
            .unwrap();
        let err = module
            .call_method("sha512_crypt", args, Some(&kwargs))
            .unwrap_err();

        assert_eq!(
            err.value(py).to_string(),
            "Invalid Salt: Salt contains an invalid character: '🐍'."
        );
        assert!(err.is_instance_of::<passwords::Sha512CryptInvalidSaltCharacterError>(py));
    });
}

#[test]
fn sha512_crypt_library_error_maps_to_specific_pyerr() {
    with_passwords_module(|py, _module| {
        let err = pyo3::PyErr::from(Sha512CryptPyError::from(
            ::passwords::Sha512CryptError::ShaCrypt(sha_crypt::Error::RoundsInvalid),
        ));

        assert!(err.is_instance_of::<passwords::Sha512CryptLibraryError>(py));
        assert!(err.is_instance_of::<passwords::PasswordError>(py));
        assert!(
            err.value(py)
                .to_string()
                .contains("SHA crypt library error")
        );
    });
}

#[test]
fn sha512_crypt_base64_error_maps_to_specific_pyerr() {
    with_passwords_module(|py, _module| {
        let err = pyo3::PyErr::from(Sha512CryptPyError::from(
            ::passwords::Sha512CryptError::Base64InvalidLength(base64ct::InvalidLengthError),
        ));

        assert!(err.is_instance_of::<passwords::Sha512CryptBase64Error>(py));
        assert!(err.is_instance_of::<passwords::PasswordError>(py));
        assert!(
            err.value(py)
                .to_string()
                .contains("SHA crypt base64 invalid length error")
        );
    });
}
