// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::create_exception;
use pyo3::exceptions::PyException;

create_exception!(
    pyavd_utils.passwords,
    PasswordError,
    PyException,
    "Base exception for pyavd_utils.passwords."
);
create_exception!(
    pyavd_utils.passwords,
    Sha512CryptInvalidSaltEmptyError,
    PasswordError,
    "SHA512 crypt salt is empty."
);
create_exception!(
    pyavd_utils.passwords,
    Sha512CryptInvalidSaltCharacterError,
    PasswordError,
    "SHA512 crypt salt contains an invalid character."
);
create_exception!(
    pyavd_utils.passwords,
    Sha512CryptLibraryError,
    PasswordError,
    "SHA512 crypt library error."
);
create_exception!(
    pyavd_utils.passwords,
    Sha512CryptBase64Error,
    PasswordError,
    "SHA512 crypt Base64 encoding error."
);
create_exception!(
    pyavd_utils.passwords,
    CBCInvalidBase64Error,
    PasswordError,
    "CBC encrypted data is not valid Base64."
);
create_exception!(
    pyavd_utils.passwords,
    CBCDecryptionFailedError,
    PasswordError,
    "CBC decryption failed."
);
create_exception!(
    pyavd_utils.passwords,
    CBCInvalidSignatureError,
    PasswordError,
    "CBC decrypted data has an invalid Arista signature."
);
create_exception!(
    pyavd_utils.passwords,
    CBCInvalidUtf8Error,
    PasswordError,
    "CBC decrypted data is not valid UTF-8."
);
create_exception!(
    pyavd_utils.passwords,
    CBCEncryptionFailedError,
    PasswordError,
    "CBC encryption failed."
);
create_exception!(
    pyavd_utils.passwords,
    CBCInvalidBase64Utf8Error,
    PasswordError,
    "CBC Base64 output is not valid UTF-8."
);
create_exception!(
    pyavd_utils.passwords,
    Simple7InvalidSaltFormatError,
    PasswordError,
    "Type-7 encrypted data has an invalid salt format."
);
create_exception!(
    pyavd_utils.passwords,
    Simple7InvalidHexEncodingError,
    PasswordError,
    "Type-7 encrypted data has invalid hex encoding."
);
create_exception!(
    pyavd_utils.passwords,
    Simple7RandomSourceUnavailableError,
    PasswordError,
    "Type-7 random salt source is unavailable."
);
create_exception!(
    pyavd_utils.passwords,
    Simple7InvalidUtf8Error,
    PasswordError,
    "Type-7 decrypted data is not valid UTF-8."
);
create_exception!(
    pyavd_utils.passwords,
    Simple7InvalidSaltValueError,
    PasswordError,
    "Type-7 salt value is outside the supported range."
);
create_exception!(
    pyavd_utils.passwords,
    Simple7DataTooShortError,
    PasswordError,
    "Type-7 encrypted data is too short."
);
create_exception!(
    pyavd_utils.passwords,
    Simple7EmptyPasswordError,
    PasswordError,
    "Type-7 password is empty."
);
