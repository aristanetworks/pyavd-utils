# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Password hashing and encryption helpers."""

from __future__ import annotations

# The native Rust module is not built in CI, so this suppression is required there.
from ._bindings import _passwords  # pyright: ignore[reportMissingModuleSource]

cbc_decrypt = _passwords.cbc_decrypt
cbc_encrypt = _passwords.cbc_encrypt
cbc_verify = _passwords.cbc_verify
sha512_crypt = _passwords.sha512_crypt
simple_7_decrypt = _passwords.simple_7_decrypt
simple_7_encrypt = _passwords.simple_7_encrypt

PasswordError = _passwords.PasswordError
Sha512CryptInvalidSaltEmptyError = _passwords.Sha512CryptInvalidSaltEmptyError
Sha512CryptInvalidSaltCharacterError = _passwords.Sha512CryptInvalidSaltCharacterError
Sha512CryptLibraryError = _passwords.Sha512CryptLibraryError
Sha512CryptBase64Error = _passwords.Sha512CryptBase64Error
CBCInvalidBase64Error = _passwords.CBCInvalidBase64Error
CBCDecryptionFailedError = _passwords.CBCDecryptionFailedError
CBCInvalidSignatureError = _passwords.CBCInvalidSignatureError
CBCInvalidUtf8Error = _passwords.CBCInvalidUtf8Error
CBCEncryptionFailedError = _passwords.CBCEncryptionFailedError
CBCInvalidBase64Utf8Error = _passwords.CBCInvalidBase64Utf8Error
Simple7InvalidSaltFormatError = _passwords.Simple7InvalidSaltFormatError
Simple7InvalidHexEncodingError = _passwords.Simple7InvalidHexEncodingError
Simple7RandomSourceUnavailableError = _passwords.Simple7RandomSourceUnavailableError
Simple7InvalidUtf8Error = _passwords.Simple7InvalidUtf8Error
Simple7InvalidSaltValueError = _passwords.Simple7InvalidSaltValueError
Simple7DataTooShortError = _passwords.Simple7DataTooShortError
Simple7EmptyPasswordError = _passwords.Simple7EmptyPasswordError

__all__ = [
    "CBCDecryptionFailedError",
    "CBCEncryptionFailedError",
    "CBCInvalidBase64Error",
    "CBCInvalidBase64Utf8Error",
    "CBCInvalidSignatureError",
    "CBCInvalidUtf8Error",
    "PasswordError",
    "Sha512CryptBase64Error",
    "Sha512CryptInvalidSaltCharacterError",
    "Sha512CryptInvalidSaltEmptyError",
    "Sha512CryptLibraryError",
    "Simple7DataTooShortError",
    "Simple7EmptyPasswordError",
    "Simple7InvalidHexEncodingError",
    "Simple7InvalidSaltFormatError",
    "Simple7InvalidSaltValueError",
    "Simple7InvalidUtf8Error",
    "Simple7RandomSourceUnavailableError",
    "cbc_decrypt",
    "cbc_encrypt",
    "cbc_verify",
    "sha512_crypt",
    "simple_7_decrypt",
    "simple_7_encrypt",
]
