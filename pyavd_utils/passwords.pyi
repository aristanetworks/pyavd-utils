# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# For now we allow docstrings in stubs
# ruff: noqa: PYI021

class PyAVDUtilsPasswordError(Exception): ...
class PyAVDUtilsSha512CryptError(PyAVDUtilsPasswordError): ...
class PyAVDUtilsSha512CryptInvalidSaltError(PyAVDUtilsSha512CryptError): ...
class PyAVDUtilsSha512CryptInvalidSaltEmptyError(PyAVDUtilsSha512CryptInvalidSaltError): ...
class PyAVDUtilsSha512CryptInvalidSaltCharacterError(PyAVDUtilsSha512CryptInvalidSaltError): ...
class PyAVDUtilsSha512CryptLibraryError(PyAVDUtilsSha512CryptError): ...
class PyAVDUtilsCBCError(PyAVDUtilsPasswordError): ...
class PyAVDUtilsCBCInvalidBase64Error(PyAVDUtilsCBCError): ...
class PyAVDUtilsCBCDecryptionFailedError(PyAVDUtilsCBCError): ...
class PyAVDUtilsCBCInvalidSignatureError(PyAVDUtilsCBCError): ...
class PyAVDUtilsCBCInvalidUtf8Error(PyAVDUtilsCBCError): ...
class PyAVDUtilsCBCEncryptionFailedError(PyAVDUtilsCBCError): ...
class PyAVDUtilsCBCInvalidBase64Utf8Error(PyAVDUtilsCBCError): ...
class PyAVDUtilsSimple7Error(PyAVDUtilsPasswordError): ...
class PyAVDUtilsSimple7InvalidSaltFormatError(PyAVDUtilsSimple7Error): ...
class PyAVDUtilsSimple7InvalidHexEncodingError(PyAVDUtilsSimple7Error): ...
class PyAVDUtilsSimple7RandomSourceUnavailableError(PyAVDUtilsSimple7Error): ...
class PyAVDUtilsSimple7InvalidUtf8Error(PyAVDUtilsSimple7Error): ...
class PyAVDUtilsSimple7InvalidSaltValueError(PyAVDUtilsSimple7Error): ...
class PyAVDUtilsSimple7DataTooShortError(PyAVDUtilsSimple7Error): ...
class PyAVDUtilsSimple7EmptyPasswordError(PyAVDUtilsSimple7Error): ...

def sha512_crypt(password: str, salt: str) -> str:
    """
    Computes the SHA512 crypt value for the password given the salt.

    The number of rounds is hardcoded to 5000 as expected by EOS.

    Args:
      password: The password.
      salt: The salt to use (truncated to 16 characters). Allowed characters are [a-zA-Z0-9/.].

    Returns:
      The sha512 crypt value.

    Raises:
      PyAVDUtilsSha512CryptInvalidSaltError: If the salt is empty or contains invalid characters.
      PyAVDUtilsSha512CryptLibraryError: If the underlying SHA crypt library returns an error.
    """

def cbc_encrypt(key: str, data: str) -> str:
    """
    Encrypt the data string using CBC TripleDES.

    Args:
        key: The encryption key.
        data: The data to be encrypted.

    Returns:
        str: The encrypted data, encoded in base64.

    Raises:
      PyAVDUtilsCBCEncryptionFailedError: If encryption fails.
      PyAVDUtilsCBCInvalidBase64Utf8Error: If base64 output contains invalid UTF-8.
    """

def cbc_decrypt(key: str, encrypted_data: str) -> str:
    """
    Decrypt the encrypted_data string using CBC TripleDES.

    Args:
        key: The encryption key.
        encrypted_data: The base64-encoded encrypted data to be decrypted.

    Returns:
        str: The decrypted data.

    Raises:
      PyAVDUtilsCBCInvalidBase64Error: If encrypted_data is not a valid base64 string.
      PyAVDUtilsCBCDecryptionFailedError: If decryption fails.
      PyAVDUtilsCBCInvalidSignatureError: If the decrypted Arista signature is invalid.
      PyAVDUtilsCBCInvalidUtf8Error: If decrypted data is not valid UTF-8.
    """

def cbc_verify(key: str, encrypted_data: str) -> str:
    """
    Verify if an encrypted password is decryptable with the given key.

    It does not return the password but only raises an error if the password cannot be decrypted.

    Args:
        key: The decryption key.
        encrypted_data: The base64-encoded encrypted data to be decrypted.

    Returns:
        bool: `True` if the password is decryptable, `False` otherwise.
    """

def simple_7_encrypt(data: str, salt: int | None) -> str:
    """
    Encrypt (obfuscate) a password with insecure type-7.

    WARNING: Type-7 encryption is NOT secure and should only be used for compatibility
    with legacy systems. It provides only obfuscation, not real encryption.

    Args:
        data: The password to encrypt.
        salt: The salt value (0-15). If None, a random salt will be generated.

    Returns:
        str: The encrypted password in type-7 format.

    Raises:
        PyAVDUtilsSimple7InvalidSaltValueError: If the salt is not in the range 0-15.
        PyAVDUtilsSimple7EmptyPasswordError: If the password is empty.
    """

def simple_7_decrypt(data: str) -> str:
    """
    Decrypt (deobfuscate) a password from insecure type-7.

    WARNING: Type-7 encryption is NOT secure and should only be used for compatibility
    with legacy systems. It provides only obfuscation, not real encryption.

    Args:
        data: The type-7 encrypted password to decrypt.

    Returns:
        str: The decrypted password.

    Raises:
        PyAVDUtilsSimple7DataTooShortError: If the encrypted data is too short.
        PyAVDUtilsSimple7InvalidSaltFormatError: If the encrypted data has an invalid salt format.
        PyAVDUtilsSimple7InvalidHexEncodingError: If the encrypted data has invalid hex encoding.
        PyAVDUtilsSimple7InvalidSaltValueError: If the salt is out of range.
        PyAVDUtilsSimple7InvalidUtf8Error: If the decrypted data is not valid UTF-8.
    """
