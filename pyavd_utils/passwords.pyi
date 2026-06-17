# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# For now we allow docstrings in stubs
# ruff: noqa: PYI021

class PasswordError(Exception): ...
class Sha512CryptInvalidSaltEmptyError(PasswordError): ...
class Sha512CryptInvalidSaltCharacterError(PasswordError): ...
class Sha512CryptLibraryError(PasswordError): ...
class Sha512CryptBase64Error(PasswordError): ...
class CBCInvalidBase64Error(PasswordError): ...
class CBCDecryptionFailedError(PasswordError): ...
class CBCInvalidSignatureError(PasswordError): ...
class CBCInvalidUtf8Error(PasswordError): ...
class CBCEncryptionFailedError(PasswordError): ...
class CBCInvalidBase64Utf8Error(PasswordError): ...
class Simple7InvalidSaltFormatError(PasswordError): ...
class Simple7InvalidHexEncodingError(PasswordError): ...
class Simple7RandomSourceUnavailableError(PasswordError): ...
class Simple7InvalidUtf8Error(PasswordError): ...
class Simple7InvalidSaltValueError(PasswordError): ...
class Simple7DataTooShortError(PasswordError): ...
class Simple7EmptyPasswordError(PasswordError): ...

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
      Sha512CryptInvalidSaltEmptyError: If the salt is empty.
      Sha512CryptInvalidSaltCharacterError: If the salt contains invalid characters.
      Sha512CryptLibraryError: If the underlying SHA crypt library returns an error.
      Sha512CryptBase64Error: If SHA512 crypt Base64 encoding fails.
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
      CBCEncryptionFailedError: If encryption fails.
      CBCInvalidBase64Utf8Error: If base64 output contains invalid UTF-8.
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
      CBCInvalidBase64Error: If encrypted_data is not a valid base64 string.
      CBCDecryptionFailedError: If decryption fails.
      CBCInvalidSignatureError: If the decrypted Arista signature is invalid.
      CBCInvalidUtf8Error: If decrypted data is not valid UTF-8.
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
        Simple7InvalidSaltValueError: If the salt is not in the range 0-15.
        Simple7EmptyPasswordError: If the password is empty.
        Simple7RandomSourceUnavailableError: If random salt generation fails.
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
        Simple7DataTooShortError: If the encrypted data is too short.
        Simple7InvalidSaltFormatError: If the encrypted data has an invalid salt format.
        Simple7InvalidHexEncodingError: If the encrypted data has invalid hex encoding.
        Simple7InvalidSaltValueError: If the salt is out of range.
        Simple7InvalidUtf8Error: If the decrypted data is not valid UTF-8.
    """
