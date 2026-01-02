# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# For now we allow docstrings in stubs
# ruff: noqa: PYI021

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
      ValueError: If the salt is empty or contain invalid characters.
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
      RunTimeError: If anything fails during encryption.
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
      ValueError: If encrypted_data is not a valid base64 string.
      RunTimeError: If anything fails during decryption.
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
        ValueError: If the salt is not in the range 0-15.
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
        ValueError: If the encrypted data is invalid (too short, invalid format, invalid hex, or salt out of range).
        RuntimeError: If the decrypted data is not valid UTF-8.
    """
