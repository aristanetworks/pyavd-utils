# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.

from contextlib import AbstractContextManager
from contextlib import nullcontext as does_not_raise

import pytest

from pyavd_utils.passwords import cbc_decrypt, cbc_encrypt, cbc_verify

CBC_ENCRYPT_TEST_DATA = [
    pytest.param(
        "42.42.42.42_passwd",
        "arista",
        "3QGcqpU2YTwKh2jVQ4Vj/A==",
        id="Valid encryption",
    ),
]


@pytest.mark.parametrize(("key", "data", "expected_b64"), CBC_ENCRYPT_TEST_DATA)
def test_cbc_encrypt_success(key: str, data: str, expected_b64: str) -> None:
    """Test cbc_encrypt."""
    assert cbc_encrypt(key, data) == expected_b64


CBC_DECRYPT_TEST_DATA = [
    pytest.param(
        "42.42.42.42_passwd",
        "3QGcqpU2YTwKh2jVQ4Vj/A==",
        "arista",
        does_not_raise(),
        id="Valid decryption",
    ),
    pytest.param(
        "any_key",
        "NotBase64!!!",
        "",
        pytest.raises(ValueError, match="Invalid Base64 encoding"),
        id="Invalid base64 input",
    ),
    pytest.param(
        "wrong_password",
        "bM7t58t04qSqLHAfZR/Szg==",
        "",
        pytest.raises(RuntimeError, match="Invalid Arista signature"),
        id="Wrong password (signature mismatch)",
    ),
    pytest.param(
        "any_key",
        "YWJjZA==",
        "",
        pytest.raises(RuntimeError, match="Decryption failed"),
        id="Block size / Alignment failure",
    ),
    pytest.param(
        "42.42.42.42_passwd",
        "Sh5yjV8SD2j//////////9pkhd5VI3SbQDy17ujMdko=",
        "",
        pytest.raises(ValueError, match="Decrypted data is not valid UTF-8"),
        id="Invalid UTF-8 sequence in decrypted data",
    ),
]


@pytest.mark.parametrize(("key", "encrypted_data", "expected_plain", "expected_raise"), CBC_DECRYPT_TEST_DATA)
def test_cbc_decrypt(key: str, encrypted_data: str, expected_plain: str, expected_raise: AbstractContextManager[None]) -> None:
    """Test cbc_decrypt susccess and various failures."""
    with expected_raise:
        assert cbc_decrypt(key, encrypted_data) == expected_plain


CBC_VERIFY_TEST_DATA = [
    pytest.param(
        "42.42.42.42_passwd",
        "3QGcqpU2YTwKh2jVQ4Vj/A==",
        True,
        id="Verify success",
    ),
    pytest.param(
        "wrong_password",
        "LIi7vE5hcmlzdGEAAAA=",
        False,
        id="Verify failure (wrong password)",
    ),
    pytest.param(
        "any_key",
        "NotBase64!!!",
        False,
        id="Verify failure (invalid base64)",
    ),
]


@pytest.mark.parametrize(("key", "encrypted_data", "expected_bool"), CBC_VERIFY_TEST_DATA)
def test_cbc_verify(key: str, encrypted_data: str, expected_bool: bool) -> None:
    """Test cbc_verify."""
    assert cbc_verify(key, encrypted_data) == expected_bool
