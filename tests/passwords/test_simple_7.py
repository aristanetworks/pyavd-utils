# Copyright (c) 2025 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.

from contextlib import AbstractContextManager
from contextlib import nullcontext as does_not_raise

import pytest

from pyavd_utils.passwords import simple_7_decrypt, simple_7_encrypt

SIMPLE_7_ENCRYPT_TEST_DATA = [
    pytest.param(
        "foo",
        1,
        "0115090B",
        does_not_raise(),
        id="Valid encryption with salt 1",
    ),
    pytest.param(
        "foo",
        6,
        "0600002E",
        does_not_raise(),
        id="Valid encryption with salt 6",
    ),
    pytest.param(
        "foo",
        9,
        "094A4106",
        does_not_raise(),
        id="Valid encryption with salt 9",
    ),
    pytest.param(
        "foo",
        15,
        "15140403",
        does_not_raise(),
        id="Valid encryption with salt 15",
    ),
    pytest.param(
        "test_password",
        16,
        "",
        pytest.raises(ValueError, match="Salt must be in the range 0-15, got 16"),
        id="Invalid salt value (16)",
    ),
    pytest.param(
        "test_password",
        99,
        "",
        pytest.raises(ValueError, match="Salt must be in the range 0-15, got 99"),
        id="Invalid salt value (99)",
    ),
]


@pytest.mark.parametrize(("data", "salt", "expected_encrypted", "expected_raise"), SIMPLE_7_ENCRYPT_TEST_DATA)
def test_simple_7_encrypt(data: str, salt: int, expected_encrypted: str, expected_raise: AbstractContextManager[None]) -> None:
    """Test simple_7_encrypt success and various failures."""
    with expected_raise:
        assert simple_7_encrypt(data, salt) == expected_encrypted


SIMPLE_7_DECRYPT_TEST_DATA = [
    pytest.param(
        "0115090B",
        "foo",
        does_not_raise(),
        id="Valid decryption with salt 1",
    ),
    pytest.param(
        "0600002E",
        "foo",
        does_not_raise(),
        id="Valid decryption with salt 6",
    ),
    pytest.param(
        "094A4106",
        "foo",
        does_not_raise(),
        id="Valid decryption with salt 9",
    ),
    pytest.param(
        "15140403",
        "foo",
        does_not_raise(),
        id="Valid decryption with salt 15",
    ),
    pytest.param(
        "",
        "",
        pytest.raises(ValueError, match="Encrypted data too short"),
        id="Data too short (empty)",
    ),
    pytest.param(
        "0",
        "",
        pytest.raises(ValueError, match="Encrypted data too short"),
        id="Data too short (1 char)",
    ),
    pytest.param(
        "01GGGG",
        "",
        pytest.raises(ValueError, match="Invalid hex encoding"),
        id="Invalid hex encoding",
    ),
    pytest.param(
        "XX1234",
        "",
        pytest.raises(ValueError, match="Invalid salt format"),
        id="Invalid salt format",
    ),
    pytest.param(
        "161234",
        "",
        pytest.raises(ValueError, match="Salt must be in the range 0-15, got 16"),
        id="Salt out of range (16)",
    ),
    pytest.param(
        "991234",
        "",
        pytest.raises(ValueError, match="Salt must be in the range 0-15, got 99"),
        id="Salt out of range (99)",
    ),
]


@pytest.mark.parametrize(("encrypted_data", "expected_plain", "expected_raise"), SIMPLE_7_DECRYPT_TEST_DATA)
def test_simple_7_decrypt(encrypted_data: str, expected_plain: str, expected_raise: AbstractContextManager[None]) -> None:
    """Test simple_7_decrypt success and various failures."""
    with expected_raise:
        assert simple_7_decrypt(encrypted_data) == expected_plain


SIMPLE_7_ROUNDTRIP_TEST_DATA = [
    pytest.param(
        "test_password",
        None,
        id="Roundtrip with random salt",
    ),
    pytest.param(
        "arista",
        5,
        id="Roundtrip with salt 5",
    ),
    pytest.param(
        "another_password",
        0,
        id="Roundtrip with salt 0",
    ),
]


@pytest.mark.parametrize(("password", "salt"), SIMPLE_7_ROUNDTRIP_TEST_DATA)
def test_simple_7_roundtrip(password: str, salt: int | None) -> None:
    """Test simple_7 encrypt/decrypt roundtrip."""
    encrypted = simple_7_encrypt(password, salt)
    decrypted = simple_7_decrypt(encrypted)
    assert decrypted == password
