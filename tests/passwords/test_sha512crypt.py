# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.

import pickle
from contextlib import AbstractContextManager
from contextlib import nullcontext as does_not_raise

import pytest

from pyavd_utils.passwords import (
    PasswordError,
    Sha512CryptBase64Error,
    Sha512CryptInvalidSaltCharacterError,
    Sha512CryptInvalidSaltEmptyError,
    Sha512CryptLibraryError,
    sha512_crypt,
)


def test_sha512_crypt_error_hierarchy() -> None:
    """Test that SHA512 crypt errors inherit from the passwords base error."""
    assert issubclass(Sha512CryptInvalidSaltEmptyError, PasswordError)
    assert issubclass(Sha512CryptInvalidSaltCharacterError, PasswordError)
    assert issubclass(Sha512CryptLibraryError, PasswordError)
    assert issubclass(Sha512CryptBase64Error, PasswordError)


def test_sha512_crypt_error_module_and_pickle() -> None:
    """Test that SHA512 crypt errors have the public module path and can be pickled."""
    err = Sha512CryptInvalidSaltEmptyError("boom")

    assert Sha512CryptInvalidSaltEmptyError.__module__ == "pyavd_utils.passwords"
    unpickled = pickle.loads(pickle.dumps(err))  # noqa: S301
    assert type(unpickled) is Sha512CryptInvalidSaltEmptyError
    assert str(unpickled) == "boom"


SHA512_CRYPT_TEST_DATA = [
    pytest.param(
        "arista",
        "1234567890ABCDEF",
        "$6$1234567890ABCDEF$5h/.K2RuwSPqXTncNaqmw./4HduYZNE4RHDfivjrQ8nrYX3AcB8gKSsKFC1VSVOl3E46/QFZ85uHZWhxQGTeS0",
        does_not_raise(),
        id="Valid hash with salt",
    ),
    pytest.param(
        "arista",
        "",
        "",
        pytest.raises(Sha512CryptInvalidSaltEmptyError, match=r"Invalid Salt: Salt cannot be empty."),
        id="Empty salt",
    ),
    pytest.param(
        "arista",
        "🐍",
        "",
        pytest.raises(Sha512CryptInvalidSaltCharacterError, match="Invalid Salt: Salt contains an invalid character"),
        id="Invalid character in salt",
    ),
]


@pytest.mark.parametrize(("password", "salt", "expected_hash", "expected_raise"), SHA512_CRYPT_TEST_DATA)
def test_sha512_crypt(password: str, salt: str, expected_hash: str, expected_raise: AbstractContextManager[None]) -> None:
    """Test sha512_crypt function."""
    with expected_raise:
        assert sha512_crypt(password, salt) == expected_hash
