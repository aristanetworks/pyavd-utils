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

__all__ = [
    "cbc_decrypt",
    "cbc_encrypt",
    "cbc_verify",
    "sha512_crypt",
    "simple_7_decrypt",
    "simple_7_encrypt",
]
