# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Shared schema store helpers."""

from __future__ import annotations

# The native Rust module is not built in CI, so this suppression is required there.
from ._bindings import _schema_store  # pyright: ignore[reportMissingModuleSource]

get_list_primary_key = _schema_store.get_list_primary_key
init_store_from_file = _schema_store.init_store_from_file

__all__ = ["get_list_primary_key", "init_store_from_file"]
