# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Shared schema store helpers."""

from __future__ import annotations

# The native Rust module is not built in CI, so this suppression is required there.
from ._bindings import _schema_store  # pyright: ignore[reportMissingModuleSource]

get_list_primary_key = _schema_store.get_list_primary_key
init_store_from_file = _schema_store.init_store_from_file

ValidationError = _schema_store.ValidationError
ValidationInvalidSchemaNameError = _schema_store.ValidationInvalidSchemaNameError
ValidationSchemaPathError = _schema_store.ValidationSchemaPathError
ValidationStoreNotInitializedError = _schema_store.ValidationStoreNotInitializedError
ValidationStoreAlreadyInitializedError = _schema_store.ValidationStoreAlreadyInitializedError
ValidationStoreLoadError = _schema_store.ValidationStoreLoadError
ValidationStoreLoadIoError = _schema_store.ValidationStoreLoadIoError

__all__ = [
    "ValidationError",
    "ValidationInvalidSchemaNameError",
    "ValidationSchemaPathError",
    "ValidationStoreAlreadyInitializedError",
    "ValidationStoreLoadError",
    "ValidationStoreLoadIoError",
    "ValidationStoreNotInitializedError",
    "get_list_primary_key",
    "init_store_from_file",
]
