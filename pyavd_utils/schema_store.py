# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Shared schema store helpers."""

from __future__ import annotations

from ._bindings import _schema_store

init_store_from_file = _schema_store.init_store_from_file

__all__ = ["init_store_from_file"]
