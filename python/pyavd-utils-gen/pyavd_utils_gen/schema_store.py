# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Build-time schema-store compilation."""

from __future__ import annotations

# The native Rust module is not built for standalone static analysis.
from ._bindings import _schema_store  # pyright: ignore[reportMissingModuleSource]

compile_schema_archive = _schema_store.compile_schema_archive

__all__ = ["compile_schema_archive"]
