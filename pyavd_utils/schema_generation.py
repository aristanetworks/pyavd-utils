# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Schema-driven artifact generation."""

from __future__ import annotations

# The native Rust module is not built in CI, so this suppression is required there.
from ._bindings import _schema_generation  # pyright: ignore[reportMissingModuleSource]

generate_python_schema_models = _schema_generation.generate_python_schema_models
generate_python_schema_models_from_paths = _schema_generation.generate_python_schema_models_from_paths

__all__ = ["generate_python_schema_models", "generate_python_schema_models_from_paths"]
