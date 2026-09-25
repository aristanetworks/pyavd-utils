# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# Including docstrings since that is why we want this.
# ruff: noqa: PYI021
from collections.abc import Mapping
from pathlib import Path

def generate_python_schema_models(
    source: Path,
    schema_name: str,
    destination: Path,
    generated_class_name: str | None = None,
    root_keys: list[str] | None = None,
) -> None:
    """Generate nested Python schema models from a raw schema store."""

def generate_python_schema_models_from_paths(
    sources: Mapping[str, Path],
    schema_name: str,
    destination: Path,
    generated_class_name: str | None = None,
    root_keys: list[str] | None = None,
) -> None:
    """Generate nested Python schema models from individually named schema files."""
