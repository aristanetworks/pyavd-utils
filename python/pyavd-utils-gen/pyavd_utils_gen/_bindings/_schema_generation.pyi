# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# ruff: noqa: PYI021
from collections.abc import Mapping
from pathlib import Path

def generate_schema_explorer_jsonl(source: Path, schema_names: list[str], destination: Path) -> dict[str, int]:
    """
    Export a raw schema store's selected field occurrences as UTF-8 JSON Lines.

    The selected schemas are compiled completely before occurrence traversal.
    Hidden dictionaries stop descendant traversal but retain effective metadata
    and cross-schema reference links. Removed fields remain available. Each line
    is one field row; defaults and constraints are JSON values, not encoded strings.
    Rows follow schema selection order and depth-first field order. Dictionary
    list items provide context without emitting their own rows. Paths use []
    for list items and <path> for dynamic keys. Consumers own persistence such
    as SQLite indexes, row IDs, and generation timestamps.

    Returns field counts by schema name, including zero for empty schemas.
    Duplicate names raise ValueError. Invalid schemas raise RuntimeError before
    the destination is opened. The caller must create its parent directory.
    No runtime archives, sibling files, or directories are modified.
    """

def generate_schema_explorer_jsonl_from_paths(sources: Mapping[str, Path], schema_names: list[str], destination: Path) -> dict[str, int]:
    """
    Export named source-schema files using the same JSON Lines field-row contract.

    Sources must include selected schemas and their reference dependencies; no
    metaschema file is required. Schema interpretation is identical to the
    raw-store variant, including hidden-key pruning and preserved field order.
    """

def generate_schema_documentation(source: Path, schema_name: str, destination: Path) -> None:
    """
    Generate Markdown schema documentation from a raw schema store.

    Output names may contain lowercase ASCII letters, digits, underscores, hyphens,
    and dots. Invalid names raise ValueError before directory creation, cleanup, or writes.
    """

def generate_schema_documentation_from_paths(sources: Mapping[str, Path], schema_name: str, destination: Path) -> None:
    """
    Generate Markdown schema documentation from individually named schema files.

    Output names may contain lowercase ASCII letters, digits, underscores, hyphens,
    and dots. Invalid names raise ValueError before directory creation, cleanup, or writes.
    """

def generate_python_schema_models(
    source: Path,
    schema_name: str,
    destination: Path,
    generated_class_name: str | None = None,
    root_keys: list[str] | None = None,
) -> None:
    """Generate nested Python schema models from a source schema-store file."""

def generate_python_schema_models_from_paths(
    sources: Mapping[str, Path],
    schema_name: str,
    destination: Path,
    generated_class_name: str | None = None,
    root_keys: list[str] | None = None,
) -> None:
    """Generate nested Python schema models from individually named source schema paths."""
