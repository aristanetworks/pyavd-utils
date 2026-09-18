# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# Including docstrings since that is why we want this.
# ruff: noqa: PYI021
from pathlib import Path
from typing import Literal

def compile_schema_archive(source: Path, destination: Path) -> None:
    """
    Compile a source schema-store file into the archived runtime format.

    The destination is written atomically. Its parent directory must already exist. The resulting
    file can be memory-mapped by `init_store_from_file`.

    Args:
        source: Path to the source schema-store file.
        destination: Path where the compiled archive should be written.

    Raises:
        RuntimeError: If the source cannot be loaded, the schemas cannot be compiled, or the
            destination cannot be written.
    """

def get_list_primary_key(schema_name: Literal["eos_config"], data_path: list[str]) -> str | None:
    """
    Return the primary key for a list schema at the given data path.

    Limitation:
        The only supported schema name is "eos_config". Path resolution does not use caller data
        or dynamic-key overrides.

    Args:
        schema_name: The name of the schema to inspect.
        data_path: Path to the data model list.

    Raises:
        RuntimeError: If the shared schema store has not been initialized, if the schema name is
            not supported, or if schema resolution fails for reasons other than an unresolved
            schema path. Schema walk failures, such as unresolved nested-list paths, return None.
    """

def init_store_from_file(file: Path) -> None:
    """
    Initialize the shared Schema store from a compiled schema archive.

    The archive is validated and memory-mapped. Initialization can happen only once in each
    process and must happen before using APIs that rely on the shared schema store.

    Args:
        file: Path to the compiled schema archive.

    Raises:
        RuntimeError: If the store was already initialized or the archive cannot be opened or validated.
    """
