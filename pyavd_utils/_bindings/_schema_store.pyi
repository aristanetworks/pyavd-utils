# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# Including docstrings since that is why we want this.
# ruff: noqa: PYI021
from pathlib import Path
from typing import Literal

class SchemaInfo:
    """Minimal metadata for a resolved schema node."""

    @property
    def schema_type(self) -> Literal["bool", "int", "str", "list", "dict"]:
        """Type of the resolved schema node."""

    @property
    def primary_key(self) -> str | None:
        """Primary key for a list schema, if configured; None for other types."""

def get_schema_info(schema_name: Literal["eos_config", "avd_design"], data_path: list[str]) -> SchemaInfo | None:
    """
    Return minimal metadata for the schema at the given data path, or None if unresolved.

    Limitation:
        General data-aware dynamic-key resolution is not supported. For "avd_design", the
        existing empty-list inputs for node_type_keys, connected_endpoints_keys, and
        network_services_keys suppress their corresponding defaults. Other schema-default
        resolution follows the existing traversal rules.
        The supported schema names are "eos_config" and "avd_design".

    Args:
        schema_name: The name of the schema to inspect.
        data_path: Path to the data model node. Numeric strings traverse list items;
            an empty path returns metadata for the root dictionary.

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

    Warning:
        The archive file must not be modified or truncated in place after initialization. Publish
        an updated archive by writing a separate file and replacing the path atomically, or use a
        new path.

    Args:
        file: Path to the compiled schema archive.

    Raises:
        RuntimeError: If the store was already initialized or the archive cannot be opened or validated.
    """
