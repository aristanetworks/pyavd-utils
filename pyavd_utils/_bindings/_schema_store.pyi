# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# Including docstrings since that is why we want this.
# ruff: noqa: PYI021
from pathlib import Path
from typing import Literal

def get_list_primary_key(schema_name: Literal["eos_config"], data_path: list[str]) -> str | None:
    """
    Return the primary key for a list schema at the given data path.

    Limitation:
        This only supports the EOS config schema for now, since other AVD schemas can use
        dynamic keys which are not supported by this helper yet. The only supported schema
        name is "eos_config".

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
    Initialize the shared Schema store from a file containing the full schema store.

    Usually this is the schema.json.gz file built with pyavd.
    This must be called before using validation or schema-merge APIs that rely on the shared store.

    Args:
        file: Path to the json, yml or json.gz file holding the schema store.

    Raises:
        RuntimeError: For any issue hit during loading, deserializing, combining and resolving schemas.
    """
