# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# Including docstrings since that is why we want this.
# ruff: noqa: PYI021
from pathlib import Path

def get_list_primary_key(schema_name: str, data_path: list[str]) -> str | None:
    """
    Return the primary key for a list schema at the given data path.

    Limitation:
        This only resolves static schema paths and dynamic root keys that can be
        inferred from schema defaults. User-defined dynamic root keys are not
        resolved because this helper does not accept input data or dynamic-key
        overrides.

    Args:
        schema_name: The name of the schema to inspect.
        data_path: Path to the data model list.

    Raises:
        RuntimeError: If the shared schema store has not been initialized.
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
