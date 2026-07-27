# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# Including docstrings since that is why we want this.
# ruff: noqa: PYI021
from pathlib import Path

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
