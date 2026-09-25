# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# ruff: noqa: PYI021
from pathlib import Path

def generate_metaschema(destination: Path) -> None:
    """
    Write the AVD source-schema metaschema as formatted JSON.

    Args:
        destination: Path where the generated JSON Schema should be written.

    Raises:
        RuntimeError: If the metaschema cannot be generated or the destination cannot be written.
    """
