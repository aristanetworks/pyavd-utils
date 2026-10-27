# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# ruff: noqa: PYI021
from pathlib import Path

def compile_schema_archive(source: Path, destination: Path) -> None:
    """
    Compile a source schema-store file into the archived runtime format.

    Authoring checks reject invalid regexes, multiline display names, and invalid schema key
    names, including in unused definitions. Source deserialization
    remains tolerant; these checks apply when generating an archive.

    The destination is written atomically. Its parent directory must already exist. The resulting
    file can be memory-mapped by `pyavd_utils.schema_store.init_store_from_file`.

    Args:
        source: Path to the source schema-store file.
        destination: Path where the compiled archive should be written.

    Raises:
        RuntimeError: If the source cannot be loaded, authoring checks fail, compilation fails, or the
            destination cannot be written.
    """
