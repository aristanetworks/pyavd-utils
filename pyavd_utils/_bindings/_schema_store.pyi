# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# Including docstrings since that is why we want this.
# ruff: noqa: PYI021
from pathlib import Path
from typing import Literal

class ValidationError(Exception): ...
class ValidationInvalidSchemaNameError(ValidationError): ...
class ValidationSchemaPathError(ValidationError): ...
class ValidationStoreNotInitializedError(ValidationError): ...
class ValidationStoreAlreadyInitializedError(ValidationError): ...
class ValidationStoreLoadError(ValidationError): ...
class ValidationStoreLoadIoError(ValidationStoreLoadError): ...

def get_list_primary_key(schema_name: Literal["eos_config", "avd_design"], data_path: list[str]) -> str | None:
    """
    Return the primary key for a list schema at the given data path.

    Limitation:
        Dynamic keys in the AVD design schema are not supported today; only static schema paths
        can be inspected. The supported schema names are "eos_config" and "avd_design".

    Args:
        schema_name: The name of the schema to inspect.
        data_path: Path to the data model list.

    Raises:
        ValidationStoreNotInitializedError: If the shared schema store has not been initialized.
        ValidationInvalidSchemaNameError: If the schema name is not supported.
        ValidationSchemaPathError: If schema resolution fails.
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
        ValidationStoreAlreadyInitializedError: If the store was already initialized.
        ValidationStoreLoadError: If the archive cannot be validated or loaded.
        ValidationStoreLoadIoError: If the archive cannot be opened.
    """
