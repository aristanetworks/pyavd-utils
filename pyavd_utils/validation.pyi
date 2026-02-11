# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# Including docstrings since that is why we want this. Also allowing bad name style to match pyo3 output.
# ruff: noqa: PYI021
from pathlib import Path
from typing import Literal

class Configuration:
    """Configuration for validation behavior."""

    ignore_required_keys_on_root_dict: bool
    """Ignore required keys on the root dictionary."""

    return_coercion_infos: bool
    """Return coercion information in the validation result."""

    restrict_null_values: bool
    """Emit type errors for Null values instead of ignoring them."""

    warn_eos_cli_config_gen_keys: bool
    """When validating avd_design, emit warnings for top-level keys that exist in eos_config but not in avd_design."""

    def __init__(
        self,
        *,
        ignore_required_keys_on_root_dict: bool = False,
        return_coercion_infos: bool = False,
        restrict_null_values: bool = False,
        warn_eos_cli_config_gen_keys: bool = False,
    ) -> None: ...

class Violation:
    """Input data violates the schema."""

    message: str
    """String detailing the violation."""
    path: list[str]
    """Path to the data which the violation concerns."""

class Deprecation:
    """Input data model is deprecated."""

    message: str
    """String detailing the deprecation."""
    path: list[str]
    """Path to the data which uses a deprecated data model."""
    removed: bool
    """True when the data model is removed."""
    version: str | None
    """Version where the model will be removed."""
    replacement: str | None
    """New data model to use instead."""
    url: str | None
    """Url where more information can be found."""

class IgnoredEosConfigKey:
    """EOS Config key found in AVD Design input."""

    message: str
    """String detailing the ignored key."""
    path: list[str]
    """Path to the ignored key."""

class ValidationResult:
    """Result of data validation."""

    violations: list[Violation]
    deprecations: list[Deprecation]
    ignored_eos_config_keys: list[IgnoredEosConfigKey]

class ValidatedDataResult:
    """Result of data validation including the validated data as JSON."""

    validation_result: ValidationResult
    validated_data: str | None

def init_store_from_file(file: Path) -> None:
    """
    Initialize the Schema store from a file containing the full schema store.

    Usually this is the schema.json.gz file built with pyavd.
    This must be called before running any validations, since the store is a write-once static.

    Args:
        file: Path to the json, yml or json.gz file holding the schema store.

    Raises:
        RuntimeError: For any issue hit during loading, deserializing, combining and resolving schemas.
    """

def validate_json(
    data_as_json: str,
    schema_name: Literal["eos_config", "avd_design"],
    configuration: Configuration | None = None,
) -> ValidationResult:
    """
    Validate data against a schema specified by name.

    Args:
        data_as_json: Structured data dumped as JSON.
        schema_name: The name of the schema to validate against.
        configuration: Optional configuration for validation behavior.

    Returns:
        ValidationResult holding lists of violations and deprecations.
    """

def get_validated_data(
    data_as_json: str,
    schema_name: Literal["eos_config", "avd_design"],
    configuration: Configuration | None = None,
) -> ValidatedDataResult:
    """
    Validate data against a schema specified by name and return the data after coercion and validation.

    This returned data is the type-coerced data encoded as JSON, which also contains default values that got inserted during validation.

    Args:
        data_as_json: Structured data dumped as JSON.
        schema_name: The name of the schema to validate against.
        configuration: Optional configuration for validation behavior.

    Returns:
        ValidatedDataResult holding the validated data and the ValidationResult with lists of violations and deprecations.
    """

def validate_json_with_adhoc_schema(
    data_as_json: str,
    schema_as_json: str,
    configuration: Configuration | None = None,
) -> ValidationResult:
    """
    Validate data against the given schema.

    Args:
        data_as_json: Structured data dumped as JSON.
        schema_as_json: A fully resolved schema dumped as JSON.
        configuration: Optional configuration for validation behavior.

    Returns:
        ValidationResult holding lists of violations and deprecations.
    """
