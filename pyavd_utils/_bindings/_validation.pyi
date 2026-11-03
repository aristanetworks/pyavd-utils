# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# Including docstrings since that is why we want this. Also allowing bad name style to match pyo3 output.
# ruff: noqa: PYI021
from typing import Literal

class Configuration:
    """Configuration for validation behavior."""

    ignore_required_keys_on_root_dict: bool
    """Ignore required keys on the root dictionary."""

    return_coercion_infos: bool
    """Return coercion information in the validation result."""

    restrict_null_values: bool
    """Emit type errors for Null values instead of ignoring them."""

    warn_eos_config_keys: bool
    """When validating avd_design, emit warnings for top-level keys that exist in eos_config but not in avd_design."""

    def __init__(
        self,
        *,
        ignore_required_keys_on_root_dict: bool = False,
        return_coercion_infos: bool = False,
        restrict_null_values: bool = False,
        warn_eos_config_keys: bool = False,
    ) -> None: ...

class SourceSpan:
    """Zero-based UTF-8 byte range in the original uncompressed input."""

    start: int
    """Inclusive start offset."""
    end: int
    """Exclusive end offset; equal offsets represent a zero-width span."""

class Violation:
    """Input data violates the schema."""

    message: str
    """String detailing the violation."""
    source: str | None
    """Opaque caller-provided source identifier, if available."""
    span: SourceSpan | None
    """Range associated with this finding, if available."""
    path: list[str]
    """Path to the data which the violation concerns."""

class Deprecation:
    """Input data model is deprecated."""

    message: str
    """String detailing the deprecation."""
    source: str | None
    """Opaque caller-provided source identifier, if available."""
    span: SourceSpan | None
    """Range associated with this finding, if available."""
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
    upgrade_handler: str | None
    """Handler to use when upgrading the data model."""

class IgnoredEosConfigKey:
    """EOS Config key found in AVD Design input."""

    message: str
    """String detailing the ignored key."""
    source: str | None
    """Opaque caller-provided source identifier, if available."""
    span: SourceSpan | None
    """Range associated with this finding, if available."""
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

def validate_json(
    data_as_json: str,
    schema_name: Literal["eos_config", "avd_design", "cv_deploy"],
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
    schema_name: Literal["eos_config", "avd_design", "cv_deploy"],
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
    Validate data against an ad hoc dictionary-root schema.

    Root properties such as ``dynamic_keys`` and ``$defs`` are supported. References
    within this document use the schema name ``adhoc``, for example
    ``adhoc#/$defs/value``. The root's ``$id`` is metadata and does not change that name.

    Args:
        data_as_json: Structured data dumped as JSON.
        schema_as_json: A dictionary-root schema dumped as JSON.
        configuration: Optional configuration for validation behavior.

    Returns:
        ValidationResult holding lists of violations and deprecations.
    """
