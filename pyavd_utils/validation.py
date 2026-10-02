# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Schema validation helpers."""

from __future__ import annotations

# The native Rust module is not built in CI, so this suppression is required there.
from ._bindings import _validation  # pyright: ignore[reportMissingModuleSource]

Configuration = _validation.Configuration
Deprecation = _validation.Deprecation
IgnoredEosConfigKey = _validation.IgnoredEosConfigKey
ValidatedDataResult = _validation.ValidatedDataResult
ValidationResult = _validation.ValidationResult
Violation = _validation.Violation
ValidationError = _validation.ValidationError
ValidationStoreAlreadyInitializedError = _validation.ValidationStoreAlreadyInitializedError
ValidationStoreLoadError = _validation.ValidationStoreLoadError
ValidationStoreLoadIoError = _validation.ValidationStoreLoadIoError
ValidationStoreNotInitializedError = _validation.ValidationStoreNotInitializedError
ValidationInvalidSchemaNameError = _validation.ValidationInvalidSchemaNameError
ValidationSchemaPathError = _validation.ValidationSchemaPathError
ValidationInvalidJsonDataError = _validation.ValidationInvalidJsonDataError
ValidationInvalidAdhocSchemaJsonError = _validation.ValidationInvalidAdhocSchemaJsonError
ValidationInvalidCoercedDataJsonError = _validation.ValidationInvalidCoercedDataJsonError
ValidationInternalError = _validation.ValidationInternalError
get_validated_data = _validation.get_validated_data
validate_json = _validation.validate_json
validate_json_with_adhoc_schema = _validation.validate_json_with_adhoc_schema

__all__ = [
    "Configuration",
    "Deprecation",
    "IgnoredEosConfigKey",
    "ValidatedDataResult",
    "ValidationError",
    "ValidationInternalError",
    "ValidationInvalidAdhocSchemaJsonError",
    "ValidationInvalidCoercedDataJsonError",
    "ValidationInvalidJsonDataError",
    "ValidationInvalidSchemaNameError",
    "ValidationResult",
    "ValidationSchemaPathError",
    "ValidationStoreAlreadyInitializedError",
    "ValidationStoreLoadError",
    "ValidationStoreLoadIoError",
    "ValidationStoreNotInitializedError",
    "Violation",
    "get_validated_data",
    "validate_json",
    "validate_json_with_adhoc_schema",
]
