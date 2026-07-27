# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Schema validation helpers."""

from __future__ import annotations

from ._bindings import _validation

Configuration = _validation.Configuration
Deprecation = _validation.Deprecation
IgnoredEosConfigKey = _validation.IgnoredEosConfigKey
ValidatedDataResult = _validation.ValidatedDataResult
ValidationResult = _validation.ValidationResult
Violation = _validation.Violation
get_validated_data = _validation.get_validated_data
validate_json = _validation.validate_json
validate_json_with_adhoc_schema = _validation.validate_json_with_adhoc_schema

__all__ = [
    "Configuration",
    "Deprecation",
    "IgnoredEosConfigKey",
    "ValidatedDataResult",
    "ValidationResult",
    "Violation",
    "get_validated_data",
    "validate_json",
    "validate_json_with_adhoc_schema",
]
