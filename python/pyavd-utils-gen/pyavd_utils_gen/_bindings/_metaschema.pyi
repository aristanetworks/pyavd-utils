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

def is_valid_avd_regex_pattern(pattern: str) -> bool:
    r"""
    Return whether a pattern compiles with AVD's runtime regex compiler and settings.

    The compiler anchors the pattern to the complete value with ``^(?:...)$``.
    It supports Unicode-aware Perl shorthand classes (``\d``, ``\s``, ``\w`` and
    their complements), word boundaries, lookarounds, and variable-length lookbehinds.
    Broad Unicode properties and scripts such as ``\p{L}`` and ``\p{Greek}`` are
    not supported. Use literal backslashes in the supplied string, for example
    by using a Python raw string.

    This only checks compilation. It does not match input data or guarantee
    that matching cannot fail at runtime, for example on the backtracking limit.

    Args:
        pattern: An AVD schema regular expression.

    Returns:
        True if the pattern compiles, or False for malformed or unsupported patterns.
    """
