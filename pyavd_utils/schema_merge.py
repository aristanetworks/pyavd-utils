# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Schema-guided merge helpers."""

from __future__ import annotations

from ._bindings import _schema_merge

merge_json = _schema_merge.merge_json

__all__ = ["merge_json"]
