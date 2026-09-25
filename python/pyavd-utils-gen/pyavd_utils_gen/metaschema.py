# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""AVD source-schema metaschema generation."""

from __future__ import annotations

from ._bindings import _metaschema  # pyright: ignore[reportMissingModuleSource]

generate_metaschema = _metaschema.generate_metaschema

__all__ = ["generate_metaschema"]
