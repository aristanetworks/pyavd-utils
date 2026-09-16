# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
# Including docstrings since that is why we want this.
# ruff: noqa: PYI021
from typing import Literal

def merge_json(
    base_as_json: str,
    nexts_as_json: list[str],
    schema_name: Literal["eos_config", "avd_design", "cv_deploy"],
    *,
    list_merge: Literal["replace", "append", "keep", "keep_merge", "prepend", "append_unique", "prepend_unique"] = "append_unique",
) -> str:
    """
    Merge JSON documents using the initialized schema store.

    Dynamic-key schemas are not blocked, but they are not fully supported. If a merge input modifies data used to resolve dynamic keys during the same merge,
    nested primary-key list merging may use stale schema resolution and produce unexpected results.

    For lists with schema primary keys, append/prepend strategies deep-merge by primary key. Replace/keep retain their full-list semantics.
    Keep-merge deep-merges matching primary-key items while keeping the existing list.
    Items with a missing or null primary key follow the ordinary list strategy, including full-value deduplication for unique strategies.

    Args:
        base_as_json: Base structured data dumped as JSON.
        nexts_as_json: Structured data dumps as JSON to merge into base in order.
        schema_name: The name of the schema to guide list primary-key merge behavior.
        list_merge: List strategy. For primary-key lists, replace/keep apply to the full list, keep_merge merges matching items only, and
            append/prepend merge by primary key.

    Returns:
        The merged data encoded as JSON.
    """
