# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
from __future__ import annotations

import json
from typing import Any, cast

import pytest

from pyavd_utils.schema_merge import merge_json
from pyavd_utils.validation import validate_json


@pytest.mark.usefixtures("init_store")
def test_schema_merge_uses_initialized_store() -> None:
    merged = merge_json(
        '{"ethernet_interfaces": [{"name": "Ethernet1", "description": "base"}]}',
        [
            '{"ethernet_interfaces": [{"name": "Ethernet1", "description": "next"}, {"name": "Ethernet2"}]}',
            '{"ethernet_interfaces": [{"name": "Ethernet1", "shutdown": true}]}',
        ],
        "eos_config",
    )

    assert json.loads(merged) == {
        "ethernet_interfaces": [
            {"name": "Ethernet1", "description": "next", "shutdown": True},
            {"name": "Ethernet2"},
        ],
    }
    assert not validate_json(merged, "eos_config").violations


@pytest.mark.usefixtures("init_store")
def test_schema_merge_invalid_strategy() -> None:
    with pytest.raises(ValueError, match="Invalid list merge strategy"):
        merge_json("{}", ["{}"], "eos_config", list_merge=cast("Any", "invalid"))


@pytest.mark.usefixtures("init_store")
def test_schema_merge_invalid_schema_name_is_always_reported() -> None:
    with pytest.raises(RuntimeError, match="Unable to look up schema path"):
        merge_json(
            '{"hostname": "base"}',
            ['{"hostname": "next"}'],
            cast("Any", "invalid"),
        )


@pytest.mark.usefixtures("init_store")
def test_schema_merge_unique_strategy_deduplicates_items_without_primary_keys() -> None:
    merged = merge_json(
        '{"ethernet_interfaces": [{"description": "same"}]}',
        [
            ('{"ethernet_interfaces": [{"description": "same"},{"description": "new"},{"description": "new"}]}'),
        ],
        "eos_config",
    )

    assert json.loads(merged) == {
        "ethernet_interfaces": [
            {"description": "same"},
            {"description": "new"},
        ],
    }


@pytest.mark.usefixtures("init_store")
def test_schema_merge_accepts_nexts_keyword() -> None:
    merged = merge_json(
        base_as_json='{"hostname": "base"}',
        nexts_as_json=['{"hostname": "next"}'],
        schema_name="eos_config",
    )

    assert json.loads(merged) == {"hostname": "next"}


@pytest.mark.usefixtures("init_store")
def test_schema_merge_keep_merge_merges_primary_key_matches_only() -> None:
    merged = merge_json(
        base_as_json=(
            '{"ethernet_interfaces": [{"name": "Ethernet1", "description": "base", "shutdown": false},{"name": "Ethernet2", "description": "base only"}]}'
        ),
        nexts_as_json=[
            ('{"ethernet_interfaces": [{"name": "Ethernet1", "shutdown": true},{"name": "Ethernet3", "description": "next only"}]}'),
        ],
        schema_name="eos_config",
        list_merge="keep_merge",
    )

    assert json.loads(merged) == {
        "ethernet_interfaces": [
            {"name": "Ethernet1", "description": "base", "shutdown": True},
            {"name": "Ethernet2", "description": "base only"},
        ],
    }
