# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
from __future__ import annotations

from typing import TYPE_CHECKING

import pytest

from pyavd_utils.schema_store import get_list_primary_key, init_store_from_file

if TYPE_CHECKING:
    from pathlib import Path


@pytest.mark.usefixtures("init_store")
def test_schema_store_init_store_from_file_twice_errors(tmp_path: Path) -> None:
    schema_file = tmp_path / "schemas.json"
    schema_file.write_text("{}", encoding="UTF-8")

    with pytest.raises(RuntimeError, match="Initialization can only happen once"):
        init_store_from_file(schema_file)


@pytest.mark.usefixtures("init_store")
@pytest.mark.parametrize(
    ("data_path", "expected_primary_key"),
    [
        pytest.param(["ethernet_interfaces"], "name", id="top_level_list"),
        pytest.param(["access_lists", "0", "sequence_numbers"], "sequence", id="nested_list"),
        pytest.param(["access_lists", "sequence_numbers"], None, id="nested_list_without_index"),
        pytest.param(["hostname"], None, id="non_list_path"),
        pytest.param(["not_a_schema_key"], None, id="unknown_path"),
    ],
)
def test_schema_store_get_list_primary_key(data_path: list[str], expected_primary_key: str | None) -> None:
    assert get_list_primary_key("eos_config", data_path) == expected_primary_key


@pytest.mark.usefixtures("init_store")
@pytest.mark.parametrize("schema_name", ["eos_cli_config_gen", "eos_designs"])
def test_schema_store_get_list_primary_key_unsupported_schema_name_errors(schema_name: str) -> None:
    with pytest.raises(RuntimeError, match="not supported"):
        # Intentionally violate the typed API contract to test runtime validation.
        get_list_primary_key(schema_name, [])  # pyright: ignore[reportArgumentType]
