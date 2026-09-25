# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
from __future__ import annotations

from typing import TYPE_CHECKING, Literal

import pytest

from pyavd_utils.schema_store import get_list_primary_key, init_store_from_file
from pyavd_utils_gen.schema_store import compile_schema_archive

if TYPE_CHECKING:
    from pathlib import Path


@pytest.mark.usefixtures("init_store")
def test_schema_store_init_store_from_file_twice_errors(tmp_path: Path) -> None:
    schema_file = tmp_path / "schemas.json"
    schema_file.write_text("{}", encoding="UTF-8")

    with pytest.raises(RuntimeError, match="Initialization can only happen once"):
        init_store_from_file(schema_file)


def test_compile_schema_archive_rejects_invalid_source(tmp_path: Path) -> None:
    source = tmp_path / "schemas.json"
    destination = tmp_path / "schemas.rkyv"
    source.write_text("not JSON", encoding="UTF-8")

    with pytest.raises(RuntimeError, match="Error while loading the Schema Store"):
        compile_schema_archive(source, destination)

    assert not destination.exists()


def test_compile_schema_archive_rejects_invalid_schema(tmp_path: Path) -> None:
    source = tmp_path / "schemas.json"
    destination = tmp_path / "schemas.rkyv"
    source.write_text(r'{"test":{"type":"str","$ref":"missing#"}}', encoding="UTF-8")

    with pytest.raises(RuntimeError, match="Unable to resolve schema reference 'missing#'"):
        compile_schema_archive(source, destination)

    assert not destination.exists()


@pytest.mark.usefixtures("init_store")
@pytest.mark.parametrize(
    ("schema_name", "data_path", "expected_primary_key"),
    [
        pytest.param("eos_config", ["ethernet_interfaces"], "name", id="eos_config_top_level_list"),
        pytest.param("eos_config", ["access_lists", "0", "sequence_numbers"], "sequence", id="eos_config_nested_list"),
        pytest.param("eos_config", ["access_lists", "sequence_numbers"], None, id="eos_config_nested_list_without_index"),
        pytest.param("eos_config", ["hostname"], None, id="eos_config_non_list_path"),
        pytest.param("eos_config", ["not_a_schema_key"], None, id="eos_config_unknown_path"),
        pytest.param("avd_design", ["node_type_keys"], "key", id="avd_design_node_type_keys"),
        pytest.param("avd_design", ["connected_endpoints_keys"], "key", id="avd_design_connected_endpoints_keys"),
        pytest.param("avd_design", ["network_services_keys"], "name", id="avd_design_network_services_keys"),
    ],
)
def test_schema_store_get_list_primary_key(schema_name: Literal["eos_config", "avd_design"], data_path: list[str], expected_primary_key: str | None) -> None:
    assert get_list_primary_key(schema_name, data_path) == expected_primary_key


@pytest.mark.usefixtures("init_store")
@pytest.mark.parametrize("schema_name", ["eos_cli_config_gen", "eos_designs", "cv_deploy"])
def test_schema_store_get_list_primary_key_unsupported_schema_name_errors(schema_name: str) -> None:
    with pytest.raises(RuntimeError, match="not supported"):
        # Intentionally violate the typed API contract to test runtime validation.
        get_list_primary_key(schema_name, [])  # pyright: ignore[reportArgumentType]
