# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
from __future__ import annotations

import json
from typing import TYPE_CHECKING, Literal

import pytest

from pyavd_utils.schema_store import SchemaInfo, get_schema_info, init_store_from_file
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
    source.write_text(r'{"test":{"type":"dict","$ref":"missing#"}}', encoding="UTF-8")

    with pytest.raises(RuntimeError, match="Unable to resolve schema reference 'missing#'"):
        compile_schema_archive(source, destination)

    assert not destination.exists()


@pytest.mark.usefixtures("init_store")
@pytest.mark.parametrize(
    ("schema_name", "data_path", "expected_schema_type", "expected_primary_key"),
    [
        pytest.param("eos_config", [], "dict", None, id="eos_config_root"),
        pytest.param("eos_config", ["ethernet_interfaces"], "list", "name", id="eos_config_top_level_list"),
        pytest.param("eos_config", ["ethernet_interfaces", "0"], "dict", None, id="eos_config_list_item"),
        pytest.param("eos_config", ["access_lists", "0", "sequence_numbers"], "list", "sequence", id="eos_config_nested_list"),
        pytest.param("eos_config", ["access_lists", "sequence_numbers"], None, None, id="eos_config_nested_list_without_index"),
        pytest.param("eos_config", ["hostname"], "str", None, id="eos_config_scalar_path"),
        pytest.param("eos_config", ["config_end"], "bool", None, id="eos_config_bool_path"),
        pytest.param("eos_config", ["ip_access_lists_max_entries"], "int", None, id="eos_config_int_path"),
        pytest.param("eos_config", ["custom_templates"], "list", None, id="eos_config_list_without_primary_key"),
        pytest.param("eos_config", ["not_a_schema_key"], None, None, id="eos_config_unknown_path"),
        pytest.param("avd_design", ["node_type_keys"], "list", "key", id="avd_design_node_type_keys"),
        pytest.param("avd_design", ["connected_endpoints_keys"], "list", "key", id="avd_design_connected_endpoints_keys"),
        pytest.param("avd_design", ["network_services_keys"], "list", "name", id="avd_design_network_services_keys"),
    ],
)
def test_schema_store_get_schema_info(
    schema_name: Literal["eos_config", "avd_design"], data_path: list[str], expected_schema_type: str | None, expected_primary_key: str | None
) -> None:
    info = get_schema_info(schema_name, data_path)
    if expected_schema_type is None:
        assert info is None
    else:
        assert isinstance(info, SchemaInfo)
        assert info.schema_type == expected_schema_type
        assert info.primary_key == expected_primary_key


@pytest.mark.usefixtures("init_store")
@pytest.mark.parametrize("schema_name", ["eos_cli_config_gen", "eos_designs", "cv_deploy"])
def test_schema_store_get_schema_info_unsupported_schema_name_errors(schema_name: str) -> None:
    with pytest.raises(RuntimeError, match="not supported"):
        # Intentionally violate the typed API contract to test runtime validation.
        get_schema_info(schema_name, [])  # pyright: ignore[reportArgumentType]


@pytest.mark.usefixtures("init_store")
def test_schema_store_get_schema_info_invalid_traversal_errors() -> None:
    with pytest.raises(RuntimeError, match="Data path cannot be traversed through this schema node"):
        get_schema_info("eos_config", ["hostname", "INVALID"])


@pytest.mark.parametrize(
    ("field", "error_path"),
    [
        pytest.param({"type": "str", "pattern": "(["}, "pattern", id="invalid_regex"),
        pytest.param({"type": "str", "pattern": r"\p{Greek}"}, "pattern", id="unsupported_regex_dialect"),
        pytest.param({"type": "str", "display_name": "two\nlines"}, "display_name", id="multiline_display_name"),
        pytest.param({"type": "str", "description": ""}, "description", id="empty_description"),
        pytest.param({"type": "list", "primary_key": "BadKey"}, "primary_key", id="invalid_primary_key"),
        pytest.param({"type": "list", "unique_keys": ["parent.Bad"]}, "unique_keys/0", id="invalid_unique_key"),
    ],
)
def test_archive_build_rejects_invalid_authoring_without_replacing_output(tmp_path: Path, field: dict[str, object], error_path: str) -> None:
    source = tmp_path / "schemas.json"
    destination = tmp_path / "schemas.rkyv"
    destination.write_bytes(b"existing archive")
    source.write_text(json.dumps({"test": {"type": "dict", "keys": {"value": field}}}), encoding="UTF-8")

    with pytest.raises(RuntimeError, match=f"test/keys/value/{error_path}"):
        compile_schema_archive(source, destination)

    assert destination.read_bytes() == b"existing archive"


def test_archive_build_checks_unused_definitions(tmp_path: Path) -> None:
    source = tmp_path / "schemas.json"
    destination = tmp_path / "schemas.rkyv"
    source.write_text(json.dumps({"test": {"type": "dict", "$defs": {"unused": {"type": "str", "pattern": "(["}}}}), encoding="UTF-8")

    with pytest.raises(RuntimeError, match=r"test/\$defs/unused/pattern"):
        compile_schema_archive(source, destination)

    assert not destination.exists()


def test_archive_build_accepts_supported_authoring(tmp_path: Path) -> None:
    source = tmp_path / "schemas.json"
    destination = tmp_path / "schemas.rkyv"
    schema = {
        "type": "dict",
        "keys": {
            "Vxlan1": {"type": "str", "pattern": r"(?<=a+)b|\w+"},
            "MIB_family_name": {"type": "str"},
            "entries": {"type": "list", "primary_key": "name", "unique_keys": ["parent.name"]},
        },
        "dynamic_keys": {"entries.name": {"type": "str"}},
    }
    source.write_text(json.dumps({"test": schema}), encoding="UTF-8")

    compile_schema_archive(source, destination)

    assert destination.is_file()
