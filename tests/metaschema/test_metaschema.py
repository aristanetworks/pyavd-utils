# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
from __future__ import annotations

import json
from typing import TYPE_CHECKING, Any

import pytest

from pyavd_utils_gen.metaschema import generate_metaschema

if TYPE_CHECKING:
    from pathlib import Path


def _variant(schema: dict[str, Any], schema_type: str) -> dict[str, Any]:
    return next(variant for variant in schema["oneOf"] if variant["properties"]["type"]["const"] == schema_type)


def _contains_null_type(value: Any) -> bool:
    if isinstance(value, dict):
        schema_type = value.get("type")
        if schema_type == "null" or (isinstance(schema_type, list) and "null" in schema_type):
            return True
        return any(_contains_null_type(child) for child in value.values())
    if isinstance(value, list):
        return any(_contains_null_type(child) for child in value)
    return False


def test_generate_metaschema(tmp_path: Path) -> None:
    destination = tmp_path / "avd_meta_schema.json"

    generate_metaschema(destination)

    schema = json.loads(destination.read_text(encoding="UTF-8"))
    assert not _contains_null_type(schema)
    assert schema["$schema"] == "http://json-schema.org/draft-07/schema#"
    assert schema["$id"] == "http://avd.sh/development/schema-schema.json"
    assert schema["title"] == "Arista AVD Schema"

    root_schema_types = {variant["properties"]["type"]["const"] for variant in schema["oneOf"]}
    assert root_schema_types == {"dict"}

    recursive_schema = schema["definitions"]["SourceSchema"]
    recursive_schema_types = {variant["properties"]["type"]["const"] for variant in recursive_schema["oneOf"]}
    assert recursive_schema_types == {"bool", "dict", "int", "list", "str"}
    root_dict_properties = _variant(schema, "dict")["properties"]
    assert list(root_dict_properties["keys"]["patternProperties"]) == [r"^(?:[a-z][a-z0-9_]*|MIB_family_name|Vxlan1)$"]
    assert list(root_dict_properties["dynamic_keys"]["patternProperties"]) == [r"^[a-z][a-z0-9_.]*$"]
    assert list(root_dict_properties["$defs"]["patternProperties"]) == [r"^[a-z][a-z0-9_]*$"]
    assert root_dict_properties["description"]["type"] == "string"
    assert root_dict_properties["required"]["type"] == "boolean"
    assert "description" not in _variant(schema, "dict")["required"]
    assert "required" not in _variant(schema, "dict")["required"]

    recursive_dict_properties = _variant(recursive_schema, "dict")["properties"]
    assert {"dynamic_keys", "$defs", "$id", "$schema"}.isdisjoint(recursive_dict_properties)

    deprecation = schema["definitions"]["Deprecation"]
    assert deprecation["properties"]["upgrade_handler"]["type"] == "string"
    assert "upgrade_handler" not in deprecation["required"]

    int_convert_type = schema["definitions"]["IntConvertType"]
    assert int_convert_type["enum"] == ["bool", "str", "float"]
    assert schema["definitions"]["StrConvertType"]["enum"] == ["bool", "int", "float"]
    assert schema["definitions"]["Pattern"]["format"] == "regex"


def test_generate_metaschema_errors_for_missing_parent(tmp_path: Path) -> None:
    with pytest.raises(RuntimeError, match="Error while writing the AVD metaschema"):
        generate_metaschema(tmp_path / "missing" / "avd_meta_schema.json")
