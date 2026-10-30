# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
from __future__ import annotations

import json
import re
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
    assert schema["$schema"] == "https://json-schema.org/draft/2020-12/schema"
    assert schema["$id"] == "http://avd.sh/development/schema-schema.json"
    assert schema["title"] == "Arista AVD Schema"

    root_schema_types = {variant["properties"]["type"]["const"] for variant in schema["oneOf"]}
    assert root_schema_types == {"dict"}

    recursive_schema = schema["$defs"]["SourceSchema"]
    recursive_schema_types = {variant["properties"]["type"]["const"] for variant in recursive_schema["oneOf"]}
    assert recursive_schema_types == {"bool", "dict", "int", "list", "str"}
    root_dict_properties = _variant(schema, "dict")["properties"]
    assert list(root_dict_properties["keys"]["patternProperties"]) == [r"^(?:[a-z][a-z0-9_]*|Vxlan1)$"]
    assert list(root_dict_properties["dynamic_keys"]["patternProperties"]) == [r"^[a-z][a-z0-9_.]*$"]
    assert list(root_dict_properties["$defs"]["patternProperties"]) == [r"^[a-z][a-z0-9_]*$"]
    assert root_dict_properties["description"]["type"] == "string"
    assert root_dict_properties["required"]["type"] == "boolean"
    assert "description" not in _variant(schema, "dict")["required"]
    assert "required" not in _variant(schema, "dict")["required"]

    recursive_dict_properties = _variant(recursive_schema, "dict")["properties"]
    assert {"dynamic_keys", "$defs", "$id", "$schema"}.isdisjoint(recursive_dict_properties)

    deprecation = schema["$defs"]["Deprecation"]
    assert deprecation["properties"]["upgrade_handler"]["type"] == "string"
    assert "upgrade_handler" not in deprecation["required"]

    int_convert_type = schema["$defs"]["IntConvertType"]
    assert int_convert_type["enum"] == ["bool", "str", "float"]
    assert schema["$defs"]["StrConvertType"]["enum"] == ["bool", "int", "float"]
    for schema_type in ("int", "str"):
        variant = _variant(recursive_schema, schema_type)
        assert variant["required"] == ["type"]
        assert variant["properties"]["convert_types"]["type"] == "array"
        assert variant["properties"]["convert_types"]["deprecated"] is True
        assert variant["properties"]["convert_types"]["description"] == "Deprecated. Do not use `convert_types` in new schema definitions."
        assert "deprecated" not in variant
    assert "deprecated" not in _variant(recursive_schema, "str")["properties"]["convert_to_lower_case"]
    assert schema["$defs"]["Pattern"]["format"] == "regex"
    for definition in ("DocumentationOptions", "DocumentationOptionsDict"):
        assert schema["$defs"][definition]["properties"]["table"]["pattern"] == "^[a-z0-9_-]+$"


def test_generate_metaschema_errors_for_missing_parent(tmp_path: Path) -> None:
    with pytest.raises(RuntimeError, match="Error while writing the AVD metaschema"):
        generate_metaschema(tmp_path / "missing" / "avd_meta_schema.json")


def test_metaschema_descriptions_are_for_schema_authors(tmp_path: Path) -> None:
    destination = tmp_path / "avd_meta_schema.json"
    generate_metaschema(destination)
    schema = json.loads(destination.read_text(encoding="UTF-8"))
    assert "[`" not in json.dumps(schema)
    assert "SourceRootDict" not in schema["description"]
    root = _variant(schema, "dict")["properties"]
    key_pattern = re.compile(next(iter(root["keys"]["patternProperties"])))
    assert key_pattern.fullmatch("Vxlan1")
    assert not key_pattern.fullmatch("MIB_family_name")
    assert not key_pattern.fullmatch("Invalid_key")
    assert "input data" in root["dynamic_keys"]["description"]
    assert "list item" in root["dynamic_keys"]["description"]
    assert "eos_cli_config_gen#/keys/hostname" in root["$ref"]["description"]
    string = _variant(schema["$defs"]["SourceSchema"], "str")["properties"]
    assert "variable-length lookbehinds" in string["pattern"]["description"]
    assert "word boundaries" in string["pattern"]["description"]
    assert "list item" in string["dynamic_valid_values"]["description"]
