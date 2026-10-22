# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
import ast
import subprocess
import sys
from json import dumps, loads
from pathlib import Path

import pytest

from pyavd_utils_gen.schema_generation import (
    generate_python_schema_models,
    generate_python_schema_models_from_paths,
    generate_schema_documentation,
    generate_schema_documentation_from_paths,
)
from pyavd_utils_gen.schema_store import compile_schema_archive

ARTIFACTS = Path(__file__).parent / "artifacts"
SCHEMAS = Path(__file__).parents[1] / "schemas" / "schemas.json"


def test_regenerate_schema_documentation_fixture() -> None:
    """Regenerate committed documentation so a mismatch remains visible in git diff."""
    destination = ARTIFACTS / "schema_documentation_fixture.expected"
    expected = {path.name: path.read_bytes() for path in destination.glob("*.md")}

    generate_schema_documentation(SCHEMAS, "schema_documentation_fixture", destination)

    assert {path.name: path.read_bytes() for path in destination.glob("*.md")} == expected


def test_schema_documentation_removes_obsolete_markdown(tmp_path: Path) -> None:
    obsolete = tmp_path / "obsolete.md"
    preserved = tmp_path / "preserved.txt"
    obsolete.touch()
    preserved.touch()

    generate_schema_documentation(SCHEMAS, "schema_documentation_fixture", tmp_path)

    assert not obsolete.exists()
    assert preserved.exists()


@pytest.mark.parametrize("table", ["", "../escape", "nested/escape", r"..\escape", "/absolute", "C:escape", "UPPER", "name\n", "name\x00"])
@pytest.mark.parametrize("existing_directory", [False, True])
def test_schema_documentation_rejects_invalid_table_before_filesystem_changes(tmp_path: Path, table: str, existing_directory: bool) -> None:
    """Reject the whole output batch before creating, cleaning, or writing its directory."""
    source = tmp_path / "schemas.json"
    source.write_text(
        dumps(
            {
                "model": {
                    "type": "dict",
                    "keys": {
                        "valid": {"type": "str", "documentation_options": {"table": "valid-table"}},
                        "invalid": {"type": "str", "documentation_options": {"table": table}},
                    },
                }
            }
        ),
        encoding="UTF-8",
    )
    destination = tmp_path / "output"
    outside = tmp_path / "escape.md"
    outside.write_bytes(b"outside output directory")
    if existing_directory:
        destination.mkdir()
        (destination / "obsolete.md").write_bytes(b"keep obsolete until validation succeeds")
        (destination / "valid-table.md").write_bytes(b"keep existing output")
    before = {path.relative_to(tmp_path): path.read_bytes() for path in tmp_path.rglob("*") if path.is_file()}

    with pytest.raises(ValueError, match="Invalid documentation table name"):
        generate_schema_documentation(source, "model", destination)

    assert destination.exists() == existing_directory
    assert {path.relative_to(tmp_path): path.read_bytes() for path in tmp_path.rglob("*") if path.is_file()} == before


def test_schema_documentation_accepts_compatible_filename_characters(tmp_path: Path) -> None:
    """Accept release table names and dotted names derived from dynamic-key paths."""
    source = tmp_path / "schemas.json"
    tables = ["dot1x-settings", "network-services-l2vlans-settings", "ptp_settings"]
    source.write_text(
        dumps(
            {
                "model": {
                    "type": "dict",
                    "keys": {f"key_{index}": {"type": "str", "documentation_options": {"table": table}} for index, table in enumerate(tables)},
                    "dynamic_keys": {"custom_node_type_keys.key": {"type": "dict", "documentation_options": {"hide_keys": True}}},
                }
            }
        ),
        encoding="UTF-8",
    )
    destination = tmp_path / "output"

    generate_schema_documentation(source, "model", destination)

    assert {path.stem for path in destination.glob("*.md")} == {*tables, "custom-node-type-keys.key"}


@pytest.mark.parametrize("hide_keys", [False, True])
@pytest.mark.parametrize("child_table", [None, "child-table"])
def test_schema_documentation_preserves_dictionary_item_tables(tmp_path: Path, hide_keys: bool, child_table: str | None) -> None:
    """Dictionary items own table context even though only their keys are rendered."""
    source = tmp_path / "schemas.json"
    source.write_text(
        dumps(
            {
                "model": {
                    "type": "dict",
                    "keys": {
                        "items": {
                            "type": "list",
                            "documentation_options": {"table": "list-table"},
                            "items": {
                                "type": "dict",
                                "documentation_options": {"table": "item-table", "hide_keys": hide_keys},
                                "keys": {
                                    "value": {
                                        "type": "str",
                                        **({"documentation_options": {"table": child_table}} if child_table else {}),
                                    }
                                },
                            },
                        }
                    },
                }
            }
        ),
        encoding="UTF-8",
    )
    destination = tmp_path / "output"

    generate_schema_documentation(source, "model", destination)

    expected_tables = {"list-table", "item-table"}
    if child_table and not hide_keys:
        expected_tables.add(child_table)
    assert {path.stem for path in destination.glob("*.md")} == expected_tables
    for table in expected_tables:
        output = (destination / f"{table}.md").read_text(encoding="UTF-8")
        assert '(## "items")' in output
        assert '(## "items.[]")' not in output
        renders_value = table == (child_table or "item-table")
        assert ('(## "items.[].value")' in output) == renders_value
        assert ("- value: <str>" in output) == renders_value

    if child_table is None:
        # Captured from AVD's Python generator, including its item hide_keys behavior.
        expected = loads((ARTIFACTS / "list_item_documentation.expected.json").read_text(encoding="UTF-8"))
        assert {path.name: path.read_bytes() for path in destination.glob("*.md")} == {name: contents.encode("UTF-8") for name, contents in expected.items()}


@pytest.mark.parametrize("item_type", ["str", "dict"])
def test_schema_documentation_preserves_nested_list_yaml(tmp_path: Path, item_type: str) -> None:
    """Keep the Python renderer's missing-key spelling for lists used as list items."""
    source = tmp_path / "schemas.json"
    item: dict[str, object] = {"type": item_type}
    if item_type == "dict":
        item["keys"] = {"value": {"type": "str"}}
    source.write_text(
        dumps(
            {
                "model": {
                    "type": "dict",
                    "keys": {
                        "outer": {
                            "type": "list",
                            "documentation_options": {"table": "outer-table"},
                            "items": {
                                "type": "list",
                                "documentation_options": {"table": "inner-table"},
                                "items": item,
                            },
                        }
                    },
                }
            }
        ),
        encoding="UTF-8",
    )
    destination = tmp_path / "output"

    generate_schema_documentation(source, "model", destination)

    output = (destination / "inner-table.md").read_text(encoding="UTF-8")
    assert "\n      - None:\n" in output
    assert "\n      - :\n" not in output


def test_schema_documentation_inherits_individual_options_across_references(tmp_path: Path) -> None:
    shared = tmp_path / "shared.json"
    shared.write_text(
        dumps(
            {
                "type": "dict",
                "documentation_options": {"table": "shared", "hide_keys": True},
                "keys": {"hidden": {"type": "str"}},
            }
        ),
        encoding="UTF-8",
    )
    model = tmp_path / "model.json"
    model.write_text(
        dumps(
            {
                "type": "dict",
                "keys": {
                    "visible": {
                        "type": "dict",
                        "$ref": "shared#",
                        "documentation_options": {"table": "visible"},
                    }
                },
            }
        ),
        encoding="UTF-8",
    )

    generate_schema_documentation_from_paths({"shared": shared, "model": model}, "model", tmp_path / "output")

    output = (tmp_path / "output/visible.md").read_text(encoding="UTF-8")
    assert "visible: <dict>" in output
    assert "hidden" not in output


def test_regenerate_python_model_fixture() -> None:
    """Regenerate committed artifacts in place so a mismatch remains visible in git diff."""
    source = SCHEMAS
    archive = ARTIFACTS / "schemas.rkyv"
    generated = ARTIFACTS / "schema_generation_fixture.py.expected"
    expected = generated.read_bytes()

    compile_schema_archive(source, archive)
    generate_python_schema_models(source, "schema_generation_fixture", generated)
    subprocess.run([sys.executable, "-m", "ruff", "check", "--fix", generated], check=True)  # noqa: S603
    subprocess.run([sys.executable, "-m", "ruff", "format", generated], check=True)  # noqa: S603

    assert generated.read_bytes() == expected


def test_generate_root_key_projection(tmp_path: Path) -> None:
    generated = tmp_path / "projection.py"
    generate_python_schema_models(
        SCHEMAS,
        "schema_generation_fixture",
        generated,
        "ProjectedSchema",
        ["interface_profiles"],
    )

    output = generated.read_text(encoding="UTF-8")
    assert "class ProjectedSchema(AvdModel):" in output
    assert "class InterfaceProfilesItem(AvdModel):" in output
    assert "class Accounting(AvdModel):" not in output


def test_projection_requires_explicit_generated_class_name(tmp_path: Path) -> None:
    with pytest.raises(ValueError, match="generated_class_name is required"):
        generate_python_schema_models(
            SCHEMAS,
            "schema_generation_fixture",
            tmp_path / "projection.py",
            root_keys=["interface_profiles"],
        )


def test_projection_rejects_unknown_root_key(tmp_path: Path) -> None:
    with pytest.raises(RuntimeError, match="Root key 'missing' was not found"):
        generate_python_schema_models(
            SCHEMAS,
            "schema_generation_fixture",
            tmp_path / "projection.py",
            "ProjectedSchema",
            ["missing"],
        )


def test_generation_rejects_scalar_root(tmp_path: Path) -> None:
    source = tmp_path / "schemas.json"
    source.write_text('{"scalar": {"type": "str"}}', encoding="UTF-8")

    with pytest.raises(RuntimeError, match="requires a dictionary root"):
        generate_python_schema_models(source, "scalar", tmp_path / "scalar.py")


def test_generation_rejects_unsupported_schema_feature(tmp_path: Path) -> None:
    source = tmp_path / "schemas.json"
    source.write_text(
        '{"model": {"type": "dict", "dynamic_keys": {"node_type_keys.key": {"type": "str"}}}}',
        encoding="UTF-8",
    )

    with pytest.raises(RuntimeError, match="does not yet support dynamic keys"):
        generate_python_schema_models(source, "model", tmp_path / "model.py")


def test_generate_from_individual_schema_paths_preserves_transitive_model_reference(tmp_path: Path) -> None:
    eos_cli_schema = tmp_path / "eos_cli_config_gen.json"
    eos_cli_schema.write_text(
        dumps({"type": "dict", "keys": {"target": {"type": "dict", "keys": {"value": {"type": "str"}}}}}),
        encoding="UTF-8",
    )
    protocol_schema = tmp_path / "eos_designs_facts_protocol.json"
    protocol_schema.write_text(
        dumps(
            {
                "type": "dict",
                "keys": {"linked": {"type": "dict", "$ref": "eos_designs_facts_protocol#/$defs/target"}},
                "$defs": {"target": {"type": "dict", "$ref": "eos_cli_config_gen#/keys/target"}},
            }
        ),
        encoding="UTF-8",
    )
    sources = {"eos_cli_config_gen": eos_cli_schema, "eos_designs_facts_protocol": protocol_schema}
    generated = tmp_path / "protocol.py"

    generate_python_schema_models_from_paths(
        sources,
        "eos_designs_facts_protocol",
        generated,
    )

    output = generated.read_text(encoding="UTF-8")
    assert "class EosDesignsFactsProtocol(Protocol):" in output
    assert '"linked": {"type": EosCliConfigGen.Target}' in output
    assert "class Linked(AvdModel):" not in output

    generate_python_schema_models_from_paths(sources, "eos_cli_config_gen", generated)
    assert "class EosCliConfigGen(EosCliConfigGenRootModel):" in generated.read_text(encoding="UTF-8")


@pytest.mark.parametrize("literal_key", ["keys", "items"])
def test_model_reference_preserves_literal_structural_key_names(tmp_path: Path, literal_key: str) -> None:
    """A field's name must not be mistaken for a structural reference component."""
    source = tmp_path / "schemas.json"
    source.write_text(
        dumps(
            {
                "shared": {"type": "dict", "keys": {literal_key: {"type": "dict", "keys": {"value": {"type": "str"}}}}},
                "model": {"type": "dict", "keys": {"linked": {"type": "dict", "$ref": f"shared#/keys/{literal_key}"}}},
            }
        ),
        encoding="UTF-8",
    )
    generated = tmp_path / "model.py"

    generate_python_schema_models(source, "model", generated)

    output = generated.read_text(encoding="UTF-8")
    assert f'"linked": {{"type": Shared.{literal_key.title()}}}' in output
    assert "class Linked(AvdModel):" not in output
    ast.parse(output)


def test_generation_supports_defaults_aliases_and_duplicate_primary_keys(tmp_path: Path) -> None:
    source = tmp_path / "schemas.json"
    source.write_text(
        dumps(
            {
                "model": {
                    "type": "dict",
                    "keys": {
                        "class": {"type": "str"},
                        "settings": {
                            "type": "dict",
                            "default": {"enabled": True},
                            "keys": {"enabled": {"type": "bool"}},
                        },
                        "entries": {
                            "type": "list",
                            "primary_key": "name",
                            "allow_duplicate_primary_key": True,
                            "default": [],
                            "items": {"type": "dict", "keys": {"name": {"type": "str"}}},
                        },
                    },
                }
            }
        ),
        encoding="UTF-8",
    )
    generated = tmp_path / "model.py"

    generate_python_schema_models(source, "model", generated, "Generated")

    output = generated.read_text(encoding="UTF-8")
    assert "_field_to_key_map: ClassVar[dict] = {'field_class': 'class'}" in output
    assert '"settings": {"type": Settings, "default": lambda cls: coerce_type({"enabled": True}, target_type=cls)}' in output
    assert "class Entries(AvdList[EntriesItem]):" in output
    assert '"entries": {"type": Entries, "default": lambda cls: coerce_type([], target_type=cls)}' in output


def test_eos_designs_generation_adds_dynamic_and_custom_structured_configuration_models(tmp_path: Path) -> None:
    source = tmp_path / "eos_designs.json"
    source.write_text(
        dumps(
            {
                "type": "dict",
                "keys": {},
                "dynamic_keys": {
                    "connected_endpoints_keys.key": {
                        "type": "list",
                        "display_name": "Connected Endpoints",
                        "items": {"type": "dict", "keys": {"name": {"type": "str"}}},
                    }
                },
            }
        ),
        encoding="UTF-8",
    )
    generated = tmp_path / "eos_designs.py"

    generate_python_schema_models_from_paths({"eos_designs": source}, "eos_designs", generated)

    output = generated.read_text(encoding="UTF-8")
    assert "class EosDesigns(EosDesignsRootModel):" in output
    assert "class _CustomStructuredConfigurations(AvdIndexedList[str, _CustomStructuredConfigurationsItem]):" in output
    assert "class DynamicConnectedEndpoints(AvdIndexedList[str, DynamicConnectedEndpointsItem]):" in output
    assert "_dynamic_key_maps: ClassVar[tuple[dict, ...]]" in output
    assert "'dynamic_keys_path': 'connected_endpoints_keys.key'" in output


def test_eos_designs_dynamic_model_requires_display_name(tmp_path: Path) -> None:
    source = tmp_path / "eos_designs.json"
    source.write_text(
        dumps({"type": "dict", "dynamic_keys": {"node_type_keys.key": {"type": "str"}}}),
        encoding="UTF-8",
    )

    with pytest.raises(RuntimeError, match=r"requires 'display_name'.*eos_designs/dynamic_keys/node_type_keys.key"):
        generate_python_schema_models_from_paths({"eos_designs": source}, "eos_designs", tmp_path / "eos_designs.py")


def test_generation_ignores_unsupported_removed_model(tmp_path: Path) -> None:
    source = tmp_path / "schemas.json"
    source.write_text(
        dumps(
            {
                "model": {
                    "type": "dict",
                    "keys": {
                        "removed": {
                            "type": "list",
                            "deprecation": {"warning": True, "removed": True},
                        }
                    },
                }
            }
        ),
        encoding="UTF-8",
    )
    generated = tmp_path / "model.py"

    generate_python_schema_models(source, "model", generated, "Generated")

    assert '"removed"' not in generated.read_text(encoding="UTF-8")


def test_generation_keeps_class_var_import_for_model_before_empty_nested_model(tmp_path: Path) -> None:
    source = tmp_path / "schemas.json"
    source.write_text(
        dumps(
            {
                "model": {
                    "type": "dict",
                    "keys": {
                        "value": {"type": "str"},
                        "empty_model": {
                            "type": "dict",
                            "keys": {
                                "removed": {
                                    "type": "str",
                                    "deprecation": {"warning": True, "removed": True},
                                }
                            },
                        },
                    },
                }
            }
        ),
        encoding="UTF-8",
    )
    generated = tmp_path / "model.py"

    generate_python_schema_models(source, "model", generated, "Generated")

    output = generated.read_text(encoding="UTF-8")
    assert "from typing import ClassVar" in output
    ast.parse(output)


def test_generation_preserves_strings_in_python_literals(tmp_path: Path) -> None:
    value = 'quote " backslash \\ newline\ncarriage\r tab\t null\0'
    object_key = 'key " \\ \n'
    source = tmp_path / "schemas.json"
    source.write_text(
        dumps(
            {
                "model": {
                    "type": "dict",
                    "keys": {
                        "value": {"type": "str", "default": value, "valid_values": [value]},
                        "mapping": {"type": "dict", "default": {object_key: value}, "keys": {}},
                    },
                }
            }
        ),
        encoding="UTF-8",
    )
    generated = tmp_path / "model.py"

    generate_python_schema_models(source, "model", generated, "Generated")

    tree = ast.parse(generated.read_text(encoding="UTF-8"))
    string_constants = [node.value for node in ast.walk(tree) if isinstance(node, ast.Constant) and isinstance(node.value, str)]
    assert string_constants.count(value) >= 3
    assert object_key in string_constants
