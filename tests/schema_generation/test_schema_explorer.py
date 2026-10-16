# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Observable JSONL contracts for field occurrences and schema semantics."""

import json
from pathlib import Path

import pytest

from pyavd_utils_gen.schema_generation import generate_schema_explorer_jsonl, generate_schema_explorer_jsonl_from_paths

ARTIFACTS = Path(__file__).parent / "artifacts"
SOURCE = ARTIFACTS / "schema_explorer_fixture.json"


@pytest.fixture
def rows(tmp_path: Path) -> dict[str, dict]:
    """Index generated fixture rows without discarding their insertion order."""
    output = tmp_path / "schema.jsonl"
    counts = generate_schema_explorer_jsonl(SOURCE, ["model"], output)
    content = output.read_bytes()
    assert content.endswith(b"\n")
    result = [json.loads(line) for line in content.splitlines()]
    assert counts == {"model": len(result)}
    return {row["key_path"]: row for row in result}


def test_occurrence_order_and_anonymous_items(rows: dict[str, dict]) -> None:
    assert list(rows) == [
        "visible",
        "visible.count",
        "visible.name",
        "hidden",
        "hidden_same",
        "plain_hidden",
        "entries",
        "entries[].name",
        "entries[].enabled",
        "duplicates",
        "duplicates[].name",
        "hidden_items",
        "deprecated",
        "removed",
        "removed.child",
        "defaults",
        "<selectors.key>",
        "<selectors.key>.value",
    ]
    assert rows["entries[].name"]["parent_path"] == "entries"
    assert rows["entries[].name"]["depth"] == 2
    assert rows["<selectors.key>"]["depth"] == 1
    assert rows["<selectors.key>.value"]["depth"] == 2
    assert all(row["module"] == "model" for row in rows.values())


def test_effective_reference_properties_and_hidden_links(rows: dict[str, dict]) -> None:
    assert rows["visible.count"]["constraints"] == {"min": 0, "max": 5}
    assert rows["visible.count"]["default_value"] == 0
    assert rows["visible.name"]["required"] is True
    assert rows["visible.name"]["constraints"] == {"convert_types": ["int"]}
    assert rows["hidden"]["description"] == "inherited description"
    assert rows["hidden"]["doc_table"] == "shared-table"
    assert rows["hidden"]["cross_ref"] == "shared#/keys/base"
    assert rows["visible"]["cross_ref"] is None
    assert rows["hidden_same"]["cross_ref"] is None
    assert "hidden.name" not in rows
    assert "hidden_same.secret" not in rows
    assert "plain_hidden.secret" not in rows
    assert "hidden_items[].secret" not in rows


def test_list_context_and_item_table(rows: dict[str, dict]) -> None:
    primary = rows["entries[].name"]
    assert primary["required"] is True
    assert primary["primary_key"] is True
    assert primary["unique_key"] is True
    assert primary["doc_table"] == "item-table"
    assert rows["entries"]["doc_table"] == "entries"
    assert rows["entries[].enabled"]["primary_key"] is False
    duplicates = rows["duplicates[].name"]
    assert duplicates["primary_key"] is True
    assert duplicates["unique_key"] is False


def test_deprecation_does_not_change_constraint_extraction(rows: dict[str, dict]) -> None:
    deprecated = rows["deprecated"]
    assert deprecated["var_type"] == "list(deprecated)"
    assert deprecated["deprecated"] is True
    assert deprecated["removed"] is False
    assert deprecated["constraints"] == {
        "min_length": 1,
        "items": {"type": "str", "valid_values": ["one", "two"], "min_length": 1},
    }
    assert rows["removed"]["removed"] is True
    assert rows["removed"]["deprecated"] is False
    assert "removed.child" in rows


def test_defaults_are_json_values_not_strings(rows: dict[str, dict]) -> None:
    assert rows["entries"]["default_value"] == []
    assert rows["entries[].enabled"]["default_value"] is False
    assert rows["visible.count"]["default_value"] == 0
    assert rows["visible.name"]["default_value"] is None
    assert rows["defaults"]["default_value"] == {"answer": 0, "enabled": False, "names": ["é"], "nested": {"nullable": None}}


def test_named_paths_and_store_have_identical_jsonl(tmp_path: Path) -> None:
    schemas = json.loads(SOURCE.read_text(encoding="UTF-8"))
    sources = {}
    for name, schema in schemas.items():
        path = tmp_path / f"{name}.json"
        path.write_text(json.dumps(schema), encoding="UTF-8")
        sources[name] = path
    from_store = tmp_path / "store.jsonl"
    from_paths = tmp_path / "paths.jsonl"
    names = ["model", "empty", "shared"]
    assert generate_schema_explorer_jsonl(SOURCE, names, from_store) == generate_schema_explorer_jsonl_from_paths(sources, names, from_paths)
    assert from_store.read_bytes() == from_paths.read_bytes()
    counts = generate_schema_explorer_jsonl(SOURCE, names, from_store)
    assert counts == {"model": 18, "empty": 0, "shared": 3}
    modules = [json.loads(line)["module"] for line in from_store.read_text(encoding="UTF-8").splitlines()]
    assert modules == ["model"] * 18 + ["shared"] * 3


@pytest.mark.parametrize(
    ("names", "error", "message"),
    [
        (["model", "model"], ValueError, "must be unique"),
        (["model", "absent"], RuntimeError, "absent"),
    ],
)
def test_invalid_selection_preserves_existing_output(tmp_path: Path, names: list[str], error: type[Exception], message: str) -> None:
    destination = tmp_path / "schema.jsonl"
    destination.write_bytes(b"existing output")
    with pytest.raises(error, match=message):
        generate_schema_explorer_jsonl(SOURCE, names, destination)
    assert destination.read_bytes() == b"existing output"
    assert set(tmp_path.iterdir()) == {destination}


def test_invalid_reference_preserves_existing_output(tmp_path: Path) -> None:
    source = tmp_path / "invalid.json"
    source.write_text(json.dumps({"model": {"type": "dict", "keys": {"broken": {"type": "dict", "$ref": "absent#"}}}}), encoding="UTF-8")
    destination = tmp_path / "schema.jsonl"
    destination.write_bytes(b"existing output")
    with pytest.raises(RuntimeError, match="absent"):
        generate_schema_explorer_jsonl(source, ["model"], destination)
    assert destination.read_bytes() == b"existing output"


def test_output_parent_is_caller_owned(tmp_path: Path) -> None:
    destination = tmp_path / "absent" / "schema.jsonl"
    with pytest.raises(RuntimeError, match="writing Schema Explorer JSONL"):
        generate_schema_explorer_jsonl(SOURCE, ["model"], destination)
    assert not destination.parent.exists()


def test_hidden_descendants_still_receive_complete_schema_analysis(tmp_path: Path) -> None:
    source = tmp_path / "invalid.json"
    source.write_text(
        json.dumps(
            {
                "model": {
                    "type": "dict",
                    "keys": {
                        "hidden": {"type": "dict", "documentation_options": {"hide_keys": True}, "keys": {"broken": {"type": "dict", "$ref": "absent#"}}},
                    },
                }
            }
        ),
        encoding="UTF-8",
    )
    destination = tmp_path / "schema.jsonl"
    with pytest.raises(RuntimeError, match="absent"):
        generate_schema_explorer_jsonl(source, ["model"], destination)
    assert not destination.exists()


def test_reference_chain_and_explicit_unhide(tmp_path: Path) -> None:
    source = tmp_path / "schemas.json"
    source.write_text(
        json.dumps(
            {
                "shared": {"type": "dict", "keys": {"value": {"type": "str"}}},
                "model": {
                    "type": "dict",
                    "$defs": {
                        "wrapper": {"type": "dict", "$ref": "shared#", "documentation_options": {"hide_keys": True}},
                    },
                    "keys": {
                        "hidden": {"type": "dict", "$ref": "model#/$defs/wrapper"},
                        "visible": {"type": "dict", "$ref": "model#/$defs/wrapper", "documentation_options": {"hide_keys": False}},
                    },
                },
            }
        ),
        encoding="UTF-8",
    )
    destination = tmp_path / "schema.jsonl"
    generate_schema_explorer_jsonl(source, ["model"], destination)
    rows = [json.loads(line) for line in destination.read_text(encoding="UTF-8").splitlines()]
    assert [row["key_path"] for row in rows] == ["hidden", "visible", "visible.value"]
    assert rows[0]["cross_ref"] == "shared#"
    assert rows[1]["cross_ref"] is None
