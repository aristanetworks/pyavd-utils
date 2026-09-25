# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
import subprocess
import sys
from json import dumps
from pathlib import Path

import pytest

from pyavd_utils.schema_generation import generate_python_schema_models, generate_python_schema_models_from_paths
from pyavd_utils.schema_store import compile_schema_archive

ARTIFACTS = Path(__file__).parent / "artifacts"


def test_regenerate_python_model_fixture() -> None:
    """Regenerate committed artifacts in place so a mismatch remains visible in git diff."""
    source = ARTIFACTS / "schemas.json"
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
        ARTIFACTS / "schemas.json",
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
            ARTIFACTS / "schemas.json",
            "schema_generation_fixture",
            tmp_path / "projection.py",
            root_keys=["interface_profiles"],
        )


def test_projection_rejects_unknown_root_key(tmp_path: Path) -> None:
    with pytest.raises(RuntimeError, match="Root key 'missing' was not found"):
        generate_python_schema_models(
            ARTIFACTS / "schemas.json",
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
        '{"model": {"type": "dict", "dynamic_keys": {"selectors.names": {"type": "str"}}}}',
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
        dumps({"type": "dict", "dynamic_keys": {"selectors.names": {"type": "str"}}}),
        encoding="UTF-8",
    )

    with pytest.raises(RuntimeError, match=r"requires 'display_name'.*eos_designs/dynamic_keys/selectors.names"):
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
