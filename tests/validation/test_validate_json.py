# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
import pytest

from pyavd_utils.validation import validate_json


@pytest.mark.usefixtures("init_store")
def test_validate_json() -> None:
    expected_violations: list[tuple[list[str], str]] = [
        (["ethernet_interfaces", "2"], "Missing the required key 'name'."),
        (["ethernet_interfaces", "0", "name"], "The value is not unique among similar items. Conflicting item: ethernet_interfaces[1].name"),
        (["ethernet_interfaces", "1", "name"], "The value is not unique among similar items. Conflicting item: ethernet_interfaces[0].name"),
    ]
    validation_result = validate_json('{"ethernet_interfaces": [{"name": "Ethernet1", "description": 12345}, {"name": "Ethernet1"}, {}]}', "eos_cli_config_gen")

    assert len(validation_result.violations) == len(expected_violations)
    for violation in validation_result.violations:
        assert (violation.path, violation.message) in expected_violations, f"Error not expected: {violation.path}, {violation.message}"

    assert len(validation_result.deprecations) == 0
    assert len(validation_result.ignored_eos_config_keys) == 0


@pytest.mark.usefixtures("init_store")
def test_validate_json_with_ignored_eos_config_key() -> None:
    """Test that eos_cli_config_gen keys are ignored when validating eos_designs."""
    from pyavd_utils.validation import Configuration

    # router_isis is a key from eos_cli_config_gen that should be ignored when validating eos_designs
    config = Configuration(warn_eos_cli_config_gen_keys=True)
    validation_result = validate_json('{"fabric_name": "TEST_FABRIC", "router_isis": {"instance": "ISIS_TEST"}}', "eos_designs", config)

    # Should have no violations
    assert len(validation_result.violations) == 0, f"Unexpected violations: {[(v.path, v.message) for v in validation_result.violations]}"

    # Should have no deprecations
    assert len(validation_result.deprecations) == 0

    # Should have one ignored_eos_config_key
    assert len(validation_result.ignored_eos_config_keys) == 1

    # Check the ignored key details
    ignored_key = validation_result.ignored_eos_config_keys[0]
    assert ignored_key.path == ["router_isis"]
    assert ignored_key.message == "The 'eos_cli_config_gen' key is present in the input to 'eos_designs' and will be ignored."


@pytest.mark.usefixtures("init_store")
def test_validate_json_without_config_no_warning() -> None:
    """Test that without configuration, no warnings are emitted for eos_cli_config_gen keys."""
    # router_isis is a key from eos_cli_config_gen
    validation_result = validate_json('{"fabric_name": "TEST_FABRIC", "router_isis": {"instance": "ISIS_TEST"}}', "eos_designs")

    # Should have no violations
    assert len(validation_result.violations) == 0, f"Unexpected violations: {[(v.path, v.message) for v in validation_result.violations]}"

    # Should have no deprecations
    assert len(validation_result.deprecations) == 0

    # Should have NO ignored_eos_config_key warnings (because warn_eos_cli_config_gen_keys is False by default)
    assert len(validation_result.ignored_eos_config_keys) == 0


@pytest.mark.usefixtures("init_store")
def test_validate_json_with_eos_cli_config_gen_role_keys_no_warning() -> None:
    """Test that special eos_cli_config_gen role keys are silently ignored without warnings."""
    from pyavd_utils.validation import Configuration

    # These special keys should be ignored: eos_cli_config_gen_documentation, custom_templates, eos_cli_config_gen_configuration
    config = Configuration(warn_eos_cli_config_gen_keys=True)
    validation_result = validate_json(
        '{"fabric_name": "TEST_FABRIC", "eos_cli_config_gen_documentation": "docs", "custom_templates": "templates", "eos_cli_config_gen_configuration": "config"}',
        "eos_designs",
        config,
    )

    # Should have no violations
    assert len(validation_result.violations) == 0, f"Unexpected violations: {[(v.path, v.message) for v in validation_result.violations]}"

    # Should have no deprecations
    assert len(validation_result.deprecations) == 0

    # Should have NO ignored_eos_config_key warnings - these special keys are silently ignored
    assert len(validation_result.ignored_eos_config_keys) == 0
