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
    # router_isis is a key from eos_cli_config_gen that should be ignored when validating eos_designs
    validation_result = validate_json('{"fabric_name": "TEST_FABRIC", "router_isis": {"instance": "ISIS_TEST"}}', "eos_designs")

    # Should have no violations
    assert len(validation_result.violations) == 0, f"Unexpected violations: {[(v.path, v.message) for v in validation_result.violations]}"

    # Should have no deprecations
    assert len(validation_result.deprecations) == 0

    # Should have one ignored_eos_config_key
    assert len(validation_result.ignored_eos_config_keys) == 1

    # Check the ignored key details
    ignored_key = validation_result.ignored_eos_config_keys[0]
    assert ignored_key.key == "router_isis"
    assert ignored_key.message == "The 'eos_cli_config_gen' key 'router_isis' is present in the input to 'eos_designs' and will be ignored."
