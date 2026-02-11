# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
import pytest

from pyavd_utils.validation import get_validated_data


@pytest.mark.usefixtures("init_store")
def test_get_validated_data() -> None:
    coercion_and_validation_result = get_validated_data('{"ethernet_interfaces": [{"name": "Ethernet1", "description": 12345}]}', "eos_config")
    validated_data = coercion_and_validation_result.validated_data
    assert validated_data == ('{"ethernet_interfaces":[{"name":"Ethernet1","description":"12345"}]}')
    validation_result = coercion_and_validation_result.validation_result
    assert len(validation_result.violations) == 0
    assert len(validation_result.deprecations) == 0
    assert len(validation_result.ignored_eos_config_keys) == 0


@pytest.mark.usefixtures("init_store")
def test_get_validated_data_not_ok() -> None:
    expected_violations: list[tuple[list[str], str]] = [
        (["ethernet_interfaces", "0", "unknown"], "Invalid key."),
    ]

    get_validated_data_result = get_validated_data('{"ethernet_interfaces": [{"name": "Ethernet1", "unknown": 12345}]}', "eos_config")
    validated_data = get_validated_data_result.validated_data
    assert validated_data is None

    validation_result = get_validated_data_result.validation_result
    assert len(validation_result.violations) == len(expected_violations)
    for violation in validation_result.violations:
        assert (violation.path, violation.message) in expected_violations, f"Violation not expected: {violation.path}, {violation.message}"

    assert len(validation_result.deprecations) == 0
    assert len(validation_result.ignored_eos_config_keys) == 0


@pytest.mark.usefixtures("init_store")
def test_get_validated_data_with_config() -> None:
    """Test that get_validated_data works with configuration."""
    from pyavd_utils.validation import Configuration

    config = Configuration(warn_eos_cli_config_gen_keys=True)
    result = get_validated_data('{"fabric_name": "TEST_FABRIC", "router_isis": {"instance": "ISIS_TEST"}}', "avd_design", config)

    # Should have no violations
    assert len(result.validation_result.violations) == 0

    # Should have no deprecations
    assert len(result.validation_result.deprecations) == 0

    # Should have one ignored_eos_config_key
    assert len(result.validation_result.ignored_eos_config_keys) == 1

    # Check the ignored key details
    ignored_key = result.validation_result.ignored_eos_config_keys[0]
    assert ignored_key.path == ["router_isis"]
    assert ignored_key.message == "The 'eos_cli_config_gen' key is present in the input to 'eos_designs' and will be ignored."

    # Validated data should be present since there are no violations
    assert result.validated_data is not None
