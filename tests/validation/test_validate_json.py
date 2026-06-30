# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
import pickle

import pytest

from pyavd_utils.validation import (
    ValidationError,
    ValidationInvalidJsonDataError,
    ValidationInvalidSchemaNameError,
    validate_json,
)


def test_validation_error_hierarchy() -> None:
    """Test that validation errors inherit from the validation base error."""
    assert issubclass(ValidationInvalidSchemaNameError, ValidationError)
    assert issubclass(ValidationInvalidJsonDataError, ValidationError)


def test_validation_error_module_and_pickle() -> None:
    """Test that validation errors have the public Python module path and can be pickled."""
    err = ValidationInvalidJsonDataError("boom")

    assert ValidationInvalidJsonDataError.__module__ == "pyavd_utils.validation"
    # Trusted test-only payload to verify PyO3 exception pickle support.
    unpickled = pickle.loads(pickle.dumps(err))  # noqa: S301
    assert type(unpickled) is ValidationInvalidJsonDataError
    assert str(unpickled) == "boom"


@pytest.mark.usefixtures("init_store")
def test_validate_json() -> None:
    expected_violations: list[tuple[list[str], str]] = [
        (["ethernet_interfaces", "2"], "Missing the required key 'name'."),
        (["ethernet_interfaces", "0", "name"], "The value is not unique among similar items. Conflicting item: ethernet_interfaces[1].name"),
        (["ethernet_interfaces", "1", "name"], "The value is not unique among similar items. Conflicting item: ethernet_interfaces[0].name"),
    ]
    validation_result = validate_json('{"ethernet_interfaces": [{"name": "Ethernet1", "description": 12345}, {"name": "Ethernet1"}, {}]}', "eos_config")

    assert len(validation_result.violations) == len(expected_violations)
    for violation in validation_result.violations:
        assert (violation.path, violation.message) in expected_violations, f"Error not expected: {violation.path}, {violation.message}"

    assert len(validation_result.deprecations) == 0
    assert len(validation_result.ignored_eos_config_keys) == 0


@pytest.mark.usefixtures("init_store")
def test_validate_json_invalid_schema_name_error() -> None:
    with pytest.raises(ValidationInvalidSchemaNameError, match="Schema name 'invalid_schema' not found"):
        validate_json("{}", "invalid_schema")  # type: ignore[arg-type]


@pytest.mark.usefixtures("init_store")
def test_validate_json_invalid_json_data_error() -> None:
    with pytest.raises(ValidationInvalidJsonDataError, match="Invalid JSON in data"):
        validate_json("invalid_json", "eos_config")


@pytest.mark.usefixtures("init_store")
def test_validate_json_with_ignored_eos_config_key() -> None:
    """Test that eos_config keys are ignored when validating avd_design."""
    from pyavd_utils.validation import Configuration

    # router_isis is a key from eos_config that should be ignored when validating avd_design
    config = Configuration(warn_eos_config_keys=True)
    validation_result = validate_json('{"fabric_name": "TEST_FABRIC", "router_isis": {"instance": "ISIS_TEST"}}', "avd_design", config)

    # Should have no violations
    assert len(validation_result.violations) == 0, f"Unexpected violations: {[(v.path, v.message) for v in validation_result.violations]}"

    # Should have no deprecations
    assert len(validation_result.deprecations) == 0

    # Should have one ignored_eos_config_key
    assert len(validation_result.ignored_eos_config_keys) == 1

    # Check the ignored key details
    ignored_key = validation_result.ignored_eos_config_keys[0]
    assert ignored_key.path == ["router_isis"]
    assert ignored_key.message == "Ignoring key from the EOS Config schema when validating with the AVD Design schema."


@pytest.mark.usefixtures("init_store")
def test_validate_json_without_config_no_warning() -> None:
    """Test that without configuration, no warnings are emitted for eos_config keys."""
    # router_isis is a key from eos_config
    validation_result = validate_json('{"fabric_name": "TEST_FABRIC", "router_isis": {"instance": "ISIS_TEST"}}', "avd_design")

    # Should have no violations
    assert len(validation_result.violations) == 0, f"Unexpected violations: {[(v.path, v.message) for v in validation_result.violations]}"

    # Should have no deprecations
    assert len(validation_result.deprecations) == 0

    # Should have NO ignored_eos_config_key warnings (because warn_eos_config_keys is False by default)
    assert len(validation_result.ignored_eos_config_keys) == 0


@pytest.mark.usefixtures("init_store")
def test_validate_json_with_eos_cli_config_gen_role_keys_no_warning() -> None:
    """Test that special eos_cli_config_gen role keys are silently ignored without warnings."""
    from pyavd_utils.validation import Configuration

    # These special keys should be ignored
    config = Configuration(warn_eos_config_keys=True)
    json_as_str = (
        '{"fabric_name": "TEST_FABRIC", '
        '"eos_cli_config_gen_validate_inputs_batch_size": 10,'
        '"avd_structured_config_file_format": "yaml",'
        '"custom_templates": "templates",'
        '"eos_cli_config_gen_keep_tmp_files": true,'
        '"eos_cli_config_gen_tmp_dir": "my_custom_tmp_dir",'
        '"eos_cli_config_gen_configuration": "config",'
        '"eos_cli_config_gen_documentation": "docs",'
        '"read_structured_config_from_file": "file"}'
    )
    validation_result = validate_json(json_as_str, "avd_design", config)

    # Should have no violations
    assert len(validation_result.violations) == 0, f"Unexpected violations: {[(v.path, v.message) for v in validation_result.violations]}"

    # Should have no deprecations
    assert len(validation_result.deprecations) == 0

    # Should have NO ignored_eos_config_key warnings - these special keys are silently ignored
    assert len(validation_result.ignored_eos_config_keys) == 0


@pytest.mark.usefixtures("init_store")
def test_configuration_fields_are_writable() -> None:
    """Test that Configuration fields can be read and written."""
    from pyavd_utils.validation import Configuration

    config = Configuration()

    # Test initial values (all should be False by default)
    assert config.ignore_required_keys_on_root_dict is False
    assert config.return_coercion_infos is False
    assert config.restrict_null_values is False
    assert config.warn_eos_config_keys is False

    # Test setting values
    config.ignore_required_keys_on_root_dict = True
    config.return_coercion_infos = True
    config.restrict_null_values = True
    config.warn_eos_config_keys = True

    # Test reading updated values
    assert config.ignore_required_keys_on_root_dict is True
    assert config.return_coercion_infos is True
    assert config.restrict_null_values is True
    assert config.warn_eos_config_keys is True
