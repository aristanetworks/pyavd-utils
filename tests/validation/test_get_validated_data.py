# Copyright (c) 2025-2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
import pytest

from pyavd_utils.validation import get_validated_data


@pytest.mark.usefixtures("init_store")
def test_get_validated_data() -> None:
    coercion_and_validation_result = get_validated_data(b'{"ethernet_interfaces": [{"name": "Ethernet1", "description": 12345}]}', "eos_cli_config_gen")
    validated_data = coercion_and_validation_result.validated_data
    assert validated_data == b'{"ethernet_interfaces":[{"name":"Ethernet1","description":"12345"}]}'
    validation_result = coercion_and_validation_result.validation_result
    assert len(validation_result.violations) == 0
    assert len(validation_result.deprecations) == 0
    assert len(validation_result.ignored_eos_config_keys) == 0


@pytest.mark.usefixtures("init_store")
def test_get_validated_data_not_ok() -> None:
    expected_violations: list[tuple[list[str], str]] = [
        (["ethernet_interfaces", "0", "unknown"], "Invalid key."),
    ]

    get_validated_data_result = get_validated_data(b'{"ethernet_interfaces": [{"name": "Ethernet1", "unknown": 12345}]}', "eos_cli_config_gen")
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
    result = get_validated_data(b'{"fabric_name": "TEST_FABRIC", "router_isis": {"instance": "ISIS_TEST"}}', "eos_designs", config)

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


@pytest.mark.usefixtures("init_store")
def test_get_validated_data_with_encryption_v1_2() -> None:
    """Test roundtrip encryption with get_validated_data using Ansible Vault v1.2 format (with vault_id)."""
    from pyavd_utils.passwords import vault_decrypt, vault_encrypt
    from pyavd_utils.validation import Configuration

    # Set up vault credentials
    password = "my_secret_password"  # noqa: S105 - this is a test.
    vault_id = "test_vault"

    # Prepare test data
    test_data = b'{"ethernet_interfaces": [{"name": "Ethernet1", "description": 12345}]}'

    # Encrypt input data using vault_encrypt with vault_id (v1.2 format)
    encrypted_input = vault_encrypt(test_data, password, vault_id)
    assert encrypted_input.startswith("$ANSIBLE_VAULT;1.2;AES256;test_vault")  # Verify v1.2 format

    # Create configuration with encryption password and vault_id
    config = Configuration(encryption_password=password, encryption_vault_id=vault_id)

    # Call get_validated_data with encrypted data (as bytes for the API)
    result = get_validated_data(encrypted_input.encode("utf-8"), "eos_cli_config_gen", config)

    # Verify no validation errors
    assert len(result.validation_result.violations) == 0

    # Verify validated_data is present and encrypted
    assert result.validated_data is not None
    assert result.validated_data.startswith(b"$ANSIBLE_VAULT;1.2;AES256;test_vault")  # Should be v1.2 format

    # Decrypt the result
    decrypted_output, returned_vault_id = vault_decrypt(result.validated_data.decode("utf-8"), password)
    assert returned_vault_id == vault_id  # Should return vault_id as string

    # Verify the decrypted data matches expected (with type coercion applied)
    expected = b'{"ethernet_interfaces":[{"name":"Ethernet1","description":"12345"}]}'
    assert decrypted_output == expected


@pytest.mark.usefixtures("init_store")
def test_get_validated_data_with_encryption_v1_1() -> None:
    """Test roundtrip encryption with get_validated_data using Ansible Vault v1.1 format (without vault_id)."""
    from pyavd_utils.passwords import vault_decrypt, vault_encrypt
    from pyavd_utils.validation import Configuration

    # Set up vault password only (no vault_id)
    password = "my_secret_password"  # noqa: S105 - this is a test.

    # Prepare test data
    test_data = b'{"ethernet_interfaces": [{"name": "Ethernet1", "description": 12345}]}'

    # Encrypt input data using vault_encrypt without vault_id (v1.1 format)
    encrypted_input = vault_encrypt(test_data, password)  # No vault_id
    assert encrypted_input.startswith("$ANSIBLE_VAULT;1.1;AES256\n")  # Verify v1.1 format

    # Create configuration with encryption password only (no vault_id)
    config = Configuration(encryption_password=password)

    # Call get_validated_data with encrypted data (as bytes for the API)
    result = get_validated_data(encrypted_input.encode("utf-8"), "eos_cli_config_gen", config)

    # Verify no validation errors
    assert len(result.validation_result.violations) == 0

    # Verify validated_data is present and encrypted
    assert result.validated_data is not None
    assert result.validated_data.startswith(b"$ANSIBLE_VAULT;1.1;AES256\n")  # Should be v1.1 format

    # Decrypt the result
    decrypted_output, returned_vault_id = vault_decrypt(result.validated_data.decode("utf-8"), password)
    assert returned_vault_id is None  # Should be None for v1.1 format

    # Verify the decrypted data matches expected (with type coercion applied)
    expected = b'{"ethernet_interfaces":[{"name":"Ethernet1","description":"12345"}]}'
    assert decrypted_output == expected
