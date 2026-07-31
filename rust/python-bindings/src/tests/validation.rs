// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::types::PyAnyMethods as _;

use super::get_path_and_message_from_py_violation;
use super::setup;
use crate::validation::ValidationResult;
use crate::validation::first_input_diagnostic_as_pyerr;

#[test]
fn validation_result_from_validation_result_maps_violation() {
    let result = ::validation::ValidationResult {
        errors: vec![::validation::feedback::Feedback {
            path: vec!["foo".into()].into(),
            span: None,
            issue: ::validation::feedback::Violation::UnexpectedKey().into(),
        }],
        warnings: vec![],
        infos: vec![],
    };

    let py_result = ValidationResult::from_validation_result(result).unwrap();

    assert_eq!(py_result.violations.len(), 1);
    assert_eq!(py_result.violations[0].path, vec!["foo"]);
    assert_eq!(py_result.violations[0].message, "Invalid key.");
    assert!(py_result.deprecations.is_empty());
    assert!(py_result.ignored_eos_config_keys.is_empty());
}

#[test]
fn validation_result_from_validation_result_internal_error_returns_pyerr() {
    setup();
    let result = ::validation::ValidationResult {
        errors: vec![::validation::feedback::Feedback {
            path: vec![].into(),
            span: None,
            issue: ::validation::feedback::ErrorIssue::InternalError {
                message: "boom".into(),
            },
        }],
        warnings: vec![],
        infos: vec![],
    };

    let Err(err) = ValidationResult::from_validation_result(result) else {
        panic!("expected internal error to convert into PyErr");
    };

    pyo3::Python::attach(|py| {
        assert_eq!(
            err.value(py).to_string(),
            "Error occurred during validation: boom"
        );
    });
}

#[test]
fn first_input_diagnostic_as_pyerr_maps_parse_diagnostic() {
    setup();
    let diagnostic = ::validation::feedback::InputDiagnostic::ParseDiagnostic(
        ::validation::feedback::ParseDiagnostic {
            kind: ::validation::feedback::ParseDiagnosticKind::JsonSyntax,
            message: "expected value at line 1 column 1".into(),
            suggestion: None,
            location: ::validation::feedback::DiagnosticLocation::LineColumn(
                ::validation::feedback::LineColumn { line: 1, column: 1 },
            ),
        },
    );

    let err = first_input_diagnostic_as_pyerr(Some(&diagnostic)).unwrap();

    pyo3::Python::attach(|py| {
        assert_eq!(
            err.value(py).to_string(),
            "Invalid JSON in data: expected value at line 1 column 1"
        );
    });
}

#[test]
fn init_store_py_twice_err() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let err = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            let file = py.detach(test_schema_store::get_store_gz_path);
            kwargs.set_item("file", file).unwrap();
            module
                .call_method("init_store_from_file", args, Some(&kwargs))
                .unwrap_err()
        };

        assert_eq!(
            err.value(py).to_string(),
            "Unable to initialize the schema store. \
                 Initialization can only happen once, and must be done before running any validations."
        );
    });
}

#[test]
fn validate_json_py_ok() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_validation")
            .unwrap();
        let data_as_json_str = serde_json::json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": 12345}, {"name": "Ethernet1"}, {}]}).to_string();
        let validation_result = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("data_as_json", data_as_json_str).unwrap();
            kwargs.set_item("schema_name", "eos_config").unwrap();
            module
                .call_method("validate_json", args, Some(&kwargs))
                .unwrap()
        };
        let violations = validation_result.getattr("violations").unwrap();
        assert!(violations.is_instance_of::<pyo3::types::PyList>());
        let expected_violations: [(Vec<String>, String); 3] = [
            (vec!["ethernet_interfaces".into(), "2".into()], "Missing the required key 'name'.".into()),
            (vec!["ethernet_interfaces".into(), "0".into(), "name".into()], "The value is not unique among similar items. Conflicting item: ethernet_interfaces[1].name".into()),
            (vec!["ethernet_interfaces".into(), "1".into(), "name".into()], "The value is not unique among similar items. Conflicting item: ethernet_interfaces[0].name".into()),
        ];

        assert_eq!(violations.len().unwrap(), expected_violations.len());
        for violation in violations.try_iter().unwrap().flatten() {
            let expected_violation = get_path_and_message_from_py_violation(violation);
            assert!(
                expected_violations.contains(&expected_violation),
                "violation was not found in expected violations: {expected_violation:?}"
            );
        }
    });
}

#[test]
fn validate_json_py_invalid_json_err() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_validation")
            .unwrap();
        let err = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("data_as_json", "invalid_json").unwrap();
            kwargs.set_item("schema_name", "eos_config").unwrap();
            module
                .call_method("validate_json", args, Some(&kwargs))
                .unwrap_err()
        };

        assert_eq!(
            err.value(py).to_string(),
            "Invalid JSON in data: expected value at line 1 column 1"
        );
    });
}

#[test]
fn validate_json_with_adhoc_schema_py_ok() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_validation")
            .unwrap();
        let validation_result = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs
                .set_item("data_as_json", serde_json::json!(1234).to_string())
                .unwrap();
            kwargs
                .set_item(
                    "schema_as_json",
                    serde_json::json!({"type": "int", "max": 1233}).to_string(),
                )
                .unwrap();
            module
                .call_method("validate_json_with_adhoc_schema", args, Some(&kwargs))
                .unwrap()
        };
        let violations = validation_result.getattr("violations").unwrap();
        assert!(violations.is_instance_of::<pyo3::types::PyList>());
        let expected_violations: [(Vec<String>, String); 1] = [(
            vec![],
            "The value '1234' is above the maximum allowed '1233'.".into(),
        )];

        assert_eq!(violations.len().unwrap(), expected_violations.len());
        for violation in violations.try_iter().unwrap().flatten() {
            let expected_violation = get_path_and_message_from_py_violation(violation);
            assert!(
                expected_violations.contains(&expected_violation),
                "violation was not found in expected violations: {expected_violation:?}"
            );
        }
    });
}

#[test]
fn validate_json_with_adhoc_schema_py_invalid_json_err() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_validation")
            .unwrap();
        let err = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("data_as_json", "invalid_json").unwrap();
            kwargs
                .set_item(
                    "schema_as_json",
                    serde_json::json!({"type": "dict"}).to_string(),
                )
                .unwrap();
            module
                .call_method("validate_json_with_adhoc_schema", args, Some(&kwargs))
                .unwrap_err()
        };

        assert_eq!(
            err.value(py).to_string(),
            "Invalid JSON in data: expected value at line 1 column 1"
        );
    });
}

#[test]
fn validate_json_with_adhoc_schema_py_invalid_schema_err() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_validation")
            .unwrap();
        let err = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("data_as_json", "{}").unwrap();
            kwargs
                .set_item(
                    "schema_as_json",
                    serde_json::json!({"tpe": "dict"}).to_string(),
                )
                .unwrap();
            module
                .call_method("validate_json_with_adhoc_schema", args, Some(&kwargs))
                .unwrap_err()
        };

        assert_eq!(
            err.value(py).to_string(),
            "Invalid JSON in adhoc schema: missing field `type` at line 1 column 14"
        );
    });
}

#[test]
fn get_validated_data_ok() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_validation")
            .unwrap();
        let data_as_json_str =
            serde_json::json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": 12345}]}).to_string();
        let get_validated_data_result = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("data_as_json", data_as_json_str).unwrap();
            kwargs.set_item("schema_name", "eos_config").unwrap();
            module
                .call_method("get_validated_data", args, Some(&kwargs))
                .unwrap()
        };
        let validated_data = get_validated_data_result.getattr("validated_data").unwrap();
        let expected_data = pyo3::types::PyString::new(
            py,
            &serde_json::json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "12345"}]}).to_string(),
        );
        assert!(
            validated_data.eq(&expected_data).unwrap(),
            "Different data: {validated_data} vs {expected_data}"
        );
    });
}

#[test]
fn get_validated_data_not_ok() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_validation")
            .unwrap();
        let data_as_json_str =
            serde_json::json!({"ethernet_interfaces": [{"name": "Ethernet1", "unknown": 12345}]})
                .to_string();
        let get_validated_data_result = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("data_as_json", data_as_json_str).unwrap();
            kwargs.set_item("schema_name", "eos_config").unwrap();
            module
                .call_method("get_validated_data", args, Some(&kwargs))
                .unwrap()
        };
        let validated_data = get_validated_data_result.getattr("validated_data").unwrap();
        assert!(
            validated_data.is_none(),
            "Different data: {validated_data} vs None"
        );

        let validation_result = get_validated_data_result
            .getattr("validation_result")
            .unwrap();
        let violations = validation_result.getattr("violations").unwrap();
        assert!(violations.is_instance_of::<pyo3::types::PyList>());
        let expected_violations: [(Vec<String>, String); 1] = [(
            vec!["ethernet_interfaces".into(), "0".into(), "unknown".into()],
            "Invalid key.".into(),
        )];

        assert_eq!(violations.len().unwrap(), expected_violations.len());
        for violation in violations.try_iter().unwrap().flatten() {
            let expected_violation = get_path_and_message_from_py_violation(violation);
            assert!(
                expected_violations.contains(&expected_violation),
                "violation was not found in expected violations: {expected_violation:?}"
            );
        }
    });
}

#[test]
fn validate_avd_design_with_ignored_eos_config_key() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_validation")
            .unwrap();
        let data_as_json_str =
            serde_json::json!({"fabric_name": "TEST-FABRIC", "router_isis": {"instance": "ISIS_TEST"}}).to_string();
        let config = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("warn_eos_config_keys", true).unwrap();
            module
                .call_method("Configuration", args, Some(&kwargs))
                .unwrap()
        };

        let get_validated_data_result = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("data_as_json", data_as_json_str).unwrap();
            kwargs.set_item("schema_name", "avd_design").unwrap();
            kwargs.set_item("configuration", config).unwrap();
            module
                .call_method("get_validated_data", args, Some(&kwargs))
                .unwrap()
        };
        let validation_result = get_validated_data_result
            .getattr("validation_result")
            .unwrap();
        let violations = validation_result.getattr("violations").unwrap();
        let deprecations = validation_result.getattr("deprecations").unwrap();
        let ignored_keys = validation_result
            .getattr("ignored_eos_config_keys")
            .unwrap();

        assert!(violations.is_instance_of::<pyo3::types::PyList>());
        assert!(deprecations.is_instance_of::<pyo3::types::PyList>());
        assert!(ignored_keys.is_instance_of::<pyo3::types::PyList>());
        assert_eq!(violations.len().unwrap(), 0);
        assert_eq!(deprecations.len().unwrap(), 0);
        assert_eq!(ignored_keys.len().unwrap(), 1);

        let ignored_key = ignored_keys.get_item(0).unwrap();
        let path = ignored_key
            .getattr("path")
            .unwrap()
            .cast_into_exact::<pyo3::types::PyList>()
            .unwrap();
        assert_eq!(path.len().unwrap(), 1);
        assert_eq!(
            path.get_item(0)
                .unwrap()
                .cast_into_exact::<pyo3::types::PyString>()
                .unwrap()
                .to_string(),
            "router_isis"
        );
        assert_eq!(
            ignored_key.getattr("message").unwrap().to_string(),
            "Ignoring key from the EOS Config schema when validating with the AVD Design schema."
        );
    });
}
