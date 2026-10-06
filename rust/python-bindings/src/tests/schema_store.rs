// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::types::PyAnyMethods as _;

use super::setup;

#[test]
fn get_schema_info_py_ok() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let info = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("schema_name", "eos_config").unwrap();
            kwargs
                .set_item("data_path", vec!["ethernet_interfaces"])
                .unwrap();
            module
                .call_method("get_schema_info", args, Some(&kwargs))
                .unwrap()
        };

        assert_eq!(info.getattr("schema_type").unwrap().to_string(), "list");
        assert_eq!(info.getattr("primary_key").unwrap().to_string(), "name");
    });
}

#[test]
fn get_schema_info_py_nested_list_ok() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let info = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("schema_name", "eos_config").unwrap();
            kwargs
                .set_item("data_path", vec!["access_lists", "0", "sequence_numbers"])
                .unwrap();
            module
                .call_method("get_schema_info", args, Some(&kwargs))
                .unwrap()
        };

        assert_eq!(info.getattr("schema_type").unwrap().to_string(), "list");
        assert_eq!(info.getattr("primary_key").unwrap().to_string(), "sequence");
    });
}

#[test]
fn get_schema_info_py_avd_design_ok() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let info = {
            let args = ("avd_design", vec!["node_type_keys"]);
            module.call_method1("get_schema_info", args).unwrap()
        };

        assert_eq!(info.getattr("schema_type").unwrap().to_string(), "list");
        assert_eq!(info.getattr("primary_key").unwrap().to_string(), "key");
    });
}

#[test]
fn get_schema_info_py_scalar_path_ok() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let info = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("schema_name", "eos_config").unwrap();
            kwargs.set_item("data_path", vec!["hostname"]).unwrap();
            module
                .call_method("get_schema_info", args, Some(&kwargs))
                .unwrap()
        };

        assert_eq!(info.getattr("schema_type").unwrap().to_string(), "str");
        assert!(info.getattr("primary_key").unwrap().is_none());
    });
}

#[test]
fn get_schema_info_py_nested_list_without_index_is_none() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let info = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("schema_name", "eos_config").unwrap();
            kwargs
                .set_item("data_path", vec!["access_lists", "sequence_numbers"])
                .unwrap();
            module
                .call_method("get_schema_info", args, Some(&kwargs))
                .unwrap()
        };

        assert!(info.is_none());
    });
}

#[test]
fn get_schema_info_py_unknown_path_is_none() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let info = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("schema_name", "eos_config").unwrap();
            kwargs.set_item("data_path", vec!["unknown_key"]).unwrap();
            module
                .call_method("get_schema_info", args, Some(&kwargs))
                .unwrap()
        };

        assert!(info.is_none());
    });
}

#[test]
fn get_schema_info_py_unsupported_schema_name_errors() {
    setup();
    for schema_name in ["eos_cli_config_gen", "eos_designs", "cv_deploy"] {
        pyo3::Python::attach(|py| {
            let module = py
                .import("_bindings")
                .unwrap()
                .getattr("_schema_store")
                .unwrap();
            let err = {
                let args = ();
                let kwargs = pyo3::types::PyDict::new(py);
                kwargs.set_item("schema_name", schema_name).unwrap();
                kwargs.set_item("data_path", Vec::<String>::new()).unwrap();
                module
                    .call_method("get_schema_info", args, Some(&kwargs))
                    .unwrap_err()
            };

            assert!(err.to_string().contains("not supported"));
        });
    }
}

#[test]
fn get_schema_info_py_invalid_schema_path_errors() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let err = {
            let args = ("eos_config", vec!["hostname", "INVALID"]);
            module.call_method1("get_schema_info", args).unwrap_err()
        };

        assert!(err.is_instance_of::<pyo3::exceptions::PyRuntimeError>(py));
        assert!(
            err.to_string()
                .contains("Data path cannot be traversed through this schema node")
        );
    });
}
