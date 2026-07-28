// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use pyo3::types::PyAnyMethods as _;

use super::setup;

#[test]
fn get_list_primary_key_py_ok() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let primary_key = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("schema_name", "eos_config").unwrap();
            kwargs
                .set_item("data_path", vec!["ethernet_interfaces"])
                .unwrap();
            module
                .call_method("get_list_primary_key", args, Some(&kwargs))
                .unwrap()
        };

        assert_eq!(primary_key.to_string(), "name");
    });
}

#[test]
fn get_list_primary_key_py_nested_list_ok() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let primary_key = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("schema_name", "eos_config").unwrap();
            kwargs
                .set_item("data_path", vec!["access_lists", "0", "sequence_numbers"])
                .unwrap();
            module
                .call_method("get_list_primary_key", args, Some(&kwargs))
                .unwrap()
        };

        assert_eq!(primary_key.to_string(), "sequence");
    });
}

#[test]
fn get_list_primary_key_py_non_list_path_is_none() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let primary_key = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("schema_name", "eos_config").unwrap();
            kwargs.set_item("data_path", vec!["hostname"]).unwrap();
            module
                .call_method("get_list_primary_key", args, Some(&kwargs))
                .unwrap()
        };

        assert!(primary_key.is_none());
    });
}

#[test]
fn get_list_primary_key_py_nested_list_without_index_is_none() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let primary_key = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("schema_name", "eos_config").unwrap();
            kwargs
                .set_item("data_path", vec!["access_lists", "sequence_numbers"])
                .unwrap();
            module
                .call_method("get_list_primary_key", args, Some(&kwargs))
                .unwrap()
        };

        assert!(primary_key.is_none());
    });
}

#[test]
fn get_list_primary_key_py_unknown_path_is_none() {
    setup();
    pyo3::Python::attach(|py| {
        let module = py
            .import("_bindings")
            .unwrap()
            .getattr("_schema_store")
            .unwrap();
        let primary_key = {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            kwargs.set_item("schema_name", "eos_config").unwrap();
            kwargs.set_item("data_path", vec!["unknown_key"]).unwrap();
            module
                .call_method("get_list_primary_key", args, Some(&kwargs))
                .unwrap()
        };

        assert!(primary_key.is_none());
    });
}

#[test]
fn get_list_primary_key_py_invalid_schema_name_errors() {
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
            kwargs.set_item("schema_name", "not_a_schema").unwrap();
            kwargs.set_item("data_path", Vec::<String>::new()).unwrap();
            module
                .call_method("get_list_primary_key", args, Some(&kwargs))
                .unwrap_err()
        };

        assert!(
            err.to_string()
                .contains("Error while resolving schema path")
        );
    });
}
