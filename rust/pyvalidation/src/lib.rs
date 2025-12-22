// Copyright (c) 2025 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

// When running from Python we wish to cache Store inside Rust,
// to avoid sending the huge object back and forth.
// The store must be initialized before running validation by calling
// `init_store_from_file` which will load the store from the given file and store it for use by future validations.
// #![deny(unused_crate_dependencies)] TODO: Find alternatives like cargo-udeps since criterion is only used in bench.

use std::sync::OnceLock;

use avdschema::Store;
use pyo3::pymodule;

static STORE: OnceLock<Store> = OnceLock::new();

#[pymodule(gil_used = false)]
pub mod validation {
    use super::STORE;
    use avdschema::{Load as _, Store, any::AnySchema, resolve_ref};
    use log::{debug, info};
    use merge::merge;
    use pyo3::{Bound, PyResult, exceptions::PyRuntimeError, pyclass, pyfunction, types::PyModule};
    use std::path::PathBuf;
    use validation::{Coercion as _, Context, StoreValidate as _, Validation as _};

    fn get_store() -> PyResult<&'static Store> {
        STORE.get().ok_or_else(|| {
            PyRuntimeError::new_err(
                "The schema store was not initialized. \
             Initialization can only happen once, and must be done before running any validations."
                    .to_string(),
            )
        })
    }

    #[pyclass(frozen, get_all)]
    #[derive(Clone)]
    pub struct Violation {
        pub message: String,
        pub path: Vec<String>,
    }

    #[pyclass(frozen, get_all)]
    #[derive(Clone)]
    pub struct Deprecation {
        pub message: String,
        pub path: Vec<String>,
        pub removed: bool,
        pub version: Option<String>,
        pub replacement: Option<String>,
        pub url: Option<String>,
    }

    #[pyclass(frozen, get_all)]
    #[derive(Clone, Default)]
    pub struct ValidationResult {
        pub violations: Vec<Violation>,
        pub deprecations: Vec<Deprecation>,
    }
    impl TryFrom<validation::ValidationResult> for ValidationResult {
        type Error = pyo3::PyErr;
        fn try_from(value: validation::ValidationResult) -> Result<ValidationResult, Self::Error> {
            let mut result = ValidationResult::default();
            value
                .errors
                .iter()
                .try_for_each(|feedback| match &feedback.issue {
                    validation::feedback::ErrorIssue::Violation(violation) => {
                        result.violations.push(Violation {
                            message: violation.to_string(),
                            path: feedback.path.to_owned().into(),
                        });
                        Ok(())
                    }
                    validation::feedback::ErrorIssue::InternalError { message } => {
                        Err(pyo3::exceptions::PyRuntimeError::new_err(format!(
                            "Error occurred during validation: {message}"
                        )))
                    }
                })?;
            value
                .warnings
                .iter()
                .for_each(|feedback| match &feedback.issue {
                    validation::feedback::WarningIssue::Deprecated(deprecated) => {
                        result.deprecations.push(Deprecation {
                            message: deprecated.to_string(),
                            path: feedback.path.to_owned().into(),
                            removed: false,
                            version: deprecated.version.to_owned().into(),
                            replacement: deprecated.replacement.to_owned().into(),
                            url: deprecated.url.to_owned().into(),
                        })
                    }
                });
            Ok(result)
        }
    }

    #[pyclass(frozen, get_all)]
    pub struct ValidatedDataResult {
        pub validation_result: ValidationResult,
        pub validated_data: Option<String>,
    }

    #[pymodule_init]
    fn init(_m: &Bound<'_, PyModule>) -> PyResult<()> {
        pyo3_log::init();
        debug!("initialized python module in pyo3");
        Ok(())
    }

    #[pyfunction]
    pub fn init_store_from_file(file: PathBuf) -> PyResult<()> {
        info!("Initialize the schema store from file.");

        // Load the store from path including resolving the $refs where applicable.
        let store = Store::from_file(Some(&file))
            .map(|store| store.as_resolved())
            .map_err(|err| {
                pyo3::exceptions::PyRuntimeError::new_err(format!(
                    "Error while loading the Schema Store from file: {err}",
                ))
            })?;

        // Insert the resolved store into the OnceLock.
        STORE.set(store).map_err(|_| {
            PyRuntimeError::new_err(
                "Unable to initialize the schema store. \
                 Initialization can only happen once, and must be done before running any validations."
                    .to_string(),
            )
            }).inspect(|_| info!("Initialized the schema store from file."))
    }

    #[pyfunction]
    pub fn validate_json(data_as_json: &str, schema_name: &str) -> PyResult<ValidationResult> {
        get_store()?
            .validate_json(data_as_json, schema_name, None)
            .map_err(|err| PyRuntimeError::new_err(format!("Invalid JSON in data: {err}")))
            .and_then(|result| result.try_into())
    }

    #[pyfunction]
    pub fn get_validated_data(
        py: pyo3::Python<'_>,
        data_as_json: &str,
        schema_name: &str,
    ) -> PyResult<ValidatedDataResult> {
        debug!("pyvalidation::get_validated_data Begin");
        let result: PyResult<ValidatedDataResult> = py.detach(|| {
            // The Value here will be in-place coerced to the correct data types.
            let mut data_as_value = serde_json::from_str::<serde_json::Value>(data_as_json)
                .map_err(|err| PyRuntimeError::new_err(format!("Invalid JSON in data: {err}")))?;

            debug!("pyvalidation::get_validated_data Deserialization Done");
            let validation_result =
                get_store()?.validate_value(&mut data_as_value, schema_name, None);
            debug!("pyvalidation::get_validated_data Validation Done");
            let validated_data = if validation_result.errors.is_empty() {
                Some(serde_json::to_string(&data_as_value).map_err(|err| {
                    PyRuntimeError::new_err(format!("Invalid JSON in coerced data: {err}"))
                })?)
            } else {
                None
            };
            Ok(ValidatedDataResult {
                validation_result: validation_result.try_into()?,
                validated_data,
            })
        });
        debug!("pyvalidation::get_validated_data End");
        result
    }

    #[pyfunction]
    pub fn validate_json_with_adhoc_schema(
        data_as_json: &str,
        schema_as_json: &str,
    ) -> PyResult<ValidationResult> {
        // Parse schema JSON
        let schema: AnySchema = serde_json::from_str(schema_as_json).map_err(|err| {
            PyRuntimeError::new_err(format!("Invalid JSON in adhoc schema: {err}"))
        })?;
        // Parse data JSON
        let mut data: serde_json::Value = serde_json::from_str(data_as_json)
            .map_err(|err| PyRuntimeError::new_err(format!("Invalid JSON in data: {err}")))?;

        let mut ctx = Context::new(get_store()?, None);
        schema.coerce(&mut data, &mut ctx);
        schema.validate_value(&data, &mut ctx);

        let validation_result: validation::ValidationResult = ctx.result;
        validation_result.try_into()
    }

    #[pyfunction]
    pub fn merge_with_schema(
        base_as_json: &str,
        nexts_as_json: Vec<String>,
        schema_ref: &str,
        list_merge: &str,
    ) -> PyResult<String> {
        debug!("pyvalidation::merge_on_schema Begin");
        let base: serde_json::Value = serde_json::from_str(base_as_json)
            .map_err(|err| PyRuntimeError::new_err(format!("Invalid JSON in base: {err}")))?;
        let nexts: Vec<serde_json::Value> = nexts_as_json
            .into_iter()
            .map(|next_as_json| {
                serde_json::from_str(&next_as_json)
                    .map_err(|err| PyRuntimeError::new_err(format!("Invalid JSON in next: {err}")))
            })
            .collect::<Result<_, _>>()?;
        let schema = resolve_ref(schema_ref, get_store()?)
            .map_err(|err| PyRuntimeError::new_err(format!("Invalid schema ref: {err}")))?;
        let list_merge = list_merge
            .try_into()
            .map_err(|err: String| PyRuntimeError::new_err(err))?;
        let merged_value = merge(base, nexts, Some(schema), &list_merge);
        debug!("pyvalidation::merge_on_schema End");
        serde_json::to_string(&merged_value)
            .map_err(|err| PyRuntimeError::new_err(format!("Invalid JSON in merged data: {err}")))
    }
}

// Partial implementation of the pytests but here using pyo3 wrappers in Rust, to ensure we get coverage data
// and that we can catch issues in Rust without building the Python first.
#[cfg(test)]
mod tests {
    use std::sync::OnceLock;

    use super::{STORE, validation};
    use pyo3::types::PyAnyMethods as _;

    // Initializing python only once. Otherwise things may crash when running in multiple threads.
    // Also downloading the test schema and extracting to fragments.
    static INIT_PY: OnceLock<()> = OnceLock::new();
    fn setup() {
        INIT_PY.get_or_init(|| {
            pyo3::append_to_inittab!(validation);
            pyo3::Python::initialize();
            pyo3::Python::attach(|py| {
                init_test_store(py);
            });
        });
    }

    // Initialize the store and ignoring errors for duplicate initialization.
    // This avoids false negatives when multiple tests are executed at once.
    fn init_test_store(py: pyo3::Python<'_>) {
        if STORE.get().is_some() {
            panic!("Already set")
        }
        let module = py.import("validation").unwrap();
        {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            let file = py.detach(test_schema_store::get_store_gz_path);
            kwargs.set_item("file", file).unwrap();
            let _ = module.call_method("init_store_from_file", args, Some(&kwargs));
        };
    }

    fn get_path_and_message_from_py_violation<'py>(
        violation: pyo3::Bound<'py, pyo3::PyAny>,
    ) -> (Vec<String>, String) {
        let path: Vec<String> = violation
            .getattr("path")
            .unwrap()
            .cast_into_exact::<pyo3::types::PyList>()
            .unwrap()
            .into_iter()
            .map(|item| {
                item.cast_into_exact::<pyo3::types::PyString>()
                    .unwrap()
                    .to_string()
            })
            .collect();
        let message = violation
            .getattr("message")
            .unwrap()
            .cast_into_exact::<pyo3::types::PyString>()
            .unwrap()
            .to_string();
        (path, message)
    }

    #[test]
    fn validate_json_py_ok() {
        setup();
        pyo3::Python::attach(|py| {
            let module = py.import("validation").unwrap();
            let data_as_json_str = serde_json::json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": 12345}, {"name": "Ethernet1"}, {}]}).to_string();
            let validation_result = {
                let args = ();
                let kwargs = pyo3::types::PyDict::new(py);
                kwargs.set_item("data_as_json", data_as_json_str).unwrap();
                kwargs
                    .set_item("schema_name", "eos_cli_config_gen")
                    .unwrap();
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
                )
            }
        });
    }

    #[test]
    fn init_store_py_twice_err() {
        setup();
        pyo3::Python::attach(|py| {
            let module = py.import("validation").unwrap();
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
            )
        })
    }

    #[test]
    fn validate_json_py_invalid_json_err() {
        setup();
        pyo3::Python::attach(|py| {
            let module = py.import("validation").unwrap();
            let err = {
                let args = ();
                let kwargs = pyo3::types::PyDict::new(py);
                kwargs.set_item("data_as_json", "invalid_json").unwrap();
                kwargs
                    .set_item("schema_name", "eos_cli_config_gen")
                    .unwrap();
                module
                    .call_method("validate_json", args, Some(&kwargs))
                    .unwrap_err()
            };
            assert_eq!(
                err.value(py).to_string(),
                "Invalid JSON in data: expected value at line 1 column 1"
            )
        });
    }

    #[test]
    fn validate_json_with_adhoc_schema_py_ok() {
        setup();
        pyo3::Python::attach(|py| {
            let module = py.import("validation").unwrap();
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
            assert!(validation_result.hasattr("violations").unwrap());
            let violations = validation_result.getattr("violations").unwrap();
            assert!(violations.is_instance_of::<pyo3::types::PyList>());
            let expected_violations: [(Vec<String>, String); 1] = [(
                vec![],
                "The value '1234' is above the maximum allowed '1233'.".into(),
            )];

            assert_eq!(violations.len().unwrap(), expected_violations.len());
            for feedback in violations.try_iter().unwrap().flatten() {
                let expected_violation = get_path_and_message_from_py_violation(feedback);
                assert!(
                    expected_violations.contains(&expected_violation),
                    "violation was not found in expected violations: {expected_violation:?}"
                )
            }
        });
    }

    #[test]
    fn validate_json_with_adhoc_schema_py_invalid_json_err() {
        setup();
        pyo3::Python::attach(|py| {
            let module = py.import("validation").unwrap();
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
            )
        });
    }

    #[test]
    fn validate_json_with_adhoc_schema_py_invalid_schema_err() {
        setup();
        pyo3::Python::attach(|py| {
            let module = py.import("validation").unwrap();
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
            )
        });
    }

    #[test]
    fn get_validated_data_ok() {
        setup();
        pyo3::Python::attach(|py| {
            let module = py.import("validation").unwrap();
            let data_as_json_str = serde_json::json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": 12345}]}).to_string();
            let get_validated_data_result = {
                let args = ();
                let kwargs = pyo3::types::PyDict::new(py);
                kwargs.set_item("data_as_json", data_as_json_str).unwrap();
                kwargs
                    .set_item("schema_name", "eos_cli_config_gen")
                    .unwrap();
                module
                    .call_method("get_validated_data", args, Some(&kwargs))
                    .unwrap()
            };
            let validated_data = get_validated_data_result.getattr("validated_data").unwrap();
            let expected_data = pyo3::types::PyString::new(py, &serde_json::json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "12345"}]}).to_string());
            assert!(
                validated_data.eq(&expected_data).unwrap(),
                "Different data: {validated_data} vs {expected_data}"
            );
            let validation_result = get_validated_data_result
                .getattr("validation_result")
                .unwrap();
            let violations = validation_result.getattr("violations").unwrap();
            assert!(violations.is_instance_of::<pyo3::types::PyList>());
            assert_eq!(violations.len().unwrap(), 0);
        });
    }

    #[test]
    fn get_validated_data_not_ok() {
        setup();
        pyo3::Python::attach(|py| {
            let module = py.import("validation").unwrap();
            let data_as_json_str = serde_json::json!({"ethernet_interfaces": [{"name": "Ethernet1", "unknown": 12345}]}).to_string();
            let get_validated_data_result = {
                let args = ();
                let kwargs = pyo3::types::PyDict::new(py);
                kwargs.set_item("data_as_json", data_as_json_str).unwrap();
                kwargs
                    .set_item("schema_name", "eos_cli_config_gen")
                    .unwrap();
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
            for feedback in violations.try_iter().unwrap().flatten() {
                let expected_violation = get_path_and_message_from_py_violation(feedback);
                assert!(
                    expected_violations.contains(&expected_violation),
                    "violation was not found in expected violations: {expected_violation:?}"
                )
            }
        });
    }
}
