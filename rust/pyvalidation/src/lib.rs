// Copyright (c) 2025-2026 Arista Networks, Inc.
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
    use avdschema::{Load as _, Store, any::AnySchema};
    use log::{debug, info};
    use pyo3::{
        Bound, PyResult, exceptions::PyRuntimeError, pyclass, pyfunction, pymethods,
        types::PyModule,
    };
    use std::path::PathBuf;
    use validation::{
        Context, StoreValidateInput as _, Validation as _, feedback::InputDiagnostic,
    };

    fn invalid_json_in_data_err(message: impl std::fmt::Display) -> pyo3::PyErr {
        PyRuntimeError::new_err(format!("Invalid JSON in data: {message}"))
    }

    pub(crate) fn first_input_diagnostic_as_pyerr(
        diagnostic: Option<&InputDiagnostic>,
    ) -> Option<pyo3::PyErr> {
        diagnostic.map(|InputDiagnostic::ParseDiagnostic(diagnostic)| {
            invalid_json_in_data_err(&diagnostic.message)
        })
    }

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
    #[derive(Clone)]
    pub struct IgnoredEosConfigKey {
        pub message: String,
        pub path: Vec<String>,
    }

    #[pyclass(get_all, set_all)]
    #[derive(Clone, Default)]
    pub struct Configuration {
        pub ignore_required_keys_on_root_dict: bool,
        pub return_coercion_infos: bool,
        pub restrict_null_values: bool,
        pub warn_eos_config_keys: bool,
        pub return_coerced_data: bool,
    }

    #[pymethods]
    impl Configuration {
        #[new]
        #[pyo3(signature = (*, ignore_required_keys_on_root_dict=false, return_coercion_infos=false, restrict_null_values=false, warn_eos_config_keys=false, return_coerced_data=false))]
        fn new(
            ignore_required_keys_on_root_dict: bool,
            return_coercion_infos: bool,
            restrict_null_values: bool,
            warn_eos_config_keys: bool,
            return_coerced_data: bool,
        ) -> Self {
            Self {
                ignore_required_keys_on_root_dict,
                return_coercion_infos,
                restrict_null_values,
                warn_eos_config_keys,
                return_coerced_data,
            }
        }
    }

    impl From<Configuration> for validation::Configuration {
        fn from(config: Configuration) -> Self {
            Self {
                ignore_required_keys_on_root_dict: config.ignore_required_keys_on_root_dict,
                return_coercion_infos: config.return_coercion_infos,
                restrict_null_values: config.restrict_null_values,
                warn_eos_config_keys: config.warn_eos_config_keys,
                ..Default::default()
            }
        }
    }

    #[pyclass(frozen, get_all)]
    #[derive(Clone, Default)]
    pub struct ValidationResult {
        pub violations: Vec<Violation>,
        pub deprecations: Vec<Deprecation>,
        pub ignored_eos_config_keys: Vec<IgnoredEosConfigKey>,
    }
    impl ValidationResult {
        pub(crate) fn from_validation_result(
            value: validation::ValidationResult,
        ) -> PyResult<ValidationResult> {
            let mut result = ValidationResult::default();
            for feedback in value.errors {
                match feedback.issue {
                    validation::feedback::ErrorIssue::Violation(violation) => {
                        result.violations.push(Violation {
                            message: violation.to_string(),
                            path: feedback.path.into(),
                        });
                    }
                    validation::feedback::ErrorIssue::InternalError { message } => {
                        return Err(pyo3::exceptions::PyRuntimeError::new_err(format!(
                            "Error occurred during validation: {message}"
                        )));
                    }
                }
            }
            for feedback in value.warnings {
                match feedback.issue {
                    validation::feedback::WarningIssue::Deprecated(deprecated) => {
                        result.deprecations.push(Deprecation {
                            message: deprecated.to_string(),
                            path: feedback.path.into(),
                            removed: false,
                            version: deprecated.version.into(),
                            replacement: deprecated.replacement.into(),
                            url: deprecated.url.into(),
                        })
                    }
                    validation::feedback::WarningIssue::IgnoredEosConfigKey(ignored) => {
                        result.ignored_eos_config_keys.push(IgnoredEosConfigKey {
                            message: ignored.to_string(),
                            path: feedback.path.into(),
                        })
                    }
                }
            }
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
        let store = {
            let store = Store::from_file(Some(&file)).map_err(|err| {
                pyo3::exceptions::PyRuntimeError::new_err(format!(
                    "Error while loading the Schema Store from file: {err}",
                ))
            })?;
            store.as_resolved().map_err(|err| {
                pyo3::exceptions::PyRuntimeError::new_err(format!(
                    "Error while resolving the Schema Store: {err}",
                ))
            })
        }?;

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
    #[pyo3(signature = (data_as_json, schema_name, configuration=None))]
    pub fn validate_json(
        data_as_json: &str,
        schema_name: &str,
        configuration: Option<Configuration>,
    ) -> PyResult<ValidationResult> {
        let config = configuration.map(Into::into);
        let output = get_store()?
            .validate_json(data_as_json, schema_name, config.as_ref())
            .map_err(|err| {
                PyRuntimeError::new_err(format!("Error while validating the data: {err}"))
            })?;
        if let Some(err) = first_input_diagnostic_as_pyerr(output.input_diagnostics.first()) {
            return Err(err);
        }
        ValidationResult::from_validation_result(output.document.result)
    }

    #[pyfunction]
    #[pyo3(signature = (data_as_json, schema_name, configuration=None))]
    pub fn get_validated_data(
        py: pyo3::Python<'_>,
        data_as_json: &str,
        schema_name: &str,
        configuration: Option<Configuration>,
    ) -> PyResult<ValidatedDataResult> {
        debug!("pyvalidation::get_validated_data Begin");
        let result: PyResult<ValidatedDataResult> = py.detach(|| {
            // Enable return_coerced_data since this function returns validated data
            let mut config: validation::Configuration =
                configuration.map(Into::into).unwrap_or_default();
            config.return_coerced_data = true;
            let output = get_store()?
                .validate_json(data_as_json, schema_name, Some(&config))
                .map_err(|err| {
                    PyRuntimeError::new_err(format!("Error while validating the data: {err}"))
                })?;
            if let Some(err) = first_input_diagnostic_as_pyerr(output.input_diagnostics.first()) {
                return Err(err);
            }
            debug!("pyvalidation::get_validated_data Validation Done");
            let validated_data = if output.document.result.errors.is_empty() {
                output
                    .document
                    .coerced
                    .map(|coerced| {
                        serde_json::to_string(&coerced).map_err(|err| {
                            PyRuntimeError::new_err(format!("Invalid JSON in coerced data: {err}"))
                        })
                    })
                    .transpose()?
            } else {
                None
            };
            Ok(ValidatedDataResult {
                validation_result: ValidationResult::from_validation_result(
                    output.document.result,
                )?,
                validated_data,
            })
        });
        debug!("pyvalidation::get_validated_data End");
        result
    }

    #[pyfunction]
    #[pyo3(signature = (data_as_json, schema_as_json, configuration=None))]
    pub fn validate_json_with_adhoc_schema(
        data_as_json: &str,
        schema_as_json: &str,
        configuration: Option<Configuration>,
    ) -> PyResult<ValidationResult> {
        // Parse schema JSON
        let schema: AnySchema = serde_json::from_str(schema_as_json).map_err(|err| {
            PyRuntimeError::new_err(format!("Invalid JSON in adhoc schema: {err}"))
        })?;
        // Parse data JSON
        let data: serde_json::Value =
            serde_json::from_str(data_as_json).map_err(invalid_json_in_data_err)?;

        let config: Option<validation::Configuration> = configuration.map(Into::into);
        let mut ctx = Context::new(get_store()?, config.as_ref());

        // Validate returns the coerced value, but we only need the validation result here
        let _ = schema.validate(&data, &mut ctx);

        let validation_result: validation::ValidationResult = ctx.result;
        ValidationResult::from_validation_result(validation_result)
    }
}

// Partial implementation of the pytests but here using pyo3 wrappers in Rust, to ensure we get coverage data
// and that we can catch issues in Rust without building the Python first.
#[cfg(test)]
mod tests {
    use std::sync::OnceLock;

    use super::{STORE, validation};
    use pyo3::types::PyAnyMethods as _;

    use crate::validation::{ValidationResult, first_input_diagnostic_as_pyerr};

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

        let err = match ValidationResult::from_validation_result(result) {
            Ok(_) => panic!("expected internal error to convert into PyErr"),
            Err(err) => err,
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
                span: ::validation::feedback::SourceSpan { start: 0, end: 0 },
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
    fn validate_json_py_ok() {
        setup();
        pyo3::Python::attach(|py| {
            let module = py.import("validation").unwrap();
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
                kwargs.set_item("schema_name", "eos_config").unwrap();
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
                kwargs.set_item("schema_name", "eos_config").unwrap();
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
    fn validate_avd_design_with_ignored_eos_config_key() {
        setup();
        pyo3::Python::attach(|py| {
            let module = py.import("validation").unwrap();
            // router_isis is a key from eos_config that should be ignored when validating avd_design
            let data_as_json_str =
                serde_json::json!({"fabric_name": "TEST-FABRIC", "router_isis": {"instance": "ISIS_TEST"}}).to_string();

            // Create configuration with warn_eos_config_keys enabled
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

            // Should have no violations
            let violations = validation_result.getattr("violations").unwrap();
            assert!(violations.is_instance_of::<pyo3::types::PyList>());
            assert_eq!(violations.len().unwrap(), 0);

            // Should have no deprecations
            let deprecations = validation_result.getattr("deprecations").unwrap();
            assert!(deprecations.is_instance_of::<pyo3::types::PyList>());
            assert_eq!(deprecations.len().unwrap(), 0);

            // Should have one ignored_eos_config_key
            let ignored_keys = validation_result
                .getattr("ignored_eos_config_keys")
                .unwrap();
            assert!(ignored_keys.is_instance_of::<pyo3::types::PyList>());
            assert_eq!(ignored_keys.len().unwrap(), 1);

            // Check the ignored key details
            let ignored_key = ignored_keys.get_item(0).unwrap();
            let path = ignored_key
                .getattr("path")
                .unwrap()
                .cast_into_exact::<pyo3::types::PyList>()
                .unwrap();
            assert_eq!(path.len().unwrap(), 1);
            let path_item = path
                .get_item(0)
                .unwrap()
                .cast_into_exact::<pyo3::types::PyString>()
                .unwrap()
                .to_string();
            assert_eq!(path_item, "router_isis");

            let message = ignored_key
                .getattr("message")
                .unwrap()
                .cast_into_exact::<pyo3::types::PyString>()
                .unwrap()
                .to_string();
            assert_eq!(
                message,
                "Ignoring key from the EOS Config schema when validating with the AVD Design schema."
            );
        });
    }
}
