// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
// TODO: Reevaluate the allow
#![allow(
    missing_docs,
    missing_debug_implementations,
    clippy::fn_params_excessive_bools,
    clippy::manual_let_else,
    clippy::needless_pass_by_value,
    clippy::module_name_repetitions,
    clippy::struct_excessive_bools,
    clippy::unnecessary_trailing_comma,
    clippy::unnecessary_wraps,
    reason = "PyO3-facing API names and test assertions mirror the exported Python module contract"
)]

// When running from Python we wish to cache Store inside Rust,
// to avoid sending the huge object back and forth.
// The store must be initialized before running validation by calling
// `init_store_from_file` which will load the store from the given file and store it for use by future validations.
// #![deny(unused_crate_dependencies)] TODO: Find alternatives like cargo-udeps since criterion is only used in bench.

use std::sync::OnceLock;

use avdschema::Store;
use pyo3::pymodule;

mod errors;
mod exceptions;

static STORE: OnceLock<Store> = OnceLock::new();

#[pymodule(gil_used = false)]
pub mod validation {
    use std::path::PathBuf;

    use avdschema::Load as _;
    use avdschema::Store;
    use avdschema::any::AnySchema;
    use log::debug;
    use log::info;
    use pyo3::Bound;
    use pyo3::PyResult;
    use pyo3::pyclass;
    use pyo3::pyfunction;
    use pyo3::pymethods;
    use pyo3::types::PyModule;
    use validation::Context;
    use validation::StoreValidateInput as _;
    use validation::Validation as _;
    use validation::feedback::InputDiagnostic;

    use super::STORE;
    use crate::errors::GetValidatedDataPyError;
    use crate::errors::InitStoreFromFilePyError;
    use crate::errors::ValidateJsonPyError;
    use crate::errors::ValidateJsonWithAdhocSchemaPyError;
    #[rustfmt::skip]
    #[pymodule_export]
    pub(crate) use crate::exceptions::{
        ValidationError,
        ValidationInternalError,
        ValidationInvalidAdhocSchemaJsonError,
        ValidationInvalidCoercedDataJsonError,
        ValidationInvalidJsonDataError,
        ValidationInvalidSchemaNameError,
        ValidationRefSyntaxError,
        ValidationSchemaPathError,
        ValidationSchemaTypeError,
        ValidationSchemaWalkError,
        ValidationStoreAlreadyInitializedError,
        ValidationStoreInvalidExtensionError,
        ValidationStoreLoadIoError,
        ValidationStoreLoadJsonError,
        ValidationStoreLoadYamlError,
        ValidationStoreNoFilesFoundError,
        ValidationStoreNotInitializedError,
    };

    pub(crate) fn first_input_diagnostic_message(
        diagnostic: Option<&InputDiagnostic>,
    ) -> Option<&str> {
        diagnostic.map(|InputDiagnostic::ParseDiagnostic(diagnostic)| diagnostic.message.as_str())
    }

    fn get_store() -> Option<&'static Store> {
        STORE.get()
    }

    #[pyclass(from_py_object, frozen, get_all)]
    #[derive(Clone)]
    pub struct Violation {
        pub message: String,
        pub path: Vec<String>,
    }

    #[pyclass(from_py_object, frozen, get_all)]
    #[derive(Clone)]
    pub struct Deprecation {
        pub message: String,
        pub path: Vec<String>,
        pub removed: bool,
        pub version: Option<String>,
        pub replacement: Option<String>,
        pub url: Option<String>,
    }

    #[pyclass(from_py_object, frozen, get_all)]
    #[derive(Clone)]
    pub struct IgnoredEosConfigKey {
        pub message: String,
        pub path: Vec<String>,
    }

    #[pyclass(from_py_object, get_all, set_all)]
    #[derive(Clone, Default)]
    pub struct Configuration {
        pub ignore_required_keys_on_root_dict: bool,
        pub return_coercion_infos: bool,
        pub restrict_null_values: bool,
        pub warn_eos_config_keys: bool,
    }

    #[pymethods]
    impl Configuration {
        #[new]
        #[pyo3(signature = (*, ignore_required_keys_on_root_dict=false, return_coercion_infos=false, restrict_null_values=false, warn_eos_config_keys=false))]
        fn new(
            ignore_required_keys_on_root_dict: bool,
            return_coercion_infos: bool,
            restrict_null_values: bool,
            warn_eos_config_keys: bool,
        ) -> Self {
            Self {
                ignore_required_keys_on_root_dict,
                return_coercion_infos,
                restrict_null_values,
                warn_eos_config_keys,
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

    #[pyclass(from_py_object, frozen, get_all)]
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
                        return Err(ValidationInternalError::new_err(format!(
                            "Error occurred during validation: {message}."
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
                        });
                    }
                    validation::feedback::WarningIssue::IgnoredEosConfigKey(ignored) => {
                        result.ignored_eos_config_keys.push(IgnoredEosConfigKey {
                            message: ignored.to_string(),
                            path: feedback.path.into(),
                        });
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
        if STORE.get().is_some() {
            return Err(InitStoreFromFilePyError::StoreAlreadyInitialized.into());
        }

        // Load the store from path including resolving the $refs where applicable.
        let store = {
            let store =
                Store::from_file(Some(&file)).map_err(Into::<InitStoreFromFilePyError>::into)?;
            store
                .as_resolved()
                .map_err(Into::<InitStoreFromFilePyError>::into)?
        };

        // Insert the resolved store into the OnceLock.
        STORE
            .set(store)
            .map_err(|_store| InitStoreFromFilePyError::StoreAlreadyInitialized)?;
        info!("Initialized the schema store from file.");
        Ok(())
    }

    #[pyfunction]
    #[pyo3(signature = (data_as_json, schema_name, configuration=None))]
    pub fn validate_json(
        data_as_json: &str,
        schema_name: &str,
        configuration: Option<Configuration>,
    ) -> PyResult<ValidationResult> {
        let config = configuration.map(Into::into);
        let output = get_store()
            .ok_or(ValidateJsonPyError::StoreNotInitialized)?
            .validate_json(data_as_json, schema_name, config.as_ref())
            .map_err(Into::<ValidateJsonPyError>::into)?;
        if let Some(message) = first_input_diagnostic_message(output.input_diagnostics.first()) {
            return Err(ValidateJsonPyError::InvalidJsonData(message.to_owned()).into());
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
        let result: Result<ValidatedDataResult, GetValidatedDataPyError> = py.detach(|| {
            // Enable return_coerced_data since this function returns validated data
            let mut config: validation::Configuration =
                configuration.map(Into::into).unwrap_or_default();
            config.return_coerced_data = true;
            let output = get_store()
                .ok_or(GetValidatedDataPyError::StoreNotInitialized)?
                .validate_json(data_as_json, schema_name, Some(&config))?;
            if let Some(message) = first_input_diagnostic_message(output.input_diagnostics.first())
            {
                return Err(GetValidatedDataPyError::InvalidJsonData(message.to_owned()));
            }
            debug!("pyvalidation::get_validated_data Validation Done");
            let validated_data = if output.document.result.errors.is_empty() {
                output
                    .document
                    .coerced
                    .map(|coerced| serde_json::to_string(&coerced))
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
        Ok(result?)
    }

    #[pyfunction]
    #[pyo3(signature = (data_as_json, schema_as_json, configuration=None))]
    pub fn validate_json_with_adhoc_schema(
        data_as_json: &str,
        schema_as_json: &str,
        configuration: Option<Configuration>,
    ) -> PyResult<ValidationResult> {
        // Parse schema JSON
        let schema: AnySchema = serde_json::from_str(schema_as_json)
            .map_err(ValidateJsonWithAdhocSchemaPyError::InvalidAdhocSchemaJson)?;
        // Parse data JSON
        let data: serde_json::Value = serde_json::from_str(data_as_json)
            .map_err(ValidateJsonWithAdhocSchemaPyError::InvalidJsonData)?;

        let config: Option<validation::Configuration> = configuration.map(Into::into);
        let mut ctx = Context::new(
            get_store().ok_or(ValidateJsonWithAdhocSchemaPyError::StoreNotInitialized)?,
            config.as_ref(),
        );

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

    use pyo3::types::PyAnyMethods as _;

    use super::STORE;
    use super::validation;
    use crate::errors::InitStoreFromFilePyError;
    use crate::errors::ValidateJsonPyError;
    use crate::validation::ValidationResult;
    use crate::validation::first_input_diagnostic_message;

    // Initializing python only once. Otherwise things may crash when running in multiple threads.
    // Also downloading the test schema and extracting to fragments.
    static INIT_PY: OnceLock<()> = OnceLock::new();
    static INIT_STORE: OnceLock<()> = OnceLock::new();
    fn setup_py() {
        INIT_PY.get_or_init(|| {
            pyo3::append_to_inittab!(validation);
            pyo3::Python::initialize();
        });
    }

    fn setup() {
        setup_py();
        INIT_STORE.get_or_init(|| {
            pyo3::Python::attach(|py| {
                init_test_store(py);
            });
        });
    }

    // Initialize the store and ignoring errors for duplicate initialization.
    // This avoids false negatives when multiple tests are executed at once.
    fn init_test_store(py: pyo3::Python<'_>) {
        assert!(STORE.get().is_none(), "Already set");
        let module = py.import("validation").unwrap();
        {
            let args = ();
            let kwargs = pyo3::types::PyDict::new(py);
            let file = py.detach(test_schema_store::get_store_gz_path);
            kwargs.set_item("file", file).unwrap();
            let _ = module.call_method("init_store_from_file", args, Some(&kwargs));
        };
    }

    fn get_path_and_message_from_py_violation(
        violation: pyo3::Bound<'_, pyo3::PyAny>,
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
        setup_py();
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
                "Error occurred during validation: boom."
            );
            assert!(err.is_instance_of::<validation::ValidationInternalError>(py));
            assert!(err.is_instance_of::<validation::ValidationError>(py));
        });
    }

    #[test]
    fn first_input_diagnostic_as_pyerr_maps_parse_diagnostic() {
        setup_py();
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

        let err = pyo3::PyErr::from(ValidateJsonPyError::InvalidJsonData(
            first_input_diagnostic_message(Some(&diagnostic))
                .unwrap()
                .to_owned(),
        ));

        pyo3::Python::attach(|py| {
            assert_eq!(
                err.value(py).to_string(),
                "Invalid JSON in data: expected value at line 1 column 1."
            );
            assert!(err.is_instance_of::<validation::ValidationInvalidJsonDataError>(py));
            assert!(err.is_instance_of::<validation::ValidationError>(py));
        });
    }

    #[test]
    fn validation_invalid_json_data_error_uses_public_module_path() {
        setup_py();
        pyo3::Python::attach(|py| {
            let module_name: String = py
                .get_type::<validation::ValidationInvalidJsonDataError>()
                .getattr("__module__")
                .unwrap()
                .extract()
                .unwrap();

            assert_eq!(module_name, "pyavd_utils.validation");
        });
    }

    #[test]
    fn schema_resolver_errors_map_to_specific_pyerrs() {
        setup_py();
        pyo3::Python::attach(|py| {
            let schema_type_err = pyo3::PyErr::from(InitStoreFromFilePyError::from(
                avdschema::SchemaResolverError::SchemaType(avdschema::SchemaType::new(
                    "schema_ref".into(),
                    "dict".into(),
                    "list".into(),
                )),
            ));
            assert!(schema_type_err.is_instance_of::<validation::ValidationSchemaTypeError>(py));
            assert!(
                schema_type_err
                    .value(py)
                    .to_string()
                    .contains("Invalid schema type")
            );

            let ref_syntax_err = pyo3::PyErr::from(InitStoreFromFilePyError::from(
                avdschema::SchemaResolverError::RefSyntax(avdschema::RefSyntax::new(
                    "bad_ref".into(),
                )),
            ));
            assert!(ref_syntax_err.is_instance_of::<validation::ValidationRefSyntaxError>(py));
            assert!(
                ref_syntax_err
                    .value(py)
                    .to_string()
                    .contains("Invalid syntax")
            );

            let schema_path_err = pyo3::PyErr::from(InitStoreFromFilePyError::from(
                avdschema::SchemaResolverError::SchemaPath(avdschema::SchemaPath::new(
                    "missing.path".into(),
                )),
            ));
            assert!(schema_path_err.is_instance_of::<validation::ValidationSchemaPathError>(py));
            assert!(
                schema_path_err
                    .value(py)
                    .to_string()
                    .contains("was not found")
            );

            let schema_store_err = pyo3::PyErr::from(InitStoreFromFilePyError::from(
                avdschema::SchemaResolverError::SchemaStoreError(
                    avdschema::SchemaStoreError::InvalidSchemaName("missing_schema".into()),
                ),
            ));
            assert!(
                schema_store_err.is_instance_of::<validation::ValidationInvalidSchemaNameError>(py)
            );

            let schema_walk_err = pyo3::PyErr::from(InitStoreFromFilePyError::from(
                avdschema::SchemaResolverError::SchemaWalkError(
                    avdschema::SchemaWalkError::InternalError(
                        avdschema::SchemaWalkInternalError::new(),
                    ),
                ),
            ));
            assert!(schema_walk_err.is_instance_of::<validation::ValidationSchemaWalkError>(py));
            assert!(
                schema_walk_err
                    .value(py)
                    .to_string()
                    .contains("Internal error")
            );
        });
    }

    #[test]
    fn load_errors_map_to_specific_pyerrs() {
        setup_py();
        pyo3::Python::attach(|py| {
            let json_err = pyo3::PyErr::from(InitStoreFromFilePyError::from(
                avdschema::LoadError::JsonError(
                    serde_json::from_str::<serde_json::Value>("invalid").unwrap_err(),
                ),
            ));
            assert!(json_err.is_instance_of::<validation::ValidationStoreLoadJsonError>(py));

            let yaml_err = pyo3::PyErr::from(InitStoreFromFilePyError::from(
                avdschema::LoadError::YamlError(
                    serde_yaml::from_str::<serde_yaml::Value>(":").unwrap_err(),
                ),
            ));
            assert!(yaml_err.is_instance_of::<validation::ValidationStoreLoadYamlError>(py));

            let io_err = pyo3::PyErr::from(InitStoreFromFilePyError::from(
                avdschema::LoadError::IoError(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "missing",
                )),
            ));
            assert!(io_err.is_instance_of::<validation::ValidationStoreLoadIoError>(py));

            let invalid_extension_err = pyo3::PyErr::from(InitStoreFromFilePyError::from(
                avdschema::LoadError::InvalidExtension {},
            ));
            assert!(
                invalid_extension_err
                    .is_instance_of::<validation::ValidationStoreInvalidExtensionError>(py)
            );

            let no_files_found_err = pyo3::PyErr::from(InitStoreFromFilePyError::from(
                avdschema::LoadError::NoFilesFound {},
            ));
            assert!(
                no_files_found_err
                    .is_instance_of::<validation::ValidationStoreNoFilesFoundError>(py)
            );
        });
    }

    #[test]
    fn store_validate_error_maps_to_specific_pyerr() {
        setup_py();
        pyo3::Python::attach(|py| {
            let err = pyo3::PyErr::from(ValidateJsonPyError::from(
                ::validation::StoreValidateError::SchemaStore(
                    avdschema::SchemaStoreError::InvalidSchemaName("missing_schema".into()),
                ),
            ));

            assert!(err.is_instance_of::<validation::ValidationInvalidSchemaNameError>(py));
        });
    }

    #[test]
    fn validation_result_from_validation_result_maps_deprecation() {
        let result = ::validation::ValidationResult {
            errors: vec![],
            warnings: vec![::validation::feedback::Feedback {
                path: vec!["old_key".into()].into(),
                span: None,
                issue: ::validation::feedback::WarningIssue::Deprecated(
                    ::validation::feedback::Deprecated {
                        path: vec!["old_key".into()].into(),
                        replacement: Some("new_key".to_owned()).into(),
                        version: Some("5.0.0".to_owned()).into(),
                        url: Some("https://example.invalid".to_owned()).into(),
                    },
                ),
            }],
            infos: vec![],
        };

        let py_result = ValidationResult::from_validation_result(result).unwrap();

        assert_eq!(py_result.deprecations.len(), 1);
        assert_eq!(py_result.deprecations[0].path, vec!["old_key"]);
        assert!(py_result.deprecations[0].message.contains("deprecated"));
        assert_eq!(py_result.deprecations[0].version.as_deref(), Some("5.0.0"));
        assert_eq!(
            py_result.deprecations[0].replacement.as_deref(),
            Some("new_key")
        );
        assert_eq!(
            py_result.deprecations[0].url.as_deref(),
            Some("https://example.invalid")
        );
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
                );
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
            );
            assert!(err.is_instance_of::<validation::ValidationStoreAlreadyInitializedError>(py));
        });
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
                "Invalid JSON in data: expected value at line 1 column 1."
            );
            assert!(err.is_instance_of::<validation::ValidationInvalidJsonDataError>(py));
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
                );
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
                "Invalid JSON in data: expected value at line 1 column 1."
            );
            assert!(err.is_instance_of::<validation::ValidationInvalidJsonDataError>(py));
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
                "Invalid JSON in adhoc schema: missing field `type` at line 1 column 14."
            );
            assert!(err.is_instance_of::<validation::ValidationInvalidAdhocSchemaJsonError>(py));
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
                );
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
