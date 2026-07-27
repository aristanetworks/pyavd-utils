// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

#[cfg(test)]
pub(crate) use _validation::ValidationResult;
#[cfg(test)]
pub(crate) use _validation::first_input_diagnostic_as_pyerr;

/// Schema validation helpers.
#[pyo3::pymodule]
pub(crate) mod _validation {
    use ::validation::Context;
    use ::validation::StoreValidateInput as _;
    use ::validation::Validation as _;
    use ::validation::feedback::InputDiagnostic;
    use avdschema::any::AnySchema;
    use log::debug;
    use pyo3::PyResult;
    use pyo3::exceptions::PyRuntimeError;
    use pyo3::pyclass;
    use pyo3::pyfunction;
    use pyo3::pymethods;

    use crate::schema_store::get_store;

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

    #[pyclass(from_py_object, frozen, get_all)]
    #[derive(Clone)]
    pub(crate) struct Violation {
        pub message: String,
        pub path: Vec<String>,
    }

    #[pyclass(from_py_object, frozen, get_all)]
    #[derive(Clone)]
    pub(crate) struct Deprecation {
        pub message: String,
        pub path: Vec<String>,
        pub removed: bool,
        pub version: Option<String>,
        pub replacement: Option<String>,
        pub url: Option<String>,
    }

    #[pyclass(from_py_object, frozen, get_all)]
    #[derive(Clone)]
    pub(crate) struct IgnoredEosConfigKey {
        pub message: String,
        pub path: Vec<String>,
    }

    #[pyclass(from_py_object, get_all, set_all)]
    #[derive(Clone, Default)]
    pub(crate) struct Configuration {
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

    impl From<Configuration> for ::validation::Configuration {
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
    pub(crate) struct ValidationResult {
        pub violations: Vec<Violation>,
        pub deprecations: Vec<Deprecation>,
        pub ignored_eos_config_keys: Vec<IgnoredEosConfigKey>,
    }

    impl ValidationResult {
        pub(crate) fn from_validation_result(
            value: ::validation::ValidationResult,
        ) -> PyResult<ValidationResult> {
            let mut result = ValidationResult::default();
            for feedback in value.errors {
                match feedback.issue {
                    ::validation::feedback::ErrorIssue::Violation(violation) => {
                        result.violations.push(Violation {
                            message: violation.to_string(),
                            path: feedback.path.into(),
                        });
                    }
                    ::validation::feedback::ErrorIssue::InternalError { message } => {
                        return Err(PyRuntimeError::new_err(format!(
                            "Error occurred during validation: {message}"
                        )));
                    }
                }
            }
            for feedback in value.warnings {
                match feedback.issue {
                    ::validation::feedback::WarningIssue::Deprecated(deprecated) => {
                        result.deprecations.push(Deprecation {
                            message: deprecated.to_string(),
                            path: feedback.path.into(),
                            removed: false,
                            version: deprecated.version.into(),
                            replacement: deprecated.replacement.into(),
                            url: deprecated.url.into(),
                        });
                    }
                    ::validation::feedback::WarningIssue::IgnoredEosConfigKey(ignored) => {
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
    pub(crate) struct ValidatedDataResult {
        pub validation_result: ValidationResult,
        pub validated_data: Option<String>,
    }

    #[pyfunction]
    #[pyo3(signature = (data_as_json, schema_name, configuration=None))]
    pub(crate) fn validate_json(
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
    pub(crate) fn get_validated_data(
        py: pyo3::Python<'_>,
        data_as_json: &str,
        schema_name: &str,
        configuration: Option<Configuration>,
    ) -> PyResult<ValidatedDataResult> {
        debug!("python_bindings::get_validated_data Begin");
        let result: PyResult<ValidatedDataResult> = py.detach(|| {
            let mut config: ::validation::Configuration =
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
            debug!("python_bindings::get_validated_data Validation Done");
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
        debug!("python_bindings::get_validated_data End");
        result
    }

    #[pyfunction]
    #[pyo3(signature = (data_as_json, schema_as_json, configuration=None))]
    pub(crate) fn validate_json_with_adhoc_schema(
        data_as_json: &str,
        schema_as_json: &str,
        configuration: Option<Configuration>,
    ) -> PyResult<ValidationResult> {
        let schema: AnySchema = serde_json::from_str(schema_as_json).map_err(|err| {
            PyRuntimeError::new_err(format!("Invalid JSON in adhoc schema: {err}"))
        })?;
        let data: serde_json::Value =
            serde_json::from_str(data_as_json).map_err(invalid_json_in_data_err)?;

        let config: Option<::validation::Configuration> = configuration.map(Into::into);
        let mut ctx = Context::new(get_store()?, config.as_ref());
        let _ = schema.validate(&data, &mut ctx);

        ValidationResult::from_validation_result(ctx.result)
    }
}
