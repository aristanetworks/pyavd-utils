// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::collections::HashMap;
use std::collections::HashSet;
use std::path::Path;
use std::path::PathBuf;

use avdschema::Load as _;
use avdschema::StoreSource;
use avdschema::generate_markdown_documentation;
use avdschema::generate_python_models;
use avdschema::generate_python_models_projection;
use pyo3::PyResult;
use pyo3::exceptions::PyRuntimeError;
use pyo3::exceptions::PyValueError;
use pyo3::pyfunction;

/// Schema-driven artifact generation helpers.
#[pyo3::pymodule]
pub(crate) mod _schema_generation {
    use super::*;

    #[pyfunction]
    #[pyo3(signature = (source, schema_name, destination, generated_class_name=None, root_keys=None))]
    pub(crate) fn generate_python_schema_models(
        source: PathBuf,
        schema_name: &str,
        destination: PathBuf,
        generated_class_name: Option<&str>,
        root_keys: Option<Vec<String>>,
    ) -> PyResult<()> {
        let store = StoreSource::from_file(Some(&source)).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while loading the Schema Store from file: {err}"
            ))
        })?;
        generate(
            &store,
            schema_name,
            destination,
            generated_class_name,
            root_keys,
        )
    }

    #[pyfunction]
    #[pyo3(signature = (sources, schema_name, destination, generated_class_name=None, root_keys=None))]
    pub(crate) fn generate_python_schema_models_from_paths(
        sources: HashMap<String, PathBuf>,
        schema_name: &str,
        destination: PathBuf,
        generated_class_name: Option<&str>,
        root_keys: Option<Vec<String>>,
    ) -> PyResult<()> {
        let store = StoreSource::new_from_paths(sources).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while loading schemas from the given paths: {err}"
            ))
        })?;
        generate(
            &store,
            schema_name,
            destination,
            generated_class_name,
            root_keys,
        )
    }

    #[pyfunction]
    pub(crate) fn generate_schema_documentation(
        source: PathBuf,
        schema_name: &str,
        destination: PathBuf,
    ) -> PyResult<()> {
        let store = StoreSource::from_file(Some(&source)).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while loading the Schema Store from file: {err}"
            ))
        })?;
        generate_documentation(&store, schema_name, &destination)
    }

    #[pyfunction]
    pub(crate) fn generate_schema_documentation_from_paths(
        sources: HashMap<String, PathBuf>,
        schema_name: &str,
        destination: PathBuf,
    ) -> PyResult<()> {
        let store = StoreSource::new_from_paths(sources).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while loading schemas from the given paths: {err}"
            ))
        })?;
        generate_documentation(&store, schema_name, &destination)
    }

    fn generate(
        store: &StoreSource,
        schema_name: &str,
        destination: PathBuf,
        generated_class_name: Option<&str>,
        root_keys: Option<Vec<String>>,
    ) -> PyResult<()> {
        let generated = match (generated_class_name, root_keys) {
            (None, None) => generate_python_models(store, schema_name),
            (None, Some(_)) => {
                return Err(PyValueError::new_err(
                    "generated_class_name is required when root_keys is set",
                ));
            }
            (Some(generated_class_name), root_keys) => generate_python_models_projection(
                store,
                schema_name,
                generated_class_name,
                root_keys.as_deref().unwrap_or_default(),
            ),
        }
        .map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while generating Python schema classes: {err}"
            ))
        })?;
        std::fs::write(&destination, generated).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while writing generated Python schema classes to '{}': {err}",
                destination.display()
            ))
        })
    }

    fn generate_documentation(
        store: &StoreSource,
        schema_name: &str,
        destination: &Path,
    ) -> PyResult<()> {
        let generated = generate_markdown_documentation(store, schema_name).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while generating schema documentation: {err}"
            ))
        })?;
        std::fs::create_dir_all(destination).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while creating schema documentation directory '{}': {err}",
                destination.display()
            ))
        })?;
        let expected_files = generated
            .iter()
            .map(|(table, _)| format!("{table}.md"))
            .collect::<HashSet<_>>();
        for entry in std::fs::read_dir(destination).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while reading schema documentation directory '{}': {err}",
                destination.display()
            ))
        })? {
            let entry = entry.map_err(|err| {
                PyRuntimeError::new_err(format!(
                    "Error while reading an entry in schema documentation directory '{}': {err}",
                    destination.display()
                ))
            })?;
            let path = entry.path();
            if path.extension().is_some_and(|extension| extension == "md")
                && !expected_files.contains(&entry.file_name().to_string_lossy().into_owned())
            {
                std::fs::remove_file(&path).map_err(|err| {
                    PyRuntimeError::new_err(format!(
                        "Error while removing obsolete schema documentation file '{}': {err}",
                        path.display()
                    ))
                })?;
            }
        }
        for (table, contents) in generated {
            let path = destination.join(format!("{table}.md"));
            std::fs::write(&path, contents).map_err(|err| {
                PyRuntimeError::new_err(format!(
                    "Error while writing generated schema documentation to '{}': {err}",
                    path.display()
                ))
            })?;
        }
        Ok(())
    }
}
