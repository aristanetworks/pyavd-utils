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
use avdschema::generate_schema_explorer_rows;
use pyo3::PyResult;
use pyo3::exceptions::PyRuntimeError;
use pyo3::exceptions::PyValueError;
use pyo3::pyfunction;

/// Schema-driven artifact generation helpers.
#[pyo3::pymodule]
pub(crate) mod _schema_generation {
    use super::*;

    /// Export a source store's selected field occurrences as JSON Lines.
    #[pyfunction]
    pub(crate) fn generate_schema_explorer_jsonl(
        source: PathBuf,
        schema_names: Vec<String>,
        destination: PathBuf,
    ) -> PyResult<HashMap<String, usize>> {
        let store = StoreSource::from_file(Some(&source)).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while loading the Schema Store from file: {err}"
            ))
        })?;
        generate_explorer_jsonl(&store, &schema_names, &destination)
    }

    /// Export individually named schemas' selected field occurrences as JSON Lines.
    #[pyfunction]
    pub(crate) fn generate_schema_explorer_jsonl_from_paths(
        sources: HashMap<String, PathBuf>,
        schema_names: Vec<String>,
        destination: PathBuf,
    ) -> PyResult<HashMap<String, usize>> {
        let store = StoreSource::new_from_paths(sources).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while loading schemas from the given paths: {err}"
            ))
        })?;
        generate_explorer_jsonl(&store, &schema_names, &destination)
    }

    fn generate_explorer_jsonl(
        store: &StoreSource,
        schema_names: &[String],
        destination: &Path,
    ) -> PyResult<HashMap<String, usize>> {
        let mut counts = HashMap::new();
        for name in schema_names {
            if counts.insert(name.clone(), 0).is_some() {
                return Err(PyValueError::new_err(
                    "Schema Explorer schema names must be unique",
                ));
            }
        }
        let rows = generate_schema_explorer_rows(store, schema_names).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while generating Schema Explorer rows: {err}"
            ))
        })?;
        let mut encoded = Vec::new();
        for row in rows {
            if let Some(count) = counts.get_mut(&row.module) {
                *count += 1;
            }
            serde_json::to_writer(&mut encoded, &row).map_err(|err| {
                PyRuntimeError::new_err(format!("Error while encoding Schema Explorer rows: {err}"))
            })?;
            encoded.push(b'\n');
        }
        // Complete schema analysis and encoding before opening the output.
        // These are build artifacts, not the runtime validation archive.
        std::fs::write(destination, encoded).map_err(|err| {
            PyRuntimeError::new_err(format!(
                "Error while writing Schema Explorer JSONL to '{}': {err}",
                destination.display()
            ))
        })?;
        Ok(counts)
    }

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
        // Authored names follow the metaschema's `[a-z0-9_-]+` pattern. Derived
        // names can also contain dots from dynamic-key paths. Check every final
        // name before filesystem changes, including when the metaschema was bypassed.
        for (table, _) in &generated {
            if table.is_empty()
                || !table.bytes().all(|character| {
                    character.is_ascii_lowercase()
                        || character.is_ascii_digit()
                        || matches!(character, b'_' | b'-' | b'.')
                })
            {
                return Err(PyValueError::new_err(format!(
                    "Invalid documentation table name {table:?}: expected lowercase ASCII letters, digits, underscores, hyphens, or dots"
                )));
            }
        }
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
