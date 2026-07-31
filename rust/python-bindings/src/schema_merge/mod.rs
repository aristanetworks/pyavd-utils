// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

/// Schema-guided merge helpers.
#[pyo3::pymodule]
pub(crate) mod _schema_merge {
    use pyo3::PyResult;
    use pyo3::exceptions::PyRuntimeError;
    use pyo3::exceptions::PyValueError;
    use pyo3::pyfunction;
    use schema_merge::StoreSchemaMerge as _;

    use crate::schema_store::get_store;

    #[pyfunction]
    #[pyo3(signature = (base_as_json, nexts_as_json, schema_name, *, list_merge="append_unique"))]
    /// Merge JSON documents using the initialized schema store.
    ///
    /// Dynamic-key schemas are not blocked, but they are not fully supported. If
    /// a merge input modifies data used to resolve dynamic keys during the same
    /// merge, nested primary-key list merging may use stale schema resolution.
    ///
    /// For lists with schema primary keys, append/prepend strategies deep-merge
    /// by primary key. Replace/keep retain their full-list semantics.
    /// Keep-merge deep-merges matching primary-key items while keeping the
    /// existing list.
    /// Items with a missing or null primary key follow the ordinary list
    /// strategy, including full-value deduplication for unique strategies.
    pub(crate) fn merge_json(
        py: pyo3::Python<'_>,
        base_as_json: &str,
        nexts_as_json: Vec<String>,
        schema_name: &str,
        list_merge: &str,
    ) -> PyResult<String> {
        let list_merge =
            list_merge
                .parse()
                .map_err(|err: schema_merge::InvalidListMergeStrategy| {
                    PyValueError::new_err(err.to_string())
                })?;
        let store = get_store()?;
        py.detach(|| {
            store
                .merge_json(base_as_json, nexts_as_json, schema_name, list_merge)
                .map_err(|err| PyRuntimeError::new_err(err.to_string()))
        })
    }
}
