// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

/// File-backed stage-cache helpers.
#[pyo3::pymodule]
pub(crate) mod _stage_cache {
    use pyo3::Bound;
    use pyo3::PyResult;
    use pyo3::types::PyModule;

    #[pymodule_init]
    fn init(module: &Bound<'_, PyModule>) -> PyResult<()> {
        stage_cache::python::add_cache_classes(module)
    }
}
