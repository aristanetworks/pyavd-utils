// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::path::PathBuf;

use avdschema::generate_metaschema_json;
use pyo3::PyResult;
use pyo3::exceptions::PyRuntimeError;
use pyo3::pyfunction;

/// AVD source-schema metaschema generation helpers.
#[pyo3::pymodule]
pub(crate) mod _metaschema {
    use super::PathBuf;
    use super::PyResult;
    use super::PyRuntimeError;
    use super::generate_metaschema_json;
    use super::pyfunction;

    #[pyfunction]
    /// Return whether a pattern compiles with AVD's runtime regex compiler and settings.
    ///
    /// This does not match input data or guarantee that matching cannot fail at runtime.
    pub(crate) fn is_valid_avd_regex_pattern(pattern: &str) -> bool {
        avdschema::str::compile_pattern(pattern).is_ok()
    }

    #[pyfunction]
    /// Write the AVD source-schema metaschema as formatted JSON.
    pub(crate) fn generate_metaschema(destination: PathBuf) -> PyResult<()> {
        let json = generate_metaschema_json().map_err(|error| {
            PyRuntimeError::new_err(format!(
                "Error while generating the AVD metaschema: {error}"
            ))
        })?;
        std::fs::write(&destination, json).map_err(|error| {
            PyRuntimeError::new_err(format!(
                "Error while writing the AVD metaschema to '{}': {error}",
                destination.display()
            ))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::_metaschema::is_valid_avd_regex_pattern;

    #[test]
    fn pattern_check_uses_the_avd_compiler() {
        for pattern in ["", "^requires-a-value$", r"\d+", "(?<=a+)b", r"\\p{Greek}"] {
            assert!(is_valid_avd_regex_pattern(pattern), "{pattern}");
        }
        for pattern in ["[", "(", r"\p{L}", r"\p{Greek}", r"\cA"] {
            assert!(!is_valid_avd_regex_pattern(pattern), "{pattern}");
        }
    }
}
