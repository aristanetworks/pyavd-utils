// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::error::Error;
use std::fmt;
use std::sync::Arc;

use avdschema::SchemaPathError;
use schema_merge::MergeError;
use validation::StoreValidateError;
use yaml_parser::ParseError;

/// Failure to decode or accept a fragment. Failed insertions leave inventory unchanged.
#[derive(Debug)]
pub struct AddFragmentError {
    /// Source identifier supplied with the rejected fragment.
    pub source: Arc<str>,
    /// Reason the fragment was rejected.
    pub kind: FragmentErrorKind,
}

/// Reasons a fragment cannot be collected.
#[derive(Debug)]
pub enum FragmentErrorKind {
    /// YAML parser diagnostics, including their source spans.
    Yaml(Vec<ParseError>),
    /// JSON decoding failure, including line and column.
    Json(serde_json::Error),
    /// Number of YAML documents found when exactly one was required.
    DocumentCount(usize),
    /// Inventory variables must have a mapping/object root.
    RootNotMapping,
}

impl fmt::Display for AddFragmentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Fragment '{}': ", self.source)?;
        match &self.kind {
            FragmentErrorKind::Yaml(errors) => write!(f, "{} YAML parser error(s)", errors.len()),
            FragmentErrorKind::Json(error) => write!(f, "{error}"),
            FragmentErrorKind::DocumentCount(count) => {
                write!(f, "expected one YAML document, found {count}")
            }
            FragmentErrorKind::RootNotMapping => f.write_str("expected a mapping root"),
        }
    }
}

impl Error for AddFragmentError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.kind {
            FragmentErrorKind::Json(error) => Some(error),
            _ => None,
        }
    }
}

/// A failure that prevents completing the inventory build.
///
/// Input validation findings are returned in [`crate::BuildOutput`], rather than
/// being execution failures. Callers must gate the common build on those findings.
#[derive(Debug)]
pub enum BuildError {
    /// The requested schema could not be selected for merging.
    Schema(SchemaPathError),
    /// A fragment could not be merged with its preceding inputs.
    Merge {
        /// Source identifier of the incoming fragment.
        source: Arc<str>,
        /// Underlying merge failure and its logical data path.
        error: MergeError,
    },
    /// The validation operation could not be performed.
    Validation(StoreValidateError),
    /// The bounded validation worker pool could not be created.
    WorkerPool(rayon::ThreadPoolBuildError),
    /// Validated output cannot be represented as JSON.
    Json(serde_json::Error),
}

impl fmt::Display for BuildError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Schema(error) => write!(f, "{error}"),
            Self::Merge { source, error } => write!(f, "Fragment '{source}': {error}"),
            Self::Validation(error) => write!(f, "{error}"),
            Self::WorkerPool(error) => write!(f, "Could not create validation workers: {error}"),
            Self::Json(error) => write!(f, "Could not encode validated JSON: {error}"),
        }
    }
}

impl Error for BuildError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Merge { error, .. } => Some(error),
            Self::WorkerPool(error) => Some(error),
            Self::Json(error) => Some(error),
            Self::Schema(_) | Self::Validation(_) => None,
        }
    }
}
