// Copyright (c) 2025 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::utils::{
    dump::Dump,
    load::{Load, LoadError},
};

#[cfg(feature = "dump_load_files")]
use crate::utils::load::LoadFromFragments;

use super::{boolean::Bool, dict::Dict, int::Int, list::List, str::Str};

/// Enum covering all AVD Schema types.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_more::From)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum AnySchema {
    Bool(Bool),
    Int(Int),
    Str(Str),
    List(List),
    Dict(Dict),
}
impl AnySchema {
    /// Create a new schema instance based on the schema file(s) in the given path.
    /// If the path points to a directory, files matching *.yml will be read and combined
    /// with a shallow merge, so avoid overlapping keys.
    /// If the path points to a single .yml or .json file it will be used directly.
    /// If the path points to a .gz file it will decompressed and the inner file must be a json file which will then be used.
    #[cfg(feature = "dump_load_files")]
    pub fn new_from_path(path: PathBuf) -> Result<Self, LoadError> {
        if path.is_dir() {
            Self::from_fragments(path)
        } else {
            Self::from_file(Some(path))
        }
    }
}

impl Dump for AnySchema {}
impl Load for AnySchema {}
#[cfg(feature = "dump_load_files")]
impl LoadFromFragments for AnySchema {}

impl From<&AnySchema> for String {
    /// Get schema type as Python-like type string
    fn from(value: &AnySchema) -> Self {
        match value {
            AnySchema::Bool(_) => "bool".to_string(),
            AnySchema::Dict(_) => "dict".to_string(),
            AnySchema::Int(_) => "int".to_string(),
            AnySchema::List(_) => "list".to_string(),
            AnySchema::Str(_) => "str".to_string(),
        }
    }
}
