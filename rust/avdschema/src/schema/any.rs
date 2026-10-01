// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

#[cfg(feature = "dump_load_files")]
use std::path::PathBuf;

use serde::Deserialize;
use serde::Serialize;

use super::boolean::SourceBool;
use super::dict::SourceDict;
use super::int::SourceInt;
use super::list::SourceList;
use super::str::SourceStr;
use crate::utils::dump::Dump;
use crate::utils::load::Load;
#[cfg(feature = "dump_load_files")]
use crate::utils::load::LoadError;
#[cfg(feature = "dump_load_files")]
use crate::utils::load::LoadFromFragments;

/// Enum covering all AVD Schema types.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_more::From)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum SourceSchema {
    Bool(SourceBool),
    Int(SourceInt),
    Str(SourceStr),
    List(SourceList),
    Dict(SourceDict),
}
impl SourceSchema {
    /// Create a new schema instance based on the schema file(s) in the given path.
    /// If the path points to a directory, files matching *.yml will be read and combined
    /// with a shallow merge, so avoid overlapping keys.
    /// If the path points to a single .yml or .json file it will be used directly.
    /// If the path points to a .gz file it will decompressed and the inner file must be a json file which will then be used.
    #[cfg(feature = "dump_load_files")]
    pub fn new_from_path(path: PathBuf) -> Result<Self, LoadError> {
        if path.is_dir() {
            Self::from_fragments(&path)
        } else {
            Self::from_file(Some(&path))
        }
    }
}

impl Dump for SourceSchema {}
impl Load for SourceSchema {}
#[cfg(feature = "dump_load_files")]
impl LoadFromFragments for SourceSchema {}
