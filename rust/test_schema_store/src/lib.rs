// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
#![deny(unused_crate_dependencies)]

use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;
use std::sync::OnceLock;

use avdschema::Dump as _;
use avdschema::Store;

const CRATE_DIR: &str = env!("CARGO_MANIFEST_DIR");
const TEST_SCHEMA_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/test_schemas");
const TEST_SCHEMA_STORE_FILENAME: &str = "test-schemas.json.gz";

static STORE_GZ_PATH: OnceLock<PathBuf> = OnceLock::new();

pub fn initialize() -> PathBuf {
    let path = _get_store_gz_path();
    test_schema_store().to_file(Some(&path)).unwrap();
    path
}

pub fn test_schema_store() -> Store {
    Store::new_from_paths(read_test_schema_paths()).unwrap()
}

pub fn write_store_file(path: &Path) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).unwrap();
    }
    test_schema_store()
        .to_file(Some(&path.to_path_buf()))
        .unwrap();
}

fn _get_store_gz_path() -> PathBuf {
    PathBuf::from(CRATE_DIR)
        .join("tmp")
        .join(TEST_SCHEMA_STORE_FILENAME)
}

fn read_test_schema_paths() -> HashMap<String, PathBuf> {
    std::fs::read_dir(TEST_SCHEMA_DIR)
        .unwrap()
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| {
            path.extension()
                .is_some_and(|extension| extension == OsStr::new("yml"))
        })
        .map(|schema_path| (schema_name(&schema_path), schema_path))
        .collect()
}

fn schema_name(path: &Path) -> String {
    path.file_name()
        .and_then(OsStr::to_str)
        .and_then(|filename| filename.strip_suffix(".schema.yml"))
        .unwrap()
        .to_owned()
}

pub fn get_store_gz_path() -> &'static PathBuf {
    STORE_GZ_PATH.get_or_init(initialize)
}
