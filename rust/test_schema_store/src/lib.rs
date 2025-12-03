// Copyright (c) 2025 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::path::PathBuf;

const CRATE_DIR: &str = env!("CARGO_MANIFEST_DIR");
const ADV_SCHEMA_URL: &str =
        "https://github.com/aristanetworks/avd/releases/download/v6.0.0-dev3/schemas.json.gz";

pub fn initialize() {
    let resp = reqwest::blocking::get(ADV_SCHEMA_URL).unwrap();
    let body = resp.bytes().unwrap();
    std::fs::write(get_store_gz_path(), &body).unwrap();
}

pub fn get_store_gz_path() -> PathBuf {
        let url = reqwest::Url::parse(ADV_SCHEMA_URL).unwrap();
        let url_as_path = PathBuf::from(url.path());
        let filename = url_as_path.file_name().unwrap();
        PathBuf::from(CRATE_DIR).join("tmp").join(filename)
}
