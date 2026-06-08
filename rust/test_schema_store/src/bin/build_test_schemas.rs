// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
#![deny(unused_crate_dependencies)]

use std::path::PathBuf;

use avdschema as _;

fn main() {
    let mut args = std::env::args().skip(1);
    let mut output_path = None;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--output" => output_path = args.next().map(PathBuf::from),
            "--help" | "-h" => {
                print_help();
                return;
            }
            _ => panic!("Unexpected argument '{arg}'. Use --help for usage."),
        }
    }

    let Some(path) = output_path else {
        panic!("--output is required. Use --help for usage.");
    };

    test_schema_store::write_store_file(&path);
}

fn print_help() {
    println!(
        "Build the pyavd-utils test schema store JSON.\n\n\
         Usage: build_test_schemas --output PATH\n\n\
         --output PATH  Write test schema store. Extension controls format: .json, .yml, or .gz."
    );
}
