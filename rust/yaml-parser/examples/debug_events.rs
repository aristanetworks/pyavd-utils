// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

#![allow(clippy::print_stdout, reason = "examples are meant to print to stdout")]
#![allow(
    clippy::use_debug,
    reason = "Debug formatting is the purpose of this example"
)]

use yaml_parser::parse;

fn main() {
    let yaml = "- &anchor hello\n- *anchor\n";
    let (docs, errors) = parse(yaml);

    println!("YAML: {yaml:?}");
    println!("Docs: {docs:#?}");
    println!("Errors: {errors:?}");
}
