// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use yaml_parser::parse;

fn main() {
    let yaml = "- &anchor hello\n- *anchor\n";
    let (docs, errors) = parse(yaml);

    println!("YAML: {:?}", yaml);
    println!("Docs: {:#?}", docs);
    println!("Errors: {:?}", errors);
}
