// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

// Simple profiling harness for serde deserialization
// Run with: cargo flamegraph --example profile_serde --features serde --release
// Or: cargo build --release --example profile_serde --features serde
//     perf record -g ./target/release/examples/profile_serde
//     perf report

use yaml_parser::Value;

fn main() {
    // Use the large_mapping benchmark data
    let input = include_str!("../benches/data/large_mapping.yml");

    // Run many iterations to get a good profile
    let mut total_keys = 0;
    for _ in 0..5000 {
        let (docs, _errors) = yaml_parser::parse(input);
        for doc in docs {
            if let Value::Mapping(ref map) = doc.value {
                total_keys += map.len();
            }
        }
    }

    println!("Profiling complete: {} total keys processed", total_keys);
}
