// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
//! Criterion benchmarks for schema-merge.
#![allow(
    clippy::unwrap_used,
    missing_docs,
    reason = "criterion_group generates an undocumented entrypoint, and benchmarks fail fast with unwrap during setup"
)]

use avdschema::Store;
use criterion::BatchSize;
use criterion::Criterion;
use criterion::criterion_group;
use criterion::criterion_main;
use schema_merge::ListMerge;
use schema_merge::StoreSchemaMerge as _;
use serde_json::Value;
use serde_json::json;

fn test_store() -> Store {
    serde_json::from_value(json!({
        "eos_config": {
            "type": "dict",
            "keys": {
                "ethernet_interfaces": {
                    "type": "list",
                    "primary_key": "name",
                    "items": {
                        "type": "dict",
                        "keys": {
                            "name": {"type": "str"},
                            "description": {"type": "str"},
                            "shutdown": {"type": "bool"}
                        }
                    }
                }
            }
        }
    }))
    .unwrap()
}

fn large_interface_merge_inputs() -> (Value, Value) {
    let base_interfaces: Vec<_> = (0_usize..10_000)
        .map(|index| {
            json!({
                "name": format!("Ethernet{index}"),
                "description": "base"
            })
        })
        .collect();
    let next_interfaces: Vec<_> = (5_000_usize..15_000)
        .map(|index| {
            json!({
                "name": format!("Ethernet{index}"),
                "description": "next",
                "shutdown": index.is_multiple_of(2)
            })
        })
        .collect();

    (
        json!({"ethernet_interfaces": base_interfaces}),
        json!({"ethernet_interfaces": next_interfaces}),
    )
}

fn benchmark_large_primary_key_list_merge(criterion: &mut Criterion) {
    let store = test_store();
    let (base, next) = large_interface_merge_inputs();
    criterion.bench_function("large_primary_key_list_merge", |bencher| {
        bencher.iter_batched(
            || (base.clone(), next.clone()),
            |(base, next)| {
                std::hint::black_box(
                    store
                        .merge_value(base, [next], "eos_config", ListMerge::AppendUnique)
                        .unwrap(),
                );
            },
            BatchSize::SmallInput,
        );
    });
}

criterion_group!(benches, benchmark_large_primary_key_list_merge);
criterion_main!(benches);
