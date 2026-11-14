// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Compare the JSON and YAML backends on large schema primary-key lists.

use std::hint::black_box;

use avdschema::Store;
use criterion::BatchSize;
use criterion::BenchmarkId;
use criterion::Criterion;
use criterion::criterion_group;
use criterion::criterion_main;
use schema_merge::MergeOptions;
use schema_merge::StoreSchema;
use schema_merge::merge;
use serde_json::json;
use yaml_parser::parse_with_source;

#[allow(
    clippy::expect_used,
    reason = "Benchmark setup and merge inputs are known valid fixtures"
)]
fn primary_key_lists(criterion: &mut Criterion) {
    let store = Store::from_json(
        &json!({"test": {"type": "dict", "keys": {
            "items": {"type": "list", "primary_key": "name", "items": {"type": "dict"}}
        }}})
        .to_string(),
    )
    .expect("valid schema");
    let schema = StoreSchema::new(&store, "test").expect("schema exists");
    let mut group = criterion.benchmark_group("primary_key_lists");
    for count in [1_000, 10_000] {
        let base = json!({"items": (0..count).map(|index| json!({
            "name": format!("Ethernet{index}"), "mtu": 1500,
        })).collect::<Vec<_>>()});
        let next = json!({"items": (0..count).map(|index| json!({
            "name": format!("Ethernet{index}"), "description": "uplink",
        })).collect::<Vec<_>>()});
        let (mut yaml_base, base_errors) =
            parse_with_source(&base.to_string(), Some("base".into()));
        let (mut yaml_next, next_errors) =
            parse_with_source(&next.to_string(), Some("next".into()));
        assert!(base_errors.is_empty() && next_errors.is_empty());
        let yaml_base = yaml_base.remove(0);
        let yaml_next = yaml_next.remove(0);
        group.bench_with_input(
            BenchmarkId::new("json", count),
            &count,
            |bencher, _count| {
                bencher.iter_batched(
                    || (base.clone(), next.clone()),
                    |(owned_base, owned_next)| {
                        black_box(
                            merge(owned_base, [owned_next], &schema, MergeOptions::default())
                                .expect("valid merge"),
                        )
                    },
                    BatchSize::SmallInput,
                );
            },
        );
        group.bench_with_input(
            BenchmarkId::new("yaml", count),
            &count,
            |bencher, _count| {
                bencher.iter_batched(
                    || (yaml_base.clone(), yaml_next.clone()),
                    |(owned_base, owned_next)| {
                        black_box(
                            merge(owned_base, [owned_next], &schema, MergeOptions::default())
                                .expect("valid merge"),
                        )
                    },
                    BatchSize::SmallInput,
                );
            },
        );
    }
    group.finish();
}

criterion_group!(benches, primary_key_lists);
criterion_main!(benches);
