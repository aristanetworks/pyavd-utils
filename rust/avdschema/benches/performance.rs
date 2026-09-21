// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Benchmarks for loading, resolving, and querying the AVD schema store.

use std::hint::black_box;

use avdschema::Load as _;
use avdschema::Store;
use avdschema::get_list_primary_key;
use criterion::Criterion;
use test_schema_store::get_store_gz_path;

#[expect(
    clippy::expect_used,
    reason = "benchmark setup must fail loudly when the schema fixture is invalid"
)]
fn resolved_store() -> Store {
    Store::from_file(Some(get_store_gz_path()))
        .expect("benchmark schema store must load")
        .as_resolved()
        .expect("benchmark schema store must resolve")
}

#[expect(
    clippy::expect_used,
    reason = "the benchmark must fail rather than measure schema load or resolution errors"
)]
fn benchmark_load_and_resolve_store(criterion: &mut Criterion) {
    let schema_file = get_store_gz_path();
    criterion.bench_function("avdschema/load_and_resolve_store", |bencher| {
        bencher.iter(|| {
            let loaded = Store::from_file(Some(black_box(schema_file)))
                .expect("benchmark schema store must load");
            black_box(
                loaded
                    .as_resolved()
                    .expect("benchmark schema store must resolve"),
            )
        });
    });
}

fn benchmark_get_list_primary_key(criterion: &mut Criterion) {
    let store = resolved_store();
    let data_path = vec!["ethernet_interfaces".to_owned()];

    criterion.bench_function("avdschema/get_list_primary_key", |bencher| {
        bencher.iter(|| {
            black_box(get_list_primary_key(
                black_box("eos_config"),
                black_box(&store),
                black_box(&data_path),
            ))
        });
    });
}

fn run_benchmarks(criterion: &mut Criterion) {
    benchmark_load_and_resolve_store(criterion);
    benchmark_get_list_primary_key(criterion);
}

#[cfg(codspeed)]
fn main() {
    let mut criterion = Criterion::new_instrumented();
    run_benchmarks(&mut criterion);
}

#[cfg(not(codspeed))]
fn main() {
    let mut criterion = Criterion::default().configure_from_args();
    run_benchmarks(&mut criterion);
}
