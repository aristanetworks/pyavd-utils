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

fn resolved_store() -> Option<Store> {
    Store::from_file(Some(get_store_gz_path()))
        .ok()?
        .as_resolved()
        .ok()
}

fn benchmark_load_and_resolve_store(criterion: &mut Criterion) {
    let schema_file = get_store_gz_path();
    criterion.bench_function("avdschema/load_and_resolve_store", |bencher| {
        bencher.iter(|| {
            let loaded = Store::from_file(Some(black_box(schema_file)));
            black_box(loaded.map(Store::as_resolved))
        });
    });
}

fn benchmark_get_list_primary_key(criterion: &mut Criterion) {
    let Some(store) = resolved_store() else {
        return;
    };
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
