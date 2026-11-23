// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Benchmarks for compiling, mapping, and querying the AVD schema store.

use std::hint::black_box;

use avdschema::Load as _;
use avdschema::Store;
use avdschema::StoreSource;
use criterion::Criterion;
use test_schema_store::get_store_path;

#[expect(
    clippy::expect_used,
    reason = "benchmark setup must fail loudly when the schema fixture is invalid"
)]
fn compiled_store() -> Store {
    let source =
        StoreSource::from_file(Some(get_store_path())).expect("benchmark schema source must load");
    Store::compile(&source).expect("benchmark schema store must compile")
}

#[expect(
    clippy::expect_used,
    reason = "the benchmark must fail rather than measure schema load or resolution errors"
)]
fn benchmark_compile_store(criterion: &mut Criterion) {
    let source =
        StoreSource::from_file(Some(get_store_path())).expect("benchmark schema source must load");
    criterion.bench_function("avdschema/compile_store", |bencher| {
        bencher.iter(|| {
            black_box(
                Store::compile(black_box(&source)).expect("benchmark schema store must compile"),
            )
        });
    });
    let archive = std::path::PathBuf::from(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../test_schema_store/tmp"
    ))
    .join(format!("codspeed-schema-{}.rkyv", std::process::id()));
    Store::compile_to_file(&source, &archive).expect("benchmark archive must be written");
    let store = Store::from_file(&archive).expect("benchmark archive must load");
    assert!(
        store.get("eos_config").is_some(),
        "benchmark archive must contain eos_config"
    );
    drop(store);
    criterion.bench_function("avdschema/map_archive", |bencher| {
        bencher.iter(|| {
            black_box(Store::from_file(black_box(&archive)).expect("benchmark archive must load"))
        });
    });
    std::fs::remove_file(&archive).expect("benchmark archive must be removed");
}

fn benchmark_get_list_primary_key(criterion: &mut Criterion) {
    let store = compiled_store();
    let data_path = vec!["ethernet_interfaces".to_owned()];

    assert!(
        matches!(store.get_schema_info("eos_config", &data_path),
        Ok(Some(info)) if info.primary_key == Some("name")),
        "benchmark list must have primary key name"
    );
    criterion.bench_function("avdschema/get_list_primary_key", |bencher| {
        bencher.iter(|| {
            black_box(store.get_schema_info(black_box("eos_config"), black_box(&data_path)))
        });
    });
}

fn run_benchmarks(criterion: &mut Criterion) {
    benchmark_compile_store(criterion);
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
    criterion.final_summary();
}
