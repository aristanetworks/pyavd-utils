// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use criterion::{Criterion, criterion_group, criterion_main};
use pyvalidation::validation::{get_validated_data, init_store_from_file};
use test_schema_store::get_store_gz_path;

const TEST_DATA: &str = "{'fabric_name': 'foo', 'type': 123}";

pub fn benchmark_init_store_from_file(c: &mut Criterion) {
    let schema_file = get_store_gz_path();
    let mut group = c.benchmark_group("sample-size-10");
    group.sample_size(10); // Lowering the sample size from the default 1000 since tests in this group are expected to take longer.
    group.bench_function("init_store_from_fragments", |b| {
        b.iter(|| init_store_from_file(schema_file.to_owned()))
    });
    group.finish();
}

pub fn benchmark_get_validated_data(c: &mut Criterion) {
    c.bench_function("get_validated_data", |b| {
        pyo3::Python::initialize();
        pyo3::Python::attach(|py| b.iter(|| get_validated_data(py, TEST_DATA, "eos_designs")))
    });
}

criterion_group!(
    benches,
    benchmark_init_store_from_file,
    benchmark_get_validated_data
);
criterion_main!(benches);
