// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
//! Criterion benchmarks.
#![allow(
    clippy::unwrap_used,
    missing_docs,
    reason = "criterion_group generates an undocumented entrypoint, and benchmarks fail fast with unwrap during setup"
)]

use std::sync::OnceLock;

use criterion::Criterion;
use criterion::criterion_group;
use criterion::criterion_main;
use pyo3::types::PyAnyMethods as _;
use pyo3::types::PyDict;
use python_bindings::_bindings;
use test_schema_store::get_store_gz_path;

static INIT_PY: OnceLock<()> = OnceLock::new();
static INIT_STORE: OnceLock<()> = OnceLock::new();
static TEST_DATA: OnceLock<String> = OnceLock::new();

fn eos_config_json(interface_count: usize) -> String {
    let mut data = String::from("{\"ethernet_interfaces\":[");
    for interface_index in 1..=interface_count {
        if interface_index > 1 {
            data.push(',');
        }
        data.push_str("{\"name\":\"Ethernet");
        data.push_str(&interface_index.to_string());
        data.push_str("\",\"description\":");
        data.push_str(&(10_000 + interface_index).to_string());
        data.push('}');
    }
    data.push_str("]}");
    data
}

fn benchmark_data() -> &'static str {
    TEST_DATA.get_or_init(|| eos_config_json(256)).as_str()
}

fn setup_python_with_store() {
    INIT_PY.get_or_init(|| {
        pyo3::append_to_inittab!(_bindings);
        pyo3::Python::initialize();
    });
    INIT_STORE.get_or_init(|| {
        pyo3::Python::attach(|py| {
            let module = py
                .import("_bindings")
                .unwrap()
                .getattr("_schema_store")
                .unwrap();
            let kwargs = PyDict::new(py);
            let file = py.detach(get_store_gz_path);
            kwargs.set_item("file", file).unwrap();
            module
                .call_method("init_store_from_file", (), Some(&kwargs))
                .unwrap();
        });
    });
}

fn benchmark_get_validated_data(criterion: &mut Criterion) {
    setup_python_with_store();
    let data = benchmark_data();
    criterion.bench_function("python_bindings/get_validated_data", |bencher| {
        pyo3::Python::attach(|py| {
            let module = py
                .import("_bindings")
                .unwrap()
                .getattr("_validation")
                .unwrap();
            bencher.iter(|| {
                let kwargs = PyDict::new(py);
                kwargs
                    .set_item("data_as_json", std::hint::black_box(data))
                    .unwrap();
                kwargs.set_item("schema_name", "eos_config").unwrap();
                std::hint::black_box(
                    module
                        .call_method("get_validated_data", (), Some(&kwargs))
                        .unwrap(),
                );
            });
        });
    });
}

fn benchmark_validate_json(criterion: &mut Criterion) {
    setup_python_with_store();
    let data = benchmark_data();
    criterion.bench_function("python_bindings/validate_json", |bencher| {
        pyo3::Python::attach(|py| {
            let module = py
                .import("_bindings")
                .unwrap()
                .getattr("_validation")
                .unwrap();
            bencher.iter(|| {
                let kwargs = PyDict::new(py);
                kwargs
                    .set_item("data_as_json", std::hint::black_box(data))
                    .unwrap();
                kwargs.set_item("schema_name", "eos_config").unwrap();
                std::hint::black_box(
                    module
                        .call_method("validate_json", (), Some(&kwargs))
                        .unwrap(),
                );
            });
        });
    });
}

criterion_group!(
    benches,
    benchmark_get_validated_data,
    benchmark_validate_json
);
criterion_main!(benches);
