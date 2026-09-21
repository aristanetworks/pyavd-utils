// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Benchmarks separating validation from JSON and YAML parsing costs.

use std::hint::black_box;

use avdschema::Load as _;
use avdschema::Store;
use criterion::Criterion;
use test_schema_store::get_store_gz_path;
use validation::Configuration;
use validation::StoreValidate as _;
use validation::StoreValidateInput as _;

const INTERFACE_COUNT: usize = 256;

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

fn eos_config_yaml(interface_count: usize) -> String {
    let mut data = String::from("ethernet_interfaces:\n");
    for interface_index in 1..=interface_count {
        data.push_str("  - name: Ethernet");
        data.push_str(&interface_index.to_string());
        data.push_str("\n    description: ");
        data.push_str(&(10_000 + interface_index).to_string());
        data.push('\n');
    }
    data
}

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
    reason = "benchmark setup must fail loudly when generated fixtures are invalid"
)]
fn benchmark_validation(criterion: &mut Criterion) {
    let store = resolved_store();
    let json_input = eos_config_json(INTERFACE_COUNT);
    let yaml_input = eos_config_yaml(INTERFACE_COUNT);
    let json_value = serde_json::from_str::<serde_json::Value>(&json_input)
        .expect("generated benchmark JSON must parse");
    let (yaml_documents, yaml_errors) = yaml_parser::parse(&yaml_input);
    assert!(
        yaml_errors.is_empty(),
        "generated benchmark YAML must parse without errors: {yaml_errors:?}"
    );
    let yaml_value = yaml_documents
        .into_iter()
        .next()
        .expect("generated benchmark YAML must contain one document");

    let validate_only = Configuration::default();
    let return_coerced = Configuration {
        return_coerced_data: true,
        return_coercion_infos: true,
        ..Default::default()
    };

    criterion.bench_function("validation/json/preparsed_validate_only", |bencher| {
        bencher.iter(|| {
            black_box(store.validate_value(
                black_box(&json_value),
                black_box("eos_config"),
                black_box(Some(&validate_only)),
            ))
        });
    });
    criterion.bench_function("validation/json/preparsed_return_coerced", |bencher| {
        bencher.iter(|| {
            black_box(store.validate_value(
                black_box(&json_value),
                black_box("eos_config"),
                black_box(Some(&return_coerced)),
            ))
        });
    });
    criterion.bench_function("validation/json/parse_and_validate", |bencher| {
        bencher.iter(|| {
            black_box(store.validate_json(
                black_box(&json_input),
                black_box("eos_config"),
                black_box(Some(&validate_only)),
            ))
        });
    });
    criterion.bench_function("validation/json/parse_validate_and_coerce", |bencher| {
        bencher.iter(|| {
            black_box(store.validate_json(
                black_box(&json_input),
                black_box("eos_config"),
                black_box(Some(&return_coerced)),
            ))
        });
    });

    criterion.bench_function("validation/yaml/preparsed_validate_only", |bencher| {
        bencher.iter(|| {
            black_box(store.validate_value(
                black_box(&yaml_value),
                black_box("eos_config"),
                black_box(Some(&validate_only)),
            ))
        });
    });
    criterion.bench_function("validation/yaml/preparsed_return_coerced", |bencher| {
        bencher.iter(|| {
            black_box(store.validate_value(
                black_box(&yaml_value),
                black_box("eos_config"),
                black_box(Some(&return_coerced)),
            ))
        });
    });
    criterion.bench_function("validation/yaml/parse_and_validate", |bencher| {
        bencher.iter(|| {
            black_box(store.validate_yaml(
                black_box(&yaml_input),
                black_box("eos_config"),
                black_box(Some(&validate_only)),
            ))
        });
    });
    criterion.bench_function("validation/yaml/parse_validate_and_coerce", |bencher| {
        bencher.iter(|| {
            black_box(store.validate_yaml(
                black_box(&yaml_input),
                black_box("eos_config"),
                black_box(Some(&return_coerced)),
            ))
        });
    });
}

fn run_benchmarks(criterion: &mut Criterion) {
    benchmark_validation(criterion);
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
