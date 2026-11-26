// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Benchmarks separating validation from JSON and YAML parsing costs.

use std::hint::black_box;

use avdschema::Load as _;
use avdschema::Store;
use avdschema::StoreSource;
use criterion::Criterion;
use test_schema_store::get_store_path;
use validation::Configuration;
use validation::StoreValidate as _;
use validation::StoreValidateInput as _;

const INTERFACE_COUNT: usize = 256;

fn eos_config_json(interface_count: usize, string_descriptions: bool) -> String {
    let mut data = String::from("{\"ethernet_interfaces\":[");
    for interface_index in 1..=interface_count {
        if interface_index > 1 {
            data.push(',');
        }
        data.push_str("{\"name\":\"Ethernet");
        data.push_str(&interface_index.to_string());
        data.push_str("\",\"description\":");
        if string_descriptions {
            data.push('"');
        }
        data.push_str(&(10_000 + interface_index).to_string());
        if string_descriptions {
            data.push('"');
        }
        data.push('}');
    }
    data.push_str("]}");
    data
}

fn eos_config_yaml(interface_count: usize, string_descriptions: bool) -> String {
    let mut data = String::from("ethernet_interfaces:\n");
    for interface_index in 1..=interface_count {
        data.push_str("  - name: Ethernet");
        data.push_str(&interface_index.to_string());
        data.push_str("\n    description: ");
        if string_descriptions {
            data.push('"');
        }
        data.push_str(&(10_000 + interface_index).to_string());
        if string_descriptions {
            data.push('"');
        }
        data.push('\n');
    }
    data
}

#[expect(
    clippy::expect_used,
    reason = "benchmark setup must fail loudly when the schema fixture is invalid"
)]
fn compiled_store() -> Store {
    let source =
        StoreSource::from_file(Some(get_store_path())).expect("benchmark schema source must load");
    Store::compile(&source).expect("benchmark schema store must compile")
}

fn expected_coerced_json(json_value: &serde_json::Value) -> serde_json::Value {
    let mut expected_json = json_value.clone();
    if let Some(interfaces) = expected_json
        .get_mut("ethernet_interfaces")
        .and_then(serde_json::Value::as_array_mut)
    {
        for interface in interfaces {
            if let Some(description) = interface.get_mut("description")
                && !description.is_string()
            {
                *description = serde_json::Value::String(description.to_string());
            }
        }
    }
    expected_json
}

fn check_diagnostics(result: &validation::ValidationResult, expected_infos: usize) {
    assert!(
        result.errors.is_empty(),
        "benchmark validation errors: {:?}",
        result.errors
    );
    assert_eq!(
        result.infos.len(),
        expected_infos,
        "benchmark must exercise the expected coercion path"
    );
}

#[expect(
    clippy::expect_used,
    reason = "benchmark setup must fail loudly when generated fixtures are invalid"
)]
fn benchmark_validation(criterion: &mut Criterion) {
    let store = compiled_store();
    for (string_descriptions, case) in [(false, ""), (true, "valid_strings/")] {
        let json_input = eos_config_json(INTERFACE_COUNT, string_descriptions);
        let yaml_input = eos_config_yaml(INTERFACE_COUNT, string_descriptions);
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

        // Check semantics outside measurement so cheap error paths cannot look like improvements.
        let expected_json = expected_coerced_json(&json_value);
        for configuration in [&validate_only, &return_coerced] {
            let expected_infos =
                usize::from(configuration.return_coercion_infos && !string_descriptions)
                    * INTERFACE_COUNT;
            let parsed_json = store
                .validate_json(&json_input, "eos_config", Some(configuration))
                .expect("JSON benchmark validation must succeed");
            assert!(
                parsed_json.input_diagnostics.is_empty(),
                "JSON benchmark input diagnostics: {:?}",
                parsed_json.input_diagnostics
            );
            let parsed_yaml = store
                .validate_yaml(&yaml_input, "eos_config", Some(configuration))
                .expect("YAML benchmark validation must succeed");
            assert!(
                parsed_yaml.input_diagnostics.is_empty(),
                "YAML benchmark input diagnostics: {:?}",
                parsed_yaml.input_diagnostics
            );
            assert_eq!(
                parsed_yaml.documents.len(),
                1,
                "YAML benchmark must validate one document"
            );
            let preparsed_json = store
                .validate_value(&json_value, "eos_config", Some(configuration))
                .expect("preparsed JSON benchmark validation must succeed");
            let preparsed_yaml = store
                .validate_value(&yaml_value, "eos_config", Some(configuration))
                .expect("preparsed YAML benchmark validation must succeed");
            for output in [&parsed_json.document, &preparsed_json] {
                check_diagnostics(&output.result, expected_infos);
                assert_eq!(
                    output.coerced.as_ref(),
                    configuration.return_coerced_data.then_some(&expected_json),
                    "JSON benchmark coercion must match"
                );
            }
            for output in parsed_yaml
                .documents
                .iter()
                .chain(std::iter::once(&preparsed_yaml))
            {
                check_diagnostics(&output.result, expected_infos);
                assert_eq!(output.coerced.is_some(), configuration.return_coerced_data);
                if let Some(coerced) = &output.coerced {
                    let coerced_json = serde_json::to_value(&coerced.value)
                        .expect("coerced YAML benchmark output must convert to JSON");
                    assert_eq!(
                        coerced_json, expected_json,
                        "YAML benchmark coercion must match"
                    );
                }
            }
        }

        benchmark_json_validation(
            criterion,
            &store,
            &json_value,
            &json_input,
            &validate_only,
            &return_coerced,
            case,
        );
        benchmark_yaml_validation(
            criterion,
            &store,
            &yaml_value,
            &yaml_input,
            &validate_only,
            &return_coerced,
            case,
        );
    }
}

fn benchmark_json_validation(
    criterion: &mut Criterion,
    store: &Store,
    json_value: &serde_json::Value,
    json_input: &str,
    validate_only: &Configuration,
    return_coerced: &Configuration,
    case: &str,
) {
    criterion.bench_function(
        &format!("validation/json/{case}preparsed_validate_only"),
        |bencher| {
            bencher.iter(|| {
                black_box(store.validate_value(
                    black_box(json_value),
                    black_box("eos_config"),
                    black_box(Some(validate_only)),
                ))
            });
        },
    );
    criterion.bench_function(
        &format!("validation/json/{case}preparsed_return_coerced"),
        |bencher| {
            bencher.iter(|| {
                black_box(store.validate_value(
                    black_box(json_value),
                    black_box("eos_config"),
                    black_box(Some(return_coerced)),
                ))
            });
        },
    );
    criterion.bench_function(
        &format!("validation/json/{case}parse_and_validate"),
        |bencher| {
            bencher.iter(|| {
                black_box(store.validate_json(
                    black_box(json_input),
                    black_box("eos_config"),
                    black_box(Some(validate_only)),
                ))
            });
        },
    );
    criterion.bench_function(
        &format!("validation/json/{case}parse_validate_and_coerce"),
        |bencher| {
            bencher.iter(|| {
                black_box(store.validate_json(
                    black_box(json_input),
                    black_box("eos_config"),
                    black_box(Some(return_coerced)),
                ))
            });
        },
    );
}

fn benchmark_yaml_validation(
    criterion: &mut Criterion,
    store: &Store,
    yaml_value: &yaml_parser::Node<'_>,
    yaml_input: &str,
    validate_only: &Configuration,
    return_coerced: &Configuration,
    case: &str,
) {
    criterion.bench_function(
        &format!("validation/yaml/{case}preparsed_validate_only"),
        |bencher| {
            bencher.iter(|| {
                black_box(store.validate_value(
                    black_box(yaml_value),
                    black_box("eos_config"),
                    black_box(Some(validate_only)),
                ))
            });
        },
    );
    criterion.bench_function(
        &format!("validation/yaml/{case}preparsed_return_coerced"),
        |bencher| {
            bencher.iter(|| {
                black_box(store.validate_value(
                    black_box(yaml_value),
                    black_box("eos_config"),
                    black_box(Some(return_coerced)),
                ))
            });
        },
    );
    criterion.bench_function(
        &format!("validation/yaml/{case}parse_and_validate"),
        |bencher| {
            bencher.iter(|| {
                black_box(store.validate_yaml(
                    black_box(yaml_input),
                    black_box("eos_config"),
                    black_box(Some(validate_only)),
                ))
            });
        },
    );
    criterion.bench_function(
        &format!("validation/yaml/{case}parse_validate_and_coerce"),
        |bencher| {
            bencher.iter(|| {
                black_box(store.validate_yaml(
                    black_box(yaml_input),
                    black_box("eos_config"),
                    black_box(Some(return_coerced)),
                ))
            });
        },
    );
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
    criterion.final_summary();
}
