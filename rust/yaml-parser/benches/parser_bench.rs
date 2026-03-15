// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Benchmarks for yaml-parser comparing against saphyr.
//!
//! Run with: `cargo bench`
//!
//! This benchmark suite measures parsing throughput (MB/s) and latency
//! for various YAML document types.
//!
//! We compare two configurations:
//! - `yaml_parser`: Our parser (always includes spans)
//! - `saphyr_marked`: Saphyr with span tracking (`MarkedYaml` type)
//!
//! Both parsers include span tracking, making this a fair comparison.
//!
//! When the `serde` feature is enabled on `yaml-parser`, we also compare
//! serde-based deserialization performance against `serde_yaml`:
//! - `yaml_parser::serde::from_str::<yaml_parser::Value<'static>>`
//! - `serde_yaml::from_str::<serde_yaml::Value>`

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use saphyr::LoadableYamlNode as _;

// Test data files included at compile time
const LARGE_MAPPING: &str = include_str!("data/large_mapping.yml");
const NESTED_MAPPING: &str = include_str!("data/nested_mapping.yml");
const LARGE_SEQUENCE: &str = include_str!("data/large_sequence.yml");
const BLOCK_SCALARS: &str = include_str!("data/block_scalars.yml");
const FLOW_COLLECTIONS: &str = include_str!("data/flow_collections.yml");
const ANCHORS_ALIASES: &str = include_str!("data/anchors_aliases.yml");
const TAGS: &str = include_str!("data/tags.yml");

/// Benchmark parsing throughput for different document types.
fn bench_parse_throughput(criterion: &mut Criterion) {
    let test_cases: &[(&str, &str)] = &[
        ("large_mapping", LARGE_MAPPING),
        ("nested_mapping", NESTED_MAPPING),
        ("large_sequence", LARGE_SEQUENCE),
        ("block_scalars", BLOCK_SCALARS),
        ("flow_collections", FLOW_COLLECTIONS),
        ("anchors_aliases", ANCHORS_ALIASES),
        ("tags", TAGS),
    ];

    let mut group = criterion.benchmark_group("parse_throughput");

    for (name, input) in test_cases {
        group.throughput(Throughput::Bytes(u64::try_from(input.len()).unwrap()));

        // Benchmark yaml-parser (always includes spans)
        group.bench_with_input(
            BenchmarkId::new("yaml_parser", name),
            input,
            |bench, data| {
                bench.iter(|| yaml_parser::parse(data));
            },
        );

        // Benchmark saphyr with span tracking (fair comparison since yaml_parser always has spans)
        group.bench_with_input(
            BenchmarkId::new("saphyr_marked", name),
            input,
            |bench, data| {
                bench.iter(|| saphyr::MarkedYaml::load_from_str(data));
            },
        );
    }

    group.finish();
}

/// Benchmark parsing latency (time per parse) for various document sizes.
fn bench_parse_latency(criterion: &mut Criterion) {
    let mut group = criterion.benchmark_group("parse_latency");

    // Small document
    let small = "key: value\n";
    group.bench_function("yaml_parser/small", |bench| {
        bench.iter(|| yaml_parser::parse(small));
    });
    group.bench_function("saphyr_marked/small", |bench| {
        bench.iter(|| saphyr::MarkedYaml::load_from_str(small));
    });

    // Medium document
    let medium = NESTED_MAPPING;
    group.bench_function("yaml_parser/medium", |bench| {
        bench.iter(|| yaml_parser::parse(medium));
    });
    group.bench_function("saphyr_marked/medium", |bench| {
        bench.iter(|| saphyr::MarkedYaml::load_from_str(medium));
    });

    // Large document (combine multiple files)
    let large = format!("{LARGE_MAPPING}\n---\n{LARGE_SEQUENCE}\n---\n{BLOCK_SCALARS}");
    group.bench_function("yaml_parser/large", |bench| {
        bench.iter(|| yaml_parser::parse(&large));
    });
    group.bench_function("saphyr_marked/large", |bench| {
        bench.iter(|| saphyr::MarkedYaml::load_from_str(&large));
    });

    group.finish();
}

/// Benchmark specific scalar types to identify performance characteristics.
fn bench_scalar_types(criterion: &mut Criterion) {
    let mut group = criterion.benchmark_group("scalar_types");

    // Plain scalars (zero-copy opportunity)
    let plain = "key1: plain_value\nkey2: another_plain\nkey3: yet_another\n";
    group.bench_function("yaml_parser/plain", |bench| {
        bench.iter(|| yaml_parser::parse(plain));
    });
    group.bench_function("saphyr_marked/plain", |bench| {
        bench.iter(|| saphyr::MarkedYaml::load_from_str(plain));
    });

    // Double-quoted scalars (escape processing)
    let double_quoted = r#"key1: "with \"escapes\""
key2: "newline\nhere"
key3: "tab\there"
"#;
    group.bench_function("yaml_parser/double_quoted", |bench| {
        bench.iter(|| yaml_parser::parse(double_quoted));
    });
    group.bench_function("saphyr_marked/double_quoted", |bench| {
        bench.iter(|| saphyr::MarkedYaml::load_from_str(double_quoted));
    });

    // Block scalars
    group.bench_function("yaml_parser/block_scalars", |bench| {
        bench.iter(|| yaml_parser::parse(BLOCK_SCALARS));
    });
    group.bench_function("saphyr_marked/block_scalars", |bench| {
        bench.iter(|| saphyr::MarkedYaml::load_from_str(BLOCK_SCALARS));
    });

    group.finish();
}

/// Benchmark serde-based deserialization throughput for different document
/// types when the `serde` feature is enabled.
#[cfg(feature = "serde")]
fn bench_serde_deserialize_throughput(criterion: &mut Criterion) {
    let test_cases: &[(&str, &str)] = &[
        ("large_mapping", LARGE_MAPPING),
        ("nested_mapping", NESTED_MAPPING),
        ("large_sequence", LARGE_SEQUENCE),
        ("block_scalars", BLOCK_SCALARS),
        ("flow_collections", FLOW_COLLECTIONS),
        ("anchors_aliases", ANCHORS_ALIASES),
        ("tags", TAGS),
    ];

    let mut group = criterion.benchmark_group("serde_deserialize_throughput");

    for (name, input) in test_cases {
        // We expect both libraries to successfully deserialize all benchmark
        // corpora. If either panics here, that's a behavioural regression we
        // want to see rather than silently skipping the dataset.
        group.throughput(Throughput::Bytes(u64::try_from(input.len()).unwrap()));

        // Benchmark yaml-parser's serde-based deserialization into a generic
        // serde_yaml::Value tree.
        group.bench_with_input(
            BenchmarkId::new("yaml_parser_serde", name),
            input,
            |bench, data| {
                bench.iter(|| {
                    let value: serde_yaml::Value = yaml_parser::serde::from_str(data).unwrap();
                    std::hint::black_box(value);
                });
            },
        );

        // Benchmark serde_yaml deserialization into its generic Value type.
        group.bench_with_input(
            BenchmarkId::new("serde_yaml", name),
            input,
            |bench, data| {
                bench.iter(|| {
                    let value: serde_yaml::Value = serde_yaml::from_str(data).unwrap();
                    std::hint::black_box(value);
                });
            },
        );
    }

    group.finish();
}

/// Fallback no-op version when the `serde` feature is disabled, so the
/// criterion group compiles but does not register any serde benchmarks.
#[cfg(not(feature = "serde"))]
fn bench_serde_deserialize_throughput(_criterion: &mut Criterion) {}

criterion_group!(
    benches,
    bench_parse_throughput,
    bench_parse_latency,
    bench_scalar_types,
    bench_serde_deserialize_throughput,
);
criterion_main!(benches);
