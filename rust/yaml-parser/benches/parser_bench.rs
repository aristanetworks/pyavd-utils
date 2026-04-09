// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Benchmarks for yaml-parser against current reference implementations.
//!
//! Run with: `cargo bench`
//!
//! This benchmark suite measures parsing throughput (MB/s) and latency
//! for various YAML document types.
//!
//! Parse-oriented groups compare:
//! - `yaml_parser`: Our parser (always includes spans)
//! - `saphyr_marked`: Saphyr with span tracking (`MarkedYaml` type)
//! - `serde_yaml`: `serde_yaml` as an additional reference parser
//!
//! The yaml-parser vs saphyr comparison is the closest parse-only comparison
//! because both include span tracking.
//!
//! When the `serde` feature is enabled on `yaml-parser`, we also compare
//! its serde-based deserialization performance against `serde_yaml`.
//!
//! To make this a fair comparison, **all** serde-based benchmarks deserialize
//! into the same logical target type: our own `yaml_parser::Value<'static>`,
//! wrapped in a small bench-only adapter `OwnedYamlValue`. That adapter
//! implements `DeserializeOwned` by first deserializing into a borrowing
//! `yaml_parser::Value<'de>` using its generic `Deserialize` impl and then
//! calling `into_owned()` to obtain `yaml_parser::Value<'static>`.
//!
//! Both serde implementations in the benchmark (`yaml_parser::serde::from_str`
//! and `serde_yaml`) therefore build the same `OwnedYamlValue` tree.

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use saphyr::LoadableYamlNode as _;

#[cfg(feature = "serde")]
use serde::Deserialize;

/// Bench-only wrapper type that always contains an owned `yaml_parser::Value<'static>`.
///
/// This lets us use a single logical target type across different YAML
/// deserializers while satisfying `DeserializeOwned` for the public
/// `yaml_parser::serde::from_str` API.
#[cfg(feature = "serde")]
#[derive(Debug)]
struct OwnedYamlValue(
    // The inner value is only used to ensure all serde backends build the same
    // logical tree; the benchmarks never inspect it directly.
    #[allow(
        dead_code,
        reason = "inner value ensures all serde backends build the same tree"
    )]
    yaml_parser::Value<'static>,
);

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for OwnedYamlValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        // First build a borrowing `Value<'de>` using its generic serde impl,
        // then convert to an owned `Value<'static>` so the result is
        // independent of the input lifetime.
        let borrowed: yaml_parser::Value<'de> = Deserialize::deserialize(deserializer)?;
        Ok(OwnedYamlValue(borrowed.into_owned()))
    }
}

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

        // Benchmark serde_yaml as an additional reference parser using its
        // native `serde_yaml::Value` representation. This is not a strict
        // apples-to-apples comparison (serde_yaml does parse+serde in one
        // step), but it provides a useful baseline alongside yaml_parser and
        // saphyr_marked.
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

        // Benchmark yaml-parser's serde-based deserialization into our own
        // generic `OwnedYamlValue` tree. This uses the crate's only serde
        // deserializer implementation via `yaml_parser::serde::from_str`.
        group.bench_with_input(
            BenchmarkId::new("yaml_parser", name),
            input,
            |bench, data| {
                bench.iter(|| {
                    let value: OwnedYamlValue = yaml_parser::serde::from_str(data).unwrap();
                    std::hint::black_box(value);
                });
            },
        );

        // Benchmark serde_yaml deserialization into the same logical
        // `OwnedYamlValue` tree via the generic `Deserialize`
        // implementation for `OwnedYamlValue`.
        group.bench_with_input(
            BenchmarkId::new("serde_yaml", name),
            input,
            |bench, data| {
                bench.iter(|| {
                    let value: OwnedYamlValue = serde_yaml::from_str(data).unwrap();
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
