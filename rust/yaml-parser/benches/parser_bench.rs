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
//! - `yaml_serde`: `yaml_serde` as an additional reference parser
//!
//! The yaml-parser vs saphyr comparison is the closest parse-only comparison
//! because both include span tracking.
//!
//! When the `serde` feature is enabled on `yaml-parser`, we also compare
//! its serde-based deserialization performance against `yaml_serde`.
//!
//! To make this a fair comparison, **all** serde-based benchmarks deserialize
//! into the same logical target type: our own `yaml_parser::Value<'static>`,
//! wrapped in a small bench-only adapter `OwnedYamlValue`. That adapter
//! implements `DeserializeOwned` by first deserializing into a borrowing
//! `yaml_parser::Value<'de>` using its generic `Deserialize` impl and then
//! calling `into_owned()` to obtain `yaml_parser::Value<'static>`.
//!
//! Both serde implementations in the benchmark (`yaml_parser::serde::from_str`
//! and `yaml_serde`) therefore build the same `OwnedYamlValue` tree.

use std::hint::black_box;
use std::time::Duration;

use criterion::BatchSize;
use criterion::BenchmarkId;
use criterion::Criterion;
use criterion::Throughput;
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

fn eos_config_yaml(interface_count: usize) -> String {
    let mut data = String::from("ethernet_interfaces:\n");
    for interface_index in 1..=interface_count {
        data.push_str("  - name: Ethernet");
        data.push_str(&interface_index.to_string());
        data.push_str("\n    description: benchmark interface ");
        data.push_str(&interface_index.to_string());
        data.push('\n');
    }
    data
}

fn bench_yaml_parser_parse(bench: &mut criterion::Bencher<'_>, input: &str) {
    bench.iter(|| {
        let value = yaml_parser::parse(black_box(input));
        let _ = black_box(value);
    });
}

fn bench_parsed_yaml_parse(bench: &mut criterion::Bencher<'_>, input: &str) {
    // The caller transfers an existing String; acquiring that buffer is setup.
    bench.iter_batched(
        || input.to_owned(),
        |input| {
            // Match the owned parser benchmark, which also includes AST teardown.
            drop(black_box(yaml_parser::ParsedYaml::parse(black_box(input))));
        },
        BatchSize::SmallInput,
    );
}

fn checked_parsers(input: &str) -> yaml_parser::ParsedYaml {
    let (documents, errors) = yaml_parser::parse(input);
    assert!(
        errors.is_empty(),
        "owned benchmark parse errors: {errors:?}"
    );
    let parsed = yaml_parser::ParsedYaml::parse(input.to_owned());
    assert!(
        parsed.errors().is_empty(),
        "borrowing benchmark parse errors: {:?}",
        parsed.errors()
    );
    assert_eq!(
        parsed.documents().len(),
        documents.len(),
        "benchmark document counts must agree"
    );
    for (borrowing, owned) in parsed.documents().iter().zip(&documents) {
        check_equivalent_nodes(borrowing, owned);
    }
    parsed
}

fn check_equivalent_nodes(borrowing: &yaml_parser::Node<'_>, owned: &yaml_parser::Node<'_>) {
    use yaml_parser::Value;

    assert_eq!(borrowing.properties, owned.properties);
    assert_eq!(borrowing.span, owned.span);
    assert_eq!(borrowing.source, owned.source);
    assert_eq!(borrowing.trailing_comment, owned.trailing_comment);
    match (&borrowing.value, &owned.value) {
        // The tags corpus contains NaN, which is unequal to itself under PartialEq.
        (Value::Float(left), Value::Float(right)) if left.is_nan() && right.is_nan() => {}
        (Value::Sequence(left), Value::Sequence(right)) => {
            assert_eq!(
                left.len(),
                right.len(),
                "benchmark sequence lengths must agree"
            );
            for (left_item, right_item) in left.iter().zip(right) {
                assert_eq!(left_item.item_span, right_item.item_span);
                assert_eq!(left_item.header_comment, right_item.header_comment);
                check_equivalent_nodes(&left_item.node, &right_item.node);
            }
        }
        (Value::Mapping(left), Value::Mapping(right)) => {
            assert_eq!(
                left.len(),
                right.len(),
                "benchmark mapping lengths must agree"
            );
            for (left_pair, right_pair) in left.iter().zip(right) {
                assert_eq!(left_pair.pair_span, right_pair.pair_span);
                assert_eq!(left_pair.header_comment, right_pair.header_comment);
                check_equivalent_nodes(&left_pair.key, &right_pair.key);
                check_equivalent_nodes(&left_pair.value, &right_pair.value);
            }
        }
        _ => assert_eq!(
            borrowing.value, owned.value,
            "benchmark scalar values must agree"
        ),
    }
}

fn bench_saphyr_marked_parse(bench: &mut criterion::Bencher<'_>, input: &str) {
    bench.iter(|| {
        let value = saphyr::MarkedYaml::load_from_str(black_box(input));
        let _ = black_box(value);
    });
}

#[expect(
    clippy::unwrap_used,
    reason = "benchmark corpora must remain valid yaml_serde input; panic preserves that precondition"
)]
fn bench_yaml_serde_value_parse(bench: &mut criterion::Bencher<'_>, input: &str) {
    bench.iter(|| {
        let value: yaml_serde::Value = yaml_serde::from_str(black_box(input)).unwrap();
        black_box(value);
    });
}

fn input_throughput(input: &str) -> Throughput {
    Throughput::Bytes(u64::try_from(input.len()).unwrap_or(u64::MAX))
}

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
        checked_parsers(input);
        group.throughput(input_throughput(input));

        // Benchmark yaml-parser (always includes spans)
        group.bench_with_input(
            BenchmarkId::new("yaml_parser", name),
            input,
            |bench, data| bench_yaml_parser_parse(bench, data),
        );

        group.bench_with_input(
            BenchmarkId::new("parsed_yaml", name),
            input,
            |bench, data| bench_parsed_yaml_parse(bench, data),
        );

        // Benchmark saphyr with span tracking (fair comparison since yaml_parser always has spans)
        group.bench_with_input(
            BenchmarkId::new("saphyr_marked", name),
            input,
            |bench, data| bench_saphyr_marked_parse(bench, data),
        );

        // Benchmark yaml_serde as an additional reference parser using its
        // native `yaml_serde::Value` representation. This is not a strict
        // apples-to-apples comparison (yaml_serde does parse+serde in one
        // step), but it provides a useful baseline alongside yaml_parser and
        // saphyr_marked.
        group.bench_with_input(
            BenchmarkId::new("yaml_serde", name),
            input,
            |bench, data| bench_yaml_serde_value_parse(bench, data),
        );
    }

    group.finish();
}

/// Benchmark parsing latency (time per parse) for various document sizes.
fn bench_parse_latency(criterion: &mut Criterion) {
    // Small document
    let small = "key: value\n";
    let mut smaller_group = criterion.benchmark_group("parse_latency");
    smaller_group.measurement_time(Duration::from_secs(15));
    smaller_group.sample_size(150);
    smaller_group.bench_function("yaml_parser/small", |bench| {
        bench_yaml_parser_parse(bench, small);
    });
    smaller_group.bench_function("saphyr_marked/small", |bench| {
        bench_saphyr_marked_parse(bench, small);
    });
    smaller_group.finish();

    // Medium document
    let medium = NESTED_MAPPING;
    // Large document (combine multiple files)
    let large = format!("{LARGE_MAPPING}\n---\n{LARGE_SEQUENCE}\n---\n{BLOCK_SCALARS}");
    let mut larger_group = criterion.benchmark_group("parse_latency");
    larger_group.bench_function("yaml_parser/medium", |bench| {
        bench_yaml_parser_parse(bench, medium);
    });
    larger_group.bench_function("saphyr_marked/medium", |bench| {
        bench_saphyr_marked_parse(bench, medium);
    });
    larger_group.bench_function("yaml_parser/large", |bench| {
        bench_yaml_parser_parse(bench, &large);
    });
    larger_group.bench_function("saphyr_marked/large", |bench| {
        bench_saphyr_marked_parse(bench, &large);
    });

    larger_group.finish();
}

/// Benchmark parser scaling on deterministic AVD-like interface data.
fn bench_avd_scale(criterion: &mut Criterion) {
    let mut group = criterion.benchmark_group("avd_scale");

    for interface_count in [10, 100, 1_000] {
        let input = eos_config_yaml(interface_count);
        let parsed = checked_parsers(&input);
        assert_eq!(
            parsed.documents().len(),
            1,
            "expected one benchmark document"
        );
        let actual_count = parsed
            .documents()
            .first()
            .and_then(|document| {
                let yaml_parser::Value::Mapping(pairs) = &document.value else {
                    return None;
                };
                pairs.iter().find_map(|pair| {
                    if matches!(&pair.key.value, yaml_parser::Value::String(key) if key == "ethernet_interfaces")
                        && let yaml_parser::Value::Sequence(items) = &pair.value.value
                    {
                        Some(items.len())
                    } else {
                        None
                    }
                })
            });
        assert_eq!(
            actual_count,
            Some(interface_count),
            "benchmark parser must retain every generated interface"
        );
        group.throughput(input_throughput(&input));
        group.bench_with_input(
            BenchmarkId::new("yaml_parser", format!("{interface_count}_interfaces")),
            &input,
            |bench, data| bench_yaml_parser_parse(bench, data),
        );
        group.bench_with_input(
            BenchmarkId::new("parsed_yaml", format!("{interface_count}_interfaces")),
            &input,
            |bench, data| bench_parsed_yaml_parse(bench, data),
        );
    }

    group.finish();
}

/// Benchmark specific scalar types to identify performance characteristics.
fn bench_scalar_types(criterion: &mut Criterion) {
    // Plain scalars (zero-copy opportunity)
    let plain = "key1: plain_value\nkey2: another_plain\nkey3: yet_another\n";
    // Double-quoted scalars (escape processing)
    let double_quoted = r#"key1: "with \"escapes\""
key2: "newline\nhere"
key3: "tab\there"
"#;
    checked_parsers(plain);
    checked_parsers(double_quoted);
    checked_parsers(BLOCK_SCALARS);
    let mut smaller_group = criterion.benchmark_group("scalar_types");
    smaller_group.measurement_time(Duration::from_secs(15));
    smaller_group.sample_size(150);
    smaller_group.bench_function("yaml_parser/plain", |bench| {
        bench_yaml_parser_parse(bench, plain);
    });
    smaller_group.bench_function("parsed_yaml/plain", |bench| {
        bench_parsed_yaml_parse(bench, plain);
    });
    smaller_group.bench_function("saphyr_marked/plain", |bench| {
        bench_saphyr_marked_parse(bench, plain);
    });
    smaller_group.bench_function("yaml_parser/double_quoted", |bench| {
        bench_yaml_parser_parse(bench, double_quoted);
    });
    smaller_group.bench_function("parsed_yaml/double_quoted", |bench| {
        bench_parsed_yaml_parse(bench, double_quoted);
    });
    smaller_group.bench_function("saphyr_marked/double_quoted", |bench| {
        bench_saphyr_marked_parse(bench, double_quoted);
    });
    smaller_group.finish();

    // Block scalars
    let mut block_group = criterion.benchmark_group("scalar_types");
    block_group.bench_function("yaml_parser/block_scalars", |bench| {
        bench_yaml_parser_parse(bench, BLOCK_SCALARS);
    });
    block_group.bench_function("parsed_yaml/block_scalars", |bench| {
        bench_parsed_yaml_parse(bench, BLOCK_SCALARS);
    });
    block_group.bench_function("saphyr_marked/block_scalars", |bench| {
        bench_saphyr_marked_parse(bench, BLOCK_SCALARS);
    });

    block_group.finish();
}

/// Benchmark serde-based deserialization throughput for different document
/// types when the `serde` feature is enabled.
#[cfg(feature = "serde")]
#[expect(
    clippy::unwrap_used,
    reason = "benchmark corpora must deserialize successfully; panic preserves that precondition"
)]
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
        group.throughput(input_throughput(input));

        // Benchmark yaml-parser's serde-based deserialization into our own
        // generic `OwnedYamlValue` tree. This uses the crate's only serde
        // deserializer implementation via `yaml_parser::serde::from_str`.
        group.bench_with_input(
            BenchmarkId::new("yaml_parser", name),
            input,
            |bench, data| {
                bench.iter(|| {
                    let value: OwnedYamlValue =
                        yaml_parser::serde::from_str(black_box(data)).unwrap();
                    black_box(value);
                });
            },
        );

        // Benchmark yaml_serde deserialization into the same logical
        // `OwnedYamlValue` tree via the generic `Deserialize`
        // implementation for `OwnedYamlValue`.
        group.bench_with_input(
            BenchmarkId::new("yaml_serde", name),
            input,
            |bench, data| {
                bench.iter(|| {
                    let value: OwnedYamlValue = yaml_serde::from_str(black_box(data)).unwrap();
                    black_box(value);
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

fn run_benchmarks(criterion: &mut Criterion) {
    bench_parse_throughput(criterion);
    bench_parse_latency(criterion);
    bench_avd_scale(criterion);
    bench_scalar_types(criterion);
    bench_serde_deserialize_throughput(criterion);
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
