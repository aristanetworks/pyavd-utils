<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Benchmark Report

**Date:** 2026-03-20
**Parser Version:** 0.0.2

This document presents performance benchmarks comparing `yaml_parser` against
`saphyr_marked` (Saphyr with span tracking), a mature Rust YAML parser, and a
serde-based comparison against `serde_yaml`.

## Comparison Methodology

We compare two configurations:

- **`yaml_parser`**: Our parser (always includes span tracking)
- **`saphyr_marked`**: Saphyr with span tracking (`MarkedYaml` type)

Both parsers include span tracking, making this a fair "apples-to-apples" comparison.

## Running Benchmarks

```bash
cd rust/yaml-parser
cargo bench --bench parser_bench --features serde
```

To run a specific benchmark group:

```bash
cargo bench --bench parser_bench --features serde -- parse_throughput
cargo bench --bench parser_bench --features serde -- parse_latency
cargo bench --bench parser_bench --features serde -- scalar_types
cargo bench --bench parser_bench --features serde -- serde_deserialize_throughput
```

## Results Summary

The numbers below are medians from a full run of:

```bash
cargo bench -p yaml-parser --bench parser_bench --features serde
```

The full terminal capture for this run was written to:

```txt
/tmp/yaml-parser-bench-20260320-full-clean-serde.txt
```

Times are in microseconds (µs) for latency-style benchmarks and MiB/s for
throughput-style benchmarks. All serde benchmarks are configured to **panic on
failure** (via `unwrap()`), so any deserialization incompatibility will cause
the benchmark run itself to fail rather than silently skipping a dataset.

### Value types per column

- `yaml_parser_parse`: parse-only pipeline, building `yaml_parser::Value<'input>`
  via the parser/AST.
- `saphyr_marked`: Saphyr's `MarkedYaml` value with span tracking enabled.
- `yaml_parser_serde`: `yaml_parser::serde::from_str::<OwnedYamlValue>`, where
  `OwnedYamlValue` wraps `yaml_parser::Value<'static>`.
- `serde_yaml`:
  - In the `parse_throughput` group: `serde_yaml::from_str::<serde_yaml::Value>`
    (serde_yaml's native value type).
  - In the `serde_deserialize_throughput` group: `serde_yaml::from_str::<OwnedYamlValue>`
    to match the yaml_parser serde targets.

|Group|Dataset|yaml_parser_parse|saphyr_marked|yaml_parser_serde|serde_yaml|Notes|
|-----|-------|-----------------|-------------|------------------|---------|-----|
|**parse_throughput** (MiB/s)|large_mapping|23.0 MiB/s|**25.2 MiB/s**|–|24.9 MiB/s|`yaml_parser_parse` lands at about 92% of `serde_yaml` and 91% of `saphyr_marked`|
|**parse_throughput** (MiB/s)|nested_mapping|**23.8 MiB/s**|21.6 MiB/s|–|19.0 MiB/s|Best parse-throughput corpus in this run for `yaml_parser_parse`: 125% of `serde_yaml` and 110% of `saphyr_marked`|
|**parse_throughput** (MiB/s)|large_sequence|25.5 MiB/s|**33.9 MiB/s**|–|28.3 MiB/s|Long flat sequences remain a clear weak spot: `yaml_parser_parse` reaches 90% of `serde_yaml` and 75% of `saphyr_marked`|
|**parse_throughput** (MiB/s)|block_scalars|**70.5 MiB/s**|97.4 MiB/s|–|67.7 MiB/s|`yaml_parser_parse` now edges out `serde_yaml` on this corpus, but still trails `saphyr_marked`|
|**parse_throughput** (MiB/s)|flow_collections|18.2 MiB/s|**21.5 MiB/s**|–|21.0 MiB/s|Flow-heavy inputs still lag: `yaml_parser_parse` reaches 87% of `serde_yaml` and 85% of `saphyr_marked`|
|**parse_throughput** (MiB/s)|anchors_aliases|**23.0 MiB/s**|19.7 MiB/s|–|19.8 MiB/s|Strongest comparative win for `yaml_parser_parse` in this run: 116% of `serde_yaml` and 117% of `saphyr_marked`|
|**parse_throughput** (MiB/s)|tags|19.9 MiB/s|**27.0 MiB/s**|–|25.2 MiB/s|Tag-heavy parse throughput remains the weakest corpus relative to both references|
|**parse_latency** (µs)|small|**1.17 µs**|1.30 µs|–|–|`yaml_parser_parse` is slightly faster than `saphyr_marked` on the smallest latency case|
|**parse_latency** (µs)|medium|**52.3 µs**|54.3 µs|–|–|Medium-document latency also slightly favors `yaml_parser_parse`|
|**parse_latency** (µs)|large|147.8 µs|**134.4 µs**|–|–|Large-document latency still favors `saphyr_marked`|
|**scalar_types** (µs)|plain|2.61 µs|**2.49 µs**|–|–|Plain scalar latency is close, with `yaml_parser_parse` about 5% slower|
|**scalar_types** (µs)|double_quoted|3.39 µs|**2.57 µs**|–|–|Double-quoted scalar handling remains noticeably slower than `saphyr_marked`|
|**scalar_types** (µs)|block_scalars|23.1 µs|**14.3 µs**|–|–|Block-scalar microbenchmarks still show a substantial gap|
|**serde_deserialize_throughput** (MiB/s)|large_mapping|–|–|23.5 MiB/s|**23.9 MiB/s**|The yaml-parser serde API is effectively tied with `serde_yaml` here at 98% of its throughput|
|**serde_deserialize_throughput** (MiB/s)|nested_mapping|–|–|22.0 MiB/s|**22.4 MiB/s**|Nested mappings are also very close, with yaml-parser at 98% of `serde_yaml`|
|**serde_deserialize_throughput** (MiB/s)|large_sequence|–|–|23.7 MiB/s|**28.3 MiB/s**|Large sequences remain the biggest serde throughput gap at about 84% of `serde_yaml`|
|**serde_deserialize_throughput** (MiB/s)|block_scalars|–|–|**72.0 MiB/s**|66.9 MiB/s|yaml-parser serde is ahead of `serde_yaml` on this block-scalar corpus|
|**serde_deserialize_throughput** (MiB/s)|flow_collections|–|–|17.7 MiB/s|**22.6 MiB/s**|Flow collections remain the weakest serde corpus at about 78% of `serde_yaml`|
|**serde_deserialize_throughput** (MiB/s)|anchors_aliases|–|–|18.8 MiB/s|**20.1 MiB/s**|Anchors and aliases are relatively close, with yaml-parser at 93% of `serde_yaml`|
|**serde_deserialize_throughput** (MiB/s)|tags|–|–|23.0 MiB/s|**26.8 MiB/s**|Tags still trail `serde_yaml`, with yaml-parser at about 86% of the reference throughput|

### Notes on serde behaviour

For the `serde_deserialize_throughput` group:

- `yaml_parser_serde` refers to `yaml_parser::serde::from_str::<OwnedYamlValue>`,
  which uses the crate's event-driven serde deserializer.
- `serde_yaml` refers to `serde_yaml::from_str::<OwnedYamlValue>`.

Both configurations deserialize into the same logical target type,
`OwnedYamlValue(yaml_parser::Value<'static>)`. As in the other groups, all
benchmarks are invoked with `unwrap()`, so any deserialization failure or
behavioural difference will surface as a panic during the benchmark run rather
than being silently skipped.

## Test Data

Benchmarks use representative YAML files in `benches/data/`:

- `large_mapping.yml`: 100 key-value pairs
- `nested_mapping.yml`: 4-level nested structure
- `large_sequence.yml`: 100-item sequence
- `block_scalars.yml`: Multiple literal and folded block scalars
- `flow_collections.yml`: Nested flow mappings and sequences
- `anchors_aliases.yml`: Documents with anchors and alias references
- `tags.yml`: Documents with explicit tags

## Hardware Notes

Results may vary based on hardware. These benchmarks were run on a typical
development machine. For reproducible results, ensure:

- No background processes consuming CPU
- Run multiple times to verify consistency
- Use `cargo bench` (which performs statistical analysis)

## Current Takeaways

- The benchmark matrix now reflects a single serde implementation for yaml-parser:
  `yaml_parser::serde::from_str`.
- `nested_mapping` and `anchors_aliases` are the strongest parse-throughput
  corpora for yaml-parser relative to both references.
- `large_sequence`, `flow_collections`, and `tags` remain the main throughput
  gaps to watch.
- On serde throughput, yaml-parser is very close to `serde_yaml` on
  `large_mapping` and `nested_mapping`, ahead on `block_scalars`, and behind on
  `large_sequence`, `flow_collections`, `anchors_aliases`, and `tags`.
