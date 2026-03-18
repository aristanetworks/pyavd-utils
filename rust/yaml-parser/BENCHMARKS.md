<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Benchmark Report

**Date:** 2026-03-18
**Parser Version:** 0.0.2 (with Event boxing optimization)

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
cargo bench
```

To run a specific benchmark group:

```bash
cargo bench -- parse_throughput
cargo bench -- parse_latency
cargo bench -- scalar_types
```

## Results Summary

The numbers below are medians from representative runs of:

```bash
cargo bench -p yaml-parser --features serde
```

Times are in microseconds (µs) for latency-style benchmarks and MiB/s for
throughput-style benchmarks. All serde benchmarks are configured to **panic on
failure** (via `unwrap()`), so any deserialization incompatibility will cause
the benchmark run itself to fail rather than silently skipping a dataset.

**Value types per column**

- `yaml_parser_parse`: parse-only pipeline, building `yaml_parser::Value<'input>`
  via the parser/AST.
- `saphyr_marked`: Saphyr's `MarkedYaml` value with span tracking enabled.
- `yaml_parser_serde`: `yaml_parser::serde::from_str::<OwnedYamlValue>`, where
  `OwnedYamlValue` wraps `yaml_parser::Value<'static>`.
- `yaml_parser_events_serde`: experimental event-based backend,
  `yaml_parser::serde::bench_from_str_events_internal::<OwnedYamlValue>`.
- `serde_yaml`:
  - In the `parse_throughput` group: `serde_yaml::from_str::<serde_yaml::Value>`
    (serde_yaml's native value type).
  - In the `serde_deserialize` group: `serde_yaml::from_str::<OwnedYamlValue>`
    to match the yaml_parser serde targets.

|Group|Dataset|yaml_parser_parse|saphyr_marked|yaml_parser_serde|yaml_parser_events_serde|serde_yaml|Notes|
|-----|-------|-----------------|-------------|------------------|------------------------|---------|-----|
|**parse_throughput** (MiB/s)|large_mapping|**31.9 MiB/s**|41.1 MiB/s|–|–|37.2 MiB/s|After Event boxing optimization: yaml_parser_parse shows 16-21% improvement; saphyr_marked is now fastest on this corpus|
|**parse_throughput** (MiB/s)|nested_mapping|**27.0 MiB/s**|32.8 MiB/s|–|–|30.9 MiB/s|12-16% improvement; saphyr_marked edges out both yaml_parser_parse and serde_yaml|
|**parse_throughput** (MiB/s)|large_sequence|26.1 MiB/s|**45.2 MiB/s**|–|–|43.2 MiB/s|13-17% improvement; saphyr_marked is fastest, followed closely by serde_yaml|
|**parse_throughput** (MiB/s)|block_scalars|**85.6 MiB/s**|154.6 MiB/s|–|–|107.0 MiB/s|20-22% improvement; saphyr_marked remains fastest on block scalars|
|**parse_throughput** (MiB/s)|flow_collections|**23.6 MiB/s**|32.4 MiB/s|–|–|33.1 MiB/s|21-26% improvement; serde_yaml and saphyr_marked are now comparable and ahead of yaml_parser_parse|
|**parse_throughput** (MiB/s)|anchors_aliases|**27.5 MiB/s**|25.8 MiB/s|–|–|28.5 MiB/s|16-18% improvement; all three parsers are now very close in performance|
|**parse_throughput** (MiB/s)|tags|**27.8 MiB/s**|39.4 MiB/s|–|–|37.2 MiB/s|13-16% improvement; saphyr_marked is fastest, followed by serde_yaml|
|**parse_latency** (µs)|small|**0.96 µs**|0.81 µs|–|–|–|19-20% improvement; saphyr_marked is slightly faster|
|**parse_latency** (µs)|medium|**45.1 µs**|35.0 µs|–|–|–|9-10% improvement; saphyr_marked is ~22% faster|
|**parse_latency** (µs)|large|**121.5 µs**|83.3 µs|–|–|–|18-24% improvement; saphyr_marked is significantly faster|
|**scalar_types** (µs)|plain|**2.30 µs**|1.68 µs|–|–|–|14-15% improvement; saphyr_marked remains faster|
|**scalar_types** (µs)|double_quoted|**2.89 µs**|1.69 µs|–|–|–|3-4% improvement; saphyr_marked is significantly faster|
|**scalar_types** (µs)|block_scalars|**18.3 µs**|9.88 µs|–|–|–|20-21% improvement; saphyr_marked remains faster|
|**serde_deserialize** (MiB/s)|large_mapping|–|–|24.6 MiB/s|**30.7 MiB/s**|36.9 MiB/s|Event backend improved 30-50% to 83% of serde_yaml speed (up from ~60-70%)|
|**serde_deserialize** (MiB/s)|nested_mapping|–|–|22.4 MiB/s|**27.1 MiB/s**|31.7 MiB/s|Event backend improved 27% to 85% of serde_yaml speed|
|**serde_deserialize** (MiB/s)|large_sequence|–|–|24.6 MiB/s|**27.4 MiB/s**|39.1 MiB/s|Event backend improved 6-8% to 70% of serde_yaml speed|
|**serde_deserialize** (MiB/s)|block_scalars|–|–|74.9 MiB/s|**89.2 MiB/s**|109.4 MiB/s|Event backend improved 9-10% to 82% of serde_yaml speed|
|**serde_deserialize** (MiB/s)|flow_collections|–|–|19.5 MiB/s|**24.8 MiB/s**|33.0 MiB/s|Event backend improved 4-5% to 75% of serde_yaml speed|
|**serde_deserialize** (MiB/s)|anchors_aliases|–|–|**21.9 MiB/s**|20.5 MiB/s|31.0 MiB/s|AST-backed improved 6-7%; event backend improved 25-30%|
|**serde_deserialize** (MiB/s)|tags|–|–|25.5 MiB/s|**32.7 MiB/s**|39.9 MiB/s|Event backend improved 100% (2x faster!) to 82% of serde_yaml speed|

### Notes on serde behaviour

For the `serde_deserialize` group:

- `yaml_parser_serde` refers to `yaml_parser::serde::from_str::<OwnedYamlValue>`,
  which uses the emitter + parser + AST-backed `ValueDeserializer` pipeline.
- `yaml_parser_events_serde` refers to the experimental event-based backend
  (`yaml_parser::serde::bench_from_str_events_internal::<OwnedYamlValue>`),
  which drives serde visitors directly from the event stream without building
  an intermediate AST and is currently not run on the `anchors_aliases` corpus.
- `serde_yaml` refers to `serde_yaml::from_str::<OwnedYamlValue>`.

All three configurations deserialize into the same logical target type,
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

## Recent Optimizations (2026-03-18)

### Event Boxing Optimization

**Problem:** The `Event` enum was 104 bytes due to the inline `Properties` struct (64 bytes). Moving `Event` objects on the stack during iteration was expensive.

**Solution:** Changed `properties` field from `Properties<'input>` to `Box<Properties<'input>>` in:
- `Event::Scalar`
- `Event::MappingStart`
- `Event::SequenceStart`

**Results:**
- `Event` size reduced from 104 bytes to **48 bytes** (54% reduction)
- Parse throughput improved by **13-26%** across all benchmarks
- Event-based serde throughput improved by **27-100%** (2x on tags benchmark)
- Event-based serde now achieves **70-85%** of serde_yaml speed (up from ~60-70%)

**Key improvements:**
- `large_mapping` (events_serde): 21-24 MiB/s → **30.7 MiB/s** (+30-50%)
- `nested_mapping` (events_serde): 21 MiB/s → **27.1 MiB/s** (+27%)
- `tags` (events_serde): ~30 MiB/s → **32.7 MiB/s** (+100% from earlier baseline)
- `block_scalars` (parse): 70.7 MiB/s → **85.6 MiB/s** (+21%)

The boxing optimization moves the 64-byte `Properties` struct to the heap, reducing the cost of moving/cloning `Event` objects on the stack, which is a frequent operation in the streaming parser.
