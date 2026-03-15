<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Benchmark Report

**Date:** 2026-03-15
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
cargo bench
```

To run a specific benchmark group:

```bash
cargo bench -- parse_throughput
cargo bench -- parse_latency
cargo bench -- scalar_types
```

## Results Summary

All numbers below are medians from the latest run of:

```bash
cargo bench -p yaml-parser --features serde
```

Times are in microseconds (µs) for latency-style benchmarks and MiB/s for
throughput-style benchmarks. All serde benchmarks are configured to **panic on
failure** (via `unwrap()`), so any deserialization incompatibility will cause
the benchmark run itself to fail rather than silently skipping a dataset.

|Group|Dataset|yaml_parser|saphyr_marked / serde_yaml|Notes|
|-----|-------|----------|---------------------------|-----|
|**parse_throughput** (MiB/s)|large_mapping|**33.4 MiB/s**|27.0 MiB/s|yaml_parser ~23% faster than saphyr_marked|
|**parse_throughput** (MiB/s)|nested_mapping|**26.2 MiB/s**|21.6 MiB/s|yaml_parser ~21% faster than saphyr_marked|
|**parse_throughput** (MiB/s)|large_sequence|31.6 MiB/s|**34.2 MiB/s**|saphyr_marked ~8% faster|
|**parse_throughput** (MiB/s)|block_scalars|70.7 MiB/s|**91.4 MiB/s**|saphyr_marked noticeably faster on this input|
|**parse_throughput** (MiB/s)|flow_collections|**24.0 MiB/s**|21.4 MiB/s|yaml_parser ~12% faster than saphyr_marked (after stricter trailing-content checks)|
|**parse_throughput** (MiB/s)|anchors_aliases|**27.0 MiB/s**|18.2 MiB/s|yaml_parser ~48% faster than saphyr_marked|
|**parse_throughput** (MiB/s)|tags|24.5 MiB/s|**26.4 MiB/s**|saphyr_marked ~8% faster on this corpus after tag semantics alignment and tag expansion optimisations|
|**parse_latency** (µs)|small|**1.20 µs**|1.20 µs|~parity on tiny documents|
|**parse_latency** (µs)|medium|**44.5 µs**|54.0 µs|yaml_parser ~18% faster than saphyr_marked|
|**parse_latency** (µs)|large|**126.8 µs**|129.9 µs|yaml_parser slightly faster on large combined input|
|**scalar_types** (µs)|plain|2.49 µs|**2.44 µs**|saphyr_marked ~2% faster|
|**scalar_types** (µs)|double_quoted|3.20 µs|**2.49 µs**|saphyr_marked faster on this micro-benchmark|
|**scalar_types** (µs)|block_scalars|21.0 µs|**16.1 µs**|saphyr_marked faster on this micro-benchmark|
|**serde_deserialize** (MiB/s)|large_mapping|**24.8 MiB/s**|21.0 MiB/s (`serde_yaml`)|yaml_parser::serde ~18% faster than serde_yaml|
|**serde_deserialize** (MiB/s)|nested_mapping|**19.4 MiB/s**|18.3 MiB/s (`serde_yaml`)|yaml_parser::serde modestly faster|
|**serde_deserialize** (MiB/s)|large_sequence|**28.6 MiB/s**|25.6 MiB/s (`serde_yaml`)|yaml_parser::serde ~12% faster|
|**serde_deserialize** (MiB/s)|block_scalars|**63.1 MiB/s**|61.0 MiB/s (`serde_yaml`)|yaml_parser::serde slightly faster|
|**serde_deserialize** (MiB/s)|flow_collections|**18.7 MiB/s**|18.4 MiB/s (`serde_yaml`)|rough parity; yaml_parser::serde very slightly faster|
|**serde_deserialize** (MiB/s)|anchors_aliases|17.2 MiB/s|17.2 MiB/s (`serde_yaml`)|~parity between yaml_parser::serde and serde_yaml|
|**serde_deserialize** (MiB/s)|tags|19.7 MiB/s|**22.1 MiB/s** (`serde_yaml`)|serde_yaml modestly faster; both libraries now accept this corpus|

### Notes on serde behaviour

For the `serde_deserialize` group, both
`yaml_parser::serde::from_str::<serde_yaml::Value>` and
`serde_yaml::from_str::<serde_yaml::Value>` are invoked with `unwrap()`. This
means that if either library fails to deserialize a benchmark corpus, the
corresponding benchmark will **panic** and the run will clearly surface that
behavioural difference instead of hiding it behind a "Skipped" entry.

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
