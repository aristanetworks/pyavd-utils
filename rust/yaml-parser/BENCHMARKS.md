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
throughput-style benchmarks. "Skipped" indicates that the serde benchmark was
not run for that dataset because at least one of the libraries failed to
deserialize it successfully.

|Group|Dataset|yaml_parser|saphyr_marked / serde_yaml|Notes|
|-----|-------|----------|---------------------------|-----|
|**parse_throughput** (MiB/s)|large_mapping|**34.2 MiB/s**|27.5 MiB/s|yaml_parser ~24% faster than saphyr_marked|
|**parse_throughput** (MiB/s)|nested_mapping|**26.7 MiB/s**|22.0 MiB/s|yaml_parser ~21% faster than saphyr_marked|
|**parse_throughput** (MiB/s)|large_sequence|32.5 MiB/s|**34.4 MiB/s**|saphyr_marked ~6% faster|
|**parse_throughput** (MiB/s)|block_scalars|73.6 MiB/s|**91.3 MiB/s**|saphyr_marked noticeably faster on this input|
|**parse_throughput** (MiB/s)|flow_collections|**44.8 MiB/s**|21.8 MiB/s|yaml_parser >2× faster than saphyr_marked|
|**parse_throughput** (MiB/s)|anchors_aliases|**45.3 MiB/s**|18.7 MiB/s|yaml_parser ~2.4× faster than saphyr_marked|
|**parse_throughput** (MiB/s)|tags|**41.9 MiB/s**|28.4 MiB/s|yaml_parser ~47% faster than saphyr_marked|
|**parse_latency** (µs)|small|**1.16 µs**|1.18 µs|~parity, yaml_parser slightly faster|
|**parse_latency** (µs)|medium|**44.0 µs**|53.5 µs|yaml_parser ~18% faster than saphyr_marked|
|**parse_latency** (µs)|large|**117.4 µs**|129.0 µs|yaml_parser ~9% faster than saphyr_marked|
|**scalar_types** (µs)|plain|2.47 µs|**2.39 µs**|saphyr_marked ~3% faster|
|**scalar_types** (µs)|double_quoted|3.12 µs|**2.51 µs**|saphyr_marked faster on this micro-benchmark|
|**scalar_types** (µs)|block_scalars|20.32 µs|**16.07 µs**|saphyr_marked faster on this micro-benchmark|
|**serde_deserialize** (MiB/s)|large_mapping|**25.7 MiB/s**|21.0 MiB/s (`serde_yaml`)|yaml_parser::serde ~22% faster than serde_yaml|
|**serde_deserialize** (MiB/s)|nested_mapping|**19.9 MiB/s**|18.3 MiB/s (`serde_yaml`)|yaml_parser::serde modestly faster|
|**serde_deserialize** (MiB/s)|large_sequence|**29.2 MiB/s**|24.9 MiB/s (`serde_yaml`)|yaml_parser::serde ~17% faster|
|**serde_deserialize** (MiB/s)|block_scalars|**65.9 MiB/s**|60.2 MiB/s (`serde_yaml`)|yaml_parser::serde consistently faster|
|**serde_deserialize** (MiB/s)|flow_collections|Skipped|Skipped|behaviour differs; yaml_parser reports trailing content|
|**serde_deserialize** (MiB/s)|anchors_aliases|Skipped|Skipped|n/a (serde benchmark not executed)|
|**serde_deserialize** (MiB/s)|tags|Skipped|Skipped|n/a (serde benchmark not executed)|

### Notes on serde exclusions

For the `serde_deserialize` group we deliberately **skip** any dataset where
either `yaml_parser::serde::from_str::<serde_yaml::Value>` or
`serde_yaml::from_str::<serde_yaml::Value>` fails. At the time of writing this
affects:

- `flow_collections`: `yaml_parser::serde` reports a `TrailingContent` parse
  error for this corpus, while `serde_yaml` accepts it. The serde benchmark is
  therefore skipped for this dataset to avoid mixing behavioural differences
  into the performance comparison.
- `anchors_aliases`, `tags`: these datasets are currently also skipped in the
  serde benchmarks (one of the deserializers does not successfully parse them).

These differences are tracked separately from throughput/latency; the numbers
reported here are only for inputs both libraries can deserialize successfully.

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
