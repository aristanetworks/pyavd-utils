<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Benchmark Report

**Date:** 2026-03-04
**Parser Version:** 0.0.2

This document presents performance benchmarks comparing `yaml_parser` against
`saphyr_marked` (Saphyr with span tracking), a mature Rust YAML parser.

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

### Throughput (MiB/s, higher is better)

| Benchmark | yaml_parser | saphyr_marked | Comparison |
| --------- | ----------- | ------------- | ---------- |
| large_mapping | **26.8** | 20.0 | **+34% faster** ✓ |
| nested_mapping | **23.9** | 16.0 | **+49% faster** ✓ |
| large_sequence | **27.6** | 24.3 | **+14% faster** ✓ |
| block_scalars | 70.0 | 69.7 | ~parity ✓ |
| flow_collections | **57.5** | 15.6 | **+269% faster** ✓ |
| anchors_aliases | **50.4** | 12.9 | **+290% faster** ✓ |
| tags | **36.3** | 17.7 | **+105% faster** ✓ |

### Latency (µs, lower is better)

| Benchmark | yaml_parser | saphyr_marked | Comparison |
| --------- | ----------- | ------------- | ---------- |
| small | **1.02** | 1.63 | **37% faster** ✓ |
| medium | **45.7** | 75.8 | **40% faster** ✓ |
| large | **152** | 182 | **16% faster** ✓ |

### Scalar Types (µs, lower is better)

| Benchmark | yaml_parser | saphyr_marked | Comparison |
| --------- | ----------- | ------------- | ---------- |
| plain | **2.46** | 3.27 | **25% faster** ✓ |
| double_quoted | **2.91** | 3.67 | **21% faster** ✓ |
| block_scalars | 21.5 | 21.6 | ~parity ✓ |

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
