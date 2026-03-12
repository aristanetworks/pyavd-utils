<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Benchmark Report

**Date:** 2026-03-10
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
| large_mapping | **27.4** | 21.4 | **+28% faster** ✓ |
| nested_mapping | **22.0** | 16.9 | **+30% faster** ✓ |
| large_sequence | 26.4 | 26.4 | ~parity ✓ |
| block_scalars | **60.2** | 54.7 | **+10% faster** ✓ |
| flow_collections | **30.1** | 16.3 | **+84% faster** ✓ |
| anchors_aliases | **32.0** | 14.1 | **+127% faster** ✓ |
| tags | **31.7** | 21.8 | **+45% faster** ✓ |

### Latency (µs, lower is better)

| Benchmark | yaml_parser | saphyr_marked | Comparison |
| --------- | ----------- | ------------- | ---------- |
| small | **1.43** | 1.56 | **8% faster** ✓ |
| medium | **55.8** | 70.2 | **20% faster** ✓ |
| large | **144** | 167 | **14% faster** ✓ |

### Scalar Types (µs, lower is better)

| Benchmark | yaml_parser | saphyr_marked | Comparison |
| --------- | ----------- | ------------- | ---------- |
| plain | 3.65 | **3.48** | saphyr ~5% faster ✓ |
| double_quoted | **3.75** | 4.50 | **17% faster** ✓ |
| block_scalars | 27.2 | **22.0** | saphyr ~23% faster ✓ |

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
