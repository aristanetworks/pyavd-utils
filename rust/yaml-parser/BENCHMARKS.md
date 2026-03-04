<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Benchmark Report

**Date:** 2026-03-04
**Parser Version:** 0.0.2

This document presents performance benchmarks comparing `yaml_parser` against
`saphyr`, a mature Rust YAML parser.

## Comparison Methodology

We compare three configurations:

- **`yaml_parser`**: Our parser (always includes span tracking)
- **`saphyr`**: Saphyr without span tracking (`Yaml` type)
- **`saphyr_marked`**: Saphyr with span tracking (`MarkedYaml` type)

Since `yaml_parser` always tracks spans for every node, comparing against
`saphyr_marked` provides the fairest "apples-to-apples" comparison.

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

| Benchmark | yaml_parser | saphyr | saphyr_marked | vs saphyr_marked |
| --------- | ----------- | ------ | ------------- | ---------------- |
| large_mapping | **26.8** | 19.7 | 20.0 | **+34% faster** ✓ |
| nested_mapping | **23.9** | 16.2 | 16.0 | **+49% faster** ✓ |
| large_sequence | **27.6** | 25.9 | 24.3 | **+14% faster** ✓ |
| block_scalars | **70.0** | 64.2 | 69.7 | ~parity ✓ |
| flow_collections | **57.5** | 16.3 | 15.6 | **+269% faster** ✓ |

### Latency (µs, lower is better)

| Benchmark | yaml_parser | saphyr | saphyr_marked | vs saphyr_marked |
| --------- | ----------- | ------ | ------------- | ---------------- |
| small | **1.02** | 1.57 | 1.63 | **37% faster** ✓ |
| medium | **45.7** | 70.8 | 75.8 | **40% faster** ✓ |
| large | **152** | 178 | 182 | **16% faster** ✓ |

### Scalar Types (µs, lower is better)

| Benchmark | yaml_parser | saphyr | saphyr_marked | vs saphyr_marked |
| --------- | ----------- | ------ | ------------- | ---------------- |
| plain | **2.46** | 3.18 | 3.27 | **25% faster** ✓ |
| double_quoted | **2.91** | 3.50 | 3.67 | **21% faster** ✓ |
| block_scalars | **21.5** | 22.3 | 21.6 | ~parity ✓ |

## Analysis

### Where We Excel

1. **Flow Collections**: 269% faster than `saphyr_marked`. Our token-based
   architecture handles nested `{}`/`[]` very efficiently.

2. **Mappings**: 34-49% faster. Zero-copy plain scalar parsing and efficient
   token dispatch contribute to this advantage.

3. **Latency**: 16-40% faster across all document sizes. Important for
   interactive use cases (IDE integration, config reloading).

4. **Plain/Double-Quoted Scalars**: 21-25% faster due to zero-copy lexer
   design that borrows directly from input.

### Block Scalars

Block scalars now achieve **parity** with `saphyr_marked` (~70 MiB/s) after
optimizations:

- `join_literal_spans`: Uses `split_first()` to eliminate branch in loop
- `apply_chomping`: Uses `trim_end_matches` + `truncate` instead of while/pop
- Pre-allocation with `Vec::with_capacity(8)` for typical block sizes

## Key Optimizations

1. **Zero-Copy Lexing**: Plain scalars borrow directly from input using
   `Cow::Borrowed`, avoiding allocations for most scalar values.

2. **Span-Based Block Scalars**: Block scalar lines are collected as spans
   pointing to the original input, then joined at the end—avoiding per-line
   string allocations.

3. **Direct Byte Iteration**: The lexer iterates over byte positions with
   direct string slicing, avoiding `Vec<char>` overhead.

4. **Compact Types**: `Span` uses `u32` for positions (8 bytes total vs 16),
   `Node` uses boxed properties (48 bytes vs 96).

## Test Data

Benchmarks use representative YAML files in `benches/data/`:

- `large_mapping.yml`: 100 key-value pairs
- `nested_mapping.yml`: 4-level nested structure
- `large_sequence.yml`: 100-item sequence
- `block_scalars.yml`: Multiple literal and folded block scalars
- `flow_collections.yml`: Nested flow mappings and sequences

## Hardware Notes

Results may vary based on hardware. These benchmarks were run on a typical
development machine. For reproducible results, ensure:

- No background processes consuming CPU
- Run multiple times to verify consistency
- Use `cargo bench` (which performs statistical analysis)
