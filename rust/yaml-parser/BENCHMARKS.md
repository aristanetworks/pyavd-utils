<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Benchmark Report

**Date:** 2026-03-21
**Parser Version:** 0.0.2
**Source of truth for this snapshot:** `/tmp/yaml-parser-bench-20260321-full-final-cleanup.txt`

## How To Run

From `rust/yaml-parser`:

```bash
cargo bench --bench parser_bench --features serde
```

For analysis work, prefer one full captured run:

```bash
cargo bench -p yaml-parser --bench parser_bench --features serde \
  > /tmp/yaml-parser-bench-YYYYMMDD-full.txt 2>&1
```

Then extract the comparisons you need from that file rather than re-running
small slices repeatedly.

## Comparison Targets

- `yaml_parser`: this crate
- `saphyr_marked`: Saphyr with span tracking
- `serde_yaml`: serde_yaml reference implementation

Notes:

- `parse_throughput` compares parse-oriented APIs, so `serde_yaml` is included
  as a useful reference rather than a perfect apples-to-apples parser match.
- `serde_deserialize_throughput` compares deserialization into the same logical
  target type, `OwnedYamlValue(yaml_parser::Value<'static>)`.
- Absolute numbers vary with machine load. Relative comparisons are the main
  signal.

## Current Takeaways

- Parse throughput is now ahead of both references on
  `large_mapping`, `nested_mapping`, `large_sequence`, `flow_collections`, and
  `anchors_aliases`.
- `tags` is still slightly behind `saphyr_marked`, but ahead of `serde_yaml`.
- `block_scalars` remains the main parse-throughput gap versus `saphyr_marked`,
  even though yaml-parser is well ahead of `serde_yaml`.
- Serde throughput is now competitive or ahead on every benchmark corpus in
  this run.
- Latency is strong overall: yaml-parser is faster than `saphyr_marked` on all
  three `parse_latency` cases in this snapshot.

## Parse Throughput

Median throughput from the captured full run.

| Dataset | yaml_parser | saphyr_marked | serde_yaml | yaml_parser vs saphyr | yaml_parser vs serde_yaml |
| --- | ---: | ---: | ---: | ---: | ---: |
| `large_mapping` | 29.312 MiB/s | 29.031 MiB/s | 21.746 MiB/s | 101.0% | 134.8% |
| `nested_mapping` | 27.119 MiB/s | 21.308 MiB/s | 20.652 MiB/s | 127.3% | 131.3% |
| `large_sequence` | 33.450 MiB/s | 30.464 MiB/s | 27.762 MiB/s | 109.8% | 120.5% |
| `block_scalars` | 75.559 MiB/s | 93.617 MiB/s | 64.808 MiB/s | 80.7% | 116.6% |
| `flow_collections` | 23.326 MiB/s | 20.349 MiB/s | 19.528 MiB/s | 114.6% | 119.4% |
| `anchors_aliases` | 25.949 MiB/s | 18.170 MiB/s | 18.550 MiB/s | 142.8% | 139.9% |
| `tags` | 26.321 MiB/s | 27.372 MiB/s | 24.459 MiB/s | 96.2% | 107.6% |

## Parse Latency

Median time per parse from the captured full run.

| Dataset | yaml_parser | saphyr_marked |
| --- | ---: | ---: |
| `small` | 1.101 us | 1.147 us |
| `medium` | 40.300 us | 59.862 us |
| `large` | 133.71 us | 136.26 us |

## Scalar Microbenchmarks

Median time per parse from the captured full run.

| Dataset | yaml_parser | saphyr_marked |
| --- | ---: | ---: |
| `plain` | 2.354 us | 2.628 us |
| `double_quoted` | 2.956 us | 2.365 us |
| `block_scalars` | 20.088 us | 18.910 us |

## Serde Deserialize Throughput

Both backends deserialize into the same logical target type:
`OwnedYamlValue(yaml_parser::Value<'static>)`.

| Dataset | yaml_parser | serde_yaml | yaml_parser vs serde_yaml |
| --- | ---: | ---: | ---: |
| `large_mapping` | 30.771 MiB/s | 22.745 MiB/s | 135.3% |
| `nested_mapping` | 28.542 MiB/s | 22.643 MiB/s | 126.0% |
| `large_sequence` | 30.847 MiB/s | 30.192 MiB/s | 102.2% |
| `block_scalars` | 76.748 MiB/s | 69.753 MiB/s | 110.0% |
| `flow_collections` | 25.042 MiB/s | 20.402 MiB/s | 122.7% |
| `anchors_aliases` | 23.579 MiB/s | 20.272 MiB/s | 116.3% |
| `tags` | 31.040 MiB/s | 25.933 MiB/s | 119.7% |

## Benchmark Matrix Notes

- The benchmark suite intentionally keeps both parse-oriented and serde-oriented
  groups because they answer different questions.
- `parse_throughput` is about the shared lexer/emitter/parser core.
- `serde_deserialize_throughput` is about the public serde API on top of that core.
- The current benchmark set no longer carries the old split naming for multiple
  yaml-parser serde implementations; `yaml_parser` is the only serde path now.
