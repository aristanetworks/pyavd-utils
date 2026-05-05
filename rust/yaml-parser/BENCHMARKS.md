<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Benchmark Report

**Date:** 2026-05-05
**Parser Version:** 0.0.4
**Source of truth for this snapshot:** `tmp/remote-bench/runs/20260505T044404Z/candidate_report.md`
**Run type:** remote workspace-vs-`HEAD` comparison over `parse_throughput`, `parse_latency`, `scalar_types`, and `serde_deserialize_throughput`

## How To Run

From the repository root, use the remote bench harness for stable numbers:

```bash
scripts/remote_bench.sh --baseline-ref HEAD \
  --filter '^(parse_throughput|parse_latency|scalar_types|serde_deserialize_throughput)/'
```

The runner sources `tmp/remote-bench/config.env` by default. Set
`REMOTE_BENCH_HOST` there and optionally `REMOTE_BENCH_SUBDIR` if the remote
workspace should live somewhere other than `~/.cache/pyavd-utils-bench`.

For focused follow-up work, either narrow the Criterion regex or pin exact
bench ids:

```bash
scripts/remote_bench.sh --filter 'parse_latency/(yaml_parser|saphyr_marked)'
scripts/remote_bench.sh --benchmark 'parse_latency/yaml_parser/small'
scripts/remote_bench.sh --benchmark 'parse_throughput/yaml_parser/block_scalars'
```

Each comparison run writes fetched Criterion artifacts plus `metadata.txt`,
`comparison.txt`, `baseline_report.md`, and `candidate_report.md` under
`tmp/remote-bench/runs/<timestamp>/`.

For local iteration only, you can still run the suite directly from
`rust/yaml-parser`:

```bash
cargo bench --bench parser_bench --features serde
```

## Comparison Targets

- `yaml_parser`: this crate
- `saphyr_marked`: Saphyr with span tracking
- `serde_yaml`: serde_yaml reference implementation

Notes:

- `parse_throughput` compares parse-oriented APIs, so `serde_yaml` is included
  as a useful reference rather than a perfect apples-to-apples parser match.
- `serde_deserialize_throughput` compares deserialization into the same logical
  target type, `OwnedYamlValue(yaml_parser::Value<'static>)`.
- Absolute numbers vary with host and load. The remote host mainly reduces
  variance; relative comparisons are still the main signal.

## Current Takeaways

- Relative to `saphyr_marked`, `yaml_parser` is ahead on 5/7 parse-throughput
  datasets: `large_mapping`, `nested_mapping`, `block_scalars`,
  `flow_collections`, and `anchors_aliases`.
- Relative to `saphyr_marked`, `yaml_parser` is still behind on
  `large_sequence` and `tags`.
- Relative to `saphyr_marked`, `yaml_parser` is ahead on `medium` and `large`
  parse latency, but still behind on `small`.
- Relative to `serde_yaml`, `yaml_parser` remains ahead on all 7/7
  serde-deserialize throughput datasets.
- The block-scalar rewrite is the main movement in this snapshot:
  `yaml_parser` now leads `saphyr_marked` on both the block-scalar throughput
  dataset and the block-scalar scalar microbenchmark.
- The plain and double-quoted scalar microbenchmarks are still behind
  `saphyr_marked`.

## Parse Throughput

Median throughput from the remote candidate report. Criterion's 95% CI
half-width stays within ±0.23% in this section (median row: ±0.05%).

| Dataset | yaml_parser | saphyr_marked | serde_yaml | yaml_parser vs saphyr | yaml_parser vs serde_yaml |
| --- | ---: | ---: | ---: | ---: | ---: |
| `large_mapping` | 18.094 MiB/s | 16.350 MiB/s | 12.481 MiB/s | 110.7% | 145.0% |
| `nested_mapping` | 15.583 MiB/s | 13.077 MiB/s | 10.519 MiB/s | 119.2% | 148.1% |
| `large_sequence` | 18.425 MiB/s | 20.700 MiB/s | 14.675 MiB/s | 89.0% | 125.6% |
| `block_scalars` | 61.474 MiB/s | 51.450 MiB/s | 34.150 MiB/s | 119.5% | 180.0% |
| `flow_collections` | 13.410 MiB/s | 13.193 MiB/s | 10.300 MiB/s | 101.6% | 130.2% |
| `anchors_aliases` | 13.307 MiB/s | 11.298 MiB/s | 9.763 MiB/s | 117.8% | 136.3% |
| `tags` | 15.932 MiB/s | 16.524 MiB/s | 12.613 MiB/s | 96.4% | 126.3% |

## Parse Latency

Median time per parse from the remote candidate report. Criterion's 95% CI
half-width stays within ±0.11% in this section (median row: ±0.04%).

| Dataset | yaml_parser | saphyr_marked | yaml_parser vs saphyr |
| --- | ---: | ---: | ---: |
| `small` | 2.123 us | 1.994 us | 106.5% |
| `medium` | 74.403 us | 89.054 us | 83.5% |
| `large` | 195.259 us | 216.086 us | 90.4% |

## Scalar Microbenchmarks

Median time per parse from the remote candidate report. Criterion's 95% CI
half-width stays within ±0.20% in this section (median row: ±0.06%).

| Dataset | yaml_parser | saphyr_marked | yaml_parser vs saphyr |
| --- | ---: | ---: | ---: |
| `plain` | 4.244 us | 4.058 us | 104.6% |
| `double_quoted` | 5.091 us | 4.208 us | 121.0% |
| `block_scalars` | 23.987 us | 28.895 us | 83.0% |

## Serde Deserialize Throughput

Both backends deserialize into the same logical target type:
`OwnedYamlValue(yaml_parser::Value<'static>)`.

Criterion's 95% CI half-width stays within ±1.08% in this section
(median row: ±0.04%).

| Dataset | yaml_parser | serde_yaml | yaml_parser vs serde_yaml |
| --- | ---: | ---: | ---: |
| `large_mapping` | 17.114 MiB/s | 12.607 MiB/s | 135.7% |
| `nested_mapping` | 15.566 MiB/s | 11.059 MiB/s | 140.8% |
| `large_sequence` | 18.247 MiB/s | 14.231 MiB/s | 128.2% |
| `block_scalars` | 61.153 MiB/s | 34.621 MiB/s | 176.6% |
| `flow_collections` | 13.305 MiB/s | 10.843 MiB/s | 122.7% |
| `anchors_aliases` | 12.131 MiB/s | 10.351 MiB/s | 117.2% |
| `tags` | 17.375 MiB/s | 12.763 MiB/s | 136.1% |

## Benchmark Matrix Notes

- The benchmark suite intentionally keeps both parse-oriented and serde-oriented
  groups because they answer different questions.
- `parse_throughput` is about the shared lexer/emitter/parser core.
- `serde_deserialize_throughput` is about the public serde API on top of that core.
- `parse_latency` and `scalar_types` stay in the matrix because short-run and
  scalar-heavy regressions are easy to miss in throughput-only views.
