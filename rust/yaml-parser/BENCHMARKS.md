<!--
  ~ Copyright (c) 2026 Arista Networks, Inc.
  ~ Use of this source code is governed by the Apache License 2.0
  ~ that can be found in the LICENSE file.
  -->

# Benchmark Report

**Date:** 2026-04-12
**Parser Version:** 0.0.4
**Source of truth for this snapshot:** `tmp/remote-bench/runs/20260412T143412Z-candidate-only/candidate_report.md`
**Run type:** remote candidate-only workspace run over `parse_throughput`, `parse_latency`, `scalar_types`, and `serde_deserialize_throughput`

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

- Relative to `saphyr_marked`, `yaml_parser` is ahead on 4/7 parse-throughput
  datasets: `large_mapping`, `nested_mapping`, `flow_collections`, and
  `anchors_aliases`.
- Relative to `saphyr_marked`, `yaml_parser` is still behind on
  `large_sequence`, `block_scalars`, and slightly behind on `tags`.
- Relative to `saphyr_marked`, `yaml_parser` is ahead on `medium` and `large`
  parse latency, but still behind on `small`.
- Relative to `serde_yaml`, `yaml_parser` remains ahead on all 7/7
  serde-deserialize throughput datasets.
- The scalar microbenchmarks are still all behind `saphyr_marked`, with the
  largest gap in `block_scalars`.

## Parse Throughput

Median throughput from the remote candidate report. Criterion's 95% CI
half-width stays within ±0.83% in this section (median row: ±0.04%).

| Dataset | yaml_parser | saphyr_marked | serde_yaml | yaml_parser vs saphyr | yaml_parser vs serde_yaml |
| --- | ---: | ---: | ---: | ---: | ---: |
| `large_mapping` | 18.245 MiB/s | 15.938 MiB/s | 12.624 MiB/s | 114.5% | 144.5% |
| `nested_mapping` | 15.932 MiB/s | 12.856 MiB/s | 10.613 MiB/s | 123.9% | 150.1% |
| `large_sequence` | 19.006 MiB/s | 19.983 MiB/s | 14.860 MiB/s | 95.1% | 127.9% |
| `block_scalars` | 41.507 MiB/s | 51.897 MiB/s | 33.986 MiB/s | 80.0% | 122.1% |
| `flow_collections` | 13.701 MiB/s | 12.664 MiB/s | 10.449 MiB/s | 108.2% | 131.1% |
| `anchors_aliases` | 13.397 MiB/s | 11.100 MiB/s | 9.886 MiB/s | 120.7% | 135.5% |
| `tags` | 16.169 MiB/s | 16.396 MiB/s | 12.845 MiB/s | 98.6% | 125.9% |

## Parse Latency

Median time per parse from the remote candidate report. Criterion's 95% CI
half-width stays within ±0.17% in this section (median row: ±0.06%).

| Dataset | yaml_parser | saphyr_marked | yaml_parser vs saphyr |
| --- | ---: | ---: | ---: |
| `small` | 2.097 us | 1.987 us | 105.5% |
| `medium` | 72.695 us | 91.034 us | 79.9% |
| `large` | 207.021 us | 219.182 us | 94.5% |

## Scalar Microbenchmarks

Median time per parse from the remote candidate report. Criterion's 95% CI
half-width stays within ±0.12% in this section (median row: ±0.06%).

| Dataset | yaml_parser | saphyr_marked | yaml_parser vs saphyr |
| --- | ---: | ---: | ---: |
| `plain` | 4.194 us | 4.107 us | 102.1% |
| `double_quoted` | 4.933 us | 4.241 us | 116.3% |
| `block_scalars` | 36.171 us | 28.752 us | 125.8% |

## Serde Deserialize Throughput

Both backends deserialize into the same logical target type:
`OwnedYamlValue(yaml_parser::Value<'static>)`.

Criterion's 95% CI half-width stays within ±0.09% in this section
(median row: ±0.03%).

| Dataset | yaml_parser | serde_yaml | yaml_parser vs serde_yaml |
| --- | ---: | ---: | ---: |
| `large_mapping` | 17.656 MiB/s | 12.701 MiB/s | 139.0% |
| `nested_mapping` | 16.180 MiB/s | 11.316 MiB/s | 143.0% |
| `large_sequence` | 19.336 MiB/s | 14.427 MiB/s | 134.0% |
| `block_scalars` | 41.381 MiB/s | 35.021 MiB/s | 118.2% |
| `flow_collections` | 13.773 MiB/s | 10.786 MiB/s | 127.7% |
| `anchors_aliases` | 12.625 MiB/s | 10.572 MiB/s | 119.4% |
| `tags` | 17.933 MiB/s | 13.091 MiB/s | 137.0% |

## Benchmark Matrix Notes

- The benchmark suite intentionally keeps both parse-oriented and serde-oriented
  groups because they answer different questions.
- `parse_throughput` is about the shared lexer/emitter/parser core.
- `serde_deserialize_throughput` is about the public serde API on top of that core.
- `parse_latency` and `scalar_types` stay in the matrix because short-run and
  scalar-heavy regressions are easy to miss in throughput-only views.
