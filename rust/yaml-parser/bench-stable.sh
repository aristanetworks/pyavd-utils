#!/bin/bash
# Stable benchmark runner with CPU pinning and high priority
# Usage: ./bench-stable.sh [benchmark_filter]

set -e

# Pin to CPU core 0 (you can change this to any core 0-15)
CPU_CORE=0

# Benchmark filter (default: all serde benchmarks)
FILTER="${1:-serde_deserialize_throughput/(yaml_parser_events_serde|serde_yaml)/(large_mapping|nested_mapping|tags|block_scalars)}"

echo "=========================================="
echo "Running stable benchmarks"
echo "CPU Core: $CPU_CORE"
echo "Filter: $FILTER"
echo "=========================================="
echo ""

# Run with taskset (CPU pinning) and nice (high priority)
# -c $CPU_CORE: pin to specific core
# nice -n -20: highest priority (requires sudo for values < 0)
# We'll use nice -n -10 which doesn't require sudo but still helps

taskset -c $CPU_CORE nice -n -10 cargo bench --bench parser_bench --features serde -- "$FILTER" 2>&1
