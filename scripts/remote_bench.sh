#!/usr/bin/env bash
# Run yaml-parser Criterion benchmarks on a remote host and compare results.

set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)
CONFIG_FILE="${REMOTE_BENCH_CONFIG:-$REPO_ROOT/tmp/remote-bench/config.env}"

if [[ -f "$CONFIG_FILE" ]]; then
    # shellcheck disable=SC1090
    source "$CONFIG_FILE"
fi

REMOTE_HOST="${REMOTE_BENCH_HOST:-}"
REMOTE_SUBDIR="${REMOTE_BENCH_SUBDIR:-.cache/pyavd-utils-bench}"
BASELINE_REF="HEAD"
CANDIDATE_REF=""
BENCH_FILTER='serde_deserialize_throughput/(yaml_parser|serde_yaml)/(large_mapping|nested_mapping|tags|block_scalars)'
declare -a SELECTED_BENCHMARKS=()
KEEP_REMOTE_RESULTS=0
CLEAN_TARGET=0

usage() {
    cat <<'EOF'
Usage: scripts/remote_bench.sh [options]

Benchmark the yaml-parser Criterion suite on a remote host, fetch the artifacts,
and compare candidate timings against a git baseline.

Options:
  --baseline-ref <ref>   Git ref to use as the baseline export (default: HEAD)
  --candidate-ref <ref>  Git ref to use as the candidate export instead of the workspace
  --filter <regex>       Criterion benchmark filter regex
  --benchmark <id>       Exact benchmark id to run, repeatable
  --clean-target         Delete the remote workspace target dir before building
  --keep-remote-results  Leave the remote run directory in place
  --help                 Show this help text

Environment:
  REMOTE_BENCH_CONFIG    Config file to source before running
  REMOTE_BENCH_HOST      SSH host
  REMOTE_BENCH_SUBDIR    Path under the remote HOME used for staging/results

Default config path:
  tmp/remote-bench/config.env

Examples:
  scripts/remote_bench.sh --benchmark 'parse_latency/yaml_parser/small'
  scripts/remote_bench.sh --benchmark 'parse_latency/yaml_parser/small' \
    --benchmark 'parse_latency/yaml_parser/medium'
  scripts/remote_bench.sh --filter 'parse_latency/(yaml_parser|saphyr_marked)/(small|medium)'
EOF
}

regex_escape() {
    printf '%s' "$1" | sed -E 's/[][(){}.^$*+?|\\]/\\&/g'
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --baseline-ref)
            BASELINE_REF="$2"
            shift 2
            ;;
        --candidate-ref)
            CANDIDATE_REF="$2"
            shift 2
            ;;
        --filter)
            BENCH_FILTER="$2"
            shift 2
            ;;
        --benchmark)
            SELECTED_BENCHMARKS+=("$2")
            shift 2
            ;;
        --keep-remote-results)
            KEEP_REMOTE_RESULTS=1
            shift
            ;;
        --clean-target)
            CLEAN_TARGET=1
            shift
            ;;
        --help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown argument: $1" >&2
            usage >&2
            exit 2
            ;;
    esac
done

if [[ "${#SELECTED_BENCHMARKS[@]}" -gt 0 ]]; then
    declare -a escaped_benchmarks=()
    for benchmark in "${SELECTED_BENCHMARKS[@]}"; do
        escaped_benchmarks+=("$(regex_escape "$benchmark")")
    done
    BENCH_FILTER="^($(IFS='|'; printf '%s' "${escaped_benchmarks[*]}"))$"
fi

if [[ -z "$REMOTE_HOST" ]]; then
    echo "REMOTE_BENCH_HOST is not set. Configure it in $CONFIG_FILE or export it in the environment." >&2
    exit 2
fi

run_id=$(date -u +"%Y%m%dT%H%M%SZ")
local_root="$REPO_ROOT/tmp/remote-bench"
local_stage="$local_root/staging/$run_id"
local_results="$local_root/runs/$run_id"
baseline_export="$local_stage/baseline"
candidate_export="$local_stage/candidate"
remote_root="$REMOTE_SUBDIR"
remote_workspace_root="$remote_root/workspaces"
remote_results_root="$remote_root/results"
remote_run_root="$remote_results_root/$run_id"
remote_candidate="$remote_workspace_root/candidate"
remote_baseline="$remote_workspace_root/baseline"

mkdir -p "$baseline_export" "$candidate_export" "$local_results"

cleanup() {
    rm -rf "$local_stage"
}
trap cleanup EXIT

echo "Preparing local exports"
git -C "$REPO_ROOT" rev-parse --verify "$BASELINE_REF" >/dev/null
git -C "$REPO_ROOT" archive --format=tar "$BASELINE_REF" -o "$local_stage/baseline.tar"
tar -xf "$local_stage/baseline.tar" -C "$baseline_export"

if [[ -n "$CANDIDATE_REF" ]]; then
    git -C "$REPO_ROOT" rev-parse --verify "$CANDIDATE_REF" >/dev/null
    git -C "$REPO_ROOT" archive --format=tar "$CANDIDATE_REF" -o "$local_stage/candidate.tar"
    tar -xf "$local_stage/candidate.tar" -C "$candidate_export"
    candidate_description="$CANDIDATE_REF"
else
    rsync -a \
        --delete \
        --filter=':- .gitignore' \
        --exclude '.git/' \
        --exclude 'tmp/remote-bench/' \
        "$REPO_ROOT/" "$candidate_export/"
    candidate_description="workspace"
fi

printf 'baseline_ref=%s\ncandidate=%s\nfilter=%s\nclean_target=%s\n' \
    "$BASELINE_REF" \
    "$candidate_description" \
    "$BENCH_FILTER" \
    "$CLEAN_TARGET" >"$local_results/metadata.txt"

echo "Creating remote run directory on $REMOTE_HOST"
ssh "$REMOTE_HOST" "bash -lc 'mkdir -p \"$remote_run_root\" \"$remote_workspace_root\"'"

echo "Syncing baseline export"
rsync -azc --delete --filter='P target/' "$baseline_export/" "$REMOTE_HOST:$remote_baseline/"

echo "Syncing candidate export"
rsync -azc --delete --filter='P target/' "$candidate_export/" "$REMOTE_HOST:$remote_candidate/"

run_remote_bench() {
    local label="$1"
    local remote_source="$2"
    local remote_result="$remote_run_root/$label"
    local remote_result_quoted
    local remote_source_quoted
    local bench_filter_quoted
    local clean_target_quoted

    printf -v remote_result_quoted '%q' "$remote_result"
    printf -v remote_source_quoted '%q' "$remote_source"
    printf -v bench_filter_quoted '%q' "$BENCH_FILTER"
    printf -v clean_target_quoted '%q' "$CLEAN_TARGET"

    echo "Running remote benchmark: $label"
    ssh "$REMOTE_HOST" \
        "REMOTE_RESULT=$remote_result_quoted REMOTE_SOURCE=$remote_source_quoted BENCH_FILTER=$bench_filter_quoted CLEAN_TARGET=$clean_target_quoted bash -s" <<'EOF'
set -euo pipefail

make_absolute() {
    local path="$1"
    if [[ "$path" = /* ]]; then
        printf '%s\n' "$path"
    else
        printf '%s/%s\n' "$HOME" "$path"
    fi
}

remote_result="$REMOTE_RESULT"
remote_source="$REMOTE_SOURCE"
bench_filter="$BENCH_FILTER"
clean_target="$CLEAN_TARGET"

export PATH="$HOME/.cargo/bin:$PATH"
remote_result=$(make_absolute "$remote_result")
remote_source=$(make_absolute "$remote_source")
mkdir -p "$remote_result"
cd "$remote_source/rust/yaml-parser"

output_file="$remote_result/output.txt"
target_dir="$remote_source/target"
result_criterion_dir="$remote_result/target/criterion"

if [[ "$clean_target" == "1" ]]; then
    rm -rf "$target_dir"
else
    rm -rf "$target_dir/criterion"
fi
CARGO_TARGET_DIR="$target_dir" cargo bench --bench parser_bench --features serde -- "$bench_filter" | tee "$output_file"
rm -rf "$result_criterion_dir"
mkdir -p "$result_criterion_dir"
rsync -a --delete "$target_dir/criterion/" "$result_criterion_dir/"
EOF
}

run_remote_bench "baseline" "$remote_baseline"
run_remote_bench "candidate" "$remote_candidate"

echo "Fetching remote artifacts"
mkdir -p "$local_results/baseline/target" "$local_results/candidate/target"
rsync -az "$REMOTE_HOST:$remote_run_root/baseline/output.txt" "$local_results/baseline/output.txt"
rsync -az "$REMOTE_HOST:$remote_run_root/candidate/output.txt" "$local_results/candidate/output.txt"
rsync -az "$REMOTE_HOST:$remote_run_root/baseline/target/criterion/" "$local_results/baseline/target/criterion/"
rsync -az "$REMOTE_HOST:$remote_run_root/candidate/target/criterion/" "$local_results/candidate/target/criterion/"

echo "Comparing results"
python3 "$SCRIPT_DIR/compare_criterion.py" \
    compare \
    "$local_results/baseline/target/criterion" \
    "$local_results/candidate/target/criterion" | tee "$local_results/comparison.txt"

echo "Writing markdown benchmark reports"
python3 "$SCRIPT_DIR/compare_criterion.py" \
    report \
    "$local_results/baseline/target/criterion" \
    --label "Baseline Benchmark Report" \
    --format markdown >"$local_results/baseline_report.md"
python3 "$SCRIPT_DIR/compare_criterion.py" \
    report \
    "$local_results/candidate/target/criterion" \
    --label "Candidate Benchmark Report" \
    --format markdown >"$local_results/candidate_report.md"

if [[ "$KEEP_REMOTE_RESULTS" -eq 0 ]]; then
    echo "Cleaning remote run directory"
    ssh "$REMOTE_HOST" "bash -lc 'rm -rf \"$remote_run_root\"'"
fi

echo
echo "Results stored in $local_results"
