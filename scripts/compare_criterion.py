#!/usr/bin/env python3
# Copyright (c) 2026 Arista Networks, Inc.
# Use of this source code is governed by the Apache License 2.0
# that can be found in the LICENSE file.
"""Compare and report Criterion benchmark medians."""

from __future__ import annotations

import argparse
import json
import sys
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path

PREFERRED_BACKEND_ORDER = ("yaml_parser", "saphyr_marked", "serde_yaml")


@dataclass(frozen=True)
class Benchmark:
    """Median timing and metadata for a single Criterion benchmark."""

    name: str
    group: str
    backend: str
    dataset: str
    median_ns: float
    throughput_bytes: float | None

    @property
    def throughput_mib_s(self) -> float | None:
        """Return throughput in MiB/s when Criterion reported bytes processed."""
        if self.throughput_bytes is None:
            return None
        return (self.throughput_bytes * 1_000_000_000.0) / (self.median_ns * 1024.0 * 1024.0)


def write_stdout(text: str = "") -> None:
    """Write one line to stdout."""
    sys.stdout.write(f"{text}\n")


def write_stderr(text: str) -> None:
    """Write one line to stderr."""
    sys.stderr.write(f"{text}\n")


def build_compare_parser() -> argparse.ArgumentParser:
    """Build the compare subcommand parser."""
    parser = argparse.ArgumentParser(
        prog="compare_criterion.py compare",
        description="Compare Criterion medians. Lower values are better.",
    )
    parser.add_argument("baseline", type=Path, help="Baseline criterion directory")
    parser.add_argument("candidate", type=Path, help="Candidate criterion directory")
    parser.add_argument(
        "--format",
        choices=("text", "markdown"),
        default="text",
        help="Output format for the comparison table",
    )
    return parser


def build_report_parser() -> argparse.ArgumentParser:
    """Build the report subcommand parser."""
    parser = argparse.ArgumentParser(
        prog="compare_criterion.py report",
        description="Render a single Criterion directory as grouped benchmark tables.",
    )
    parser.add_argument("root", type=Path, help="Criterion directory to summarize")
    parser.add_argument(
        "--label",
        default="Benchmark Report",
        help="Heading label for the report",
    )
    parser.add_argument(
        "--format",
        choices=("text", "markdown"),
        default="markdown",
        help="Output format for the report",
    )
    return parser


def parse_args(argv: list[str]) -> tuple[str, argparse.Namespace]:
    """Parse CLI arguments, keeping backward compatibility for compare mode."""
    if argv and argv[0] == "report":
        return "report", build_report_parser().parse_args(argv[1:])

    compare_parser = build_compare_parser()
    if argv and argv[0] == "compare":
        return "compare", compare_parser.parse_args(argv[1:])
    return "compare", compare_parser.parse_args(argv)


def split_function_id(name: str) -> tuple[str, str]:
    """Split a Criterion function id into backend and dataset."""
    parts = name.split("/", 1)
    if len(parts) == 2:
        return parts[0], parts[1]
    return "value", name


def load_benchmarks(root: Path) -> dict[str, Benchmark]:
    """Load all benchmark medians under a Criterion output directory."""
    if not root.exists():
        msg = f"Criterion directory does not exist: {root}"
        raise FileNotFoundError(msg)

    benchmarks: dict[str, Benchmark] = {}
    for estimates_path in root.rglob("estimates.json"):
        relative = estimates_path.relative_to(root)
        if len(relative.parts) < 3 or relative.parts[-2] != "new":
            continue

        benchmark_path = estimates_path.with_name("benchmark.json")
        with estimates_path.open(encoding="utf-8") as file:
            estimate_data = json.load(file)

        if benchmark_path.exists():
            with benchmark_path.open(encoding="utf-8") as file:
                benchmark_data = json.load(file)
            group = benchmark_data.get("group_id") or relative.parts[0]
            function_id = benchmark_data.get("function_id") or "/".join(relative.parts[1:-2])
            throughput = benchmark_data.get("throughput")
            throughput_bytes = None
            if isinstance(throughput, dict) and "Bytes" in throughput:
                throughput_bytes = float(throughput["Bytes"])
        else:
            group = relative.parts[0]
            function_id = "/".join(relative.parts[1:-2])
            throughput_bytes = None

        median = estimate_data.get("median", {})
        point_estimate = median.get("point_estimate")
        if point_estimate is None:
            continue

        backend, dataset = split_function_id(function_id)
        name = f"{group}/{function_id}"
        benchmarks[name] = Benchmark(
            name=name,
            group=group,
            backend=backend,
            dataset=dataset,
            median_ns=float(point_estimate),
            throughput_bytes=throughput_bytes,
        )

    if not benchmarks:
        msg = f"No Criterion estimates found under {root}"
        raise ValueError(msg)

    return benchmarks


def format_ns(value: float) -> str:
    """Format nanoseconds with human readable units."""
    units = [
        ("s", 1_000_000_000.0),
        ("ms", 1_000_000.0),
        ("us", 1_000.0),
        ("ns", 1.0),
    ]
    for suffix, scale in units:
        if value >= scale or suffix == "ns":
            return f"{value / scale:.3f} {suffix}"
    return f"{value:.3f} ns"


def preferred_backends(backends: set[str]) -> list[str]:
    """Sort backend names with known implementations first."""
    ordered = [backend for backend in PREFERRED_BACKEND_ORDER if backend in backends]
    ordered.extend(sorted(backends - set(ordered)))
    return ordered


def render_text_table(headers: tuple[str, ...], rows: list[tuple[str, ...]]) -> str:
    """Render rows as an aligned text table."""
    widths = [max(len(headers[idx]), *(len(row[idx]) for row in rows)) if rows else len(headers[idx]) for idx in range(len(headers))]
    lines = [
        "  ".join(header.ljust(widths[idx]) for idx, header in enumerate(headers)),
        "  ".join("-" * widths[idx] for idx in range(len(headers))),
    ]
    lines.extend("  ".join(value.ljust(widths[idx]) for idx, value in enumerate(row)) for row in rows)
    return "\n".join(lines)


def render_markdown_table(headers: tuple[str, ...], rows: list[tuple[str, ...]]) -> str:
    """Render rows as a markdown table."""
    header_line = "| " + " | ".join(headers) + " |"
    separator_line = "| " + " | ".join("---" for _ in headers) + " |"
    body = ["| " + " | ".join(row) + " |" for row in rows]
    return "\n".join([header_line, separator_line, *body])


def render_comparison(rows: list[tuple[str, float, float, float]], output_format: str) -> str:
    """Render comparison rows in the selected format."""
    headers = ("Benchmark", "Baseline", "Candidate", "Delta")
    data = [
        (
            name,
            format_ns(baseline_ns),
            format_ns(candidate_ns),
            f"{improvement:+.2f}%",
        )
        for name, baseline_ns, candidate_ns, improvement in rows
    ]
    if output_format == "markdown":
        return render_markdown_table(headers, data)
    return render_text_table(headers, data)


def render_group_report(group: str, benchmarks: list[Benchmark], output_format: str) -> str:
    """Render one benchmark group as a dataset x backend table."""
    backends = preferred_backends({benchmark.backend for benchmark in benchmarks})
    datasets = sorted({benchmark.dataset for benchmark in benchmarks})
    lookup = {(benchmark.dataset, benchmark.backend): benchmark for benchmark in benchmarks}
    is_throughput = all(benchmark.throughput_mib_s is not None for benchmark in benchmarks)

    headers = ("Dataset", *backends)
    rows: list[tuple[str, ...]] = []
    for dataset in datasets:
        row = [dataset]
        for backend in backends:
            benchmark = lookup.get((dataset, backend))
            if benchmark is None:
                row.append("-")
            elif is_throughput:
                row.append(f"{benchmark.throughput_mib_s:.3f} MiB/s")
            else:
                row.append(format_ns(benchmark.median_ns))
        rows.append(tuple(row))

    title = f"## {group}"
    table = render_markdown_table(headers, rows) if output_format == "markdown" else render_text_table(headers, rows)
    return f"{title}\n\n{table}"


def run_compare(args: argparse.Namespace) -> int:
    """Run the comparison workflow."""
    baseline = load_benchmarks(args.baseline)
    candidate = load_benchmarks(args.candidate)

    common = sorted(set(baseline) & set(candidate))
    if not common:
        write_stderr("No overlapping benchmarks found between the two Criterion directories.")
        return 2

    rows: list[tuple[str, float, float, float]] = []
    for name in common:
        baseline_ns = baseline[name].median_ns
        candidate_ns = candidate[name].median_ns
        improvement = ((baseline_ns - candidate_ns) / baseline_ns) * 100.0
        rows.append((name, baseline_ns, candidate_ns, improvement))

    rows.sort(key=lambda item: item[3], reverse=True)

    improved = sum(1 for *_, improvement in rows if improvement > 0)
    regressed = sum(1 for *_, improvement in rows if improvement < 0)
    unchanged = len(rows) - improved - regressed
    average_delta = sum(item[3] for item in rows) / len(rows)

    write_stdout("Criterion median comparison")
    write_stdout(f"Baseline:  {args.baseline}")
    write_stdout(f"Candidate: {args.candidate}")
    write_stdout(f"Benchmarks compared: {len(rows)}")
    write_stdout(f"Improved: {improved}  Regressed: {regressed}  Unchanged: {unchanged}")
    write_stdout(f"Average delta: {average_delta:+.2f}%")
    write_stdout()
    write_stdout(render_comparison(rows, args.format))

    baseline_only = sorted(set(baseline) - set(candidate))
    candidate_only = sorted(set(candidate) - set(baseline))
    if baseline_only:
        write_stdout()
        write_stdout("Only in baseline:")
        for name in baseline_only:
            write_stdout(f"  {name}")
    if candidate_only:
        write_stdout()
        write_stdout("Only in candidate:")
        for name in candidate_only:
            write_stdout(f"  {name}")

    return 0


def run_report(args: argparse.Namespace) -> int:
    """Run the grouped single-run report workflow."""
    benchmarks = load_benchmarks(args.root)
    grouped: dict[str, list[Benchmark]] = defaultdict(list)
    for benchmark in benchmarks.values():
        grouped[benchmark.group].append(benchmark)

    sections = [f"# {args.label}", ""]
    for group in sorted(grouped):
        sections.append(render_group_report(group, grouped[group], args.format))
        sections.append("")

    text = "\n".join(section for section in sections if section is not None).rstrip() + "\n"
    sys.stdout.write(text)
    return 0


def main(argv: list[str] | None = None) -> int:
    """Run the selected command."""
    command, args = parse_args(sys.argv[1:] if argv is None else argv)
    if command == "report":
        return run_report(args)
    return run_compare(args)


if __name__ == "__main__":
    raise SystemExit(main())
