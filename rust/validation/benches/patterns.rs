// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! First-use regex compilation and cached matching through validation.

use std::hint::black_box;

use avdschema::Store;
use avdschema::StoreSource;
use criterion::BatchSize;
use criterion::Criterion;
use validation::StoreValidate as _;
use validation::ValidationResult;
use validation::feedback::ErrorIssue;
use validation::feedback::Violation;

fn benchmark_patterns(criterion: &mut Criterion) -> Result<(), String> {
    for (pattern_name, pattern, matching, nonmatching) in [
        ("simple", "[a-z][a-z0-9]+", "spine42", "42spine"),
        (
            "lookahead",
            "(?=[a-z0-9]*[0-9])[a-z0-9]+",
            "spine42",
            "spine",
        ),
    ] {
        let schema = serde_json::json!({
            "type": "dict",
            "keys": {"name": {"type": "str", "pattern": pattern}}
        });
        let source = StoreSource::from_schema_json("patterns", &schema.to_string())
            .map_err(|error| format!("invalid {pattern_name} benchmark schema: {error}"))?;

        for (case, value, expected_errors) in [("match", matching, 0), ("no_match", nonmatching, 1)]
        {
            let input = serde_json::json!({"name": value});
            let cached_store = Store::compile(&source).map_err(|error| {
                format!("cannot compile {pattern_name} benchmark store: {error}")
            })?;
            // This validates the fixture with a cold cache, then leaves it warm.
            for _ in 0..2 {
                let output = cached_store
                    .validate_value(&input, "patterns", None)
                    .map_err(|error| format!("cannot validate {pattern_name}/{case}: {error}"))?;
                check_pattern_result(&output.result, pattern, value, expected_errors);
            }
            register_pattern_benchmarks(
                criterion,
                &source,
                &cached_store,
                &input,
                &format!("validation/regex/{pattern_name}/{case}"),
            );
        }
    }
    Ok(())
}

fn check_pattern_result(
    result: &ValidationResult,
    pattern: &str,
    value: &str,
    expected_errors: usize,
) {
    assert_eq!(
        result.errors.len(),
        expected_errors,
        "pattern {pattern:?} with value {value:?} must exercise the expected outcome: {:?}",
        result.errors
    );
    assert!(
        result.errors.iter().all(|feedback| matches!(
            &feedback.issue,
            ErrorIssue::Violation(Violation::NotMatchingPattern { pattern: actual, found })
                if actual == pattern && found == value
        )),
        "pattern {pattern:?} with value {value:?} must report mismatches, not internal errors: {:?}",
        result.errors
    );
    assert_eq!(result.warnings, Vec::new());
    assert_eq!(result.infos, Vec::new());
}

fn register_pattern_benchmarks(
    criterion: &mut Criterion,
    source: &StoreSource,
    cached_store: &Store,
    input: &serde_json::Value,
    name: &str,
) {
    criterion.bench_function(&format!("{name}/cold"), |bencher| {
        bencher.iter_batched(
            || {
                let setup_result = Store::compile(source);
                assert!(
                    setup_result.is_ok(),
                    "cold regex benchmark store must compile: {setup_result:?}"
                );
                setup_result
            },
            |setup_result| {
                black_box(setup_result.map(|fresh_store| {
                    let output = fresh_store.validate_value(black_box(input), "patterns", None);
                    // Keep store teardown outside the measured routine too.
                    (fresh_store, output)
                }))
            },
            BatchSize::SmallInput,
        );
    });
    criterion.bench_function(&format!("{name}/cached"), |bencher| {
        bencher.iter_with_large_drop(|| {
            black_box(cached_store.validate_value(black_box(input), "patterns", None))
        });
    });
}

#[cfg(codspeed)]
fn main() -> Result<(), String> {
    benchmark_patterns(&mut Criterion::new_instrumented())
}

#[cfg(not(codspeed))]
fn main() -> Result<(), String> {
    let mut criterion = Criterion::default().configure_from_args();
    benchmark_patterns(&mut criterion)?;
    criterion.final_summary();
    Ok(())
}
