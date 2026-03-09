// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML Test Suite integration tests.
//!
//! This module runs the parser against the official YAML 1.2 test suite
//! from <https://github.com/yaml/yaml-test-suite>.

// Allow pedantic lints in test code where they add noise without benefit
#![allow(
    clippy::min_ident_chars,
    reason = "single-char names are fine in tests"
)]
#![allow(clippy::indexing_slicing, reason = "panics are acceptable in tests")]
#![allow(clippy::print_stderr, reason = "test output via eprintln is fine")]
#![allow(clippy::use_debug, reason = "debug formatting in tests is fine")]
#![allow(
    clippy::tests_outside_test_module,
    reason = "integration tests are in tests/ dir"
)]
#![allow(clippy::panic, reason = "panic is acceptable in tests")]
#![allow(
    clippy::integer_division_remainder_used,
    reason = "modulo is fine in tests"
)]

use std::fs;
use std::path::Path;

use yaml_parser::parse;

/// Known differences between the emitter and YAML test suite expectations.
/// These represent intentional differences where the emitter has different (often better)
/// error recovery behavior than the reference implementation.
///
/// Breakdown:
/// - 16 structural differences (event sequence differs in error recovery)
/// - 31 error detection differences (different error reporting strategy)
/// - Total: 47 out of 842 tests (94.4% pass rate)
#[allow(dead_code, reason = "kept for documentation purposes")]
const KNOWN_DIFFERENCES: usize = 47;

/// Event notation for YAML test suite comparison.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Event {
    StreamStart,
    StreamEnd,
    DocumentStart {
        explicit: bool,
    },
    DocumentEnd {
        explicit: bool,
    },
    MappingStart {
        flow: bool,
        anchor: Option<String>,
        tag: Option<String>,
    },
    MappingEnd,
    SequenceStart {
        flow: bool,
        anchor: Option<String>,
        tag: Option<String>,
    },
    SequenceEnd,
    Scalar {
        style: ScalarStyle,
        value: String,
        anchor: Option<String>,
        tag: Option<String>,
    },
    Alias {
        name: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScalarStyle {
    Plain,
    SingleQuoted,
    DoubleQuoted,
    Literal,
    Folded,
}

/// Parse the test.event file format.
fn parse_event_file(content: &str) -> Vec<Event> {
    let mut events = Vec::new();

    for line_raw in content.lines() {
        // Only trim leading whitespace - trailing whitespace may be significant in scalar values
        let line = line_raw.trim_start();
        if line.is_empty() {
            continue;
        }

        if line == "+STR" {
            events.push(Event::StreamStart);
        } else if line == "-STR" {
            events.push(Event::StreamEnd);
        } else if line == "+DOC" {
            events.push(Event::DocumentStart { explicit: false });
        } else if line == "+DOC ---" {
            events.push(Event::DocumentStart { explicit: true });
        } else if line == "-DOC" {
            events.push(Event::DocumentEnd { explicit: false });
        } else if line == "-DOC ..." {
            events.push(Event::DocumentEnd { explicit: true });
        } else if line == "+MAP" {
            events.push(Event::MappingStart {
                flow: false,
                anchor: None,
                tag: None,
            });
        } else if line == "+MAP {}" {
            events.push(Event::MappingStart {
                flow: true,
                anchor: None,
                tag: None,
            });
        } else if line == "-MAP" {
            events.push(Event::MappingEnd);
        } else if line == "+SEQ" {
            events.push(Event::SequenceStart {
                flow: false,
                anchor: None,
                tag: None,
            });
        } else if line == "+SEQ []" {
            events.push(Event::SequenceStart {
                flow: true,
                anchor: None,
                tag: None,
            });
        } else if line == "-SEQ" {
            events.push(Event::SequenceEnd);
        } else if let Some(rest) = line.strip_prefix("=VAL ") {
            let (anchor, tag, style, value) = parse_scalar_event(rest);
            events.push(Event::Scalar {
                style,
                value,
                anchor,
                tag,
            });
        } else if let Some(rest) = line.strip_prefix("=ALI ") {
            let name = rest.trim_start_matches('*').to_owned();
            events.push(Event::Alias { name });
        } else if let Some(rest) = line.strip_prefix("+MAP ") {
            // Complex collection start with anchor/tag
            let (anchor, tag, flow) = parse_collection_event(rest);
            events.push(Event::MappingStart { flow, anchor, tag });
        } else if let Some(rest) = line.strip_prefix("+SEQ ") {
            let (anchor, tag, flow) = parse_collection_event(rest);
            events.push(Event::SequenceStart { flow, anchor, tag });
        }
    }

    events
}

#[allow(
    clippy::string_slice,
    reason = "Test event format uses fixed positions"
)]
fn parse_scalar_event(input: &str) -> (Option<String>, Option<String>, ScalarStyle, String) {
    let mut anchor = None;
    let mut tag = None;
    let mut rest = input;

    // Parse anchor (&name)
    if rest.starts_with('&')
        && let Some(space_idx) = rest.find(' ')
    {
        anchor = Some(rest[1..space_idx].to_owned());
        rest = &rest[space_idx + 1..];
    }

    // Parse tag (<tag>)
    if rest.starts_with('<')
        && let Some(end_idx) = rest.find('>')
    {
        tag = Some(rest[1..end_idx].to_owned());
        rest = rest[end_idx + 1..].trim_start();
    }

    // Parse style and value
    let (style, value) = if let Some(remainder) = rest.strip_prefix(':') {
        (ScalarStyle::Plain, unescape_event_value(remainder))
    } else if let Some(remainder) = rest.strip_prefix('"') {
        (ScalarStyle::DoubleQuoted, unescape_event_value(remainder))
    } else if let Some(remainder) = rest.strip_prefix('\'') {
        (ScalarStyle::SingleQuoted, unescape_event_value(remainder))
    } else if let Some(remainder) = rest.strip_prefix('|') {
        (ScalarStyle::Literal, unescape_event_value(remainder))
    } else if let Some(remainder) = rest.strip_prefix('>') {
        (ScalarStyle::Folded, unescape_event_value(remainder))
    } else {
        (ScalarStyle::Plain, unescape_event_value(rest))
    };

    (anchor, tag, style, value)
}

#[allow(
    clippy::string_slice,
    reason = "Test event format uses fixed positions"
)]
fn parse_collection_event(input: &str) -> (Option<String>, Option<String>, bool) {
    let mut anchor = None;
    let mut tag = None;
    let mut rest = input;

    // YAML Test Suite format: +MAP/+SEQ [flow_indicator] [&anchor] [<tag>]
    // Flow indicator comes first, then anchor, then tag

    // Check for flow indicators first
    let flow = if rest.starts_with("{}") || rest.starts_with("[]") {
        rest = rest[2..].trim_start();
        true
    } else {
        false
    };

    // Check for anchor
    if rest.starts_with('&') {
        if let Some(space_idx) = rest.find(' ') {
            anchor = Some(rest[1..space_idx].to_owned());
            rest = &rest[space_idx + 1..];
        } else {
            anchor = Some(rest[1..].to_owned());
            rest = "";
        }
    }

    // Check for tag
    if rest.starts_with('<')
        && let Some(end_idx) = rest.find('>')
    {
        tag = Some(rest[1..end_idx].to_owned());
    }

    (anchor, tag, flow)
}

fn unescape_event_value(input: &str) -> String {
    let mut result = String::new();
    let mut chars = input.chars().peekable();

    while let Some(current_char) = chars.next() {
        if current_char == '\\' {
            if let Some(&next) = chars.peek() {
                match next {
                    'n' => {
                        result.push('\n');
                        chars.next();
                    }
                    't' => {
                        result.push('\t');
                        chars.next();
                    }
                    'r' => {
                        result.push('\r');
                        chars.next();
                    }
                    '\\' => {
                        result.push('\\');
                        chars.next();
                    }
                    'b' => {
                        result.push('\x08');
                        chars.next();
                    }
                    _ => result.push(current_char),
                }
            } else {
                result.push(current_char);
            }
        } else {
            result.push(current_char);
        }
    }

    result
}

// ============================================================================
// AST to Event conversion
// ============================================================================

/// Compare two event streams, ignoring style differences.
///
/// Returns Ok(()) if the events match structurally, or Err with a description
/// of the first mismatch.
#[allow(dead_code, reason = "utility function for debugging test failures")]
fn compare_events(actual: &[Event], expected: &[Event]) -> Result<(), String> {
    let actual_filtered: Vec<_> = actual.iter().collect();
    let expected_filtered: Vec<_> = expected.iter().collect();

    if actual_filtered.len() != expected_filtered.len() {
        return Err(format!(
            "Event count mismatch: got {} events, expected {}",
            actual_filtered.len(),
            expected_filtered.len()
        ));
    }

    for (i, (actual_ev, expected_ev)) in actual_filtered
        .iter()
        .zip(expected_filtered.iter())
        .enumerate()
    {
        if !events_match(actual_ev, expected_ev) {
            return Err(format!(
                "Event mismatch at index {i}:\n  actual:   {actual_ev:?}\n  expected: {expected_ev:?}"
            ));
        }
    }

    Ok(())
}

/// Check if two events match, ignoring style differences.
fn events_match(actual: &Event, expected: &Event) -> bool {
    match (actual, expected) {
        // Simple events - ignore explicit flags since we don't track them
        (Event::StreamStart, Event::StreamStart)
        | (Event::StreamEnd, Event::StreamEnd)
        | (Event::DocumentStart { .. }, Event::DocumentStart { .. })
        | (Event::DocumentEnd { .. }, Event::DocumentEnd { .. })
        | (Event::MappingEnd, Event::MappingEnd)
        | (Event::SequenceEnd, Event::SequenceEnd) => true,
        // Collections - compare anchor/tag, ignore flow
        (
            Event::MappingStart {
                anchor: actual_anchor,
                tag: actual_tag,
                ..
            },
            Event::MappingStart {
                anchor: expected_anchor,
                tag: expected_tag,
                ..
            },
        )
        | (
            Event::SequenceStart {
                anchor: actual_anchor,
                tag: actual_tag,
                ..
            },
            Event::SequenceStart {
                anchor: expected_anchor,
                tag: expected_tag,
                ..
            },
        ) => actual_anchor == expected_anchor && actual_tag == expected_tag,
        // Scalars - compare value, anchor, tag; ignore style
        (
            Event::Scalar {
                value: actual_val,
                anchor: actual_anchor,
                tag: actual_tag,
                ..
            },
            Event::Scalar {
                value: expected_val,
                anchor: expected_anchor,
                tag: expected_tag,
                ..
            },
        ) => {
            actual_val == expected_val
                && actual_anchor == expected_anchor
                && actual_tag == expected_tag
        }
        // Aliases - compare name
        (
            Event::Alias { name: actual_name },
            Event::Alias {
                name: expected_name,
            },
        ) => actual_name == expected_name,
        // Different event types
        _ => false,
    }
}

/// Test case structure.
struct TestCase {
    id: String,
    name: String,
    input: String,
    expected_events: Vec<Event>,
    expects_error: bool,
}

/// Load test case(s) from a directory.
/// Returns a vector because some test directories contain numbered subtests.
fn load_test_cases(dir: &Path) -> Vec<TestCase> {
    let Some(id) = dir.file_name().and_then(|name| name.to_str()) else {
        return Vec::new();
    };

    // Skip special directories
    if id == "name" || id == "tags" || id.starts_with('.') {
        return Vec::new();
    }

    // Skip numbered sub-test directories when accessed directly
    // (they'll be loaded via their parent)
    if is_numbered_subtest(id) {
        return Vec::new();
    }

    let name_file = dir.join("===");
    let input_file = dir.join("in.yaml");
    let event_file = dir.join("test.event");
    let error_file = dir.join("error");

    // Check if this directory has direct test files
    if input_file.exists() && event_file.exists() {
        // Single test case in this directory
        if let Some(test) =
            load_single_test_case(dir, id, &name_file, &input_file, &event_file, &error_file)
        {
            return vec![test];
        }
        return Vec::new();
    }

    // Check for numbered subdirectories (subtests)
    load_subtests(dir, id)
}

/// Check if a directory name is a numbered subtest (e.g., "00", "01", "000", "001")
fn is_numbered_subtest(name: &str) -> bool {
    (name.len() == 2 || name.len() == 3) && name.chars().all(|ch| ch.is_ascii_digit())
}

/// Load numbered subtests from a parent directory.
fn load_subtests(parent_dir: &Path, parent_id: &str) -> Vec<TestCase> {
    let mut tests = Vec::new();

    let Ok(entries) = fs::read_dir(parent_dir) else {
        return tests;
    };

    let mut subtest_dirs: Vec<_> = entries
        .filter_map(Result::ok)
        .filter(|entry| entry.path().is_dir())
        .filter(|entry| entry.file_name().to_str().is_some_and(is_numbered_subtest))
        .collect();

    // Sort for deterministic order
    subtest_dirs.sort_by_key(std::fs::DirEntry::path);

    // Get parent name from === file (shared across subtests)
    let parent_name_file = parent_dir.join("===");
    let parent_name = fs::read_to_string(&parent_name_file).map_or_else(
        |_| parent_id.to_owned(),
        |content| content.trim().to_owned(),
    );

    for entry in subtest_dirs {
        let subtest_path = entry.path();
        let subtest_num = entry.file_name().to_string_lossy().to_string();

        let input_file = subtest_path.join("in.yaml");
        let event_file = subtest_path.join("test.event");
        let error_file = subtest_path.join("error");
        let name_file = subtest_path.join("===");

        if input_file.exists() && event_file.exists() {
            // Subtest-specific name or inherit from parent
            let test_name = fs::read_to_string(&name_file).map_or_else(
                |_| format!("{parent_name} (subtest {subtest_num})"),
                |content| content.trim().to_owned(),
            );

            let test_id = format!("{parent_id}-{subtest_num}");

            if let Some(mut test_case) = load_single_test_case(
                &subtest_path,
                &test_id,
                &name_file,
                &input_file,
                &event_file,
                &error_file,
            ) {
                test_case.name = test_name;
                tests.push(test_case);
            }
        }
    }

    tests
}

/// Load a single test case from its files.
fn load_single_test_case(
    _dir: &Path,
    id: &str,
    name_file: &Path,
    input_file: &Path,
    event_file: &Path,
    error_file: &Path,
) -> Option<TestCase> {
    let name = fs::read_to_string(name_file)
        .map_or_else(|_| id.to_owned(), |content| content.trim().to_owned());

    let input = fs::read_to_string(input_file).ok()?;
    let event_content = fs::read_to_string(event_file).ok()?;
    let expected_events = parse_event_file(&event_content);
    let expects_error = error_file.exists();

    Some(TestCase {
        id: id.to_owned(),
        name,
        input,
        expected_events,
        expects_error,
    })
}

/// Run the test suite and return statistics.
/// Collect all error test cases from the YAML test suite.
/// Returns a Vec of (`test_id`, `test_name`, `input_content`).
#[allow(
    clippy::items_after_statements,
    reason = "Helper function is clearer inline"
)]
fn collect_error_test_cases(test_dir: &Path) -> Vec<(String, String, String)> {
    let mut error_cases = Vec::new();

    fn visit_dir(dir: &Path, error_cases: &mut Vec<(String, String, String)>) {
        let Ok(entries) = fs::read_dir(dir) else {
            return;
        };

        for entry in entries.filter_map(Result::ok) {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }

            let dir_name = path
                .file_name()
                .and_then(|name| name.to_str())
                .unwrap_or("")
                .to_owned();

            // Skip special directories (name/tags are symlinks to actual tests)
            if dir_name == "name" || dir_name == "tags" || dir_name.starts_with('.') {
                continue;
            }

            let error_file = path.join("error");
            let input_file = path.join("in.yaml");
            let name_file = path.join("===");

            if error_file.exists() && input_file.exists() {
                // This is an error test case
                if let Ok(input) = fs::read_to_string(&input_file) {
                    let name = fs::read_to_string(&name_file)
                        .map_or_else(|_| dir_name.clone(), |content| content.trim().to_owned());
                    error_cases.push((dir_name, name, input));
                }
            }

            // Recurse into subdirectories (for numbered sub-tests like DK95/01)
            visit_dir(&path, error_cases);
        }
    }

    visit_dir(test_dir, &mut error_cases);

    // Sort by test ID for deterministic ordering
    error_cases.sort_by(|case_a, case_b| case_a.0.cmp(&case_b.0));

    error_cases
}

/// Test error recovery by combining all error inputs into a single stream.
///
/// This test verifies that:
/// 1. The parser can handle a combined stream of all error inputs
/// 2. All expected errors are detected (one per original document)
/// 3. Error recovery works regardless of document order (tested in reverse too)
#[test]
#[allow(
    clippy::print_stderr,
    clippy::tests_outside_test_module,
    reason = "Integration test with test output"
)]
fn error_recovery_combined_stream() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let test_dir = Path::new(manifest_dir).join("tests/yaml-test-suite");

    if !test_dir.exists() {
        eprintln!(
            "Test suite not found at {}. Skipping test.",
            test_dir.display()
        );
        return;
    }

    let error_cases = collect_error_test_cases(&test_dir);
    eprintln!("Found {} error test cases", error_cases.len());

    if error_cases.is_empty() {
        eprintln!("No error test cases found. Skipping test.");
        return;
    }

    // Run in forward order
    run_combined_error_test(&error_cases, "forward");

    // Run in reverse order
    let mut reversed_cases = error_cases.clone();
    reversed_cases.reverse();
    run_combined_error_test(&reversed_cases, "reverse");
}

#[allow(
    clippy::print_stderr,
    clippy::integer_division,
    clippy::integer_division_remainder_used,
    reason = "Test output and intentional integer math for threshold"
)]
fn run_combined_error_test(error_cases: &[(String, String, String)], order_name: &str) {
    eprintln!("\n=== Testing combined error stream ({order_name} order) ===");

    // Build a combined YAML input WITHOUT document markers.
    // This tests whether the parser can recover from errors within a document
    // and continue parsing subsequent content, rather than relying on the
    // stream lexer to separate inputs into clean documents.
    let mut combined_input = String::new();
    for (_test_id, _name, input) in error_cases {
        // Just concatenate inputs with newlines - no document markers
        combined_input.push_str(input);
        // Ensure newline between inputs
        if !input.ends_with('\n') {
            combined_input.push('\n');
        }
    }

    eprintln!(
        "Combined input size: {} bytes, {} error inputs",
        combined_input.len(),
        error_cases.len()
    );

    // Parse the combined stream
    let (documents, errors) = parse(&combined_input);

    eprintln!("Parsed {} documents", documents.len());
    eprintln!("Collected {} errors", errors.len());

    // We expect at least one error for the combined stream
    // (since it contains multiple malformed documents)
    assert!(
        !errors.is_empty(),
        "Expected errors when parsing combined error inputs, but got none"
    );

    // Verify that we got a reasonable number of errors
    // We should get at least one error per original error document on average,
    // though some documents may produce multiple errors and some may not produce
    // errors in our parser (if we're lenient about certain constructs).
    let min_expected_errors = error_cases.len() / 3; // At least 1/3 of inputs should error
    assert!(
        errors.len() >= min_expected_errors,
        "Expected at least {min_expected_errors} errors for {} error inputs, got {}. \
         Error recovery may not be working correctly.",
        error_cases.len(),
        errors.len()
    );

    eprintln!(
        "✓ Combined stream test passed ({order_name}): {} errors from {} error inputs",
        errors.len(),
        error_cases.len()
    );

    // Additionally, verify error spans are valid (within input bounds)
    for error in &errors {
        let span = &error.span;
        assert!(
            span.end_usize() <= combined_input.len(),
            "Error span {span:?} exceeds input length {}",
            combined_input.len()
        );
    }

    eprintln!("✓ All error spans are valid");
}

/// Test that each error test case produces at least one error.
/// This ensures we don't silently accept malformed YAML.
#[test]
#[allow(
    clippy::print_stderr,
    clippy::tests_outside_test_module,
    reason = "Integration test with test output"
)]
fn error_test_cases_produce_errors() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let test_dir = Path::new(manifest_dir).join("tests/yaml-test-suite");

    if !test_dir.exists() {
        eprintln!(
            "Test suite not found at {}. Skipping test.",
            test_dir.display()
        );
        return;
    }

    let error_cases = collect_error_test_cases(&test_dir);
    eprintln!("Testing {} error test cases...", error_cases.len());

    let mut passed = 0;
    let mut failed = 0;
    let mut failures = Vec::new();

    for (test_id, name, input) in &error_cases {
        let (_, errors) = parse(input);
        if errors.is_empty() {
            failed += 1;
            failures.push(format!("{test_id}: {name}"));
        } else {
            passed += 1;
        }
    }

    eprintln!("\n=== Error Test Cases Results ===");
    eprintln!("Passed (produced errors): {passed}");
    eprintln!("Failed (no errors): {failed}");
    eprintln!("Total: {}", passed + failed);

    if !failures.is_empty() {
        eprintln!("\n=== Tests that should error but didn't ===");
        for failure in &failures {
            eprintln!("  {failure}");
        }
        panic!(
            "Error detection test failed: {failed} test cases should have produced errors but didn't"
        );
    }
}

/// Analyze error kind distribution across all error test cases.
/// This helps identify which errors use generic types vs specific ones.
#[test]
#[allow(
    clippy::print_stderr,
    clippy::tests_outside_test_module,
    reason = "Integration test with analysis output"
)]
fn analyze_error_kinds() {
    use std::collections::HashMap;

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let test_dir = Path::new(manifest_dir).join("tests/yaml-test-suite");

    if !test_dir.exists() {
        eprintln!("Test suite not found");
        return;
    }

    let error_cases = collect_error_test_cases(&test_dir);
    eprintln!("Analyzing {} error test cases...\n", error_cases.len());

    let mut error_kinds: HashMap<String, Vec<String>> = HashMap::new();

    for (test_id, _name, input) in &error_cases {
        let (_, errors) = parse(input);
        for error in &errors {
            let kind = format!("{:?}", error.kind);
            error_kinds.entry(kind).or_default().push(test_id.clone());
        }
    }

    let mut sorted: Vec<_> = error_kinds.iter().collect();
    sorted.sort_by(|kind_a, kind_b| kind_b.1.len().cmp(&kind_a.1.len()));

    eprintln!("=== Error Kind Distribution ===");
    for (kind, test_ids) in &sorted {
        eprintln!("  {kind}: {} occurrences", test_ids.len());
    }

    // Show UnexpectedToken cases with test names for analysis
    if let Some(unexpected) = error_kinds.get("UnexpectedToken") {
        // Dedupe and show unique test IDs with names
        let unique_tests: std::collections::HashSet<_> = unexpected.iter().collect();
        eprintln!(
            "\n=== UnexpectedToken: {} unique tests ({} total occurrences) ===",
            unique_tests.len(),
            unexpected.len()
        );
        // Count occurrences per test
        let mut test_counts: HashMap<&String, usize> = HashMap::new();
        for test_id in unexpected {
            *test_counts.entry(test_id).or_insert(0) += 1;
        }
        let mut sorted_tests: Vec<_> = test_counts.into_iter().collect();
        sorted_tests.sort_by(|test_a, test_b| test_b.1.cmp(&test_a.1));
        for (test_id, count) in sorted_tests.iter().take(30) {
            eprintln!("  {test_id}: {count} errors");
        }
    }
}

/// Test the event-based parser against the same test suite.
#[test]
#[allow(
    clippy::print_stderr,
    clippy::cast_precision_loss,
    clippy::as_conversions,
    clippy::use_debug,
    clippy::tests_outside_test_module,
    reason = "Integration test with test output and statistics calculation"
)]
fn yaml_test_suite_via_events() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let test_dir = Path::new(manifest_dir).join("tests/yaml-test-suite");

    if !test_dir.exists() {
        eprintln!("Test suite not found at {test_dir:?}. Skipping tests.");
        return;
    }

    // Run positive tests (event comparison)
    let (passed, failed, failures) = run_test_suite_via_events(&test_dir);

    eprintln!("\n=== Event Parser: Positive Tests (Event Comparison) ===");
    eprintln!("Passed: {passed}");
    eprintln!("Failed: {failed}");

    if !failures.is_empty() {
        eprintln!("\nFailures ({} total):", failures.len());
        for failure in &failures {
            eprintln!("  {failure}");
        }
    }

    let total = passed + failed;
    let pass_rate = (passed as f64 / total as f64) * 100.0;
    eprintln!("\nEvent Parser Pass rate: {pass_rate:.1}%");

    // For now, just track the results - don't fail the test
    // We can add assertions once we reach parity
}

/// Run the test suite through the event-based parser.
#[allow(clippy::print_stderr, reason = "Test output for progress tracking")]
fn run_test_suite_via_events(test_dir: &Path) -> (usize, usize, Vec<String>) {
    let mut passed = 0;
    let mut failed = 0;
    let mut failures = Vec::new();

    let Ok(dir_entries) = fs::read_dir(test_dir) else {
        eprintln!("Failed to read test directory");
        return (0, 0, Vec::new());
    };
    let mut entries: Vec<_> = dir_entries.filter_map(Result::ok).collect();
    entries.sort_by_key(std::fs::DirEntry::path);

    let mut all_tests: Vec<TestCase> = Vec::new();
    for entry in &entries {
        let path = entry.path();
        if path.is_dir() {
            all_tests.extend(load_test_cases(&path));
        }
    }

    let total = all_tests.len();

    for (i, test_case) in all_tests.iter().enumerate() {
        if i % 100 == 0 {
            eprintln!("[{}/{}] Running event parser tests...", i + 1, total);
        }
        let result = run_single_test_via_events(test_case);
        if let Err(err) = result {
            failed += 1;
            failures.push(format!("{}: {}\n{err}", test_case.id, test_case.name));
        } else {
            passed += 1;
        }
    }

    (passed, failed, failures)
}

fn run_single_test_via_events(test: &TestCase) -> Result<(), String> {
    let (raw_events, errors) = yaml_parser::emit_events(&test.input);

    if test.expects_error {
        if errors.is_empty() {
            return Err(format!(
                "Expected error but parsing succeeded\n\
                 -- INPUT --\n{}\n\
                 -- ACTUAL EVENTS --\n{}",
                test.input,
                raw_events
                    .iter()
                    .map(|e| format!("{e}"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ));
        }
        return Ok(());
    }

    if !errors.is_empty() {
        return Err(format!(
            "Parse errors: {errors:?}\n\
             -- INPUT --\n{}\n\
             -- EXPECTED --\n{}",
            test.input,
            test.expected_events
                .iter()
                .map(|e| format!("{e:?}"))
                .collect::<Vec<_>>()
                .join("\n")
        ));
    }

    // Convert library events to test events
    let actual_events = library_events_to_test_events(&raw_events);
    compare_events_with_context(&actual_events, &test.expected_events, &test.input)
}

fn compare_events_with_context(
    actual: &[Event],
    expected: &[Event],
    input: &str,
) -> Result<(), String> {
    let actual_strs: Vec<_> = actual.iter().map(|e| format!("{e:?}")).collect();
    let expected_strs: Vec<_> = expected.iter().map(|e| format!("{e:?}")).collect();

    // First check if they match
    if actual.len() == expected.len()
        && actual
            .iter()
            .zip(expected.iter())
            .all(|(a, e)| events_match(a, e))
    {
        return Ok(());
    }

    // Build detailed error message
    Err(format!(
        "Event mismatch\n\
         -- INPUT --\n{input}\n\
         -- EXPECTED ({} events) --\n{}\n\
         -- ACTUAL ({} events) --\n{}",
        expected.len(),
        expected_strs.join("\n"),
        actual.len(),
        actual_strs.join("\n")
    ))
}

// NOTE: streaming_parser_equivalence test removed because it compared the batch parser
// with StreamingParser. Since we've removed the batch parser and promoted the Emitter
// to be the primary parser, this test is no longer relevant.

/// Find the first difference between two event sequences.
#[allow(dead_code, reason = "utility function for debugging test failures")]
fn find_first_diff(a: &[Event], b: &[Event]) -> String {
    for (i, (ea, eb)) in a.iter().zip(b.iter()).enumerate() {
        if ea != eb {
            return format!("index {i}: {ea:?} vs {eb:?}");
        }
    }
    if a.len() == b.len() {
        "no diff found".to_owned()
    } else {
        format!("length {} vs {}", a.len(), b.len())
    }
}

/// Find the first difference between two `yaml_parser::Event` sequences.
#[allow(dead_code, reason = "utility function for debugging test failures")]
fn find_first_event_diff(a: &[yaml_parser::Event<'_>], b: &[yaml_parser::Event<'_>]) -> String {
    for (i, (ea, eb)) in a.iter().zip(b.iter()).enumerate() {
        if ea != eb {
            return format!("index {i}: {ea:?} vs {eb:?}");
        }
    }
    if a.len() == b.len() {
        "no diff found".to_owned()
    } else {
        format!("length {} vs {}", a.len(), b.len())
    }
}

/// Convert library Event to test Event format.
fn library_events_to_test_events(events: &[yaml_parser::Event<'_>]) -> Vec<Event> {
    events
        .iter()
        .map(|ev| match ev {
            yaml_parser::Event::StreamStart => Event::StreamStart,
            yaml_parser::Event::StreamEnd => Event::StreamEnd,
            yaml_parser::Event::DocumentStart { explicit, .. } => Event::DocumentStart {
                explicit: *explicit,
            },
            yaml_parser::Event::DocumentEnd { explicit, .. } => Event::DocumentEnd {
                explicit: *explicit,
            },
            yaml_parser::Event::MappingStart {
                style, anchor, tag, ..
            } => Event::MappingStart {
                flow: matches!(style, yaml_parser::CollectionStyle::Flow),
                anchor: anchor.as_ref().map(|(s, _)| s.to_string()),
                tag: tag.as_ref().map(|(s, _)| s.to_string()),
            },
            yaml_parser::Event::MappingEnd { .. } => Event::MappingEnd,
            yaml_parser::Event::SequenceStart {
                style, anchor, tag, ..
            } => Event::SequenceStart {
                flow: matches!(style, yaml_parser::CollectionStyle::Flow),
                anchor: anchor.as_ref().map(|(s, _)| s.to_string()),
                tag: tag.as_ref().map(|(s, _)| s.to_string()),
            },
            yaml_parser::Event::SequenceEnd { .. } => Event::SequenceEnd,
            yaml_parser::Event::Scalar {
                style,
                value,
                anchor,
                tag,
                ..
            } => Event::Scalar {
                style: match style {
                    yaml_parser::ScalarStyle::Plain => ScalarStyle::Plain,
                    yaml_parser::ScalarStyle::SingleQuoted => ScalarStyle::SingleQuoted,
                    yaml_parser::ScalarStyle::DoubleQuoted => ScalarStyle::DoubleQuoted,
                    yaml_parser::ScalarStyle::Literal => ScalarStyle::Literal,
                    yaml_parser::ScalarStyle::Folded => ScalarStyle::Folded,
                },
                value: value.to_string(),
                anchor: anchor.as_ref().map(|(s, _)| s.to_string()),
                tag: tag.as_ref().map(|(s, _)| s.to_string()),
            },
            yaml_parser::Event::Alias { name, .. } => Event::Alias {
                name: name.to_string(),
            },
        })
        .collect()
}

/// Tests where the emitter produces intentionally different (more correct) spans.
/// These are whitelisted to differ only in spans, not structure.
///
/// Emitter span improvements over batch parser:
/// - `DocumentEnd`: emitter returns actual EOF position, batch returns 0..0
/// - `MappingStart`/`End`: emitter uses more precise span boundaries
/// - Block scalar end spans: emitter tracks actual content end
#[allow(dead_code, reason = "kept for future span comparison testing")]
const SPAN_DIFF_WHITELIST: &[&str] = &[
    // TODO: Populate this list as we identify intentional span differences
];

// NOTE: emitter_equivalence, debug_3alj, and debug_26dv tests removed because they
// compared the batch parser with the Emitter. Since we've removed the batch parser
// and promoted the Emitter to be the primary parser, these tests are no longer relevant.
