// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Basic tests for the event-based YAML writer.

#![allow(
    clippy::tests_outside_test_module,
    reason = "integration tests in tests/ are top-level by design"
)]
#![allow(
    clippy::expect_used,
    reason = "panicking on unexpected writer failure is fine in these focused tests"
)]

use yaml_parser::{emit_events, parse, writer};

fn roundtrip_value(input: &str) {
    let (docs_before, errors_before) = parse(input);
    assert!(
        errors_before.is_empty(),
        "expected no errors before roundtrip, got: {errors_before:?}"
    );

    let (events, errors_events) = emit_events(input);
    assert!(
        errors_events.is_empty(),
        "expected no errors from emit_events, got: {errors_events:?}"
    );

    let mut buf = Vec::new();
    writer::write_yaml_from_events(&mut buf, &events)
        .expect("writing YAML from events should succeed");

    let output = String::from_utf8(buf).expect("writer must produce valid UTF-8");

    let (docs_after, errors_after) = parse(&output);
    assert!(
        errors_after.is_empty(),
        "expected no errors after roundtrip, got: {errors_after:?}\nOUTPUT:\n{output}",
    );

    assert_eq!(
        docs_before.len(),
        docs_after.len(),
        "document count changed"
    );

    for (before, after) in docs_before.iter().zip(docs_after.iter()) {
        assert_eq!(before.value, after.value, "value changed after roundtrip");
    }
}

#[test]
fn writer_roundtrip_simple_mapping() {
    roundtrip_value("key: value\n");
}

#[test]
fn writer_roundtrip_sequence() {
    roundtrip_value("- a\n- b\n- c\n");
}

#[test]
fn writer_roundtrip_nested_structures() {
    let input = "outer:\n  inner:\n    - item1\n    - item2\n";
    roundtrip_value(input);
}

#[test]
fn writer_roundtrip_anchors_and_tags() {
    let input = "anchor_node: &a 1\nalias_node: *a\n!!map-tag tagged: !tagged 2\n";
    roundtrip_value(input);
}

#[test]
fn writer_roundtrip_big_integer() {
    let input = "huge: 12345678901234567890123456789012345678901234567890\n";
    roundtrip_value(input);
}
