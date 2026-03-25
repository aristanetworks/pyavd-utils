// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Comprehensive span correctness tests.
//!
//! This module tests that spans accurately represent the byte offsets
//! of YAML constructs in the input string.

#![allow(clippy::indexing_slicing, reason = "panics are acceptable in tests")]
#![allow(clippy::string_slice, reason = "test code with known-safe slicing")]
#![allow(
    clippy::tests_outside_test_module,
    reason = "integration tests don't need cfg(test)"
)]
#![allow(clippy::expect_used, reason = "expect is acceptable in tests")]
#![allow(clippy::panic, reason = "panic is acceptable in tests")]
#![allow(
    clippy::manual_let_else,
    reason = "match expressions are acceptable in test setup"
)]

mod support;

use support::{emit_events_ok, parse_ok};
use yaml_parser::{Event, ScalarStyle, Value, parse};

/// Helper to extract the text covered by a span from the input.
fn extract_span_text(input: &str, start: usize, end: usize) -> &str {
    &input[start..end]
}

#[test]
fn test_scalar_spans_plain() {
    let input = "hello";
    let events = emit_events_ok(input);

    // Find the scalar event
    let scalar = events
        .iter()
        .find_map(|event| match event {
            Event::Scalar { value, span, .. } => Some((value, span)),
            _ => None,
        })
        .expect("Should have scalar event");

    let span_text = extract_span_text(input, scalar.1.start_usize(), scalar.1.end_usize());
    assert_eq!(span_text, "hello", "Span should cover the scalar value");
    assert_eq!(scalar.0.as_ref(), "hello");
}

#[test]
fn test_scalar_spans_quoted() {
    let input = r#""hello world""#;
    let events = emit_events_ok(input);

    let scalar = events
        .iter()
        .find_map(|event| match event {
            Event::Scalar {
                value, span, style, ..
            } => Some((value, span, style)),
            _ => None,
        })
        .expect("Should have scalar event");

    assert_eq!(scalar.2, &ScalarStyle::DoubleQuoted);
    let span_text = extract_span_text(input, scalar.1.start_usize(), scalar.1.end_usize());
    // Span should include the quotes
    assert_eq!(span_text, r#""hello world""#, "Span should include quotes");
    assert_eq!(
        scalar.0.as_ref(),
        "hello world",
        "Value should not include quotes"
    );
}

#[test]
fn test_scalar_spans_block_literal() {
    let input = "key: |\n  line1\n  line2\n";
    let events = emit_events_ok(input);

    let scalar = events
        .iter()
        .find_map(|event| match event {
            Event::Scalar {
                value, span, style, ..
            } if *style == ScalarStyle::Literal => Some((value, span)),
            _ => None,
        })
        .expect("Should have literal scalar event");

    let span_text = extract_span_text(input, scalar.1.start_usize(), scalar.1.end_usize());
    // Span should include the | indicator and content
    assert!(
        span_text.starts_with('|'),
        "Span should include | indicator"
    );
    assert_eq!(scalar.0.as_ref(), "line1\nline2\n");
}

#[test]
fn test_mapping_spans() {
    let input = "key: value";
    let docs = parse_ok(input);

    assert_eq!(docs.len(), 1);
    let doc = &docs[0];

    // The document span should cover the entire input
    let span_text = extract_span_text(input, doc.span.start_usize(), doc.span.end_usize());
    assert_eq!(span_text, "key: value");
}

#[test]
fn test_sequence_spans() {
    let input = "- item1\n- item2";
    let docs = parse_ok(input);

    assert_eq!(docs.len(), 1);
    let doc = &docs[0];

    let items = match &doc.value {
        Value::Sequence(items) => Some(items),
        _ => None,
    }
    .expect("Expected sequence");

    assert_eq!(items.len(), 2);

    // First item span
    let item1_text = extract_span_text(
        input,
        items[0].span.start_usize(),
        items[0].span.end_usize(),
    );
    assert_eq!(item1_text, "item1");

    // Second item span
    let item2_text = extract_span_text(
        input,
        items[1].span.start_usize(),
        items[1].span.end_usize(),
    );
    assert_eq!(item2_text, "item2");
}

#[test]
fn test_sequence_item_structural_spans() {
    let input = "- item1\n- item2";
    let docs = parse_ok(input);

    let items = match &docs[0].value {
        Value::Sequence(items) => items,
        _ => panic!("Expected sequence"),
    };

    assert_eq!(
        extract_span_text(
            input,
            items[0].item_span.start_usize(),
            items[0].item_span.end_usize(),
        ),
        "- item1",
    );
    assert_eq!(
        extract_span_text(
            input,
            items[1].item_span.start_usize(),
            items[1].item_span.end_usize(),
        ),
        "- item2",
    );
    assert_eq!(
        extract_span_text(
            input,
            items[0].span.start_usize(),
            items[0].span.end_usize()
        ),
        "item1",
    );
}

#[test]
fn test_nested_structure_spans() {
    let input = "outer:\n  inner: value";
    let docs = parse_ok(input);

    assert_eq!(docs.len(), 1);
    let doc = &docs[0];

    let pairs = match &doc.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("Expected mapping");
    assert_eq!(pairs.len(), 1);
    let pair = &pairs[0];
    let key = &pair.key;
    let value = &pair.value;

    // Key span
    let key_text = extract_span_text(input, key.span.start_usize(), key.span.end_usize());
    assert_eq!(key_text, "outer");

    // Value is a nested mapping
    let inner_pairs = match &value.value {
        Value::Mapping(inner_pairs) => Some(inner_pairs),
        _ => None,
    }
    .expect("Expected nested mapping");
    let inner_pair = &inner_pairs[0];
    let inner_key = &inner_pair.key;
    let inner_value = &inner_pair.value;
    let inner_key_text = extract_span_text(
        input,
        inner_key.span.start_usize(),
        inner_key.span.end_usize(),
    );
    assert_eq!(inner_key_text, "inner");

    let inner_value_text = extract_span_text(
        input,
        inner_value.span.start_usize(),
        inner_value.span.end_usize(),
    );
    assert_eq!(inner_value_text, "value");
}

#[test]
fn test_mapping_pair_structural_spans() {
    let input = "outer:\n  inner: value";
    let docs = parse_ok(input);

    let pairs = match &docs[0].value {
        Value::Mapping(pairs) => pairs,
        _ => panic!("Expected mapping"),
    };

    assert_eq!(
        extract_span_text(
            input,
            pairs[0].pair_span.start_usize(),
            pairs[0].pair_span.end_usize(),
        ),
        "outer:\n  inner: value",
    );

    let inner_pairs = match &pairs[0].value.value {
        Value::Mapping(inner_pairs) => inner_pairs,
        _ => panic!("Expected nested mapping"),
    };
    assert_eq!(
        extract_span_text(
            input,
            inner_pairs[0].pair_span.start_usize(),
            inner_pairs[0].pair_span.end_usize(),
        ),
        "inner: value",
    );
    assert_eq!(
        extract_span_text(
            input,
            inner_pairs[0].key.span.start_usize(),
            inner_pairs[0].key.span.end_usize(),
        ),
        "inner",
    );
    assert_eq!(
        extract_span_text(
            input,
            inner_pairs[0].value.span.start_usize(),
            inner_pairs[0].value.span.end_usize(),
        ),
        "value",
    );
}

#[test]
fn test_flow_sequence_spans() {
    let input = "[a, b, c]";
    let docs = parse_ok(input);

    assert_eq!(docs.len(), 1);
    let doc = &docs[0];

    // Document span should cover the entire flow sequence
    let span_text = extract_span_text(input, doc.span.start_usize(), doc.span.end_usize());
    assert_eq!(span_text, "[a, b, c]");

    let items = match &doc.value {
        Value::Sequence(items) => Some(items),
        _ => None,
    }
    .expect("Expected sequence");
    assert_eq!(items.len(), 3);

    // Each item should have correct span
    assert_eq!(
        extract_span_text(
            input,
            items[0].span.start_usize(),
            items[0].span.end_usize()
        ),
        "a"
    );
    assert_eq!(
        extract_span_text(
            input,
            items[1].span.start_usize(),
            items[1].span.end_usize()
        ),
        "b"
    );
    assert_eq!(
        extract_span_text(
            input,
            items[2].span.start_usize(),
            items[2].span.end_usize()
        ),
        "c"
    );
}

#[test]
fn test_flow_mapping_spans() {
    let input = "{a: 1, b: 2}";
    let docs = parse_ok(input);

    assert_eq!(docs.len(), 1);
    let doc = &docs[0];

    // Document span should cover the entire flow mapping
    let span_text = extract_span_text(input, doc.span.start_usize(), doc.span.end_usize());
    assert_eq!(span_text, "{a: 1, b: 2}");
}

#[test]
fn test_multiline_scalar_spans() {
    let input = "key: >\n  folded\n  text\n";
    let events = emit_events_ok(input);

    let scalar = events
        .iter()
        .find_map(|event| match event {
            Event::Scalar {
                value, span, style, ..
            } if *style == ScalarStyle::Folded => Some((value, span)),
            _ => None,
        })
        .expect("Should have folded scalar event");

    let span_text = extract_span_text(input, scalar.1.start_usize(), scalar.1.end_usize());
    // Span should include the > indicator and content
    assert!(
        span_text.starts_with('>'),
        "Span should include > indicator"
    );
}

#[test]
fn test_anchor_and_alias_spans() {
    let input = "- &anchor value\n- *anchor";
    let events = emit_events_ok(input);

    // Find the scalar with anchor
    let scalar_with_anchor = events
        .iter()
        .find_map(|event| match event {
            Event::Scalar {
                value,
                span,
                properties,
                ..
            } if properties
                .as_ref()
                .and_then(|event_props| event_props.anchor.as_ref())
                .is_some() =>
            {
                Some((value, span))
            }
            _ => None,
        })
        .expect("Should have scalar with anchor");

    // The scalar span should cover just "value", not the anchor
    let span_text = extract_span_text(
        input,
        scalar_with_anchor.1.start_usize(),
        scalar_with_anchor.1.end_usize(),
    );
    assert_eq!(span_text, "value", "Scalar span should not include anchor");

    // Find the alias
    let alias = events
        .iter()
        .find_map(|event| match event {
            Event::Alias { name, span } => Some((name, span)),
            _ => None,
        })
        .expect("Should have alias event");

    // Alias span should cover the *anchor reference
    let alias_span_text = extract_span_text(input, alias.1.start_usize(), alias.1.end_usize());
    assert_eq!(
        alias_span_text, "*anchor",
        "Alias span should include * and name"
    );
}

#[test]
fn test_tag_spans() {
    let input = "!!str value";
    let events = emit_events_ok(input);

    let scalar = events
        .iter()
        .find_map(|event| match event {
            Event::Scalar {
                value,
                span,
                properties,
                ..
            } => Some((
                value,
                span,
                properties
                    .as_ref()
                    .and_then(|event_props| event_props.tag.as_ref()),
            )),
            _ => None,
        })
        .expect("Should have scalar with tag");

    // The scalar span should cover just "value", not the tag
    let span_text = extract_span_text(input, scalar.1.start_usize(), scalar.1.end_usize());
    assert_eq!(span_text, "value", "Scalar span should not include tag");
    assert!(scalar.2.is_some(), "Should have tag");
}

#[test]
fn test_empty_scalar_spans() {
    let input = "key:";
    let docs = parse_ok(input);

    assert_eq!(docs.len(), 1);
    let doc = &docs[0];
    let pairs = match &doc.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("Expected mapping");
    let value = &pairs[0].value;

    // Empty value should have a valid span (even if zero-length)
    assert!(value.span.start_usize() <= value.span.end_usize());
    assert!(value.span.end_usize() <= input.len());
}

#[test]
fn test_error_spans_are_valid() {
    let test_cases = vec![
        "key: [a, , b]",                     // Empty flow sequence item
        "- item\n  - nested\n - bad_indent", // Indentation error
        "{a: 1, b}",                         // Missing value in flow mapping
        "!!invalid value",                   // Invalid tag
    ];

    for input in test_cases {
        let (_, errors) = parse(input);

        for error in &errors {
            // All error spans must be within input bounds
            assert!(
                error.span.start_usize() <= input.len(),
                "Error span start {} exceeds input length {} for input: {:?}",
                error.span.start_usize(),
                input.len(),
                input
            );
            assert!(
                error.span.end_usize() <= input.len(),
                "Error span end {} exceeds input length {} for input: {:?}",
                error.span.end_usize(),
                input.len(),
                input
            );
            assert!(
                error.span.start_usize() <= error.span.end_usize(),
                "Error span start {} > end {} for input: {:?}",
                error.span.start_usize(),
                error.span.end_usize(),
                input
            );
        }
    }
}

#[test]
fn test_document_marker_spans() {
    let input = "---\nkey: value\n...";
    let events = emit_events_ok(input);

    // Find document start
    let doc_start = events
        .iter()
        .find_map(|event| match event {
            Event::DocumentStart { span, explicit, .. } if *explicit => Some(span),
            _ => None,
        })
        .expect("Should have explicit document start");

    let start_text = extract_span_text(input, doc_start.start_usize(), doc_start.end_usize());
    assert_eq!(start_text, "---", "Document start span should cover ---");

    // Find document end
    let doc_end = events
        .iter()
        .find_map(|event| match event {
            Event::DocumentEnd { span, explicit, .. } if *explicit => Some(span),
            _ => None,
        })
        .expect("Should have explicit document end");

    let end_text = extract_span_text(input, doc_end.start_usize(), doc_end.end_usize());
    assert_eq!(end_text, "...", "Document end span should cover ...");
}
