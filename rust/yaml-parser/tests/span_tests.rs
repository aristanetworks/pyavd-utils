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

use support::emit_events_ok;
use support::parse_ok;
use yaml_parser::Event;
use yaml_parser::Position;
use yaml_parser::ScalarStyle;
use yaml_parser::SourceMap;
use yaml_parser::Value;
use yaml_parser::parse;

/// Helper to extract the text covered by a span from the input.
fn extract_span_text(input: &str, start: usize, end: usize) -> &str {
    &input[start..end]
}

fn assert_span_offsets_and_text(
    input: &str,
    actual_start: usize,
    actual_end: usize,
    expected_start: usize,
    expected_end: usize,
    expected_text: &str,
) {
    assert_eq!(actual_start, expected_start, "unexpected span start");
    assert_eq!(actual_end, expected_end, "unexpected span end");
    assert_eq!(
        extract_span_text(input, actual_start, actual_end),
        expected_text,
        "unexpected span text",
    );
}

fn assert_span_positions(
    map: &SourceMap,
    actual_start: usize,
    actual_end: usize,
    expected_start: Position,
    expected_end: Position,
) {
    assert_eq!(
        map.position(actual_start),
        expected_start,
        "unexpected span start position"
    );
    assert_eq!(
        map.position(actual_end),
        expected_end,
        "unexpected span end position"
    );
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
    let input = "outer:\r\n  inner: value\r  other: more";
    let map = SourceMap::new(input);
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
        "outer:\r\n  inner: value\r  other: more",
    );
    assert_span_positions(
        &map,
        pairs[0].pair_span.start_usize(),
        pairs[0].pair_span.end_usize(),
        Position::new(1, 1),
        Position::new(3, 14),
    );

    let inner_pairs = match &pairs[0].value.value {
        Value::Mapping(inner_pairs) => inner_pairs,
        _ => panic!("Expected nested mapping"),
    };
    assert_eq!(inner_pairs.len(), 2);
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
    assert_span_positions(
        &map,
        inner_pairs[0].key.span.start_usize(),
        inner_pairs[0].key.span.end_usize(),
        Position::new(2, 3),
        Position::new(2, 8),
    );
    assert_span_positions(
        &map,
        inner_pairs[0].value.span.start_usize(),
        inner_pairs[0].value.span.end_usize(),
        Position::new(2, 10),
        Position::new(2, 15),
    );

    assert_eq!(
        extract_span_text(
            input,
            inner_pairs[1].pair_span.start_usize(),
            inner_pairs[1].pair_span.end_usize(),
        ),
        "other: more",
    );
    assert_span_positions(
        &map,
        inner_pairs[1].key.span.start_usize(),
        inner_pairs[1].key.span.end_usize(),
        Position::new(3, 3),
        Position::new(3, 8),
    );
    assert_span_positions(
        &map,
        inner_pairs[1].value.span.start_usize(),
        inner_pairs[1].value.span.end_usize(),
        Position::new(3, 10),
        Position::new(3, 14),
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

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "this is a deliberate byte-offset matrix for a complex Unicode fixture"
)]
fn test_unicode_byte_spans_across_nested_structures() {
    let input = concat!(
        "ascii🙂mix: café🚀\n",
        "絵文字: 🌍🎉\n",
        "純粋: [😀, βeta🎯]\n",
        "mix🎉key:\n",
        "  inner漢: \"välue🌍\"\n",
        "  emoji😀: plain🚀text\n",
    );
    let docs = parse_ok(input);

    assert_eq!(docs.len(), 1);
    let doc = &docs[0];

    // Byte layout:
    // - "ascii🙂mix" = 5 ASCII + 4-byte emoji + 3 ASCII = 12 bytes
    // - "café🚀" = 3 ASCII + 2-byte "é" + 4-byte emoji = 9 bytes
    // - "絵文字" = 3 CJK chars = 9 bytes
    // - "🌍🎉" = 2 emojis = 8 bytes
    // - "純粋" = 2 CJK chars = 6 bytes
    // - "βeta🎯" = 2-byte beta + 3 ASCII + 4-byte emoji = 9 bytes
    // - "mix🎉key" = 3 ASCII + 4-byte emoji + 3 ASCII = 10 bytes
    // - "inner漢" = 5 ASCII + 3-byte CJK = 8 bytes
    // - "\"välue🌍\"" = quotes + 1 ASCII + 2-byte "ä" + 4 ASCII + 4-byte emoji = 12 bytes
    // - "emoji😀" = 5 ASCII + 4-byte emoji = 9 bytes
    // - "plain🚀text" = 5 ASCII + 4-byte emoji + 4 ASCII = 13 bytes
    //
    // Expected byte ranges:
    // full input      0..134  (final newline at 133..134)
    // doc             0..133  (document/value span intentionally excludes the trailing newline)
    // pair 1 key      0..12   value 14..23   pair 0..23
    // pair 2 key      24..33  value 35..43   pair 24..43
    // pair 3 key      44..50  value 52..69   pair 44..69
    //   seq item 1    53..57
    //   seq item 2    59..68
    // pair 4 key      70..80  value 84..133  pair 70..133
    //   inner key 1   84..92  value 94..106  pair 84..106
    //   inner key 2   109..118 value 120..133 pair 109..133
    assert_span_offsets_and_text(
        input,
        doc.span.start_usize(),
        doc.span.end_usize(),
        0,
        133,
        concat!(
            "ascii🙂mix: café🚀\n",
            "絵文字: 🌍🎉\n",
            "純粋: [😀, βeta🎯]\n",
            "mix🎉key:\n",
            "  inner漢: \"välue🌍\"\n",
            "  emoji😀: plain🚀text",
        ),
    );

    let pairs = match &doc.value {
        Value::Mapping(pairs) => pairs,
        _ => panic!("Expected mapping"),
    };
    assert_eq!(pairs.len(), 4);

    assert_span_offsets_and_text(
        input,
        pairs[0].key.span.start_usize(),
        pairs[0].key.span.end_usize(),
        0,
        12,
        "ascii🙂mix",
    );
    assert_span_offsets_and_text(
        input,
        pairs[0].value.span.start_usize(),
        pairs[0].value.span.end_usize(),
        14,
        23,
        "café🚀",
    );
    assert_span_offsets_and_text(
        input,
        pairs[0].pair_span.start_usize(),
        pairs[0].pair_span.end_usize(),
        0,
        23,
        "ascii🙂mix: café🚀",
    );

    assert_span_offsets_and_text(
        input,
        pairs[1].key.span.start_usize(),
        pairs[1].key.span.end_usize(),
        24,
        33,
        "絵文字",
    );
    assert_span_offsets_and_text(
        input,
        pairs[1].value.span.start_usize(),
        pairs[1].value.span.end_usize(),
        35,
        43,
        "🌍🎉",
    );
    assert_span_offsets_and_text(
        input,
        pairs[1].pair_span.start_usize(),
        pairs[1].pair_span.end_usize(),
        24,
        43,
        "絵文字: 🌍🎉",
    );

    assert_span_offsets_and_text(
        input,
        pairs[2].key.span.start_usize(),
        pairs[2].key.span.end_usize(),
        44,
        50,
        "純粋",
    );
    assert_span_offsets_and_text(
        input,
        pairs[2].value.span.start_usize(),
        pairs[2].value.span.end_usize(),
        52,
        69,
        "[😀, βeta🎯]",
    );
    assert_span_offsets_and_text(
        input,
        pairs[2].pair_span.start_usize(),
        pairs[2].pair_span.end_usize(),
        44,
        69,
        "純粋: [😀, βeta🎯]",
    );

    let sequence = match &pairs[2].value.value {
        Value::Sequence(items) => items,
        _ => panic!("Expected flow sequence"),
    };
    assert_eq!(sequence.len(), 2);
    assert_span_offsets_and_text(
        input,
        sequence[0].node.span.start_usize(),
        sequence[0].node.span.end_usize(),
        53,
        57,
        "😀",
    );
    assert_span_offsets_and_text(
        input,
        sequence[1].node.span.start_usize(),
        sequence[1].node.span.end_usize(),
        59,
        68,
        "βeta🎯",
    );

    assert_span_offsets_and_text(
        input,
        pairs[3].key.span.start_usize(),
        pairs[3].key.span.end_usize(),
        70,
        80,
        "mix🎉key",
    );
    assert_span_offsets_and_text(
        input,
        pairs[3].value.span.start_usize(),
        pairs[3].value.span.end_usize(),
        84,
        133,
        "inner漢: \"välue🌍\"\n  emoji😀: plain🚀text",
    );
    assert_span_offsets_and_text(
        input,
        pairs[3].pair_span.start_usize(),
        pairs[3].pair_span.end_usize(),
        70,
        133,
        "mix🎉key:\n  inner漢: \"välue🌍\"\n  emoji😀: plain🚀text",
    );

    let inner_pairs = match &pairs[3].value.value {
        Value::Mapping(nested_pairs) => nested_pairs,
        _ => panic!("Expected nested mapping"),
    };
    assert_eq!(inner_pairs.len(), 2);

    assert_span_offsets_and_text(
        input,
        inner_pairs[0].key.span.start_usize(),
        inner_pairs[0].key.span.end_usize(),
        84,
        92,
        "inner漢",
    );
    assert_span_offsets_and_text(
        input,
        inner_pairs[0].value.span.start_usize(),
        inner_pairs[0].value.span.end_usize(),
        94,
        106,
        "\"välue🌍\"",
    );
    assert_span_offsets_and_text(
        input,
        inner_pairs[0].pair_span.start_usize(),
        inner_pairs[0].pair_span.end_usize(),
        84,
        106,
        "inner漢: \"välue🌍\"",
    );

    assert_span_offsets_and_text(
        input,
        inner_pairs[1].key.span.start_usize(),
        inner_pairs[1].key.span.end_usize(),
        109,
        118,
        "emoji😀",
    );
    assert_span_offsets_and_text(
        input,
        inner_pairs[1].value.span.start_usize(),
        inner_pairs[1].value.span.end_usize(),
        120,
        133,
        "plain🚀text",
    );
    assert_span_offsets_and_text(
        input,
        inner_pairs[1].pair_span.start_usize(),
        inner_pairs[1].pair_span.end_usize(),
        109,
        133,
        "emoji😀: plain🚀text",
    );
}
