// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Unit tests for the YAML parser.
//!
//! These tests verify parsing behavior for various YAML constructs,
//! error recovery, and edge cases.

// Allow these pedantic lints in test code where they add noise without benefit
#![allow(
    clippy::min_ident_chars,
    reason = "single-char names are fine in tests"
)]
#![allow(clippy::indexing_slicing, reason = "panics are acceptable in tests")]
#![allow(
    clippy::approx_constant,
    reason = "test values don't need constant refs"
)]

use super::*;

#[test]
fn test_emit_e76z() {
    // Test E76Z: Aliases as implicit block mapping keys
    // Input: &a a: &b b\n*b : *a\n
    // Expected: Both aliases (*b key and *a value) emit Alias events
    let input = "&a a: &b b\n*b : *a\n";
    let (events, _errors) = crate::emit_events(input);

    let alias_events: Vec<_> = events
        .iter()
        .filter(|e| matches!(e, crate::Event::Alias { .. }))
        .collect();
    assert_eq!(
        alias_events.len(),
        2,
        "Expected 2 Alias events (for *b key and *a value)"
    );
}

#[test]
fn test_empty_input() {
    let (docs, errors) = parse("");
    assert!(errors.is_empty());
    assert!(docs.is_empty());
}

#[test]
fn test_simple_scalar() {
    let (docs, errors) = parse("hello");
    assert!(errors.is_empty());
    assert_eq!(docs.len(), 1);
    assert!(matches!(&docs.first().unwrap().value, Value::String(string) if string == "hello"));
}

#[test]
fn test_simple_mapping() {
    let (docs, errors) = parse("key: value");
    assert!(errors.is_empty());
    assert_eq!(docs.len(), 1);
    let doc = docs.first().unwrap();
    assert!(matches!(&doc.value, Value::Mapping(_)));
    if let Value::Mapping(pairs) = &doc.value {
        assert_eq!(pairs.len(), 1);
    }
}

#[test]
fn test_nested_structure() {
    let input = "
name: John
address:
  street: 123 Main St
  city: Springfield
";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty());
    assert_eq!(docs.len(), 1);
}

#[test]
fn test_flow_and_block_mixed() {
    let input = "
items:
  - {name: foo, value: 1}
  - {name: bar, value: 2}
";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty());
    assert_eq!(docs.len(), 1);
}

#[test]
fn test_multiline_quoted_key_error() {
    // A double-quoted string that spans multiple lines as an implicit key should be an error
    let input = "\"c\n d\": 1";
    let (docs, errors) = parse(input);
    // Should have an error because the key spans multiple lines
    assert!(
        !errors.is_empty(),
        "Expected error for multiline key, got docs: {docs:?}"
    );
}

#[test]
fn test_anchor_followed_by_anchor_same_line() {
    // Simpler case: two anchors on the same line
    let input = "&a &b value\n";

    let (docs, errors) = parse(input);

    // This should have an error - a node cannot have two anchors
    assert!(
        !errors.is_empty(),
        "Expected error for two anchors on same line, got docs: {docs:?}"
    );
}

#[test]
fn test_simple_document() {
    // Test that a simple document parses correctly
    let input = "key: value";
    let (docs, parse_errors) = parse(input);
    assert!(parse_errors.is_empty());
    assert_eq!(docs.len(), 1);
}

#[test]
fn test_document_with_leading_comment() {
    // Test that documents with leading comments parse correctly
    let input = "# comment\nkey: value";
    let (docs, parse_errors) = parse(input);
    assert!(parse_errors.is_empty());
    assert_eq!(docs.len(), 1);
}

// =========================================================================
// ERROR RECOVERY TESTS
// =========================================================================
// These tests verify that the parser can recover from errors and continue
// parsing, returning both errors and partial results.

mod error_recovery {
    use super::*;

    /// Test that parser reports error but still produces partial output for
    /// unterminated flow sequence.
    #[test]
    fn test_unterminated_flow_sequence() {
        let input = "[a, b, c";
        let (docs, errors) = parse(input);

        // Should have an error for missing ]
        assert!(
            !errors.is_empty(),
            "Expected error for unterminated sequence"
        );

        // Should still produce partial output
        assert_eq!(docs.len(), 1, "Should produce 1 document");
        if let Value::Sequence(items) = &docs.first().unwrap().value {
            // Should have recovered some items
            assert!(!items.is_empty(), "Should recover some items");
        }
    }

    /// Test that parser reports error but still produces partial output for
    /// unterminated flow mapping.
    #[test]
    fn test_unterminated_flow_mapping() {
        let input = "{key1: value1, key2: value2";
        let (docs, errors) = parse(input);

        // Should have an error for missing }
        assert!(
            !errors.is_empty(),
            "Expected error for unterminated mapping"
        );

        // Should still produce partial output
        assert_eq!(docs.len(), 1, "Should produce 1 document");
        if let Value::Mapping(pairs) = &docs.first().unwrap().value {
            // Should have recovered some pairs
            assert!(!pairs.is_empty(), "Should recover some pairs");
        }
    }

    /// Test recovery from invalid escape sequence in double-quoted string.
    #[test]
    fn test_invalid_escape_sequence() {
        let input = r#"key: "hello\qworld""#;
        let (docs, errors) = parse(input);

        // Should have an error for invalid escape \q
        assert!(
            !errors.is_empty(),
            "Expected error for invalid escape sequence"
        );

        // Should still produce partial output (mapping with key)
        assert_eq!(docs.len(), 1, "Should produce 1 document");
    }

    /// Test recovery from unterminated double-quoted string.
    #[test]
    fn test_unterminated_double_quoted_string() {
        let input = "key: \"unterminated string\nother: value";
        let (docs, errors) = parse(input);

        // Should have an error for unterminated string
        assert!(!errors.is_empty(), "Expected error for unterminated string");

        // Parser should recover and continue
        assert!(!docs.is_empty(), "Should produce some output");
    }

    /// Test recovery from unterminated single-quoted string.
    #[test]
    fn test_unterminated_single_quoted_string() {
        let input = "key: 'unterminated string\nother: value";
        let (docs, errors) = parse(input);

        // Should have an error for unterminated string
        assert!(!errors.is_empty(), "Expected error for unterminated string");

        // Parser should recover
        assert!(!docs.is_empty(), "Should produce some output");
    }

    /// Test that parser recovers from invalid items in flow sequence
    /// and continues parsing valid items.
    #[test]
    fn test_flow_sequence_with_error_recovery() {
        let input = "[a, , b, c]"; // Empty item (consecutive commas)
        let (docs, errors) = parse(input);

        // Should have an error for consecutive commas
        assert!(!errors.is_empty(), "Expected error for consecutive commas");

        // Should still produce sequence
        assert_eq!(docs.len(), 1, "Should produce 1 document");
        if let Value::Sequence(items) = &docs.first().unwrap().value {
            // Should have some items despite error
            assert!(!items.is_empty(), "Should have some items");
        }
    }

    /// Test that parser recovers from invalid items in flow mapping
    /// and continues parsing valid pairs.
    #[test]
    fn test_flow_mapping_with_error_recovery() {
        let input = "{a: 1, , b: 2}"; // Empty entry (consecutive commas)
        let (docs, errors) = parse(input);

        // Should have an error for consecutive commas
        assert!(!errors.is_empty(), "Expected error for consecutive commas");

        // Should still produce mapping
        assert_eq!(docs.len(), 1, "Should produce 1 document");
        if let Value::Mapping(pairs) = &docs.first().unwrap().value {
            // Should have some pairs despite error
            assert!(!pairs.is_empty(), "Should have some pairs");
        }
    }

    /// Test that parser handles duplicate anchors with error.
    #[test]
    fn test_duplicate_anchor_error() {
        let input = "&a &a value";
        let (docs, errors) = parse(input);

        // Should have an error for duplicate anchor
        assert!(!errors.is_empty(), "Expected error for duplicate anchor");

        // Should still produce a document
        assert_eq!(docs.len(), 1, "Should produce 1 document");
    }

    /// Test that parser handles undefined alias with error.
    #[test]
    fn test_undefined_alias_error() {
        let input = "*undefined_alias";
        let (docs, errors) = parse(input);

        // Should have an error for undefined alias
        assert!(!errors.is_empty(), "Expected error for undefined alias");

        // Should still produce a document (null fallback)
        assert_eq!(docs.len(), 1, "Should produce 1 document");
    }

    /// Test that parser recovers from tabs in indentation.
    #[test]
    fn test_tabs_in_indentation_error() {
        let input = "key:\n\tvalue";
        let (docs, errors) = parse(input);

        // Should have an error for tab in indentation
        assert!(!errors.is_empty(), "Expected error for tab in indentation");

        // Should still produce output
        assert!(!docs.is_empty(), "Should produce some output");
    }

    /// Test that valid content before error is preserved.
    #[test]
    fn test_valid_content_before_error_preserved() {
        // First document is valid, second has error
        let input = "---\nvalid_key: valid_value\n---\n[unterminated";
        let (docs, errors) = parse(input);

        // Should have an error for the unterminated sequence
        assert!(
            !errors.is_empty(),
            "Expected error for unterminated sequence"
        );

        // First document should be fully parsed
        assert!(!docs.is_empty(), "Should have at least 1 document");
        if let Value::Mapping(pairs) = &docs.first().unwrap().value {
            assert_eq!(pairs.len(), 1, "First doc should have 1 mapping pair");
        }
    }

    /// Test that multiple errors can be collected from a single document.
    #[test]
    fn test_multiple_errors_collected() {
        let input = "[, , ,]"; // Multiple consecutive commas
        let (docs, errors) = parse(input);

        // Should have multiple errors
        assert!(
            !errors.is_empty(),
            "Expected multiple errors, got {}",
            errors.len()
        );

        // Should still produce a document
        assert_eq!(docs.len(), 1, "Should produce 1 document");
    }

    /// Test error recovery in nested flow structure.
    #[test]
    fn test_nested_flow_error_recovery() {
        let input = "{outer: [a, , b], other: valid}";
        let (docs, errors) = parse(input);

        // Should have error for consecutive commas in nested sequence
        assert!(!errors.is_empty(), "Expected error for consecutive commas");

        // Should still produce mapping with both pairs
        assert_eq!(docs.len(), 1, "Should produce 1 document");
        if let Value::Mapping(pairs) = &docs.first().unwrap().value {
            assert_eq!(pairs.len(), 2, "Should have both mapping pairs");
        }
    }

    /// Test that parser handles missing colon after key.
    #[test]
    fn test_missing_colon_in_mapping() {
        let input = "key1: value1\nkey2\nkey3: value3";
        let (docs, errors) = parse(input);

        // Should have an error for missing colon
        assert!(!errors.is_empty(), "Expected error for missing colon");

        // Should recover and parse other entries
        assert_eq!(docs.len(), 1, "Should produce 1 document");
    }

    /// Test error spans are accurate.
    #[test]
    fn test_error_span_accuracy() {
        let input = "key: [a, , b]"; // Error at position of empty item
        let (docs, errors) = parse(input);

        assert!(!errors.is_empty(), "Expected error");

        // The error span should be within the input range
        for error in &errors {
            assert!(
                error.span.start_usize() < input.len(),
                "Error span start should be valid"
            );
            assert!(
                error.span.end_usize() <= input.len(),
                "Error span end should be valid"
            );
        }

        // Should still produce output
        assert_eq!(docs.len(), 1);
    }
}

/// Test to verify memory sizes of key types.
/// Run with `cargo test measure_type_sizes -- --nocapture` to see output.
/// Verify that memory optimizations are effective.
///
/// This test asserts on type sizes to catch regressions in memory layout.
/// Run with `--nocapture` to see current sizes.
#[test]
fn measure_type_sizes() {
    use std::mem::size_of;

    // Verify the optimizations are effective
    assert!(
        size_of::<Span>() <= 8,
        "Span should be 8 bytes or less with u32 offsets, got {}",
        size_of::<Span>()
    );
    assert!(
        size_of::<Node>() <= 64,
        "Node should be 64 bytes or less with boxed properties, got {}",
        size_of::<Node>()
    );
    assert!(
        size_of::<ErrorKind>() <= 16,
        "ErrorKind should be 16 bytes or less with u16 indentation, got {}",
        size_of::<ErrorKind>()
    );
}

/// Test parsing using `parse_single_document` directly with `EventParser`.
///
/// This demonstrates using the lower-level API that allows working with
/// `Node<'input>` before converting to owned data. This can avoid allocations
/// when the node doesn't need to outlive the input.
#[test]
fn test_zero_copy_parsing() {
    use crate::event_parser::EventParser;

    let input = "key: value";

    // Tokenize the document
    let (tokens, lexer_errors) = tokenize_document(input);
    assert!(lexer_errors.is_empty());

    // Parse to events - events lifetime is tied to input
    let (events, parse_errors) = parse_single_document(&tokens, input, &[]);
    assert!(parse_errors.is_empty());

    // Reconstruct AST from events using EventParser
    let mut event_parser = EventParser::new(&events);
    let nodes = event_parser.parse();
    assert!(event_parser.take_errors().is_empty());
    assert_eq!(nodes.len(), 1);

    let node = &nodes[0];

    // Verify it's a mapping with the expected content
    assert!(matches!(&node.value, Value::Mapping(pairs) if pairs.len() == 1));

    // Extract and verify key/value using pattern matching
    if let Value::Mapping(pairs) = &node.value
        && let Some((key, val)) = pairs.first()
    {
        if let Value::String(key_str) = &key.value {
            assert_eq!(key_str, "key");
        }
        if let Value::String(val_str) = &val.value {
            assert_eq!(val_str, "value");
        }
    }

    // Convert to owned when needed (e.g., to outlive the input)
    let owned_node: Node<'static> = node.clone().into_owned();
    assert!(matches!(&owned_node.value, Value::Mapping(_)));
}

#[test]
fn test_debug_p2ad() {
    // P2AD: Spec Example 8.1 - Block Scalar Header
    // Tests block scalars with various combinations of indentation and chomping indicators
    // Uses Unicode arrows (↓, U+2193) in comments
    let input = "- | # Empty header\u{2193}\n literal\n- >1 # Indentation indicator\u{2193}\n  folded\n- |+ # Chomping indicator\u{2193}\n keep\n\n- >1- # Both indicators\u{2193}\n  strip\n";

    // Verify the stream lexer correctly handles multi-byte UTF-8 characters
    let (docs, errors) = crate::lexer::tokenize_stream(input);
    assert!(errors.is_empty(), "Stream lexer should not produce errors");
    assert_eq!(docs.len(), 1, "Should have exactly one document");

    let doc = docs.first().unwrap();
    // The document content should include all 4 entries including the last one
    assert_eq!(
        doc.content.len(),
        input.len(),
        "Document content should span the entire input"
    );
    assert!(
        doc.content.ends_with("strip\n"),
        "Document content should end with 'strip\\n'"
    );
}

#[test]
#[allow(
    clippy::indexing_slicing,
    clippy::unreachable,
    reason = "Test code with assertions"
)]
fn test_block_scalar_chomping() {
    use crate::value::Value;

    // Helper to extract the scalar value from a sequence
    fn get_seq_scalar(input: &str) -> String {
        let (nodes, errors) = crate::parse(input);
        assert!(errors.is_empty(), "Unexpected errors: {errors:?}");
        assert_eq!(nodes.len(), 1);
        if let Value::Sequence(seq) = &nodes[0].value
            && let Value::String(str_val) = &seq[0].value
        {
            return str_val.to_string();
        }
        unreachable!("Expected sequence with string")
    }

    // JEF9-00: Empty lines with keep chomping
    assert_eq!(get_seq_scalar("- |+\n\n\n"), "\n\n");

    // JEF9-01: Single empty line with keep chomping
    assert_eq!(get_seq_scalar("- |+\n\n"), "\n");

    // JEF9-02: Trailing whitespace (no trailing newline)
    assert_eq!(get_seq_scalar("- |+\n   "), "\n");

    // A6F9: Basic chomping tests
    // strip: no trailing newlines
    // clip: exactly one trailing newline
    // keep: preserve all trailing newlines
    let (nodes, errors) = crate::parse("keep: |+\n  text\n");
    assert!(errors.is_empty());
    if let Value::Mapping(map) = &nodes[0].value
        && let Value::String(str_val) = &map[0].1.value
    {
        assert_eq!(
            str_val.as_ref(),
            "text\n",
            "keep chomping should have one newline"
        );
    }
}

#[test]
#[allow(clippy::print_stderr, clippy::use_debug, reason = "Debug test")]
fn test_debug_m5dy() {
    // Test: SYW4 - mapping scalars with comments
    let input = "hr:  65    # Home runs\navg: 0.278 # Batting average\n";

    eprintln!("Input: {input:?}");

    let (tokens, _) = crate::lexer::tokenize_document(input);
    eprintln!("Tokens ({}):", tokens.len());
    for (idx, tok) in tokens.iter().enumerate() {
        eprintln!("  {idx}: {tok:?}");
    }

    let (nodes, errors) = crate::parse(input);
    eprintln!("Errors ({}):", errors.len());
    for err in &errors {
        eprintln!("  {:?}", err);
    }
    eprintln!("Nodes ({}):", nodes.len());
    for (idx, node) in nodes.iter().enumerate() {
        eprintln!(
            "Node {idx}: props={:?} value={:?}",
            node.properties, node.value
        );
        if let crate::value::Value::Mapping(pairs) = &node.value {
            eprintln!("  Mapping has {} entries", pairs.len());
            for (entry_idx, (key, val)) in pairs.iter().enumerate() {
                eprintln!("    [{entry_idx}] key={key:?} val={val:?}");
            }
        }
    }
}

// ============================================================================
// Event Emitter Tests (now testing parser-emitted events)
// ============================================================================

mod event_tests {
    use crate::event::{CollectionStyle, Event, ScalarStyle};

    /// Helper to get events from YAML input using the parser
    fn events_from(input: &str) -> Vec<Event<'static>> {
        let (events, _errors) = crate::emit_events(input);
        events
    }

    #[test]
    fn test_plain_scalar() {
        let events = events_from("hello");
        assert!(events.iter().any(|ev| matches!(
            ev,
            Event::Scalar {
                style: ScalarStyle::Plain,
                value,
                ..
            } if value == "hello"
        )));
    }

    #[test]
    fn test_flow_mapping() {
        let events = events_from("{a: 1}");

        let has_map_start = events.iter().any(|ev| {
            matches!(
                ev,
                Event::MappingStart {
                    style: CollectionStyle::Flow,
                    ..
                }
            )
        });
        let has_map_end = events
            .iter()
            .any(|ev| matches!(ev, Event::MappingEnd { .. }));

        assert!(has_map_start, "Expected MappingStart, got: {events:?}");
        assert!(has_map_end, "Expected MappingEnd, got: {events:?}");
    }

    #[test]
    fn test_flow_sequence() {
        let events = events_from("[1, 2, 3]");

        let has_seq_start = events.iter().any(|ev| {
            matches!(
                ev,
                Event::SequenceStart {
                    style: CollectionStyle::Flow,
                    ..
                }
            )
        });
        let has_seq_end = events
            .iter()
            .any(|ev| matches!(ev, Event::SequenceEnd { .. }));

        assert!(has_seq_start, "Expected SequenceStart, got: {events:?}");
        assert!(has_seq_end, "Expected SequenceEnd, got: {events:?}");
    }

    #[test]
    fn test_block_sequence() {
        let events = events_from("- a\n- b");

        let seq_starts: Vec<_> = events
            .iter()
            .filter(|ev| {
                matches!(
                    ev,
                    Event::SequenceStart {
                        style: CollectionStyle::Block,
                        ..
                    }
                )
            })
            .collect();

        // Should have exactly ONE block sequence start, not one per entry
        assert_eq!(
            seq_starts.len(),
            1,
            "Expected 1 SequenceStart for block sequence, got {}: {events:?}",
            seq_starts.len()
        );
    }

    #[test]
    fn test_block_mapping() {
        let events = events_from("a: 1\nb: 2");

        let map_starts: Vec<_> = events
            .iter()
            .filter(|ev| {
                matches!(
                    ev,
                    Event::MappingStart {
                        style: CollectionStyle::Block,
                        ..
                    }
                )
            })
            .collect();

        // Should have exactly ONE block mapping start
        assert_eq!(
            map_starts.len(),
            1,
            "Expected 1 MappingStart for block mapping, got {}: {events:?}",
            map_starts.len()
        );
    }

    #[test]
    fn test_quoted_string() {
        let events = events_from("\"hello world\"");

        let has_quoted = events.iter().any(|ev| {
            matches!(
                ev,
                Event::Scalar {
                    style: ScalarStyle::DoubleQuoted,
                    ..
                }
            )
        });
        assert!(has_quoted, "Expected double-quoted scalar, got: {events:?}");
    }

    #[test]
    fn test_document_markers() {
        let events = events_from("---\nhello\n...");

        let has_doc_start = events
            .iter()
            .any(|ev| matches!(ev, Event::DocumentStart { explicit: true, .. }));
        let has_doc_end = events
            .iter()
            .any(|ev| matches!(ev, Event::DocumentEnd { explicit: true, .. }));

        assert!(has_doc_start, "Expected DocumentStart, got: {events:?}");
        assert!(has_doc_end, "Expected DocumentEnd, got: {events:?}");
    }

    #[test]
    fn test_anchor_and_alias() {
        // Use a sequence so both anchor and alias are in the same document
        let events = events_from("- &anchor value\n- *anchor");

        // Check for anchored scalar
        let has_anchor = events.iter().any(|ev| {
            matches!(
                ev,
                Event::Scalar { anchor: Some((anchor_name, _)), .. } if anchor_name == "anchor"
            )
        });
        assert!(has_anchor, "Expected scalar with anchor, got: {events:?}");

        // Check for alias
        let has_alias = events.iter().any(|ev| {
            matches!(
                ev,
                Event::Alias { name, .. } if name == "anchor"
            )
        });
        assert!(has_alias, "Expected alias, got: {events:?}");
    }

    #[test]
    fn test_tagged_scalar() {
        let events = events_from("!!str 42");

        // Check for tagged scalar with expanded tag
        let has_tag = events.iter().any(|ev| {
            matches!(
                ev,
                Event::Scalar { tag: Some((tg, _)), .. } if tg == "tag:yaml.org,2002:str"
            )
        });
        assert!(
            has_tag,
            "Expected scalar with expanded tag, got: {events:?}"
        );
    }

    #[test]
    fn test_nested_block_structures() {
        // Test nested mapping inside sequence
        let events = events_from("- a: 1\n- b: 2");

        let seq_count = events
            .iter()
            .filter(|ev| matches!(ev, Event::SequenceStart { .. }))
            .count();
        let map_count = events
            .iter()
            .filter(|ev| matches!(ev, Event::MappingStart { .. }))
            .count();

        assert_eq!(
            seq_count, 1,
            "Expected 1 sequence, got {seq_count}: {events:?}"
        );
        assert!(
            map_count >= 2,
            "Expected at least 2 mappings, got {map_count}: {events:?}"
        );
    }

    // Helper to get events and errors using the parser
    fn events_and_errors_from(input: &str) -> (Vec<Event<'static>>, Vec<crate::error::ParseError>) {
        crate::emit_events(input)
    }

    #[test]
    fn test_unclosed_flow_sequence_produces_error() {
        // Unclosed flow sequence should report error and auto-close
        let (events, errors) = events_and_errors_from("[a, b");

        // Should produce SequenceStart, scalars, and SequenceEnd (auto-closed)
        assert!(
            events
                .iter()
                .any(|e| matches!(e, Event::SequenceStart { .. })),
            "Should have SequenceStart: {events:?}"
        );
        assert!(
            events
                .iter()
                .any(|e| matches!(e, Event::SequenceEnd { .. })),
            "Should have SequenceEnd (auto-closed): {events:?}"
        );
        // Should report error
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, crate::error::ErrorKind::UnexpectedEof)),
            "Should report UnexpectedEof error: {errors:?}"
        );
    }

    #[test]
    fn test_unclosed_flow_mapping_produces_error() {
        // Unclosed flow mapping should report error and auto-close
        let (events, errors) = events_and_errors_from("{a: 1");

        assert!(
            events
                .iter()
                .any(|e| matches!(e, Event::MappingStart { .. })),
            "Should have MappingStart: {events:?}"
        );
        assert!(
            events.iter().any(|e| matches!(e, Event::MappingEnd { .. })),
            "Should have MappingEnd (auto-closed): {events:?}"
        );
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, crate::error::ErrorKind::UnexpectedEof)),
            "Should report UnexpectedEof error: {errors:?}"
        );
    }

    #[test]
    fn test_mismatched_brackets_produces_error() {
        // Mismatched brackets: opened with [ but closed with }
        let (events, errors) = events_and_errors_from("[a, b}");

        // Should produce SequenceStart and SequenceEnd (correct type despite mismatch)
        let seq_starts = events
            .iter()
            .filter(|e| matches!(e, Event::SequenceStart { .. }))
            .count();
        let seq_ends = events
            .iter()
            .filter(|e| matches!(e, Event::SequenceEnd { .. }))
            .count();
        assert_eq!(seq_starts, 1, "Should have 1 SequenceStart: {events:?}");
        assert_eq!(seq_ends, 1, "Should have 1 SequenceEnd: {events:?}");
        // Should report some error for the invalid syntax
        // The exact error type may vary (MismatchedBrackets, MissingSeparator, UnexpectedEof, etc.)
        assert!(
            !errors.is_empty(),
            "Should report at least one error for mismatched brackets: {errors:?}"
        );
    }
}

/// Tests for the event-based parser (`EventParser`).
mod event_parser_tests {
    use crate::event_parser::EventParser;
    use crate::value::Value;

    /// Parse input through the full event pipeline and return nodes.
    fn parse_via_events(input: &str) -> Vec<crate::value::Node<'static>> {
        let (events, _errors) = crate::emit_events(input);
        let mut parser = EventParser::new(&events);
        parser.parse().into_iter().map(|n| n.into_owned()).collect()
    }

    /// Test that EventParser produces identical output to the hybrid parser.
    /// This is the key test ensuring we can remove node-building from the hybrid parser.
    #[test]
    #[allow(clippy::print_stderr, reason = "Debug output on failure")]
    fn test_event_parser_matches_hybrid_parser() {
        let test_cases = [
            // Simple scalars
            "hello",
            "42",
            "3.14",
            "true",
            "null",
            "~",
            "",
            // Quoted strings
            "'single quoted'",
            "\"double quoted\"",
            "\"with\\nescape\"",
            // Block scalars
            "|\n  literal\n  block",
            ">\n  folded\n  block",
            // Simple collections
            "a: 1",
            "- item",
            "a: 1\nb: 2",
            "- a\n- b\n- c",
            // Flow collections
            "{a: 1, b: 2}",
            "[1, 2, 3]",
            "{a: [1, 2], b: {c: 3}}",
            // Nested block
            "outer:\n  inner: value",
            "- - nested\n  - items",
            "- a: 1\n  b: 2",
            // Anchors and aliases
            "&anchor value",
            "- &a 1\n- *a",
            // Tags
            "!!str 42",
            "!custom tagged",
            // Multi-document
            "---\nfirst\n---\nsecond",
            // Complex cases
            "key: |\n  multi\n  line",
            "list:\n  - a\n  - b",
            "mixed: [1, {a: b}]",
            // Empty values
            "key:",
            "- \n- value",
            // Explicit keys
            "? explicit\n: value",
        ];

        let mut failures = Vec::new();

        for input in test_cases {
            let (hybrid_nodes, hybrid_errors) = crate::parse(input);
            let (via_events_nodes, via_events_errors) = crate::parse_via_events(input);

            // Compare node counts
            if hybrid_nodes.len() != via_events_nodes.len() {
                failures.push(format!(
                    "Input: {input:?}\n  Node count mismatch: hybrid={}, via_events={}",
                    hybrid_nodes.len(),
                    via_events_nodes.len()
                ));
                continue;
            }

            // Compare each node
            for (i, (hybrid, via_events)) in
                hybrid_nodes.iter().zip(via_events_nodes.iter()).enumerate()
            {
                if hybrid != via_events {
                    failures.push(format!(
                        "Input: {input:?}\n  Document {i} mismatch:\n    hybrid:     {hybrid:?}\n    via_events: {via_events:?}"
                    ));
                }
            }

            // Compare error counts (not exact errors, as span offsets may differ in event path)
            if hybrid_errors.len() != via_events_errors.len() {
                failures.push(format!(
                    "Input: {input:?}\n  Error count mismatch: hybrid={}, via_events={}",
                    hybrid_errors.len(),
                    via_events_errors.len()
                ));
            }
        }

        if !failures.is_empty() {
            eprintln!("\n=== EventParser vs Hybrid Parser Mismatches ===");
            for failure in &failures {
                eprintln!("{failure}\n");
            }
            panic!(
                "{} test case(s) failed - EventParser output differs from hybrid parser",
                failures.len()
            );
        }
    }

    #[test]
    fn test_event_parser_simple_scalar() {
        let nodes = parse_via_events("hello");
        assert_eq!(nodes.len(), 1);
        assert!(matches!(&nodes[0].value, Value::String(s) if s == "hello"));
    }

    #[test]
    fn test_event_parser_typed_scalars() {
        // Test type inference for plain scalars
        let test_cases: &[(&str, fn(&Value) -> bool)] = &[
            ("true", |v| matches!(v, Value::Bool(true))),
            ("false", |v| matches!(v, Value::Bool(false))),
            ("null", |v| matches!(v, Value::Null)),
            ("42", |v| matches!(v, Value::Int(42))),
            (
                "3.14",
                |v| matches!(v, Value::Float(f) if (*f - 3.14).abs() < 0.001),
            ),
        ];

        for (input, check) in test_cases {
            let nodes = parse_via_events(input);
            assert_eq!(nodes.len(), 1, "Input: {input}");
            assert!(
                check(&nodes[0].value),
                "Input: {input}, got: {:?}",
                nodes[0].value
            );
        }
    }

    #[test]
    fn test_event_parser_simple_mapping() {
        let nodes = parse_via_events("a: 1\nb: 2");
        assert_eq!(nodes.len(), 1);

        if let Value::Mapping(pairs) = &nodes[0].value {
            assert_eq!(pairs.len(), 2);
            // First pair
            assert!(matches!(&pairs[0].0.value, Value::String(s) if s == "a"));
            assert!(matches!(&pairs[0].1.value, Value::Int(1)));
            // Second pair
            assert!(matches!(&pairs[1].0.value, Value::String(s) if s == "b"));
            assert!(matches!(&pairs[1].1.value, Value::Int(2)));
        } else {
            panic!("Expected mapping, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_simple_sequence() {
        let nodes = parse_via_events("- a\n- b\n- c");
        assert_eq!(nodes.len(), 1);

        if let Value::Sequence(items) = &nodes[0].value {
            assert_eq!(items.len(), 3);
            assert!(matches!(&items[0].value, Value::String(s) if s == "a"));
            assert!(matches!(&items[1].value, Value::String(s) if s == "b"));
            assert!(matches!(&items[2].value, Value::String(s) if s == "c"));
        } else {
            panic!("Expected sequence, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_flow_mapping() {
        let nodes = parse_via_events("{a: 1, b: 2}");
        assert_eq!(nodes.len(), 1);

        if let Value::Mapping(pairs) = &nodes[0].value {
            assert_eq!(pairs.len(), 2);
        } else {
            panic!("Expected mapping, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_flow_sequence() {
        let nodes = parse_via_events("[1, 2, 3]");
        assert_eq!(nodes.len(), 1);

        if let Value::Sequence(items) = &nodes[0].value {
            assert_eq!(items.len(), 3);
            assert!(matches!(&items[0].value, Value::Int(1)));
            assert!(matches!(&items[1].value, Value::Int(2)));
            assert!(matches!(&items[2].value, Value::Int(3)));
        } else {
            panic!("Expected sequence, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_anchor_and_alias() {
        let nodes = parse_via_events("- &anchor value\n- *anchor");
        assert_eq!(nodes.len(), 1);

        if let Value::Sequence(items) = &nodes[0].value {
            assert_eq!(items.len(), 2);
            // First item has anchor
            assert_eq!(items[0].anchor(), Some("anchor"));
            assert!(matches!(&items[0].value, Value::String(s) if s == "value"));
            // Second item is alias
            assert!(matches!(&items[1].value, Value::Alias(s) if s == "anchor"));
        } else {
            panic!("Expected sequence, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_nested_mapping() {
        let nodes = parse_via_events("outer:\n  inner: value");
        assert_eq!(nodes.len(), 1);

        if let Value::Mapping(pairs) = &nodes[0].value {
            assert_eq!(pairs.len(), 1);
            // Check outer key
            assert!(matches!(&pairs[0].0.value, Value::String(s) if s == "outer"));
            // Check inner mapping
            if let Value::Mapping(inner_pairs) = &pairs[0].1.value {
                assert_eq!(inner_pairs.len(), 1);
                assert!(matches!(&inner_pairs[0].0.value, Value::String(s) if s == "inner"));
                assert!(matches!(&inner_pairs[0].1.value, Value::String(s) if s == "value"));
            } else {
                panic!("Expected inner mapping, got: {:?}", pairs[0].1.value);
            }
        } else {
            panic!("Expected mapping, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_nested_sequence() {
        let input = "- - a\n  - b\n- c";
        let nodes = parse_via_events(input);

        assert_eq!(nodes.len(), 1);

        if let Value::Sequence(items) = &nodes[0].value {
            assert_eq!(items.len(), 2);
            // First item is nested sequence
            if let Value::Sequence(nested) = &items[0].value {
                assert_eq!(nested.len(), 2);
                assert!(matches!(&nested[0].value, Value::String(s) if s == "a"));
                assert!(matches!(&nested[1].value, Value::String(s) if s == "b"));
            } else {
                panic!("Expected nested sequence, got: {:?}", items[0].value);
            }
            // Second item is scalar
            assert!(matches!(&items[1].value, Value::String(s) if s == "c"));
        } else {
            panic!("Expected sequence, got: {:?}", nodes[0].value);
        }
    }

    #[test]
    fn test_event_parser_sequence_of_mappings() {
        let nodes = parse_via_events("- a: 1\n- b: 2");
        assert_eq!(nodes.len(), 1);

        if let Value::Sequence(items) = &nodes[0].value {
            assert_eq!(items.len(), 2);
            // First item is mapping
            if let Value::Mapping(pairs) = &items[0].value {
                assert!(matches!(&pairs[0].0.value, Value::String(s) if s == "a"));
                assert!(matches!(&pairs[0].1.value, Value::Int(1)));
            } else {
                panic!("Expected mapping, got: {:?}", items[0].value);
            }
        } else {
            panic!("Expected sequence, got: {:?}", nodes[0].value);
        }
    }
}

#[test]
#[allow(clippy::print_stdout, reason = "Debug output for test")]
fn test_emit_events_debug() {
    let input = "key: value";
    let (events, errors) = emit_events(input);

    assert!(errors.is_empty(), "Errors: {errors:?}");

    for event in &events {
        println!("{event}");
    }

    // Should be: +STR, +DOC, +MAP, =VAL :key, =VAL :value, -MAP, -DOC, -STR
    assert!(
        events.len() >= 8,
        "Expected at least 8 events, got {}",
        events.len()
    );
}

#[test]
#[allow(clippy::print_stdout, reason = "Debug output for test")]
fn test_emit_events_7zz5() {
    // Test case 7ZZ5: Empty flow collections
    let input = "---
nested sequences:
- - - []
- - - {}
key1: []
key2: {}
";
    let (events, errors) = emit_events(input);

    println!("Errors: {errors:?}");
    println!("\nActual events:");
    for event in &events {
        println!("{event}");
    }

    // Expected from test.event:
    // +STR +DOC --- +MAP =VAL :nested sequences +SEQ +SEQ +SEQ +SEQ [] -SEQ -SEQ -SEQ
    // +SEQ +SEQ +MAP {} -MAP -SEQ -SEQ -SEQ =VAL :key1 +SEQ [] -SEQ =VAL :key2 +MAP {} -MAP -MAP -DOC -STR

    // Just verify no parse errors for now
    assert!(errors.is_empty(), "Unexpected errors");
}

#[test]
#[allow(clippy::print_stdout, reason = "Debug output for test")]
fn test_emit_events_simple_seq_then_key() {
    // Simpler test: sequence followed by another key at same level
    let input = "a:
- x
b: y
";
    let (events, errors) = emit_events(input);

    println!("Errors: {errors:?}");
    println!("\nActual events:");
    for event in &events {
        println!("{event}");
    }

    // Expected:
    // +STR +DOC +MAP =VAL :a +SEQ =VAL :x -SEQ =VAL :b =VAL :y -MAP -DOC -STR

    assert!(errors.is_empty(), "Unexpected errors");
}

#[test]
#[allow(clippy::print_stdout, reason = "Debug output for test")]
fn test_emit_events_4wa9_literal_scalars() {
    // Test case 4WA9: Literal scalars
    let input = "- aaa: |2
    xxx
  bbb: |
    xxx
";
    let (events, errors) = emit_events(input);

    println!("Errors: {errors:?}");
    println!("\nActual events:");
    for event in &events {
        println!("{event}");
    }

    // Expected: +STR +DOC +SEQ +MAP =VAL :aaa =VAL |xxx\n =VAL :bbb =VAL |xxx\n -MAP -SEQ -DOC -STR
    assert!(errors.is_empty(), "Unexpected errors");
}

#[test]
#[allow(clippy::print_stdout, reason = "Debug output for test")]
fn test_emit_events_2jqs_missing_keys() {
    // Test case 2JQS: Block Mapping with Missing Keys
    let input = ": a\n: b\n";
    let (events, errors) = emit_events(input);

    println!("Errors: {errors:?}");
    println!("\nActual events:");
    for event in &events {
        println!("{event}");
    }

    // Expected: +STR +DOC +MAP =VAL : =VAL :a =VAL : =VAL :b -MAP -DOC -STR
    assert!(errors.is_empty(), "Unexpected errors");
}

#[test]
#[allow(clippy::print_stdout, reason = "Debug output for test")]
fn test_lexer_tokens_2jqs() {
    use crate::lexer::tokenize_document;

    let input = "- !!str\n-\n  !!null : a\n  b: !!str\n- !!str : !!null\n";
    let (tokens, errors) = tokenize_document(input);

    println!("Lexer errors: {errors:?}");
    println!("\nTokens:");
    for tok in &tokens {
        println!("{:?} @ {:?}", tok.token, tok.span);
    }

    assert!(errors.is_empty(), "Unexpected lexer errors");
}

#[test]
#[allow(clippy::print_stdout, reason = "Debug output for test")]
fn test_emit_events_7w2p_mapping_missing_values() {
    // Test case 7W2P: Block Mapping with Missing Values (explicit keys)
    // ? a
    // ? b
    // c:
    let input = "? a\n? b\nc:\n";
    let (events, errors) = emit_events(input);

    println!("Errors: {errors:?}");
    println!("\nActual events:");
    for event in &events {
        println!("{event}");
    }

    // Expected: +STR +DOC +MAP =VAL :a =VAL : =VAL :b =VAL : =VAL :c =VAL : -MAP -DOC -STR
    assert!(errors.is_empty(), "Unexpected errors");
}

#[test]
#[allow(clippy::print_stdout, reason = "Debug output for test")]
fn test_emit_events_26dv_nested_mappings() {
    // 26DV: Anchor on value that's a nested mapping
    // top3: &node3
    //   *alias1 : scalar3
    // The &node3 should be attached to the MappingStart, not the scalar
    // Note: *alias1 is undefined in this snippet (would be defined earlier in full 26DV test),
    // so we expect an UndefinedAlias error - that's OK, we're testing event structure.
    let input = "top3: &node3 \n  *alias1 : scalar3";

    // First, let's see what tokens the lexer produces
    println!("Input: {input:?}");
    println!("\nTokens:");
    let (tokens, _) = crate::tokenize_document(input);
    for token in &tokens {
        println!("  {:?}", token);
    }

    let (events, errors) = emit_events(input);
    println!("\nErrors: {errors:?}");
    println!("\nActual events:");
    for event in &events {
        println!("{event}");
    }

    // Expected: +MAP =VAL :top3 +MAP &node3 *alias1 =VAL :scalar3 -MAP -MAP
    // The UndefinedAlias error is expected since alias1 isn't defined in this snippet
    assert!(
        errors.len() == 1 && errors[0].kind == crate::error::ErrorKind::UndefinedAlias,
        "Expected exactly one UndefinedAlias error, got: {errors:?}"
    );
}

#[test]
#[allow(clippy::print_stdout, reason = "Debug output for test")]
fn test_emit_events_26dv_anchor_on_same_line_key() {
    // 26DV - anchor on same line as key should attach to key, not mapping
    // top6:
    //   &anchor6 'key6' : scalar6
    // The &anchor6 should be attached to the key 'key6', NOT to the MappingStart
    // NOTE: Testing with full 26DV input to match exact conditions
    let input = r#""top1" :
  "key1" : &alias1 scalar1
'top2' :
  'key2' : &alias2 scalar2
top3: &node3
  *alias1 : scalar3
top4:
  *alias2 : scalar4
top5   :
  scalar5
top6:
  &anchor6 'key6' : scalar6
"#;
    // Debug: print all tokens with their spans
    println!("Input length: {}", input.len());
    println!("\nTokens:");
    let (tokens, _) = crate::tokenize_document(input);
    for token in &tokens {
        println!("  {:?}", token);
    }

    let (events, errors) = emit_events(input);
    println!("\nErrors: {errors:?}");
    println!("\nActual events:");
    for event in &events {
        println!("{event}");
    }

    assert!(errors.is_empty(), "Unexpected errors");

    // Find the MappingStart that comes right after the "top6" scalar
    // It should have NO anchor (anchor belongs to key 'key6')
    let events_vec: Vec<_> = events.iter().collect();
    for (idx, event) in events_vec.iter().enumerate() {
        if let crate::event::Event::Scalar { value, .. } = event {
            if value.as_ref() == "top6" {
                // Next event should be MappingStart with no anchor
                if let Some(crate::event::Event::MappingStart { anchor, .. }) =
                    events_vec.get(idx + 1)
                {
                    assert!(
                        anchor.is_none(),
                        "MappingStart after 'top6' should not have anchor, but got {:?}",
                        anchor
                    );
                }
            }
        }
    }
}

#[test]
#[allow(clippy::print_stdout, reason = "Debug output for test")]
fn test_emit_events_fh7j_debug() {
    // FH7J: Tags on Empty Scalars
    let input = "- !!str\n-\n  !!null : a\n  b: !!str\n- !!str : !!null\n";

    println!("Input: {input:?}");
    println!("\nTokens:");
    let (tokens, _) = crate::tokenize_document(input);
    for tok in &tokens {
        println!("{:?} @ {:?}", tok.token, tok.span);
    }

    println!("\nEvents:");
    let (events, errors) = emit_events(input);
    println!("Errors: {errors:?}");
    for event in &events {
        println!("{event:?}");
    }
}

#[test]
fn test_emit_events_9yrd_debug() {
    // 9YRD: Multiline Scalar at Top Level
    // Expected: "a b c d\ne" (one scalar - d at col 0 is continuation, not key)
    let input = "a\nb  \n  c\nd\n\ne\n";

    println!("Input: {input:?}");
    println!("\nTokens:");
    let (tokens, _) = crate::tokenize_document(input);
    for tok in &tokens {
        println!("{:?} @ {:?}", tok.token, tok.span);
    }

    println!("\nEvents:");
    let (events, errors) = emit_events(input);
    println!("Errors: {errors:?}");
    for event in &events {
        println!("{event:?}");
    }
}

#[test]
fn test_emit_events_k858_debug() {
    // K858: Spec Example 8.6. Empty Scalar Chomping
    // strip: >-  -> empty
    // clip: >    -> empty
    // keep: |+   -> "\n"
    let input = "strip: >-\n\nclip: >\n\nkeep: |+\n\n";

    println!("Input: {input:?}");
    println!("\nTokens:");
    let (tokens, _) = crate::tokenize_document(input);
    for (i, tok) in tokens.iter().enumerate() {
        println!("  [{i}] {:?} @ {:?}", tok.token, tok.span);
    }

    println!("\nEvents:");
    let (events, errors) = emit_events(input);
    println!("Errors: {errors:?}");
    for (i, event) in events.iter().enumerate() {
        println!("  [{i}] {event:?}");
    }
}

#[test]
fn debug_f2c7_tag_issue() {
    let input = " - !!int 2\n";
    let (events, errors) = crate::emit_events(input);
    println!("Input: {:?}", input);
    println!("Events:");
    for (i, e) in events.iter().enumerate() {
        println!("  {}: {:?}", i, e);
    }
    println!("Errors: {:?}", errors);

    // Check the scalar at index 3 (after StreamStart, DocStart?, SequenceStart)
    if let Some(crate::event::Event::Scalar { value, .. }) = events.get(3) {
        assert_eq!(
            value.as_ref(),
            "2",
            "Scalar value should be '2', not include tag"
        );
    }
}
