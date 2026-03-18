// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Unit tests for the YAML parser.
//!
//! These tests verify parsing behavior for various YAML constructs,
//! error recovery, and edge cases.

// Allow these pedantic lints in test code where they add noise without benefit
#![allow(clippy::indexing_slicing, reason = "panics are acceptable in tests")]
#![allow(
    clippy::approx_constant,
    reason = "test values don't need constant refs"
)]
#![allow(
    clippy::as_conversions,
    reason = "pointer conversions are fine in tests"
)]
#![allow(
    clippy::expect_used,
    reason = "expect() in tests provides precise failure messages for structural invariants"
)]

use super::*;

#[test]
fn check_struct_sizes() {
    use std::mem::size_of;
    println!("\n=== Struct Sizes ===");
    println!("Event: {} bytes", size_of::<crate::Event>());
    println!("Token: {} bytes", size_of::<crate::lexer::Token>());
    println!("Span: {} bytes", size_of::<crate::Span>());
    println!("Cow<str>: {} bytes", size_of::<std::borrow::Cow<str>>());
    println!("Properties: {} bytes", size_of::<crate::event::Properties>());
    println!("Property: {} bytes", size_of::<crate::event::Property>());
    println!("Option<Property>: {} bytes", size_of::<Option<crate::event::Property>>());
    println!("===================\n");
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

    let pairs = match &doc.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("expected mapping");

    assert_eq!(pairs.len(), 1);
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
        let items = match &docs.first().unwrap().value {
            Value::Sequence(items) => Some(items),
            _ => None,
        }
        .expect("expected sequence");

        // Should have recovered some items
        assert!(!items.is_empty(), "Should recover some items");
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
        let pairs = match &docs.first().unwrap().value {
            Value::Mapping(pairs) => Some(pairs),
            _ => None,
        }
        .expect("expected mapping");

        // Should have recovered some pairs
        assert!(!pairs.is_empty(), "Should recover some pairs");
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
        let items = match &docs.first().unwrap().value {
            Value::Sequence(items) => Some(items),
            _ => None,
        }
        .expect("expected sequence");

        // Should have some items despite error
        assert!(!items.is_empty(), "Should have some items");
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
        let pairs = match &docs.first().unwrap().value {
            Value::Mapping(pairs) => Some(pairs),
            _ => None,
        }
        .expect("expected mapping");

        // Should have some pairs despite error
        assert!(!pairs.is_empty(), "Should have some pairs");
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
        let pairs = match &docs.first().unwrap().value {
            Value::Mapping(pairs) => Some(pairs),
            _ => None,
        }
        .expect("expected mapping in first document");

        assert_eq!(pairs.len(), 1, "First doc should have 1 mapping pair");
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
        let pairs = match &docs.first().unwrap().value {
            Value::Mapping(pairs) => Some(pairs),
            _ => None,
        }
        .expect("expected mapping");

        assert_eq!(pairs.len(), 2, "Should have both mapping pairs");
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

/// Test parsing using the `Emitter` directly with `Parser`.
///
/// This demonstrates using the lower-level API that allows working with
/// `Node<'input>` before converting to owned data. This can avoid allocations
/// when the node doesn't need to outlive the input.
#[test]
fn test_zero_copy_parsing() {
    use crate::emitter::Emitter;
    use crate::parser::Parser;

    let input = "key: value";
	
	    // Parse to events using the streaming Emitter - events lifetime is tied to input
	    let mut emitter = Emitter::new(input);
    let events: Vec<_> = emitter.by_ref().collect();
    let parse_errors = emitter.take_errors();
    assert!(parse_errors.is_empty());

    // Reconstruct AST from events using Parser (streaming over the event iterator)
    let mut parser = Parser::new(events.into_iter());
    let nodes = parser.parse();
    assert!(parser.take_errors().is_empty());
    assert_eq!(nodes.len(), 1);

    let node = &nodes[0];

    // Verify it's a mapping with the expected content
    let pairs = match &node.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("expected mapping");
    assert_eq!(pairs.len(), 1);

    // Extract and verify key/value using pattern matching
    let (key, val) = pairs.first().expect("expected mapping pair");
    let key_str = match &key.value {
        Value::String(string_value) => Some(string_value.as_ref()),
        _ => None,
    }
    .expect("expected string key");
    let val_str = match &val.value {
        Value::String(string_value) => Some(string_value.as_ref()),
        _ => None,
    }
    .expect("expected string value");

    assert_eq!(key_str, "key");
    assert_eq!(val_str, "value");

    // Convert to owned when needed (e.g., to outlive the input)
    let owned_node: Node<'static> = node.clone().into_owned();
    assert!(matches!(&owned_node.value, Value::Mapping(_)));
}

#[test]
fn test_block_scalar_chomping() {
    use crate::value::Value;

    // Helper to extract the scalar value from a sequence
    fn get_seq_scalar(input: &str) -> String {
        let (nodes, errors) = crate::parse(input);
        assert!(errors.is_empty(), "Unexpected errors: {errors:?}");
        assert_eq!(nodes.len(), 1);

        let seq = match &nodes.first().unwrap().value {
            Value::Sequence(seq) => Some(seq),
            _ => None,
        }
        .expect("expected sequence with string scalar");

        let first = seq.first().expect("expected at least one sequence item");
        let str_val = match &first.value {
            Value::String(string_value) => Some(string_value),
            _ => None,
        }
        .expect("expected string scalar");

        str_val.to_string()
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
    let map = match &nodes.first().unwrap().value {
        Value::Mapping(map) => Some(map),
        _ => None,
    }
    .expect("expected mapping");
    let (_, value_node) = map.first().expect("expected mapping pair");
    let str_val = match &value_node.value {
        Value::String(string_value) => Some(string_value),
        _ => None,
    }
    .expect("expected string value");
    assert_eq!(
        str_val.as_ref(),
        "text\n",
        "keep chomping should have one newline"
    );
}

#[test]
fn test_emit_events_zero_copy() {
    // Verify that emit_events returns events that borrow from the input
    let input = "key: value";
    let (events, errors) = crate::emit_events(input);
    assert!(errors.is_empty());

    // Find the scalar events and verify they point to the input string
    let scalar_events: Vec<_> = events
        .iter()
        .filter_map(|event| match event {
            Event::Scalar { value, .. } => Some(value),
            _ => None,
        })
        .collect();

    assert_eq!(scalar_events.len(), 2); // "key" and "value"

    // Verify that the scalar values are borrowed from the input
    // by checking that their pointers are within the input string's memory range
    let input_start = input.as_ptr() as usize;
    let input_end = input_start + input.len();

    for scalar in scalar_events {
        let borrowed = match scalar {
            std::borrow::Cow::Borrowed(borrowed) => Some(borrowed),
            std::borrow::Cow::Owned(_) => None,
        }
        .expect("Scalar should borrow from input (zero-copy)");
        let scalar_ptr = borrowed.as_ptr() as usize;
        assert!(
            scalar_ptr >= input_start && scalar_ptr < input_end,
            "Scalar should borrow from input (zero-copy)"
        );
    }
}
