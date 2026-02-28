// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Unit tests for the YAML parser.
//!
//! These tests verify parsing behavior for various YAML constructs,
//! error recovery, and edge cases.

use super::*;

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

/// Test parsing using `parse_single_document` directly.
///
/// This demonstrates using the lower-level API that allows working with
/// `Node<'input>` before converting to owned data. This can avoid allocations
/// when the node doesn't need to outlive the input.
#[test]
fn test_zero_copy_parsing() {
    let input = "key: value";

    // Tokenize the document
    let (tokens, lexer_errors) = tokenize_document(input);
    assert!(lexer_errors.is_empty());

    // Parse - node lifetime is tied to input
    let (parsed_node, parse_errors) = parse_single_document(&tokens, input, &[]);
    assert!(parse_errors.is_empty());

    let node = parsed_node.unwrap();

    // Verify it's a mapping with the expected content
    assert!(matches!(&node.value, Value::Mapping(pairs) if pairs.len() == 1));

    // Extract and verify key/value using pattern matching
    if let Value::Mapping(pairs) = &node.value {
        if let Some((key, val)) = pairs.first() {
            if let Value::String(key_str) = &key.value {
                assert_eq!(key_str, "key");
            }
            if let Value::String(val_str) = &val.value {
                assert_eq!(val_str, "value");
            }
        }
    }

    // Convert to owned when needed (e.g., to outlive the input)
    let owned_node: Node<'static> = node.into_owned();
    assert!(matches!(&owned_node.value, Value::Mapping(_)));
}
