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
#![allow(
    clippy::panic,
    reason = "panic() in tests provides precise failure messages for structural invariants"
)]

use super::*;

#[test]
#[allow(
    clippy::print_stdout,
    reason = "this test prints struct sizes for manual inspection"
)]
fn check_struct_sizes() {
    use std::mem::size_of;
    let (emitter_properties_size, parse_state_size) = crate::emitter::internal_type_sizes();
    println!("\n=== Struct Sizes ===");
    println!("Event: {} bytes", size_of::<crate::Event>());
    println!("Token: {} bytes", size_of::<crate::lexer::Token>());
    println!("Span: {} bytes", size_of::<crate::Span>());
    println!("Cow<str>: {} bytes", size_of::<std::borrow::Cow<str>>());
    println!(
        "Properties: {} bytes",
        size_of::<crate::event::Properties>()
    );
    println!("Property: {} bytes", size_of::<crate::event::Property>());
    println!(
        "Option<Property>: {} bytes",
        size_of::<Option<crate::event::Property>>()
    );
    println!("EmitterProperties: {emitter_properties_size} bytes");
    println!("ParseState: {parse_state_size} bytes");
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
fn test_duplicate_mapping_key_reports_second_key_span() {
    let input = "\
foo: 1
foo: 2
";
    let (docs, errors) = parse(input);

    let duplicate_key = errors
        .iter()
        .find(|error| error.kind == ErrorKind::DuplicateKey)
        .expect("Expected DuplicateKey error");
    let second_key_start = input.rfind("foo").expect("second foo should exist");
    assert_eq!(
        duplicate_key.span.start_usize(),
        second_key_start,
        "DuplicateKey should point at the later duplicate key"
    );
    assert_eq!(
        duplicate_key.span.end_usize(),
        second_key_start + "foo".len(),
        "DuplicateKey should cover only the later duplicate key"
    );
    assert_eq!(docs.len(), 1, "Should still produce 1 document");
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

#[test]
fn test_inline_value_comment_attaches_to_node() {
    let input = "key: value # trailing";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");

    let Value::Mapping(pairs) = &docs[0].value else {
        panic!("expected mapping");
    };
    assert_eq!(pairs.len(), 1);
    assert_eq!(
        pairs[0]
            .value
            .trailing_comment()
            .map(|comment| comment.text.as_ref()),
        Some(" trailing")
    );
    assert!(pairs[0].header_comment().is_none());
}

#[test]
fn test_mapping_header_comment_attaches_to_pair() {
    let input = "key: # header\n  - value\n";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");

    let Value::Mapping(pairs) = &docs[0].value else {
        panic!("expected mapping");
    };
    assert_eq!(pairs.len(), 1);
    assert_eq!(
        pairs[0]
            .header_comment()
            .map(|comment| comment.text.as_ref()),
        Some(" header")
    );
    assert!(pairs[0].value.trailing_comment().is_none());
}

#[test]
fn test_mapping_header_comment_attaches_to_pair_with_nested_mapping() {
    let input = "foo: # hello\n  bar: 1\n";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");

    let Value::Mapping(pairs) = &docs[0].value else {
        panic!("expected mapping");
    };
    assert_eq!(pairs.len(), 1);
    assert_eq!(
        pairs[0]
            .header_comment()
            .map(|comment| comment.text.as_ref()),
        Some(" hello")
    );
}

#[test]
fn test_sequence_header_comment_attaches_with_nested_mapping() {
    let input = "- # hello\n  foo: 1\n";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");

    let Value::Sequence(items) = &docs[0].value else {
        panic!("expected sequence");
    };
    assert_eq!(items.len(), 1);
    assert_eq!(
        items[0]
            .header_comment()
            .map(|comment| comment.text.as_ref()),
        Some(" hello")
    );
}

#[test]
fn test_sequence_header_comment_attaches_with_nested_sequence() {
    let input = "- # hello\n  - 1\n";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");

    let Value::Sequence(items) = &docs[0].value else {
        panic!("expected sequence");
    };
    assert_eq!(items.len(), 1);
    assert_eq!(
        items[0]
            .header_comment()
            .map(|comment| comment.text.as_ref()),
        Some(" hello")
    );
}

#[test]
fn test_mapping_header_comment_attaches_to_pair_with_nested_flow_sequence() {
    let input = "foo: # hello\n  [1, 2]\n";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");

    let Value::Mapping(pairs) = &docs[0].value else {
        panic!("expected mapping");
    };
    assert_eq!(pairs.len(), 1);
    assert_eq!(
        pairs[0]
            .header_comment()
            .map(|comment| comment.text.as_ref()),
        Some(" hello")
    );
}

#[test]
fn test_mapping_header_comment_attaches_to_pair_with_nested_flow_mapping() {
    let input = "foo: # hello\n  {bar: 1}\n";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");

    let Value::Mapping(pairs) = &docs[0].value else {
        panic!("expected mapping");
    };
    assert_eq!(pairs.len(), 1);
    assert_eq!(
        pairs[0]
            .header_comment()
            .map(|comment| comment.text.as_ref()),
        Some(" hello")
    );
}

#[test]
fn test_explicit_key_trailing_comment_attaches_to_key_node() {
    let input = "? foo # hello\n: 1\n";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");

    let Value::Mapping(pairs) = &docs[0].value else {
        panic!("expected mapping");
    };
    assert_eq!(pairs.len(), 1);
    assert_eq!(
        pairs[0]
            .key()
            .trailing_comment()
            .map(|comment| comment.text.as_ref()),
        Some(" hello")
    );
}

#[test]
fn test_nested_value_comment_stays_on_nested_node() {
    let input = "key:\n  value # nested\n";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");

    let Value::Mapping(pairs) = &docs[0].value else {
        panic!("expected mapping");
    };
    assert_eq!(pairs.len(), 1);
    assert!(pairs[0].header_comment().is_none());
    assert_eq!(
        pairs[0]
            .value
            .trailing_comment()
            .map(|comment| comment.text.as_ref()),
        Some(" nested")
    );
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

    /// Test that a missing comma in a flow sequence points at the insertion
    /// site and recovery continues with later items.
    #[test]
    fn test_missing_separator_in_flow_sequence_points_to_insertion_site() {
        let input = "[a [b], c]";
        let (docs, errors) = parse(input);

        let missing_separator = errors
            .iter()
            .find(|error| error.kind == ErrorKind::MissingSeparator)
            .expect("Expected MissingSeparator error");
        let insertion_point = input.find('a').expect("a should exist") + "a".len();
        assert_eq!(
            missing_separator.span.start_usize(),
            insertion_point,
            "MissingSeparator should point after the previous entry"
        );
        assert!(
            missing_separator.span.is_empty(),
            "MissingSeparator span should be zero-width, got {:?}",
            missing_separator.span
        );

        assert_eq!(docs.len(), 1, "Should produce 1 document");
        let items = match &docs.first().unwrap().value {
            Value::Sequence(items) => Some(items),
            _ => None,
        }
        .expect("expected sequence");
        assert_eq!(items.len(), 3, "Should recover all sequence items");
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

    /// Test that a missing comma in a flow mapping points at the insertion
    /// site and recovery continues with later pairs.
    #[test]
    fn test_missing_separator_in_flow_mapping_points_to_insertion_site() {
        let input = "{a: [] b: 2, c: 3}";
        let (docs, errors) = parse(input);

        let missing_separator = errors
            .iter()
            .find(|error| error.kind == ErrorKind::MissingSeparator)
            .expect("Expected MissingSeparator error");
        let insertion_point = input.find("[]").expect("[] should exist") + "[]".len();
        assert_eq!(
            missing_separator.span.start_usize(),
            insertion_point,
            "MissingSeparator should point after the previous value"
        );
        assert!(
            missing_separator.span.is_empty(),
            "MissingSeparator span should be zero-width, got {:?}",
            missing_separator.span
        );

        assert_eq!(docs.len(), 1, "Should produce 1 document");
        let pairs = match &docs.first().unwrap().value {
            Value::Mapping(pairs) => Some(pairs),
            _ => None,
        }
        .expect("expected mapping");
        assert_eq!(pairs.len(), 3, "Should recover all mapping pairs");
    }

    /// Test that a malformed flow mapping entry without a colon is dropped
    /// instead of recovered as a fake `null` pair.
    #[test]
    fn test_missing_colon_in_flow_mapping_drops_pair_and_recovers() {
        let input = "{a, b: 2, c: 3}";
        let (docs, errors) = parse(input);

        let missing_colon = errors
            .iter()
            .find(|error| error.kind == ErrorKind::MissingColon)
            .expect("Expected MissingColon error");
        let insertion_point = input.find('a').expect("a should exist") + "a".len();
        assert_eq!(
            missing_colon.span.start_usize(),
            insertion_point,
            "MissingColon should point after the malformed flow-map key"
        );
        assert!(
            missing_colon.span.is_empty(),
            "MissingColon span should be zero-width, got {:?}",
            missing_colon.span
        );

        assert_eq!(docs.len(), 1, "Should produce 1 document");
        let Value::Mapping(pairs) = &docs[0].value else {
            panic!("expected mapping, got docs: {docs:#?}");
        };

        let keys: Vec<_> = pairs
            .iter()
            .map(|pair| match &pair.key.value {
                Value::String(value) => value.as_ref(),
                other => panic!("expected string key, got {other:?} in docs: {docs:#?}"),
            })
            .collect();

        assert_eq!(pairs.len(), 2, "malformed pair should be dropped");
        assert!(
            !keys.contains(&"a"),
            "malformed key should not survive as a fake pair, got keys: {keys:?}\ndocs: {docs:#?}"
        );
        assert!(
            keys.contains(&"b"),
            "expected `b` pair to survive, got keys: {keys:?}"
        );
        assert!(
            keys.contains(&"c"),
            "expected `c` pair to survive, got keys: {keys:?}"
        );
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
        let missing_colon = errors
            .iter()
            .find(|error| error.kind == ErrorKind::MissingColon)
            .expect("Expected MissingColon error");
        let missing_key_end = input.find("key2").expect("key2 should exist") + "key2".len();
        assert_eq!(
            missing_colon.span.start_usize(),
            missing_key_end,
            "MissingColon should point at the insertion site after the key"
        );
        assert!(
            missing_colon.span.is_empty(),
            "MissingColon span should be zero-width, got {:?}",
            missing_colon.span
        );

        // Should recover and parse other entries
        assert_eq!(docs.len(), 1, "Should produce 1 document");
    }

    /// Test recovery from a missing colon inside a nested mapping does not
    /// trap later sibling keys inside the deeper structure.
    #[test]
    fn test_missing_colon_in_nested_mapping_recovers_to_root_sibling() {
        let input = "\
ipv4_prefix_list_catalog:
  - name: ALLOW-DEFAULT
    sequence_numbers
      - sequence: 10
        action: foo

ipv4_acls:
  - name: ACL-INTERNET-IN
";
        let (docs, errors) = parse(input);

        assert!(
            errors
                .iter()
                .any(|error| error.kind == ErrorKind::MissingColon),
            "Expected MissingColon error, got: {errors:?}"
        );
        assert!(
            errors
                .iter()
                .any(|error| error.kind == ErrorKind::InvalidIndentation),
            "Expected InvalidIndentation from dropped malformed subtree, got: {errors:?}"
        );
        assert_eq!(docs.len(), 1, "Should produce 1 document");

        let Value::Mapping(root_pairs) = &docs[0].value else {
            panic!("expected root mapping, got docs: {docs:#?}");
        };

        let root_keys: Vec<_> = root_pairs
            .iter()
            .map(|pair| match &pair.key.value {
                Value::String(value) => value.as_ref(),
                other => panic!("expected string key, got {other:?} in docs: {docs:#?}"),
            })
            .collect();

        assert!(
            root_keys.contains(&"ipv4_prefix_list_catalog"),
            "expected first top-level key to survive recovery, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
        assert!(
            root_keys.contains(&"ipv4_acls"),
            "expected later top-level key to recover at root, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
    }

    /// Test that a stray closing flow bracket in block value context does not
    /// cause document-level trailing-content cascades.
    #[test]
    fn test_unmatched_bracket_in_block_value_recovers_to_following_root_key() {
        let input = "\
foo: ]
ipv4_prefix_list_catalog:
";
        let (docs, errors) = parse(input);

        let unmatched_brackets = errors
            .iter()
            .filter(|error| error.kind == ErrorKind::UnmatchedBracket)
            .count();
        assert_eq!(
            unmatched_brackets, 1,
            "expected exactly one UnmatchedBracket error, got: {errors:?}"
        );
        assert!(
            !errors
                .iter()
                .any(|error| error.kind == ErrorKind::TrailingContent),
            "unexpected TrailingContent cascade after unmatched bracket: {errors:?}"
        );
        assert_eq!(docs.len(), 1, "Should produce 1 document");

        let Value::Mapping(root_pairs) = &docs[0].value else {
            panic!("expected root mapping, got docs: {docs:#?}");
        };

        let root_keys: Vec<_> = root_pairs
            .iter()
            .map(|pair| match &pair.key.value {
                Value::String(value) => value.as_ref(),
                other => panic!("expected string key, got {other:?} in docs: {docs:#?}"),
            })
            .collect();

        assert!(
            root_keys.contains(&"foo"),
            "expected first key to survive recovery, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
        assert!(
            root_keys.contains(&"ipv4_prefix_list_catalog"),
            "expected later top-level key to recover at root, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
    }

    /// Test that a malformed would-be key inside an established root mapping
    /// does not terminate the rest of the mapping.
    #[test]
    fn test_missing_colon_like_key_in_root_mapping_recovers_to_following_sibling() {
        let input = "\
underlay_routing_protocol: ebgp
foo:,
ipv4_prefix_list_catalog:
";
        let (docs, errors) = parse(input);

        assert!(
            errors
                .iter()
                .any(|error| error.kind == ErrorKind::MissingColon),
            "Expected MissingColon error, got: {errors:?}"
        );
        assert!(
            !errors
                .iter()
                .any(|error| error.kind == ErrorKind::TrailingContent),
            "unexpected TrailingContent cascade after malformed key line: {errors:?}"
        );
        assert_eq!(docs.len(), 1, "Should produce 1 document");

        let Value::Mapping(root_pairs) = &docs[0].value else {
            panic!("expected root mapping, got docs: {docs:#?}");
        };

        let root_keys: Vec<_> = root_pairs
            .iter()
            .map(|pair| match &pair.key.value {
                Value::String(value) => value.as_ref(),
                other => panic!("expected string key, got {other:?} in docs: {docs:#?}"),
            })
            .collect();

        assert!(
            root_keys.contains(&"underlay_routing_protocol"),
            "expected first key to survive recovery, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
        assert!(
            !root_keys.contains(&"foo:,"),
            "malformed line should be dropped instead of recovered as a fake key, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
        assert!(
            root_keys.contains(&"ipv4_prefix_list_catalog"),
            "expected later top-level key to remain in the same mapping, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
    }

    /// Test that an extra-indented would-be nested key under a plain scalar
    /// value is reported locally and does not poison later root keys.
    #[test]
    fn test_invalid_indented_mapping_after_plain_scalar_recovers_to_following_root_key() {
        let input = "\
underlay_routing_protocol: ebgp
 foo:
ipv4_prefix_list_catalog:
";
        let (docs, errors) = parse(input);

        let invalid_indentation = errors
            .iter()
            .find(|error| error.kind == ErrorKind::InvalidIndentation)
            .expect("Expected InvalidIndentation error");
        let invalid_indent_start = input.find(" foo:").expect("indented foo should exist");
        assert_eq!(
            invalid_indentation.span.start_usize(),
            invalid_indent_start,
            "InvalidIndentation should point at the extra leading space"
        );
        assert_eq!(
            invalid_indentation.span.end_usize(),
            invalid_indent_start + 1,
            "InvalidIndentation should cover only the extra leading space"
        );
        assert!(
            !errors
                .iter()
                .any(|error| error.kind == ErrorKind::TrailingContent),
            "unexpected TrailingContent cascade after invalid indentation: {errors:?}"
        );
        assert_eq!(docs.len(), 1, "Should produce 1 document");

        let Value::Mapping(root_pairs) = &docs[0].value else {
            panic!("expected root mapping, got docs: {docs:#?}");
        };

        let root_keys: Vec<_> = root_pairs
            .iter()
            .map(|pair| match &pair.key.value {
                Value::String(value) => value.as_ref(),
                other => panic!("expected string key, got {other:?} in docs: {docs:#?}"),
            })
            .collect();

        assert!(
            root_keys.contains(&"underlay_routing_protocol"),
            "expected first key to survive recovery, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
        assert!(
            !root_keys.contains(&"foo"),
            "invalid indented line should not be promoted into the root mapping, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
        assert!(
            root_keys.contains(&"ipv4_prefix_list_catalog"),
            "expected later top-level key to recover at root, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
    }

    /// Test that a colon-like continuation after a plain scalar value is
    /// reported locally and does not poison following root keys.
    #[test]
    fn test_unexpected_colon_after_plain_scalar_recovers_to_following_root_key() {
        let input = "\
underlay_routing_protocol: ebgpfoo:
ipv4_prefix_list_catalog:
";
        let (docs, errors) = parse(input);

        let unexpected_colon = errors
            .iter()
            .find(|error| error.kind == ErrorKind::UnexpectedColon)
            .expect("Expected UnexpectedColon error");
        let colon_pos = input.find("ebgpfoo:").expect("ebgpfoo should exist") + "ebgpfoo".len();
        assert_eq!(
            unexpected_colon.span.start_usize(),
            colon_pos,
            "UnexpectedColon should point at the colon after the scalar value"
        );
        assert_eq!(
            unexpected_colon.span.end_usize(),
            colon_pos + 1,
            "UnexpectedColon should cover only the offending colon"
        );
        assert!(
            !errors
                .iter()
                .any(|error| error.kind == ErrorKind::TrailingContent),
            "unexpected TrailingContent cascade after unexpected colon: {errors:?}"
        );
        assert_eq!(docs.len(), 1, "Should produce 1 document");

        let Value::Mapping(root_pairs) = &docs[0].value else {
            panic!("expected root mapping, got docs: {docs:#?}");
        };

        let root_keys: Vec<_> = root_pairs
            .iter()
            .map(|pair| match &pair.key.value {
                Value::String(value) => value.as_ref(),
                other => panic!("expected string key, got {other:?} in docs: {docs:#?}"),
            })
            .collect();

        assert!(
            root_keys.contains(&"underlay_routing_protocol"),
            "expected first key to survive recovery, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
        assert!(
            root_keys.contains(&"ipv4_prefix_list_catalog"),
            "expected later top-level key to recover at root, got root keys: {root_keys:?}\ndocs: {docs:#?}"
        );
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
    let (emitter_properties_size, parse_state_size) = crate::emitter::internal_type_sizes();

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
    assert!(
        emitter_properties_size <= 8,
        "EmitterProperties should stay pointer-sized, got {emitter_properties_size}"
    );
    assert!(
        parse_state_size <= 88,
        "ParseState should stay 88 bytes or less after sparse property optimization, got {parse_state_size}"
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
    let pair = pairs.first().expect("expected mapping pair");
    let key_str = match &pair.key.value {
        Value::String(string_value) => Some(string_value.as_ref()),
        _ => None,
    }
    .expect("expected string key");
    let val_str = match &pair.value.value {
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
        let str_val = match &first.node.value {
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
    let value_node = &map.first().expect("expected mapping pair").value;
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
