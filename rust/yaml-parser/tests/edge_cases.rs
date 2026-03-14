// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Edge case tests for YAML parser.
//!
//! This module tests corner cases, boundary conditions, and unusual
//! but valid YAML constructs.

#![allow(
    clippy::tests_outside_test_module,
    reason = "integration tests in tests/ are top-level by design"
)]
#![allow(
    clippy::expect_used,
    reason = "expect() in tests provides precise failure messages for invariants \
              like 'exactly one document' and makes assertions more readable"
)]

use yaml_parser::{Number, Value, parse};

#[test]
fn test_empty_input() {
    let (docs, errors) = parse("");
    assert_eq!(docs.len(), 0, "Empty input should produce no documents");
    assert_eq!(errors.len(), 0, "Empty input should produce no errors");
}

#[test]
fn test_whitespace_only() {
    let (docs, errors) = parse("   \n  \n   ");
    // Whitespace-only input produces no documents (same as empty input)
    assert_eq!(
        docs.len(),
        0,
        "Whitespace-only input should produce no documents"
    );
    assert_eq!(errors.len(), 0, "Whitespace-only should produce no errors");
}

#[test]
fn test_comments_only() {
    let (docs, errors) = parse("# comment 1\n# comment 2\n");
    assert_eq!(
        docs.len(),
        0,
        "Comments-only input should produce no documents"
    );
    assert_eq!(errors.len(), 0, "Comments-only should produce no errors");
}

#[test]
fn test_single_null() {
    let (docs, _) = parse("~");
    assert_eq!(docs.len(), 1);
    let doc = docs.first().expect("expected exactly one document");
    assert!(matches!(doc.value, Value::Null));
}

#[test]
fn test_explicit_null_variants() {
    let inputs = vec!["null", "Null", "NULL", "~"];

    for input in inputs {
        let (docs, _) = parse(input);
        assert_eq!(docs.len(), 1, "Input {input:?} should produce 1 document",);
        let doc = docs.first().expect("expected exactly one document");
        assert!(
            matches!(doc.value, Value::Null),
            "Input {input:?} should be null",
        );
    }

    // Empty string is a special case - it produces no documents
    let (docs, _) = parse("");
    assert_eq!(docs.len(), 0, "Empty string should produce no documents");
}

#[test]
fn test_bool_variants() {
    let true_inputs = vec!["true", "True", "TRUE"];
    let false_inputs = vec!["false", "False", "FALSE"];

    for input in true_inputs {
        let (docs, _) = parse(input);
        assert_eq!(docs.len(), 1);
        let doc = docs.first().expect("expected exactly one document");
        assert!(
            matches!(doc.value, Value::Bool(true)),
            "Input {input:?} should be true",
        );
    }

    for input in false_inputs {
        let (docs, _) = parse(input);
        assert_eq!(docs.len(), 1);
        let doc = docs.first().expect("expected exactly one document");
        assert!(
            matches!(doc.value, Value::Bool(false)),
            "Input {input:?} should be false",
        );
    }
}

#[test]
fn test_integer_edge_cases() {
    let test_cases = vec![
        ("0", 0i64),
        ("-0", 0i64),
        ("42", 42i64),
        ("-42", -42i64),
        ("9223372036854775807", i64::MAX),  // Max i64
        ("-9223372036854775808", i64::MIN), // Min i64
    ];

    for (input, expected) in test_cases {
        let (docs, _) = parse(input);
        assert_eq!(docs.len(), 1, "Input {input:?} should produce 1 document",);
        let doc = docs.first().expect("expected exactly one document");
        assert!(
            matches!(doc.value, Value::Int(Number::I64(val)) if val == expected),
            "Input {input:?} should parse to {expected}, got {:?}",
            doc.value,
        );
    }
}

#[test]
fn test_very_large_integer_as_bigintstr() {
    let input = "123456789012345678901234567890123456789012345678901234567890";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty(), "unexpected parse errors: {errors:?}");
    assert_eq!(docs.len(), 1, "expected a single document");
    let doc = docs.first().expect("expected exactly one document");
    assert!(
        matches!(
            doc.value,
            Value::Int(Number::BigIntStr(ref text)) if text.as_ref() == input
        ),
        "expected BigIntStr for very large integer, got {:?}",
        doc.value,
    );
}

#[test]
fn test_float_edge_cases() {
    let test_cases = vec![
        ("0.0", 0.0f64),
        ("-0.0", -0.0f64),
        ("1.5", 1.5f64),
        ("-1.5", -1.5f64),
        ("1e10", 1e10f64),
        ("1.5e-10", 1.5e-10f64),
    ];

    for (input, expected) in test_cases {
        let (docs, _) = parse(input);
        assert_eq!(docs.len(), 1, "Input {input:?} should produce 1 document",);
        let doc = docs.first().expect("expected exactly one document");
        assert!(
            matches!(doc.value, Value::Float(val) if val.to_bits() == expected.to_bits()),
            "Input {input:?} should parse to {expected}, got {:?}",
            doc.value,
        );
    }
}

#[test]
fn test_special_float_values() {
    {
        let (docs, _) = parse(".inf");
        let doc = docs.first().expect("expected .inf document");
        assert!(matches!(
            doc.value,
            Value::Float(float) if float.is_infinite() && float.is_sign_positive()
        ));
    }

    {
        let (docs, _) = parse("-.inf");
        let doc = docs.first().expect("expected -.inf document");
        assert!(matches!(
            doc.value,
            Value::Float(float) if float.is_infinite() && float.is_sign_negative()
        ));
    }

    {
        let (docs, _) = parse(".nan");
        let doc = docs.first().expect("expected .nan document");
        assert!(matches!(doc.value, Value::Float(float) if float.is_nan()));
    }
}

#[test]
fn test_empty_sequence() {
    let (docs, _) = parse("[]");
    assert_eq!(docs.len(), 1);
    let doc = docs.first().expect("expected exactly one document");
    assert!(
        matches!(&doc.value, Value::Sequence(items) if items.is_empty()),
        "Empty sequence should have no items, got {:?}",
        doc.value,
    );
}

#[test]
fn test_empty_mapping() {
    let (docs, _) = parse("{}");
    assert_eq!(docs.len(), 1);
    let doc = docs.first().expect("expected exactly one document");
    assert!(
        matches!(&doc.value, Value::Mapping(pairs) if pairs.is_empty()),
        "Empty mapping should have no pairs, got {:?}",
        doc.value,
    );
}

#[test]
fn test_nested_empty_collections() {
    let (docs, _) = parse("[[[]]]");
    assert_eq!(docs.len(), 1);
    let doc = docs.first().expect("expected exactly one document");

    let outer = match &doc.value {
        Value::Sequence(items) => Some(items),
        _ => None,
    }
    .expect("expected outer sequence");

    assert_eq!(outer.len(), 1);
    let middle_node = outer.first().expect("expected middle sequence node");

    let middle = match &middle_node.value {
        Value::Sequence(items) => Some(items),
        _ => None,
    }
    .expect("expected middle sequence");

    assert_eq!(middle.len(), 1);
    let inner_node = middle.first().expect("expected inner sequence node");

    let inner = match &inner_node.value {
        Value::Sequence(items) => Some(items),
        _ => None,
    }
    .expect("expected inner sequence");

    assert_eq!(inner.len(), 0, "Innermost sequence should be empty");
}

#[test]
fn test_deeply_nested_structure() {
    // Test 10 levels of nesting
    let input = "a:\n  b:\n    c:\n      d:\n        e:\n          f:\n            g:\n              h:\n                i:\n                  j: value";
    let (docs, errors) = parse(input);
    assert_eq!(docs.len(), 1);
    assert_eq!(
        errors.len(),
        0,
        "Deeply nested structure should parse without errors"
    );
}

#[test]
fn test_unicode_scalars() {
    let test_cases = vec![
        ("emoji: 🎉", "🎉"),
        ("chinese: 你好", "你好"),
        ("arabic: مرحبا", "مرحبا"),
        ("mixed: Hello世界🌍", "Hello世界🌍"),
    ];

    for (input, expected_value) in test_cases {
        let (docs, _) = parse(input);
        assert_eq!(docs.len(), 1, "Input {input:?} should produce 1 document",);
        let doc = docs.first().expect("expected exactly one document");

        let pairs = match &doc.value {
            Value::Mapping(pairs) => Some(pairs),
            _ => None,
        }
        .expect("Expected mapping");

        let (_, value_node) = pairs.first().expect("Expected at least one mapping pair");

        let string_value = match &value_node.value {
            Value::String(string_value) => Some(string_value.as_ref()),
            _ => None,
        }
        .expect("Expected string value");

        assert_eq!(
            string_value, expected_value,
            "Unicode value mismatch for {input:?}",
        );
    }
}

#[test]
fn test_escape_sequences() {
    let test_cases = vec![
        (r#""hello\nworld""#, "hello\nworld"),
        (r#""tab\there""#, "tab\there"),
        (r#""quote\"here""#, "quote\"here"),
        (r#""backslash\\here""#, "backslash\\here"),
        (r#""carriage\rreturn""#, "carriage\rreturn"),
    ];

    for (input, expected) in test_cases {
        let (docs, _) = parse(input);
        assert_eq!(docs.len(), 1, "Input {input:?} should produce 1 document",);
        let doc = docs.first().expect("expected exactly one document");

        let string_value = match &doc.value {
            Value::String(string_value) => Some(string_value.as_ref()),
            _ => None,
        }
        .expect("Expected string value");

        assert_eq!(
            string_value, expected,
            "Escape sequence mismatch for {input:?}",
        );
    }
}

#[test]
fn test_block_scalar_chomping() {
    // Strip chomping (-)
    let input_strip = "text: |-\n  content\n  \n";
    {
        let (docs, _) = parse(input_strip);
        let doc = docs.first().expect("expected exactly one document");

        let pairs = match &doc.value {
            Value::Mapping(pairs) => Some(pairs),
            _ => None,
        }
        .expect("expected mapping");

        let (_, value_node) = pairs.first().expect("expected mapping pair");

        let string_value = match &value_node.value {
            Value::String(string_value) => Some(string_value.as_ref()),
            _ => None,
        }
        .expect("expected string value");

        assert_eq!(
            string_value, "content",
            "Strip chomping should remove trailing newlines",
        );
    }

    // Keep chomping (+)
    let input_keep = "text: |+\n  content\n  \n";
    {
        let (docs, _) = parse(input_keep);
        let doc = docs.first().expect("expected exactly one document");

        let pairs = match &doc.value {
            Value::Mapping(pairs) => Some(pairs),
            _ => None,
        }
        .expect("expected mapping");

        let (_, value_node) = pairs.first().expect("expected mapping pair");

        let string_value = match &value_node.value {
            Value::String(string_value) => Some(string_value.as_ref()),
            _ => None,
        }
        .expect("expected string value");

        assert_eq!(
            string_value, "content\n\n",
            "Keep chomping should preserve trailing newlines",
        );
    }
}

#[test]
fn test_multiple_documents() {
    let input = "---\ndoc1\n---\ndoc2\n---\ndoc3";
    let (docs, _) = parse(input);
    assert_eq!(docs.len(), 3, "Should parse 3 documents");
    {
        let doc = docs.first().expect("expected first document");
        let value1 = match &doc.value {
            Value::String(string) => Some(string.as_ref()),
            _ => None,
        }
        .expect("expected string in first document");
        assert_eq!(value1, "doc1");
    }
    {
        let doc = docs.get(1).expect("expected second document");
        let value2 = match &doc.value {
            Value::String(string) => Some(string.as_ref()),
            _ => None,
        }
        .expect("expected string in second document");
        assert_eq!(value2, "doc2");
    }
    {
        let doc = docs.get(2).expect("expected third document");
        let value3 = match &doc.value {
            Value::String(string) => Some(string.as_ref()),
            _ => None,
        }
        .expect("expected string in third document");
        assert_eq!(value3, "doc3");
    }
}

#[test]
fn test_document_end_marker() {
    let input = "---\nvalue\n...\n---\nvalue2";
    let (docs, _) = parse(input);
    assert_eq!(docs.len(), 2, "Should parse 2 documents separated by ...");
}

#[test]
fn test_trailing_whitespace() {
    let input = "key: value   \n  \n";
    let (docs, errors) = parse(input);
    assert_eq!(docs.len(), 1);
    assert_eq!(
        errors.len(),
        0,
        "Trailing whitespace should not cause errors"
    );
}

#[test]
fn test_mixed_flow_and_block() {
    let input = "block:\n  - item1\n  - item2\nflow: [a, b, c]";
    let (docs, _) = parse(input);
    assert_eq!(docs.len(), 1);

    let doc = docs.first().expect("expected exactly one document");

    let pairs = match &doc.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("expected mapping");

    assert_eq!(pairs.len(), 2);

    // First pair: block sequence
    let (_, first_value) = pairs.first().expect("expected first mapping pair");
    let first_items = match &first_value.value {
        Value::Sequence(items) => Some(items),
        _ => None,
    }
    .expect("expected block sequence");
    assert_eq!(first_items.len(), 2);

    // Second pair: flow sequence
    let (_, second_value) = pairs.get(1).expect("expected second mapping pair");
    let second_items = match &second_value.value {
        Value::Sequence(items) => Some(items),
        _ => None,
    }
    .expect("expected flow sequence");
    assert_eq!(second_items.len(), 3);
}

#[test]
fn test_complex_keys() {
    // Simple complex key (quoted string as key)
    let input = "\"complex key\": value";
    let (docs, _) = parse(input);
    assert_eq!(docs.len(), 1);

    let doc = docs.first().expect("expected exactly one document");

    let pairs = match &doc.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("expected mapping");

    assert_eq!(pairs.len(), 1);
    let (key, value) = pairs.first().expect("expected exactly one pair");
    assert_eq!(key.value, Value::String("complex key".into()));
    assert_eq!(value.value, Value::String("value".into()));
}

#[test]
fn test_anchor_before_tag() {
    let input = "&anchor !!str value";
    let (docs, _) = parse(input);
    assert_eq!(docs.len(), 1);

    // Should have both anchor and tag
    let doc = docs.first().expect("expected exactly one document");
    let props = doc
        .properties
        .as_ref()
        .expect("expected anchor and tag properties");
    assert!(props.anchor.is_some());
    assert!(props.tag.is_some());
}

#[test]
fn test_tag_before_anchor() {
    let input = "!!str &anchor value";
    let (docs, _) = parse(input);
    assert_eq!(docs.len(), 1);

    // Should have both tag and anchor
    let doc = docs.first().expect("expected exactly one document");
    let props = doc
        .properties
        .as_ref()
        .expect("expected tag and anchor properties");
    assert!(props.anchor.is_some());
    assert!(props.tag.is_some());
}

#[test]
fn test_very_long_line() {
    // Test with a 1000-character line
    let long_value = "x".repeat(1000);
    let input = format!("key: {long_value}");
    let (docs, errors) = parse(&input);
    assert_eq!(docs.len(), 1);
    assert_eq!(errors.len(), 0, "Long lines should parse without errors");

    let doc = docs.first().expect("expected exactly one document");

    let pairs = match &doc.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("expected mapping");

    let (_, value_node) = pairs.first().expect("expected mapping pair");

    let string_value = match &value_node.value {
        Value::String(string_value) => Some(string_value),
        _ => None,
    }
    .expect("expected string value");

    assert_eq!(string_value.len(), 1000);
}

#[test]
fn test_many_items_in_sequence() {
    // Test with 100 items
    let mut input = String::new();
    for i in 0..100 {
        use std::fmt::Write as _;
        let _ = writeln!(input, "- item{i}");
    }

    let (docs, errors) = parse(&input);
    assert_eq!(docs.len(), 1);
    assert_eq!(
        errors.len(),
        0,
        "Large sequences should parse without errors"
    );

    let doc = docs.first().expect("expected exactly one document");

    let items = match &doc.value {
        Value::Sequence(items) => Some(items),
        _ => None,
    }
    .expect("expected sequence");

    assert_eq!(items.len(), 100);
}

#[test]
fn test_many_pairs_in_mapping() {
    // Test with 100 key-value pairs
    let mut input = String::new();
    for i in 0..100 {
        use std::fmt::Write as _;
        let _ = writeln!(input, "key{i}: value{i}");
    }

    let (docs, errors) = parse(&input);
    assert_eq!(docs.len(), 1);
    assert_eq!(
        errors.len(),
        0,
        "Large mappings should parse without errors"
    );

    let doc = docs.first().expect("expected exactly one document");

    let pairs = match &doc.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("expected mapping");

    assert_eq!(pairs.len(), 100);
}
