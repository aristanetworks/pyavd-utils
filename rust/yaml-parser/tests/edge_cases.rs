// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Edge case tests for YAML parser.
//!
//! This module tests corner cases, boundary conditions, and unusual
//! but valid YAML constructs.

#![allow(clippy::indexing_slicing, reason = "panics are acceptable in tests")]

use yaml_parser::{Value, parse};

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
    assert!(matches!(docs[0].value, Value::Null));
}

#[test]
fn test_explicit_null_variants() {
    let inputs = vec!["null", "Null", "NULL", "~"];

    for input in inputs {
        let (docs, _) = parse(input);
        assert_eq!(docs.len(), 1, "Input {:?} should produce 1 document", input);
        assert!(
            matches!(docs[0].value, Value::Null),
            "Input {:?} should be null",
            input
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
        assert!(
            matches!(docs[0].value, Value::Bool(true)),
            "Input {:?} should be true",
            input
        );
    }

    for input in false_inputs {
        let (docs, _) = parse(input);
        assert_eq!(docs.len(), 1);
        assert!(
            matches!(docs[0].value, Value::Bool(false)),
            "Input {:?} should be false",
            input
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
        assert_eq!(docs.len(), 1, "Input {:?} should produce 1 document", input);
        if let Value::Int(val) = docs[0].value {
            assert_eq!(
                val, expected,
                "Input {:?} should parse to {}",
                input, expected
            );
        } else {
            panic!(
                "Input {:?} should be an integer, got {:?}",
                input, docs[0].value
            );
        }
    }
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
        assert_eq!(docs.len(), 1, "Input {:?} should produce 1 document", input);
        if let Value::Float(val) = docs[0].value {
            assert_eq!(
                val, expected,
                "Input {:?} should parse to {}",
                input, expected
            );
        } else {
            panic!(
                "Input {:?} should be a float, got {:?}",
                input, docs[0].value
            );
        }
    }
}

#[test]
fn test_special_float_values() {
    let (docs, _) = parse(".inf");
    assert!(matches!(docs[0].value, Value::Float(f) if f.is_infinite() && f.is_sign_positive()));

    let (docs, _) = parse("-.inf");
    assert!(matches!(docs[0].value, Value::Float(f) if f.is_infinite() && f.is_sign_negative()));

    let (docs, _) = parse(".nan");
    assert!(matches!(docs[0].value, Value::Float(f) if f.is_nan()));
}

#[test]
fn test_empty_sequence() {
    let (docs, _) = parse("[]");
    assert_eq!(docs.len(), 1);
    if let Value::Sequence(items) = &docs[0].value {
        assert_eq!(items.len(), 0, "Empty sequence should have no items");
    } else {
        panic!("Expected sequence");
    }
}

#[test]
fn test_empty_mapping() {
    let (docs, _) = parse("{}");
    assert_eq!(docs.len(), 1);
    if let Value::Mapping(pairs) = &docs[0].value {
        assert_eq!(pairs.len(), 0, "Empty mapping should have no pairs");
    } else {
        panic!("Expected mapping");
    }
}

#[test]
fn test_nested_empty_collections() {
    let (docs, _) = parse("[[[]]]");
    assert_eq!(docs.len(), 1);
    if let Value::Sequence(outer) = &docs[0].value {
        assert_eq!(outer.len(), 1);
        if let Value::Sequence(middle) = &outer[0].value {
            assert_eq!(middle.len(), 1);
            if let Value::Sequence(inner) = &middle[0].value {
                assert_eq!(inner.len(), 0, "Innermost sequence should be empty");
            } else {
                panic!("Expected innermost sequence");
            }
        } else {
            panic!("Expected middle sequence");
        }
    } else {
        panic!("Expected outer sequence");
    }
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
        assert_eq!(docs.len(), 1, "Input {:?} should produce 1 document", input);
        if let Value::Mapping(pairs) = &docs[0].value {
            if let Value::String(s) = &pairs[0].1.value {
                assert_eq!(
                    s.as_ref(),
                    expected_value,
                    "Unicode value mismatch for {:?}",
                    input
                );
            } else {
                panic!("Expected string value");
            }
        } else {
            panic!("Expected mapping");
        }
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
        assert_eq!(docs.len(), 1, "Input {:?} should produce 1 document", input);
        if let Value::String(s) = &docs[0].value {
            assert_eq!(
                s.as_ref(),
                expected,
                "Escape sequence mismatch for {:?}",
                input
            );
        } else {
            panic!("Expected string value for {:?}", input);
        }
    }
}

#[test]
fn test_block_scalar_chomping() {
    // Strip chomping (-)
    let input = "text: |-\n  content\n  \n";
    let (docs, _) = parse(input);
    if let Value::Mapping(pairs) = &docs[0].value {
        if let Value::String(s) = &pairs[0].1.value {
            assert_eq!(
                s.as_ref(),
                "content",
                "Strip chomping should remove trailing newlines"
            );
        }
    }

    // Keep chomping (+)
    let input = "text: |+\n  content\n  \n";
    let (docs, _) = parse(input);
    if let Value::Mapping(pairs) = &docs[0].value {
        if let Value::String(s) = &pairs[0].1.value {
            assert_eq!(
                s.as_ref(),
                "content\n\n",
                "Keep chomping should preserve trailing newlines"
            );
        }
    }
}

#[test]
fn test_multiple_documents() {
    let input = "---\ndoc1\n---\ndoc2\n---\ndoc3";
    let (docs, _) = parse(input);
    assert_eq!(docs.len(), 3, "Should parse 3 documents");

    if let Value::String(s) = &docs[0].value {
        assert_eq!(s.as_ref(), "doc1");
    }
    if let Value::String(s) = &docs[1].value {
        assert_eq!(s.as_ref(), "doc2");
    }
    if let Value::String(s) = &docs[2].value {
        assert_eq!(s.as_ref(), "doc3");
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

    if let Value::Mapping(pairs) = &docs[0].value {
        assert_eq!(pairs.len(), 2);

        // First pair: block sequence
        if let Value::Sequence(items) = &pairs[0].1.value {
            assert_eq!(items.len(), 2);
        } else {
            panic!("Expected block sequence");
        }

        // Second pair: flow sequence
        if let Value::Sequence(items) = &pairs[1].1.value {
            assert_eq!(items.len(), 3);
        } else {
            panic!("Expected flow sequence");
        }
    }
}

#[test]
fn test_complex_keys() {
    // Simple complex key (quoted string as key)
    let input = "\"complex key\": value";
    let (docs, _) = parse(input);
    assert_eq!(docs.len(), 1);

    if let Value::Mapping(pairs) = &docs[0].value {
        if let Value::String(key) = &pairs[0].0.value {
            assert_eq!(key.as_ref(), "complex key");
        }
    }
}

#[test]
fn test_anchor_before_tag() {
    let input = "&anchor !!str value";
    let (docs, _) = parse(input);
    assert_eq!(docs.len(), 1);

    // Should have both anchor and tag
    assert!(docs[0].properties.is_some());
    if let Some(props) = &docs[0].properties {
        assert!(props.anchor.is_some());
        assert!(props.tag.is_some());
    }
}

#[test]
fn test_tag_before_anchor() {
    let input = "!!str &anchor value";
    let (docs, _) = parse(input);
    assert_eq!(docs.len(), 1);

    // Should have both tag and anchor
    assert!(docs[0].properties.is_some());
    if let Some(props) = &docs[0].properties {
        assert!(props.anchor.is_some());
        assert!(props.tag.is_some());
    }
}

#[test]
fn test_very_long_line() {
    // Test with a 1000-character line
    let long_value = "x".repeat(1000);
    let input = format!("key: {}", long_value);
    let (docs, errors) = parse(&input);
    assert_eq!(docs.len(), 1);
    assert_eq!(errors.len(), 0, "Long lines should parse without errors");

    if let Value::Mapping(pairs) = &docs[0].value {
        if let Value::String(s) = &pairs[0].1.value {
            assert_eq!(s.len(), 1000);
        }
    }
}

#[test]
fn test_many_items_in_sequence() {
    // Test with 100 items
    let mut input = String::new();
    for i in 0..100 {
        input.push_str(&format!("- item{}\n", i));
    }

    let (docs, errors) = parse(&input);
    assert_eq!(docs.len(), 1);
    assert_eq!(
        errors.len(),
        0,
        "Large sequences should parse without errors"
    );

    if let Value::Sequence(items) = &docs[0].value {
        assert_eq!(items.len(), 100);
    }
}

#[test]
fn test_many_pairs_in_mapping() {
    // Test with 100 key-value pairs
    let mut input = String::new();
    for i in 0..100 {
        input.push_str(&format!("key{}: value{}\n", i, i));
    }

    let (docs, errors) = parse(&input);
    assert_eq!(docs.len(), 1);
    assert_eq!(
        errors.len(),
        0,
        "Large mappings should parse without errors"
    );

    if let Value::Mapping(pairs) = &docs[0].value {
        assert_eq!(pairs.len(), 100);
    }
}
