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
#![allow(clippy::panic, reason = "panic is expected in test assertions")]
#![allow(
    clippy::print_stderr,
    reason = "eprintln is used for diagnostic output in test helpers"
)]
#![allow(
    clippy::use_debug,
    reason = "Debug formatting is needed for diagnostic output in test helpers"
)]
#![allow(
    clippy::float_cmp,
    reason = "exact float comparison is intentional for semantic equality checks"
)]
#![allow(clippy::indexing_slicing, reason = "panics are acceptable in tests")]

mod support;

#[cfg(feature = "serde")]
use saphyr::{LoadableYamlNode as _, Yaml};

use support::parse_ok;
use yaml_parser::{Integer, Value};

#[cfg(feature = "serde")]
use serde::Deserialize;

#[cfg(feature = "serde")]
use yaml_parser::serde::DeError;

#[test]
fn test_empty_input() {
    let docs = parse_ok("");
    assert_eq!(docs.len(), 0, "Empty input should produce no documents");
}

#[test]
fn test_whitespace_only() {
    let docs = parse_ok("   \n  \n   ");
    // Whitespace-only input produces no documents (same as empty input)
    assert_eq!(
        docs.len(),
        0,
        "Whitespace-only input should produce no documents",
    );
}

#[test]
fn test_comments_only() {
    let docs = parse_ok("# comment 1\n# comment 2\n");
    assert_eq!(
        docs.len(),
        0,
        "Comments-only input should produce no documents",
    );
}

#[test]
fn test_single_null() {
    let docs = parse_ok("~");
    assert_eq!(docs.len(), 1);
    let doc = docs.first().expect("expected exactly one document");
    assert!(matches!(doc.value, Value::Null));
}

#[test]
fn test_explicit_null_variants() {
    let inputs = vec!["null", "Null", "NULL", "~"];

    for input in inputs {
        let docs = parse_ok(input);
        assert_eq!(docs.len(), 1, "Input {input:?} should produce 1 document",);
        let doc = docs.first().expect("expected exactly one document");
        assert!(
            matches!(doc.value, Value::Null),
            "Input {input:?} should be null",
        );
    }

    // Empty string is a special case - it produces no documents and no errors.
    let docs = parse_ok("");
    assert_eq!(docs.len(), 0, "Empty string should produce no documents");
}

#[test]
fn test_bool_variants() {
    let true_inputs = vec!["true", "True", "TRUE"];
    let false_inputs = vec!["false", "False", "FALSE"];

    for input in true_inputs {
        let docs = parse_ok(input);
        assert_eq!(docs.len(), 1);
        let doc = docs.first().expect("expected exactly one document");
        assert!(
            matches!(doc.value, Value::Bool(true)),
            "Input {input:?} should be true",
        );
    }

    for input in false_inputs {
        let docs = parse_ok(input);
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
        let docs = parse_ok(input);
        assert_eq!(docs.len(), 1, "Input {input:?} should produce 1 document",);
        let doc = docs.first().expect("expected exactly one document");
        assert!(
            matches!(doc.value, Value::Int(Integer::I64(val)) if val == expected),
            "Input {input:?} should parse to {expected}, got {:?}",
            doc.value,
        );
    }
}

#[test]
fn test_very_large_integer_as_bigintstr() {
    let input = "123456789012345678901234567890123456789012345678901234567890";
    let docs = parse_ok(input);
    assert_eq!(docs.len(), 1, "expected a single document");
    let doc = docs.first().expect("expected exactly one document");
    assert!(
        matches!(
            doc.value,
            Value::Int(Integer::BigIntStr(ref text)) if text.as_ref() == input
        ),
        "expected BigIntStr for very large integer, got {:?}",
        doc.value,
    );
}

#[test]
fn test_json_style_flow_mapping_line_has_no_parse_errors() {
    let input = r#"json_style: {"key":"value","number":42,"nested":{"a":"b"}}"#;
    let _docs = parse_ok(input);
}

#[test]
fn test_bench_corpora_have_no_parse_errors() {
    let corpora: &[(&str, &str)] = &[
        (
            "large_mapping",
            include_str!("../benches/data/large_mapping.yml"),
        ),
        (
            "nested_mapping",
            include_str!("../benches/data/nested_mapping.yml"),
        ),
        (
            "large_sequence",
            include_str!("../benches/data/large_sequence.yml"),
        ),
        (
            "block_scalars",
            include_str!("../benches/data/block_scalars.yml"),
        ),
        (
            "flow_collections",
            include_str!("../benches/data/flow_collections.yml"),
        ),
        (
            "anchors_aliases",
            include_str!("../benches/data/anchors_aliases.yml"),
        ),
        ("tags", include_str!("../benches/data/tags.yml")),
    ];

    for (_name, input) in corpora {
        // All positive corpora used by benchmarks must be parse-error free.
        let _docs = parse_ok(input);
    }
}

#[cfg(feature = "serde")]
#[test]
fn tags_corpus_serde_deserializes_across_backends() {
    let input = include_str!("../benches/data/tags.yml");

    let yaml_parser_result: Result<serde_yaml::Value, _> = yaml_parser::serde::from_str(input);
    let serde_yaml_result: Result<serde_yaml::Value, _> = serde_yaml::from_str(input);

    // Also check how saphyr interprets the same document.
    let saphyr_docs = Yaml::load_from_str(input).expect("saphyr failed to parse tags.yml");
    let root = saphyr_docs
        .first()
        .expect("saphyr returned no documents for tags.yml");
    let nulls = &root["nulls"];
    let empty = &nulls["empty"];

    // This test now asserts the harmonised behaviour across libraries on
    // our `tags.yml` benchmark corpus:
    // - yaml-parser's serde integration successfully deserializes `tags.yml`.
    // - serde_yaml also successfully deserializes `tags.yml`.
    // - saphyr parses the document and classifies `nulls.empty` as a real
    //   null value (not `BadValue`).
    //
    // If any of these libraries start rejecting `tags.yml` in the future,
    // this test will catch that regression.
    assert!(yaml_parser_result.is_ok());
    assert!(serde_yaml_result.is_ok());
    assert!(empty.is_null());
}

#[cfg(feature = "serde")]
#[derive(Debug, PartialEq)]
struct OwnedYamlValue(Value<'static>);

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for OwnedYamlValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        // First build a borrowing `Value<'de>` using its generic serde impl,
        // then convert to an owned `Value<'static>` so the result is
        // independent of the input lifetime. This mirrors the bench-only
        // adapter used in `benches/parser_bench.rs`.
        let borrowed: Value<'de> = Deserialize::deserialize(deserializer)?;
        Ok(OwnedYamlValue(borrowed.into_owned()))
    }
}

/// Semantic equality for `Integer` that ignores internal signed/unsigned
/// representation differences and compares the underlying decimal value.
#[cfg(feature = "serde")]
fn numbers_semantically_equal(left: &Integer<'static>, right: &Integer<'static>) -> bool {
    left.to_decimal_string() == right.to_decimal_string()
}

/// Semantic equality for `Value` trees used in serde equivalence tests.
///
/// This intentionally:
/// - Ignores span information and node properties (anchors/tags), since
///   serde-based paths (including `serde_yaml`) do not preserve them.
/// - Treats integers as equal when their decimal representations match,
///   even if they use different `Integer` variants (e.g. `I64` vs `U64`).
#[cfg(feature = "serde")]
fn values_semantically_equal(left: &Value<'static>, right: &Value<'static>) -> bool {
    match (left, right) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(lv), Value::Bool(rv)) => lv == rv,
        (Value::Float(lv), Value::Float(rv)) => {
            if lv.is_nan() && rv.is_nan() {
                true
            } else {
                lv == rv
            }
        }
        (Value::String(lv), Value::String(rv)) => lv == rv,
        (Value::Int(lv), Value::Int(rv)) => numbers_semantically_equal(lv, rv),
        (Value::Sequence(left_items), Value::Sequence(right_items)) => {
            left_items.len() == right_items.len()
                && left_items
                    .iter()
                    .zip(right_items.iter())
                    .all(|(ln, rn)| values_semantically_equal(&ln.value, &rn.value))
        }
        (Value::Mapping(left_pairs), Value::Mapping(right_pairs)) => {
            left_pairs.len() == right_pairs.len()
                && left_pairs
                    .iter()
                    .zip(right_pairs.iter())
                    .all(|(left_pair, right_pair)| {
                        values_semantically_equal(&left_pair.key.value, &right_pair.key.value)
                            && values_semantically_equal(
                                &left_pair.value.value,
                                &right_pair.value.value,
                            )
                    })
        }
        _ => false,
    }
}

#[cfg(feature = "serde")]
fn owned_semantically_equal(left: &OwnedYamlValue, right: &OwnedYamlValue) -> bool {
    values_semantically_equal(&left.0, &right.0)
}

/// Walk two `Value` trees and report the first path at which they differ.
///
/// This is only used to improve failure messages in serde equivalence tests.
#[cfg(feature = "serde")]
fn first_difference_path(
    path: &str,
    left: &Value<'static>,
    right: &Value<'static>,
) -> Option<String> {
    use Value::*;

    match (left, right) {
        (Null, Null) => None,
        (Bool(lv), Bool(rv)) => {
            if lv == rv {
                None
            } else {
                eprintln!("diff at {path}: left Bool({lv}), right Bool({rv})");
                Some(path.to_owned())
            }
        }
        (Float(lv), Float(rv)) => {
            let both_nan = lv.is_nan() && rv.is_nan();
            if both_nan || lv == rv {
                None
            } else {
                eprintln!("diff at {path}: left Float({lv}), right Float({rv})");
                Some(path.to_owned())
            }
        }
        (String(ls), String(rs)) => {
            if ls == rs {
                None
            } else {
                eprintln!("diff at {path}: left String({ls:?}), right String({rs:?})");
                Some(path.to_owned())
            }
        }
        (Int(ln), Int(rn)) => {
            if numbers_semantically_equal(ln, rn) {
                None
            } else {
                eprintln!("diff at {path}: left Int({ln:?}), right Int({rn:?})");
                Some(path.to_owned())
            }
        }
        (Sequence(left_items), Sequence(right_items)) => {
            if left_items.len() != right_items.len() {
                eprintln!(
                    "diff at {path}: sequence length mismatch (left {}, right {})",
                    left_items.len(),
                    right_items.len()
                );
                return Some(path.to_owned());
            }
            for (idx, (ln, rn)) in left_items.iter().zip(right_items.iter()).enumerate() {
                let child_path = format!("{path}[{idx}]");
                if let Some(diff_path) = first_difference_path(&child_path, &ln.value, &rn.value) {
                    return Some(diff_path);
                }
            }
            None
        }
        (Mapping(left_pairs), Mapping(right_pairs)) => {
            if left_pairs.len() != right_pairs.len() {
                eprintln!(
                    "diff at {path}: mapping length mismatch (left {}, right {})",
                    left_pairs.len(),
                    right_pairs.len()
                );
                return Some(path.to_owned());
            }
            for (idx, (left_pair, right_pair)) in
                left_pairs.iter().zip(right_pairs.iter()).enumerate()
            {
                let key_path = format!("{path}.<key#{idx}>");
                if let Some(diff_path) =
                    first_difference_path(&key_path, &left_pair.key.value, &right_pair.key.value)
                {
                    return Some(diff_path);
                }

                let key_name = match &left_pair.key.value {
                    String(str_val) => str_val.as_ref().to_owned(),
                    other => format!("<{other:?}>"),
                };
                let value_path = format!("{path}.{key_name}");
                if let Some(diff_path) = first_difference_path(
                    &value_path,
                    &left_pair.value.value,
                    &right_pair.value.value,
                ) {
                    return Some(diff_path);
                }
            }
            None
        }
        // Different variants (e.g. String vs Int)
        _ => {
            eprintln!("diff at {path}: left {left:?}, right {right:?}");
            Some(path.to_owned())
        }
    }
}

/// For each benchmark corpus, verify that yaml-parser's serde API and
/// `serde_yaml` deserialize into the same logical `OwnedYamlValue` tree.
#[cfg(feature = "serde")]
#[test]
fn bench_corpora_serde_equivalence_against_serde_yaml() {
    let corpora: &[(&str, &str)] = &[
        (
            "large_mapping",
            include_str!("../benches/data/large_mapping.yml"),
        ),
        (
            "nested_mapping",
            include_str!("../benches/data/nested_mapping.yml"),
        ),
        (
            "large_sequence",
            include_str!("../benches/data/large_sequence.yml"),
        ),
        (
            "block_scalars",
            include_str!("../benches/data/block_scalars.yml"),
        ),
        (
            "flow_collections",
            include_str!("../benches/data/flow_collections.yml"),
        ),
        (
            "anchors_aliases",
            include_str!("../benches/data/anchors_aliases.yml"),
        ),
        ("tags", include_str!("../benches/data/tags.yml")),
    ];

    for (name, input) in corpora {
        let parsed: OwnedYamlValue = yaml_parser::serde::from_str(input)
            .unwrap_or_else(|err| panic!("yaml_parser::serde::from_str failed on {name}: {err:?}"));

        // serde_yaml using the same logical target type. This ensures that the
        // `serde_deserialize` throughput benchmarks are comparing equivalent
        // trees across both libraries.
        let sy: OwnedYamlValue = serde_yaml::from_str(input)
            .unwrap_or_else(|err| panic!("serde_yaml::from_str failed on {name}: {err:?}"));

        // For most corpora we expect serde_yaml to produce a value tree that is
        // *semantically* equivalent to yaml-parser's serde backend when
        // deserializing into `OwnedYamlValue`. Two corpora still contain
        // implementation-defined areas where different libraries may
        // legitimately disagree:
        //
        // - `block_scalars`: exact string result for certain folded+keep
        //   combinations.
        // - `tags`: generic-value deserialization of custom tags such as
        //   `!timestamp 2024-01-15T10:30:00Z`. Our serde path preserves these
        //   as tagged string-like scalars, while `serde_yaml` may deserialize
        //   them through tagged-value machinery into a different generic
        //   representation.
        //
        // For now we treat those corpora as known divergences rather than
        // forcing either library's behaviour.
        if *name == "block_scalars" || *name == "tags" {
            continue;
        }

        if !owned_semantically_equal(&parsed, &sy) {
            eprintln!(
                "yaml_parser::serde::from_str and serde_yaml produced semantically different values on {name}; locating first differing path...",
            );
            if let Some(path) = first_difference_path("root", &parsed.0, &sy.0) {
                panic!(
                    "serde_yaml produced a semantically different value on {name}; first differing path: {path}",
                );
            }
            // Fallback assertion if the debug helper somehow failed to locate
            // a difference but the semantic check still says they differ.
            assert!(
                owned_semantically_equal(&parsed, &sy),
                "serde_yaml produced a semantically different value on {name}",
            );
        }
    }
}

#[cfg(feature = "serde")]
#[test]
fn from_str_reports_multiple_documents() {
    let input = "---\n1\n---\n2\n";

    let result = yaml_parser::serde::from_str::<i64>(input);
    match result {
        Err(DeError::MultipleDocuments) => {}
        other => panic!("expected MultipleDocuments error, got {other:?}"),
    }
}

#[cfg(feature = "serde")]
#[test]
fn stream_from_str_docs_yields_all_documents() {
    let input = "---\n1\n---\n2\n---\n3\n";

    let iter = yaml_parser::serde::stream_from_str_docs::<i64>(input)
        .expect("failed to create streaming deserializer");
    let docs: Vec<Result<i64, DeError>> = iter.collect();

    assert_eq!(docs.len(), 3);
    match &docs[0] {
        Ok(val) => assert_eq!(*val, 1),
        Err(err) => panic!("unexpected error in first document: {err:?}"),
    }
    match &docs[1] {
        Ok(val) => assert_eq!(*val, 2),
        Err(err) => panic!("unexpected error in second document: {err:?}"),
    }
    match &docs[2] {
        Ok(val) => assert_eq!(*val, 3),
        Err(err) => panic!("unexpected error in third document: {err:?}"),
    }
}

#[cfg(feature = "serde")]
#[test]
fn stream_from_str_docs_resets_anchors_between_documents() {
    // First document defines &a, second document reuses &a independently.
    let input = "&a 1\n---\n&a 2\n";

    let iter = yaml_parser::serde::stream_from_str_docs::<serde_yaml::Value>(input)
        .expect("failed to create streaming deserializer");
    let docs: Vec<Result<serde_yaml::Value, DeError>> = iter.collect();

    // Both documents should deserialize successfully; if anchors leaked
    // across documents, we'd see incorrect alias behaviour or errors.
    assert_eq!(docs.len(), 2);
    assert!(docs[0].is_ok());
    assert!(docs[1].is_ok());
}

#[cfg(feature = "serde")]
#[test]
fn serde_to_string_preserves_string_scalars_that_look_typed() {
    for input in ["true", "null", "42"] {
        let yaml = yaml_parser::serde::to_string(&input.to_owned())
            .expect("serializing string scalar should succeed");
        let reparsed: serde_yaml::Value =
            serde_yaml::from_str(&yaml).expect("serde_yaml should parse emitted YAML");

        assert_eq!(
            reparsed,
            serde_yaml::Value::String(input.to_owned()),
            "serialized YAML should roundtrip as a string for input {input:?}, got YAML {yaml:?}"
        );
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
        let docs = parse_ok(input);
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
        let docs = parse_ok(".inf");
        let doc = docs.first().expect("expected .inf document");
        assert!(matches!(
            doc.value,
            Value::Float(float) if float.is_infinite() && float.is_sign_positive()
        ));
    }

    {
        let docs = parse_ok("-.inf");
        let doc = docs.first().expect("expected -.inf document");
        assert!(matches!(
            doc.value,
            Value::Float(float) if float.is_infinite() && float.is_sign_negative()
        ));
    }

    {
        let docs = parse_ok(".nan");
        let doc = docs.first().expect("expected .nan document");
        assert!(matches!(doc.value, Value::Float(float) if float.is_nan()));
    }
}

#[test]
fn test_empty_sequence() {
    let docs = parse_ok("[]");
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
    let docs = parse_ok("{}");
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
    let docs = parse_ok("[[[]]]");
    assert_eq!(docs.len(), 1);
    let doc = docs.first().expect("expected exactly one document");

    let outer = match &doc.value {
        Value::Sequence(items) => Some(items),
        _ => None,
    }
    .expect("expected outer sequence");

    assert_eq!(outer.len(), 1);
    let middle_node = outer.first().expect("expected middle sequence node");

    let middle = match &middle_node.node.value {
        Value::Sequence(items) => Some(items),
        _ => None,
    }
    .expect("expected middle sequence");

    assert_eq!(middle.len(), 1);
    let inner_node = middle.first().expect("expected inner sequence node");

    let inner = match &inner_node.node.value {
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
    let docs = parse_ok(input);
    assert_eq!(docs.len(), 1);
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
        let docs = parse_ok(input);
        assert_eq!(docs.len(), 1, "Input {input:?} should produce 1 document",);
        let doc = docs.first().expect("expected exactly one document");

        let pairs = match &doc.value {
            Value::Mapping(pairs) => Some(pairs),
            _ => None,
        }
        .expect("Expected mapping");

        let value_node = &pairs
            .first()
            .expect("Expected at least one mapping pair")
            .value;

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
        let docs = parse_ok(input);
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
        let docs = parse_ok(input_strip);
        let doc = docs.first().expect("expected exactly one document");

        let pairs = match &doc.value {
            Value::Mapping(pairs) => Some(pairs),
            _ => None,
        }
        .expect("expected mapping");

        let value_node = &pairs.first().expect("expected mapping pair").value;

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
        let docs = parse_ok(input_keep);
        let doc = docs.first().expect("expected exactly one document");

        let pairs = match &doc.value {
            Value::Mapping(pairs) => Some(pairs),
            _ => None,
        }
        .expect("expected mapping");

        let value_node = &pairs.first().expect("expected mapping pair").value;

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
    let docs = parse_ok(input);
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
    let docs = parse_ok(input);
    assert_eq!(docs.len(), 2, "Should parse 2 documents separated by ...");
}

#[test]
fn test_trailing_whitespace() {
    let input = "key: value   \n  \n";
    let docs = parse_ok(input);
    assert_eq!(docs.len(), 1);
}

#[test]
fn test_mixed_flow_and_block() {
    let input = "block:\n  - item1\n  - item2\nflow: [a, b, c]";
    let docs = parse_ok(input);
    assert_eq!(docs.len(), 1);

    let doc = docs.first().expect("expected exactly one document");

    let pairs = match &doc.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("expected mapping");

    assert_eq!(pairs.len(), 2);

    // First pair: block sequence
    let first_value = &pairs.first().expect("expected first mapping pair").value;
    let first_items = match &first_value.value {
        Value::Sequence(items) => Some(items),
        _ => None,
    }
    .expect("expected block sequence");
    assert_eq!(first_items.len(), 2);

    // Second pair: flow sequence
    let second_value = &pairs.get(1).expect("expected second mapping pair").value;
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
    let docs = parse_ok(input);
    assert_eq!(docs.len(), 1);

    let doc = docs.first().expect("expected exactly one document");

    let pairs = match &doc.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("expected mapping");

    assert_eq!(pairs.len(), 1);
    let pair = pairs.first().expect("expected exactly one pair");
    assert_eq!(pair.key.value, Value::String("complex key".into()));
    assert_eq!(pair.value.value, Value::String("value".into()));
}

#[test]
fn test_anchor_before_tag() {
    let input = "&anchor !!str value";
    let docs = parse_ok(input);
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
    let docs = parse_ok(input);
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
    let docs = parse_ok(&input);
    assert_eq!(docs.len(), 1);

    let doc = docs.first().expect("expected exactly one document");

    let pairs = match &doc.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("expected mapping");

    let value_node = &pairs.first().expect("expected mapping pair").value;

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

    let docs = parse_ok(&input);
    assert_eq!(docs.len(), 1);

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

    let docs = parse_ok(&input);
    assert_eq!(docs.len(), 1);

    let doc = docs.first().expect("expected exactly one document");

    let pairs = match &doc.value {
        Value::Mapping(pairs) => Some(pairs),
        _ => None,
    }
    .expect("expected mapping");

    assert_eq!(pairs.len(), 100);
}
