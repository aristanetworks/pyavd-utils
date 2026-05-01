#![allow(clippy::indexing_slicing, reason = "panics are acceptable in tests")]
#![allow(clippy::panic, reason = "panic is acceptable in tests")]
#![allow(
// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

    clippy::min_ident_chars,
    reason = "single-char closure params are fine in tests"
)]
#![allow(clippy::type_complexity, reason = "complex types are fine in tests")]
#![allow(
    clippy::approx_constant,
    reason = "test values don't need to use consts"
)]
#![allow(clippy::float_cmp, reason = "exact equality is fine for these tests")]

use std::borrow::Cow;

use crate::Stream;
use crate::error::{ErrorKind, ParseError};
use crate::value::{Integer, Node, Value};

use super::Parser;

/// Use the standard parse function for tests.
fn parse(input: &str) -> (Stream<'static>, Vec<ParseError>) {
    let (nodes, errors) = crate::parse(input);
    (nodes.into_iter().map(Node::into_owned).collect(), errors)
}

fn nodes_equal_ignoring_structural_spans(left: &Node<'_>, right: &Node<'_>) -> bool {
    left.properties == right.properties
        && left.span == right.span
        && values_equal_ignoring_structural_spans(&left.value, &right.value)
}

fn values_equal_ignoring_structural_spans(left: &Value<'_>, right: &Value<'_>) -> bool {
    match (left, right) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(lb), Value::Bool(rb)) => lb == rb,
        (Value::Int(li), Value::Int(ri)) => li == ri,
        (Value::Float(lf), Value::Float(rf)) => lf == rf,
        (Value::String(ls), Value::String(rs)) => ls == rs,
        (Value::Sequence(left_items), Value::Sequence(right_items)) => {
            left_items.len() == right_items.len()
                && left_items
                    .iter()
                    .zip(right_items.iter())
                    .all(|(left_item, right_item)| {
                        nodes_equal_ignoring_structural_spans(&left_item.node, &right_item.node)
                    })
        }
        (Value::Mapping(left_pairs), Value::Mapping(right_pairs)) => {
            left_pairs.len() == right_pairs.len()
                && left_pairs
                    .iter()
                    .zip(right_pairs.iter())
                    .all(|(left_pair, right_pair)| {
                        nodes_equal_ignoring_structural_spans(&left_pair.key, &right_pair.key)
                            && nodes_equal_ignoring_structural_spans(
                                &left_pair.value,
                                &right_pair.value,
                            )
                    })
        }
        _ => false,
    }
}

#[test]
fn test_parse_simple_scalar() {
    let (docs, errors) = parse("hello");
    assert!(errors.is_empty());
    assert_eq!(docs.len(), 1);
    assert!(matches!(&docs.first().unwrap().value, Value::String(string) if string == "hello"));
}

#[test]
fn test_parse_simple_mapping() {
    let (docs, errors) = parse("key: value");
    assert!(errors.is_empty());
    assert_eq!(docs.len(), 1);
    let value = &docs.first().unwrap().value;
    assert!(matches!(value, Value::Mapping(_)));
    if let Value::Mapping(pairs) = value {
        assert_eq!(pairs.len(), 1);
        let pair = pairs.first().unwrap();
        assert!(matches!(&pair.key.value, Value::String(string) if string == "key"));
        assert!(matches!(&pair.value.value, Value::String(string) if string == "value"));
    }
}

#[test]
fn test_parse_flow_mapping() {
    let (docs, errors) = parse("{a: 1, b: 2}");
    assert!(errors.is_empty());
    assert_eq!(docs.len(), 1);
    let value = &docs.first().unwrap().value;
    assert!(matches!(value, Value::Mapping(_)));
    if let Value::Mapping(pairs) = value {
        assert_eq!(pairs.len(), 2);
    }
}

#[test]
fn test_parse_flow_sequence() {
    let (docs, errors) = parse("[1, 2, 3]");
    assert!(errors.is_empty());
    assert_eq!(docs.len(), 1);
    let value = &docs.first().unwrap().value;
    assert!(matches!(value, Value::Sequence(_)));
    if let Value::Sequence(items) = value {
        assert_eq!(items.len(), 3);
    }
}

#[test]
fn test_parse_block_sequence() {
    let (docs, errors) = parse("- a\n- b\n- c");
    assert!(errors.is_empty());
    assert_eq!(docs.len(), 1);
    let value = &docs.first().unwrap().value;
    assert!(matches!(value, Value::Sequence(_)));
    if let Value::Sequence(items) = value {
        assert_eq!(items.len(), 3);
    }
}

#[test]
fn test_parse_null_values() {
    let (docs, errors) = parse("~");
    assert!(errors.is_empty());
    assert_eq!(docs.len(), 1);
    assert!(matches!(&docs.first().unwrap().value, Value::Null));
}

#[test]
fn test_parse_tilde_prefixed_plain_scalar_stays_string() {
    let (docs, errors) = parse("~foo");
    assert!(errors.is_empty());
    assert_eq!(docs.len(), 1);
    assert!(matches!(&docs.first().unwrap().value, Value::String(string) if string == "~foo"));
}

#[test]
fn test_parse_boolean_values() {
    let (docs, errors) = parse("true");
    assert!(errors.is_empty());
    assert!(matches!(&docs.first().unwrap().value, Value::Bool(true)));
}

#[test]
fn test_parse_number_values() {
    let (docs, errors) = parse("42");
    assert!(errors.is_empty());
    assert!(matches!(
        &docs.first().unwrap().value,
        Value::Int(Integer::I64(42))
    ));

    let (docs_, errors_) = parse("3.45");
    assert!(errors_.is_empty());
    assert_eq!(docs_.len(), 1);
    assert_eq!(docs_.first().unwrap().value, Value::Float(3.45));
}

#[test]
fn test_parse_multi_document() {
    let (docs, _) = parse("---\na\n---\nb");
    assert!(docs.len() >= 2);
}

#[test]
fn test_parse_anchor_alias() {
    let (docs, errors) = parse("a: &anchor 1\nb: *anchor");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert_eq!(docs.len(), 1);
    let value = &docs.first().unwrap().value;
    assert!(matches!(&docs.first().unwrap().value, Value::Mapping(_)));
    if let Value::Mapping(pairs) = value {
        assert_eq!(pairs.len(), 2, "expected 2 pairs but got {pairs:?}");
        let first_value = &pairs.first().unwrap().value;
        assert_eq!(
            first_value.anchor(),
            Some("anchor"),
            "First value should have anchor 'anchor'"
        );
        assert!(matches!(
            &pairs.last().unwrap().value.value,
            Value::Int(Integer::I64(1))
        ));
        assert_eq!(pairs.last().unwrap().value.anchor(), None);
    }
}

#[test]
fn test_parse_alias_root_span_uses_alias_site() {
    let input = "- &anchor value\n- *anchor";
    let (docs, errors) = parse(input);
    assert!(errors.is_empty(), "errors: {errors:?}");

    let Value::Sequence(items) = &docs[0].value else {
        panic!("Expected sequence");
    };
    let Some(alias_text) =
        input.get(items[1].node.span.start_usize()..items[1].node.span.end_usize())
    else {
        panic!("alias span should be on UTF-8 boundaries");
    };
    assert_eq!(alias_text, "*anchor");
    assert_eq!(
        &items[1].node.value,
        &Value::String(Cow::Owned("value".to_owned()))
    );
}

#[test]
fn test_unknown_alias_drops_sequence_item() {
    let (docs, errors) = parse("- 1\n- *missing\n- 2");
    assert!(
        !errors.is_empty(),
        "expected emitter error for unknown alias"
    );

    let Value::Sequence(items) = &docs[0].value else {
        panic!("Expected sequence");
    };
    assert_eq!(items.len(), 2);
    assert!(matches!(items[0].node.value, Value::Int(Integer::I64(1))));
    assert!(matches!(items[1].node.value, Value::Int(Integer::I64(2))));
}

#[test]
fn test_unknown_alias_drops_mapping_pair() {
    let (docs, errors) = parse("good: 1\nbad: *missing\nalso_good: 2");
    assert!(
        !errors.is_empty(),
        "expected emitter error for unknown alias"
    );

    let Value::Mapping(pairs) = &docs[0].value else {
        panic!("Expected mapping");
    };
    assert_eq!(pairs.len(), 2);
    assert!(matches!(&pairs[0].key.value, Value::String(s) if s == "good"));
    assert!(matches!(&pairs[1].key.value, Value::String(s) if s == "also_good"));
}

#[test]
fn test_unknown_alias_key_drops_mapping_pair() {
    let (docs, errors) = parse("good: 0\n*missing: 1\nok: 2");
    assert!(
        !errors.is_empty(),
        "expected emitter error for unknown alias"
    );

    let Value::Mapping(pairs) = &docs[0].value else {
        panic!("Expected mapping");
    };
    assert_eq!(pairs.len(), 2);
    assert!(matches!(&pairs[0].key.value, Value::String(s) if s == "good"));
    assert!(matches!(&pairs[1].key.value, Value::String(s) if s == "ok"));
}

#[test]
fn test_self_referential_alias_drops_item_and_reports_error() {
    let (docs, errors) = parse("&a [*a]");
    assert!(
        errors
            .iter()
            .any(|error| error.kind == ErrorKind::UndefinedAlias),
        "expected UndefinedAlias error, got {errors:?}"
    );

    let Value::Sequence(items) = &docs[0].value else {
        panic!("Expected sequence");
    };
    assert!(
        items.is_empty(),
        "expected self-referential alias item to be dropped"
    );
}

#[test]
fn test_multiline_quoted_key_error() {
    let input = "\"c\n d\": 1";
    let (_, errors) = parse(input);
    assert!(!errors.is_empty());
}

/// Parse input through the full event pipeline and return nodes.
fn parse_via_events(input: &str) -> Vec<Node<'static>> {
    let (events, _errors) = crate::emit_events(input);
    let mut parser = Parser::new(events.into_iter());
    parser.parse().into_iter().map(Node::into_owned).collect()
}

/// Test that parsing through collected events matches the public AST path.
#[test]
#[allow(clippy::print_stderr, reason = "Debug output on failure")]
fn test_event_parser_matches_public_parse_pipeline() {
    let test_cases = [
        "hello",
        "42",
        "3.14",
        "true",
        "null",
        "~",
        "",
        "'single quoted'",
        "\"double quoted\"",
        "\"with\\nescape\"",
        "|\n  literal\n  block",
        ">\n  folded\n  block",
        "a: 1",
        "- item",
        "a: 1\nb: 2",
        "- a\n- b\n- c",
        "{a: 1, b: 2}",
        "[1, 2, 3]",
        "{a: [1, 2], b: {c: 3}}",
        "outer:\n  inner: value",
        "- - nested\n  - items",
        "- a: 1\n  b: 2",
        "&anchor value",
        "- &a 1\n- *a",
        "!!str 42",
        "!custom tagged",
        "---\nfirst\n---\nsecond",
        "key: |\n  multi\n  line",
        "list:\n  - a\n  - b",
        "mixed: [1, {a: b}]",
        "key:",
        "- \n- value",
        "? explicit\n: value",
    ];

    let mut failures = Vec::new();

    for input in test_cases {
        let (parsed_nodes, _parse_errors) = crate::parse(input);
        let via_events_nodes = parse_via_events(input);

        if parsed_nodes.len() != via_events_nodes.len() {
            failures.push(format!(
                "Input: {input:?}\n  Node count mismatch: parse={}, via_events={}",
                parsed_nodes.len(),
                via_events_nodes.len()
            ));
            continue;
        }

        for (i, (parsed, via_events)) in
            parsed_nodes.iter().zip(via_events_nodes.iter()).enumerate()
        {
            if !nodes_equal_ignoring_structural_spans(parsed, via_events) {
                failures.push(format!(
                    "Input: {input:?}\n  Document {i} mismatch:\n    parse:      {parsed:?}\n    via_events: {via_events:?}"
                ));
            }
        }
    }

    if !failures.is_empty() {
        eprintln!("\n=== Parse vs Event-Pipeline Mismatches ===");
        for failure in &failures {
            eprintln!("{failure}\n");
        }
        panic!(
            "{} test case(s) failed - parse() output differs from the event pipeline",
            failures.len()
        );
    }
}

#[test]
fn test_parser_simple_scalar() {
    let nodes = parse_via_events("hello");
    assert_eq!(nodes.len(), 1);
    assert!(matches!(&nodes[0].value, Value::String(s) if s == "hello"));
}

#[test]
fn test_event_parser_typed_scalars() {
    let test_cases: &[(&str, fn(&Value) -> bool)] = &[
        ("true", |v| matches!(v, Value::Bool(true))),
        ("false", |v| matches!(v, Value::Bool(false))),
        ("null", |v| matches!(v, Value::Null)),
        ("42", |v| matches!(v, Value::Int(Integer::I64(42)))),
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
fn test_core_schema_plain_scalar_resolution() {
    let test_cases: &[(&str, fn(&Value) -> bool)] = &[
        ("0o7", |value| matches!(value, Value::Int(Integer::I64(7)))),
        ("0x3A", |value| {
            matches!(value, Value::Int(Integer::I64(58)))
        }),
        (
            ".inf",
            |value| matches!(value, Value::Float(float) if float.is_infinite() && float.is_sign_positive()),
        ),
        (
            "-.Inf",
            |value| matches!(value, Value::Float(float) if float.is_infinite() && float.is_sign_negative()),
        ),
        (
            "+.INF",
            |value| matches!(value, Value::Float(float) if float.is_infinite() && float.is_sign_positive()),
        ),
        (
            ".nan",
            |value| matches!(value, Value::Float(float) if float.is_nan()),
        ),
        ("True", |value| matches!(value, Value::Bool(true))),
        ("FALSE", |value| matches!(value, Value::Bool(false))),
        ("~", |value| matches!(value, Value::Null)),
    ];

    for (input, check) in test_cases {
        let (docs, errors) = parse(input);
        assert!(
            errors.is_empty(),
            "unexpected errors for {input:?}: {errors:?}"
        );
        assert_eq!(docs.len(), 1, "expected one document for {input:?}");
        assert!(
            check(&docs[0].value),
            "unexpected value for {input:?}: {:?}",
            docs[0].value
        );
    }
}

#[test]
fn test_empty_scalar_in_value_position_resolves_as_null() {
    let (docs, errors) = parse("key:");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    let Value::Mapping(pairs) = &docs[0].value else {
        panic!("expected mapping");
    };
    assert!(matches!(pairs[0].value.value, Value::Null));
}

#[test]
fn test_quoted_scalars_do_not_undergo_implicit_resolution() {
    for input in ["\"0o7\"", "'.inf'"] {
        let (docs, errors) = parse(input);
        assert!(
            errors.is_empty(),
            "unexpected errors for {input:?}: {errors:?}"
        );
        assert!(
            matches!(docs[0].value, Value::String(_)),
            "expected quoted scalar to stay string for {input:?}, got {:?}",
            docs[0].value
        );
    }
}

#[test]
fn test_explicit_builtin_tags_override_resolution() {
    let test_cases: &[(&str, fn(&Value) -> bool)] = &[
        (
            "!!str 42",
            |value| matches!(value, Value::String(text) if text == "42"),
        ),
        ("!!int 0o52", |value| {
            matches!(value, Value::Int(Integer::I64(42)))
        }),
        ("!!int 0x2A", |value| {
            matches!(value, Value::Int(Integer::I64(42)))
        }),
        (
            "!!float .inf",
            |value| matches!(value, Value::Float(float) if float.is_infinite() && float.is_sign_positive()),
        ),
        ("!!bool TRUE", |value| matches!(value, Value::Bool(true))),
    ];

    for (input, check) in test_cases {
        let (docs, errors) = parse(input);
        assert!(
            errors.is_empty(),
            "unexpected errors for {input:?}: {errors:?}"
        );
        assert!(
            check(&docs[0].value),
            "unexpected value for {input:?}: {:?}",
            docs[0].value
        );
    }
}

#[test]
fn test_invalid_explicit_builtin_tags_report_error_and_recover_to_string() {
    for input in ["!!int hello", "!!float nope", "!!bool 1"] {
        let (docs, errors) = parse(input);
        assert!(
            errors
                .iter()
                .any(|error| error.kind == ErrorKind::InvalidValue),
            "expected InvalidValue error for {input:?}, got {errors:?}"
        );
        let expected_text = input.split_once(' ').map_or("", |(_, text)| text);
        assert!(
            matches!(docs[0].value, Value::String(ref text) if text.as_ref() == expected_text),
            "expected string recovery for {input:?}, got {:?}",
            docs[0].value
        );
    }
}

#[test]
fn test_explicit_builtin_tags_accept_valid_yaml_lexemes() {
    let cases = [
        ("!!float 42", Value::Float(42.0)),
        ("!!null ~", Value::Null),
        (
            "-0x80000000000000000000000000000000",
            Value::Int(Integer::I128(i128::MIN)),
        ),
    ];

    for (input, expected) in cases {
        let (docs, errors) = parse(input);
        assert!(
            errors.is_empty(),
            "unexpected errors for {input:?}: {errors:?}"
        );
        assert_eq!(docs[0].value, expected, "unexpected value for {input:?}");
    }
}

#[test]
fn test_non_specific_and_custom_tags_disable_implicit_resolution() {
    let (non_specific_docs, non_specific_errors) = parse("! 42");
    assert!(
        non_specific_errors.is_empty(),
        "unexpected errors: {non_specific_errors:?}"
    );
    assert!(matches!(non_specific_docs[0].value, Value::String(ref text) if text == "42"));
    assert_eq!(
        non_specific_docs[0].tag().map(|tag| tag.value.as_ref()),
        Some("!")
    );

    let (custom_docs, custom_errors) = parse("!custom 42");
    assert!(
        custom_errors.is_empty(),
        "unexpected errors: {custom_errors:?}"
    );
    assert!(matches!(custom_docs[0].value, Value::String(ref text) if text == "42"));
    assert_eq!(
        custom_docs[0].tag().map(|tag| tag.value.as_ref()),
        Some("!custom")
    );

    let (explicit_str_docs, explicit_str_errors) = parse("!<tag:yaml.org,2002:str> 42");
    assert!(
        explicit_str_errors.is_empty(),
        "unexpected errors: {explicit_str_errors:?}"
    );
    assert!(matches!(explicit_str_docs[0].value, Value::String(ref text) if text == "42"));

    let (explicit_int_docs, explicit_int_errors) = parse("!<tag:yaml.org,2002:int> 0o52");
    assert!(
        explicit_int_errors.is_empty(),
        "unexpected errors: {explicit_int_errors:?}"
    );
    assert!(matches!(
        explicit_int_docs[0].value,
        Value::Int(Integer::I64(42))
    ));
}

#[test]
fn test_event_parser_simple_mapping() {
    let nodes = parse_via_events("a: 1\nb: 2");
    assert_eq!(nodes.len(), 1);

    if let Value::Mapping(pairs) = &nodes[0].value {
        assert_eq!(pairs.len(), 2);
        assert!(matches!(&pairs[0].key.value, Value::String(s) if s == "a"));
        assert!(matches!(&pairs[0].value.value, Value::Int(Integer::I64(1))));
        assert!(matches!(&pairs[1].key.value, Value::String(s) if s == "b"));
        assert!(matches!(&pairs[1].value.value, Value::Int(Integer::I64(2))));
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
        assert!(matches!(&items[0].node.value, Value::String(s) if s == "a"));
        assert!(matches!(&items[1].node.value, Value::String(s) if s == "b"));
        assert!(matches!(&items[2].node.value, Value::String(s) if s == "c"));
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
        assert!(matches!(&items[0].node.value, Value::Int(Integer::I64(1))));
        assert!(matches!(&items[1].node.value, Value::Int(Integer::I64(2))));
        assert!(matches!(&items[2].node.value, Value::Int(Integer::I64(3))));
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
        assert_eq!(items[0].node.anchor(), Some("anchor"));
        assert!(matches!(&items[0].node.value, Value::String(s) if s == "value"));
        assert!(matches!(&items[1].node.value, Value::String(s) if s == "value"));
        assert_eq!(items[1].node.anchor(), None);
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
        assert!(matches!(&pairs[0].key.value, Value::String(s) if s == "outer"));
        if let Value::Mapping(inner_pairs) = &pairs[0].value.value {
            assert_eq!(inner_pairs.len(), 1);
            assert!(matches!(&inner_pairs[0].key.value, Value::String(s) if s == "inner"));
            assert!(matches!(&inner_pairs[0].value.value, Value::String(s) if s == "value"));
        } else {
            panic!("Expected inner mapping, got: {:?}", pairs[0].value.value);
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
        if let Value::Sequence(nested) = &items[0].node.value {
            assert_eq!(nested.len(), 2);
            assert!(matches!(&nested[0].node.value, Value::String(s) if s == "a"));
            assert!(matches!(&nested[1].node.value, Value::String(s) if s == "b"));
        } else {
            panic!("Expected nested sequence, got: {:?}", items[0].node.value);
        }
        assert!(matches!(&items[1].node.value, Value::String(s) if s == "c"));
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
        if let Value::Mapping(pairs) = &items[0].node.value {
            assert!(matches!(&pairs[0].key.value, Value::String(s) if s == "a"));
            assert!(matches!(&pairs[0].value.value, Value::Int(Integer::I64(1))));
        } else {
            panic!("Expected mapping, got: {:?}", items[0].node.value);
        }
    } else {
        panic!("Expected sequence, got: {:?}", nodes[0].value);
    }
}
