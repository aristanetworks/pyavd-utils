// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;
use std::sync::Arc;

use crate::Node;
use crate::ParsedYaml;
use crate::Value;
use crate::parse_with_source;

#[test]
fn retains_input_allocation_and_borrowed_text_after_move() {
    let input = String::from("hello # comment\n");
    let input_pointer = input.as_ptr();
    let original = ParsedYaml::parse(input);
    let mut values = Vec::new();
    values.push(original);
    let parsed = values.pop().unwrap();

    assert_eq!(parsed.input().as_ptr(), input_pointer);
    assert!(parsed.errors().is_empty());
    let node = parsed.documents().first().unwrap();
    let Value::String(Cow::Borrowed(text)) = &node.value else {
        panic!("expected borrowed scalar");
    };
    assert_eq!(*text, "hello");
    assert_eq!(text.as_ptr(), parsed.input().as_ptr());
    assert_eq!(parsed.input().get(node.span.to_range()), Some("hello"));
    let comment = node.trailing_comment().unwrap();
    assert!(matches!(&comment.text, Cow::Borrowed(_)));
}

#[test]
fn keeps_decoded_strings_owned() {
    let parsed = ParsedYaml::parse(String::from("\"hello\\nworld\""));
    assert!(parsed.errors().is_empty());
    assert!(matches!(
        &parsed.documents().first().unwrap().value,
        Value::String(Cow::Owned(text)) if text == "hello\nworld"
    ));
}

#[test]
fn shares_source_identifier_with_keys_and_nested_nodes() {
    fn assert_source(node: &Node<'_>, source: &Arc<str>) {
        assert!(Arc::ptr_eq(node.source.as_ref().unwrap(), source));
        match &node.value {
            Value::Mapping(pairs) => {
                for pair in pairs {
                    assert_source(&pair.key, source);
                    assert_source(&pair.value, source);
                }
            }
            Value::Sequence(items) => {
                for item in items {
                    assert_source(&item.node, source);
                }
            }
            _ => {}
        }
    }

    let source: Arc<str> = Arc::from("config.yaml");
    let parsed = ParsedYaml::parse_with_source(
        String::from("items:\n  - name: example\n"),
        Some(Arc::clone(&source)),
    );
    assert!(parsed.errors().is_empty());
    assert_source(parsed.documents().first().unwrap(), &source);
}

#[test]
fn matches_owned_parser_documents_metadata_and_errors() {
    let source: Arc<str> = Arc::from("config.yaml");
    for input in [
        "",
        " \n\t\n",
        "# comment only\n",
        "hello",
        "name: café # comment\n---\nitems: [one, two]\n",
        "base: &base {name: example}\ncopy: *base\n",
        "tagged: !custom value\nblock: |\n  hello\n  world\n",
        "valid: yes\ninvalid: *missing\nkept: value\n",
    ] {
        let parsed = ParsedYaml::parse_with_source(input.to_owned(), Some(Arc::clone(&source)));
        let (documents, errors) = parse_with_source(input, Some(Arc::clone(&source)));
        assert_eq!(parsed.input(), input);
        assert_eq!(parsed.documents(), documents, "input: {input:?}");
        assert_eq!(parsed.errors(), errors, "input: {input:?}");
    }
}

#[test]
fn retains_partial_documents_and_error_spans() {
    let parsed = ParsedYaml::parse(String::from("valid: yes\ninvalid: *missing\nkept: value\n"));
    assert!(!parsed.documents().is_empty());
    assert!(!parsed.errors().is_empty());
    for error in parsed.errors() {
        assert!(parsed.input().get(error.span.to_range()).is_some());
    }
    assert!(parsed.documents().first().unwrap().source.is_none());
}
