// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde_json::json;

use super::{ValidatableSequence, ValidatableValue};

#[test]
fn test_serde_json_null() {
    let value = json!(null);
    assert!(value.is_null());
    assert!(value.as_str().is_none());
    assert!(value.as_mapping().is_none());
}

#[test]
fn test_serde_json_string() {
    let value = json!("hello");
    assert!(!value.is_null());
    assert_eq!(value.as_str(), Some("hello"));
    assert!(value.as_i64().is_none());
    assert!(value.as_bool().is_none());
}

#[test]
fn test_serde_json_integer() {
    let value = json!(42);
    // Native as_i64 works
    assert_eq!(value.as_i64(), Some(42));
    // Trait as_str coerces integer to string
    assert_eq!(ValidatableValue::as_str(&value).as_deref(), Some("42"));
}

#[test]
fn test_serde_json_float_to_str_coercion() {
    let value = json!(1.5);
    // Trait as_str coerces float to string
    assert_eq!(ValidatableValue::as_str(&value).as_deref(), Some("1.5"));
}

#[test]
fn test_serde_json_float_value_type_is_not_int() {
    let value = json!(1.5);
    assert_eq!(value.value_type(), crate::feedback::Type::Float);
}

#[test]
fn test_serde_json_bool() {
    let value_true = json!(true);
    let value_false = json!(false);
    assert_eq!(value_true.as_bool(), Some(true));
    assert_eq!(value_false.as_bool(), Some(false));
    // Trait as_str coerces bool to string (Title case to match Python behavior)
    assert_eq!(
        ValidatableValue::as_str(&value_true).as_deref(),
        Some("True")
    );
    assert_eq!(
        ValidatableValue::as_str(&value_false).as_deref(),
        Some("False")
    );
}

#[test]
fn test_serde_json_str_to_int_coercion() {
    let value = json!("123");
    // Trait as_i64 coerces string to int if parseable
    assert_eq!(ValidatableValue::as_i64(&value), Some(123));
    // Invalid string does not coerce
    let invalid = json!("not a number");
    assert!(ValidatableValue::as_i64(&invalid).is_none());
}

#[test]
fn test_serde_json_mapping() {
    let value = json!({
        "name": "Alice",
        "age": 30
    });

    let mapping = value.as_mapping().expect("should be a mapping");
    assert_eq!(mapping.len(), 2);
    assert!(!mapping.is_empty());
    assert!(mapping.contains_key("name"));
    assert!(!mapping.contains_key("missing"));

    let name = mapping.get("name").expect("should have name");
    assert_eq!(name.as_str(), Some("Alice"));

    let keys: Vec<_> = mapping.keys().collect();
    assert!(keys.contains(&&"name".to_owned()));
    assert!(keys.contains(&&"age".to_owned()));
}

#[test]
fn test_serde_json_sequence() {
    let value = json!([1, 2, 3]);

    let seq = value.as_sequence().expect("should be a sequence");
    assert_eq!(seq.len(), 3);
    assert!(!seq.is_empty());

    let items: Vec<i64> = seq.iter().filter_map(|v| v.as_i64()).collect();
    assert_eq!(items, vec![1, 2, 3]);
}

#[test]
fn test_serde_json_get() {
    let value = json!({
        "nested": {
            "key": "value"
        }
    });

    let nested = value.get("nested").expect("should have nested");
    let key = nested.get("key").expect("should have key");
    assert_eq!(key.as_str(), Some("value"));

    assert!(value.get("missing").is_none());
}

#[test]
fn test_serde_json_empty_mapping() {
    let value = json!({});
    let mapping = value.as_mapping().expect("should be a mapping");
    assert!(mapping.is_empty());
    assert_eq!(mapping.len(), 0);
}

#[test]
fn test_serde_json_empty_sequence() {
    let value = json!([]);
    let seq = value.as_sequence().expect("should be a sequence");
    assert!(seq.is_empty());
    assert_eq!(seq.len(), 0);
}

// === Tests for yaml_parser::Node ===

mod yaml_parser_tests {
    use std::borrow::Cow;

    use yaml_parser::{Integer, MappingPair, Node, SequenceItem, Value};

    use crate::validatable::{
        ValidatableMapping, ValidatableMappingPair, ValidatableSequence, ValidatableValue,
    };

    fn make_span() -> yaml_parser::Span {
        yaml_parser::Span::new(0..1)
    }

    fn string_node(s: &str) -> Node<'static> {
        Node::new(Value::String(Cow::Owned(s.to_owned())), make_span())
    }

    fn int_node(i: i64) -> Node<'static> {
        Node::new(Value::Int(Integer::I64(i)), make_span())
    }

    #[test]
    fn test_yaml_null() {
        let node = Node::new(Value::Null, make_span());
        assert!(node.is_null());
        assert!(node.as_str().is_none());
        assert!(node.as_mapping().is_none());
    }

    #[test]
    fn test_yaml_string() {
        let node = string_node("hello");
        assert!(!node.is_null());
        assert_eq!(node.as_str().as_deref(), Some("hello"));
        assert!(node.as_i64().is_none());
    }

    #[test]
    fn test_yaml_integer() {
        let node = int_node(42);
        assert_eq!(node.as_i64(), Some(42));
        // Integer coerces to string
        assert_eq!(node.as_str().as_deref(), Some("42"));
    }

    #[test]
    fn test_yaml_float_to_str_coercion() {
        let node = Node::new(Value::Float(1.5), make_span());
        // Float coerces to string
        assert_eq!(node.as_str().as_deref(), Some("1.5"));
    }

    #[test]
    fn test_yaml_float_value_type_is_not_int() {
        let node = Node::new(Value::Float(1.5), make_span());
        assert_ne!(node.value_type(), crate::feedback::Type::Int);
    }

    #[test]
    fn test_yaml_bool() {
        let node_true = Node::new(Value::Bool(true), make_span());
        let node_false = Node::new(Value::Bool(false), make_span());
        assert_eq!(node_true.as_bool(), Some(true));
        assert_eq!(node_false.as_bool(), Some(false));
        // Bool coerces to string (Title case to match Python behavior)
        assert_eq!(node_true.as_str().as_deref(), Some("True"));
        assert_eq!(node_false.as_str().as_deref(), Some("False"));
    }

    #[test]
    fn test_yaml_str_to_int_coercion() {
        let node = string_node("123");
        // String coerces to int if parseable
        assert_eq!(node.as_i64(), Some(123));
        // Invalid string does not coerce
        let invalid = string_node("not a number");
        assert!(invalid.as_i64().is_none());
    }

    #[test]
    fn test_yaml_mapping() {
        let node = Node::new(
            Value::Mapping(vec![
                MappingPair::new(make_span(), string_node("name"), string_node("Alice")),
                MappingPair::new(make_span(), string_node("age"), int_node(30)),
            ]),
            make_span(),
        );

        let mapping = node.as_mapping().expect("should be a mapping");
        assert_eq!(mapping.len(), 2);
        assert!(!mapping.is_empty());
        assert!(mapping.contains_key("name"));
        assert!(!mapping.contains_key("missing"));

        let name = mapping.get("name").expect("should have name");
        assert_eq!(name.as_str().as_deref(), Some("Alice"));

        // Test iteration
        let keys: Vec<String> = ValidatableMapping::iter(&mapping)
            .map(|pair| pair.key().into_owned())
            .collect();
        assert!(keys.contains(&"name".to_string()));
        assert!(keys.contains(&"age".to_string()));
    }

    #[test]
    fn test_yaml_sequence() {
        let node = Node::new(
            Value::Sequence(vec![
                SequenceItem::new(make_span(), int_node(1)),
                SequenceItem::new(make_span(), int_node(2)),
                SequenceItem::new(make_span(), int_node(3)),
            ]),
            make_span(),
        );

        let seq = node.as_sequence().expect("should be a sequence");
        assert_eq!(seq.len(), 3);
        assert!(!seq.is_empty());

        let items: Vec<i64> = ValidatableSequence::iter(&seq)
            .filter_map(|v| v.as_i64())
            .collect();
        assert_eq!(items, vec![1, 2, 3]);
    }

    #[test]
    fn test_yaml_get() {
        let node = Node::new(
            Value::Mapping(vec![MappingPair::new(
                make_span(),
                string_node("nested"),
                Node::new(
                    Value::Mapping(vec![MappingPair::new(
                        make_span(),
                        string_node("key"),
                        string_node("value"),
                    )]),
                    make_span(),
                ),
            )]),
            make_span(),
        );

        let nested = node.get("nested").expect("should have nested");
        let key = nested.get("key").expect("should have key");
        assert_eq!(key.as_str().as_deref(), Some("value"));

        assert!(node.get("missing").is_none());
    }

    #[test]
    fn test_yaml_int_key_coercion() {
        // YAML allows non-string keys like: `123: value`
        // These should be coerced to string keys
        let node = Node::new(
            Value::Mapping(vec![
                MappingPair::new(make_span(), int_node(123), string_node("int_key_value")),
                MappingPair::new(
                    make_span(),
                    Node::new(Value::Bool(true), make_span()),
                    string_node("bool_key_value"),
                ),
            ]),
            make_span(),
        );

        let mapping = node.as_mapping().expect("should be a mapping");

        // Int key 123 should be coerced to "123"
        let value = mapping.get("123").expect("should find int key as string");
        assert_eq!(value.as_str().as_deref(), Some("int_key_value"));

        // Bool key true should be coerced to "true"
        let value = mapping.get("true").expect("should find bool key as string");
        assert_eq!(value.as_str().as_deref(), Some("bool_key_value"));

        // Iteration should also coerce keys
        let keys: Vec<String> = ValidatableMapping::iter(&mapping)
            .map(|pair| pair.key().into_owned())
            .collect();
        assert!(keys.contains(&"123".to_string()));
        assert!(keys.contains(&"true".to_string()));
    }

    #[test]
    fn test_yaml_get_coerces_scalar_keys() {
        let node = Node::new(
            Value::Mapping(vec![
                MappingPair::new(make_span(), int_node(123), string_node("int_key_value")),
                MappingPair::new(
                    make_span(),
                    Node::new(Value::Bool(true), make_span()),
                    string_node("bool_key_value"),
                ),
            ]),
            make_span(),
        );

        let int_value = node.get("123").expect("should find int key through get()");
        assert_eq!(int_value.as_str().as_deref(), Some("int_key_value"));

        let bool_value = node
            .get("true")
            .expect("should find bool key through get()");
        assert_eq!(bool_value.as_str().as_deref(), Some("bool_key_value"));
    }

    #[test]
    fn test_yaml_coerce_mapping_uses_distinct_key_and_value_spans() {
        let original = Node::new(Value::Null, yaml_parser::Span::new(10..20));
        let coerced = original.coerce_mapping(vec![(
            "foo".to_owned(),
            Node::new(
                Value::String(Cow::Owned("bar".to_owned())),
                yaml_parser::Span::new(14..20),
            ),
        )]);

        let Value::Mapping(pairs) = coerced.value else {
            panic!("coerce_mapping should create a mapping");
        };
        let pair = pairs.first().expect("coerce_mapping should emit one pair");

        assert_eq!(pair.key.span, yaml_parser::Span::new(14..14));
        assert_eq!(pair.value.span, yaml_parser::Span::new(14..20));
        assert_eq!(pair.pair_span, yaml_parser::Span::new(14..20));
    }
}
