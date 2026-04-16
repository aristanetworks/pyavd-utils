// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Implementation of ValidatableValue traits for yaml_parser types.

use std::borrow::Cow;

use yaml_parser::Integer;
use yaml_parser::MappingPair;
use yaml_parser::Node;
use yaml_parser::SequenceItem;
use yaml_parser::Value;

use super::ValidatableMapping;
use super::ValidatableMappingPair;
use super::ValidatableSequence;
use super::ValidatableValue;

// === ValidatableValue for yaml_parser::Node ===

impl<'input> ValidatableValue for Node<'input> {
    type Mapping<'a>
        = NodeMapping<'a, 'input>
    where
        Self: 'a;
    type Sequence<'a>
        = &'a [SequenceItem<'input>]
    where
        Self: 'a;
    type Coerced = Node<'static>;

    fn is_null(&self) -> bool {
        matches!(self.value, Value::Null)
    }

    fn is_str(&self) -> bool {
        matches!(self.value, Value::String(_))
    }

    fn is_int(&self) -> bool {
        matches!(self.value, Value::Int(_))
    }

    fn is_bool(&self) -> bool {
        matches!(self.value, Value::Bool(_))
    }

    fn as_str(&self) -> Option<Cow<'_, str>> {
        match &self.value {
            Value::String(cow) => Some(Cow::Borrowed(cow.as_ref())),
            Value::Int(i) => match i {
                Integer::I64(i) => Some(Cow::Owned(i.to_string())),
                Integer::U64(u) => Some(Cow::Owned(u.to_string())),
                Integer::I128(i) => Some(Cow::Owned(i.to_string())),
                Integer::U128(u) => Some(Cow::Owned(u.to_string())),
                Integer::BigIntStr(s) => Some(s.clone()),
            },
            Value::Float(f) => Some(Cow::Owned(f.to_string())),
            // Using Title case to match Python behavior
            Value::Bool(b) => Some(Cow::Borrowed(if *b { "True" } else { "False" })),
            _ => None,
        }
    }

    fn as_i64(&self) -> Option<i64> {
        match &self.value {
            Value::Int(Integer::I64(i)) => Some(*i),
            Value::Float(float) => integral_float_to_i64(*float),
            Value::String(s) => s.parse().ok(),
            Value::Bool(b) => Some(if *b { 1 } else { 0 }),
            _ => None,
        }
    }

    fn as_bool(&self) -> Option<bool> {
        match &self.value {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    fn as_mapping(&self) -> Option<Self::Mapping<'_>> {
        match &self.value {
            Value::Mapping(pairs) => Some(NodeMapping { pairs }),
            _ => None,
        }
    }

    fn as_sequence(&self) -> Option<Self::Sequence<'_>> {
        match &self.value {
            Value::Sequence(items) => Some(items.as_slice()),
            _ => None,
        }
    }

    fn get(&self, key: &str) -> Option<&Self> {
        match &self.value {
            Value::Mapping(pairs) => {
                for pair in pairs {
                    if let Some(key_str) = coerce_key_to_string(&pair.key)
                        && key_str == key
                    {
                        return Some(&pair.value);
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn value_type(&self) -> crate::feedback::Type {
        use crate::feedback::Type;
        match &self.value {
            Value::Null => Type::Null,
            Value::Bool(_) => Type::Bool,
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::String(_) => Type::Str,
            Value::Sequence(_) => Type::List,
            Value::Mapping(_) => Type::Dict,
        }
    }

    // === Coercion builders ===
    // These preserve the span from the original node.

    fn coerce_null(&self) -> Self::Coerced {
        Node::new(Value::Null, self.span)
    }

    fn coerce_bool(&self, value: bool) -> Self::Coerced {
        Node::new(Value::Bool(value), self.span)
    }

    fn coerce_int(&self, value: i64) -> Self::Coerced {
        Node::new(Value::Int(Integer::I64(value)), self.span)
    }

    fn coerce_str(&self, value: String) -> Self::Coerced {
        Node::new(Value::String(Cow::Owned(value)), self.span)
    }

    fn coerce_sequence(&self, items: Vec<Self::Coerced>) -> Self::Coerced {
        Node::new(
            Value::Sequence(
                items
                    .into_iter()
                    .map(|node| SequenceItem::new(node.span, node))
                    .collect(),
            ),
            self.span,
        )
    }

    fn coerce_mapping(&self, items: Vec<(String, Self::Coerced)>) -> Self::Coerced {
        // Convert string keys to Node keys
        let pairs: Vec<MappingPair<'static>> = items
            .into_iter()
            .map(|(key, value)| {
                let key_span = yaml_parser::Span::at(value.span.start);
                let pair_span = yaml_parser::Span::new(key_span.start..value.span.end);
                let key_node = Node::new(Value::String(Cow::Owned(key)), key_span);
                MappingPair::new(pair_span, key_node, value)
            })
            .collect();
        Node::new(Value::Mapping(pairs), self.span)
    }

    fn clone_to_coerced(&self) -> Self::Coerced {
        self.clone().into_owned()
    }

    fn to_feedback_value(&self) -> crate::feedback::Value {
        use crate::feedback::Value as FV;
        match &self.value {
            Value::Null => FV::Null(),
            Value::Bool(b) => FV::Bool(*b),
            Value::Int(Integer::I64(i)) => FV::Int(*i),
            Value::Int(_) => FV::Str(self.as_str().unwrap().into_owned()),
            Value::Float(f) => FV::Float(*f),
            Value::String(s) => FV::Str(s.to_string()),
            Value::Sequence(seq) => FV::List(
                seq.iter()
                    .map(|item| item.node.to_feedback_value())
                    .collect(),
            ),
            Value::Mapping(map) => FV::Dict(
                map.iter()
                    .filter_map(|pair| {
                        pair.key
                            .as_str()
                            .map(|key| (key.to_string(), pair.value.to_feedback_value()))
                    })
                    .collect(),
            ),
        }
    }

    fn is_float(&self) -> bool {
        matches!(self.value, Value::Float(_))
    }

    fn source_span(&self) -> Option<crate::feedback::SourceSpan> {
        Some(crate::feedback::SourceSpan {
            start: self.span.start_usize(),
            end: self.span.end_usize(),
        })
    }
}

// === Wrapper for yaml_parser Mapping ===

/// A wrapper around yaml_parser's mapping representation.
///
/// Note: YAML mappings can have non-string keys, but for validation purposes
/// we coerce scalar keys (int, bool, float) to strings. Only complex keys
/// (mappings, sequences) are skipped.
pub struct NodeMapping<'a, 'input> {
    pairs: &'a [MappingPair<'input>],
}

/// Try to coerce a YAML node to a string key.
/// Returns None for complex types (mappings, sequences, null).
fn coerce_key_to_string<'a>(node: &'a Node<'_>) -> Option<Cow<'a, str>> {
    match &node.value {
        Value::String(s) => Some(Cow::Borrowed(s.as_ref())),
        Value::Int(i) => Some(i.to_decimal_string()),
        Value::Float(f) => Some(Cow::Owned(f.to_string())),
        // Mapping keys are looked up using YAML's lowercase bool spelling.
        Value::Bool(b) => Some(Cow::Borrowed(if *b { "true" } else { "false" })),
        Value::Null | Value::Sequence(_) | Value::Mapping(_) => None,
    }
}

fn integral_float_to_i64(float: f64) -> Option<i64> {
    if float.is_finite()
        && float.fract() == 0.0
        && float >= i64::MIN as f64
        && float <= i64::MAX as f64
    {
        Some(float as i64)
    } else {
        None
    }
}

impl<'a, 'input: 'a> ValidatableMapping<'a> for NodeMapping<'a, 'input> {
    type Value = Node<'input>;
    type SchemaDataMapping<'s>
        = yaml_parser::YamlMapping<'s, 'input>
    where
        Self: 's;
    type Pair = NodeMappingPair<'a, 'input>;
    type Iter = NodeMappingIter<'a, 'input>;

    fn get(&self, key: &str) -> Option<&Self::Value> {
        // This is intentionally a linear scan: YAML mappings preserve order,
        // may contain duplicate keys, and may use non-string scalar keys that
        // are coerced at lookup time.
        for pair in self.pairs {
            if let Some(k_str) = coerce_key_to_string(&pair.key)
                && k_str == key
            {
                return Some(&pair.value);
            }
        }
        None
    }

    fn as_schema_data_mapping(&self) -> Self::SchemaDataMapping<'_> {
        yaml_parser::YamlMapping::new(self.pairs)
    }

    fn contains_key(&self, key: &str) -> bool {
        self.get(key).is_some()
    }

    fn iter(&self) -> Self::Iter {
        NodeMappingIter {
            inner: self.pairs.iter(),
        }
    }

    fn len(&self) -> usize {
        // Count entries with coercible keys
        self.pairs
            .iter()
            .filter(|pair| coerce_key_to_string(&pair.key).is_some())
            .count()
    }
}

/// Iterator over yaml_parser mapping entries with coercible keys.
pub struct NodeMappingIter<'a, 'input> {
    inner: std::slice::Iter<'a, MappingPair<'input>>,
}

impl<'a, 'input: 'a> Iterator for NodeMappingIter<'a, 'input> {
    type Item = NodeMappingPair<'a, 'input>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let pair = self.inner.next()?;
            if let Some(key_str) = coerce_key_to_string(&pair.key) {
                return Some(NodeMappingPair { key_str, pair });
            }
            // Skip non-coercible keys (mappings, sequences, null)
        }
    }
}

pub struct NodeMappingPair<'a, 'input> {
    key_str: Cow<'a, str>,
    pair: &'a MappingPair<'input>,
}

impl<'a, 'input: 'a> ValidatableMappingPair<'a> for NodeMappingPair<'a, 'input> {
    type Value = Node<'input>;

    fn key(&self) -> Cow<'a, str> {
        self.key_str.clone()
    }

    fn value(&self) -> &'a Self::Value {
        &self.pair.value
    }

    fn key_span(&self) -> Option<crate::feedback::SourceSpan> {
        Some(crate::feedback::SourceSpan {
            start: self.pair.key.span.start_usize(),
            end: self.pair.key.span.end_usize(),
        })
    }
}

// === ValidatableSequence for slice of sequence items ===

impl<'a, 'input: 'a> ValidatableSequence<'a> for &'a [SequenceItem<'input>] {
    type Value = Node<'input>;
    type Iter = SequenceIter<'a, 'input>;

    fn iter(&self) -> Self::Iter {
        SequenceIter {
            inner: <[SequenceItem<'input>]>::iter(self),
        }
    }

    fn len(&self) -> usize {
        <[SequenceItem<'input>]>::len(self)
    }
}

pub struct SequenceIter<'a, 'input> {
    inner: std::slice::Iter<'a, SequenceItem<'input>>,
}

impl<'a, 'input: 'a> Iterator for SequenceIter<'a, 'input> {
    type Item = &'a Node<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|item| &item.node)
    }
}

// === Tests for yaml_parser::Node ===

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use yaml_parser::Integer;
    use yaml_parser::MappingPair;
    use yaml_parser::Node;
    use yaml_parser::SequenceItem;
    use yaml_parser::Value;

    use crate::validatable::ValidatableMapping;
    use crate::validatable::ValidatableMappingPair;
    use crate::validatable::ValidatableSequence;
    use crate::validatable::ValidatableValue;

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
