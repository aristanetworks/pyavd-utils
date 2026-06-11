// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Implementation of `ValidatableValue` traits for `yaml_parser` types.

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
use super::integral_float_to_i64;

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
    type CoercedMappingItem = MappingPair<'static>;

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
            Value::Int(integer) => Some(integer.to_decimal_string()),
            Value::Float(float) => Some(Cow::Owned(float.to_string())),
            // Using Title case to match Python behavior
            Value::Bool(boolean) => Some(Cow::Borrowed(if *boolean { "True" } else { "False" })),
            _ => None,
        }
    }

    fn as_i64(&self) -> Option<i64> {
        match &self.value {
            Value::Int(integer) => integer.as_i64(),
            Value::Float(float) => integral_float_to_i64(*float),
            Value::String(string) => string.parse().ok(),
            Value::Bool(boolean) => Some(i64::from(*boolean)),
            _ => None,
        }
    }

    fn as_bool(&self) -> Option<bool> {
        match &self.value {
            Value::Bool(boolean) => Some(*boolean),
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
                    if let Some(key_str) = schema_key_to_string(&pair.key)
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
    // Properties and trailing_comment are ignored since they are not used later and are expensive to clone.
    // TODO: Consider a simpler Coerced type where we don't carry unused metadata.

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

    fn coerce_mapping(&self, items: Vec<Self::CoercedMappingItem>) -> Self::Coerced {
        Node::new(Value::Mapping(items), self.span)
    }

    fn clone_to_coerced(&self) -> Self::Coerced {
        self.clone().into_owned()
    }

    fn to_feedback_value(&self) -> crate::feedback::Value {
        use crate::feedback::Value as FV;
        match &self.value {
            Value::Null => FV::Null(),
            Value::Bool(boolean) => FV::Bool(*boolean),
            Value::Int(integer) => integer
                .as_i64()
                .map_or_else(|| FV::Str(integer.to_string()), FV::Int),
            Value::Float(float) => FV::Float(*float),
            Value::String(string) => FV::Str(string.to_string()),
            Value::Sequence(seq) => FV::List(
                seq.iter()
                    .map(|item| item.node.to_feedback_value())
                    .collect(),
            ),
            Value::Mapping(map) => FV::Dict(
                map.iter()
                    .filter_map(|pair| {
                        scalar_key_to_string(&pair.key)
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

/// A wrapper around `yaml_parser`'s mapping representation.
///
/// Note: YAML mappings can have non-string keys, but only string keys
/// participate in schema lookups.
pub struct NodeMapping<'a, 'input> {
    pairs: &'a [MappingPair<'input>],
}

/// Return the schema lookup key for YAML mappings.
///
/// AVD schemas only define string keys, so non-string YAML keys do not match
/// schema keys even if they have a scalar textual representation.
fn schema_key_to_string<'a>(node: &'a Node<'_>) -> Option<Cow<'a, str>> {
    match &node.value {
        Value::String(string) => Some(Cow::Borrowed(string.as_ref())),
        _ => None,
    }
}

fn scalar_key_to_string<'a>(node: &'a Node<'_>) -> Option<Cow<'a, str>> {
    match &node.value {
        Value::String(string) => Some(Cow::Borrowed(string.as_ref())),
        Value::Int(integer) => Some(integer.to_decimal_string()),
        Value::Float(float) => Some(Cow::Owned(float.to_string())),
        Value::Bool(boolean) => Some(Cow::Borrowed(if *boolean { "true" } else { "false" })),
        _ => None,
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
        // This is intentionally a linear scan: YAML mappings preserve order
        // and may contain duplicate keys.
        for pair in self.pairs {
            if let Some(k_str) = schema_key_to_string(&pair.key)
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
        self.pairs.len()
    }
}

/// Iterator over `yaml_parser` mapping entries.
pub struct NodeMappingIter<'a, 'input> {
    inner: std::slice::Iter<'a, MappingPair<'input>>,
}

impl<'a, 'input: 'a> Iterator for NodeMappingIter<'a, 'input> {
    type Item = NodeMappingPair<'a, 'input>;

    fn next(&mut self) -> Option<Self::Item> {
        let pair = self.inner.next()?;
        Some(NodeMappingPair { pair })
    }
}

/// Validation view over a YAML mapping pair.
///
/// Keeps access to the original key node so validation can distinguish schema
/// lookup keys from display keys and preserve non-string keys in coerced output.
pub struct NodeMappingPair<'a, 'input> {
    pair: &'a MappingPair<'input>,
}

impl<'a, 'input: 'a> ValidatableMappingPair<'a> for NodeMappingPair<'a, 'input> {
    type Value = Node<'input>;

    /// Return only string YAML keys for schema lookup.
    ///
    /// Non-string YAML keys are valid YAML, but they do not match AVD schema
    /// keys and are handled as allowed/unknown keys by dict validation.
    fn schema_key(&self) -> Option<Cow<'a, str>> {
        schema_key_to_string(&self.pair.key)
    }

    /// Return a path-friendly representation for diagnostics.
    ///
    /// This is deliberately broader than `schema_key` so numeric, boolean,
    /// null, and complex YAML keys can still produce a useful validation path.
    fn display_key(&self) -> Cow<'a, str> {
        match &self.pair.key.value {
            Value::String(string) => Cow::Borrowed(string.as_ref()),
            Value::Int(integer) => integer.to_decimal_string(),
            Value::Float(float) => Cow::Owned(float.to_string()),
            Value::Bool(boolean) => Cow::Borrowed(if *boolean { "true" } else { "false" }),
            Value::Null => Cow::Borrowed("null"),
            Value::Sequence(_) | Value::Mapping(_) => Cow::Borrowed("<complex key>"),
        }
    }

    /// Return the YAML value associated with this pair.
    fn value(&self) -> &'a Self::Value {
        &self.pair.value
    }

    /// Build the YAML mapping pair for coerced output.
    ///
    /// Cheap-cloning everything as most strings are Cow<Borrow> and only replacing value.
    fn coerced_item(
        &self,
        value: <Self::Value as ValidatableValue>::Coerced,
    ) -> <Self::Value as ValidatableValue>::CoercedMappingItem {
        let mut pair = self.pair.clone();
        pair.value = value;
        pair.into_owned()
    }

    /// Return the span for the original YAML key node.
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
    use crate::validatable::ValidatableMappingPair as _;
    use crate::validatable::ValidatableSequence;
    use crate::validatable::ValidatableValue;

    fn make_span() -> yaml_parser::Span {
        yaml_parser::Span::new(0..1)
    }

    fn string_node(string: &str) -> Node<'static> {
        Node::new(Value::String(Cow::Owned(string.to_owned())), make_span())
    }

    fn int_node(i: i64) -> Node<'static> {
        Node::new(Value::Int(Integer::I64(i)), make_span())
    }

    fn bool_node(value: bool) -> Node<'static> {
        Node::new(Value::Bool(value), make_span())
    }

    fn float_node(value: f64) -> Node<'static> {
        Node::new(Value::Float(value), make_span())
    }

    #[test]
    fn test_yaml_null() {
        let node = Node::new(Value::Null, make_span());
        assert!(node.is_null());
        assert!(node.as_str().is_none());
        assert!(node.as_mapping().is_none());
    }

    #[test]
    fn test_yaml_scalar_type_checks() {
        let null = Node::new(Value::Null, make_span());
        let string = string_node("hello");
        let int = int_node(42);
        let bool = bool_node(true);
        let float = float_node(42.0);
        let sequence = Node::new(Value::Sequence(Vec::new()), make_span());
        let mapping = Node::new(Value::Mapping(Vec::new()), make_span());

        assert!(string.is_str());
        assert!(int.is_int());
        assert!(bool.is_bool());
        assert!(float.is_float());
        assert!(!null.is_str());
        assert!(!string.is_int());
        assert!(!int.is_bool());
        assert!(!bool.is_float());

        assert_eq!(null.value_type(), crate::feedback::Type::Null);
        assert_eq!(string.value_type(), crate::feedback::Type::Str);
        assert_eq!(int.value_type(), crate::feedback::Type::Int);
        assert_eq!(bool.value_type(), crate::feedback::Type::Bool);
        assert_eq!(float.value_type(), crate::feedback::Type::Float);
        assert_eq!(sequence.value_type(), crate::feedback::Type::List);
        assert_eq!(mapping.value_type(), crate::feedback::Type::Dict);

        assert!(string.as_bool().is_none());
        assert!(null.as_i64().is_none());
        assert!(string.as_sequence().is_none());
        assert!(string.get("missing").is_none());
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
    fn test_yaml_integer_variants_coerce_to_strings() {
        let values = [
            (Integer::U64(u64::MAX), u64::MAX.to_string()),
            (Integer::I128(i128::MIN), i128::MIN.to_string()),
            (Integer::U128(u128::MAX), u128::MAX.to_string()),
            (
                Integer::BigIntStr(Cow::Borrowed("340282366920938463463374607431768211456")),
                "340282366920938463463374607431768211456".to_owned(),
            ),
        ];

        for (integer, expected) in values {
            let node = Node::new(Value::Int(integer), make_span());
            assert_eq!(node.as_str().as_deref(), Some(expected.as_str()));
        }
    }

    #[test]
    fn test_yaml_float_to_str_coercion() {
        let node = Node::new(Value::Float(1.5), make_span());
        // Float coerces to string
        assert_eq!(node.as_str().as_deref(), Some("1.5"));
    }

    #[test]
    fn test_yaml_float_to_int_coercion() {
        assert_eq!(float_node(42.0).as_i64(), Some(42));
        assert_eq!(float_node(42.5).as_i64(), None);
        assert_eq!(float_node(f64::INFINITY).as_i64(), None);
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
        assert_eq!(node_true.as_i64(), Some(1));
        assert_eq!(node_false.as_i64(), Some(0));
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
            .map(|pair| pair.display_key().into_owned())
            .collect();
        assert!(keys.contains(&"name".to_owned()));
        assert!(keys.contains(&"age".to_owned()));
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
        assert_eq!(ValidatableSequence::len(&seq), 3);
        assert!(!seq.is_empty());

        let items: Vec<i64> = ValidatableSequence::iter(&seq)
            .filter_map(ValidatableValue::as_i64)
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
    fn test_yaml_mapping_only_matches_string_keys() {
        let node = Node::new(
            Value::Mapping(vec![
                MappingPair::new(
                    make_span(),
                    Node::new(Value::Null, make_span()),
                    string_node("null"),
                ),
                MappingPair::new(
                    make_span(),
                    Node::new(Value::Sequence(Vec::new()), make_span()),
                    string_node("sequence"),
                ),
                MappingPair::new(
                    make_span(),
                    Node::new(Value::Mapping(Vec::new()), make_span()),
                    string_node("mapping"),
                ),
                MappingPair::new(make_span(), float_node(1.5), string_node("float")),
                MappingPair::new(make_span(), bool_node(false), string_node("bool")),
                MappingPair::new(make_span(), string_node("kept"), string_node("string")),
            ]),
            make_span(),
        );

        let mapping = node.as_mapping().expect("should be a mapping");
        assert_eq!(mapping.len(), 6);
        assert!(mapping.get("1.5").is_none());
        assert!(mapping.get("false").is_none());
        assert_eq!(
            mapping
                .get("kept")
                .and_then(ValidatableValue::as_str)
                .as_deref(),
            Some("string")
        );

        let keys: Vec<String> = ValidatableMapping::iter(&mapping)
            .map(|pair| pair.display_key().into_owned())
            .collect();
        assert_eq!(
            keys,
            vec![
                "null",
                "<complex key>",
                "<complex key>",
                "1.5",
                "false",
                "kept"
            ]
        );
    }

    #[test]
    fn test_yaml_non_string_keys_do_not_match_schema_keys() {
        // YAML allows non-string keys like: `123: value`
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

        assert!(mapping.get("123").is_none());
        assert!(mapping.get("true").is_none());

        // Iteration still exposes display keys for paths and diagnostics.
        let keys: Vec<String> = ValidatableMapping::iter(&mapping)
            .map(|pair| pair.display_key().into_owned())
            .collect();
        assert_eq!(keys, vec!["123", "true"]);
    }

    #[test]
    fn test_yaml_coercion_builders_preserve_span() {
        let original = Node::new(Value::Null, yaml_parser::Span::new(10..20));
        let item = Node::new(
            Value::String(Cow::Borrowed("item")),
            yaml_parser::Span::new(12..16),
        );

        let coerced_null = original.coerce_null();
        let coerced_bool = original.coerce_bool(true);
        let coerced_int = original.coerce_int(42);
        let coerced_str = original.coerce_str("value".to_owned());
        let coerced_sequence = original.coerce_sequence(vec![item]);

        assert_eq!(coerced_null.span, original.span);
        assert_eq!(coerced_bool.span, original.span);
        assert_eq!(coerced_int.span, original.span);
        assert_eq!(coerced_str.span, original.span);
        assert_eq!(coerced_sequence.span, original.span);
        assert!(matches!(coerced_null.value, Value::Null));
        assert!(matches!(coerced_bool.value, Value::Bool(true)));
        assert!(matches!(coerced_int.value, Value::Int(Integer::I64(42))));
        assert!(matches!(coerced_str.value, Value::String(value) if value == "value"));
        assert!(matches!(coerced_sequence.value, Value::Sequence(items) if items.len() == 1));
    }

    #[test]
    fn test_yaml_clone_to_coerced_owns_borrowed_data() {
        let node = Node::new(Value::String(Cow::Borrowed("borrowed")), make_span());
        let coerced = node.clone_to_coerced();

        assert!(matches!(coerced.value, Value::String(Cow::Owned(value)) if value == "borrowed"));
    }

    #[test]
    fn test_yaml_feedback_value_conversion() {
        let node = Node::new(
            Value::Mapping(vec![
                MappingPair::new(
                    make_span(),
                    string_node("items"),
                    Node::new(
                        Value::Sequence(vec![
                            SequenceItem::new(make_span(), Node::new(Value::Null, make_span())),
                            SequenceItem::new(make_span(), bool_node(true)),
                            SequenceItem::new(make_span(), int_node(42)),
                            SequenceItem::new(
                                make_span(),
                                Node::new(Value::Int(Integer::U64(u64::MAX)), make_span()),
                            ),
                            SequenceItem::new(make_span(), float_node(1.5)),
                            SequenceItem::new(make_span(), string_node("text")),
                        ]),
                        make_span(),
                    ),
                ),
                MappingPair::new(make_span(), int_node(7), string_node("skipped")),
                MappingPair::new(make_span(), bool_node(true), string_node("bool key")),
                MappingPair::new(
                    make_span(),
                    Node::new(Value::Sequence(Vec::new()), make_span()),
                    string_node("complex key"),
                ),
            ]),
            make_span(),
        );

        let mut expected = std::collections::HashMap::new();
        expected.insert(
            "items".to_owned(),
            crate::feedback::Value::List(vec![
                crate::feedback::Value::Null(),
                crate::feedback::Value::Bool(true),
                crate::feedback::Value::Int(42),
                crate::feedback::Value::Str(u64::MAX.to_string()),
                crate::feedback::Value::Float(1.5),
                crate::feedback::Value::Str("text".to_owned()),
            ]),
        );
        expected.insert(
            "7".to_owned(),
            crate::feedback::Value::Str("skipped".to_owned()),
        );
        expected.insert(
            "true".to_owned(),
            crate::feedback::Value::Str("bool key".to_owned()),
        );

        assert_eq!(
            node.to_feedback_value(),
            crate::feedback::Value::Dict(expected)
        );
    }

    #[test]
    fn test_yaml_get_only_matches_string_keys() {
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

        assert!(node.get("123").is_none());
        assert!(node.get("true").is_none());
    }

    #[test]
    fn test_yaml_coerce_mapping_copies_original_key_and_pair_spans() {
        let original = Node::new(
            Value::Mapping(vec![
                MappingPair::new(
                    yaml_parser::Span::new(1..5),
                    Node::new(Value::Null, yaml_parser::Span::new(1..1)),
                    string_node("skipped"),
                ),
                MappingPair::new(
                    yaml_parser::Span::new(10..30),
                    Node::new(
                        Value::String(Cow::Borrowed("foo")),
                        yaml_parser::Span::new(10..13),
                    ),
                    Node::new(Value::Null, yaml_parser::Span::new(15..20)),
                ),
            ]),
            yaml_parser::Span::new(0..31),
        );
        let coerced = original.coerce_mapping(vec![MappingPair::new(
            yaml_parser::Span::new(10..30),
            Node::new(
                Value::String(Cow::Owned("foo".to_owned())),
                yaml_parser::Span::new(10..13),
            ),
            Node::new(
                Value::String(Cow::Owned("bar".to_owned())),
                yaml_parser::Span::new(14..20),
            ),
        )]);

        let Value::Mapping(pairs) = coerced.value else {
            panic!("coerce_mapping should create a mapping");
        };
        let pair = pairs.first().expect("coerce_mapping should emit one pair");

        assert_eq!(pair.key.span, yaml_parser::Span::new(10..13));
        assert_eq!(pair.value.span, yaml_parser::Span::new(14..20));
        assert_eq!(pair.pair_span, yaml_parser::Span::new(10..30));
    }
}
