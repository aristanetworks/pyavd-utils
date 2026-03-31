// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Implementation of ValidatableValue traits for yaml_parser types.

use std::borrow::Cow;

use yaml_parser::{Integer, MappingPair, Node, SequenceItem, Value};

use super::{ValidatableMapping, ValidatableMappingPair, ValidatableSequence, ValidatableValue};

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
            Value::String(_) | Value::Alias(_) => Type::Str,
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
                let key_node = Node::new(Value::String(Cow::Owned(key)), value.span);
                MappingPair::new(value.span, key_node, value)
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
            // Alias shouldn't appear in validated data
            Value::Alias(_) => FV::Null(),
        }
    }

    fn is_float(&self) -> bool {
        matches!(self.value, Value::Float(_))
    }

    fn source_span(&self) -> Option<crate::feedback::SourceSpan> {
        Some(self.span.into())
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
        Value::Alias(alias) => Some(Cow::Borrowed(alias.as_ref())),
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
    type Pair = NodeMappingPair<'a, 'input>;
    type Iter = NodeMappingIter<'a, 'input>;

    fn get(&self, key: &str) -> Option<&Self::Value> {
        for pair in self.pairs {
            if let Some(k_str) = coerce_key_to_string(&pair.key)
                && k_str == key
            {
                return Some(&pair.value);
            }
        }
        None
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
        Some(self.pair.key.span.into())
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
