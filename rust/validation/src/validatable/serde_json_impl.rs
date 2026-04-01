// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Implementation of ValidatableValue traits for serde_json types.

use std::borrow::Cow;

use serde_json::{Map, Value};

use super::{ValidatableMapping, ValidatableMappingPair, ValidatableSequence, ValidatableValue};

// === ValidatableValue for serde_json::Value ===

impl ValidatableValue for Value {
    type Mapping<'a> = &'a Map<String, Value>;
    type Sequence<'a> = &'a Vec<Value>;
    type Coerced = Value;

    fn is_null(&self) -> bool {
        self.is_null()
    }

    fn is_str(&self) -> bool {
        self.is_string()
    }

    fn is_int(&self) -> bool {
        self.is_i64() || self.is_u64()
    }

    fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    fn as_str(&self) -> Option<Cow<'_, str>> {
        match self {
            Value::String(s) => Some(Cow::Borrowed(s.as_str())),
            Value::Number(n) => Some(Cow::Owned(n.to_string())),
            // Using Title case to match Python behavior
            Value::Bool(b) => Some(Cow::Borrowed(if *b { "True" } else { "False" })),
            _ => None,
        }
    }

    fn as_i64(&self) -> Option<i64> {
        match self {
            Value::Number(n) => n.as_i64().or_else(|| {
                n.as_f64()
                    .filter(|float| float.is_finite() && float.fract() == 0.0)
                    .filter(|float| *float >= i64::MIN as f64 && *float <= i64::MAX as f64)
                    .map(|float| float as i64)
            }),
            Value::String(s) => s.parse().ok(),
            Value::Bool(b) => Some(if *b { 1 } else { 0 }),
            _ => None,
        }
    }

    fn as_bool(&self) -> Option<bool> {
        self.as_bool()
    }

    fn as_mapping(&self) -> Option<Self::Mapping<'_>> {
        self.as_object()
    }

    fn as_sequence(&self) -> Option<Self::Sequence<'_>> {
        self.as_array()
    }

    fn get(&self, key: &str) -> Option<&Self> {
        self.get(key)
    }

    fn value_type(&self) -> crate::feedback::Type {
        self.into()
    }

    // === Coercion builders ===

    fn coerce_null(&self) -> Self::Coerced {
        Value::Null
    }

    fn coerce_bool(&self, value: bool) -> Self::Coerced {
        Value::Bool(value)
    }

    fn coerce_int(&self, value: i64) -> Self::Coerced {
        Value::Number(value.into())
    }

    fn coerce_str(&self, value: String) -> Self::Coerced {
        Value::String(value)
    }

    fn coerce_sequence(&self, items: Vec<Self::Coerced>) -> Self::Coerced {
        Value::Array(items)
    }

    fn coerce_mapping(&self, items: Vec<(String, Self::Coerced)>) -> Self::Coerced {
        Value::Object(items.into_iter().collect())
    }

    fn clone_to_coerced(&self) -> Self::Coerced {
        self.clone()
    }

    fn to_feedback_value(&self) -> crate::feedback::Value {
        // feedback::Value has From<serde_json::Value>
        self.clone().into()
    }

    fn is_float(&self) -> bool {
        matches!(self, Value::Number(n) if n.is_f64())
    }
}

// === ValidatableMapping for serde_json::Map ===

impl<'a> ValidatableMapping<'a> for &'a Map<String, Value> {
    type Value = Value;
    type Pair = MapPair<'a>;
    type Iter = MapIter<'a>;

    fn get(&self, key: &str) -> Option<&Self::Value> {
        Map::get(self, key)
    }

    fn contains_key(&self, key: &str) -> bool {
        Map::contains_key(self, key)
    }

    fn iter(&self) -> Self::Iter {
        MapIter {
            inner: Map::iter(self),
        }
    }

    fn len(&self) -> usize {
        Map::len(self)
    }
}

/// Iterator adapter for serde_json Map.
pub struct MapIter<'a> {
    inner: serde_json::map::Iter<'a>,
}

impl<'a> Iterator for MapIter<'a> {
    type Item = MapPair<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(key, value)| MapPair { key, value })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl ExactSizeIterator for MapIter<'_> {}

pub struct MapPair<'a> {
    key: &'a String,
    value: &'a Value,
}

impl<'a> ValidatableMappingPair<'a> for MapPair<'a> {
    type Value = Value;

    fn key(&self) -> Cow<'a, str> {
        Cow::Borrowed(self.key.as_str())
    }

    fn value(&self) -> &'a Self::Value {
        self.value
    }
}

// === ValidatableSequence for Vec<Value> ===

impl<'a> ValidatableSequence<'a> for &'a Vec<Value> {
    type Value = Value;
    type Iter = std::slice::Iter<'a, Value>;

    fn iter(&self) -> Self::Iter {
        <[Value]>::iter(self)
    }

    fn len(&self) -> usize {
        Vec::len(self)
    }
}
