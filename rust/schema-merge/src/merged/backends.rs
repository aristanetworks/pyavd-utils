// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use serde::Serializer;
use serde_json::Value;
use validation::ValidatableValue as _;
use validation::feedback::SourceSpan;
use yaml_parser::Integer;
use yaml_parser::MappingPair;
use yaml_parser::Node;
use yaml_parser::SequenceItem;
use yaml_parser::Value as YamlValue;

use super::MergeBackend;
use crate::SemanticValue;

impl MergeBackend for Value {
    type Key = str;
    type Element = Self;
    type SerializedKey = str;
    type Entries<'a> = std::iter::Map<
        std::iter::Flatten<std::option::IntoIter<&'a serde_json::Map<String, Self>>>,
        fn((&'a String, &'a Self)) -> (&'a str, &'a Self),
    >;
    type Elements<'a> =
        std::iter::Map<std::slice::Iter<'a, Self>, fn(&'a Self) -> (&'a Self, &'a Self)>;

    fn entries(&self) -> Self::Entries<'_> {
        self.as_object()
            .into_iter()
            .flatten()
            .map(|(key, value)| (key.as_str(), value))
    }
    fn elements(&self) -> Self::Elements<'_> {
        self.as_array()
            .map_or(&[][..], Vec::as_slice)
            .iter()
            .map(|value| (value, value))
    }
    fn key_name(key: &str) -> Option<&str> {
        Some(key)
    }
    fn display_key(key: &str) -> Cow<'_, str> {
        Cow::Borrowed(key)
    }
    fn key_identity(key: &str) -> SemanticValue {
        SemanticValue::string(key)
    }
    fn serialized_key(key: &str) -> &str {
        key
    }
    fn key_source(_key: &str) -> Option<&str> {
        None
    }
    fn key_span(_key: &str) -> Option<SourceSpan> {
        None
    }
    fn make_null(&self) -> Self {
        Self::Null
    }
    fn make_bool(&self, value: bool) -> Self {
        Self::Bool(value)
    }
    fn make_int(&self, value: i64) -> Self {
        Self::Number(value.into())
    }
    fn make_str(&self, value: String) -> Self {
        Self::String(value)
    }
    fn empty_mapping(&self) -> Self {
        Self::Object(serde_json::Map::new())
    }
    fn empty_sequence(&self) -> Self {
        Self::Array(Vec::new())
    }
    fn serialize_value<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serde::Serialize::serialize(self, serializer)
    }
}

impl<'input> MergeBackend for Node<'input> {
    type Key = MappingPair<'input>;
    type Element = SequenceItem<'input>;
    type SerializedKey = YamlValue<'input>;
    type Entries<'a>
        = std::iter::Map<
        std::slice::Iter<'a, MappingPair<'input>>,
        fn(&'a MappingPair<'input>) -> (&'a MappingPair<'input>, &'a Self),
    >
    where
        Self: 'a;
    type Elements<'a>
        = std::iter::Map<
        std::slice::Iter<'a, SequenceItem<'input>>,
        fn(&'a SequenceItem<'input>) -> (&'a SequenceItem<'input>, &'a Self),
    >
    where
        Self: 'a;

    fn entries(&self) -> Self::Entries<'_> {
        let pairs = match &self.value {
            YamlValue::Mapping(pairs) => pairs.as_slice(),
            _ => &[],
        };
        pairs.iter().map(|pair| (pair, &pair.value))
    }
    fn elements(&self) -> Self::Elements<'_> {
        let items = match &self.value {
            YamlValue::Sequence(items) => items.as_slice(),
            _ => &[],
        };
        items.iter().map(|item| (item, &item.node))
    }
    fn key_name(key: &Self::Key) -> Option<&str> {
        match &key.key.value {
            YamlValue::String(value) => Some(value),
            _ => None,
        }
    }
    fn display_key(key: &Self::Key) -> Cow<'_, str> {
        match &key.key.value {
            YamlValue::String(value) => Cow::Borrowed(value),
            YamlValue::Int(value) => value.to_decimal_string(),
            YamlValue::Float(value) => Cow::Owned(value.to_string()),
            YamlValue::Bool(value) => Cow::Borrowed(if *value { "true" } else { "false" }),
            YamlValue::Null => Cow::Borrowed("null"),
            YamlValue::Mapping(_) | YamlValue::Sequence(_) => Cow::Borrowed("<complex key>"),
        }
    }
    fn feedback_key(key: &Self::Key) -> Option<Cow<'_, str>> {
        match key.key.value {
            YamlValue::Null | YamlValue::Mapping(_) | YamlValue::Sequence(_) => None,
            _ => Some(Self::display_key(key)),
        }
    }
    fn key_identity(key: &Self::Key) -> SemanticValue {
        (&key.key).into()
    }
    fn serialized_key(key: &Self::Key) -> &Self::SerializedKey {
        &key.key.value
    }
    fn key_source(key: &Self::Key) -> Option<&str> {
        key.key.source.as_deref()
    }
    fn key_span(key: &Self::Key) -> Option<SourceSpan> {
        key.key.source_span()
    }
    fn make_null(&self) -> Self {
        Node::new(YamlValue::Null, self.span, self.source.clone())
    }
    fn make_bool(&self, value: bool) -> Self {
        Node::new(YamlValue::Bool(value), self.span, self.source.clone())
    }
    fn make_int(&self, value: i64) -> Self {
        Node::new(
            YamlValue::Int(Integer::I64(value)),
            self.span,
            self.source.clone(),
        )
    }
    fn make_str(&self, value: String) -> Self {
        Node::new(
            YamlValue::String(Cow::Owned(value)),
            self.span,
            self.source.clone(),
        )
    }
    fn empty_mapping(&self) -> Self {
        Node::new(
            YamlValue::Mapping(Vec::new()),
            self.span,
            self.source.clone(),
        )
    }
    fn empty_sequence(&self) -> Self {
        Node::new(
            YamlValue::Sequence(Vec::new()),
            self.span,
            self.source.clone(),
        )
    }
    fn serialize_value<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serde::Serialize::serialize(&self.value, serializer)
    }
}
