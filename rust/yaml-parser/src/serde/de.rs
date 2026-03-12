// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::collections::HashMap;
use std::fmt;
use std::io::Read;

use serde::de::{self, DeserializeOwned, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use serde::forward_to_deserialize_any;

use crate::{Node, ParseError, Value, parse};

/// Error type for serde-based deserialization from yaml-parser.
#[derive(Debug)]
pub enum DeError {
    /// YAML was syntactically invalid.
    Parse(ParseError),
    /// No document found in the input.
    NoDocument,
    /// More than one document was found where a single document was expected.
    MultipleDocuments,
    /// Generic serde error.
    Message(String),
}

impl fmt::Display for DeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(err) => write!(f, "YAML parse error: {err}"),
            Self::NoDocument => write!(f, "expected at least one YAML document"),
            Self::MultipleDocuments => write!(f, "expected a single YAML document"),
            Self::Message(msg) => f.write_str(msg),
        }
    }
}

impl std::error::Error for DeError {}

impl de::Error for DeError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Self::Message(msg.to_string())
    }
}

impl From<ParseError> for DeError {
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}

/// Mapping from anchor names to the nodes they refer to.
type AnchorIndex<'de> = HashMap<&'de str, &'de Node<'de>>;

fn collect_anchors<'de>(node: &'de Node<'de>, anchors: &mut AnchorIndex<'de>) {
    if let Some(name) = node.anchor() {
        anchors.insert(name, node);
    }
    match &node.value {
        Value::Sequence(items) => {
            for item in items {
                collect_anchors(item, anchors);
            }
        }
        Value::Mapping(pairs) => {
            for (key_node, value_node) in pairs {
                collect_anchors(key_node, anchors);
                collect_anchors(value_node, anchors);
            }
        }
        _ => {}
    }
}

struct ValueDeserializer<'a, 'de> {
    value: &'de Value<'de>,
    anchors: &'a AnchorIndex<'de>,
}

impl<'a, 'de> ValueDeserializer<'a, 'de> {
    fn new(value: &'de Value<'de>, anchors: &'a AnchorIndex<'de>) -> Self {
        Self { value, anchors }
    }
}

struct SeqAccessImpl<'a, 'de> {
    iter: std::slice::Iter<'de, Node<'de>>,
    anchors: &'a AnchorIndex<'de>,
}

impl<'a, 'de> SeqAccess<'de> for SeqAccessImpl<'a, 'de> {
    type Error = DeError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, DeError>
    where
        T: DeserializeSeed<'de>,
    {
        if let Some(node) = self.iter.next() {
            let de = ValueDeserializer::new(&node.value, self.anchors);
            seed.deserialize(de).map(Some)
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.iter.len())
    }
}

struct MapAccessImpl<'a, 'de> {
    iter: std::slice::Iter<'de, (Node<'de>, Node<'de>)>,
    next_value: Option<&'de Node<'de>>,
    anchors: &'a AnchorIndex<'de>,
}

impl<'a, 'de> MapAccessImpl<'a, 'de> {
    fn new(
        iter: std::slice::Iter<'de, (Node<'de>, Node<'de>)>,
        anchors: &'a AnchorIndex<'de>,
    ) -> Self {
        Self {
            iter,
            next_value: None,
            anchors,
        }
    }
}

impl<'a, 'de> MapAccess<'de> for MapAccessImpl<'a, 'de> {
    type Error = DeError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, DeError>
    where
        K: DeserializeSeed<'de>,
    {
        if let Some((key_node, value_node)) = self.iter.next() {
            self.next_value = Some(value_node);
            let de = ValueDeserializer::new(&key_node.value, self.anchors);
            seed.deserialize(de).map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, DeError>
    where
        V: DeserializeSeed<'de>,
    {
        let node = self
            .next_value
            .take()
            .ok_or_else(|| DeError::Message("value without corresponding key".to_owned()))?;
        let de = ValueDeserializer::new(&node.value, self.anchors);
        seed.deserialize(de)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.iter.len())
    }
}

impl<'de> de::Deserializer<'de> for ValueDeserializer<'_, 'de> {
    type Error = DeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        use std::borrow::Cow;

        match self.value {
            Value::Null => visitor.visit_unit(),
            Value::Bool(bool_value) => visitor.visit_bool(*bool_value),
            Value::Int(int_value) => visitor.visit_i64(*int_value),
            Value::Float(float_value) => visitor.visit_f64(*float_value),
            Value::String(string_value) => match string_value {
                Cow::Borrowed(borrowed) => visitor.visit_borrowed_str(borrowed),
                Cow::Owned(owned) => visitor.visit_string(owned.clone()),
            },
            Value::Sequence(items) => {
                let seq = SeqAccessImpl {
                    iter: items.iter(),
                    anchors: self.anchors,
                };
                visitor.visit_seq(seq)
            }
            Value::Mapping(pairs) => {
                let map = MapAccessImpl::new(pairs.iter(), self.anchors);
                visitor.visit_map(map)
            }
            Value::Alias(name) => {
                if let Some(target) = self.anchors.get(name.as_ref()) {
                    ValueDeserializer::new(&target.value, self.anchors).deserialize_any(visitor)
                } else {
                    Err(DeError::Message("unknown alias".to_owned()))
                }
            }
            Value::Invalid => Err(DeError::Message("invalid YAML node".to_owned())),
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Null => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_enum<V>(
        self,
        _name: &str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        use serde::de::value::BorrowedStrDeserializer;

        match self.value {
            Value::String(string_value) => {
                let de = BorrowedStrDeserializer::new(string_value.as_ref());
                visitor.visit_enum(de)
            }
            _ => Err(DeError::Message("expected string for enum".to_owned())),
        }
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 i128 u128
        f32 f64 char str string bytes byte_buf unit unit_struct
        newtype_struct seq tuple tuple_struct map struct
        identifier ignored_any
    }
}

/// Deserialize a single YAML document from a string into `T`.
pub fn from_str<T>(input: &str) -> Result<T, DeError>
where
    T: DeserializeOwned,
{
    let (docs, errors) = parse(input);
    if let Some(err) = errors.into_iter().next() {
        return Err(DeError::from(err));
    }
    let mut iter = docs.into_iter();
    let first = iter.next().ok_or(DeError::NoDocument)?;
    if iter.next().is_some() {
        return Err(DeError::MultipleDocuments);
    }

    let mut anchors = AnchorIndex::new();
    collect_anchors(&first, &mut anchors);

    let de = ValueDeserializer::new(&first.value, &anchors);
    T::deserialize(de)
}

/// Deserialize a single YAML document from a reader into `T`.
pub fn from_reader<R, T>(mut reader: R) -> Result<T, DeError>
where
    R: Read,
    T: DeserializeOwned,
{
    let mut buf = String::new();
    reader
        .read_to_string(&mut buf)
        .map_err(|err| DeError::Message(err.to_string()))?;
    from_str(&buf)
}
