// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Serde-based YAML serialization for `yaml-parser`.
//!
//! This module builds on the AST (`Node` / `Value`) and the event-based
//! YAML writer to provide a `serde::Serializer` implementation and
//! convenience helpers (`to_writer`, `to_string`).
//!
//! The current implementation focuses on the common configuration-shaped
//! data seen in this repository. Some edge cases (very large integers,
//! non-finite floats, custom enum representations) are reported as
//! `SerError` variants rather than being fully supported.

use std::fmt;
use std::io::{self, Write};

use serde::ser::{
    self, Serialize, SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant,
    SerializeTuple, SerializeTupleStruct, SerializeTupleVariant, Serializer,
};

use crate::ast_events::{self, AstToEventsError};
use crate::value::{Integer, MappingPair, SequenceItem};
use crate::{Node, Value, writer};

/// Error type for serde-based *serialization* using yaml-parser.
#[derive(Debug, derive_more::Display)]
pub enum SerError {
    /// I/O error while writing YAML output.
    #[display("I/O error while writing YAML: {}", _0)]
    Io(io::Error),

    /// Non-finite floating-point value (`NaN` or `±inf`) is not yet supported.
    #[display("unsupported floating-point value {}", _0)]
    UnsupportedFloat(f64),

    /// A mapping contained a value without a corresponding key.
    #[display("value without corresponding key in mapping")]
    ValueWithoutKey,

    /// Generic serde error created via `serde::ser::Error::custom`.
    #[display("serde custom error: {}", _0)]
    Custom(String),
}

impl std::error::Error for SerError {}

impl ser::Error for SerError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Self::Custom(msg.to_string())
    }
}

impl From<io::Error> for SerError {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<AstToEventsError> for SerError {
    fn from(err: AstToEventsError) -> Self {
        match err {
            AstToEventsError::UnsupportedFloat(float_value) => {
                SerError::UnsupportedFloat(float_value)
            }
        }
    }
}

/// Serializer that builds a `Value<'static>` from any `T: Serialize`.
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct ValueSerializer;

impl ValueSerializer {
    /// Convenience helper to turn any `T: Serialize` into a `Value<'static>`.
    pub fn to_value<T>(value: &T) -> Result<Value<'static>, SerError>
    where
        T: Serialize,
    {
        value.serialize(Self)
    }
}

impl Serializer for ValueSerializer {
    type Ok = Value<'static>;
    type Error = SerError;

    type SerializeSeq = SeqSerializer;
    type SerializeTuple = SeqSerializer;
    type SerializeTupleStruct = SeqSerializer;
    type SerializeTupleVariant = TupleVariantSerializer;
    type SerializeMap = MapSerializer;
    type SerializeStruct = StructSerializer;
    type SerializeStructVariant = StructVariantSerializer;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(Integer::I64(i64::from(v))))
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(Integer::I64(i64::from(v))))
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(Integer::I64(i64::from(v))))
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(Integer::I64(v)))
    }

    fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(Integer::I128(v)))
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(Integer::U64(u64::from(v))))
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(Integer::U64(u64::from(v))))
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(Integer::U64(u64::from(v))))
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(Integer::U64(v)))
    }

    fn serialize_u128(self, v: u128) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(Integer::U128(v)))
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_f64(f64::from(v))
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        if !v.is_finite() {
            return Err(SerError::UnsupportedFloat(v));
        }
        Ok(Value::Float(v))
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        Ok(Value::String(std::borrow::Cow::Owned(v.to_string())))
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        Ok(Value::String(std::borrow::Cow::Owned(v.to_owned())))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        let mut items = Vec::with_capacity(v.len());
        for &byte in v {
            let value = Value::Int(Integer::U64(u64::from(byte)));
            let node = node_from_value(value);
            items.push(SequenceItem::new(node.span, node));
        }
        Ok(Value::Sequence(items))
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Null)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize + ?Sized,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Null)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Null)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(Value::String(std::borrow::Cow::Owned(variant.to_owned())))
    }

    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize + ?Sized,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: Serialize + ?Sized,
    {
        let inner = value.serialize(ValueSerializer)?;
        let key = node_from_value(Value::String(std::borrow::Cow::Owned(variant.to_owned())));
        let val = node_from_value(inner);
        Ok(Value::Mapping(vec![MappingPair::new(val.span, key, val)]))
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(SeqSerializer {
            elements: Vec::with_capacity(len.unwrap_or(0)),
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Ok(TupleVariantSerializer {
            name: variant.to_owned(),
            elements: Vec::with_capacity(len),
        })
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(MapSerializer {
            entries: Vec::with_capacity(len.unwrap_or(0)),
            next_key: None,
        })
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(StructSerializer {
            fields: Vec::with_capacity(len),
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Ok(StructVariantSerializer {
            name: variant.to_owned(),
            fields: Vec::with_capacity(len),
        })
    }

    fn collect_str<T: fmt::Display + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(&value.to_string())
    }
}

pub(crate) struct SeqSerializer {
    elements: Vec<SequenceItem<'static>>,
}

impl SerializeSeq for SeqSerializer {
    type Ok = Value<'static>;
    type Error = SerError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize + ?Sized,
    {
        let element_value = value.serialize(ValueSerializer)?;
        let node = node_from_value(element_value);
        self.elements.push(SequenceItem::new(node.span, node));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Sequence(self.elements))
    }
}

impl SerializeTuple for SeqSerializer {
    type Ok = Value<'static>;
    type Error = SerError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize + ?Sized,
    {
        SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        SerializeSeq::end(self)
    }
}

impl SerializeTupleStruct for SeqSerializer {
    type Ok = Value<'static>;
    type Error = SerError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize + ?Sized,
    {
        SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        SerializeSeq::end(self)
    }
}

pub(crate) struct TupleVariantSerializer {
    name: String,
    elements: Vec<SequenceItem<'static>>,
}

impl SerializeTupleVariant for TupleVariantSerializer {
    type Ok = Value<'static>;
    type Error = SerError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize + ?Sized,
    {
        let field_value = value.serialize(ValueSerializer)?;
        let node = node_from_value(field_value);
        self.elements.push(SequenceItem::new(node.span, node));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let seq_value = Value::Sequence(self.elements);
        let key = node_from_value(Value::String(std::borrow::Cow::Owned(self.name)));
        let val = node_from_value(seq_value);
        Ok(Value::Mapping(vec![MappingPair::new(val.span, key, val)]))
    }
}

pub(crate) struct MapSerializer {
    entries: Vec<MappingPair<'static>>,
    next_key: Option<Value<'static>>,
}

impl SerializeMap for MapSerializer {
    type Ok = Value<'static>;
    type Error = SerError;

    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: Serialize + ?Sized,
    {
        let key_value = key.serialize(ValueSerializer)?;
        self.next_key = Some(key_value);
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize + ?Sized,
    {
        let key_value = self.next_key.take().ok_or(SerError::ValueWithoutKey)?;
        let key_node = node_from_value(key_value);
        let val_node = node_from_value(value.serialize(ValueSerializer)?);
        self.entries
            .push(MappingPair::new(val_node.span, key_node, val_node));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Mapping(self.entries))
    }
}

pub(crate) struct StructSerializer {
    fields: Vec<MappingPair<'static>>,
}

impl SerializeStruct for StructSerializer {
    type Ok = Value<'static>;
    type Error = SerError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize + ?Sized,
    {
        let key_node = node_from_value(Value::String(std::borrow::Cow::Owned(key.to_owned())));
        let val_node = node_from_value(value.serialize(ValueSerializer)?);
        self.fields
            .push(MappingPair::new(val_node.span, key_node, val_node));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Mapping(self.fields))
    }
}

pub(crate) struct StructVariantSerializer {
    name: String,
    fields: Vec<MappingPair<'static>>,
}

impl SerializeStructVariant for StructVariantSerializer {
    type Ok = Value<'static>;
    type Error = SerError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize + ?Sized,
    {
        let key_node = node_from_value(Value::String(std::borrow::Cow::Owned(key.to_owned())));
        let val_node = node_from_value(value.serialize(ValueSerializer)?);
        self.fields
            .push(MappingPair::new(val_node.span, key_node, val_node));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let inner = Value::Mapping(self.fields);
        let key = node_from_value(Value::String(std::borrow::Cow::Owned(self.name)));
        let val = node_from_value(inner);
        Ok(Value::Mapping(vec![MappingPair::new(val.span, key, val)]))
    }
}

/// Internal helper: build a `Node<'static>` from a `Value<'static>` using a
/// dummy span and no properties.
///
/// This is sufficient for serialization, where we no longer have access to
/// original source locations.
fn node_from_value(value: Value<'static>) -> Node<'static> {
    Node {
        properties: None,
        value,
        span: crate::span::Span::default(),
        trailing_comment: None,
    }
}

/// Serialize any `T: Serialize` directly to a writer as YAML text.
pub fn to_writer<W, T>(mut writer: W, value: &T) -> Result<(), SerError>
where
    W: Write,
    T: Serialize,
{
    let root_value = ValueSerializer::to_value(value)?;
    let node = node_from_value(root_value);
    let events = ast_events::node_to_events(&node)?;
    writer::write_yaml_from_events(&mut writer, &events)?;
    Ok(())
}

/// Serialize any `T: Serialize` to a YAML string.
pub fn to_string<T>(value: &T) -> Result<String, SerError>
where
    T: Serialize,
{
    let mut buf = Vec::new();
    to_writer(&mut buf, value)?;
    String::from_utf8(buf)
        .map_err(|err| SerError::Io(io::Error::new(io::ErrorKind::InvalidData, err)))
}
#[cfg(test)]
mod tests {
    #![allow(
        clippy::expect_used,
        reason = "tests use expect with explicit messages for clearer diagnostics"
    )]
    use super::*;
    use crate::{
        MappingPair, Node, SequenceItem,
        span::Span,
        value::{Comment, Value},
    };
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct SimpleConfig {
        name: String,
        enabled: bool,
        count: i64,
    }

    #[test]
    fn to_string_roundtrips_via_serde() {
        let value = SimpleConfig {
            name: "example".to_owned(),
            enabled: true,
            count: 42,
        };

        let yaml = to_string(&value).expect("serialization to YAML should succeed");

        let decoded: SimpleConfig = crate::serde::from_str(&yaml)
            .expect("deserialization from YAML via yaml-parser::serde should succeed");
        assert_eq!(decoded, value);
    }

    #[test]
    fn ast_to_events_writer_roundtrip_preserves_values() {
        let input = "root:\n  answer: 42\n  list:\n    - a\n    - b\n";
        let (docs_before, errors_before) = crate::parse(input);
        assert!(
            errors_before.is_empty(),
            "expected no parse errors before roundtrip, got: {errors_before:?}",
        );
        assert_eq!(docs_before.len(), 1, "expected a single document");
        let doc = docs_before
            .first()
            .expect("expected exactly one document before roundtrip");

        let events = crate::ast_events::node_to_events(doc)
            .expect("node_to_events should succeed for valid AST");
        let mut buf = Vec::new();
        writer::write_yaml_from_events(&mut buf, &events)
            .expect("writing YAML from AST-derived events should succeed");
        let output = String::from_utf8(buf).expect("writer must produce valid UTF-8");

        let (docs_after, errors_after) = crate::parse(&output);
        assert!(
            errors_after.is_empty(),
            "expected no parse errors after roundtrip, got: {errors_after:?}\nOUTPUT:\n{output}",
        );
        assert_eq!(
            docs_after.len(),
            1,
            "document count changed after roundtrip"
        );
        let roundtripped_doc = docs_after
            .first()
            .expect("expected exactly one document after roundtrip");
        assert_eq!(
            normalize_value(&roundtripped_doc.value),
            normalize_value(&doc.value),
            "AST value changed after roundtrip"
        );
    }

    fn normalize_value(value: &Value<'_>) -> Value<'static> {
        match value {
            Value::Null => Value::Null,
            Value::Bool(boolean) => Value::Bool(*boolean),
            Value::Int(integer) => Value::Int(integer.clone().into_owned()),
            Value::Float(float) => Value::Float(*float),
            Value::String(string) => Value::String(std::borrow::Cow::Owned(string.to_string())),
            Value::Sequence(items) => Value::Sequence(
                items
                    .iter()
                    .map(|item| SequenceItem {
                        item_span: Span::default(),
                        node: normalize_node(&item.node),
                        header_comment: normalize_comment(item.header_comment.as_deref()),
                    })
                    .collect(),
            ),
            Value::Mapping(pairs) => Value::Mapping(
                pairs
                    .iter()
                    .map(|pair| MappingPair {
                        pair_span: Span::default(),
                        key: normalize_node(&pair.key),
                        value: normalize_node(&pair.value),
                        header_comment: normalize_comment(pair.header_comment.as_deref()),
                    })
                    .collect(),
            ),
            Value::Alias(alias) => Value::Alias(std::borrow::Cow::Owned(alias.to_string())),
        }
    }

    fn normalize_node(node: &Node<'_>) -> Node<'static> {
        Node {
            properties: None,
            value: normalize_value(&node.value),
            span: Span::default(),
            trailing_comment: normalize_comment(node.trailing_comment.as_deref()),
        }
    }

    fn normalize_comment(comment: Option<&Comment<'_>>) -> Option<Box<Comment<'static>>> {
        comment.map(|input_comment| {
            Box::new(Comment {
                text: std::borrow::Cow::Owned(input_comment.text.to_string()),
                span: Span::default(),
            })
        })
    }
}
