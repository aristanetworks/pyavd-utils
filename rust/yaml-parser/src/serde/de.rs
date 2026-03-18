// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::collections::HashMap;
use std::fmt;
use std::io::Read;
use std::marker::PhantomData;

use serde::de::{self, DeserializeOwned, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use serde::forward_to_deserialize_any;

use crate::emitter::Emitter;
use crate::parser::Parser;
use crate::value::Number;
use crate::{Node, ParseError, Value};

/// Error type for serde-based deserialization from yaml-parser.
#[derive(Debug, derive_more::Display)]
pub enum DeError {
    /// YAML was syntactically invalid.
    #[display("YAML parse error: {}", _0)]
    Parse(ParseError),

    /// No document found in the input.
    #[display("expected at least one YAML document")]
    NoDocument,

    /// More than one document was found where a single document was expected.
    #[display("expected a single YAML document")]
    MultipleDocuments,

    /// A mapping contained a value without a corresponding key.
    #[display("value without corresponding key in mapping")]
    ValueWithoutKey,

    /// An alias referenced an unknown anchor name.
    #[display("unknown YAML alias '{}'", _0)]
    UnknownAlias(String),

    /// Enum deserialization expected a string representation.
    #[display("expected string for enum")]
    ExpectedEnumString,

    /// I/O error while reading YAML input.
    #[display("I/O error while reading YAML: {}", _0)]
    Io(std::io::Error),

    /// Generic serde error created via `serde::de::Error::custom`.
    #[display("serde custom error: {}", _0)]
    Custom(String),
}

impl std::error::Error for DeError {}

impl de::Error for DeError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Self::Custom(msg.to_string())
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

impl<'de> SeqAccess<'de> for SeqAccessImpl<'_, 'de> {
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

impl<'de> MapAccess<'de> for MapAccessImpl<'_, 'de> {
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
        let node = self.next_value.take().ok_or(DeError::ValueWithoutKey)?;
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
            Value::Int(number) => match number {
                Number::I64(i64_value) => visitor.visit_i64(*i64_value),
                Number::U64(u64_value) => visitor.visit_u64(*u64_value),
                Number::I128(i128_value) => visitor.visit_i128(*i128_value),
                Number::U128(u128_value) => visitor.visit_u128(*u128_value),
                Number::BigIntStr(text) => match text {
                    Cow::Borrowed(borrowed) => visitor.visit_borrowed_str(borrowed),
                    Cow::Owned(owned) => visitor.visit_borrowed_str(owned),
                },
            },
            Value::Float(float_value) => visitor.visit_f64(*float_value),
            Value::String(string_value) => match string_value {
                Cow::Borrowed(borrowed) => visitor.visit_borrowed_str(borrowed),
                Cow::Owned(owned) => visitor.visit_borrowed_str(owned),
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
                    Err(DeError::UnknownAlias(name.as_ref().to_owned()))
                }
            }
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
            _ => Err(DeError::ExpectedEnumString),
        }
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 i128 u128
        f32 f64 char str string bytes byte_buf unit unit_struct
        newtype_struct seq tuple tuple_struct map struct
        identifier ignored_any
    }
}

	/// Streaming deserializer over multiple YAML documents in a string.
	///
	/// This implementation drives the streaming emitter+parser pipeline directly
	/// and does **not** require building a `Vec<Node>` for all documents up-front.
	/// Each call to [`Iterator::next`] parses at most one additional document,
	/// converts it to an owned `Node<'static>`, and then deserializes it into `T`.
	pub struct StreamDeserializer<'de, T> {
	    emitter: Emitter<'de>,
	    finished: bool,
	    _marker: PhantomData<T>,
	}
	
	impl<'de, T> StreamDeserializer<'de, T> {
	    fn new(input: &'de str) -> Self {
	        Self {
	            emitter: Emitter::new(input),
	            finished: false,
	            _marker: PhantomData,
	        }
	    }
	}
	
	impl<'de, T> Iterator for StreamDeserializer<'de, T>
	where
	    T: DeserializeOwned,
	{
	    type Item = Result<T, DeError>;

	    fn next(&mut self) -> Option<Self::Item> {
	        if self.finished {
	            return None;
	        }

	        // Construct a parser starting at the current emitter position and
	        // parse the next document, if any.
	        let mut parser = Parser::new(&mut self.emitter);
	        let node_opt = parser.parse_next_document();

	        // Collect parse and emitter errors that occurred while parsing this
	        // document. If any are present, surface the first as a `DeError` and
	        // stop the stream.
	        let mut errors = parser.take_errors();
	        errors.extend(self.emitter.take_errors());
	        if let Some(err) = errors.into_iter().next() {
	            self.finished = true;
	            return Some(Err(DeError::from(err)));
	        }

	        let node = match node_opt {
	            Some(node) => node,
	            None => {
	                // No more documents.
	                self.finished = true;
	                return None;
	            }
	        };

	        // For serde we still reuse the existing AST-backed deserializer by
	        // converting the document root into an owned form and collecting
	        // anchors per document.
	        let node_owned = node.into_owned();
	        let mut anchors = AnchorIndex::new();
	        collect_anchors(&node_owned, &mut anchors);
	        let de = ValueDeserializer::new(&node_owned.value, &anchors);
	        Some(T::deserialize(de))
	    }
	}

		/// Deserialize a single YAML document from a string into `T`.
		pub fn from_str<T>(input: &str) -> Result<T, DeError>
		where
		    T: DeserializeOwned,
		{
		    let mut emitter = Emitter::new(input);
		    let mut parser = Parser::new(&mut emitter);

	    // Parse the first document, if any.
	    let first = parser.parse_next_document();

	    // Parse and discard any remaining documents so we can both detect extra
	    // documents and collect all parse/emitter errors for the full stream.
	    let mut has_extra_doc = false;
	    while let Some(_doc) = parser.parse_next_document() {
	        has_extra_doc = true;
	    }

	    // Collect and aggregate parse + emitter errors.
	    let mut errors = parser.take_errors();
	    errors.extend(emitter.take_errors());
	    if let Some(err) = errors.into_iter().next() {
	        return Err(DeError::from(err));
	    }

	    let first = first.ok_or(DeError::NoDocument)?;
	    if has_extra_doc {
	        return Err(DeError::MultipleDocuments);
	    }

	    let first_owned = first.into_owned();
	    let mut anchors = AnchorIndex::new();
	    collect_anchors(&first_owned, &mut anchors);
	    let de = ValueDeserializer::new(&first_owned.value, &anchors);
	    T::deserialize(de)
	}

/// Deserialize a single YAML document from a reader into `T`.
pub fn from_reader<R, T>(mut reader: R) -> Result<T, DeError>
where
    R: Read,
    T: DeserializeOwned,
{
    let mut buf = String::new();
    reader.read_to_string(&mut buf).map_err(DeError::Io)?;
    from_str(&buf)
}

	/// Deserialize zero or more YAML documents from a string into a streaming
	/// iterator of `T`.
	///
	/// This variant is fully streaming: it does not pre-parse the entire input
	/// into a `Vec<Node>`, but instead parses and deserializes each document on
	/// demand from the underlying event stream.
	pub fn stream_from_str_docs<'de, T>(
	    input: &'de str,
	) -> Result<StreamDeserializer<'de, T>, DeError>
	where
	    T: DeserializeOwned,
	{
	    Ok(StreamDeserializer::new(input))
	}
