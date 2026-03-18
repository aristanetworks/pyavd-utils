// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Experimental event-based serde deserializer (Option A).
//!
//! This module prototypes a serde `Deserializer` that reads directly from
//! the YAML event stream emitted by `Emitter`, without building the
//! `Node` / `Value` AST. It currently supports single-document inputs
//! without anchors/aliases; anchor handling will be added in a later step.

use std::borrow::Cow;

use serde::de::{self, Deserialize, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use serde::forward_to_deserialize_any;

use crate::emitter::Emitter;
use crate::event::{Event, ScalarStyle};
use crate::parser::{could_be_numeric, looks_like_decimal_integer};
use crate::value::Number;

use super::DeError;

/// Internal helper: streaming view over the event iterator from `Emitter`.
pub(crate) struct EventStream<'de> {
    emitter: Emitter<'de>,
    peeked: Option<Event<'de>>,
}

impl<'de> EventStream<'de> {
    pub(crate) fn new(input: &'de str) -> Self {
        Self {
            emitter: Emitter::new(input),
            peeked: None,
        }
    }

    pub(crate) fn take_errors(&mut self) -> Vec<crate::ParseError> {
        self.emitter.take_errors()
    }

    fn peek(&mut self) -> Option<&Event<'de>> {
        if self.peeked.is_none() {
            self.peeked = self.emitter.next();
        }
        self.peeked.as_ref()
    }

    fn advance(&mut self) {
        if self.peeked.is_some() {
            self.peeked = None;
	        } else {
	            let _ = self.emitter.next();
	        }
    }

    fn next_event(&mut self) -> Option<Event<'de>> {
	        if let Some(ev) = self.peeked.take() {
	            Some(ev)
	        } else {
	            self.emitter.next()
	        }
    }

	    /// Push an already-fetched event back so that the next call to
	    /// `next_event` will return it. This is used by higher-level serde
	    /// adapters (sequences, mappings, options) that need to inspect an
	    /// event to decide what to do, but then want to hand it off to normal
	    /// deserialization without forcing another read from the emitter.
	    fn put_back(&mut self, event: Event<'de>) {
	        debug_assert!(self.peeked.is_none());
	        self.peeked = Some(event);
	    }

	    /// Position the stream at the start of the next document's root node, if any.
	    ///
	    /// This mirrors `Parser::parse_next_document`'s document-skipping logic but
	    /// does not parse the document; it only positions the cursor.
	    pub(crate) fn begin_next_document(&mut self) -> bool {
	        loop {
	            let event = match self.peek() {
	                Some(ev) => ev,
	                None => return false,
	            };
	            match event {
	                Event::StreamStart => {
	                    self.advance();
	                }
	                Event::StreamEnd => {
	                    self.advance();
	                    return false;
	                }
	                Event::DocumentStart { .. } => {
	                    self.advance();
	                    return true;
	                }
	                Event::MappingEnd { .. } | Event::SequenceEnd { .. } => {
	                    // Stray end markers - skip them to avoid infinite loops.
	                    self.advance();
	                }
	                _ => {
	                    // Implicit document: first content event starts the document.
	                    return true;
	                }
	            }
	        }
	    }

	    fn deserialize_scalar<V>(
	        &mut self,
	        style: ScalarStyle,
	        value: Cow<'de, str>,
	        visitor: V,
	    ) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        let kind = if style == ScalarStyle::Plain {
	            infer_scalar_kind(value)
	        } else {
	            ScalarKind::String(value)
	        };
	        match kind {
	            ScalarKind::Null => visitor.visit_unit(),
	            ScalarKind::Bool(b) => visitor.visit_bool(b),
	            ScalarKind::Int(num) => match num {
	                Number::I64(i) => visitor.visit_i64(i),
	                Number::U64(u) => visitor.visit_u64(u),
	                Number::I128(i) => visitor.visit_i128(i),
	                Number::U128(u) => visitor.visit_u128(u),
	                Number::BigIntStr(text) => match text {
	                    Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
	                    Cow::Owned(s) => visitor.visit_string(s),
	                },
	            },
	            ScalarKind::Float(f) => visitor.visit_f64(f),
	            ScalarKind::String(text) => match text {
	                Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
	                Cow::Owned(s) => visitor.visit_string(s),
	            },
	        }
	    }
	}

/// Internal classification of a plain scalar value used by the event-based
/// serde backend. This mirrors the parser's scalar inference but avoids
/// constructing a full `Value`.
#[derive(Debug)]
enum ScalarKind<'de> {
    Null,
    Bool(bool),
    Int(Number<'de>),
    Float(f64),
    String(Cow<'de, str>),
}

fn infer_scalar_kind<'de>(value: Cow<'de, str>) -> ScalarKind<'de> {
    let s = value.as_ref();

    // Fast path: check length first to avoid expensive matches on long strings
    if s.len() > 6 {
        // Strings longer than 6 chars can't be null/bool/special floats
        // Only check if numeric
        if !could_be_numeric(s) {
            return ScalarKind::String(value);
        }
        // Fall through to numeric parsing below
    } else {
        // Short strings: check for special values
        match s {
            "null" | "Null" | "NULL" | "~" | "" => return ScalarKind::Null,
            "true" | "True" | "TRUE" => return ScalarKind::Bool(true),
            "false" | "False" | "FALSE" => return ScalarKind::Bool(false),
            // Special float values (all start with '.' or '-.')
            ".inf" | ".Inf" | ".INF" => return ScalarKind::Float(f64::INFINITY),
            "-.inf" | "-.Inf" | "-.INF" => return ScalarKind::Float(f64::NEG_INFINITY),
            ".nan" | ".NaN" | ".NAN" => return ScalarKind::Float(f64::NAN),
            _ => {}
        }

        // Not a special value - check if numeric
        if !could_be_numeric(s) {
            return ScalarKind::String(value);
        }
    }

    // At this point we know it could be numeric
    // Check for float indicators (decimal point or exponent)
    let has_float_chars = s.bytes().any(|b| b == b'.' || b == b'e' || b == b'E');

    if has_float_chars {
        // Try parsing as float
        if let Ok(float) = s.parse::<f64>() {
            return ScalarKind::Float(float);
        }
    } else {
        // No float indicators - try integer parsing
        if let Ok(int) = s.parse::<i64>() {
            return ScalarKind::Int(Number::I64(int));
        }
        if let Ok(int) = s.parse::<i128>() {
            return ScalarKind::Int(Number::I128(int));
        }
        if let Ok(uint) = s.parse::<u128>() {
            return ScalarKind::Int(Number::U128(uint));
        }

        // If it still looks like a plain decimal integer but does not fit in
        // i128/u128, store it as a textual big integer.
        if looks_like_decimal_integer(s) {
            return ScalarKind::Int(Number::BigIntStr(value));
        }
    }

    // Default to string - return the Cow as-is (zero-copy if borrowed!).
    ScalarKind::String(value)
}

/// Parse a YAML 1.2 core schema boolean literal (case-insensitive variants of
/// `true` / `false`). This mirrors the bool handling in `infer_scalar_kind`
/// but is cheap enough to use in the hot paths of primitive `deserialize_*`
/// methods without running full scalar inference.
fn parse_core_bool(s: &str) -> Option<bool> {
    match s {
        "true" | "True" | "TRUE" => Some(true),
        "false" | "False" | "FALSE" => Some(false),
        _ => None,
    }
}

struct SeqAccessImpl<'a, 'de> {
	    stream: &'a mut EventStream<'de>,
	    finished: bool,
}

struct MapAccessImpl<'a, 'de> {
	    stream: &'a mut EventStream<'de>,
	    value_pending: bool,
}

impl<'a, 'de> SeqAccess<'de> for SeqAccessImpl<'a, 'de> {
	    type Error = DeError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, DeError>
	    where
	        T: DeserializeSeed<'de>,
	    {
		        if self.finished {
		            return Ok(None);
		        }

		        let event = self
		            .stream
		            .next_event()
		            .ok_or_else(|| {
		                DeError::Custom("unexpected end of input inside sequence".into())
		            })?;

		        match event {
		            Event::SequenceEnd { .. } => {
		                // Normal end-of-sequence marker.
		                self.finished = true;
		                Ok(None)
		            }
		            Event::StreamEnd | Event::DocumentEnd { .. } | Event::MappingEnd { .. } => Err(
		                DeError::Custom("unexpected end of structure inside sequence".into()),
		            ),
		            other => {
		                // Hand the element's first event back to the main
		                // deserializer so the element can start from a normal
		                // `next_event` call.
		                self.stream.put_back(other);
		                let value = seed.deserialize(&mut *self.stream)?;
		                Ok(Some(value))
		            }
		        }
	    }
}

impl<'a, 'de> MapAccess<'de> for MapAccessImpl<'a, 'de> {
	    type Error = DeError;

	    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, DeError>
	    where
	        K: DeserializeSeed<'de>,
	    {
	        if self.value_pending {
	            // We returned a key but its value was never consumed.
	            return Err(DeError::ValueWithoutKey);
	        }

		        let event = self
		            .stream
		            .next_event()
		            .ok_or_else(|| {
		                DeError::Custom("unexpected end of input inside mapping".into())
		            })?;

		        match event {
		            Event::MappingEnd { .. } => {
		                // Normal end-of-mapping marker.
		                Ok(None)
		            }
		            Event::StreamEnd | Event::DocumentEnd { .. } | Event::SequenceEnd { .. } => Err(
		                DeError::Custom("unexpected end of structure inside mapping".into()),
		            ),
		            other => {
		                // Put the first key event back so the key's
		                // deserialization starts from a normal `next_event`.
		                self.stream.put_back(other);
		                let key = seed.deserialize(&mut *self.stream)?;
		                self.value_pending = true;
		                Ok(Some(key))
		            }
		        }
	    }

	    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, DeError>
	    where
	        V: DeserializeSeed<'de>,
	    {
	        if !self.value_pending {
	            return Err(DeError::ValueWithoutKey);
	        }

	        let value = seed.deserialize(&mut *self.stream)?;
	        self.value_pending = false;
	        Ok(value)
	    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut EventStream<'de> {
	    type Error = DeError;

	    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        let event = self
	            .next_event()
	            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;

	        match event {
	            Event::Scalar { style, value, .. } => {
	                self.deserialize_scalar(style, value, visitor)
	            }
	            Event::SequenceStart { .. } => {
	                let seq = SeqAccessImpl {
	                    stream: self,
	                    finished: false,
	                };
	                visitor.visit_seq(seq)
	            }
	            Event::MappingStart { .. } => {
	                let map = MapAccessImpl {
	                    stream: self,
	                    value_pending: false,
	                };
	                visitor.visit_map(map)
	            }
	            Event::Alias { name, .. } => {
	                // For A2 we do not support anchors/aliases yet.
	                Err(DeError::UnknownAlias(name.into_owned()))
	            }
	            _ => Err(DeError::Custom(
	                "unexpected event in value position".into(),
	            )),
	        }
	    }

	    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        // Treat core-schema null scalars as `None`, everything else as `Some`.
        let event = self
            .next_event()
            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;

        // Fast null check: avoid full scalar inference by checking patterns directly.
        let is_plain_null = match &event {
            Event::Scalar { style, value, .. } if *style == ScalarStyle::Plain => {
                matches!(value.as_ref(), "null" | "Null" | "NULL" | "~" | "")
            }
            _ => false,
        };

        if is_plain_null {
            // Consume the null and report `None`.
            visitor.visit_none()
        } else {
            // Not a null scalar – put the event back so that the nested
            // value deserialization sees it as usual.
            self.put_back(event);
            visitor.visit_some(self)
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
        use serde::de::value::StringDeserializer;

        let event = self
            .next_event()
            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;

        if let Event::Scalar { style, value, .. } = event {
            // Fast path: quoted scalars are always strings.
            // For plain scalars, only reject explicit null/bool patterns.
            let is_string = if style != ScalarStyle::Plain {
                true
            } else {
                // Check only for YAML special values that would NOT be strings.
                !matches!(
                    value.as_ref(),
                    "null" | "Null" | "NULL" | "~" | ""
                        | "true" | "True" | "TRUE"
                        | "false" | "False" | "FALSE"
                ) && !could_be_numeric(value.as_ref())
            };
            if is_string {
                let owned = value.into_owned();
                let de = StringDeserializer::new(owned);
                visitor.visit_enum(de)
            } else {
                Err(DeError::ExpectedEnumString)
            }
        } else {
            Err(DeError::ExpectedEnumString)
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self
            .next_event()
            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;

        match event {
            Event::Scalar { value, .. } => {
                if let Some(b) = parse_core_bool(value.as_ref()) {
                    visitor.visit_bool(b)
                } else {
                    Err(DeError::Custom(format!(
                        "invalid bool value: {}",
                        value.as_ref()
                    )))
                }
            }
            _ => Err(DeError::Custom("expected scalar for bool".into())),
        }
    }

	    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self
            .next_event()
            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;

        match event {
            Event::Scalar { value, .. } => {
                let s = value.as_ref();
                if let Ok(int) = s.parse::<i64>() {
                    visitor.visit_i64(int)
                } else if let Some(b) = parse_core_bool(s) {
                    // Allow bool -> integer conversion via serde's visitors.
                    visitor.visit_bool(b)
                } else {
                    Err(DeError::Custom(format!(
                        "invalid i64 value: {s}"
                    )))
                }
            }
            _ => Err(DeError::Custom("expected scalar for integer".into())),
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self
            .next_event()
            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;
        match event {
            Event::Scalar { value, .. } => {
                let s = value.as_ref();
                if let Ok(int) = s.parse::<u64>() {
                    visitor.visit_u64(int)
                } else if let Some(b) = parse_core_bool(s) {
                    visitor.visit_bool(b)
                } else {
                    Err(DeError::Custom(format!(
                        "invalid u64 value: {s}"
                    )))
                }
            }
            _ => Err(DeError::Custom("expected scalar for integer".into())),
        }
    }

    fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self
            .next_event()
            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;
        match event {
            Event::Scalar { value, .. } => {
                let s = value.as_ref();
                if let Ok(int) = s.parse::<i128>() {
                    visitor.visit_i128(int)
                } else if let Some(b) = parse_core_bool(s) {
                    visitor.visit_bool(b)
                } else {
                    Err(DeError::Custom(format!(
                        "invalid i128 value: {s}"
                    )))
                }
            }
            _ => Err(DeError::Custom("expected scalar for integer".into())),
        }
    }

    fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self
            .next_event()
            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;
        match event {
            Event::Scalar { value, .. } => {
                let s = value.as_ref();
                if let Ok(int) = s.parse::<u128>() {
                    visitor.visit_u128(int)
                } else if let Some(b) = parse_core_bool(s) {
                    visitor.visit_bool(b)
                } else {
                    Err(DeError::Custom(format!(
                        "invalid u128 value: {s}"
                    )))
                }
            }
            _ => Err(DeError::Custom("expected scalar for integer".into())),
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        // Delegate to f64 and let serde handle the narrowing conversion.
        self.deserialize_f64(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self
            .next_event()
            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;
        match event {
            Event::Scalar { value, .. } => {
                let s = value.as_ref();
                if let Ok(float) = s.parse::<f64>() {
                    visitor.visit_f64(float)
                } else {
                    // Handle special YAML float values.
                    match s {
                        ".inf" | ".Inf" | ".INF" => visitor.visit_f64(f64::INFINITY),
                        "-.inf" | "-.Inf" | "-.INF" => visitor.visit_f64(f64::NEG_INFINITY),
                        ".nan" | ".NaN" | ".NAN" => visitor.visit_f64(f64::NAN),
                        _ => {
                            if let Some(b) = parse_core_bool(s) {
                                visitor.visit_bool(b)
                            } else {
                                Err(DeError::Custom(format!("invalid f64 value: {s}")))
                            }
                        }
                    }
                }
            }
            _ => Err(DeError::Custom("expected scalar for float".into())),
        }
    }

	    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        let event = self
	            .next_event()
	            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;
	        match event {
	            Event::Scalar { value, .. } => {
	                let s = value.as_ref();
	                let mut chars = s.chars();
	                if let (Some(ch), None) = (chars.next(), chars.next()) {
	                    visitor.visit_char(ch)
	                } else {
	                    Err(DeError::Custom("expected single-character string for char".into()))
	                }
	            }
	            _ => Err(DeError::Custom("expected scalar for char".into())),
	        }
	    }

	    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        let event = self
	            .next_event()
	            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;
		    match event {
		        Event::Scalar { value, .. } => match value {
		            Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
		            Cow::Owned(s) => visitor.visit_string(s),
		        },
		        _ => Err(DeError::Custom("expected scalar for str".into())),
		    }
	    }

	    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        self.deserialize_str(visitor)
	    }

	    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        let event = self
	            .next_event()
	            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;
	        match event {
	            Event::Scalar { style, value, .. } => self.deserialize_scalar(style, value, visitor),
	            _ => Err(DeError::Custom("expected scalar for bytes".into())),
	        }
	    }

	    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        self.deserialize_bytes(visitor)
	    }

	    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        let event = self
	            .next_event()
	            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;
	        match event {
	            Event::Scalar { style, value, .. } => self.deserialize_scalar(style, value, visitor),
	            _ => Err(DeError::Custom("expected scalar for unit".into())),
	        }
	    }

	    fn deserialize_unit_struct<V>(
	        self,
	        _name: &'static str,
	        visitor: V,
	    ) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        self.deserialize_unit(visitor)
	    }

	    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        let event = self
	            .next_event()
	            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;

	        match event {
	            Event::SequenceStart { .. } => {
	                let seq = SeqAccessImpl {
	                    stream: self,
	                    finished: false,
	                };
	                visitor.visit_seq(seq)
	            }
	            _ => Err(DeError::Custom(
	                "expected sequence start for seq deserialization".to_string(),
	            )),
	        }
	    }

	    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        // YAML has no native tuple distinction; treat as a sequence.
	        self.deserialize_seq(visitor)
	    }

	    fn deserialize_tuple_struct<V>(
	        self,
	        _name: &'static str,
	        _len: usize,
	        visitor: V,
	    ) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        self.deserialize_seq(visitor)
	    }

	    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        let event = self
	            .next_event()
	            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;

	        match event {
	            Event::MappingStart { .. } => {
	                let map = MapAccessImpl {
	                    stream: self,
	                    value_pending: false,
	                };
	                visitor.visit_map(map)
	            }
	            _ => Err(DeError::Custom(
	                "expected mapping start for map deserialization".to_string(),
	            )),
	        }
	    }

	    fn deserialize_struct<V>(
	        self,
	        _name: &'static str,
	        _fields: &'static [&'static str],
	        visitor: V,
	    ) -> Result<V::Value, DeError>
	    where
	        V: Visitor<'de>,
	    {
	        // Represent structs as mappings in YAML.
	        self.deserialize_map(visitor)
	    }

	    forward_to_deserialize_any! {
	        i8 i16 i32 u8 u16 u32
	        newtype_struct identifier ignored_any
	    }
	}

/// Internal helper: deserialize a single document via the event-based backend.
///
/// This is exposed publicly only for tests and benchmarks. It is not a stable
/// part of the public API and may change or be removed at any time. Public
/// serde entrypoints (`from_str`, `stream_from_str_docs`) continue to use the
/// AST-backed path for now.
pub fn from_str_events_internal<'de, T>(input: &'de str) -> Result<T, DeError>
where
	    T: Deserialize<'de>,
{
	    let mut stream = EventStream::new(input);

	    let has_doc = stream.begin_next_document();
	    if !has_doc {
	        let mut errors = stream.take_errors();
	        if let Some(err) = errors.pop() {
	            return Err(DeError::from(err));
	        }
	        return Err(DeError::NoDocument);
	    }

	    let value = T::deserialize(&mut stream)?;

	    // After deserializing the root, ensure there are no extra documents.
	    let mut has_extra_doc = false;
	    while let Some(ev) = stream.next_event() {
	        match ev {
	            Event::StreamStart => {}
	            Event::StreamEnd => break,
	            Event::DocumentStart { .. } => {
	                has_extra_doc = true;
	                break;
	            }
	            Event::DocumentEnd { .. } => {}
	            _ => {
	                has_extra_doc = true;
	                break;
	            }
	        }
	    }

	    let mut errors = stream.take_errors();
	    if let Some(err) = errors.pop() {
	        return Err(DeError::from(err));
	    }
	    if has_extra_doc {
	        return Err(DeError::MultipleDocuments);
	    }

	    Ok(value)
}

#[cfg(test)]
mod tests {
	    use super::from_str_events_internal;
	    use super::super::de;
	    use serde::Deserialize;
	    use std::collections::BTreeMap;

	    #[test]
	    fn event_backend_matches_ast_for_simple_scalars() {
	        let yaml_i64 = "42\n";
	        let ast_i64: i64 = de::from_str(yaml_i64).unwrap();
	        let ev_i64: i64 = from_str_events_internal(yaml_i64).unwrap();
	        assert_eq!(ast_i64, ev_i64);

	        let yaml_bool = "true\n";
	        let ast_bool: bool = de::from_str(yaml_bool).unwrap();
	        let ev_bool: bool = from_str_events_internal(yaml_bool).unwrap();
	        assert_eq!(ast_bool, ev_bool);

	        let yaml_str = "hello\n";
	        let ast_str: String = de::from_str(yaml_str).unwrap();
	        let ev_str: String = from_str_events_internal(yaml_str).unwrap();
	        assert_eq!(ast_str, ev_str);
	    }

	    #[test]
	    fn event_backend_matches_ast_for_seq_and_map() {
	        let yaml_seq = "- 1\n- 2\n- 3\n";
	        let ast_seq: Vec<i64> = de::from_str(yaml_seq).unwrap();
	        let ev_seq: Vec<i64> = from_str_events_internal(yaml_seq).unwrap();
	        assert_eq!(ast_seq, ev_seq);

	        let yaml_map = "a: 1\nb: 2\n";
	        let ast_map: BTreeMap<String, i64> = de::from_str(yaml_map).unwrap();
	        let ev_map: BTreeMap<String, i64> = from_str_events_internal(yaml_map).unwrap();
	        assert_eq!(ast_map, ev_map);
	    }

	    #[derive(Debug, PartialEq, Deserialize)]
	    struct Foo {
	        a: i64,
	        b: String,
	    }

	    #[test]
	    fn event_backend_matches_ast_for_struct() {
	        let yaml = "a: 10\nb: foo\n";
	        let ast: Foo = de::from_str(yaml).unwrap();
	        let ev: Foo = from_str_events_internal(yaml).unwrap();
	        assert_eq!(ast, ev);
	    }
}

