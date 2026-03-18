// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Experimental event-based serde deserializer (Option A).
//!
//! This module prototypes a serde `Deserializer` that reads directly from
//! the YAML event stream emitted by `Emitter`, without building the
//! `Node` / `Value` AST. It supports single-document inputs including
//! anchors and aliases (YAML 1.2 style, not YAML 1.1 merge keys).

use std::borrow::Cow;
use std::collections::HashMap;

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
    /// Storage for anchored event sequences. Maps anchor name to the sequence
    /// of events that make up the anchored value. Events are stored with the
    /// 'de lifetime, avoiding the need to clone string content into owned form.
    anchors: HashMap<String, Vec<Event<'de>>>,
    /// When replaying an alias, this holds the events to replay.
    replay_buffer: Option<std::vec::IntoIter<Event<'de>>>,
    /// When recording an anchor, this holds the events being recorded.
    /// Format: (anchor_name, events, nesting_depth)
    recording: Option<(String, Vec<Event<'de>>, usize)>,
}

impl<'de> EventStream<'de> {
    pub(crate) fn new(input: &'de str) -> Self {
        Self {
            emitter: Emitter::new(input),
            peeked: None,
            anchors: HashMap::new(),
            replay_buffer: None,
            recording: None,
        }
    }

    pub(crate) fn take_errors(&mut self) -> Vec<crate::ParseError> {
        self.emitter.take_errors()
    }

    fn peek(&mut self) -> Option<&Event<'de>> {
        if self.peeked.is_none() {
            // Use next_event() instead of self.emitter.next() to ensure
            // recording and replay logic is applied
            self.peeked = self.next_event();
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
        // First check if we have a peeked event - this takes priority over everything
        if let Some(ev) = self.peeked.take() {
            return Some(ev);
        }

        // Fast path: if we're not replaying and not recording, just get from emitter
        // This is the common case for documents without anchors/aliases
        if self.replay_buffer.is_none() && self.recording.is_none() {
            let event = self.emitter.next()?;

            // Quick check: does this event have anchor/alias activity?
            match &event {
                Event::Alias { name, .. } => {
                    // Start replaying if we have this anchor
                    let name_str = name.to_string();
                    if let Some(recorded) = self.anchors.get(&name_str) {
                        self.peeked = None;
                        self.replay_buffer = Some(recorded.clone().into_iter());
                        return self.next_event();
                    } else {
                        // Unknown alias - return it and let the deserializer handle the error
                        return Some(event);
                    }
                }
                Event::Scalar { properties, .. }
                | Event::MappingStart { properties, .. }
                | Event::SequenceStart { properties, .. } => {
                    if let Some(anchor) = &properties.anchor {
                        // Start recording
                        let name = anchor.value.to_string();
                        let initial_depth = match &event {
                            Event::MappingStart { .. } | Event::SequenceStart { .. } => 1,
                            _ => 0, // Scalar - will complete immediately
                        };
                        self.recording = Some((name.clone(), vec![event.clone()], initial_depth));

                        // For scalars, recording is complete immediately
                        if initial_depth == 0 {
                            let (name, recorded, _) = self.recording.take().unwrap();
                            self.anchors.insert(name, recorded);
                        }

                        return Some(event);
                    }
                }
                _ => {}
            }

            // No anchor/alias activity - just return the event
            return Some(event);
        }

        // Slow path: we're replaying or recording
        // Then check if we're replaying from a buffer
        if let Some(ref mut replay) = self.replay_buffer {
            if let Some(event) = replay.next() {
                // If we're also recording, record this replayed event
                if let Some((_, ref mut events, ref mut depth)) = self.recording {
                    events.push(event.clone());
                    // Update depth tracking
                    match &event {
                        Event::MappingStart { .. } | Event::SequenceStart { .. } => *depth += 1,
                        Event::MappingEnd { .. } | Event::SequenceEnd { .. } => {
                            *depth -= 1;
                            if *depth == 0 {
                                // Recording complete
                                let (name, recorded, _) = self.recording.take().unwrap();
                                self.anchors.insert(name, recorded);
                            }
                        }
                        _ => {}
                    }
                }

                return Some(event);
            } else {
                // Replay buffer exhausted
                self.replay_buffer = None;
            }
        }

        // Normal path: get from emitter
        let event = self.emitter.next();

        let event = match event {
            Some(e) => e,
            None => return None,
        };

        // Check if this is an Alias - if so, start replaying
        if let Event::Alias { ref name, .. } = event {
            let name_str = name.to_string();
            if let Some(recorded) = self.anchors.get(&name_str) {
                // Clear any peeked event to avoid confusion during replay
                self.peeked = None;
                self.replay_buffer = Some(recorded.clone().into_iter());
                // Recursively call to get the first replayed event
                return self.next_event();
            } else {
                // Unknown alias - return it and let the deserializer handle the error
                return Some(event);
            }
        }

        // Check if this event has an anchor - if so, start recording
        let anchor_name = match &event {
            Event::Scalar { properties, .. }
            | Event::MappingStart { properties, .. }
            | Event::SequenceStart { properties, .. } => {
                properties.anchor.as_ref().map(|p| p.value.to_string())
            }
            _ => None,
        };

        if let Some(name) = anchor_name {
            // Clear any peeked event to avoid it interfering with recording/replay
            self.peeked = None;
            let initial_depth = match &event {
                Event::MappingStart { .. } | Event::SequenceStart { .. } => 1,
                _ => 0, // Scalar - will complete immediately
            };
            self.recording = Some((name.clone(), vec![event.clone()], initial_depth));

            // For scalars, recording is complete immediately
            if initial_depth == 0 {
                let (name, recorded, _) = self.recording.take().unwrap();
                self.anchors.insert(name, recorded);
            }

            // Return the event - we've already added it to the recording above
            return Some(event);
        }

        // If we're recording (and this event doesn't have an anchor), add this event to the recording
        if let Some((_, ref mut events, ref mut depth)) = self.recording {
            events.push(event.clone());

            // Update depth tracking
            match &event {
                Event::MappingStart { .. } | Event::SequenceStart { .. } => *depth += 1,
                Event::MappingEnd { .. } | Event::SequenceEnd { .. } => {
                    *depth -= 1;
                    if *depth == 0 {
                        // Recording complete
                        let (name, recorded, _) = self.recording.take().unwrap();
                        self.anchors.insert(name, recorded);
                    }
                }
                _ => {}
            }
        }

        Some(event)
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

    /// Clear all stored anchors.
    ///
    /// This should be called between documents to ensure anchors don't leak
    /// across document boundaries.
    pub(crate) fn clear_anchors(&mut self) {
        self.anchors.clear();
    }

    #[inline]
    fn deserialize_scalar<V>(
        &mut self,
        style: ScalarStyle,
        value: Cow<'de, str>,
        visitor: V,
    ) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        // Fast path for quoted scalars - always strings
        if style != ScalarStyle::Plain {
            return match value {
                Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
                Cow::Owned(s) => visitor.visit_string(s),
            };
        }

        // Plain scalars need type inference
        let kind = infer_scalar_kind(value);
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

    // Fast path: check first byte to quickly categorize the scalar
    let first_byte = match s.bytes().next() {
        Some(b) => b,
        None => return ScalarKind::Null, // Empty string is null
    };

    // Dispatch based on first character for maximum efficiency
    match first_byte {
        // Numeric indicators - could be int or float
        b'0'..=b'9' | b'+' | b'-' => {
            // Check for float indicators (decimal point or exponent)
            let has_float_chars = s.bytes().any(|b| b == b'.' || b == b'e' || b == b'E');

            if has_float_chars {
                // Special float values starting with '-'
                if first_byte == b'-' {
                    match s {
                        "-.inf" | "-.Inf" | "-.INF" => return ScalarKind::Float(f64::NEG_INFINITY),
                        _ => {}
                    }
                }
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
            // Failed to parse as number - fall through to string
        }

        // Special float values starting with '.'
        b'.' => {
            match s {
                ".inf" | ".Inf" | ".INF" => return ScalarKind::Float(f64::INFINITY),
                ".nan" | ".NaN" | ".NAN" => return ScalarKind::Float(f64::NAN),
                _ => {
                    // Could be a float like ".5" - try parsing
                    if let Ok(float) = s.parse::<f64>() {
                        return ScalarKind::Float(float);
                    }
                }
            }
        }

        // Boolean and null values
        b'n' | b'N' => match s {
            "null" | "Null" | "NULL" => return ScalarKind::Null,
            _ => {}
        },
        b't' | b'T' => match s {
            "true" | "True" | "TRUE" => return ScalarKind::Bool(true),
            _ => {}
        },
        b'f' | b'F' => match s {
            "false" | "False" | "FALSE" => return ScalarKind::Bool(false),
            _ => {}
        },
        b'~' => {
            if s == "~" {
                return ScalarKind::Null;
            }
        }

        // Everything else is a string
        _ => {}
    }

    // Default to string - return the Cow as-is (zero-copy if borrowed!).
    ScalarKind::String(value)
}

/// Parse a YAML 1.2 core schema boolean literal (case-insensitive variants of
/// `true` / `false`). This mirrors the bool handling in `infer_scalar_kind`
/// but is cheap enough to use in the hot paths of primitive `deserialize_*`
/// methods without running full scalar inference.
#[inline]
fn parse_core_bool(s: &str) -> Option<bool> {
    // Fast path: check first byte and length to avoid full string comparison
    match (s.len(), s.as_bytes().first()?) {
        (4, b't' | b'T') => {
            if s.eq_ignore_ascii_case("true") {
                Some(true)
            } else {
                None
            }
        }
        (5, b'f' | b'F') => {
            if s.eq_ignore_ascii_case("false") {
                Some(false)
            } else {
                None
            }
        }
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

        // Peek at the next event to see if it's the end of the sequence
        let event = self
            .stream
            .peek()
            .ok_or_else(|| DeError::Custom("unexpected end of input inside sequence".into()))?;

        match event {
            Event::SequenceEnd { .. } => {
                // Normal end-of-sequence marker - consume it
                self.stream.next_event();
                self.finished = true;
                Ok(None)
            }
            Event::StreamEnd | Event::DocumentEnd { .. } | Event::MappingEnd { .. } => Err(
                DeError::Custom("unexpected end of structure inside sequence".into()),
            ),
            _ => {
                // It's an element - let the deserializer consume it
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

        // Peek at the next event to see if it's the end of the mapping
        let event = self
            .stream
            .peek()
            .ok_or_else(|| DeError::Custom("unexpected end of input inside mapping".into()))?;

        match event {
            Event::MappingEnd { .. } => {
                // Normal end-of-mapping marker - consume it
                self.stream.next_event();
                Ok(None)
            }
            Event::StreamEnd | Event::DocumentEnd { .. } | Event::SequenceEnd { .. } => Err(
                DeError::Custom("unexpected end of structure inside mapping".into()),
            ),
            _ => {
                // It's a key - let the deserializer consume it
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

        // Anchors and aliases are now handled transparently in next_event(),
        // so we just process the event normally
        match event {
            Event::Scalar { style, value, .. } => self.deserialize_scalar(style, value, visitor),
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
                // This should not happen if next_event() is working correctly,
                // but handle it gracefully
                Err(DeError::UnknownAlias(name.into_owned()))
            }
            _ => Err(DeError::Custom("unexpected event in value position".into())),
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        // Treat core-schema null scalars as `None`, everything else as `Some`.
        let event = self
            .peek()
            .ok_or_else(|| DeError::Custom("unexpected end of input".into()))?;

        // Fast null check: avoid full scalar inference by checking patterns directly.
        let is_plain_null = match event {
            Event::Scalar { style, value, .. } if *style == ScalarStyle::Plain => {
                matches!(value.as_ref(), "null" | "Null" | "NULL" | "~" | "")
            }
            _ => false,
        };

        if is_plain_null {
            // Consume the null and report `None`.
            self.next_event();
            visitor.visit_none()
        } else {
            // Not a null scalar – let the nested deserializer consume it
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
                    "null"
                        | "Null"
                        | "NULL"
                        | "~"
                        | ""
                        | "true"
                        | "True"
                        | "TRUE"
                        | "false"
                        | "False"
                        | "FALSE"
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
                    Err(DeError::Custom(format!("invalid i64 value: {s}")))
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
                    Err(DeError::Custom(format!("invalid u64 value: {s}")))
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
                    Err(DeError::Custom(format!("invalid i128 value: {s}")))
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
                    Err(DeError::Custom(format!("invalid u128 value: {s}")))
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
                    Err(DeError::Custom(
                        "expected single-character string for char".into(),
                    ))
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

/// Streaming deserializer for multiple YAML documents using the event-based backend.
///
/// This is exposed publicly for use by the public `stream_from_str_docs` API.
pub struct EventStreamDeserializer<'de, T> {
    stream: EventStream<'de>,
    finished: bool,
    _marker: std::marker::PhantomData<T>,
}

impl<'de, T> EventStreamDeserializer<'de, T> {
    pub fn new(input: &'de str) -> Self {
        Self {
            stream: EventStream::new(input),
            finished: false,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<'de, T> Iterator for EventStreamDeserializer<'de, T>
where
    T: serde::de::DeserializeOwned,
{
    type Item = Result<T, DeError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        // Clear anchors from the previous document to ensure they don't leak
        // across document boundaries.
        self.stream.clear_anchors();

        let has_doc = self.stream.begin_next_document();
        if !has_doc {
            // No more documents. Check for errors.
            let mut errors = self.stream.take_errors();
            if let Some(err) = errors.pop() {
                self.finished = true;
                return Some(Err(DeError::from(err)));
            }
            self.finished = true;
            return None;
        }

        // Deserialize the document.
        let result = T::deserialize(&mut self.stream);
        if result.is_err() {
            self.finished = true;
            return Some(result);
        }

        // After deserializing, consume any remaining events in this document
        // (e.g., DocumentEnd) to position the stream at the start of the next
        // document.
        loop {
            match self.stream.peek() {
                Some(Event::DocumentEnd { .. }) => {
                    self.stream.advance();
                    break;
                }
                Some(Event::StreamEnd) | None => {
                    break;
                }
                Some(Event::DocumentStart { .. }) => {
                    // Next document starts immediately
                    break;
                }
                _ => {
                    // Unexpected event after document root - this shouldn't happen
                    // if deserialization was successful, but consume it anyway
                    self.stream.advance();
                }
            }
        }

        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use super::super::de;
    use super::from_str_events_internal;
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

    #[test]
    fn event_backend_supports_simple_anchor_alias() {
        // Simple scalar anchor and alias
        let yaml = "- &anchor hello\n- *anchor\n";
        let ast: Vec<String> = de::from_str(yaml).unwrap();
        let ev: Vec<String> = from_str_events_internal(yaml).unwrap();
        assert_eq!(ast, ev);
        assert_eq!(ev, vec!["hello", "hello"]);
    }

    #[test]
    fn event_backend_supports_sequence_anchor_alias() {
        // Sequence with anchor and alias
        let yaml = "tags: &tags\n  - web\n  - api\nservice:\n  name: auth\n  tags: *tags\n";

        #[derive(Debug, PartialEq, Deserialize)]
        struct Service {
            name: String,
            tags: Vec<String>,
        }

        #[derive(Debug, PartialEq, Deserialize)]
        struct Config {
            tags: Vec<String>,
            service: Service,
        }

        let ast: Config = de::from_str(yaml).unwrap();
        let ev: Config = from_str_events_internal(yaml).unwrap();
        assert_eq!(ast, ev);
        assert_eq!(ev.service.tags, vec!["web", "api"]);
    }
}
