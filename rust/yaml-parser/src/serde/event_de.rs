// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Event-driven serde deserializer.
//!
//! This module implements the crate's serde `Deserializer` directly on top of
//! the YAML event stream emitted by `Emitter`, without building the `Node` /
//! `Value` AST first. It powers both single-document deserialization and the
//! public document-by-document streaming API, including anchors and aliases
//! (YAML 1.2 style, not YAML 1.1 merge keys).

use std::borrow::Cow;
use std::collections::HashMap;

use serde::de::{
    self, Deserialize, DeserializeOwned, DeserializeSeed, MapAccess, SeqAccess, Visitor,
};
use serde::forward_to_deserialize_any;

use crate::emitter::Emitter;
use crate::event::{Event, Properties, ScalarStyle};
use crate::scalar_resolver::{
    ResolvedScalar, ScalarResolutionError, resolve_tagged_scalar, resolve_untagged_scalar,
};

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
    /// Format: (`anchor_name`, events, `nesting_depth`)
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
                    }
                    // Unknown alias - return it and let the deserializer handle the error
                    return Some(event);
                }
                Event::Scalar { properties, .. }
                | Event::MappingStart { properties, .. }
                | Event::SequenceStart { properties, .. } => {
                    if let Some(anchor) = properties
                        .as_ref()
                        .and_then(|event_props| event_props.anchor.as_ref())
                    {
                        // Start recording
                        let anchor_name = anchor.value.to_string();
                        let initial_depth = match &event {
                            Event::MappingStart { .. } | Event::SequenceStart { .. } => 1,
                            _ => 0, // Scalar - will complete immediately
                        };
                        self.recording =
                            Some((anchor_name.clone(), vec![event.clone()], initial_depth));

                        // For scalars, recording is complete immediately
                        if initial_depth == 0
                            && let Some((anchor_key, recorded, _)) = self.recording.take()
                        {
                            self.anchors.insert(anchor_key, recorded);
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
        self.next_event_slow_path()
    }

    /// Slow path for `next_event` when replaying aliases or recording anchors.
    fn next_event_slow_path(&mut self) -> Option<Event<'de>> {
        // Check if we're replaying from a buffer
        if let Some(ref mut replay) = self.replay_buffer {
            if let Some(event) = replay.next() {
                // If we're also recording, record this replayed event
                self.record_event_if_active(&event);
                return Some(event);
            }
            // Replay buffer exhausted
            self.replay_buffer = None;
        }

        // Normal path: get from emitter
        let event = self.emitter.next()?;

        // Check if this is an Alias - if so, start replaying
        if let Event::Alias { ref name, .. } = event {
            let name_str = name.to_string();
            if let Some(recorded) = self.anchors.get(&name_str) {
                debug_assert!(
                    self.peeked.is_none(),
                    "peeked event must be empty before starting alias replay"
                );
                self.replay_buffer = Some(recorded.clone().into_iter());
                // Recursively call to get the first replayed event
                return self.next_event();
            }
            // Unknown alias - return it and let the deserializer handle the error
            return Some(event);
        }

        // Check if this event has an anchor - if so, start recording
        let anchor_name = match &event {
            Event::Scalar { properties, .. }
            | Event::MappingStart { properties, .. }
            | Event::SequenceStart { properties, .. } => properties
                .as_ref()
                .and_then(|event_props| event_props.anchor.as_ref())
                .map(|prop| prop.value.to_string()),
            _ => None,
        };

        if let Some(name) = anchor_name {
            debug_assert!(
                self.peeked.is_none(),
                "peeked event must be empty before starting anchor recording"
            );
            let initial_depth = match &event {
                Event::MappingStart { .. } | Event::SequenceStart { .. } => 1,
                _ => 0, // Scalar - will complete immediately
            };
            self.recording = Some((name.clone(), vec![event.clone()], initial_depth));

            // For scalars, recording is complete immediately
            if initial_depth == 0
                && let Some((anchor_key, recorded, _)) = self.recording.take()
            {
                self.anchors.insert(anchor_key, recorded);
            }

            // Return the event - we've already added it to the recording above
            return Some(event);
        }

        // If we're recording (and this event doesn't have an anchor), add this event to the recording
        self.record_event_if_active(&event);

        Some(event)
    }

    /// If anchor recording is active, record the event and update depth tracking.
    fn record_event_if_active(&mut self, event: &Event<'de>) {
        if let Some((_, ref mut events, ref mut depth)) = self.recording {
            events.push(event.clone());

            // Update depth tracking
            match event {
                Event::MappingStart { .. } | Event::SequenceStart { .. } => *depth += 1,
                Event::MappingEnd { .. } | Event::SequenceEnd { .. } => {
                    *depth -= 1;
                    if *depth == 0 {
                        // Recording complete
                        if let Some((anchor_key, recorded, _)) = self.recording.take() {
                            self.anchors.insert(anchor_key, recorded);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Position the stream at the start of the next document's root node, if any.
    ///
    /// This mirrors `Parser::parse_next_document`'s document-skipping logic but
    /// does not parse the document; it only positions the cursor.
    pub(crate) fn begin_next_document(&mut self) -> bool {
        loop {
            let Some(event) = self.peek() else {
                return false;
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

    fn invalid_pair_error() -> DeError {
        DeError::Custom("invalid mapping entry: expected ':' after the key".into())
    }

    fn next_value_event(&mut self) -> Result<Event<'de>, DeError> {
        match self.next_event() {
            Some(Event::InvalidatePair { .. }) => Err(Self::invalid_pair_error()),
            Some(event) => Ok(event),
            None => Err(DeError::Custom("unexpected end of input".into())),
        }
    }

    #[inline]
    fn deserialize_scalar<V>(
        style: ScalarStyle,
        value: Cow<'de, str>,
        properties: Option<&Properties<'de>>,
        visitor: V,
    ) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let resolution = Self::resolve_scalar_event(style, value, properties)?;
        match resolution {
            ResolvedScalar::Null => visitor.visit_unit(),
            ResolvedScalar::Bool(bool_val) => visitor.visit_bool(bool_val),
            ResolvedScalar::Int(num) => match num {
                crate::value::Integer::I64(int_val) => visitor.visit_i64(int_val),
                crate::value::Integer::U64(uint_val) => visitor.visit_u64(uint_val),
                crate::value::Integer::I128(int_val) => visitor.visit_i128(int_val),
                crate::value::Integer::U128(uint_val) => visitor.visit_u128(uint_val),
                crate::value::Integer::BigIntStr(text) => match text {
                    Cow::Borrowed(str_ref) => visitor.visit_borrowed_str(str_ref),
                    Cow::Owned(str_owned) => visitor.visit_string(str_owned),
                },
            },
            ResolvedScalar::Float(float_val) => visitor.visit_f64(float_val),
            ResolvedScalar::String(text) => match text {
                Cow::Borrowed(str_ref) => visitor.visit_borrowed_str(str_ref),
                Cow::Owned(str_owned) => visitor.visit_string(str_owned),
            },
        }
    }

    fn resolve_scalar_event(
        style: ScalarStyle,
        value: Cow<'de, str>,
        properties: Option<&Properties<'de>>,
    ) -> Result<ResolvedScalar<'de>, DeError> {
        if let Some(tag) = properties.and_then(|event_props| event_props.tag.as_ref()) {
            resolve_tagged_scalar(value, tag.value.as_ref()).map_err(Self::scalar_resolution_error)
        } else {
            Ok(resolve_untagged_scalar(value, style))
        }
    }

    fn scalar_resolution_error(error: ScalarResolutionError<'_>) -> DeError {
        match error {
            ScalarResolutionError::InvalidExplicitBuiltinTagValue { tag, original_text } => {
                DeError::Custom(format!(
                    "invalid value for explicit {} tag: {}",
                    tag.display_name(),
                    original_text.as_ref()
                ))
            }
        }
    }

    fn unexpected_scalar_kind(expected: &str, value: &ResolvedScalar<'_>) -> DeError {
        let found = match value {
            ResolvedScalar::Null => "null",
            ResolvedScalar::Bool(_) => "bool",
            ResolvedScalar::Int(_) => "int",
            ResolvedScalar::Float(_) => "float",
            ResolvedScalar::String(_) => "string",
        };
        DeError::Custom(format!("expected {expected}, found {found}"))
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

impl<'de> SeqAccess<'de> for SeqAccessImpl<'_, 'de> {
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
            Event::InvalidatePair { .. } => Err(EventStream::invalid_pair_error()),
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

impl<'de> MapAccess<'de> for MapAccessImpl<'_, 'de> {
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
            Event::InvalidatePair { .. } => Err(EventStream::invalid_pair_error()),
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

        if matches!(self.stream.peek(), Some(Event::InvalidatePair { .. })) {
            let _ = self.stream.next_event();
            self.value_pending = false;
            return Err(EventStream::invalid_pair_error());
        }

        let value = seed.deserialize(&mut *self.stream)?;
        self.value_pending = false;
        Ok(value)
    }
}

impl<'de> de::Deserializer<'de> for &mut EventStream<'de> {
    type Error = DeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self.next_value_event()?;

        // Anchors and aliases are now handled transparently in next_event(),
        // so we just process the event normally
        match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => EventStream::deserialize_scalar(style, value, properties.as_deref(), visitor),
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

        if matches!(event, Event::InvalidatePair { .. }) {
            return Err(EventStream::invalid_pair_error());
        }

        let is_null = match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => matches!(
                EventStream::resolve_scalar_event(*style, value.clone(), properties.as_deref())?,
                ResolvedScalar::Null
            ),
            _ => false,
        };

        if is_null {
            // Consume the null and report `None`.
            self.next_event();
            visitor.visit_none()
        } else {
            // Not a null scalar – let the nested deserializer consume it
            visitor.visit_some(self)
        }
    }

    // TODO: Improve this to deserialize struct and tuple variants. Same for serializer.
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

        let event = self.next_value_event()?;

        if let Event::Scalar {
            style,
            value,
            properties,
            ..
        } = event
        {
            let resolved = EventStream::resolve_scalar_event(style, value, properties.as_deref())?;
            if let ResolvedScalar::String(text) = resolved {
                let owned = text.into_owned();
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
        let event = self.next_value_event()?;

        match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => match EventStream::resolve_scalar_event(style, value, properties.as_deref())? {
                ResolvedScalar::Bool(bool_val) => visitor.visit_bool(bool_val),
                other => Err(EventStream::unexpected_scalar_kind("bool", &other)),
            },
            _ => Err(DeError::Custom("expected scalar for bool".into())),
        }
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self.next_value_event()?;

        match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => match EventStream::resolve_scalar_event(style, value, properties.as_deref())? {
                ResolvedScalar::Int(crate::value::Integer::I64(int)) => visitor.visit_i64(int),
                ResolvedScalar::Int(crate::value::Integer::I128(int)) => visitor.visit_i128(int),
                ResolvedScalar::Int(crate::value::Integer::U64(int)) => visitor.visit_u64(int),
                ResolvedScalar::Int(crate::value::Integer::U128(int)) => visitor.visit_u128(int),
                other => Err(EventStream::unexpected_scalar_kind(
                    "i64-compatible integer",
                    &other,
                )),
            },
            _ => Err(DeError::Custom("expected scalar for integer".into())),
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self.next_value_event()?;
        match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => match EventStream::resolve_scalar_event(style, value, properties.as_deref())? {
                ResolvedScalar::Int(crate::value::Integer::I64(int)) => visitor.visit_i64(int),
                ResolvedScalar::Int(crate::value::Integer::I128(int)) => visitor.visit_i128(int),
                ResolvedScalar::Int(crate::value::Integer::U64(int)) => visitor.visit_u64(int),
                ResolvedScalar::Int(crate::value::Integer::U128(int)) => visitor.visit_u128(int),
                other => Err(EventStream::unexpected_scalar_kind(
                    "u64-compatible integer",
                    &other,
                )),
            },
            _ => Err(DeError::Custom("expected scalar for integer".into())),
        }
    }

    fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self.next_value_event()?;
        match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => match EventStream::resolve_scalar_event(style, value, properties.as_deref())? {
                ResolvedScalar::Int(crate::value::Integer::I64(int)) => visitor.visit_i64(int),
                ResolvedScalar::Int(crate::value::Integer::I128(int)) => visitor.visit_i128(int),
                ResolvedScalar::Int(crate::value::Integer::U64(int)) => visitor.visit_u64(int),
                ResolvedScalar::Int(crate::value::Integer::U128(int)) => visitor.visit_u128(int),
                other => Err(EventStream::unexpected_scalar_kind(
                    "i128-compatible integer",
                    &other,
                )),
            },
            _ => Err(DeError::Custom("expected scalar for integer".into())),
        }
    }

    fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self.next_value_event()?;
        match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => match EventStream::resolve_scalar_event(style, value, properties.as_deref())? {
                ResolvedScalar::Int(crate::value::Integer::I64(int)) => visitor.visit_i64(int),
                ResolvedScalar::Int(crate::value::Integer::I128(int)) => visitor.visit_i128(int),
                ResolvedScalar::Int(crate::value::Integer::U64(int)) => visitor.visit_u64(int),
                ResolvedScalar::Int(crate::value::Integer::U128(int)) => visitor.visit_u128(int),
                other => Err(EventStream::unexpected_scalar_kind(
                    "u128-compatible integer",
                    &other,
                )),
            },
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
        let event = self.next_value_event()?;
        match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => match EventStream::resolve_scalar_event(style, value, properties.as_deref())? {
                ResolvedScalar::Float(float) => visitor.visit_f64(float),
                other => Err(EventStream::unexpected_scalar_kind("float", &other)),
            },
            _ => Err(DeError::Custom("expected scalar for float".into())),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, DeError>
    where
        V: Visitor<'de>,
    {
        let event = self.next_value_event()?;
        match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => {
                let _ =
                    EventStream::resolve_scalar_event(style, value.clone(), properties.as_deref())?;
                let scalar = value.as_ref();
                let mut chars = scalar.chars();
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
        let event = self.next_value_event()?;
        match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => {
                let _ =
                    EventStream::resolve_scalar_event(style, value.clone(), properties.as_deref())?;
                match value {
                    Cow::Borrowed(str_ref) => visitor.visit_borrowed_str(str_ref),
                    Cow::Owned(str_owned) => visitor.visit_string(str_owned),
                }
            }
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
        let event = self.next_value_event()?;
        match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => EventStream::deserialize_scalar(style, value, properties.as_deref(), visitor),
            _ => Err(DeError::Custom("expected scalar for bytes".into())),
        }
    }

    // TODO: Improve and add roundtrip tests
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
        let event = self.next_value_event()?;
        match event {
            Event::Scalar {
                style,
                value,
                properties,
                ..
            } => EventStream::deserialize_scalar(style, value, properties.as_deref(), visitor),
            _ => Err(DeError::Custom("expected scalar for unit".into())),
        }
    }

    // TODO: Improve and add roundtrip tests
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
        let event = self.next_value_event()?;

        match event {
            Event::SequenceStart { .. } => {
                let seq = SeqAccessImpl {
                    stream: self,
                    finished: false,
                };
                visitor.visit_seq(seq)
            }
            _ => Err(DeError::Custom(
                "expected sequence start for seq deserialization".to_owned(),
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
        let event = self.next_value_event()?;

        match event {
            Event::MappingStart { .. } => {
                let map = MapAccessImpl {
                    stream: self,
                    value_pending: false,
                };
                visitor.visit_map(map)
            }
            _ => Err(DeError::Custom(
                "expected mapping start for map deserialization".to_owned(),
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
/// The public serde entrypoints (`from_str`, `from_reader`,
/// `stream_from_str_docs`) all route through this implementation.
pub(super) fn from_str_internal<'de, T>(input: &'de str) -> Result<T, DeError>
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
            Event::StreamStart | Event::DocumentEnd { .. } => {}
            Event::StreamEnd => break,
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

impl<T> std::fmt::Debug for EventStreamDeserializer<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EventStreamDeserializer")
            .field("finished", &self.finished)
            .finish_non_exhaustive()
    }
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

impl<T> Iterator for EventStreamDeserializer<'_, T>
where
    T: DeserializeOwned,
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
#[allow(
    clippy::expect_used,
    reason = "Approved for test assertions in this module"
)]
mod tests {
    use super::from_str_internal;
    use serde::Deserialize;

    use crate::value::{Integer, Value};

    #[test]
    fn deserializes_simple_scalars() {
        let yaml_i64 = "42\n";
        let value_i64: i64 = from_str_internal(yaml_i64).unwrap();
        assert_eq!(value_i64, 42);

        let yaml_bool = "true\n";
        let value_bool: bool = from_str_internal(yaml_bool).unwrap();
        assert!(value_bool);

        let yaml_str = "hello\n";
        let value_str: String = from_str_internal(yaml_str).unwrap();
        assert_eq!(value_str, "hello");
    }

    #[test]
    fn deserializes_sequences_and_mappings() {
        let yaml_seq = "- 1\n- 2\n- 3\n";
        let value_seq: Vec<i64> = from_str_internal(yaml_seq).unwrap();
        assert_eq!(value_seq, vec![1, 2, 3]);

        let yaml_map = "a: 1\nb: 2\n";
        let value_map: std::collections::BTreeMap<String, i64> =
            from_str_internal(yaml_map).unwrap();
        assert_eq!(value_map.get("a"), Some(&1));
        assert_eq!(value_map.get("b"), Some(&2));
    }

    #[derive(Debug, PartialEq, Deserialize)]
    struct Foo {
        num: i64,
        text: String,
    }

    #[test]
    fn deserializes_structs() {
        let yaml = "num: 10\ntext: foo\n";
        let value: Foo = from_str_internal(yaml).unwrap();
        assert_eq!(
            value,
            Foo {
                num: 10,
                text: "foo".to_owned(),
            }
        );
    }

    #[test]
    fn supports_simple_anchor_alias() {
        // Simple scalar anchor and alias
        let yaml = "- &anchor hello\n- *anchor\n";
        let value: Vec<String> = from_str_internal(yaml).unwrap();
        assert_eq!(value, vec!["hello", "hello"]);
    }

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

    #[test]
    fn supports_sequence_anchor_alias() {
        // Sequence with anchor and alias
        let yaml = "tags: &tags\n  - web\n  - api\nservice:\n  name: auth\n  tags: *tags\n";

        let value: Config = from_str_internal(yaml).unwrap();
        assert_eq!(value.service.tags, vec!["web", "api"]);
    }

    #[test]
    fn reports_missing_colon_in_flow_mapping_during_deserialization() {
        let yaml = "{\"a\" [1], c: 3}";
        let result = from_str_internal::<std::collections::BTreeMap<String, i64>>(yaml);
        assert!(
            result.is_err(),
            "invalid mapping pair should fail serde deserialization"
        );
        let Err(error) = result else {
            return;
        };

        assert!(
            error
                .to_string()
                .contains("invalid mapping entry: expected ':' after the key"),
            "unexpected error message: {error}"
        );
    }

    #[test]
    fn quoted_scalars_do_not_deserialize_as_typed_primitives() {
        let quoted_bool = from_str_internal::<bool>("\"true\"");
        assert!(
            quoted_bool.is_err(),
            "quoted YAML string should not deserialize as bool"
        );

        let quoted_int = from_str_internal::<i64>("'42'");
        assert!(
            quoted_int.is_err(),
            "quoted YAML string should not deserialize as integer"
        );
    }

    #[test]
    fn shared_resolver_handles_core_schema_numbers_and_tags() {
        let octal: i64 = from_str_internal("0o52").expect("octal integer should deserialize");
        assert_eq!(octal, 42);

        let tagged_hex: i64 =
            from_str_internal("!!int 0x2A").expect("explicit int tag should deserialize");
        assert_eq!(tagged_hex, 42);

        let tagged_str: String =
            from_str_internal("!!str 42").expect("explicit str tag should deserialize");
        assert_eq!(tagged_str, "42");

        let positive_inf: f64 =
            from_str_internal("+.INF").expect("positive infinity should deserialize");
        assert!(positive_inf.is_infinite() && positive_inf.is_sign_positive());
    }

    #[test]
    fn deserialize_any_and_typed_deserialization_agree_on_scalar_meaning() {
        let value: Value<'_> = from_str_internal("0x2A").expect("Value should deserialize");
        assert!(matches!(value, Value::Int(Integer::I64(42))));

        let typed: i64 = from_str_internal("0x2A").expect("i64 should deserialize");
        assert_eq!(typed, 42);

        let tagged_string_value: Value<'_> =
            from_str_internal("!custom 42").expect("custom-tagged string should deserialize");
        assert!(matches!(tagged_string_value, Value::String(text) if text == "42"));

        let typed_custom_int = from_str_internal::<i64>("!custom 42");
        assert!(
            typed_custom_int.is_err(),
            "custom-tagged scalar should not implicitly deserialize as integer"
        );
    }

    #[test]
    fn invalid_explicit_builtin_tag_content_returns_error() {
        for input in ["!!int hello", "!!float nope", "!!bool 1"] {
            let error = from_str_internal::<Value<'_>>(input).expect_err("expected tag error");
            assert!(
                error.to_string().contains("invalid value for explicit"),
                "unexpected error for {input:?}: {error}",
            );
        }
    }
}
