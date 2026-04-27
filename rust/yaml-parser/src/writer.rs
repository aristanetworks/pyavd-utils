// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Event-based YAML writer.
//!
//! This module implements a YAML serializer that operates directly on the
//! `Event` stream produced by the emitter.
//!
//! The writer is intentionally conservative: it prioritizes structurally
//! correct output and successful round-tripping through the lexer + emitter
//! pipeline over preserving every presentation detail from the original
//! source.
//!
//! Public contract:
//!
//! - input is an event slice, not an AST,
//! - output is valid YAML for supported event shapes,
//! - explicit `---` and `...` document markers are preserved when present,
//! - comments are not emitted,
//! - formatting is chosen by the writer and is not intended to be a
//!   presentation-perfect reproduction of the original source.

use std::io::{self, Write};

use crate::event::{CollectionStyle, Event, Properties, ScalarStyle};

const EMPTY_PROPERTIES: Properties<'static> = Properties {
    anchor: None,
    tag: None,
};
const INDENT_CHUNK: [u8; 64] = [b' '; 64];

/// Write YAML text from a sequence of events.
///
/// This function consumes an in-memory event slice and emits a single YAML
/// stream using a simple, mostly block-style presentation.
pub fn write_yaml_from_events<W>(mut writer: W, events: &[Event<'_>]) -> io::Result<()>
where
    W: Write,
{
    let mut state = WriterState::new(&mut writer, events);
    state.write_stream()
}

struct WriterState<'a, 'input, W: Write> {
    out: &'a mut W,
    events: &'a [Event<'input>],
    pos: usize,
    indent: usize,
    at_line_start: bool,
    /// Global iteration guard to prevent infinite loops. We expect to perform
    /// at most `O(events.len())` iterations across all loops; if this counter
    /// is exhausted, the writer will return an error instead of hanging.
    guard: usize,
}

impl<'a, 'input, W: Write> WriterState<'a, 'input, W> {
    fn properties_or_empty<'b>(
        properties: Option<&'b Properties<'input>>,
    ) -> &'b Properties<'input> {
        properties.unwrap_or(&EMPTY_PROPERTIES)
    }

    fn new(out: &'a mut W, events: &'a [Event<'input>]) -> Self {
        Self {
            out,
            events,
            pos: 0,
            indent: 0,
            at_line_start: true,
            guard: events.len().saturating_mul(10).saturating_add(10),
        }
    }

    fn tick(&mut self, context: &str) -> io::Result<()> {
        if self.guard == 0 {
            return Err(io::Error::other(format!(
                "yaml writer made no progress (guard exhausted) in {context} at pos {} with event {:?}",
                self.pos,
                self.peek(),
            )));
        }
        self.guard -= 1;
        Ok(())
    }

    fn write_stream(&mut self) -> io::Result<()> {
        while let Some(event) = self.peek().cloned() {
            self.tick("write_stream")?;
            match event {
                Event::StreamStart
                | Event::MappingEnd { .. }
                | Event::SequenceEnd { .. }
                | Event::InvalidatePair { .. } => {
                    // Stream start and stray collection end markers at the
                    // top level should not cause us to loop; if they
                    // appear here it means a nested writer already
                    // consumed the corresponding start event.
                    self.advance();
                }
                Event::StreamEnd => {
                    self.advance();
                    break;
                }
                Event::DocumentStart { explicit, .. } => {
                    self.advance();
                    if explicit {
                        if !self.at_line_start {
                            self.out.write_all(b"\n")?;
                        }
                        self.out.write_all(b"---\n")?;
                        self.at_line_start = true;
                    }
                    self.write_node()?;
                }
                Event::DocumentEnd { explicit, .. } => {
                    self.advance();
                    if explicit {
                        if !self.at_line_start {
                            self.out.write_all(b"\n")?;
                        }
                        self.out.write_all(b"...\n")?;
                        self.at_line_start = true;
                    } else if !self.at_line_start {
                        self.out.write_all(b"\n")?;
                        self.at_line_start = true;
                    }
                }
                _ => {
                    // Implicit document
                    self.write_node()?;
                }
            }
        }
        if !self.at_line_start {
            self.out.write_all(b"\n")?;
        }
        Ok(())
    }

    fn write_node(&mut self) -> io::Result<()> {
        let Some(next_event) = self.peek().cloned() else {
            return Ok(());
        };
        match next_event {
            Event::MappingStart {
                style,
                ref properties,
                ..
            } => self.write_mapping(style, properties.as_deref()),
            Event::SequenceStart {
                style,
                ref properties,
                ..
            } => self.write_sequence(style, properties.as_deref()),
            Event::Scalar {
                style,
                ref properties,
                ..
            } => {
                // For all scalar nodes, delegate to the generic scalar writer.
                // The scalar writer is responsible for choosing an appropriate
                // YAML presentation (plain / quoted / block) based solely on
                // the scalar value and properties.
                self.write_scalar(style, properties.as_deref(), None)
            }
            Event::Alias { .. } => self.write_alias(),
            Event::DocumentEnd { .. }
            | Event::StreamEnd
            | Event::MappingEnd { .. }
            | Event::SequenceEnd { .. }
            | Event::InvalidatePair { .. } => Ok(()),
            Event::StreamStart | Event::DocumentStart { .. } => {
                // Should have been handled by write_stream
                self.advance();
                Ok(())
            }
        }
    }

    fn write_mapping(
        &mut self,
        style: CollectionStyle,
        properties: Option<&Properties<'input>>,
    ) -> io::Result<()> {
        let mapping_props = Self::properties_or_empty(properties);
        match style {
            CollectionStyle::Block => self.write_block_mapping(mapping_props),
            CollectionStyle::Flow => self.write_flow_mapping(mapping_props),
        }
    }

    fn write_sequence(
        &mut self,
        style: CollectionStyle,
        properties: Option<&Properties<'input>>,
    ) -> io::Result<()> {
        let sequence_props = Self::properties_or_empty(properties);
        match style {
            CollectionStyle::Block => self.write_block_sequence(sequence_props),
            CollectionStyle::Flow => self.write_flow_sequence(sequence_props),
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "block mapping writer is intentionally monolithic for now; refactoring would be non-trivial"
    )]
    fn write_block_mapping(&mut self, mapping_props: &Properties<'input>) -> io::Result<()> {
        // Simple block-style mapping with scalar keys and scalar/collection values.
        self.advance(); // consume MappingStart
        let base_indent = self.indent;
        if !mapping_props.is_empty() {
            if !self.at_line_start {
                self.out.write_all(b"\n")?;
            }
            self.write_indent()?;
            self.write_properties(mapping_props)?;
            self.out.write_all(b"\n")?;
            self.at_line_start = true;
        }
        loop {
            self.tick("write_block_mapping")?;
            match self.peek().cloned() {
                Some(Event::MappingEnd { .. }) | None => {
                    if matches!(self.peek(), Some(Event::MappingEnd { .. })) {
                        self.advance();
                    }
                    self.indent = base_indent;
                    break;
                }
                // Complex key: sequence or mapping used as the key. We render
                // this using the explicit "?" syntax so that the parser
                // reconstructs the same event structure (e.g. for tests like
                // M5DY - Mapping between Sequences).
                Some(Event::SequenceStart {
                    style: seq_style,
                    properties: seq_props,
                    ..
                }) => {
                    if !self.at_line_start {
                        self.out.write_all(b"\n")?;
                        self.at_line_start = true;
                    }
                    // "?" line introducing the complex key
                    self.write_indent()?;
                    self.out.write_all(b"?")?;
                    self.out.write_all(b"\n")?;
                    self.at_line_start = true;
                    // Key block (sequence) indented one level further
                    self.indent = base_indent + 2;
                    self.write_sequence(seq_style, seq_props.as_deref())?;
                    self.indent = base_indent;
                    // Now emit the value header ':' at the mapping indent.
                    if !self.at_line_start {
                        self.out.write_all(b"\n")?;
                        self.at_line_start = true;
                    }
                    self.write_indent()?;
                    self.out.write_all(b":")?;
                    // Decide how to render the value.
                    match self.peek().cloned() {
                        Some(Event::MappingStart {
                            style: map_style,
                            properties: map_props,
                            ..
                        }) => {
                            self.out.write_all(b"\n")?;
                            self.at_line_start = true;
                            self.indent = base_indent + 2;
                            self.write_mapping(map_style, map_props.as_deref())?;
                            self.indent = base_indent;
                        }
                        Some(Event::SequenceStart {
                            style: seq_style2,
                            properties: seq_props2,
                            ..
                        }) => {
                            self.out.write_all(b"\n")?;
                            self.at_line_start = true;
                            self.indent = base_indent + 2;
                            self.write_sequence(seq_style2, seq_props2.as_deref())?;
                            self.indent = base_indent;
                        }
                        Some(Event::Scalar {
                            style: scalar_style,
                            properties: scalar_props,
                            ..
                        }) => {
                            // Scalar value for a complex sequence key.
                            self.out.write_all(b" ")?;
                            self.at_line_start = false;
                            self.write_scalar(scalar_style, scalar_props.as_deref(), None)?;
                        }
                        Some(Event::Alias { .. }) => {
                            self.out.write_all(b" ")?;
                            self.at_line_start = false;
                            self.write_alias()?;
                        }
                        Some(Event::MappingEnd { .. }) | None => {
                            // Complex key with empty value.
                        }
                        _ => {
                            self.advance();
                        }
                    }
                }
                Some(Event::MappingStart {
                    style: map_style,
                    properties: map_props,
                    ..
                }) => {
                    if !self.at_line_start {
                        self.out.write_all(b"\n")?;
                        self.at_line_start = true;
                    }
                    self.write_indent()?;
                    self.out.write_all(b"?")?;
                    self.out.write_all(b"\n")?;
                    self.at_line_start = true;
                    self.indent = base_indent + 2;
                    self.write_mapping(map_style, map_props.as_deref())?;
                    self.indent = base_indent;
                    if !self.at_line_start {
                        self.out.write_all(b"\n")?;
                        self.at_line_start = true;
                    }
                    self.write_indent()?;
                    self.out.write_all(b":")?;
                    match self.peek().cloned() {
                        Some(Event::MappingStart {
                            style: value_map_style,
                            properties: value_map_props,
                            ..
                        }) => {
                            self.out.write_all(b"\n")?;
                            self.at_line_start = true;
                            self.indent = base_indent + 2;
                            self.write_mapping(value_map_style, value_map_props.as_deref())?;
                            self.indent = base_indent;
                        }
                        Some(Event::SequenceStart {
                            style: value_seq_style,
                            properties: value_seq_props,
                            ..
                        }) => {
                            self.out.write_all(b"\n")?;
                            self.at_line_start = true;
                            self.indent = base_indent + 2;
                            self.write_sequence(value_seq_style, value_seq_props.as_deref())?;
                            self.indent = base_indent;
                        }
                        Some(Event::Scalar {
                            style: value_scalar_style,
                            properties: value_scalar_props,
                            ..
                        }) => {
                            self.out.write_all(b" ")?;
                            self.at_line_start = false;
                            self.write_scalar(
                                value_scalar_style,
                                value_scalar_props.as_deref(),
                                None,
                            )?;
                        }
                        Some(Event::Alias { .. }) => {
                            self.out.write_all(b" ")?;
                            self.at_line_start = false;
                            self.write_alias()?;
                        }
                        Some(Event::MappingEnd { .. }) | None => {}
                        _ => {
                            self.advance();
                        }
                    }
                }
                // Simple key: scalar or alias.
                _ => {
                    if !self.at_line_start {
                        self.out.write_all(b"\n")?;
                        self.at_line_start = true;
                    }
                    self.write_indent()?;
                    match self.peek().cloned() {
                        Some(Event::Scalar {
                            style: key_style,
                            properties: key_props,
                            ..
                        }) => {
                            // Simple scalar key rendered inline as `key:`.
                            self.write_scalar(key_style, key_props.as_deref(), Some(':'))?;
                        }
                        Some(Event::Alias { .. }) => {
                            // Alias used as a simple mapping key, e.g. `*b : *a`.
                            self.write_alias()?;
                            // For alias keys, match the YAML test-suite's expectation
                            // in tests like E76Z/26DV by surrounding the ':' with
                            // spaces. This keeps the emitter's colon detection
                            // straightforward and avoids `MissingColon` errors.
                            self.out.write_all(b" :")?;
                        }
                        _ => {
                            self.advance();
                            continue;
                        }
                    }
                    match self.peek().cloned() {
                        Some(Event::MappingStart {
                            style: map_style,
                            properties: map_props,
                            ..
                        }) => {
                            self.out.write_all(b"\n")?;
                            self.at_line_start = true;
                            self.indent = base_indent + 2;
                            self.write_mapping(map_style, map_props.as_deref())?;
                            self.indent = base_indent;
                        }
                        Some(Event::SequenceStart {
                            style: seq_style,
                            properties: seq_props,
                            ..
                        }) => {
                            self.out.write_all(b"\n")?;
                            self.at_line_start = true;
                            self.indent = base_indent + 2;
                            self.write_sequence(seq_style, seq_props.as_deref())?;
                            self.indent = base_indent;
                        }
                        Some(Event::Scalar {
                            style: scalar_style,
                            properties: scalar_props,
                            ref value,
                            ..
                        }) => {
                            // Simple scalar value rendered inline after `key:`.
                            self.out.write_all(b" ")?;
                            self.at_line_start = false;
                            if value.is_empty()
                                && Self::properties_or_empty(scalar_props.as_deref()).is_empty()
                            {
                                // Represent an empty scalar value explicitly as "" to
                                // avoid the ambiguous `key:` form that can confuse
                                // the emitter when followed by lines that start with
                                // anchors/properties (see tests PW8X / ZWK4).
                                self.out.write_all(b"\"\"")?;
                                self.advance();
                            } else {
                                self.write_scalar(scalar_style, scalar_props.as_deref(), None)?;
                            }
                        }
                        Some(Event::Alias { .. }) => {
                            self.out.write_all(b" ")?;
                            self.at_line_start = false;
                            self.write_alias()?;
                        }
                        _ => {
                            self.advance();
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn write_block_sequence(&mut self, sequence_props: &Properties<'input>) -> io::Result<()> {
        self.advance(); // consume SequenceStart
        let base_indent = self.indent;
        if !sequence_props.is_empty() {
            if !self.at_line_start {
                self.out.write_all(b"\n")?;
            }
            self.write_indent()?;
            self.write_properties(sequence_props)?;
            self.out.write_all(b"\n")?;
            self.at_line_start = true;
        }
        loop {
            self.tick("write_block_sequence")?;
            match self.peek().cloned() {
                Some(Event::SequenceEnd { .. }) | None => {
                    if matches!(self.peek(), Some(Event::SequenceEnd { .. })) {
                        self.advance();
                    }
                    self.indent = base_indent;
                    break;
                }
                _ => {
                    if !self.at_line_start {
                        self.out.write_all(b"\n")?;
                        self.at_line_start = true;
                    }
                    self.write_indent()?;
                    self.out.write_all(b"- ")?;
                    self.at_line_start = false;
                    match self.peek().cloned() {
                        Some(Event::MappingStart {
                            style: map_style,
                            properties: map_props,
                            ..
                        }) => {
                            self.indent = base_indent + 2;
                            self.write_mapping(map_style, map_props.as_deref())?;
                            // Restore indentation for the next sequence item.
                            self.indent = base_indent;
                        }
                        Some(Event::SequenceStart {
                            style: seq_style,
                            properties: seq_props,
                            ..
                        }) => {
                            self.indent = base_indent + 2;
                            self.write_sequence(seq_style, seq_props.as_deref())?;
                            self.indent = base_indent;
                        }
                        _ => {
                            self.write_node()?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn write_flow_sequence(&mut self, sequence_props: &Properties<'input>) -> io::Result<()> {
        self.advance(); // consume SequenceStart
        if self.at_line_start {
            self.write_indent()?;
        }
        if !sequence_props.is_empty() {
            self.write_properties(sequence_props)?;
            self.out.write_all(b" ")?;
        }
        self.out.write_all(b"[")?;
        self.at_line_start = false;
        let mut first = true;
        loop {
            self.tick("write_flow_sequence")?;
            match self.peek().cloned() {
                Some(Event::SequenceEnd { .. }) | None => {
                    if matches!(self.peek(), Some(Event::SequenceEnd { .. })) {
                        self.advance();
                    }
                    break;
                }
                _ => {
                    if !first {
                        self.out.write_all(b", ")?;
                    }
                    first = false;
                    self.write_node()?;
                }
            }
        }
        self.out.write_all(b"]")?;
        self.at_line_start = false;
        Ok(())
    }

    fn write_flow_mapping(&mut self, mapping_props: &Properties<'input>) -> io::Result<()> {
        self.advance(); // consume MappingStart
        if self.at_line_start {
            self.write_indent()?;
        }
        if !mapping_props.is_empty() {
            self.write_properties(mapping_props)?;
            self.out.write_all(b" ")?;
        }
        self.out.write_all(b"{")?;
        self.at_line_start = false;
        let mut first = true;
        loop {
            self.tick("write_flow_mapping")?;
            match self.peek().cloned() {
                Some(Event::MappingEnd { .. }) | None => {
                    if matches!(self.peek(), Some(Event::MappingEnd { .. })) {
                        self.advance();
                    }
                    break;
                }
                _ => {
                    if first {
                        // For the first entry in a non-empty flow mapping,
                        // emit a space after the opening `{` so that keys
                        // like `&a [a, &b b]` in tests such as X38W roundtrip
                        // cleanly and are tokenised correctly by the emitter.
                        self.out.write_all(b" ")?;
                    } else {
                        self.out.write_all(b", ")?;
                    }
                    first = false;
                    // Key
                    if let Some(Event::Alias { .. }) = self.peek().cloned() {
                        // Alias used as a flow mapping key; print it and
                        // surround the ':' with spaces to avoid the
                        // ambiguous `*a:` form that the emitter can
                        // mis-tokenise.
                        self.write_alias()?;
                        self.out.write_all(b" : ")?;
                    } else {
                        self.write_node()?;
                        self.out.write_all(b": ")?;
                    }
                    // Value
                    self.write_node()?;
                }
            }
        }
        self.out.write_all(b"}")?;
        self.at_line_start = false;
        Ok(())
    }

    fn write_scalar(
        &mut self,
        style: ScalarStyle,
        properties: Option<&Properties<'input>>,
        suffix: Option<char>,
    ) -> io::Result<()> {
        let scalar_props = Self::properties_or_empty(properties);
        if let Some(Event::Scalar { value, .. }) = self.peek().cloned() {
            self.advance();
            let mut buf = String::new();
            if !scalar_props.is_empty() {
                // Anchors and tags precede the scalar.
                // We build them into `buf` first and add a separating space.
                // This is only used in flow style or inline block contexts.
                // For block mappings/sequences, caller emits properties on their own line.
                // For simplicity we only format global tags as `!<tag>`.
                if let Some(anchor) = &scalar_props.anchor {
                    buf.push('&');
                    buf.push_str(anchor.value.as_ref());
                    buf.push(' ');
                }
                if let Some(tag) = &scalar_props.tag {
                    let tag_value = tag.value.as_ref();
                    if tag_value.starts_with('!') {
                        buf.push_str(tag_value);
                    } else {
                        buf.push_str("!<");
                        buf.push_str(tag_value);
                        buf.push('>');
                    }
                    buf.push(' ');
                }
            }
            // Choose an effective style for serialization.
            //
            // We prioritise preserving the scalar *value* and allow the writer
            // to choose whatever presentation is most convenient:
            // - Any scalar whose value contains `\n` is rendered as a
            //   double-quoted string with `\n` escapes. This avoids the
            //   subtleties of block/quoted folding semantics while still
            //   roundtripping the exact string.
            // - Block scalar styles (Literal/Folded) that reach this helper are
            //   also normalised to double-quoted.
            // - Other styles (plain, single-quoted, already double-quoted) are
            //   preserved for single-line values.
            let render_style = if value.contains('\n') {
                ScalarStyle::DoubleQuoted
            } else {
                match style {
                    ScalarStyle::Literal | ScalarStyle::Folded => ScalarStyle::DoubleQuoted,
                    other => other,
                }
            };
            match render_style {
                ScalarStyle::Plain => {
                    buf.push_str(&value);
                }
                ScalarStyle::DoubleQuoted | ScalarStyle::Literal | ScalarStyle::Folded => {
                    // For Literal/Folded we normalize to double-quoted to make
                    // folding/chomping semantics explicit via escapes.
                    buf.push('"');
                    for ch in value.chars() {
                        match ch {
                            '\\' => buf.push_str("\\\\"),
                            '"' => buf.push_str("\\\""),
                            '\n' => buf.push_str("\\n"),
                            '\r' => buf.push_str("\\r"),
                            '\t' => buf.push_str("\\t"),
                            _ => buf.push(ch),
                        }
                    }
                    buf.push('"');
                }
                ScalarStyle::SingleQuoted => {
                    // Generic single-quoted scalar: we keep the content on a
                    // single logical line, relying on the quoted-scalar parser to
                    // not alter it (except for doubling of internal quotes).
                    buf.push('\'');
                    for ch in value.chars() {
                        if ch == '\'' {
                            buf.push_str("''");
                        } else {
                            buf.push(ch);
                        }
                    }
                    buf.push('\'');
                }
            }
            if let Some(ch) = suffix {
                buf.push(ch);
            }
            self.out.write_all(buf.as_bytes())?;
            self.at_line_start = false;
        }
        Ok(())
    }

    fn write_alias(&mut self) -> io::Result<()> {
        if let Some(Event::Alias { name, .. }) = self.peek().cloned() {
            self.advance();
            let mut buf = String::new();
            buf.push('*');
            buf.push_str(&name);
            self.out.write_all(buf.as_bytes())?;
            self.at_line_start = false;
        }
        Ok(())
    }

    #[allow(clippy::indexing_slicing, reason = "chunk size handled")]
    fn write_indent(&mut self) -> io::Result<()> {
        let mut remaining = self.indent;
        while remaining >= INDENT_CHUNK.len() {
            self.out.write_all(&INDENT_CHUNK)?;
            remaining -= INDENT_CHUNK.len();
        }
        if remaining > 0 {
            self.out.write_all(&INDENT_CHUNK[..remaining])?;
        }
        self.at_line_start = false;
        Ok(())
    }

    fn write_properties(&mut self, properties: &Properties<'input>) -> io::Result<()> {
        if let Some(anchor) = &properties.anchor {
            self.out.write_all(b"&")?;
            self.out.write_all(anchor.value.as_ref().as_bytes())?;
            if properties.tag.is_some() {
                self.out.write_all(b" ")?;
            }
        }
        if let Some(tag) = &properties.tag {
            let tag_value = tag.value.as_ref();
            if tag_value.starts_with('!') {
                self.out.write_all(tag_value.as_bytes())?;
            } else {
                self.out.write_all(b"!<")?;
                self.out.write_all(tag_value.as_bytes())?;
                self.out.write_all(b">")?;
            }
        }
        Ok(())
    }

    fn peek(&self) -> Option<&Event<'input>> {
        let mut pos = self.pos;
        while matches!(self.events.get(pos), Some(Event::InvalidatePair { .. })) {
            pos += 1;
        }
        self.events.get(pos)
    }

    fn advance(&mut self) {
        while matches!(
            self.events.get(self.pos),
            Some(Event::InvalidatePair { .. })
        ) {
            self.pos += 1;
        }
        if self.pos < self.events.len() {
            self.pos += 1;
        }
        while matches!(
            self.events.get(self.pos),
            Some(Event::InvalidatePair { .. })
        ) {
            self.pos += 1;
        }
    }
}
