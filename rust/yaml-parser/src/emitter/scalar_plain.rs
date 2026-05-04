// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;
use std::ops::ControlFlow;

use super::Emitter;
use super::states::EmitterProperties;
use super::states::ParseState;
use crate::error::ErrorKind;
use crate::event::Event;
use crate::event::ScalarStyle;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::span::IndentLevel;
use crate::span::Span;

impl<'input> Emitter<'input> {
    pub(super) fn try_consume_plain_continuation(
        &mut self,
        content: &mut String,
        end_span: &mut Span,
        consecutive_newlines: &mut usize,
        _min_indent: IndentLevel,
    ) -> bool {
        match self.peek_kind() {
            Some(TokenKind::Plain) => {
                // Before consuming as continuation, check if it's a mapping key
                if self.current_plain_terminated_by_mapping_value_indicator() {
                    return false;
                }

                let Some((Token::Plain(next_plain), next_span)) = self.take_current() else {
                    debug_assert!(false, "peek_kind/plain desynchronized");
                    return false;
                };
                let next_text = next_plain.into_text();

                // Apply folding: single newline → space, multiple → n-1 newlines
                Self::append_folded_separator(content, *consecutive_newlines);
                content.push_str(&next_text);
                *end_span = next_span;
                *consecutive_newlines = 0;
                true
            }
            Some(TokenKind::LineStart) => {
                // Another newline, continue counting (caller will handle)
                true
            }
            // Handle BlockSeqIndicator inside the content area.
            // We only reach here when the line's indent >= min_indent (checked by
            // the caller). Any `-` on such a line is at column >= current_indent
            // >= min_indent, so it's always plain text, never a valid entry marker.
            // (Valid entry markers are at indent < min_indent, handled by
            // has_continuation_after_low_indent.)
            Some(TokenKind::BlockSeqIndicator) => {
                let span = self.current_span();
                let start_pos = span.start_usize();
                let (line_text, line_end) = self.consume_line_as_text(start_pos);

                if !line_text.is_empty() {
                    Self::append_folded_separator(content, *consecutive_newlines);
                    content.push_str(line_text);
                    *consecutive_newlines = 0;
                    *end_span = Span::from_usize_range(start_pos..line_end);
                }
                true
            }
            // Handle Anchor, Tag, Alias as plain text (continuation)
            // These look like special tokens but in plain scalar context they're just text.
            // IMPORTANT: If an Anchor/Tag is at the start of a line and followed by a
            // scalar+colon pattern, it's a mapping key, not continuation text.
            Some(TokenKind::Anchor | TokenKind::Tag | TokenKind::Alias) => {
                // Check if this anchor/tag starts a mapping key pattern
                // Look ahead: (Anchor|Tag)* (Plain|Quoted) Colon
                if self.is_anchor_tag_mapping_key() {
                    return false;
                }

                let span = self.current_span();
                let start_pos = span.start_usize();
                let (line_text, line_end) = self.consume_line_as_text(start_pos);

                if !line_text.is_empty() {
                    Self::append_folded_separator(content, *consecutive_newlines);
                    content.push_str(line_text);
                    *consecutive_newlines = 0;
                    *end_span = Span::from_usize_range(start_pos..line_end);
                }
                true
            }
            _ => false,
        }
    }

    /// Consume all tokens until `LineStart`, returning the text as a slice.
    /// Used for treating anchor/tag tokens as plain text in scalar continuations.
    #[allow(
        clippy::string_slice,
        reason = "Positions from tokens are UTF-8 boundaries"
    )]
    pub(super) fn consume_line_as_text(&mut self, start_pos: usize) -> (&'input str, usize) {
        let mut line_end = start_pos;
        while let Some(is_line_start) = self.peek_with(|tok, tok_span| {
            if matches!(tok, Token::LineStart(_)) {
                return ControlFlow::Break(());
            }
            ControlFlow::Continue(tok_span.end_usize())
        }) {
            let ControlFlow::Continue(end_usize) = is_line_start else {
                break;
            };
            line_end = end_usize;
            let _ = self.take_current();
        }
        let line_text = &self.input[start_pos..line_end];
        (line_text.trim_end(), line_end)
    }

    /// Append folded separator based on number of consecutive newlines.
    /// Single newline → space, multiple → n-1 newlines.
    pub(super) fn append_folded_separator(content: &mut String, consecutive_newlines: usize) {
        if consecutive_newlines == 1 {
            content.push(' ');
        } else {
            for _ in 1..consecutive_newlines {
                content.push('\n');
            }
        }
    }

    /// Check if there's a valid continuation after a low-indent line (empty line case).
    /// Looks ahead to see if there's a properly indented `LineStart` followed by content.
    pub(super) fn has_continuation_after_low_indent(&self, min_indent: IndentLevel) -> bool {
        self.with_lookahead(20, |window| {
            Self::scan_has_continuation_after_low_indent(&window, min_indent)
        })
    }

    /// Check if current Anchor/Tag starts a mapping key pattern.
    /// Pattern: (Anchor|Tag)+ (Whitespace|LineStart)* (Plain|Quoted) Whitespace* Colon
    /// This handles properties that span multiple lines, e.g.:
    /// ```yaml
    /// &m2
    /// key2: val2
    /// ```
    pub(super) fn is_anchor_tag_mapping_key(&self) -> bool {
        self.with_lookahead(50, |window| Self::scan_is_anchor_tag_mapping_key(&window))
    }

    pub(super) fn parse_scalar_or_mapping(
        &mut self,
        min_indent: IndentLevel,
        properties: EmitterProperties<'input>,
        is_implicit_key: bool,
        crossed_line_boundary: bool,
        allow_implicit_mapping: bool,
        content_column: Option<IndentLevel>,
    ) -> Event<'input> {
        // Check if this is an implicit mapping key (scalar followed by colon)
        // Skip this check if:
        // - we're already parsing an implicit key (avoid infinite recursion)
        // - we're in flow context (different rules apply)
        // - nested implicit mappings aren't allowed (same line as parent colon)
        let looks_like_implicit_key = !is_implicit_key
            && self.flow_depth == 0
            && allow_implicit_mapping
            && match self.peek_kind() {
                Some(TokenKind::Plain) => {
                    self.current_plain_terminated_by_mapping_value_indicator()
                }
                Some(TokenKind::StringStart | TokenKind::Alias) => self.is_implicit_key(),
                _ => false,
            };
        if looks_like_implicit_key {
            // This is a block mapping with an implicit key
            let span = self.current_span();

            // Determine where properties belong based on crossed_line_boundary:
            // - If crossed_line_boundary: properties belong to MAPPING, key has none
            // - Otherwise: properties belong to KEY (same line), mapping has none
            let (map_props, key_props) = if crossed_line_boundary {
                // Properties crossed a line boundary, so they belong to the mapping
                (properties, EmitterProperties::default())
            } else {
                // Same-line properties already flowed through ValueCollectProperties,
                // so there is nothing left to collect here.
                (EmitterProperties::default(), properties)
            };
            self.skip_ws();

            // Parse the key scalar with its properties
            let mut key_props = key_props;
            let key_event = self.parse_plain_scalar(key_props.take(), min_indent);

            // Determine mapping indent based on context:
            // - If there are properties (anchor or tag), use current_indent because
            //   the mapping's indent is the line's indent (e.g., `&a a: b` at root = indent 0)
            // - Otherwise (no properties, e.g., `- key: value`), use key's column position
            //   because that's where continuation lines need to align
            let has_properties = !map_props.is_empty()
                || matches!(
                    &key_event,
                    Event::Scalar {
                        properties: ev_props,
                        ..
                    } if ev_props
                        .as_ref()
                        .is_some_and(|event_props| {
                            event_props.anchor.is_some() || event_props.tag.is_some()
                        })
                );
            // content_column tracks where the key started on the line.
            // When there are properties or we crossed a line, use current_indent
            // (mapping is at line indent level). Otherwise use content_column
            // (mapping is at the key's column position).
            let map_indent = if crossed_line_boundary || has_properties {
                self.current_indent
            } else {
                content_column.unwrap_or(self.current_indent)
            };

            let mapping_start = self.build_block_mapping_from_scalar_key(
                map_indent,
                span,
                map_props,
                key_event,
                content_column,
            );
            return mapping_start;
        }

        // Not a mapping key, just parse as a scalar
        let result = self.parse_plain_scalar(properties, min_indent);
        // If this is a mapping key, check for multiline implicit key error
        if is_implicit_key && let Event::Scalar { span, .. } = &result {
            self.check_multiline_implicit_key(*span);
        }
        result
    }

    /// Parse a plain scalar (potentially multiline).
    ///
    /// Implements basic multiline plain scalar handling:
    /// - Single newline followed by content becomes a space
    /// - Multiple consecutive newlines preserve (n-1) newlines
    ///
    /// `min_indent` specifies the minimum indentation for continuation lines.
    /// Continuation lines must have indent >= `min_indent` to be considered part of the scalar.
    pub(super) fn parse_plain_scalar(
        &mut self,
        properties: EmitterProperties<'input>,
        min_indent: IndentLevel,
    ) -> Event<'input> {
        match self.peek_kind() {
            Some(TokenKind::Plain) => {
                let Some((Token::Plain(first_plain), span)) = self.take_current() else {
                    debug_assert!(false, "peek_kind/plain desynchronized");
                    return self.emit_null();
                };
                let first_meta = first_plain.meta();
                let first_line = first_plain.into_text();
                let start_span = span;

                // Check for reserved indicator `%` at column 0 starting a plain scalar.
                // Per YAML 1.2 spec production [22] c-indicator and [126] ns-plain-first,
                // `%` is a c-indicator and cannot start a plain scalar.

                if self.flow_depth > 0 {
                    if !first_meta.may_continue_on_next_line_in_flow {
                        return Event::Scalar {
                            style: ScalarStyle::Plain,
                            value: first_line,
                            properties: properties.into_event_box(),
                            span,
                        };
                    }

                    self.state_stack.push(ParseState::PlainScalarFlow {
                        first_line,
                        properties,
                        start_span,
                        end_span: span,
                        has_continuation: false,
                        content: None,
                    });
                    return self
                        .process_state_stack()
                        .unwrap_or_else(|| self.emit_null());
                }

                if let Some((next_indent, _)) = self.peek_line_start() {
                    if next_indent < min_indent {
                        match self.peek_kind_nth(1) {
                            Some(
                                TokenKind::BlockSeqIndicator
                                | TokenKind::MappingKey
                                | TokenKind::DocStart
                                | TokenKind::DocEnd,
                            )
                            | None => {
                                return Event::Scalar {
                                    style: ScalarStyle::Plain,
                                    value: first_line,
                                    properties: properties.into_event_box(),
                                    span,
                                };
                            }
                            _ if !self.has_continuation_after_low_indent(min_indent) => {
                                return Event::Scalar {
                                    style: ScalarStyle::Plain,
                                    value: first_line,
                                    properties: properties.into_event_box(),
                                    span,
                                };
                            }
                            _ => {}
                        }
                    }

                    self.state_stack.push(ParseState::PlainScalarBlock {
                        first_line,
                        properties,
                        start_span,
                        end_span: span,
                        min_indent,
                        consecutive_newlines: 0,
                        has_continuation: false,
                        content: None,
                    });
                    return self
                        .process_state_stack()
                        .unwrap_or_else(|| self.emit_null());
                }

                // Combine spans
                Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: first_line,
                    properties: properties.into_event_box(),
                    span,
                }
            }

            Some(TokenKind::StringStart) => {
                let Some(quote_style) = self
                    .peek_with(|tok, _| match tok {
                        Token::StringStart(quote_style) => Some(*quote_style),
                        _ => None,
                    })
                    .flatten()
                else {
                    debug_assert!(false, "peek_kind/string_start desynchronized");
                    return self.emit_null();
                };
                self.parse_quoted_scalar(properties, quote_style, min_indent)
            }

            _ => self.emit_null(),
        }
    }

    /// Continue parsing a block-context plain scalar after the first line has been consumed.
    #[allow(
        clippy::too_many_arguments,
        reason = "arguments mirror the resumable plain-scalar block state payload"
    )]
    pub(super) fn process_plain_scalar_block_state(
        &mut self,
        first_line: Cow<'input, str>,
        properties: EmitterProperties<'input>,
        start_span: Span,
        mut end_span: Span,
        min_indent: IndentLevel,
        mut consecutive_newlines: usize,
        mut has_continuation: bool,
        mut content: Option<String>,
    ) -> Event<'input> {
        if let Some((indent, line_span)) = self.peek_line_start() {
            if indent >= min_indent {
                let _ = self.take_current();
                consecutive_newlines += 1;

                self.skip_indent_tokens();

                let content_str = content.get_or_insert_with(|| first_line.clone().into_owned());

                if self.try_consume_plain_continuation(
                    content_str,
                    &mut end_span,
                    &mut consecutive_newlines,
                    min_indent,
                ) {
                    has_continuation = true;
                    self.state_stack.push(ParseState::PlainScalarBlock {
                        first_line,
                        properties,
                        start_span,
                        end_span,
                        min_indent,
                        consecutive_newlines,
                        has_continuation,
                        content,
                    });
                    return self
                        .process_state_stack()
                        .unwrap_or_else(|| self.emit_null());
                }

                if !self.is_valid_indent(indent) && self.has_content_at_orphan_level_from(1) {
                    self.error(
                        ErrorKind::InvalidIndentation,
                        Self::indented_line_error_span(line_span, indent),
                    );
                    self.skip_to_line_end();
                    self.skip_invalid_indented_recovery_lines(min_indent.saturating_sub(1));
                }
            } else if self.has_continuation_after_low_indent(min_indent) {
                let _ = self.take_current();
                consecutive_newlines += 1;
                self.state_stack.push(ParseState::PlainScalarBlock {
                    first_line,
                    properties,
                    start_span,
                    end_span,
                    min_indent,
                    consecutive_newlines,
                    has_continuation,
                    content,
                });
                return self
                    .process_state_stack()
                    .unwrap_or_else(|| self.emit_null());
            }
        }

        let final_span = Span::new(start_span.start..end_span.end);
        let value = if has_continuation {
            Cow::Owned(content.unwrap_or_else(|| first_line.into_owned()))
        } else {
            first_line
        };

        Event::Scalar {
            style: ScalarStyle::Plain,
            value,
            properties: properties.into_event_box(),
            span: final_span,
        }
    }

    /// Continue parsing a flow-context plain scalar after the first line has been consumed.
    pub(super) fn process_plain_scalar_flow_state(
        &mut self,
        first_line: Cow<'input, str>,
        properties: EmitterProperties<'input>,
        start_span: Span,
        mut end_span: Span,
        mut has_continuation: bool,
        mut content: Option<String>,
    ) -> Event<'input> {
        loop {
            let next_kind = loop {
                let kind = self.peek_kind();
                match kind {
                    Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs) => {
                        let _ = self.take_current();
                    }
                    _ => break kind,
                }
            };

            if next_kind == Some(TokenKind::LineStart)
                && self.peek_kind_nth(1) == Some(TokenKind::Plain)
            {
                has_continuation = true;
                let _ = self.take_current();
                let Some((Token::Plain(continuation_plain), next_span)) = self.take_current()
                else {
                    debug_assert!(false, "peek_kind_nth/plain desynchronized");
                    return self.emit_null();
                };
                let continuation = continuation_plain.into_text();
                let content_str = content.get_or_insert_with(|| first_line.clone().into_owned());
                content_str.push(' ');
                content_str.push_str(&continuation);
                end_span = next_span;
                continue;
            }

            let final_span = Span::new(start_span.start..end_span.end);
            let value = if has_continuation {
                Cow::Owned(content.unwrap_or_else(|| first_line.into_owned()))
            } else {
                first_line
            };

            return Event::Scalar {
                style: ScalarStyle::Plain,
                value,
                properties: properties.into_event_box(),
                span: final_span,
            };
        }
    }
}
