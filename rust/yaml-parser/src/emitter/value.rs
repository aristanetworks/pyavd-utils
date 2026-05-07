// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use crate::error::ErrorKind;
use crate::event::{Event, Properties, ScalarStyle};
use crate::lexer::{Token, TokenKind};
use crate::span::{IndentLevel, Span, usize_to_indent};

use super::states::{
    BlockMapPhase, BlockSeqPhase, EmitterProperties, FlowMapPhase, FlowSeqPhase, ParseState,
    ValueContext, ValueKind,
};
use super::{Emitter, MaybeEmptyScalarDecision, PendingAstWrap};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PercentDecodeError {
    InvalidEscape,
    InvalidUtf8,
}

/// Decode percent-encoded bytes into UTF-8 text.
///
/// YAML tag suffixes use URI percent-encoding, so multi-byte Unicode sequences
/// must be decoded as bytes and then validated as UTF-8.
///
/// Returns a borrowed `Cow` when the input contains no percent escapes.
#[allow(clippy::indexing_slicing, reason = "position tracked from input")]
fn percent_decode(input: &str) -> Result<Cow<'_, str>, PercentDecodeError> {
    let input_bytes = input.as_bytes();
    let Some(first_escape) = input_bytes.iter().position(|&byte| byte == b'%') else {
        return Ok(Cow::Borrowed(input));
    };

    let mut decoded = Vec::with_capacity(input.len());
    decoded.extend_from_slice(&input_bytes[..first_escape]);
    let mut idx = first_escape;

    while idx < input_bytes.len() {
        if input_bytes[idx] == b'%' {
            let Some((&hi_digit, &lo_digit)) =
                input_bytes.get(idx + 1).zip(input_bytes.get(idx + 2))
            else {
                return Err(PercentDecodeError::InvalidEscape);
            };
            let Some(hi) = decode_hex_digit(hi_digit) else {
                return Err(PercentDecodeError::InvalidEscape);
            };
            let Some(lo) = decode_hex_digit(lo_digit) else {
                return Err(PercentDecodeError::InvalidEscape);
            };

            decoded.push((hi << 4) | lo);
            idx += 3;
            continue;
        }

        let next_escape = input_bytes[idx..]
            .iter()
            .position(|&byte| byte == b'%')
            .map_or(input_bytes.len(), |offset| idx + offset);
        decoded.extend_from_slice(&input_bytes[idx..next_escape]);
        idx = next_escape;
    }

    String::from_utf8(decoded)
        .map(Cow::Owned)
        .map_err(|_ignored| PercentDecodeError::InvalidUtf8)
}

fn decode_hex_digit(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        b'A'..=b'F' => Some(byte - b'A' + 10),
        _ => None,
    }
}

impl<'input> Emitter<'input> {
    pub(super) fn in_sequence_entry_context(&self) -> bool {
        self.state_stack.last().is_some_and(|state| {
            matches!(
                state,
                ParseState::BlockSeq {
                    phase: BlockSeqPhase::BeforeEntryScan,
                    ..
                }
            )
        })
    }

    /// Decide whether collected properties at a dedented indent should result in
    /// an empty scalar value instead of "bridging" to a block collection.
    ///
    /// This encapsulates the bridging rules used by `parse_value`:
    /// - For sequence entries, never bridge: `- !!str\n-` means `!!str` applies
    ///   to an empty scalar and the second `-` is a sibling entry.
    /// - For mapping values, only bridge when the dedented content is at the
    ///   parent collection's indent and the next content is a block collection
    ///   indicator (`-` or `?`).
    ///
    /// Returns a `MaybeEmptyScalarDecision<'input>` describing whether to emit
    /// an empty scalar now or continue parsing this value with updated
    /// properties.
    pub(super) fn maybe_emit_empty_scalar_for_non_bridging_properties(
        &mut self,
        min_indent: IndentLevel,
        properties: EmitterProperties<'input>,
        initial_crossed_line: bool,
        property_indent: Option<IndentLevel>,
    ) -> MaybeEmptyScalarDecision<'input> {
        // No properties collected – nothing to decide.
        if properties.is_empty() {
            return MaybeEmptyScalarDecision::Continue { properties };
        }

        // Only care about the case where the next token is a LineStart at a
        // dedented level (< min_indent). Otherwise, we leave the properties
        // attached to the upcoming value.
        let Some((next_indent, _)) = self.peek_line_start() else {
            return MaybeEmptyScalarDecision::Continue { properties };
        };
        if next_indent >= min_indent {
            return MaybeEmptyScalarDecision::Continue { properties };
        }

        // Check if we're in a sequence entry context.
        let in_sequence_entry = self.in_sequence_entry_context();

        // Only the owning key's indent may bridge an empty value to a block
        // collection. More-dedented indicators belong to an ancestor context.
        let bridge_indent = min_indent.saturating_sub(1);

        // Check if properties were collected at an invalid (dedented) indent.
        // Properties are invalid if:
        // 1. We crossed a line BEFORE collecting them (`initial_crossed_line`)
        //    meaning they're on a separate line from the key
        // 2. AND the indent of that line is < min_indent
        // E.g., `seq:\n&anchor\n- a` where &anchor is at indent 0 < min_indent 1
        // But `seq: !!seq\n- a` is valid - the tag is inline with the key
        // (initial_crossed_line=false, prop_crossed_line=true means we crossed
        // AFTER collecting, not before).
        let properties_at_invalid_indent =
            initial_crossed_line && property_indent.is_some_and(|prop_ind| prop_ind < min_indent);

        // Even if not too dedented, we should NOT bridge to a sibling mapping key.
        // Look ahead past the `LineStart` token to see what follows.
        // Only bridge if the next content token is a block collection indicator (- or ?).
        let can_bridge = !in_sequence_entry
            && next_indent == bridge_indent
            && !properties_at_invalid_indent
            && matches!(
                self.peek_kind_nth(1),
                Some(TokenKind::BlockSeqIndicator | TokenKind::MappingKey)
            );

        if !can_bridge {
            // Can't bridge - emit empty scalar.
            // If properties were collected at an invalid indent (below min_indent),
            // drop them (e.g. `seq:\n&anchor\n- a` where &anchor is at indent 0
            // < min_indent 1).
            let event_properties = if properties_at_invalid_indent {
                EmitterProperties::default() // Drop properties collected at invalid indent
            } else {
                properties
            };
            return MaybeEmptyScalarDecision::EmitEmptyScalar {
                event: Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    properties: event_properties.into_event_box(),
                    span: self.current_span(),
                },
            };
        }

        // Bridging to block collection at valid dedent level.
        MaybeEmptyScalarDecision::Continue { properties }
    }

    /// Check if crossing to a lower indent after a line boundary indicates an
    /// empty value (the lower-indented content belongs to a parent context).
    ///
    /// In block context, if we're at an indent < `ctx.min_indent` after crossing
    /// a line, the value is empty. For example:
    ///
    /// ```yaml
    /// key:
    ///   value
    /// ```
    ///
    /// has `min_indent = 1` for the value. But in:
    ///
    /// ```yaml
    /// key:
    /// next:
    /// ```
    ///
    /// `next:` is at indent 0 < 1, so `key` has an empty value.
    ///
    /// EXCEPTION: Block collection indicators (-, ?, :) can appear at the key's
    /// indent level to start a collection as the value. E.g.:
    ///
    /// ```yaml
    /// key:
    /// - item
    /// ```
    ///
    /// Here `-` is at indent 0, same as `key`, but it's the value (a sequence).
    ///
    /// BUT NOT for sequence entries: a `-` at the sequence's indent is a
    /// sibling entry, not a nested value. E.g.:
    ///
    /// ```yaml
    /// - # Empty
    /// - value
    /// ```
    ///
    /// The second `-` is a sibling, so the first entry's value is empty.
    pub(super) fn maybe_emit_empty_due_to_lower_indent(
        &mut self,
        ctx: ValueContext,
        crossed_line_after_properties: bool,
        properties: &EmitterProperties<'input>,
    ) -> Option<Event<'input>> {
        let min_indent = ctx.min_indent;

        // Only lower-indent-based emptiness if we crossed a line.
        if !crossed_line_after_properties {
            return None;
        }

        // If we haven't actually dedented below the minimum indent, there is no
        // empty-value-to-parent decision to make. Early-return before any
        // lookahead to keep the common non-dedent paths cheap (especially for
        // block scalars and multi-line values that remain at the same indent).
        if self.current_indent >= min_indent {
            return None;
        }

        let in_sequence_entry = self.in_sequence_entry_context();

        let at_block_indicator = !in_sequence_entry
            && self.current_indent == min_indent.saturating_sub(1)
            && matches!(
                self.peek_kind(),
                Some(TokenKind::BlockSeqIndicator | TokenKind::MappingKey | TokenKind::Colon)
            );

        if self.current_indent < min_indent && !at_block_indicator {
            return Some(Event::Scalar {
                style: ScalarStyle::Plain,
                value: Cow::Borrowed(""),
                properties: properties.clone().into_event_box(),
                span: self.current_span(),
            });
        }

        None
    }

    /// Decide whether properties collected before a colon in a null-key mapping
    /// (`: value`) belong to the implicit null key or the mapping itself.
    ///
    /// This encapsulates the `props_for_key` logic used for cases like:
    /// - `-\n  !!null : a` → properties belong to the key
    /// - `!!null : a` (at root) → properties belong to the key
    pub(super) fn properties_belong_to_null_key(
        &self,
        prop_crossed_line: bool,
        properties: &EmitterProperties<'input>,
    ) -> bool {
        !prop_crossed_line
            && (properties.has_anchor() || properties.has_tag())
            && self.peek_kind() == Some(TokenKind::Colon)
    }

    /// Common helper for creating a block mapping from a scalar key that has
    /// already been parsed.
    ///
    /// This sets up the `BlockMap` continuation state, queues the already
    /// parsed key event for the next `next()` call, and returns the
    /// `MappingStart` event. The `start_span` is the span of the indicator that
    /// triggered implicit mapping detection (used as `start_span` for
    /// `BlockMap`), while the returned event's span is the full key span.
    pub(super) fn build_block_mapping_from_scalar_key(
        &mut self,
        map_indent: IndentLevel,
        start_span: Span,
        map_properties: EmitterProperties<'input>,
        key_event: Event<'input>,
        content_column: Option<IndentLevel>,
    ) -> Event<'input> {
        if let Event::Scalar {
            value,
            properties,
            span: k_span,
            style,
        } = key_event
        {
            let (k_anchor, k_tag) = properties.map_or((None, None), |event_props| {
                let Properties { anchor, tag } = *event_props;
                (anchor, tag)
            });

            // Check for multiline implicit key error.
            self.check_multiline_implicit_key(k_span);

            // Compute key_end_column: key started at content_column, span gives width.
            let key_end_column = content_column
                .map(|col| col + usize_to_indent(k_span.end_usize() - k_span.start_usize()));

            // Stack setup: emit the key first, then parse the value after colon.
            self.state_stack.push(ParseState::BlockMap {
                indent: map_indent,
                phase: BlockMapPhase::AfterKey {
                    is_implicit_scalar_key: true,
                    key_end_column,
                },
                start_span,
                properties: EmitterProperties::default(),
            });
            self.set_pending_ast_wrap(PendingAstWrap::MappingPair {
                pair_start: start_span.start,
            });
            self.set_pending_event(Event::Scalar {
                style,
                value,
                properties: EmitterProperties::from_parts(k_anchor, k_tag).into_event_box(),
                span: k_span,
            });
            // Push indent level for orphan indent detection.
            self.push_indent(map_indent);

            Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties: map_properties.into_event_box(),
                span: k_span, // Use full key span for MappingStart
            }
        } else {
            // Fallback - shouldn't happen, but keep stack consistent.
            self.push_indent(map_indent);
            Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties: map_properties.into_event_box(),
                span: start_span,
            }
        }
    }

    /// Handle invalid indentation after crossing a line boundary at the start of a value.
    /// When we cross a line and land at `indent < min_indent`, we may need to report
    /// `InvalidIndentation` and/or `OrphanedProperties` errors for properties or
    /// scalar-like content at that position.
    pub(super) fn handle_invalid_indent_after_line_cross(
        &mut self,
        min_indent: IndentLevel,
        initial_crossed_line: bool,
    ) {
        if initial_crossed_line && self.current_indent < min_indent {
            // Check if there's content (properties or values) at this invalid indent
            if let Some(kind) = self.peek_kind() {
                let span = self.current_span();
                if matches!(kind, TokenKind::Anchor | TokenKind::Tag) {
                    // Properties at invalid indent get both InvalidIndentation and OrphanedProperties
                    self.error(ErrorKind::InvalidIndentation, span);
                    self.error(ErrorKind::OrphanedProperties, span);
                } else if matches!(kind, TokenKind::Plain | TokenKind::StringStart) {
                    // Don't report InvalidIndentation if this is a mapping key (followed by colon).
                    // E.g., `: # comment\n"key":` - the "key": is a new mapping entry, not a value.
                    let is_mapping_key = if kind == TokenKind::Plain {
                        self.current_plain_terminated_by_mapping_value_indicator()
                    } else {
                        self.with_lookahead(50, |window| {
                            Self::scan_find_string_end_from(&window, 1, 50)
                                .map(|end| Self::scan_skip_inline_whitespace_from(&window, end + 1))
                                .is_some_and(|offset| window.kind(offset) == Some(TokenKind::Colon))
                        })
                    };

                    if !is_mapping_key {
                        self.error(ErrorKind::InvalidIndentation, span);
                    }
                }
            }
        }
    }

    /// Return true if a block sequence at the given `indent` is at the
    /// root level. Root-level sequences have `indent_stack == [0, 0]`
    /// (base + sequence level).
    pub(super) fn is_root_level_sequence(&self, indent: IndentLevel) -> bool {
        indent == 0 && self.indent_stack.len() == 2
    }

    /// Report orphaned properties when property collection crossed a line boundary and
    /// landed at an invalid indent level for the current value.
    pub(super) fn report_orphaned_properties_after_invalid_indent(
        &mut self,
        min_indent: IndentLevel,
        prop_crossed_line: bool,
    ) {
        // Check for orphaned properties: if we crossed a line boundary but stopped because
        // of invalid indent, and the next content is a property, report OrphanedProperties.
        // This handles cases like `key: &x\n!!map\n  a: b` where !!map is at invalid indent.
        if prop_crossed_line
            && let Some((indent, _)) = self.peek_line_start()
            && indent < min_indent
        {
            // Look for property tokens after the LineStart
            let mut lookahead = 1;
            while matches!(
                self.peek_kind_nth(lookahead),
                Some(TokenKind::Anchor | TokenKind::Tag)
            ) {
                let span = self
                    .peek_nth_with(lookahead, |_, span| span)
                    .unwrap_or_else(|| self.current_span());
                self.error(ErrorKind::OrphanedProperties, span);
                lookahead += 1;
                // Skip whitespace between properties
                while matches!(
                    self.peek_kind_nth(lookahead),
                    Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs)
                ) {
                    lookahead += 1;
                }
            }
        }
    }

    /// Handle a flow sequence start token as a value, including the case where
    /// the flow sequence acts as a complex mapping key in block context.
    /// Handle a flow collection start token (`[` or `{`) as a value, including
    /// the case where the flow collection acts as a complex mapping key in
    /// block context.
    pub(super) fn process_flow_collection_value_state(
        &mut self,
        is_map: bool,
        span: Span,
        properties: EmitterProperties<'input>,
        kind: ValueKind,
        content_column: Option<IndentLevel>,
    ) -> Event<'input> {
        let explicit_key = matches!(kind, ValueKind::ExplicitKey);
        // In block context, check if this flow collection is a complex key.
        debug_assert_eq!(
            self.peek_kind(),
            Some(if is_map {
                TokenKind::FlowMapStart
            } else {
                TokenKind::FlowSeqStart
            })
        );
        let is_complex_key =
            self.flow_depth == 0 && self.current_flow_collection_is_complex_key(explicit_key);
        if is_complex_key {
            // This is a block mapping with a flow collection as key.
            let map_indent = self.current_indent;

            let _ = self.take_current();
            self.enter_flow_collection(content_column);

            // Push BlockMap state for after the key.
            self.state_stack.push(ParseState::BlockMap {
                indent: map_indent,
                phase: BlockMapPhase::AfterKey {
                    is_implicit_scalar_key: false, // Flow collection key, not plain scalar
                    key_end_column: None,
                },
                start_span: span,
                properties: EmitterProperties::default(),
            });

            if is_map {
                self.state_stack.push(ParseState::FlowMap {
                    phase: FlowMapPhase::BeforeKey,
                    start_span: span,
                });
            } else {
                self.state_stack.push(ParseState::FlowSeq {
                    phase: FlowSeqPhase::BeforeEntry,
                    start_span: span,
                });
            }
            self.set_pending_event(if is_map {
                Event::MappingStart {
                    style: crate::event::CollectionStyle::Flow,
                    properties: None,
                    span,
                }
            } else {
                Event::SequenceStart {
                    style: crate::event::CollectionStyle::Flow,
                    properties: None,
                    span,
                }
            });

            // Push indent level for orphan indent detection.
            self.push_indent(map_indent);
            // Emit MappingStart with the properties.
            return Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties: properties.into_event_box(),
                span,
            };
        }

        // Not a complex key: treat as regular flow collection value.
        let _ = self.take_current();
        self.enter_flow_collection(content_column);
        if is_map {
            self.state_stack.push(ParseState::FlowMap {
                phase: FlowMapPhase::BeforeKey,
                start_span: span,
            });
            Event::MappingStart {
                style: crate::event::CollectionStyle::Flow,
                properties: properties.into_event_box(),
                span,
            }
        } else {
            self.state_stack.push(ParseState::FlowSeq {
                phase: FlowSeqPhase::BeforeEntry,
                start_span: span,
            });
            Event::SequenceStart {
                style: crate::event::CollectionStyle::Flow,
                properties: properties.into_event_box(),
                span,
            }
        }
    }

    /// Handle an alias token as a value, including error reporting and the
    /// special case where the alias acts as a mapping key.
    ///
    /// This logic is driven from the `AliasValue` parse state, which
    /// centralises how aliases behave in different contexts.
    pub(super) fn process_alias_value_state(
        &mut self,
        alias_name: Cow<'input, str>,
        alias_span: Span,
        properties: EmitterProperties<'input>,
        crossed_line_boundary: bool,
    ) -> Event<'input> {
        // Advance past the alias token and skip any immediate whitespace.
        let _ = self.take_current();
        self.skip_ws();

        // Error: Properties (anchor/tag) on alias are invalid when on the same line.
        if !properties.is_empty() && !crossed_line_boundary {
            self.error(ErrorKind::PropertiesOnAlias, alias_span);
        }

        // Error: Undefined alias name.
        if !self.anchors.contains(alias_name.as_ref()) {
            self.error(ErrorKind::UndefinedAlias, alias_span);
        }

        // Check if alias is a mapping key (followed by colon) after crossing line boundary.
        // This can create a nested mapping even without outer anchor/tag.
        if crossed_line_boundary && self.peek_kind() == Some(TokenKind::Colon) {
            let map_indent = self.current_indent;

            // Push BlockMap state in AfterKey phase (we already have the key).
            self.state_stack.push(ParseState::BlockMap {
                indent: map_indent,
                phase: BlockMapPhase::AfterKey {
                    is_implicit_scalar_key: false, // Alias, not a plain scalar
                    key_end_column: None,
                },
                start_span: alias_span,
                properties: EmitterProperties::default(),
            });
            self.set_pending_event(Event::Alias {
                name: alias_name,
                span: alias_span,
            });

            // Push indent level for orphan indent detection.
            self.push_indent(map_indent);
            // Emit MappingStart with any outer anchor/tag.
            return Event::MappingStart {
                style: crate::event::CollectionStyle::Block,
                properties: properties.into_event_box(),
                span: alias_span,
            };
        }

        // Not a mapping key: emit the alias as a simple value.
        Event::Alias {
            name: alias_name,
            span: alias_span,
        }
    }

    /// Parse any value. Returns the first event for this value.
    ///
    /// Dispatch a value after properties have been collected and
    ///
    /// This function hosts the main dispatch logic that was previously the
    /// second half of `parse_value`, including:
    /// - Deciding whether properties at a dedented indent should bridge to a
    ///   block collection or emit an empty scalar.
    /// - Handling dedented empty values.
    /// - Dispatching to scalars, block/flow collections, aliases, and
    ///   additional property handling.
    #[allow(clippy::too_many_lines, reason = "Complex value dispatch logic")]
    pub(super) fn process_value_dispatch_state(
        &mut self,
        ctx: ValueContext,
        mut properties: EmitterProperties<'input>,
        initial_crossed_line: bool,
        prop_crossed_line: bool,
        property_indent: Option<IndentLevel>,
    ) -> Option<Event<'input>> {
        let min_indent = ctx.min_indent;

        // Decide whether collected properties at a dedented indent should stay attached
        // to this value or result in an empty scalar before a parent context.
        // This is only relevant when we actually have properties, which is encoded
        // by `property_indent` being `Some(..)`. Skipping the helper entirely for
        // the common "no properties" case avoids extra lookahead work on the hot
        // plain-scalar path.
        if property_indent.is_some() {
            match self.maybe_emit_empty_scalar_for_non_bridging_properties(
                min_indent,
                properties,
                initial_crossed_line,
                property_indent,
            ) {
                MaybeEmptyScalarDecision::EmitEmptyScalar { event } => {
                    return Some(event);
                }
                MaybeEmptyScalarDecision::Continue {
                    properties: new_props,
                } => {
                    properties = new_props;
                }
            }
        }

        self.state_stack.push(ParseState::ValueDispatchToken {
            ctx,
            properties,
            initial_crossed_line,
            prop_crossed_line,
        });
        None
    }

    #[allow(
        clippy::too_many_lines,
        reason = "value dispatch stays inline to keep the hot state-machine flow easy to follow"
    )]
    pub(super) fn process_value_after_properties(
        &mut self,
        mut ctx: ValueContext,
        properties: EmitterProperties<'input>,
        initial_crossed_line: bool,
        prop_crossed_line: bool,
    ) -> Option<Event<'input>> {
        let min_indent = ctx.min_indent;
        let is_implicit_key = matches!(ctx.kind, ValueKind::ImplicitKey);
        let mut allow_implicit_mapping = ctx.allow_implicit_mapping;

        let mut crossed_line_after_properties = initial_crossed_line || prop_crossed_line;
        allow_implicit_mapping = allow_implicit_mapping || crossed_line_after_properties;

        // Skip any more whitespace/newlines after properties and record if we
        // cross additional line boundaries. UPDATE content_column.
        let (additional_crossed, ws_width) = if self.current_token_starts_trivia_run() {
            self.skip_ws_and_newlines_tracked()
        } else {
            (false, 0)
        };
        crossed_line_after_properties |= additional_crossed;
        if additional_crossed {
            ctx.content_column = Some(self.current_indent + ws_width);
        } else if ws_width > 0 {
            ctx.content_column = Some(ctx.content_column.map_or(ws_width, |col| col + ws_width));
        }

        // Lower-indent empty-value check. This is only meaningful when we've
        // actually crossed a line boundary; avoid calling the helper at all on
        // the very common single-line value paths.
        if crossed_line_after_properties
            && let Some(event) = self.maybe_emit_empty_due_to_lower_indent(
                ctx,
                crossed_line_after_properties,
                &properties,
            )
        {
            return Some(event);
        }

        // Dispatch based on current token kind. Only fetch payload-bearing
        // tokens when a branch actually needs them.
        match self.peek_kind() {
            None => {
                // EOF - emit empty value / null
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    properties: properties.into_event_box(),
                    span: self.current_span(),
                })
            }

            Some(TokenKind::DocEnd | TokenKind::DocStart) => {
                let span = self.current_span();
                if self.flow_depth > 0 {
                    // Document markers inside flow context are invalid.
                    // Report error and continue - the flow state machine will handle them.
                    // Don't emit a null here; let the caller re-dispatch.
                    self.error(ErrorKind::DocumentMarkerInFlow, span);
                    let _ = self.take_current();
                    // Re-enter Value state to parse the actual value
                    self.state_stack.push(ParseState::Value {
                        ctx: ValueContext {
                            min_indent,
                            content_column: None,
                            kind: ctx.kind,
                            allow_implicit_mapping,
                            prior_crossed_line: !properties.is_empty(),
                        },
                        properties,
                    });
                    None
                } else {
                    // Block context - document marker ends the value, emit null
                    Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        properties: properties.into_event_box(),
                        span: self.current_span(),
                    })
                }
            }

            Some(TokenKind::FlowSeqEnd | TokenKind::FlowMapEnd) if self.flow_depth == 0 => {
                let span = self.current_span();
                self.error(ErrorKind::UnmatchedBracket, span);
                let _ = self.take_current();
                self.state_stack.push(ParseState::Value {
                    ctx: ValueContext {
                        min_indent,
                        content_column: ctx.content_column,
                        kind: ctx.kind,
                        allow_implicit_mapping,
                        prior_crossed_line: !properties.is_empty(),
                    },
                    properties,
                });
                None
            }

            Some(TokenKind::Alias) => {
                // Defer alias handling to the `AliasValue` state so that
                // complex-key behaviour is driven by the state machine.
                let Some((alias_name, alias_span)) = self
                    .peek_with(|tok, span| match tok {
                        Token::Alias(name) => Some((Cow::Borrowed(*name), span)),
                        _ => None,
                    })
                    .flatten()
                else {
                    debug_assert!(false, "expected Alias token");
                    return Some(self.emit_null());
                };
                self.state_stack.push(ParseState::AliasValue {
                    name: alias_name,
                    span: alias_span,
                    properties,
                    crossed_line_after_properties,
                });
                None
            }

            Some(TokenKind::BlockSeqIndicator) => {
                let span = self.current_span();

                // YAML spec: Anchors/tags on the same line as a block sequence indicator
                // are ambiguous and disallowed. They must be on a separate line.
                // E.g., `&anchor - item` is invalid, but `&anchor\n- item` is valid.
                //
                // Check if properties are on the same line as the block indicator.
                // We can't rely on crossed_line_boundary because it may be reset when
                // we cross a line boundary during value scanning.
                // Properties are on the same line as `-` iff no LineStart was consumed
                // at any point: before properties (initial), during collection (prop),
                // or after collection (additional).
                let properties_on_same_line = !properties.is_empty()
                    && !initial_crossed_line
                    && !prop_crossed_line
                    && !additional_crossed;

                if properties_on_same_line {
                    // Properties on same line as block sequence indicator - error
                    self.error(ErrorKind::ContentOnSameLine, span);
                    // Continue parsing to provide better error recovery
                }

                // content_column has been updated through each state transition.
                let seq_indent = ctx.content_column.unwrap_or(self.current_indent);
                // Emit the collection start immediately and leave the entry scan
                // as the next parse-state step.
                self.push_indent(seq_indent);
                self.current_indent = seq_indent;
                self.state_stack.push(ParseState::BlockSeq {
                    indent: seq_indent,
                    phase: BlockSeqPhase::BeforeEntryScan,
                    start_span: span,
                    properties: EmitterProperties::default(),
                });
                Some(Event::SequenceStart {
                    style: crate::event::CollectionStyle::Block,
                    properties: properties.into_event_box(),
                    span,
                })
            }

            Some(TokenKind::MappingKey | TokenKind::Colon) => {
                // In flow context, a tag/anchor followed by colon means an empty tagged/anchored
                // scalar as the key - do NOT start a block mapping inside flow context.
                // E.g., `{ !!str : bar }` - the `!!str` is an empty key with tag
                if self.flow_depth > 0 {
                    return Some(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        properties: properties.into_event_box(),
                        span: self.current_span(),
                    });
                }

                let span = self.current_span();
                // content_column tracks the token position through state transitions.
                // For compact notation (same line as parent indicator) it gives the
                // actual column; for regular notation it equals current_indent.
                let map_indent = ctx.content_column.unwrap_or(self.current_indent);

                // Determine if properties belong to the mapping or the first key.
                // If we crossed a line boundary BEFORE the properties (initial_crossed_line),
                // AND there was no line crossing AFTER the properties (prop_crossed_line=false),
                // AND we see Colon (not MappingKey), then the properties are for the
                // implicit null key, not the mapping.
                //
                // Examples:
                // - `!!map\n  key: value` → initial_crossed_line=false, prop_crossed_line=true
                //   → tag belongs to mapping (it's on a line before the mapping content)
                // - `-\n  !!null : a` → initial_crossed_line=true, prop_crossed_line=false
                //   → tag belongs to the key (it's on the same line as the colon)
                // - `!!null : a` (at root) → initial_crossed_line=false, prop_crossed_line=false
                //   → tag belongs to the key
                let props_for_key =
                    self.properties_belong_to_null_key(prop_crossed_line, &properties);

                if props_for_key {
                    // Properties belong to the implicit null key
                    // Emit MappingStart with no properties, then emit null scalar with properties
                    // Use current_indent for mapping indent (the line's indent level),
                    // not the colon's column position
                    self.state_stack.push(ParseState::BlockMap {
                        indent: self.current_indent,
                        phase: BlockMapPhase::AfterKey {
                            is_implicit_scalar_key: false, // Null key with properties, not plain scalar
                            key_end_column: None,
                        },
                        start_span: span,
                        properties: EmitterProperties::default(),
                    });
                    self.set_pending_event(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        properties: properties.into_event_box(),
                        span,
                    });
                    // Push indent level for orphan indent detection
                    self.push_indent(self.current_indent);
                    Some(Event::MappingStart {
                        style: crate::event::CollectionStyle::Block,
                        properties: None,
                        span,
                    })
                } else {
                    // Properties belong to the mapping.
                    // Emit the mapping start immediately and resume through the
                    // regular block-mapping phases on the next turn.
                    self.push_indent(map_indent);
                    self.crossed_line_boundary = false;
                    self.state_stack.push(ParseState::BlockMap {
                        indent: map_indent,
                        phase: BlockMapPhase::BeforeKeyScan {
                            require_line_boundary: false,
                            crossed_line: false,
                        },
                        start_span: span,
                        properties: EmitterProperties::default(),
                    });
                    Some(Event::MappingStart {
                        style: crate::event::CollectionStyle::Block,
                        properties: properties.into_event_box(),
                        span,
                    })
                }
            }

            Some(TokenKind::FlowSeqStart) => {
                let span = self.current_span();
                self.state_stack.push(ParseState::FlowCollectionValue {
                    is_map: false,
                    span,
                    properties,
                    kind: ctx.kind,
                    content_column: ctx.content_column,
                });
                None
            }

            Some(TokenKind::FlowMapStart) => {
                let span = self.current_span();
                self.state_stack.push(ParseState::FlowCollectionValue {
                    is_map: true,
                    span,
                    properties,
                    kind: ctx.kind,
                    content_column: ctx.content_column,
                });
                None
            }

            Some(TokenKind::LiteralBlockHeader | TokenKind::FoldedBlockHeader) => {
                Some(self.parse_block_scalar(min_indent, properties))
            }

            Some(TokenKind::Plain) => {
                if ctx.content_column == Some(0)
                    && self
                        .peek_with(|tok, span| match tok {
                            Token::Plain(text) if text.as_str().starts_with('%') => Some(span),
                            _ => None,
                        })
                        .flatten()
                        .is_some_and(|span| {
                            self.error(ErrorKind::InvalidDirective, span);
                            true
                        })
                {
                    return Some(self.parse_scalar_or_mapping(
                        min_indent,
                        properties,
                        is_implicit_key,
                        prop_crossed_line,
                        allow_implicit_mapping || prop_crossed_line || initial_crossed_line,
                        ctx.content_column,
                    ));
                }

                // Could be scalar or start of block mapping (unless we're parsing a key)
                // NOTE: Pass `prop_crossed_line` (not `crossed_line_boundary`) to determine
                // property ownership. What matters is whether there's a line boundary AFTER
                // the properties, not whether we crossed a line from the parent context.
                // - `&anchor\n  key:` → prop_crossed_line=true → anchor on MAPPING
                // - `\n  &anchor key:` → prop_crossed_line=false → anchor on KEY
                //
                // For allow_implicit_mapping: Only allow nested implicit mappings if:
                // - We explicitly allow them (not same-line after colon), OR
                // - We crossed a line boundary (properties or whitespace)
                // This prevents `a: b: c` from creating nested mappings.
                let effective_allow =
                    allow_implicit_mapping || prop_crossed_line || initial_crossed_line;
                Some(self.parse_scalar_or_mapping(
                    min_indent,
                    properties,
                    is_implicit_key,
                    prop_crossed_line,
                    effective_allow,
                    ctx.content_column,
                ))
            }

            Some(TokenKind::StringStart) => {
                // Could be scalar or start of block mapping (unless we're parsing a key)
                let effective_allow =
                    allow_implicit_mapping || prop_crossed_line || initial_crossed_line;
                Some(self.parse_scalar_or_mapping(
                    min_indent,
                    properties,
                    is_implicit_key,
                    prop_crossed_line,
                    effective_allow,
                    ctx.content_column,
                ))
            }

            Some(TokenKind::Anchor | TokenKind::Tag) => {
                // More properties after line boundary - indicates nested structure.
                // Properties may span multiple lines (e.g., `&anchor\n!!str\nvalue`).
                // Defer to the `AdditionalPropertiesValue` state so complex
                // key behaviour is driven by the state machine.
                self.state_stack
                    .push(ParseState::AdditionalPropertiesCollect {
                        ctx,
                        outer: properties,
                        inner: EmitterProperties::default(),
                        crossed_line_boundary: false,
                        consumed_width: 0,
                    });
                None
            }

            Some(_) => {
                // Default: emit null
                Some(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    properties: properties.into_event_box(),
                    span: self.current_span(),
                })
            }
        }
    }

    /// Check if we should continue collecting properties across a line boundary.
    ///
    /// We should continue if:
    /// - The next line has properties (anchor/tag)
    /// - AND those properties are NOT followed by an implicit mapping key
    ///
    /// An implicit mapping key is content followed by `:`, like `[...]:` or `key:`.
    /// When we see this pattern, the properties before the line boundary should go
    /// on the parent mapping, not merged with the key's properties.
    pub(super) fn should_continue_collecting_properties(&self) -> bool {
        let mut idx = 1; // Start after the LineStart at position 0

        // First, check if there are any properties on the next line.
        let mut found_property = false;
        loop {
            match self.peek_kind_nth(idx) {
                Some(TokenKind::Anchor | TokenKind::Tag) => {
                    found_property = true;
                    idx += 1;
                }
                Some(TokenKind::Whitespace | TokenKind::WhitespaceWithTabs) => {
                    idx += 1;
                }
                _ => break,
            }
            if idx > 20 {
                return false; // Safety limit
            }
        }

        if !found_property {
            // No property on the next line - don't continue collecting.
            return false;
        }

        match self.peek_kind_nth(idx) {
            Some(TokenKind::Plain) => {
                !self.plain_at_offset_terminated_by_mapping_value_indicator(idx)
            }
            Some(TokenKind::Alias) => !self.with_lookahead(idx + 8, |window| {
                let colon_idx = Self::scan_skip_inline_whitespace_from(&window, idx + 1);
                window.kind(colon_idx) == Some(TokenKind::Colon)
            }),
            Some(TokenKind::StringStart | TokenKind::FlowSeqStart | TokenKind::FlowMapStart) => {
                !self.is_implicit_key_at_offset_incremental(idx)
            }
            _ => true,
        }
    }

    fn is_implicit_key_at_offset_incremental(&self, start_idx: usize) -> bool {
        match self.peek_kind_nth(start_idx) {
            Some(TokenKind::StringStart) => {
                let mut idx = start_idx + 1;
                while idx <= start_idx + 200 {
                    match self.peek_kind_nth(idx) {
                        Some(TokenKind::StringEnd) => {
                            return self.with_lookahead(idx + 8, |window| {
                                let colon_idx =
                                    Self::scan_skip_inline_whitespace_from(&window, idx + 1);
                                window.kind(colon_idx) == Some(TokenKind::Colon)
                            });
                        }
                        Some(TokenKind::DocStart | TokenKind::DocEnd) | None => return false,
                        _ => idx += 1,
                    }
                }
                false
            }
            Some(TokenKind::FlowSeqStart) => self
                .flow_collection_at_offset_terminated_by_mapping_value_indicator(
                    start_idx,
                    TokenKind::FlowSeqStart,
                ),
            Some(TokenKind::FlowMapStart) => self
                .flow_collection_at_offset_terminated_by_mapping_value_indicator(
                    start_idx,
                    TokenKind::FlowMapStart,
                ),
            _ => false,
        }
    }

    fn flow_collection_at_offset_terminated_by_mapping_value_indicator(
        &self,
        start_idx: usize,
        start_kind: TokenKind,
    ) -> bool {
        let target_end = match start_kind {
            TokenKind::FlowSeqStart => TokenKind::FlowSeqEnd,
            TokenKind::FlowMapStart => TokenKind::FlowMapEnd,
            _ => return false,
        };

        let mut depth = 0;
        let mut idx = start_idx;
        while idx <= start_idx + 200 {
            match self.peek_kind_nth(idx) {
                Some(TokenKind::FlowSeqStart | TokenKind::FlowMapStart) => depth += 1,
                Some(TokenKind::FlowSeqEnd | TokenKind::FlowMapEnd) => {
                    depth -= 1;
                    if depth == 0 && self.peek_kind_nth(idx) == Some(target_end) {
                        return self.with_lookahead(idx + 8, |window| {
                            let colon_idx =
                                Self::scan_skip_inline_whitespace_from(&window, idx + 1);
                            window.kind(colon_idx) == Some(TokenKind::Colon)
                        });
                    }
                }
                Some(TokenKind::DocStart | TokenKind::DocEnd) | None => return false,
                _ => {}
            }
            idx += 1;
        }
        false
    }

    /// Expand a tag handle to its full form.
    ///
    /// The lexer transforms tags as follows:
    /// - `!!str` → `Tag("!!str")` (secondary handle)
    /// - `!name!suffix` → `Tag("!name!suffix")` (named handle)
    /// - `!<uri>` → `Tag("\0uri")` (verbatim, marked with NUL)
    /// - `!` alone → `Tag("!")` (non-specific)
    #[allow(clippy::too_many_lines, reason = "Tag expansion with many cases")]
    pub(super) fn expand_tag(&mut self, tag_cow: Cow<'input, str>, span: Span) -> Cow<'input, str> {
        fn join_prefix_and_suffix(prefix: &str, suffix: &str) -> String {
            let mut result = String::with_capacity(prefix.len() + suffix.len());
            result.push_str(prefix);
            result.push_str(suffix);
            result
        }

        // Verbatim tag: marked with leading '\0' by lexer - return as-is (without marker)
        if let Some(verbatim) = tag_cow.as_ref().strip_prefix('\0') {
            return Cow::Owned(String::from(verbatim));
        }

        // Now tags include the '!' prefix from the lexer
        // Empty tag (just `!`) - non-specific tag
        if tag_cow.as_ref() == "!" {
            // Can return the borrowed tag directly!
            return tag_cow;
        }

        // Secondary handle: !!type
        if let Some(suffix) = tag_cow.as_ref().strip_prefix("!!") {
            const TAG_PREFIX: &str = "tag:yaml.org,2002:";
            let prefix = self.tag_handles.get("!!").copied().unwrap_or(TAG_PREFIX);

            if let Ok(decoded_suffix) = percent_decode(suffix) {
                if let Cow::Borrowed(org_suffix) = decoded_suffix {
                    // When still borrowed nothing was escaped so check for well-known suffixes
                    if prefix == TAG_PREFIX {
                        match org_suffix {
                            "str" => return Cow::Borrowed("tag:yaml.org,2002:str"),
                            "seq" => return Cow::Borrowed("tag:yaml.org,2002:seq"),
                            "map" => return Cow::Borrowed("tag:yaml.org,2002:map"),
                            "int" => return Cow::Borrowed("tag:yaml.org,2002:int"),
                            "float" => return Cow::Borrowed("tag:yaml.org,2002:float"),
                            "bool" => return Cow::Borrowed("tag:yaml.org,2002:bool"),
                            "null" => return Cow::Borrowed("tag:yaml.org,2002:null"),
                            "timestamp" => return Cow::Borrowed("tag:yaml.org,2002:timestamp"),
                            _ => {}
                        }
                    }

                    return Cow::Owned(join_prefix_and_suffix(prefix, suffix));
                }

                return Cow::Owned(join_prefix_and_suffix(prefix, decoded_suffix.as_ref()));
            }
            return tag_cow;
        }

        // Named handle: !name!suffix
        #[allow(
            clippy::string_slice,
            reason = "Slicing at positions returned by find('!'), which are guaranteed UTF-8 boundaries"
        )]
        if let Some(first_bang) = tag_cow.as_ref().find('!')
            && let Some(second_bang) = tag_cow.as_ref()[first_bang + 1..].find('!')
        {
            let tag_str = tag_cow.as_ref();
            let second_bang_pos = first_bang + 1 + second_bang;
            let handle = &tag_str[0..=second_bang_pos]; // e.g., "!yaml!"
            let suffix = &tag_str[second_bang_pos + 1..];
            // Look up handle using as_str() since HashMap keys are &str
            if let Some(prefix) = self.tag_handles.get(handle) {
                if let Ok(decoded_suffix) = percent_decode(suffix) {
                    return Cow::Owned(join_prefix_and_suffix(prefix, decoded_suffix.as_ref()));
                }
                self.error(ErrorKind::InvalidTag, span);
                return tag_cow;
            }
            // Handle not declared - emit error and return unexpanded
            self.error(ErrorKind::UndefinedTagHandle, span);
            return tag_cow; // Return as-is
        }

        // Primary handle: !type
        if let Some(suffix) = tag_cow.as_ref().strip_prefix('!') {
            let prefix = self.tag_handles.get("!").copied().unwrap_or("!");
            if prefix == "!" && !suffix.contains('%') {
                return tag_cow;
            }
            if let Ok(decoded_suffix) = percent_decode(suffix) {
                return Cow::Owned(join_prefix_and_suffix(prefix, decoded_suffix.as_ref()));
            }
            self.error(ErrorKind::InvalidTag, span);
            return tag_cow;
        }

        // Shouldn't reach here, but return as-is
        tag_cow
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use super::{PercentDecodeError, percent_decode};

    #[test]
    fn percent_decode_leaves_unescaped_input_borrowable() {
        assert_eq!(percent_decode("tag-name"), Ok(Cow::Borrowed("tag-name")));
    }

    #[test]
    fn percent_decode_handles_ascii_escape() {
        assert_eq!(
            percent_decode("tag%21"),
            Ok(Cow::Owned(String::from("tag!")))
        );
    }

    #[test]
    fn percent_decode_handles_multibyte_utf8_escape() {
        assert_eq!(
            percent_decode("caf%C3%A9"),
            Ok(Cow::Owned(String::from("café")))
        );
    }

    #[test]
    fn percent_decode_handles_four_byte_utf8_escape() {
        assert_eq!(
            percent_decode("emoji-%F0%9F%98%80"),
            Ok(Cow::Owned(String::from("emoji-😀")))
        );
    }

    #[test]
    fn percent_decode_rejects_incomplete_escape() {
        assert_eq!(
            percent_decode("tag%"),
            Err(PercentDecodeError::InvalidEscape)
        );
    }

    #[test]
    fn percent_decode_rejects_non_hex_escape() {
        assert_eq!(
            percent_decode("tag%ZZ"),
            Err(PercentDecodeError::InvalidEscape)
        );
    }

    #[test]
    fn percent_decode_rejects_invalid_utf8_sequence() {
        assert_eq!(
            percent_decode("%C3%28"),
            Err(PercentDecodeError::InvalidUtf8)
        );
    }
}
