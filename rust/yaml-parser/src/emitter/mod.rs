// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML event emitter with error recovery.
//!
//! This module provides `Emitter`, a YAML parser that produces events
//! using an explicit state stack instead of recursion. It consumes
//! tokens from the unified lexer via a small cursor abstraction and
//! emits events via the `Iterator` interface.
//!
//! The `Emitter` is validated against the YAML Test Suite to ensure
//! correct event sequences for all inputs.
#![allow(
    clippy::multiple_inherent_impl,
    reason = "the emitter state machine is intentionally split across multiple files without changing the owning type"
)]

mod ast;
mod block;
mod cursor;
mod diagnostics;
mod document;
mod flow;
mod scalar_block;
mod scalar_plain;
mod scalar_quoted;
mod states;
mod structure;
mod tokens;
mod trivia;
mod value;

use std::borrow::Cow;
use std::collections::HashSet;

use crate::ast_event::AstEvent;
use crate::error::{ErrorKind, ParseError};
use crate::event::{Comment, Event, Property, ScalarStyle};
use crate::lexer::{Token, TokenKind};
use crate::span::{BytePosition, IndentLevel, Span, usize_to_indent};

use cursor::TokenCursor;
use states::{
    BlockMapPhase, BlockSeqPhase, DocState, EmitterProperties, FlowMapPhase, FlowSeqPhase,
    ParseState, ValueContext, ValueKind,
};

/// Result of deciding what to do with collected properties at a dedented
/// indent: either emit an empty scalar now, or keep the properties attached to
/// the upcoming value.
#[derive(Debug)]
enum MaybeEmptyScalarDecision<'input> {
    /// Do not emit an empty scalar; continue parsing this value with the
    /// (possibly updated) properties.
    Continue {
        properties: EmitterProperties<'input>,
    },
    /// Emit an empty scalar event using the given properties, and stop
    /// parsing the current value.
    EmitEmptyScalar { event: Event<'input> },
}

/// Line classification used when folding block scalars.
#[derive(Clone, Copy, PartialEq, Debug)]
enum LineType {
    /// Regular content line
    Normal,
    /// No content on this line
    Empty,
    /// Line with extra indentation (preserve)
    MoreIndent,
}

#[derive(Debug, Clone, Copy)]
enum PendingAstWrap {
    SequenceItem { item_start: BytePosition },
    MappingPair { pair_start: BytePosition },
}

#[derive(Debug, Default)]
struct PendingAstWrapQueue {
    first: Option<PendingAstWrap>,
    second: Option<PendingAstWrap>,
}

impl PendingAstWrapQueue {
    fn clear(&mut self) {
        self.first = None;
        self.second = None;
    }

    #[allow(
        clippy::unreachable,
        reason = "queue overflow indicates a broken internal invariant and should fail loudly"
    )]
    fn push_back(&mut self, wrap: PendingAstWrap) {
        if self.first.is_none() {
            self.first = Some(wrap);
        } else if self.second.is_none() {
            self.second = Some(wrap);
        } else {
            unreachable!("pending AST wrap queue overflowed expected maximum depth");
        }
    }

    fn pop_front(&mut self) -> Option<PendingAstWrap> {
        let first = self.first.take()?;
        self.first = self.second.take();
        Some(first)
    }
}

/// A YAML event emitter using an explicit state machine.
///
/// Processes tokens and produces YAML events via the `Iterator` interface.
/// Uses an explicit state stack instead of recursion for parsing.
///
/// ## Architecture Note
///
/// The emitter does not own the tokens directly; all tokenization and
/// buffering is handled by [`TokenCursor`], which in turn owns a
/// streaming [`Lexer`]. The emitter tracks only a logical position
/// (`pos`) into that cursor and focuses on higher-level YAML
/// structure, indentation, and error recovery.
pub(crate) struct Emitter<'input> {
    /// Token cursor wrapper. Provides read-only access helpers over the token stream.
    cursor: TokenCursor<'input>,
    /// Original input string.
    input: &'input str,
    /// Current position in tokens.
    pos: usize,
    /// Current line indent level.
    current_indent: IndentLevel,
    /// Flow depth (0 = block context).
    flow_depth: usize,
    /// Document-level state.
    doc_state: DocState,
    /// Parse state stack (replaces call stack).
    state_stack: Vec<ParseState<'input>>,
    /// Collected errors.
    errors: Vec<ParseError>,
    /// Defined anchors in current document.
    anchors: HashSet<&'input str>,
    /// Whether `StreamStart` has been emitted.
    emitted_stream_start: bool,
    /// Event already determined by the current state transition and scheduled
    /// to be emitted before any further parsing work.
    pending_event: Option<Event<'input>>,
    /// Tag handles from directives.
    tag_handles: std::collections::HashMap<&'input str, &'input str>,
    /// Last content span - used for `MappingEnd`/`SequenceEnd` to produce
    /// intuitive end spans based on the last piece of content rather than the
    /// next structural token.
    /// Updated when emitting content events (scalars, aliases, nested collection ends).
    last_content_span: Option<Span>,
    /// Whether we've crossed a line boundary since the last time this flag was cleared.
    /// Set when consuming `LineStart` tokens. Used by `AfterValue` to determine if
    /// a nested structure crossed a line, even after those tokens were consumed.
    crossed_line_boundary: bool,
    /// Indentation stack tracking active block structure levels.
    /// Each entry is the indentation level of an active block structure.
    /// Used to detect orphan indentation (content at levels not in the stack).
    indent_stack: Vec<IndentLevel>,
    /// Span of the last `LineStart` token that set `current_indent`.
    /// Used for reporting `InvalidIndentation` errors at the line start position.
    last_line_start_span: Span,
    /// Stack of columns where each flow context started.
    /// Used to validate that continuation lines are indented relative to the flow start.
    /// Empty when not in flow context.
    flow_context_columns: Vec<IndentLevel>,
    /// Structural AST metadata to apply to the next emitted node-start event.
    pending_ast_wraps: PendingAstWrapQueue,
    /// Comment captured after `key:` / `-` before the next node begins.
    pending_ast_leading_comment: Option<Comment<'input>>,
}

pub(crate) struct AstEmitter<'a, 'input> {
    emitter: &'a mut Emitter<'input>,
}

impl<'input> Emitter<'input> {
    /// Create a new emitter from raw input.
    ///
    /// This constructs an internal streaming lexer and cursor. Tokens are
    /// produced on demand as the emitter peeks and consumes them.
    #[must_use]
    pub(crate) fn new(input: &'input str) -> Self {
        Self {
            cursor: TokenCursor::new(input),
            input,
            pos: 0,
            current_indent: 0,
            flow_depth: 0,
            doc_state: DocState::Ready,
            state_stack: Vec::with_capacity(16),
            errors: Vec::new(),
            anchors: HashSet::new(),
            emitted_stream_start: false,
            pending_event: None,
            tag_handles: std::collections::HashMap::new(),
            last_content_span: None,
            crossed_line_boundary: false,
            indent_stack: vec![0],                 // Start with base level 0
            last_line_start_span: Span::new(0..0), // Default span at start
            flow_context_columns: Vec::new(),
            pending_ast_wraps: PendingAstWrapQueue::default(),
            pending_ast_leading_comment: None,
        }
    }

    pub(crate) fn ast_events(&mut self) -> AstEmitter<'_, 'input> {
        AstEmitter { emitter: self }
    }

    /// Take collected errors from both lexer and emitter.
    pub(crate) fn take_errors(&mut self) -> Vec<ParseError> {
        let mut all = self.cursor.take_lexer_errors();
        all.extend(std::mem::take(&mut self.errors));
        all
    }

    fn set_pending_event(&mut self, event: Event<'input>) {
        // Hot path: once we have parsed enough input to know this is a mapping, we must
        // emit `MappingStart` before the already-parsed first key event. To avoid an
        // extra intermediate state on this path, we store that key in `pending_event`.
        //
        // This is intentionally a single-slot buffer. Overwriting it would corrupt the
        // emitted event stream, so we verify the invariant with `debug_assert!` in
        // development/test builds and rely on targeted stress coverage in CI.
        debug_assert!(
            self.pending_event.is_none(),
            "pending_event slot unexpectedly occupied"
        );
        self.pending_event = Some(event);
    }

    /// Emit a null scalar event.
    fn emit_null(&self) -> Event<'input> {
        Event::Scalar {
            style: ScalarStyle::Plain,
            value: Cow::Borrowed(""),
            properties: None,
            span: self.current_span(),
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "Complex state machine with value dispatch logic"
    )]
    /// Process the state stack and return the next event, if any.
    ///
    /// Returns `None` when the stack is empty (document content complete).
    fn process_state_stack(&mut self) -> Option<Event<'input>> {
        loop {
            let state = self.state_stack.pop()?;

            match state {
                ParseState::Value {
                    mut ctx,
                    properties,
                } => {
                    let current_kind = self.peek_kind();
                    if (ctx.prior_crossed_line || current_kind != Some(TokenKind::LineStart))
                        && matches!(
                            current_kind,
                            Some(
                                TokenKind::Whitespace
                                    | TokenKind::WhitespaceWithTabs
                                    | TokenKind::Comment
                            )
                        )
                        && let Some(comment) = self.take_same_line_comment_after_ws()
                    {
                        self.set_pending_ast_leading_comment(comment);
                    }

                    // Phase 1: Skip initial whitespace/newlines and update content_column.
                    let has_leading_linestart = self.peek_kind() == Some(TokenKind::LineStart);
                    let (crossed, ws_width) = if self.current_token_starts_trivia_run() {
                        self.skip_ws_and_newlines_tracked()
                    } else {
                        (false, 0)
                    };
                    let initial_crossed_line =
                        ctx.prior_crossed_line || has_leading_linestart || crossed;

                    // UPDATE content_column
                    if initial_crossed_line {
                        ctx.content_column = Some(self.current_indent + ws_width);
                    } else if ws_width > 0 {
                        ctx.content_column =
                            Some(ctx.content_column.map_or(ws_width, |col| col + ws_width));
                    }

                    self.handle_invalid_indent_after_line_cross(
                        ctx.min_indent,
                        initial_crossed_line,
                    );

                    if properties.is_empty()
                        && !matches!(self.peek_kind(), Some(TokenKind::Anchor | TokenKind::Tag))
                    {
                        if let Some(event) = self.process_value_after_properties(
                            ctx,
                            properties,
                            initial_crossed_line,
                            false,
                        ) {
                            return Some(event);
                        }
                        continue;
                    }

                    self.state_stack.push(ParseState::ValueCollectProperties {
                        ctx,
                        properties,
                        initial_crossed_line,
                        crossed_property_line_boundary: false,
                        consumed_width: 0,
                    });
                }
                ParseState::ValueCollectProperties {
                    mut ctx,
                    mut properties,
                    initial_crossed_line,
                    mut crossed_property_line_boundary,
                    mut consumed_width,
                } => loop {
                    let has_props = !properties.is_empty();

                    match self.peek_kind() {
                        Some(TokenKind::Anchor) => {
                            let Some((Token::Anchor(name_ref), span)) = self.take_current() else {
                                debug_assert!(false, "expected Anchor token");
                                break;
                            };
                            if properties.has_anchor() {
                                self.error(ErrorKind::DuplicateAnchor, span);
                            }

                            self.anchors.insert(name_ref);
                            let token_width =
                                usize_to_indent(span.end_usize() - span.start_usize());
                            properties.set_anchor(Property {
                                value: Cow::Borrowed(name_ref),
                                span,
                            });
                            let ws_width = self.skip_ws();
                            consumed_width += token_width + ws_width;
                            continue;
                        }
                        Some(TokenKind::Tag) => {
                            let Some((Token::Tag(tag_cow), span)) = self.take_current() else {
                                debug_assert!(false, "expected Tag token");
                                break;
                            };
                            let tag_str = tag_cow.as_ref();
                            let tag_looks_legitimate =
                                !tag_str.contains('"') && !tag_str.contains('`');

                            if properties.has_tag() {
                                self.error(ErrorKind::DuplicateTag, span);
                            }

                            let expanded = self.expand_tag(tag_cow, span);
                            let token_width =
                                usize_to_indent(span.end_usize() - span.start_usize());
                            properties.set_tag(Property {
                                value: expanded,
                                span,
                            });

                            let tag_end = span.end_usize();
                            if tag_looks_legitimate
                                && let Some(next_span) = self
                                    .peek_with(|next_tok, next_span| {
                                        matches!(
                                            next_tok,
                                            Token::Plain(_)
                                                | Token::StringStart(_)
                                                | Token::FlowSeqStart
                                                | Token::FlowMapStart
                                                | Token::BlockSeqIndicator
                                        )
                                        .then_some(next_span)
                                    })
                                    .flatten()
                                && next_span.start_usize() == tag_end
                            {
                                self.error(ErrorKind::ContentOnSameLine, next_span);
                            }

                            let ws_width = self.skip_ws();
                            consumed_width += token_width + ws_width;
                            continue;
                        }
                        Some(TokenKind::Comment) if has_props => {
                            let _ = self.take_current();
                            continue;
                        }
                        Some(TokenKind::LineStart) if has_props => {
                            let Some((next_indent, _)) = self.peek_line_start() else {
                                debug_assert!(false, "expected LineStart token");
                                break;
                            };
                            let should_continue = self.should_continue_collecting_properties();

                            if next_indent < ctx.min_indent {
                                crossed_property_line_boundary = true;
                            } else if should_continue {
                                crossed_property_line_boundary = true;
                                consumed_width = 0;
                                let _ = self.take_current();
                                continue;
                            } else {
                                crossed_property_line_boundary = true;
                                consumed_width = 0;
                            }
                        }
                        _ => {}
                    }

                    if crossed_property_line_boundary {
                        ctx.content_column = Some(self.current_indent + consumed_width);
                    } else if consumed_width > 0 {
                        ctx.content_column = Some(
                            ctx.content_column
                                .map_or(consumed_width, |col| col + consumed_width),
                        );
                    }

                    self.report_orphaned_properties_after_invalid_indent(
                        ctx.min_indent,
                        crossed_property_line_boundary,
                    );
                    let property_indent = (!properties.is_empty()).then_some(self.current_indent);

                    self.state_stack.push(ParseState::ValueDispatch {
                        ctx,
                        properties,
                        initial_crossed_line,
                        prop_crossed_line: crossed_property_line_boundary,
                        property_indent,
                    });
                    break;
                },
                ParseState::ValueDispatch {
                    ctx,
                    properties,
                    initial_crossed_line,
                    prop_crossed_line,
                    property_indent,
                } => {
                    if let Some(event) = self.process_value_dispatch_state(
                        ctx,
                        properties,
                        initial_crossed_line,
                        prop_crossed_line,
                        property_indent,
                    ) {
                        return Some(event);
                    }
                    // No value produced, continue with next state
                }
                ParseState::ValueDispatchToken {
                    ctx,
                    properties,
                    initial_crossed_line,
                    prop_crossed_line,
                } => {
                    if let Some(event) = self.process_value_after_properties(
                        ctx,
                        properties,
                        initial_crossed_line,
                        prop_crossed_line,
                    ) {
                        return Some(event);
                    }
                }
                ParseState::AliasValue {
                    name,
                    span,
                    properties,
                    crossed_line_after_properties,
                } => {
                    return Some(self.process_alias_value_state(
                        name,
                        span,
                        properties,
                        crossed_line_after_properties,
                    ));
                }
                ParseState::PlainScalarBlock {
                    first_line,
                    properties,
                    start_span,
                    end_span,
                    min_indent,
                    consecutive_newlines,
                    has_continuation,
                    content,
                } => {
                    return Some(self.process_plain_scalar_block_state(
                        first_line,
                        properties,
                        start_span,
                        end_span,
                        min_indent,
                        consecutive_newlines,
                        has_continuation,
                        content,
                    ));
                }
                ParseState::PlainScalarFlow {
                    first_line,
                    properties,
                    start_span,
                    end_span,
                    has_continuation,
                    content,
                } => {
                    return Some(self.process_plain_scalar_flow_state(
                        first_line,
                        properties,
                        start_span,
                        end_span,
                        has_continuation,
                        content,
                    ));
                }
                ParseState::QuotedScalar {
                    properties,
                    quote_style,
                    min_indent,
                    start_span,
                    parts,
                    end_span,
                    pending_newlines,
                    needs_trim,
                } => {
                    return Some(self.process_quoted_scalar_state(
                        properties,
                        quote_style,
                        min_indent,
                        start_span,
                        parts,
                        end_span,
                        pending_newlines,
                        needs_trim,
                    ));
                }
                ParseState::BlockScalar {
                    properties,
                    header,
                    start_span,
                    min_indent,
                    is_literal,
                } => {
                    return Some(self.process_block_scalar_state(
                        properties, header, start_span, min_indent, is_literal,
                    ));
                }
                ParseState::AdditionalPropertiesCollect {
                    mut ctx,
                    outer,
                    mut inner,
                    mut crossed_line_boundary,
                    mut consumed_width,
                } => loop {
                    let has_props = !inner.is_empty();
                    match self.peek_kind() {
                        Some(TokenKind::Anchor) => {
                            let Some((Token::Anchor(name_ref), span)) = self.take_current() else {
                                debug_assert!(false, "expected Anchor token");
                                break;
                            };
                            if inner.has_anchor() {
                                self.error(ErrorKind::DuplicateAnchor, span);
                            }

                            self.anchors.insert(name_ref);
                            let token_width =
                                usize_to_indent(span.end_usize() - span.start_usize());
                            inner.set_anchor(Property {
                                value: Cow::Borrowed(name_ref),
                                span,
                            });
                            let ws_width = self.skip_ws();
                            consumed_width += token_width + ws_width;
                            continue;
                        }
                        Some(TokenKind::Tag) => {
                            let Some((Token::Tag(tag_cow), span)) = self.take_current() else {
                                debug_assert!(false, "expected Tag token");
                                break;
                            };
                            let tag_str = tag_cow.as_ref();
                            let tag_looks_legitimate =
                                !tag_str.contains('"') && !tag_str.contains('`');

                            if inner.has_tag() {
                                self.error(ErrorKind::DuplicateTag, span);
                            }

                            let expanded = self.expand_tag(tag_cow, span);
                            let token_width =
                                usize_to_indent(span.end_usize() - span.start_usize());
                            inner.set_tag(Property {
                                value: expanded,
                                span,
                            });

                            let tag_end = span.end_usize();
                            if tag_looks_legitimate
                                && let Some(next_span) = self
                                    .peek_with(|next_tok, next_span| {
                                        matches!(
                                            next_tok,
                                            Token::Plain(_)
                                                | Token::StringStart(_)
                                                | Token::FlowSeqStart
                                                | Token::FlowMapStart
                                                | Token::BlockSeqIndicator
                                        )
                                        .then_some(next_span)
                                    })
                                    .flatten()
                                && next_span.start_usize() == tag_end
                            {
                                self.error(ErrorKind::ContentOnSameLine, next_span);
                            }

                            let ws_width = self.skip_ws();
                            consumed_width += token_width + ws_width;
                            continue;
                        }
                        Some(TokenKind::Comment) if has_props => {
                            let _ = self.take_current();
                            continue;
                        }
                        Some(TokenKind::LineStart) if has_props => {
                            let should_continue = self.should_continue_collecting_properties();
                            if should_continue {
                                crossed_line_boundary = true;
                                consumed_width = 0;
                                let _ = self.take_current();
                                continue;
                            }
                        }
                        _ => {}
                    }

                    let ws_width = self.skip_ws();
                    if crossed_line_boundary {
                        ctx.content_column = Some(self.current_indent + consumed_width + ws_width);
                    } else {
                        let extra = consumed_width + ws_width;
                        if extra > 0 {
                            ctx.content_column =
                                Some(ctx.content_column.map_or(extra, |col| col + extra));
                        }
                    }

                    if self.peek_kind() == Some(TokenKind::LineStart) {
                        let (_, w) = self.skip_ws_and_newlines_tracked();
                        ctx.content_column = Some(self.current_indent + w);
                        if matches!(self.peek_kind(), Some(TokenKind::Anchor | TokenKind::Tag)) {
                            self.state_stack
                                .push(ParseState::AdditionalPropertiesCollect {
                                    ctx,
                                    outer,
                                    inner,
                                    crossed_line_boundary: true,
                                    consumed_width: 0,
                                });
                        } else {
                            self.state_stack
                                .push(ParseState::AdditionalPropertiesValue { ctx, outer, inner });
                        }
                    } else {
                        self.state_stack
                            .push(ParseState::AdditionalPropertiesValue { ctx, outer, inner });
                    }
                    break;
                },
                ParseState::AdditionalPropertiesValue { ctx, outer, inner } => {
                    if !inner.is_empty()
                        && matches!(
                            self.peek_kind(),
                            Some(TokenKind::FlowSeqStart | TokenKind::FlowMapStart)
                        )
                        && self.current_flow_collection_is_complex_key(false)
                    {
                        let (outer_anchor, outer_tag) = outer.into_parts();
                        let span = self.current_span();
                        let map_indent = self.current_indent;

                        let is_seq = self.peek_kind() == Some(TokenKind::FlowSeqStart);
                        let _ = self.take_current();
                        self.enter_flow_collection(ctx.content_column);

                        self.state_stack.push(ParseState::BlockMap {
                            indent: map_indent,
                            phase: BlockMapPhase::AfterKey {
                                is_implicit_scalar_key: false,
                                key_end_column: None,
                            },
                            start_span: span,
                            properties: EmitterProperties::default(),
                        });

                        if is_seq {
                            self.state_stack.push(ParseState::FlowSeq {
                                phase: FlowSeqPhase::BeforeEntry,
                                start_span: span,
                            });
                            self.set_pending_event(Event::SequenceStart {
                                style: crate::event::CollectionStyle::Flow,
                                properties: inner.into_event_box(),
                                span,
                            });
                        } else {
                            self.state_stack.push(ParseState::FlowMap {
                                phase: FlowMapPhase::BeforeKey,
                                start_span: span,
                            });
                            self.set_pending_event(Event::MappingStart {
                                style: crate::event::CollectionStyle::Flow,
                                properties: inner.into_event_box(),
                                span,
                            });
                        }

                        self.push_indent(map_indent);
                        return Some(Event::MappingStart {
                            style: crate::event::CollectionStyle::Block,
                            properties: EmitterProperties::from_parts(outer_anchor, outer_tag)
                                .into_event_box(),
                            span,
                        });
                    }
                    self.state_stack
                        .push(ParseState::AdditionalPropertiesDispatchToken { ctx, outer, inner });
                }
                ParseState::AdditionalPropertiesDispatchToken { ctx, outer, inner } => {
                    let (outer_anchor, outer_tag) = outer.into_parts();
                    let min_indent = ctx.min_indent;
                    let is_implicit_key = matches!(ctx.kind, ValueKind::ImplicitKey);

                    match self.peek_kind() {
                        Some(TokenKind::Plain | TokenKind::StringStart) => {
                            if !is_implicit_key && self.flow_depth == 0 && self.is_implicit_key() {
                                let span = self.current_span();
                                let map_indent = self.current_indent;
                                let key_event = self.parse_plain_scalar(inner, min_indent);
                                let mapping_start = self.build_block_mapping_from_scalar_key(
                                    map_indent,
                                    span,
                                    EmitterProperties::from_parts(outer_anchor, outer_tag),
                                    key_event,
                                    ctx.content_column,
                                );
                                return Some(mapping_start);
                            }

                            let result = self.parse_plain_scalar(inner, min_indent);
                            if is_implicit_key && let Event::Scalar { span, .. } = &result {
                                self.check_multiline_implicit_key(*span);
                            }
                            return Some(result);
                        }
                        Some(TokenKind::BlockSeqIndicator) => {
                            let merged = inner
                                .merged(EmitterProperties::from_parts(outer_anchor, outer_tag));
                            let span = self.current_span();
                            let seq_indent = ctx.content_column.unwrap_or(self.current_indent);
                            self.push_indent(seq_indent);
                            self.current_indent = seq_indent;
                            self.state_stack.push(ParseState::BlockSeq {
                                indent: seq_indent,
                                phase: BlockSeqPhase::BeforeEntryScan,
                                start_span: span,
                                properties: EmitterProperties::default(),
                            });
                            return Some(Event::SequenceStart {
                                style: crate::event::CollectionStyle::Block,
                                properties: merged.into_event_box(),
                                span,
                            });
                        }
                        Some(TokenKind::FlowSeqStart) => {
                            let merged = inner
                                .merged(EmitterProperties::from_parts(outer_anchor, outer_tag));
                            let span = self.current_span();
                            let _ = self.take_current();
                            self.enter_flow_collection(ctx.content_column);
                            self.state_stack.push(ParseState::FlowSeq {
                                phase: FlowSeqPhase::BeforeEntry,
                                start_span: span,
                            });
                            return Some(Event::SequenceStart {
                                style: crate::event::CollectionStyle::Flow,
                                properties: merged.into_event_box(),
                                span,
                            });
                        }
                        Some(TokenKind::FlowMapStart) => {
                            let merged = inner
                                .merged(EmitterProperties::from_parts(outer_anchor, outer_tag));
                            let span = self.current_span();
                            let _ = self.take_current();
                            self.enter_flow_collection(ctx.content_column);
                            self.state_stack.push(ParseState::FlowMap {
                                phase: FlowMapPhase::BeforeKey,
                                start_span: span,
                            });
                            return Some(Event::MappingStart {
                                style: crate::event::CollectionStyle::Flow,
                                properties: merged.into_event_box(),
                                span,
                            });
                        }
                        Some(_) | None => {
                            let merged = inner
                                .merged(EmitterProperties::from_parts(outer_anchor, outer_tag));
                            return Some(Event::Scalar {
                                style: ScalarStyle::Plain,
                                value: Cow::Borrowed(""),
                                properties: merged.into_event_box(),
                                span: self.current_span(),
                            });
                        }
                    }
                }
                ParseState::FlowCollectionValue {
                    is_map,
                    span,
                    properties,
                    kind,
                    content_column,
                } => {
                    return Some(self.process_flow_collection_value_state(
                        is_map,
                        span,
                        properties,
                        kind,
                        content_column,
                    ));
                }

                ParseState::BlockSeq {
                    indent,
                    phase,
                    start_span,
                    properties,
                } => {
                    if let Some(event) =
                        self.process_block_seq(indent, phase, start_span, properties)
                    {
                        return Some(event);
                    }
                }

                ParseState::BlockMap {
                    indent,
                    phase,
                    start_span,
                    properties,
                } => {
                    if let Some(event) =
                        self.process_block_map(indent, phase, start_span, properties)
                    {
                        return Some(event);
                    }
                }

                ParseState::FlowSeq { phase, start_span } => {
                    if let Some(event) = self.process_flow_seq(phase, start_span) {
                        return Some(event);
                    }
                }

                ParseState::FlowMap { phase, start_span } => {
                    if let Some(event) = self.process_flow_map(phase, start_span) {
                        return Some(event);
                    }
                }
            }
        }
    }

    fn next_event_core(&mut self) -> Option<Event<'input>> {
        // Emit StreamStart first
        if !self.emitted_stream_start {
            self.emitted_stream_start = true;
            self.pending_ast_wraps.clear();
            return Some(Event::StreamStart);
        }

        if let Some(event) = self.pending_event.take() {
            self.track_emitted_event(&event);
            return Some(event);
        }

        loop {
            // Document-level state machine
            match &self.doc_state {
                DocState::Ready => {
                    if let Some((explicit, span, initial_col)) = self.prepare_document() {
                        self.doc_state = DocState::EmitDocStart {
                            explicit,
                            span,
                            initial_col,
                        };
                    } else {
                        self.doc_state = DocState::Done;
                        return Some(Event::StreamEnd);
                    }
                }

                DocState::EmitDocStart {
                    explicit,
                    span,
                    initial_col,
                } => {
                    let event = Event::DocumentStart {
                        explicit: *explicit,
                        span: *span,
                    };
                    let initial_col = *initial_col;
                    // Push initial value parse state (top-level document value).
                    // content_column is seeded from prepare_document's tracked position.
                    self.state_stack.push(ParseState::Value {
                        ctx: ValueContext {
                            min_indent: 0,
                            content_column: Some(initial_col),
                            kind: ValueKind::TopLevelValue,
                            allow_implicit_mapping: true, // Document root allows implicit mappings
                            prior_crossed_line: false,
                        },
                        properties: EmitterProperties::default(),
                    });
                    self.doc_state = DocState::Content;
                    return Some(event);
                }

                DocState::Content => {
                    // Process state stack
                    if let Some(event) = self.process_state_stack() {
                        self.track_emitted_event(&event);
                        return Some(event);
                    }
                    // Stack empty, finish document
                    let (explicit, span) = self.finish_document();
                    self.doc_state = DocState::EmitDocEnd { explicit, span };
                }

                DocState::EmitDocEnd { explicit, span } => {
                    let event = Event::DocumentEnd {
                        explicit: *explicit,
                        span: *span,
                    };
                    self.doc_state = DocState::Ready;
                    return Some(event);
                }

                DocState::Done => {
                    return None;
                }
            }
        }
    }
}

impl<'input> Iterator for Emitter<'input> {
    type Item = Event<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        let event = self.next_event_core()?;
        self.discard_pending_ast_wrap();
        Some(event)
    }
}

impl<'input> Iterator for AstEmitter<'_, 'input> {
    type Item = AstEvent<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        self.emitter.next_ast_event()
    }
}

#[cfg(test)]
pub(crate) fn internal_type_sizes() -> (usize, usize) {
    (
        size_of::<EmitterProperties<'static>>(),
        size_of::<ParseState<'static>>(),
    )
}

#[cfg(test)]
mod tests;
