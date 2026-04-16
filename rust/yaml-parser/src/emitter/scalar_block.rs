// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use crate::error::ErrorKind;
use crate::event::{Event, ScalarStyle};
use crate::lexer::{Token, TokenKind};
use crate::span::{BytePosition, IndentLevel, Span};

use super::states::{EmitterProperties, ParseState};
use super::{Emitter, LineType};

impl<'input> Emitter<'input> {
    pub(super) fn parse_block_scalar(
        &mut self,
        min_indent: IndentLevel,
        properties: EmitterProperties<'input>,
    ) -> Event<'input> {
        let Some((header, start_span, is_literal)) = self
            .peek_with(|tok, span| match tok {
                Token::LiteralBlockHeader(header) => Some((*header, span, true)),
                Token::FoldedBlockHeader(header) => Some((*header, span, false)),
                _ => None,
            })
            .flatten()
        else {
            return self.emit_null();
        };

        let _ = self.take_current(); // consume header
        self.state_stack.push(ParseState::BlockScalar {
            properties,
            header,
            start_span,
            min_indent,
            is_literal,
        });
        self.process_state_stack()
            .unwrap_or_else(|| self.emit_null())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "block scalar folding and chomping rules are easier to validate in one place"
    )]
    pub(super) fn process_block_scalar_state(
        &mut self,
        properties: EmitterProperties<'input>,
        header: crate::lexer::BlockScalarHeader,
        start_span: Span,
        min_indent: IndentLevel,
        is_literal: bool,
    ) -> Event<'input> {
        let mut end_span = start_span;
        // Streaming builder for the block scalar value. For literal style we
        // append one newline per logical line. For folded style we track
        // previous and last-content line types to implement YAML's folding
        // rules (mirroring `join_folded_lines`).
        let mut value = String::new();
        let mut prev_type = LineType::Empty;
        let mut last_content_type: Option<LineType> = None;
        let mut scalar_has_any_part = false; // any non-empty line (including whitespace-only)
        let mut had_any_line = false; // any logical line at all

        // Per YAML spec 8.1.1.1: content_indent = block_scalar_indent + explicit_indicator
        // The block_scalar_indent is min_indent - 1 (the parent block's indentation).
        let explicit_indent: Option<IndentLevel> = header.indent.map(IndentLevel::from);
        // When explicit indicator is present, calculate content_indent from it
        // Otherwise, auto-detect from first content line
        let mut content_indent: Option<IndentLevel> =
            explicit_indent.map(|ei| min_indent.saturating_sub(1).saturating_add(ei));

        // Track empty lines before content for indentation validation.
        // If an empty line has MORE indentation than the first content line,
        // it's an InvalidIndentation error.
        let mut empty_lines_before_content: Vec<(IndentLevel, Span)> = Vec::new();

        // Skip to first line of content
        while let Some(kind) = self.peek_kind() {
            match kind {
                TokenKind::Comment => {
                    let _ = self.take_current();
                }
                _ => break,
            }
        }

        // Expect LineStart to begin each line
        while let Some((indent_level, line_start_span)) = self.peek_line_start() {
            let _ = self.take_current();
            let current_kind = self.peek_kind();

            // Check for tabs used as indentation in block scalar content.
            // Per YAML spec, tabs are allowed as content but not as indentation.
            // Tabs are invalid if they appear BEFORE reaching the content indent level.
            // For example, in `foo: |\n\t\tbar`, if content_indent is 2, the tabs at column 0
            // are indentation (invalid). But in `foo: |\n  \tbar`, the tab at column 2 is
            // content (valid).
            if current_kind == Some(TokenKind::WhitespaceWithTabs) {
                let tab_span = self.current_span();
                // Tab is right after LineStart — its column is current_indent.
                let tab_col = self.current_indent;
                if let Some(ci) = content_indent {
                    // Content indent is known - tabs before it are indentation (invalid)
                    if tab_col < ci {
                        self.error(ErrorKind::InvalidIndentation, tab_span);
                    }
                } else {
                    // Content indent not yet determined.
                    // Tabs before min_indent are definitely indentation (invalid).
                    // Tabs at or after min_indent might be content, so we can't check yet.
                    if tab_col < min_indent {
                        self.error(ErrorKind::InvalidIndentation, tab_span);
                    }
                }
            }

            // Check if this line ends the block scalar
            let is_terminator = matches!(
                current_kind,
                None | Some(TokenKind::DocEnd | TokenKind::DocStart)
            );
            if is_terminator && indent_level == 0 {
                break;
            }

            // Check if line has content (for termination and indent detection)
            // Include flow indicators since they can be content in block scalars.
            // Also include block structure indicators (BlockSeqIndicator, MappingKey) since they
            // indicate sibling nodes and should trigger termination checks.
            let has_content = matches!(
                current_kind,
                Some(
                    TokenKind::Plain
                        | TokenKind::Whitespace
                        | TokenKind::WhitespaceWithTabs
                        | TokenKind::Comment
                        | TokenKind::FlowSeqStart
                        | TokenKind::FlowSeqEnd
                        | TokenKind::FlowMapStart
                        | TokenKind::FlowMapEnd
                        | TokenKind::Colon
                        | TokenKind::Comma
                        | TokenKind::BlockSeqIndicator
                        | TokenKind::MappingKey
                )
            );

            // For block scalars with no content yet: a non-empty line at or below the parent
            // indent level (min_indent - 1) cannot be block scalar content - it must be
            // the next sibling node. For example, empty block scalars like:
            //   strip: >-
            //
            //   clip: >
            // The "clip:" is at the same indent as "strip:", so it's not block scalar content.
            if content_indent.is_none() && has_content && indent_level < min_indent {
                break;
            }

            // Track empty lines before content for indentation validation
            if content_indent.is_none() && !has_content {
                empty_lines_before_content.push((indent_level, line_start_span));
            }

            // Determine content indent from first non-empty line
            if content_indent.is_none() && has_content && indent_level > 0 {
                content_indent = Some(indent_level);
                // Validate preceding empty lines - they should not have MORE indentation
                // than the first content line.
                for (empty_indent, empty_span) in &empty_lines_before_content {
                    if *empty_indent > indent_level {
                        self.error(ErrorKind::InvalidIndentation, *empty_span);
                    }
                }
            }

            // Check termination: lines with content at indent < content_indent terminate the block
            // When explicit indicator is present, content_indent was already calculated correctly
            let should_terminate = if let Some(ci) = content_indent {
                has_content && indent_level < ci
            } else {
                false
            };

            if should_terminate {
                break;
            }

            had_any_line = true;

            // Track the raw source range for the accepted line content.
            // Once a line is known to belong to this block scalar, the handled
            // token kinds form one contiguous source slice, so we can rebuild
            // the line from spans instead of assembling per-token fragments.
            let mut line_start: Option<BytePosition> = None;
            let mut line_end: Option<BytePosition> = None;
            // Track whether this line has any non-whitespace content so we can
            // classify purely-whitespace lines without re-scanning the text.
            let mut has_non_whitespace = false;
            let mut has_tab_in_prefix = false;
            let line_type;
            let mut line_end_span = line_start_span;

            // Check for extra indentation (more-indented)
            let extra_indent = content_indent.map_or(0, |ci| indent_level.saturating_sub(ci));

            // Hot path: do not use `self.peek()` here. It clones the token
            // payload, and this loop usually consumes the token immediately
            // via `take_current()` anyway. Keep this loop on kind-based
            // lookahead plus `take_current()` unless a future benchmark proves
            // otherwise.
            while let Some((kind, span)) = self.peek_kind_with_span() {
                match kind {
                    TokenKind::Whitespace => {
                        let _ = self.take_current();
                        line_start.get_or_insert(span.start);
                        line_end = Some(span.end);
                        line_end_span = span;
                    }
                    TokenKind::WhitespaceWithTabs => {
                        let _ = self.take_current();
                        line_start.get_or_insert(span.start);
                        line_end = Some(span.end);
                        if !has_non_whitespace {
                            // Tabs before any non-whitespace content are part of
                            // the leading indentation within the block scalar
                            // content. Treat such lines as more-indented so folded
                            // scalars preserve them more literally.
                            has_tab_in_prefix = true;
                        }
                        line_end_span = span;
                    }
                    TokenKind::Plain
                    | TokenKind::Comment
                    | TokenKind::FlowSeqStart
                    | TokenKind::FlowSeqEnd
                    | TokenKind::FlowMapStart
                    | TokenKind::FlowMapEnd
                    | TokenKind::Colon
                    | TokenKind::Comma => {
                        let _ = self.take_current();
                        line_start.get_or_insert(span.start);
                        line_end = Some(span.end);
                        has_non_whitespace = true;
                        line_end_span = span;
                    }
                    _ => break,
                }
            }

            // Recover trailing whitespace that the lexer didn't include in tokens.
            // The lexer creates Plain("foo") for "foo " (missing trailing space).
            // If the next token is a LineStart, any gap between line_end_span.end and
            // LineStart.start is trailing whitespace to preserve.
            #[allow(clippy::string_slice, reason = "Span positions are UTF-8 boundaries")]
            if let Some(end_pos) = line_end
                && let Some((_, next_span)) = self.peek_line_start()
            {
                let next_start = next_span.start;
                if next_start > end_pos {
                    // There's a gap - check if it's all whitespace
                    let trailing = &self.input[Span::new(end_pos..next_start).to_range()];
                    if trailing
                        .as_bytes()
                        .iter()
                        .all(|char| *char == b' ' || *char == b'\t')
                    {
                        // This is guaranteed to be whitespace-only.
                        line_end = Some(next_start);
                    }
                }
            }

            let has_virtual_indent_content = extra_indent > 0;

            // Classify line type using the tracked non-whitespace flag.
            if (line_start.is_none() && !has_virtual_indent_content) || !has_non_whitespace {
                if line_start.is_none() && !has_virtual_indent_content {
                    line_type = LineType::Empty;
                } else {
                    // Whitespace-only line: treat as more-indented whitespace.
                    line_type = LineType::MoreIndent;
                }
            } else if extra_indent > 0 || has_tab_in_prefix {
                // Lines that are visually more-indented than the content
                // indent, including lines whose leading whitespace contains
                // tabs before the first non-whitespace character, are
                // treated as `MoreIndent`. This matches the YAML test suite
                // case MJS9 (Spec Example 6.7. Block Folding), where a line
                // starting with a tab must be preserved more literally.
                line_type = LineType::MoreIndent;
            } else {
                line_type = LineType::Normal;
            }

            // Only update end_span if this line had actual content (including
            // whitespace-only content). Structurally empty lines don't extend the span.
            if line_start.is_some() || has_virtual_indent_content {
                end_span = line_end_span;
                scalar_has_any_part = true;
            }

            #[allow(clippy::string_slice, reason = "Span positions are UTF-8 boundaries")]
            let line_slice = line_start
                .zip(line_end)
                .map(|(start, end)| &self.input[Span::new(start..end).to_range()]);
            let line_text_len = line_slice.map_or(0, str::len);
            let extra_indent_usize = usize::from(extra_indent);
            let line_capacity = extra_indent_usize + line_text_len + 1;

            // Stream this line directly into the final value instead of
            // accumulating all lines first.
            if is_literal {
                // Literal: preserve all newlines. Each logical line contributes
                // exactly one newline, regardless of whether it has content.
                value.reserve(line_capacity);
                if extra_indent > 0 {
                    const SPACES: &str = "                                ";
                    if let Some(spaces) = SPACES.get(..extra_indent_usize) {
                        value.push_str(spaces);
                    } else {
                        for _ in 0..extra_indent_usize {
                            value.push(' ');
                        }
                    }
                }
                if let Some(line_text) = line_slice {
                    value.push_str(line_text);
                }
                value.push('\n');
            } else {
                // Folded: mirror `join_folded_lines` but operate per-line.
                match line_type {
                    LineType::Empty => {
                        // Empty line contributes newline
                        if prev_type == LineType::MoreIndent {
                            value.push('\n'); // Line break after more-indented
                        }
                        value.push('\n');
                    }
                    LineType::MoreIndent => {
                        // More-indented lines preserve the preceding newline, but we
                        // need to be careful about Empty→MoreIndent transitions.
                        value.reserve(line_capacity);
                        if !value.is_empty() {
                            match prev_type {
                                LineType::Normal | LineType::MoreIndent => {
                                    // Line break before more-indented is NOT folded
                                    value.push('\n');
                                }
                                LineType::Empty => {
                                    // Empty already added its newline. We only add
                                    // another if we came from Normal context through
                                    // Empty. MoreIndent→Empty→MoreIndent: no extra
                                    // newline needed. Normal→Empty→MoreIndent: need
                                    // extra newline.
                                    if last_content_type == Some(LineType::Normal) {
                                        value.push('\n');
                                    }
                                }
                            }
                        }
                        if extra_indent > 0 {
                            const SPACES: &str = "                                ";
                            if let Some(spaces) = SPACES.get(..extra_indent_usize) {
                                value.push_str(spaces);
                            } else {
                                for _ in 0..extra_indent_usize {
                                    value.push(' ');
                                }
                            }
                        }
                        if let Some(line_text) = line_slice {
                            value.push_str(line_text);
                        }
                        last_content_type = Some(LineType::MoreIndent);
                    }
                    LineType::Normal => {
                        value.reserve(line_capacity);
                        if !value.is_empty() {
                            match prev_type {
                                LineType::Normal => {
                                    value.push(' '); // Fold
                                }
                                LineType::MoreIndent => {
                                    value.push('\n'); // Preserve
                                }
                                LineType::Empty => {
                                    // Newlines already added
                                }
                            }
                        }
                        if let Some(line_text) = line_slice {
                            value.push_str(line_text);
                        }
                        last_content_type = Some(LineType::Normal);
                    }
                }
                prev_type = line_type;
            }
        }

        // For folded scalars, mirror the final newline behaviour of
        // `join_folded_lines`: add a trailing newline when there was at
        // least one line and the last line was not Empty.
        if !is_literal && had_any_line && prev_type != LineType::Empty {
            value.push('\n');
        }

        // For empty block scalars (no actual content), use just the header span
        // Otherwise, span from header start to end of last content
        // A scalar is empty if no line contributed any parts at all (even
        // whitespace-only lines).
        let is_empty_scalar = !scalar_has_any_part;

        // Apply chomping in place so block-scalar assembly only needs one
        // full-string allocation.
        Self::apply_chomping_in_place(&mut value, header, is_empty_scalar);

        let style = if is_literal {
            ScalarStyle::Literal
        } else {
            ScalarStyle::Folded
        };

        let full_span = if is_empty_scalar {
            start_span
        } else {
            Span::new(start_span.start..end_span.end)
        };

        Event::Scalar {
            style,
            value: Cow::Owned(value),
            properties: properties.into_event_box(),
            span: full_span,
        }
    }

    // `join_folded_lines` logic is now inlined into `parse_block_scalar` for
    // a streaming implementation; helper removed.

    /// Apply chomping indicator to a block scalar value in place.
    ///
    /// `is_empty_scalar` is true when no logical line contributed any
    /// content parts at all. This lets us distinguish truly empty block
    /// scalars (like K858's `clip: >` with no content lines) from scalars
    /// that merely end with trailing newlines.
    pub(super) fn apply_chomping_in_place(
        value: &mut String,
        header: crate::lexer::BlockScalarHeader,
        is_empty_scalar: bool,
    ) {
        use crate::lexer::Chomping;

        #[inline]
        #[allow(
            clippy::indexing_slicing,
            reason = "len > 0 guarantees len - 1 is in bounds"
        )]
        fn trim_trailing_newlines_len(input: &str) -> usize {
            let bytes = input.as_bytes();
            let mut len = bytes.len();
            while len > 0 && bytes[len - 1] == b'\n' {
                len -= 1;
            }
            len
        }

        match header.chomping {
            Chomping::Strip => {
                // Strip: remove all trailing newlines without reallocating.
                let trimmed_len = trim_trailing_newlines_len(value);
                value.truncate(trimmed_len);
            }
            Chomping::Clip => {
                // Clip: ensure at most one trailing newline, and only if
                // there's actual content.
                //
                // For *truly empty* block scalars (no content lines at all),
                // the YAML Test Suite (K858) expects an empty string even for
                // `>` (clip). Treat those separately based on
                // `is_empty_scalar` instead of just looking at `value`.
                if is_empty_scalar {
                    value.clear();
                    return;
                }

                if value.ends_with('\n') {
                    // Has at least one trailing newline: keep exactly one.
                    let trimmed_len = trim_trailing_newlines_len(value);
                    value.truncate(trimmed_len);
                    value.push('\n');
                } else if value.is_empty() {
                    // Non-empty scalar should always have at least one
                    // newline, but be defensive.
                    value.clear();
                } else {
                    // No trailing newline yet: add exactly one.
                    value.push('\n');
                }
            }
            Chomping::Keep => {
                // Keep: the builder for block scalars already preserves all
                // trailing newlines and ensures at least one when there is
                // content, so no further work is required.
            }
        }
    }

    // ═══════════════════════════════════════════════════════════════════
    // Iterator Implementation - The State Machine
    // ═══════════════════════════════════════════════════════════════════
}
