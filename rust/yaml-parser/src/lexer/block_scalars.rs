// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use super::{BlockScalarHeader, BlockScalarToken, Chomping, Lexer, Token};
use crate::error::ErrorKind;
use crate::span::{Span, Spanned};

#[derive(Clone, Copy, PartialEq)]
enum BlockLineType {
    Normal,
    Empty,
    MoreIndent,
}

struct BlockScalarAppendState {
    prev_type: BlockLineType,
    last_content_type: Option<BlockLineType>,
}

impl<'input> Lexer<'input> {
    /// Try to lex a full block scalar (`|` or `>`), including its content.
    pub(super) fn try_lex_block_scalar_header(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        let is_literal = match ch {
            '|' => true,
            '>' => false,
            _ => return None,
        };

        let header_line_indent = self.current_line.indent;
        let header_column = start.saturating_sub(self.current_line.start);

        self.advance();
        let header = self.consume_block_header();
        let (value, span) = self.consume_block_scalar_content(
            start,
            header,
            header_line_indent,
            header_column,
            is_literal,
        );
        let token = BlockScalarToken::new(Cow::Owned(value));

        if is_literal {
            Some((Token::LiteralBlockScalar(token), span))
        } else {
            Some((Token::FoldedBlockScalar(token), span))
        }
    }

    /// Consume the indentation/chomping indicators and optional same-line
    /// comment after `|` / `>`, leaving the cursor at the header line break.
    fn consume_block_header(&mut self) -> BlockScalarHeader {
        let mut indent = None;
        let mut chomping = Chomping::Clip;

        // Parse indent and chomping indicators (can be in any order)
        for _ in 0..2 {
            match self.peek() {
                Some('+') => {
                    chomping = Chomping::Keep;
                    self.advance();
                }
                Some('-') => {
                    chomping = Chomping::Strip;
                    self.advance();
                }
                Some(ch) if ch.is_ascii_digit() && ch != '0' => {
                    // ch is guaranteed to be ASCII digit 1-9, so to_digit is safe
                    indent = ch.to_digit(10).and_then(|digit| u8::try_from(digit).ok());
                    self.advance();
                }
                _ => break,
            }
        }

        // After block header, only whitespace and comments are allowed on the same line
        // Any other content is invalid (e.g., `> first line` is invalid)
        // Comments require preceding whitespace (e.g., `># comment` is invalid)
        let error_start = self.byte_pos;
        let mut has_invalid_content = false;
        let mut saw_whitespace = false;
        while let Some(peek_ch) = self.peek() {
            if Self::is_newline(peek_ch) {
                break;
            }
            if peek_ch == ' ' || peek_ch == '\t' {
                saw_whitespace = true;
                self.advance();
                continue;
            }
            if peek_ch == '#' {
                if saw_whitespace {
                    let comment_start = self.byte_pos;
                    self.advance();
                    let content_start = self.byte_pos;
                    while let Some(ch) = self.peek() {
                        if Self::is_newline(ch) {
                            break;
                        }
                        self.advance();
                    }
                    let content = self
                        .input
                        .get(content_start..self.byte_pos)
                        .unwrap_or_default();
                    self.pending_tokens.push_back(super::RichToken::new(
                        Token::Comment(Cow::Borrowed(content)),
                        Span::from_usize_range(comment_start..self.byte_pos),
                    ));
                } else {
                    has_invalid_content = true;
                    while let Some(ch) = self.peek() {
                        if Self::is_newline(ch) {
                            break;
                        }
                        self.advance();
                    }
                }
                break;
            }
            has_invalid_content = true;
            self.advance();
        }

        if has_invalid_content {
            let span = Span::from_usize_range(error_start..self.byte_pos);
            self.add_error(ErrorKind::ContentOnSameLine, span);
        }

        BlockScalarHeader { indent, chomping }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "block scalar scanning mirrors YAML indentation, folding and chomping rules"
    )]
    /// Consume all physical content lines for this block scalar and build the
    /// final scalar value/span. Terminates before the first sibling/document line.
    fn consume_block_scalar_content(
        &mut self,
        token_start: usize,
        header: BlockScalarHeader,
        header_line_indent: usize,
        header_column: usize,
        is_literal: bool,
    ) -> (String, Span) {
        let mut value = String::new();
        let mut append_state = BlockScalarAppendState {
            prev_type: BlockLineType::Empty,
            last_content_type: None,
        };
        let mut had_any_line = false;
        let mut scalar_has_any_part = false;
        let mut end_pos = self.byte_pos;
        let indent_context = self.pending_value_indent.unwrap_or_else(|| {
            // A scalar used as a pending value inherits the indentation context
            // from the key/sequence marker; standalone scalars fall back to
            // their own header line.
            if header_column == header_line_indent {
                super::PendingValueIndentContext {
                    base_indent: header_line_indent,
                    min_auto_indent: header_line_indent,
                }
            } else {
                super::PendingValueIndentContext {
                    base_indent: header_line_indent,
                    min_auto_indent: header_line_indent.saturating_add(1),
                }
            }
        });
        let mut content_indent = header.indent.map(|indent| {
            indent_context
                .base_indent
                .saturating_add(usize::from(indent))
        });
        let min_auto_indent = if header_column == header_line_indent {
            // Top-level block scalars may start at the header indent; nested
            // pending values must stay under their parent context.
            header_line_indent.min(indent_context.min_auto_indent)
        } else {
            indent_context.min_auto_indent
        };
        let mut leading_empty_lines: Vec<(usize, Span)> = Vec::new();

        // A valid block scalar header must end before content begins. If it
        // does not, treat this as an empty scalar and let chomping normalize it.
        if !self.consume_line_break_at_current() {
            Self::apply_chomping_in_place(&mut value, header, true);
            return (value, Span::from_usize_range(token_start..self.byte_pos));
        }

        while let Some((line_start_span, indent, line_start, line_end, next_line_pos)) =
            self.peek_next_block_scalar_line()
        {
            // Document markers at column zero terminate the scalar and must be
            // lexed by the outer token stream.
            if self.is_document_marker_at(line_start, indent) {
                self.pending_tokens.push_back(super::RichToken::new(
                    Token::LineStart(crate::span::usize_to_indent(indent)),
                    line_start_span,
                ));
                self.byte_pos = line_start;
                break;
            }

            let has_content = line_end > line_start;
            let line_text = self.input.get(line_start..line_end).unwrap_or_default();
            let has_tab_in_prefix = line_text
                .bytes()
                .take_while(|byte| matches!(byte, b' ' | b'\t'))
                .any(|byte| byte == b'\t');
            // Before auto-indent is known, the first non-empty line must be
            // indented far enough to belong to this scalar. Otherwise it is a
            // sibling line and scalar scanning stops before consuming it.
            if content_indent.is_none() && has_content && indent < min_auto_indent {
                if has_tab_in_prefix {
                    // Tabs cannot satisfy indentation, but report them before
                    // returning control to the outer lexer.
                    self.add_error(ErrorKind::InvalidIndentation, line_start_span);
                }
                self.pending_tokens.push_back(super::RichToken::new(
                    Token::LineStart(crate::span::usize_to_indent(indent)),
                    line_start_span,
                ));
                self.byte_pos = line_start;
                break;
            }
            // Once the content indent is fixed, any non-empty line with a
            // smaller indent belongs to the parent structure.
            if let Some(ci) = content_indent
                && has_content
                && indent < ci
            {
                self.pending_tokens.push_back(super::RichToken::new(
                    Token::LineStart(crate::span::usize_to_indent(indent)),
                    line_start_span,
                ));
                self.byte_pos = line_start;
                break;
            }

            // Empty lines before the first content line are kept as scalar
            // content, but their indentation can only be validated after the
            // auto-detected content indent is known.
            if content_indent.is_none() && !has_content {
                leading_empty_lines.push((indent, line_start_span));
            }

            // The first non-empty content line establishes auto-indent and
            // lets us diagnose leading empty lines that were over-indented.
            if content_indent.is_none() && has_content {
                content_indent = Some(indent);
                for (empty_indent, empty_span) in &leading_empty_lines {
                    if *empty_indent > indent {
                        self.add_error(ErrorKind::InvalidIndentation, *empty_span);
                    }
                }
            }

            let ci = content_indent.unwrap_or(indent);
            let extra_indent = indent.saturating_sub(ci);
            let has_non_whitespace = line_text.bytes().any(|byte| !matches!(byte, b' ' | b'\t'));
            // A tab in the visible prefix is content only after the content
            // indent; before that it is an invalid indentation character.
            if has_tab_in_prefix && indent < content_indent.unwrap_or(min_auto_indent) {
                self.add_error(ErrorKind::InvalidIndentation, line_start_span);
            }
            // Folding depends on whether a physical line is empty, more
            // indented than the scalar body, or a normal content line.
            let line_type = if !has_content && extra_indent == 0 {
                BlockLineType::Empty
            } else if !has_non_whitespace || extra_indent > 0 || has_tab_in_prefix {
                BlockLineType::MoreIndent
            } else {
                BlockLineType::Normal
            };

            Self::append_block_scalar_line(
                &mut value,
                is_literal,
                line_type,
                &mut append_state,
                extra_indent,
                line_text,
            );

            had_any_line = true;
            // Spans should end at the last meaningful scalar byte; pure empty
            // lines still advance the lexer but may be excluded after chomping.
            if has_content || extra_indent > 0 {
                scalar_has_any_part = true;
                end_pos = line_end;
            } else {
                end_pos = next_line_pos;
            }

            self.byte_pos = next_line_pos;
        }

        // Folded scalars synthesize a final line break for a non-empty final
        // line; chomping then decides whether to keep, clip, or strip it.
        if !is_literal && had_any_line && append_state.prev_type != BlockLineType::Empty {
            value.push('\n');
        }

        Self::apply_chomping_in_place(&mut value, header, !scalar_has_any_part);
        // Empty/all-chomped scalars span through what was consumed, while
        // scalars with content report the last content byte before chomping.
        let span_end = if scalar_has_any_part {
            end_pos
        } else {
            self.byte_pos
        };
        (value, Span::from_usize_range(token_start..span_end))
    }

    /// Append one accepted content line according to literal or folded style.
    /// `line_text` is the source slice after removing the detected content indent.
    fn append_block_scalar_line(
        value: &mut String,
        is_literal: bool,
        line_type: BlockLineType,
        state: &mut BlockScalarAppendState,
        extra_indent: usize,
        line_text: &str,
    ) {
        if is_literal {
            Self::push_spaces(value, extra_indent);
            value.push_str(line_text);
            value.push('\n');
            return;
        }

        match line_type {
            BlockLineType::Empty => {
                if state.prev_type == BlockLineType::MoreIndent {
                    value.push('\n');
                }
                value.push('\n');
            }
            BlockLineType::MoreIndent => {
                if !value.is_empty() {
                    match state.prev_type {
                        BlockLineType::Normal | BlockLineType::MoreIndent => value.push('\n'),
                        BlockLineType::Empty => {
                            if state.last_content_type == Some(BlockLineType::Normal) {
                                value.push('\n');
                            }
                        }
                    }
                }
                Self::push_spaces(value, extra_indent);
                value.push_str(line_text);
                state.last_content_type = Some(BlockLineType::MoreIndent);
            }
            BlockLineType::Normal => {
                if !value.is_empty() {
                    match state.prev_type {
                        BlockLineType::Normal => value.push(' '),
                        BlockLineType::MoreIndent => value.push('\n'),
                        BlockLineType::Empty => {}
                    }
                }
                value.push_str(line_text);
                state.last_content_type = Some(BlockLineType::Normal);
            }
        }
        state.prev_type = line_type;
    }

    /// Append `count` ASCII spaces without allocating for common small indents.
    fn push_spaces(value: &mut String, count: usize) {
        const SPACES: &str = "                                ";
        if let Some(spaces) = SPACES.get(..count) {
            value.push_str(spaces);
        } else {
            for _ in 0..count {
                value.push(' ');
            }
        }
    }

    /// Consume one YAML line break (`\n`, `\r`, or `\r\n`) at the cursor.
    fn consume_line_break_at_current(&mut self) -> bool {
        let Some(ch) = self.peek() else {
            return false;
        };
        if !Self::is_newline(ch) {
            return false;
        }
        self.advance();
        if ch == '\r' && self.peek() == Some('\n') {
            self.advance();
        }
        true
    }

    /// Inspect the physical line at the cursor without moving it.
    ///
    /// Returns `(line_start_span, indent, line_start, line_end, next_line_pos)`,
    /// where `line_start..line_end` is the content after leading spaces and
    /// `next_line_pos` is just after the line break, or EOF.
    fn peek_next_block_scalar_line(&self) -> Option<(Span, usize, usize, usize, usize)> {
        let line_prefix_start = self.byte_pos;
        if line_prefix_start >= self.input.len() {
            return None;
        }

        let bytes = self.input.as_bytes();
        let mut line_start = line_prefix_start;
        while bytes.get(line_start) == Some(&b' ') {
            line_start += 1;
        }

        let indent = line_start.saturating_sub(line_prefix_start);
        let line_start_span = Span::from_usize_range(line_prefix_start..line_start);
        let mut line_end = line_start;
        while let Some(ch) = self
            .input
            .get(line_end..)
            .and_then(|tail| tail.chars().next())
        {
            if Self::is_newline(ch) {
                break;
            }
            line_end += ch.len_utf8();
        }

        let next_line_pos = match self
            .input
            .get(line_end..)
            .and_then(|tail| tail.chars().next())
        {
            Some('\r') if self.input.as_bytes().get(line_end + 1) == Some(&b'\n') => line_end + 2,
            Some(ch) if Self::is_newline(ch) => line_end + ch.len_utf8(),
            _ => line_end,
        };

        Some((line_start_span, indent, line_start, line_end, next_line_pos))
    }

    /// Check whether a zero-indented line starts a document marker that ends
    /// the current block scalar instead of becoming scalar content.
    fn is_document_marker_at(&self, pos: usize, indent: usize) -> bool {
        if indent != 0 {
            return false;
        }
        let Some(tail) = self.input.as_bytes().get(pos..) else {
            return false;
        };
        let marker = tail.starts_with(b"---") || tail.starts_with(b"...");
        if !marker {
            return false;
        }
        matches!(tail.get(3), None | Some(b' ' | b'\t' | b'\n' | b'\r'))
    }

    /// Apply strip/clip/keep chomping to the built scalar value without
    /// allocating another string.
    fn apply_chomping_in_place(
        value: &mut String,
        header: BlockScalarHeader,
        is_empty_scalar: bool,
    ) {
        fn trim_trailing_newlines_len(input: &str) -> usize {
            let bytes = input.as_bytes();
            let mut len = bytes.len();
            while len > 0 && bytes.get(len - 1) == Some(&b'\n') {
                len -= 1;
            }
            len
        }

        match header.chomping {
            Chomping::Strip => {
                value.truncate(trim_trailing_newlines_len(value));
            }
            Chomping::Clip => {
                if is_empty_scalar {
                    value.clear();
                    return;
                }
                if value.ends_with('\n') {
                    let trimmed_len = trim_trailing_newlines_len(value);
                    value.truncate(trimmed_len);
                    value.push('\n');
                } else if !value.is_empty() {
                    value.push('\n');
                }
            }
            Chomping::Keep => {}
        }
    }
}
