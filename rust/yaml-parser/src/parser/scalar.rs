// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Scalar parsing (plain, quoted, and block scalars).

use std::borrow::Cow;

use crate::error::ErrorKind;
use crate::event::{CollectionStyle, Event, ScalarStyle};
use crate::lexer::{BlockScalarHeader, Chomping, QuoteStyle, Token};
use crate::span::{IndentLevel, Span, indent_to_usize};
use crate::value::{Node, Value};

use super::{NodeProperties, Parser};

/// Result of checking a low-indent line for scalar continuation.
enum LowIndentResult {
    /// Found a continuation - keep looping
    Continue,
    /// Should return early with current content
    Return,
    /// Should break out of main loop
    Break,
}

/// Line type for folded block scalar processing.
/// Used to differentiate between normal lines (which fold) and "more indented" lines (which don't).
#[derive(Clone, Copy, PartialEq)]
enum FoldedLineType {
    /// Normal content line - folds with adjacent lines (newline becomes space)
    Normal,
    /// More-indented line - preserves leading whitespace and surrounding newlines
    MoreIndented,
    /// Empty line - always preserved as newline
    Empty,
}

impl<'tokens: 'input, 'input> Parser<'tokens, 'input> {
    /// Parse a simple scalar token.
    /// For mapping keys (typically single-line), uses `min_indent=0`.
    /// Parse a plain or quoted scalar.
    ///
    /// Returns `(Span, Value)` where Value is the inferred type.
    /// Emits: `Scalar` event with the appropriate style.
    pub fn parse_scalar(&mut self) -> Option<(Span, Value<'input>)> {
        self.parse_scalar_with_indent(0)
    }

    /// Parse a scalar token with a specified minimum indentation for continuations.
    /// This is used when parsing values where multi-line strings must respect indentation.
    ///
    /// Returns `(Span, Value)` where Value is the inferred type.
    /// Emits: `Scalar` event with the appropriate style.
    pub fn parse_scalar_with_indent(
        &mut self,
        min_indent: IndentLevel,
    ) -> Option<(Span, Value<'input>)> {
        let (tok, span) = self.peek()?;

        match tok {
            Token::Plain(string) => {
                // Check for reserved indicator `%` at column 0 starting a plain scalar.
                // Per YAML 1.2 spec production [22] c-indicator and [126] ns-plain-first,
                // `%` is a c-indicator and cannot start a plain scalar.
                // This is only an error at the START of a scalar, not in continuations
                // (e.g., XLQ9 where `%YAML 1.2` is a continuation of `scalar`).
                // We check if the token starts with `%` and is at column 0.
                let starts_with_percent = string.starts_with('%');
                let col = self.column_of_position(span.start_usize());

                // Clone the string value before we lose the borrow
                let string_value = string.to_string();
                let value = Self::scalar_to_value(string_value.clone());
                self.advance();

                // Report error after releasing the borrow from peek()
                if starts_with_percent && col == 0 {
                    self.error(ErrorKind::InvalidDirective, span);
                }

                // Emit Scalar event
                self.emit(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Owned(string_value),
                    anchor: None, // Caller handles properties
                    tag: None,
                    span,
                });

                Some((span, value))
            }
            Token::StringStart(_style) => self.parse_quoted_string(min_indent),
            _ => None,
        }
    }

    /// Parse a quoted string from `StringStart`/`StringContent`/`LineStart`/`StringEnd` tokens.
    /// Returns `(Span, Value)` or None if not at a `StringStart` token.
    /// Validates that continuation lines have proper indentation (>= `min_indent`).
    ///
    /// Implements YAML 1.2 flow folding rules:
    /// - Trailing whitespace before line breaks is trimmed
    /// - Single line break followed by content becomes a space
    /// - Multiple consecutive line breaks preserve (n-1) newlines
    /// - Leading whitespace on continuation lines is trimmed (handled by lexer)
    ///
    /// Emits: `Scalar` event with the appropriate quoted style.
    #[allow(clippy::too_many_lines, reason = "Complex quoted string parsing")]
    pub fn parse_quoted_string(
        &mut self,
        min_indent: IndentLevel,
    ) -> Option<(Span, Value<'input>)> {
        let (first_token, start_span) = self.peek()?;

        let style = match first_token {
            Token::StringStart(style) => *style,
            _ => return None,
        };
        self.advance(); // consume StringStart

        let mut content = String::new();
        let mut end_span = start_span;
        // Track consecutive newlines for flow folding
        let mut pending_newlines: usize = 0;

        loop {
            let Some((tok, span)) = self.peek() else {
                // Unexpected EOF - unterminated string
                self.errors.push(crate::error::ParseError::new(
                    ErrorKind::UnterminatedString,
                    Span::from_usize_range(
                        start_span.start_usize()
                            ..self
                                .tokens
                                .last()
                                .map_or(start_span.end_usize(), |rt| rt.span.end_usize()),
                    ),
                ));
                break;
            };

            match tok {
                Token::StringContent(string) => {
                    // Determine the actual content to add
                    // If this is a continuation line, the lexer has already stripped the
                    // "flow line prefix" (leading spaces and tabs). However, we still need
                    // to handle any residual spaces the lexer may not have stripped.
                    // Note: tabs are NOT stripped here because:
                    // 1. The lexer strips leading whitespace including tabs
                    // 2. Any tabs remaining are content (e.g., from \t escape sequences)
                    let string_to_add = if pending_newlines > 0 {
                        string.trim_start_matches(' ')
                    } else {
                        string.as_ref()
                    };

                    // Apply pending newlines before this content
                    // YAML 1.2: A single line break folds to a space. Multiple line breaks
                    // are preserved as (n-1) newlines.
                    if pending_newlines > 0 {
                        if pending_newlines == 1 {
                            // Single newline always folds to space, even at string start/end
                            content.push(' ');
                        } else {
                            // Multiple newlines: preserve (n-1) newlines
                            // First newline folds to nothing, rest become actual newlines
                            for _ in 1..pending_newlines {
                                content.push('\n');
                            }
                        }
                        pending_newlines = 0;
                    }
                    // Append content (trimming will happen when we see LineStart)
                    content.push_str(string_to_add);
                    end_span = span;
                    self.advance();
                }
                Token::LineStart(indent) => {
                    let indent = *indent;
                    self.advance();

                    // Trim trailing spaces from content before this line break.
                    // We only trim spaces, not tabs, because:
                    // 1. YAML uses spaces for indentation, so trailing spaces before
                    //    line breaks are likely formatting whitespace
                    // 2. Trailing tabs might come from \t escape sequences which are
                    //    content. After escape processing we can't distinguish escaped
                    //    vs natural tabs.
                    // 3. Natural trailing tabs before line breaks are uncommon
                    let trimmed_len = content.trim_end_matches(' ').len();
                    content.truncate(trimmed_len);

                    // Track this newline
                    pending_newlines += 1;

                    // Validate indentation for content lines
                    let is_content_line = matches!(self.peek(), Some((Token::StringContent(_), _)));
                    if is_content_line && indent < min_indent {
                        self.errors.push(crate::error::ParseError::new(
                            ErrorKind::InvalidIndentationContext {
                                expected: min_indent,
                                found: indent,
                            },
                            span,
                        ));
                    }
                }
                Token::StringEnd(end_style) => {
                    if *end_style != style {
                        self.errors.push(crate::error::ParseError::new(
                            ErrorKind::MismatchedQuotes,
                            span,
                        ));
                    }
                    // Apply any pending newlines at end of string
                    // Single newline before closing quote → trailing space
                    // Multiple newlines → (n-1) trailing newlines
                    if pending_newlines == 1 {
                        content.push(' ');
                    } else if pending_newlines > 1 {
                        for _ in 1..pending_newlines {
                            content.push('\n');
                        }
                    }
                    end_span = span;
                    self.advance();
                    break;
                }
                _ => {
                    self.errors.push(crate::error::ParseError::new(
                        ErrorKind::UnterminatedString,
                        span,
                    ));
                    break;
                }
            }
        }

        let full_span = Span::from_usize_range(start_span.start_usize()..end_span.end_usize());

        // Convert lexer QuoteStyle to event ScalarStyle
        let event_style = match style {
            QuoteStyle::Single => ScalarStyle::SingleQuoted,
            QuoteStyle::Double => ScalarStyle::DoubleQuoted,
        };

        // Emit Scalar event
        self.emit(Event::Scalar {
            style: event_style,
            value: Cow::Owned(content.clone()),
            anchor: None, // Caller handles properties
            tag: None,
            span: full_span,
        });

        Some((full_span, Value::String(content.into())))
    }

    /// Convert a plain scalar string to an appropriate Value.
    ///
    /// Per YAML 1.2 Core Schema, we only recognize:
    /// - null: null, Null, NULL, ~, empty
    /// - bool: true, True, TRUE, false, False, FALSE
    /// - int: decimal integers (no 0x hex or 0o octal - those are YAML 1.1)
    /// - float: decimal floats, .inf, -.inf, .nan
    /// - everything else is a string
    pub fn scalar_to_value(scalar: String) -> Value<'static> {
        match scalar.as_str() {
            "null" | "Null" | "NULL" | "~" | "" => Value::Null,
            "true" | "True" | "TRUE" => Value::Bool(true),
            "false" | "False" | "FALSE" => Value::Bool(false),
            _ => {
                // YAML 1.2 Core Schema: only decimal integers
                if let Ok(integer) = scalar.parse::<i64>() {
                    return Value::Int(integer);
                }
                // YAML 1.2 Core Schema: decimal floats and special values
                if let Ok(float) = scalar.parse::<f64>() {
                    return Value::Float(float);
                }
                match scalar.as_str() {
                    ".inf" | ".Inf" | ".INF" => return Value::Float(f64::INFINITY),
                    "-.inf" | "-.Inf" | "-.INF" => return Value::Float(f64::NEG_INFINITY),
                    ".nan" | ".NaN" | ".NAN" => return Value::Float(f64::NAN),
                    _ => {}
                }
                Value::String(scalar.into())
            }
        }
    }

    /// Parse an alias: *name
    ///
    /// Emits: `Alias` event.
    /// Parse an alias reference (*name).
    ///
    /// Returns the span of the alias if parsed, None otherwise.
    /// Emits: `Alias` event.
    pub fn parse_alias(&mut self) -> Option<Span> {
        let (tok, span) = self.advance()?;

        let name = if let Token::Alias(name) = tok {
            *name
        } else {
            return None;
        };

        if !self.anchors.contains(name) {
            self.error(ErrorKind::UndefinedAlias, span);
        }

        // Emit Alias event
        self.emit(Event::Alias {
            name: Cow::Borrowed(name),
            span,
        });

        Some(span)
    }

    /// Parse a literal block scalar: |
    ///
    /// The `min_indent` parameter is the minimum indentation for block content
    /// (typically the parent block's indent + 1). For explicit indentation indicators,
    /// the content indent is calculated relative to `min_indent - 1` (the parent's indent).
    ///
    /// Returns the span and content of the parsed scalar.
    /// Emits: `Scalar` event with `Literal` style.
    pub fn parse_literal_block_scalar(
        &mut self,
        min_indent: IndentLevel,
    ) -> Option<(Span, Cow<'input, str>)> {
        let (tok, span) = self.advance()?;
        let start = span.start_usize();

        let header = if let Token::LiteralBlockHeader(hdr) = tok {
            hdr.clone()
        } else {
            return None;
        };

        let (content, end) = self.collect_block_scalar_content(&header, true, min_indent);
        let scalar_span = Span::from_usize_range(start..end);

        // Emit Scalar event
        self.emit(Event::Scalar {
            style: ScalarStyle::Literal,
            value: Cow::Owned(content.clone()),
            anchor: None, // Caller handles properties
            tag: None,
            span: scalar_span,
        });

        Some((scalar_span, Cow::Owned(content)))
    }

    /// Parse a folded block scalar: >
    ///
    /// The `min_indent` parameter is the minimum indentation for block content
    /// (typically the parent block's indent + 1). For explicit indentation indicators,
    /// the content indent is calculated relative to `min_indent - 1` (the parent's indent).
    ///
    /// Returns the span and content of the parsed scalar.
    /// Emits: `Scalar` event with `Folded` style.
    pub fn parse_folded_block_scalar(
        &mut self,
        min_indent: IndentLevel,
    ) -> Option<(Span, Cow<'input, str>)> {
        let (tok, span) = self.advance()?;
        let start = span.start_usize();

        let header = if let Token::FoldedBlockHeader(hdr) = tok {
            hdr.clone()
        } else {
            return None;
        };

        let (content, end) = self.collect_block_scalar_content(&header, false, min_indent);
        let scalar_span = Span::from_usize_range(start..end);

        // Emit Scalar event
        self.emit(Event::Scalar {
            style: ScalarStyle::Folded,
            value: Cow::Owned(content.clone()),
            anchor: None, // Caller handles properties
            tag: None,
            span: scalar_span,
        });

        Some((scalar_span, Cow::Owned(content)))
    }

    /// Consume content tokens from a block line.
    ///
    /// Returns `(content_end, has_content, starts_with_whitespace)`:
    /// - `content_end`: byte position after the last content token (or `initial_end_pos` if none)
    /// - `has_content`: true if any actual content (not just structural tokens) was consumed
    /// - `starts_with_whitespace`: true if the first content token is whitespace (tab/space),
    ///   which makes this a "more-indented" line per YAML spec section 8.1.3
    ///
    /// This combines token consumption and empty-line detection in a single pass,
    /// eliminating the need for lookahead.
    ///
    /// Optimized to iterate directly over the token slice, avoiding per-iteration bounds checks.
    ///
    /// Note: For block scalars, trailing whitespace must be preserved. The lexer trims
    /// trailing whitespace from Plain tokens but consumes it without emitting a Whitespace
    /// token. We fix this by scanning the input directly to find whitespace between
    /// the last token's span end and the newline.
    fn consume_block_line_with_detection(&mut self, initial_end_pos: usize) -> (usize, bool, bool) {
        let Some(remaining) = self.tokens.get(self.pos..) else {
            return (initial_end_pos, false, false);
        };

        let mut end_pos = initial_end_pos;
        let mut has_content = false;
        let mut starts_with_whitespace = false;
        let mut consumed = 0;
        let mut newline_start_pos: Option<usize> = None;

        for rt in remaining {
            match &rt.token {
                // Skip structural tokens without updating end position or content flag
                Token::Dedent | Token::Indent(_) => {
                    consumed += 1;
                }
                // Line-ending tokens terminate the line; remember their start position
                Token::LineStart(_) | Token::DocStart | Token::DocEnd => {
                    newline_start_pos = Some(rt.span.start_usize());
                    break;
                }
                // Whitespace tokens: content, but also mark as "starts with whitespace" if first
                Token::Whitespace | Token::WhitespaceWithTabs => {
                    if !has_content {
                        starts_with_whitespace = true;
                    }
                    end_pos = rt.span.end_usize();
                    has_content = true;
                    consumed += 1;
                }
                // All other tokens are content
                _ => {
                    end_pos = rt.span.end_usize();
                    has_content = true;
                    consumed += 1;
                }
            }
        }

        // For block scalars, recover trailing whitespace that the lexer consumed but didn't
        // include in the Plain token span. Scan from end_pos to newline/EOF to find it.
        // The newline token's span starts at the newline character. Any characters
        // between end_pos and newline_pos are trailing whitespace to preserve.
        if let Some(newline_pos) = newline_start_pos {
            if has_content && newline_pos > end_pos {
                // Validate it's all whitespace (spaces/tabs)
                // Positions are from token spans, guaranteed to be UTF-8 boundaries
                #[allow(
                    clippy::string_slice,
                    reason = "positions from lexer spans are UTF-8 boundaries"
                )]
                let trailing = &self.input[end_pos..newline_pos];
                if trailing.chars().all(|ch| ch == ' ' || ch == '\t') {
                    end_pos = newline_pos;
                }
            }
        }

        self.pos += consumed;
        (end_pos, has_content, starts_with_whitespace)
    }

    /// Check if a line starting at `pos` is empty (no content tokens before next `LineStart`).
    /// Used for lookahead when checking if an under-indented line should break the block scalar.
    ///
    /// Returns `(is_empty, is_eof)`:
    /// - `is_empty`: true if no content tokens before next `LineStart` or EOF
    /// - `is_eof`: true if this empty line leads directly to EOF (no more content follows)
    fn is_line_empty_at(&self, pos: usize) -> (bool, bool) {
        let mut check_pos = pos;
        loop {
            match self.tokens.get(check_pos) {
                Some(rt) => match &rt.token {
                    Token::Dedent | Token::Indent(_) => check_pos += 1,
                    Token::LineStart(_) => return (true, false), // Empty but more content follows
                    _ => return (false, false),
                },
                None => return (true, true), // Empty and at EOF
            }
        }
    }

    /// Join line spans for literal block scalar (`|`). Preserves newlines between lines.
    #[allow(
        clippy::string_slice,
        reason = "spans from lexer are guaranteed to be valid UTF-8 boundaries"
    )]
    fn join_literal_spans(&self, spans: &[Span]) -> String {
        let Some((first, rest)) = spans.split_first() else {
            return String::new();
        };

        // Calculate total capacity needed: content + newlines (one per line)
        let content_len: usize = spans.iter().map(Span::len).sum();
        let newlines_len = spans.len(); // One newline per line

        let mut result = String::with_capacity(content_len + newlines_len);

        // Process first span
        result.push_str(&self.input[first.start_usize()..first.end_usize()]);
        result.push('\n');

        // Process remaining spans with newline after each
        for span in rest {
            result.push_str(&self.input[span.start_usize()..span.end_usize()]);
            result.push('\n');
        }

        result
    }

    /// Join line spans for folded block scalar (`>`).
    ///
    /// Folding rules per YAML 1.2 spec:
    /// - Normal lines: fold (single newline becomes space)
    /// - Empty lines: preserve as newlines
    /// - "More indented" lines: preserve with newlines, don't fold with surrounding content
    ///
    /// Key insight: More-indented blocks don't participate in folding. The line break
    /// before and after a more-indented block is preserved (not folded to space).
    #[allow(
        clippy::string_slice,
        reason = "spans from lexer are guaranteed to be valid UTF-8 boundaries"
    )]
    fn join_folded_spans(&self, spans: &[Span], line_types: &[FoldedLineType]) -> String {
        if spans.is_empty() {
            return String::new();
        }

        // Calculate total capacity (estimate: content + separators)
        let content_len: usize = spans.iter().map(Span::len).sum();

        let mut result = String::with_capacity(content_len + spans.len());
        let mut prev_type = FoldedLineType::Empty; // Treat start as if preceded by empty

        // YAML 1.2 folding rules (section 8.1.3):
        // - Line breaks are folded to a space UNLESS:
        //   1. They are followed by an empty line, OR
        //   2. They are followed by a more-indented line
        // - Line breaks after empty/more-indented lines are NOT folded
        //
        // Track last non-empty line type to handle Empty sequences correctly.
        // Start with None to indicate no content has been seen yet.
        let mut last_content_type: Option<FoldedLineType> = None;

        for (span, &line_type) in spans.iter().zip(line_types.iter()) {
            match line_type {
                FoldedLineType::Empty => {
                    // Empty lines contribute one newline.
                    // Additionally, if previous was MoreIndented, the line break between
                    // MoreIndented and Empty is NOT folded, so we need an extra \n.
                    if prev_type == FoldedLineType::MoreIndented {
                        result.push('\n');
                    }
                    result.push('\n');
                }
                FoldedLineType::MoreIndented => {
                    if !result.is_empty() {
                        match prev_type {
                            FoldedLineType::Normal | FoldedLineType::MoreIndented => {
                                // Line break before more-indented is NOT folded
                                result.push('\n');
                            }
                            FoldedLineType::Empty => {
                                // If coming from Normal context through Empty, we need
                                // the preserved line break into the more-indented block.
                                // If coming from MoreIndented context (or no prior content),
                                // Empty already added its \n.
                                if last_content_type == Some(FoldedLineType::Normal) {
                                    result.push('\n');
                                }
                            }
                        }
                    }
                    result.push_str(&self.input[span.start_usize()..span.end_usize()]);
                    last_content_type = Some(FoldedLineType::MoreIndented);
                }
                FoldedLineType::Normal => {
                    if !result.is_empty() {
                        match prev_type {
                            FoldedLineType::Normal => {
                                // Normal→Normal: line break IS folded to space
                                result.push(' ');
                            }
                            FoldedLineType::MoreIndented => {
                                // Line break AFTER MoreIndented: NOT folded
                                result.push('\n');
                            }
                            FoldedLineType::Empty => {
                                // Empty line already added its \n, no additional separator needed
                            }
                        }
                    }
                    result.push_str(&self.input[span.start_usize()..span.end_usize()]);
                    last_content_type = Some(FoldedLineType::Normal);
                }
            }
            prev_type = line_type;
        }

        // Add trailing newline for the final line (chomping will adjust as needed)
        if !result.is_empty() {
            result.push('\n');
        }

        result
    }

    /// Apply chomping rules to the final block scalar content.
    /// - `Strip`: Remove all trailing newlines.
    /// - `Clip`: Keep exactly one trailing newline.
    /// - `Keep`: Preserve all trailing newlines.
    ///
    /// Optimized to find the trim point once via `trim_end_matches`, avoiding
    /// repeated `ends_with`/`pop` loops.
    fn apply_chomping(content: &mut String, chomping: Chomping) {
        match chomping {
            Chomping::Strip => {
                let trimmed_len = content.trim_end_matches('\n').len();
                content.truncate(trimmed_len);
            }
            Chomping::Clip => {
                // Keep exactly one trailing newline
                let trimmed_len = content.trim_end_matches('\n').len();
                content.truncate(trimmed_len);
                if !content.is_empty() {
                    content.push('\n');
                }
            }
            Chomping::Keep => {
                // Ensure at least one trailing newline
                if !content.is_empty() && !content.ends_with('\n') {
                    content.push('\n');
                }
            }
        }
    }

    /// Collect block scalar content, respecting indentation.
    /// Uses span-based collection for efficiency: instead of building strings per line,
    /// we collect spans pointing to the original input and build the final string at the end.
    ///
    /// Optimized to avoid lookahead: we consume tokens first, then determine if the line
    /// was empty based on whether content was found.
    ///
    /// Per YAML 1.2 spec section 8.1.1.1: If a block scalar has an indentation indicator,
    /// then the content indentation level equals the indentation level of the block scalar
    /// plus the integer value of the indentation indicator.
    ///
    /// The `min_indent` parameter is the minimum indent for content (typically parent + 1).
    /// For explicit indicators, we use `min_indent - 1` as the block scalar's indentation,
    /// since the indicator is relative to the parent block's indentation level.
    pub fn collect_block_scalar_content(
        &mut self,
        header: &BlockScalarHeader,
        literal: bool,
        min_indent: IndentLevel,
    ) -> (String, usize) {
        // Pre-allocate for typical block scalar sizes (4-8 lines common)
        // For folded scalars, we also track line type to handle "more indented" lines
        let mut line_spans: Vec<Span> = Vec::with_capacity(8);
        let mut line_types: Vec<FoldedLineType> = Vec::with_capacity(8);
        // Per YAML spec 8.1.1.1: content_indent = block_scalar_indent + explicit_indicator
        // The block_scalar_indent is min_indent - 1 (the parent block's indentation)
        let mut content_indent: Option<usize> = header
            .indent
            .map(|i| usize::from(min_indent.saturating_sub(1)) + usize::from(i));
        let mut end_pos = self.current_span().end_usize();

        // Track whitespace-only lines before the first content line.
        // These must have at most as many spaces as the first content line's indentation.
        // Typically 0-2 empty lines before content, so small capacity is fine.
        let mut empty_lines_before_content: Vec<(usize, Span)> = Vec::new();

        loop {
            // Skip any Dedent/Indent tokens before LineStart
            while let Some((Token::Dedent | Token::Indent(_), _)) = self.peek() {
                self.advance();
            }

            let Some((Token::LineStart(indent_level), ls_span)) = self.peek() else {
                break;
            };
            let (n, line_start_span) = (indent_to_usize(*indent_level), ls_span);

            // Check if this line should terminate the block scalar.
            let (is_empty, is_eof) = self.is_line_empty_at(self.pos + 1);

            // An empty line at indent 0 leading to EOF is the structural terminator, not content.
            // But indented empty lines before EOF are still content (they represent a trailing
            // line break that should be preserved with keep chomping).
            if is_empty && is_eof && n == 0 {
                break;
            }

            // For under-indented lines with established content, break on non-empty lines
            if let Some(required_indent) = content_indent
                && n < required_indent
                && !is_empty
            {
                break;
            }

            // Check for tabs used as indentation BEFORE breaking.
            // This ensures we still emit errors for invalid tab indentation even when
            // the block scalar terminates early.
            // Look at the token following the LineStart to check for tabs.
            if n == 0
                && let Some(rt) = self.peek_nth(1)
                && matches!(rt.token, Token::WhitespaceWithTabs)
            {
                self.error(ErrorKind::InvalidIndentation, rt.span);
            }

            // For block scalars with no content yet: a non-empty line at or below the parent
            // indent level (min_indent - 1) cannot be block scalar content - it must be
            // the next sibling node. For example, empty block scalars like:
            //   strip: >-
            //
            //   clip: >
            // The "clip:" is at the same indent as "strip:", so it's not block scalar content.
            if content_indent.is_none() && !is_empty && n < usize::from(min_indent) {
                break;
            }

            self.advance(); // consume LineStart
            end_pos = line_start_span.end_usize();

            // Consume content tokens and detect if line has content
            let (content_end, has_content, starts_with_ws) =
                self.consume_block_line_with_detection(end_pos);
            end_pos = content_end;

            if has_content {
                // Content line - discover indentation from first content line
                if content_indent.is_none() {
                    content_indent = Some(n);
                    // Validate preceding empty lines
                    for (empty_indent, empty_span) in &empty_lines_before_content {
                        if *empty_indent > n {
                            self.error(ErrorKind::InvalidIndentation, *empty_span);
                        }
                    }
                }

                // Calculate content start including extra indent
                let base_indent = content_indent.unwrap_or(1);
                let extra_indent = n.saturating_sub(base_indent);
                let content_start = line_start_span.end_usize() - extra_indent;

                line_spans.push(Span::from_usize_range(content_start..content_end));
                // Line is "more indented" if it has extra whitespace beyond base,
                // OR if the content starts with whitespace (tab or space per YAML spec)
                let line_type = if extra_indent > 0 || starts_with_ws {
                    FoldedLineType::MoreIndented
                } else {
                    FoldedLineType::Normal
                };
                line_types.push(line_type);
            } else {
                // Empty line (no content tokens)
                // Check for "more indented" whitespace-only lines - these preserve extra spaces
                let base_indent = content_indent.unwrap_or(0);
                if n > base_indent && content_indent.is_some() {
                    // More-indented blank line: the extra spaces are content
                    let extra_indent = n - base_indent;
                    let content_start = line_start_span.end_usize() - extra_indent;
                    line_spans.push(Span::from_usize_range(
                        content_start..line_start_span.end_usize(),
                    ));
                    line_types.push(FoldedLineType::MoreIndented);
                } else {
                    // Truly empty line
                    if content_indent.is_none() {
                        empty_lines_before_content.push((n, line_start_span));
                    }
                    line_spans.push(Span::from_usize_range(end_pos..end_pos));
                    line_types.push(FoldedLineType::Empty);
                }
            }
        }

        // Join line spans and apply chomping
        let mut content = if literal {
            self.join_literal_spans(&line_spans)
        } else {
            self.join_folded_spans(&line_spans, &line_types)
        };
        Self::apply_chomping(&mut content, header.chomping);

        (content, end_pos)
    }

    /// Convert multiline scalar content to final value.
    /// Returns `(Span, Value)` tuple.
    fn finalize_multiline_scalar(
        content: String,
        had_continuations: bool,
        start: usize,
        end: usize,
    ) -> (Span, Value<'static>) {
        // If we had continuations, always treat as string (no type coercion)
        // If no continuations, apply normal scalar type detection
        let value = if had_continuations {
            Value::String(content.into())
        } else {
            Self::scalar_to_value(content)
        };
        (Span::from_usize_range(start..end), value)
    }

    /// Parse a scalar and check if it's actually a mapping key.
    pub fn parse_scalar_or_mapping(&mut self, min_indent: IndentLevel) -> Option<Node<'input>> {
        // Remember if the scalar we're about to parse is a quoted scalar, and its start position
        let is_quoted_scalar = matches!(self.peek(), Some((Token::StringStart(_), _)));
        let scalar_start_pos = self.pos;

        // Use min_indent for proper continuation validation in quoted strings
        let scalar = self.parse_scalar_with_indent(min_indent)?;

        // Skip whitespace and track if we saw a comment
        // A comment terminates a plain scalar, so we shouldn't look for continuations
        let mut saw_comment = false;
        while let Some((tok, _)) = self.peek() {
            match tok {
                Token::Whitespace | Token::WhitespaceWithTabs | Token::Indent(_) => {
                    self.advance();
                }
                Token::Comment(_) => {
                    saw_comment = true;
                    self.advance();
                }
                _ => break,
            }
        }

        // Check if this is a mapping key (followed by colon)
        if let Some((Token::Colon, colon_span)) = self.peek() {
            // Check for multiline implicit key (invalid in block context)
            let (scalar_span, _scalar_value) = scalar;
            self.check_multiline_implicit_key(scalar_span.start_usize(), scalar_span.end_usize());

            // Check for invalid nested mapping on same line.
            // Pattern like `a: b: c` or `a: 'b': c` is invalid
            let is_nested_value_position = {
                let mut found_implicit_key_colon = false;
                for i in (0..scalar_start_pos).rev() {
                    #[allow(
                        clippy::indexing_slicing,
                        reason = "i is from range 0..scalar_start_pos"
                    )]
                    match &self.tokens[i].token {
                        Token::Colon => {
                            let mut has_key_before_colon = false;
                            for j in (0..i).rev() {
                                #[allow(clippy::indexing_slicing, reason = "j is from range 0..i")]
                                match &self.tokens[j].token {
                                    Token::LineStart(_) | Token::MappingKey => {
                                        break;
                                    }
                                    Token::Plain(_)
                                    | Token::StringStart(_)
                                    | Token::StringEnd(_)
                                    | Token::FlowSeqEnd
                                    | Token::FlowMapEnd => {
                                        has_key_before_colon = true;
                                        break;
                                    }
                                    _ => {}
                                }
                            }
                            if has_key_before_colon {
                                found_implicit_key_colon = true;
                            }
                        }
                        Token::LineStart(_) => {
                            break;
                        }
                        _ => {}
                    }
                }
                found_implicit_key_colon
            };

            if is_nested_value_position {
                self.error(ErrorKind::UnexpectedColon, colon_span);
                return Some(Node::null(scalar_span));
            }

            // This is actually a mapping - reparse
            let start = scalar_span.start_usize();
            let start_span = scalar_span;
            let map_indent = self.column_of_position(start);

            // The key scalar event was already emitted by parse_scalar_with_indent.
            // We need to insert MappingStart BEFORE that scalar event.
            // Pop the last event (the key scalar), emit MappingStart, then re-emit the scalar.
            let key_event = self.events.pop();
            self.emit(Event::MappingStart {
                style: CollectionStyle::Block,
                anchor: None, // Properties are handled by caller
                tag: None,
                span: start_span,
            });
            if let Some(ev) = key_event {
                self.events.push(ev);
            }
            // Key event already emitted - no need to track Node

            // Track this mapping's indentation for orphan indent detection
            self.push_indent(map_indent);

            self.advance(); // consume ':'
            self.check_tabs_after_block_indicator();
            self.skip_ws();

            // Check for block sequence indicator on same line as key - invalid in YAML
            if let Some((Token::BlockSeqIndicator, span)) = self.peek() {
                self.error(ErrorKind::ContentOnSameLine, span);
            }

            // Parse the value
            // Track whether this is a same-line scalar (not a block collection spanning lines)
            let mut check_trailing = false;
            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.check_tabs_as_indentation();
                // We crossed a line boundary - pass this info so anchors/tags
                // can bridge indentation gaps (e.g., SKE5 test case)
                let props = NodeProperties {
                    crossed_line_boundary: true,
                    ..Default::default()
                };
                self.parse_value_with_properties(map_indent + 1, props)
            } else if let Some((Token::Anchor(_) | Token::Tag(_), _)) = self.peek() {
                // Collect properties on same line, then check for block collection on next line
                // This handles 57H4: `key: !!seq\n- entry`
                let props = self.collect_node_properties(NodeProperties::default());

                // Check if we have LineStart followed by block collection at lower indentation
                let line_indent = if let Some((Token::LineStart(n), _)) = self.peek() {
                    Some(*n)
                } else {
                    None
                };

                if let Some(n) = line_indent {
                    if n < map_indent + 1 {
                        let saved_pos = self.pos;
                        self.advance(); // past LineStart
                        while let Some((Token::Dedent, _)) = self.peek() {
                            self.advance();
                        }

                        if let Some((
                            Token::BlockSeqIndicator | Token::MappingKey | Token::Colon,
                            _,
                        )) = self.peek()
                        {
                            // Block collection follows - parse it with properties and bridging
                            // Don't check trailing content - block collections span multiple lines
                            let mut new_props = props;
                            new_props.crossed_line_boundary = true;
                            self.parse_value_with_properties(n, new_props)
                        } else {
                            // No block collection - restore and continue normally
                            self.pos = saved_pos;
                            self.parse_value_with_properties(map_indent + 1, props)
                        }
                    } else {
                        // LineStart at valid indentation - continue normally
                        self.parse_value_with_properties(map_indent + 1, props)
                    }
                } else {
                    // No LineStart - content is on same line with tag/anchor
                    // This is a same-line scalar like `key: !!int 42`
                    check_trailing = true;
                    self.parse_value_with_properties(map_indent + 1, props)
                }
            } else {
                // Value directly on same line (no anchor/tag) - check for trailing content
                check_trailing = true;
                self.parse_value(map_indent + 1)
            };

            // Value event was emitted by parse_value - just check trailing content
            let _value_span = value
                .map(|n| n.span)
                .unwrap_or_else(|| self.emit_null_scalar());

            // Check for trailing content after same-line scalars in block context
            // Don't check after block collections that span multiple lines
            if check_trailing {
                self.check_trailing_content_after_scalar();
            }

            // Continue parsing more key-value pairs at same indentation
            self.parse_remaining_mapping_entries(map_indent);

            // Pop the indent level we pushed for this mapping
            self.pop_indent();

            // Use the last event's end position for span calculation
            let end = self.last_event_end_position().max(start);

            // Emit MappingEnd event
            self.emit(Event::MappingEnd {
                span: Span::from_usize_range(end..end),
            });

            Some(Node::null(Span::from_usize_range(start..end)))
        } else {
            // Just a scalar - check for multiline continuation (plain scalars only)
            // Note: If we saw a comment after the scalar, it terminates the plain scalar
            // and we should NOT look for continuation lines (YAML spec section 7.3.3)
            let (scalar_span, _scalar_value) = scalar;
            if is_quoted_scalar || saw_comment {
                // Comment terminated the scalar - check for trailing content at root
                if min_indent == 0 && saw_comment {
                    self.check_trailing_content_at_root(0);
                }
                Some(Node::null(scalar_span))
            } else {
                let (span, _value) = self
                    .consume_plain_scalar_continuations((scalar_span, _scalar_value), min_indent);
                Some(Node::null(span))
            }
        }
    }

    /// Helper to parse remaining mapping entries at the same indentation level.
    /// Emits key-value events directly without collecting into a pairs vector.
    fn parse_remaining_mapping_entries(&mut self, map_indent: IndentLevel) {
        loop {
            let loop_start_pos = self.pos;

            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            // Check if we're already at a token that can start a mapping entry at the correct indent
            // (this can happen after parse_explicit_mapping_entry which leaves us at the next entry)
            let already_at_entry = {
                let is_entry_token = matches!(
                    self.peek(),
                    Some((
                        Token::Plain(_)
                            | Token::StringStart(_)
                            | Token::MappingKey
                            | Token::Colon
                            | Token::Anchor(_)
                            | Token::Tag(_)
                            | Token::Alias(_),
                        _
                    ))
                );
                // Only consider it a valid entry if it's at the correct column
                is_entry_token && self.current_token_column() == map_indent
            };

            // Advance to next line at same indent, or break if we can't
            if !already_at_entry && !self.advance_to_same_indent(map_indent) {
                break;
            }

            // Handle explicit key indicator (?)
            if let Some((Token::MappingKey, _)) = self.peek() {
                if self.parse_explicit_mapping_entry(map_indent) {
                    // Entry was parsed and events emitted
                    continue;
                }
                break;
            }

            // Handle empty key (: at start of line)
            if let Some((Token::Colon, _colon_span)) = self.peek() {
                // Emit scalar event for the null key
                self.emit_null_scalar();
                self.advance();

                // Skip whitespace only (not comments) - check for Whitespace/WhitespaceWithTabs
                while let Some((Token::Whitespace | Token::WhitespaceWithTabs, _)) = self.peek() {
                    self.advance();
                }

                // If there's a comment, the value is null (comment terminates)
                if let Some((Token::Comment(_), _)) = self.peek() {
                    self.advance();
                    self.emit_null_scalar();
                    continue;
                }

                // If we hit LineStart immediately, value is null
                if let Some((Token::LineStart(_), _)) = self.peek() {
                    self.emit_null_scalar();
                    continue;
                }

                // Otherwise, parse the value
                let value = self.parse_value(map_indent + 1);
                if value.is_none() {
                    self.emit_null_scalar();
                }
                continue;
            }

            // Try to parse an implicit key-value pair
            if !self.parse_implicit_mapping_entry(map_indent) {
                break;
            }

            // Ensure progress
            if self.pos == loop_start_pos && !self.is_eof() {
                break;
            }
        }
    }

    /// Parse an explicit mapping entry (? key : value).
    /// Returns true if entry was parsed, false otherwise.
    /// Emits key and value events directly.
    fn parse_explicit_mapping_entry(&mut self, map_indent: IndentLevel) -> bool {
        // Consume the ?
        self.advance();
        self.skip_ws();

        // Parse the key (may be on same line or next line)
        let key = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            while let Some((Token::Dedent, _)) = self.peek() {
                self.advance();
            }
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        if key.is_none() {
            self.emit_null_scalar();
        }

        // Skip to colon (may be on next line)
        self.skip_ws();
        while let Some((Token::LineStart(line_n), _)) = self.peek() {
            if *line_n < map_indent {
                break;
            }
            self.advance();
            while let Some((Token::Dedent, _)) = self.peek() {
                self.advance();
            }
        }

        // Expect colon
        if !matches!(self.peek(), Some((Token::Colon, _))) {
            self.emit_null_scalar();
            return true;
        }
        self.advance();
        self.skip_ws();

        // Parse the value
        let value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            while let Some((Token::Dedent, _)) = self.peek() {
                self.advance();
            }
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        if value.is_none() {
            self.emit_null_scalar();
        }

        true
    }

    /// Advance to the next line at the specified indent level.
    /// Returns true if we're now at a line with the target indent, false otherwise.
    fn advance_to_same_indent(&mut self, target_indent: IndentLevel) -> bool {
        loop {
            match self.peek() {
                Some((Token::LineStart(n), span)) => {
                    let n = *n;
                    let span = span;
                    if n < target_indent {
                        // Check for orphan indentation: n is not in the parser's indent stack
                        // and is between valid levels (e.g., indent 1 when stack is [0, 2])
                        if !self.is_valid_indent(n) {
                            self.error(ErrorKind::InvalidIndentation, span);
                        }
                        return false;
                    }
                    if n == target_indent {
                        self.advance();
                        while let Some((Token::Dedent, _)) = self.peek() {
                            self.advance();
                        }
                        self.check_tabs_as_indentation();

                        self.skip_ws();

                        // Skip any additional LineStart tokens (e.g., from blank lines)
                        while let Some((Token::LineStart(next_indent), _)) = self.peek() {
                            if *next_indent != target_indent {
                                break;
                            }
                            self.advance();
                        }

                        self.skip_ws();
                        return true;
                    }
                    // n > target_indent: this is an over-indented line
                    // Check if this is invalid orphan indentation (content at unexpected level)
                    // by checking if there's actual content (not just comments)
                    let saved_pos = self.pos;
                    self.advance(); // past LineStart
                    // Skip Dedent/Indent tokens
                    while let Some((Token::Dedent | Token::Indent(_), _)) = self.peek() {
                        self.advance();
                    }
                    // Check what's on this line
                    if let Some((tok, _)) = self.peek() {
                        match tok {
                            // These are valid on over-indented lines (comments, blank lines)
                            Token::Comment(_) | Token::LineStart(_) => {
                                // Continue looking - this is just a comment or blank line
                            }
                            // Content that would start a new entry at wrong indent
                            Token::Plain(_)
                            | Token::StringStart(_)
                            | Token::MappingKey
                            | Token::Colon
                            | Token::Anchor(_)
                            | Token::Tag(_) => {
                                // This is content at an orphan indent - report error
                                self.error(ErrorKind::InvalidIndentation, span);
                            }
                            _ => {}
                        }
                    }
                    // Restore position and skip past this line to continue looking
                    self.pos = saved_pos;
                    self.advance();
                }
                // Skip Dedent, Comment, and Indent tokens - they don't affect our search
                // for a LineStart at the target indent level
                Some((Token::Dedent | Token::Comment(_) | Token::Indent(_), _)) => {
                    self.advance();
                }
                _ => return false,
            }
        }
    }

    /// Parse a single implicit mapping entry (key: value).
    /// Returns false if no valid entry could be parsed.
    /// Emits key and value events directly.
    fn parse_implicit_mapping_entry(&mut self, map_indent: IndentLevel) -> bool {
        // Collect any anchor/tag properties before the key
        let key_props = self.collect_node_properties(NodeProperties::default());

        // Parse the key (scalar or alias)
        let Some(key_span) = self.parse_mapping_key_or_alias(key_props) else {
            return false;
        };

        self.skip_ws();

        // Expect colon after key
        if let Some((Token::Colon, _)) = self.peek() {
            self.check_multiline_implicit_key(key_span.start_usize(), key_span.end_usize());
            self.advance();
        } else {
            self.error(ErrorKind::MissingColon, key_span);
            return false;
        }

        self.skip_ws();

        // Check for block sequence indicator on same line as key - invalid
        if let Some((Token::BlockSeqIndicator, span)) = self.peek() {
            self.error(ErrorKind::ContentOnSameLine, span);
        }

        let value_on_same_line = !matches!(self.peek(), Some((Token::LineStart(_), _)));
        self.parse_mapping_value(map_indent);

        if value_on_same_line {
            self.check_trailing_content_after_scalar();
        }

        true
    }

    /// Parse a mapping key that can be a scalar or alias, with optional properties.
    /// Returns None if no valid key could be parsed.
    /// Emits key event directly, returns key span for error reporting.
    fn parse_mapping_key_or_alias(&mut self, key_props: NodeProperties<'input>) -> Option<Span> {
        if let Some((Token::Alias(name), span)) = self.peek() {
            // Alias key - properties on aliases are invalid
            let alias_name = *name;
            if !key_props.is_empty() {
                self.error(ErrorKind::PropertiesOnAlias, span);
            }
            self.advance();
            if !self.anchors.contains(alias_name) {
                self.error(ErrorKind::UndefinedAlias, span);
            }
            // Emit Alias event for the key
            self.emit(Event::Alias {
                name: Cow::Borrowed(alias_name),
                span,
            });
            Some(span)
        } else if let Some((span, _value)) = self.parse_scalar() {
            // Scalar key - apply properties if present (event already emitted by parse_scalar)
            if !key_props.is_empty() {
                self.apply_properties_to_events(&key_props);
                if let Some((name, _)) = &key_props.anchor {
                    self.anchors.insert(name);
                }
            }
            Some(span)
        } else {
            // Couldn't parse a key - report orphaned properties error
            if let Some((_, anchor_span)) = key_props.anchor {
                self.error(ErrorKind::OrphanedProperties, anchor_span);
            } else if let Some((_, tag_span)) = key_props.tag {
                self.error(ErrorKind::OrphanedProperties, tag_span);
            }
            None
        }
    }

    /// Append the appropriate separator for folded content.
    ///
    /// In plain scalars, single newlines fold to spaces, but multiple consecutive
    /// newlines are preserved (minus one, which becomes the space).
    fn append_folded_separator(content: &mut String, consecutive_newlines: usize) {
        if consecutive_newlines == 0 {
            content.push(' ');
        } else {
            for _ in 0..consecutive_newlines {
                content.push('\n');
            }
        }
    }

    /// Consume tokens until `LineStart` and return the text content with its end position.
    ///
    /// Used to collect a line of content when the token structure is complex
    /// (e.g., anchor/tag/alias tokens mixed with content).
    #[allow(
        clippy::string_slice,
        reason = "Positions are from lexer tokens and guaranteed to be on UTF-8 boundaries"
    )]
    fn consume_line_as_text(
        &mut self,
        start_pos: usize,
        initial_end: usize,
    ) -> (&'input str, usize) {
        let mut line_end = initial_end;
        while let Some((tok, tok_span)) = self.peek() {
            if let Token::LineStart(_) = tok {
                break;
            }
            line_end = tok_span.end_usize();
            self.advance();
        }
        let line_text = &self.input[start_pos..line_end];
        (line_text.trim_end(), line_end)
    }

    /// Handle a low-indent line during plain scalar continuation.
    ///
    /// When we see a `LineStart` with indent less than `min_indent`, it could be:
    /// - An empty/blank line (should continue collecting)
    /// - A mapping key on the next line (should stop)
    /// - End of the scalar (should stop)
    ///
    /// This function performs the lookahead and may consume tokens if it finds
    /// a valid continuation.
    #[allow(
        clippy::indexing_slicing,
        reason = "Token positions are validated by parser logic before access"
    )]
    fn handle_low_indent_continuation(
        &mut self,
        min_indent: IndentLevel,
        content: &mut String,
        end: &mut usize,
        consecutive_newlines: &mut usize,
        had_continuations: &mut bool,
    ) -> LowIndentResult {
        let mut next_pos = self.pos + 1;
        while next_pos < self.tokens.len() {
            match &self.tokens[next_pos].token {
                Token::Dedent | Token::Indent(_) => {
                    next_pos += 1;
                }
                Token::LineStart(next_indent) if *next_indent >= min_indent => {
                    // Found a valid continuation after empty line(s)
                    *consecutive_newlines += 1;
                    *had_continuations = true;
                    self.advance();
                    while matches!(self.peek(), Some((Token::Dedent, _))) {
                        self.advance();
                    }
                    return LowIndentResult::Continue;
                }
                Token::Whitespace | Token::WhitespaceWithTabs => {
                    // Tab/whitespace at start of line followed by content
                    let after_ws = next_pos + 1;
                    if after_ws < self.tokens.len() {
                        // Check if this is a mapping key - if so, stop
                        if self.is_mapping_key_at_position(after_ws) {
                            return LowIndentResult::Return;
                        }

                        match &self.tokens[after_ws].token {
                            Token::Plain(_)
                            | Token::Anchor(_)
                            | Token::Tag(_)
                            | Token::Alias(_)
                            | Token::BlockSeqIndicator => {
                                Self::append_folded_separator(content, *consecutive_newlines);

                                self.advance(); // consume LineStart
                                while matches!(self.peek(), Some((Token::Dedent, _))) {
                                    self.advance();
                                }
                                self.advance(); // consume Whitespace (tab)

                                match self.peek() {
                                    Some((Token::Plain(string), plain_span)) => {
                                        let continuation = string.clone();
                                        *end = plain_span.end_usize();
                                        content.push_str(&continuation);
                                        self.advance();
                                    }
                                    Some((
                                        Token::Anchor(_)
                                        | Token::Tag(_)
                                        | Token::Alias(_)
                                        | Token::BlockSeqIndicator,
                                        span,
                                    )) => {
                                        let (line_text, line_end) = self.consume_line_as_text(
                                            span.start_usize(),
                                            span.end_usize(),
                                        );
                                        content.push_str(line_text);
                                        *end = line_end;
                                    }
                                    _ => {}
                                }

                                *had_continuations = true;
                                *consecutive_newlines = 0;
                                return LowIndentResult::Continue;
                            }
                            _ => {}
                        }
                    }
                    return LowIndentResult::Return;
                }
                _ => {
                    return LowIndentResult::Return;
                }
            }
        }
        // Reached end of tokens without finding continuation
        LowIndentResult::Break
    }

    /// Extract the string content from a scalar node for multiline continuation.
    ///
    /// Returns `None` if the node is not a scalar type (e.g., Mapping or Sequence).
    /// For typed scalars like `null`, `true`, `false`, returns the original text representation
    /// so that multiline continuations produce correct results (e.g., `null\n  d` → `null d`).
    fn extract_scalar_content(value: &Value<'_>) -> Option<String> {
        match value {
            Value::String(string) => Some(string.clone().into_owned()),
            Value::Bool(boolean) => Some(boolean.to_string()),
            Value::Int(integer) => Some(integer.to_string()),
            Value::Float(float) => Some(float.to_string()),
            Value::Null => Some("null".to_owned()),
            _ => None,
        }
    }

    /// Skip whitespace tokens starting at `start_pos` and return the position of the first
    /// non-whitespace content token.
    ///
    /// Used to find the actual content position after `LineStart` when checking for mapping keys.
    #[allow(clippy::indexing_slicing, reason = "start_pos is validated by caller")]
    fn skip_to_content_position(&self, start_pos: usize) -> usize {
        let mut pos = start_pos;
        while pos < self.tokens.len() {
            match &self.tokens[pos].token {
                Token::Whitespace | Token::WhitespaceWithTabs | Token::Indent(_) => {
                    pos += 1;
                }
                _ => break,
            }
        }
        pos
    }

    /// Check if a Plain token at `pos` is followed by a Colon (indicating a mapping key).
    ///
    /// This lookahead is used to stop plain scalar continuation when the next line
    /// looks like a mapping key (`key: value`).
    #[allow(
        clippy::indexing_slicing,
        reason = "pos is validated by caller before access"
    )]
    fn is_mapping_key_at_position(&self, pos: usize) -> bool {
        if pos >= self.tokens.len() {
            return false;
        }
        if !matches!(&self.tokens[pos].token, Token::Plain(_)) {
            return false;
        }
        let mut lookahead = pos + 1;
        while lookahead < self.tokens.len() {
            match &self.tokens[lookahead].token {
                Token::Whitespace | Token::WhitespaceWithTabs => lookahead += 1,
                Token::Colon => return true,
                _ => return false,
            }
        }
        false
    }

    /// Consume multiline continuation lines for a plain scalar.
    ///
    /// Called after the first line of a plain scalar has been parsed.
    /// Continuation lines must be more indented than the scalar's starting column.
    ///
    /// Note: The initial scalar event was already emitted by `parse_scalar_with_indent`.
    /// If continuations are found, we update the last emitted Scalar event with the
    /// full multiline content.
    #[allow(
        clippy::string_slice,
        reason = "Positions are from lexer tokens and guaranteed to be on UTF-8 boundaries"
    )]
    #[allow(
        clippy::indexing_slicing,
        reason = "Token positions are validated by parser logic before access"
    )]
    pub fn consume_plain_scalar_continuations(
        &mut self,
        initial: (Span, Value<'input>),
        block_min_indent: IndentLevel,
    ) -> (Span, Value<'static>) {
        let (initial_span, initial_value) = initial;
        let start = initial_span.start_usize();
        let mut end = initial_span.end_usize();

        // For plain scalar continuations, the minimum indent depends on context:
        // - At top level (block_min_indent == 0): continuations can be at column 0
        // - Inside a block: continuations must be at least at block_min_indent
        // Per YAML 1.2 spec section 7.3.1, plain scalars can span multiple lines
        // at the top level with continuations at the same indentation.
        let min_indent = block_min_indent;

        // Extract the initial content
        let Some(mut content) = Self::extract_scalar_content(&initial_value) else {
            // Not a scalar type, return as-is (convert to owned)
            return (initial_span, initial_value.into_owned());
        };

        // Track the index of the scalar event we need to update if continuations are found
        let initial_event_index = self.events.len().saturating_sub(1);

        // Track consecutive empty lines for folding
        let mut consecutive_newlines = 0;
        let mut had_continuations = false;

        while let Some((Token::LineStart(n), _)) = self.peek() {
            let indent = *n;

            // Check if this might be an empty line (LineStart(0) followed by LineStart)
            if indent < min_indent {
                match self.handle_low_indent_continuation(
                    min_indent,
                    &mut content,
                    &mut end,
                    &mut consecutive_newlines,
                    &mut had_continuations,
                ) {
                    LowIndentResult::Continue => continue,
                    LowIndentResult::Return => {
                        return self.finalize_multiline_scalar_with_event(
                            content,
                            had_continuations,
                            initial_event_index,
                            start,
                            end,
                        );
                    }
                    LowIndentResult::Break => break,
                }
            }

            // Check if the following content is a mapping key
            let content_pos = self.skip_to_content_position(self.pos + 1);
            if self.is_mapping_key_at_position(content_pos) {
                return self.finalize_multiline_scalar_with_event(
                    content,
                    had_continuations,
                    initial_event_index,
                    start,
                    end,
                );
            }

            // Valid continuation, consume the LineStart
            self.advance();

            // Skip indentation tokens - including Dedent which can appear when
            // we're returning to base indentation level (e.g., line 3 indented, line 4 not)
            while let Some((
                Token::Indent(_) | Token::Dedent | Token::Whitespace | Token::WhitespaceWithTabs,
                _,
            )) = self.peek()
            {
                self.advance();
            }

            match self.peek() {
                Some((Token::Plain(string), plain_span)) => {
                    let continuation = string.clone();
                    end = plain_span.end_usize();
                    had_continuations = true;

                    Self::append_folded_separator(&mut content, consecutive_newlines);
                    content.push_str(&continuation);
                    consecutive_newlines = 0;

                    self.advance();
                }
                Some((Token::LineStart(_), _)) => {
                    consecutive_newlines += 1;
                    had_continuations = true;
                }
                Some((
                    Token::Anchor(_) | Token::Tag(_) | Token::Alias(_) | Token::BlockSeqIndicator,
                    span,
                )) => {
                    let (continuation, line_end) =
                        self.consume_line_as_text(span.start_usize(), span.end_usize());

                    if !continuation.is_empty() {
                        had_continuations = true;

                        Self::append_folded_separator(&mut content, consecutive_newlines);
                        content.push_str(continuation);
                        consecutive_newlines = 0;
                        end = line_end;
                    }
                }
                _ => {
                    break;
                }
            }
        }

        self.finalize_multiline_scalar_with_event(
            content,
            had_continuations,
            initial_event_index,
            start,
            end,
        )
    }

    /// Update a previously emitted Scalar event with new content and span.
    /// Used when multiline plain scalar continuations are collected.
    fn update_scalar_event(&mut self, event_index: usize, content: &str, start: usize, end: usize) {
        if let Some(Event::Scalar { value, span, .. }) = self.events.get_mut(event_index) {
            *value = Cow::Owned(content.to_owned());
            *span = Span::from_usize_range(start..end);
        }
    }

    /// Finalize a multiline scalar AND update the corresponding event.
    /// This is an instance method that ensures the event buffer is updated when returning early.
    /// Returns `(Span, Value)` tuple.
    fn finalize_multiline_scalar_with_event(
        &mut self,
        content: String,
        had_continuations: bool,
        event_index: usize,
        start: usize,
        end: usize,
    ) -> (Span, Value<'static>) {
        if had_continuations {
            self.update_scalar_event(event_index, &content, start, end);
        }
        Self::finalize_multiline_scalar(content, had_continuations, start, end)
    }
}
