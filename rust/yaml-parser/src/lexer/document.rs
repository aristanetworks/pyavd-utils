// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML lexer.
//!
//! This lexer tokenizes YAML streams, including multi-document streams with
//! directives (`%YAML`, `%TAG`) and document markers (`---`, `...`).
//!
//! It is context-aware, tracking flow depth to properly tokenize characters
//! that have different meanings:
//! - In **block context** (`flow_depth` = 0): `,[]{}` are valid in plain scalars
//! - In **flow context** (`flow_depth` > 0): `,[]{}` are delimiters
//!
//! Uses `Cow<'input, str>` for zero-copy tokenization where possible.

use std::borrow::Cow;
use std::collections::VecDeque;

use crate::error::{ErrorKind, ParseError};
use crate::span::{IndentLevel, Span, Spanned};

use super::rich_token::RichToken;
use super::token::{BlockScalarHeader, Chomping, QuoteStyle, Token};

/// Check if a character is valid in an anchor/alias name.
/// Per YAML 1.2 spec, ns-anchor-char is any non-whitespace char
/// except c-flow-indicator: `[`, `]`, `{`, `}`, `,`
fn is_anchor_char(ch: char) -> bool {
    !ch.is_whitespace() && !matches!(ch, '[' | ']' | '{' | '}' | ',')
}

/// Lexer mode based on flow depth.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexMode {
    /// Block context - flow indicators are part of plain scalars
    Block,
    /// Flow context - flow indicators are delimiters
    Flow,
}

/// Iterator phase for the lexer state machine.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IteratorPhase {
    /// Haven't emitted the initial LineStart(0) yet
    Initial,
    /// Normal tokenization
    Running,
    /// Emitting final Dedent tokens at EOF
    FinalDedents,
    /// Iterator exhausted
    Done,
}

/// Lexer phase for multi-document stream handling.
///
/// Tracks where we are in the YAML stream structure to determine
/// what tokens are valid (e.g., directives only in prologue).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexerPhase {
    /// Before any document content - directives are valid here.
    /// This is either at the start of the stream or after `...` (document end).
    DirectivePrologue,
    /// Inside a document - directives are NOT valid here.
    /// `%` at column 0 is plain scalar content.
    InDocument,
}

/// YAML lexer state.
///
/// The lifetime `'input` refers to the input string being tokenized.
///
/// Implements `Iterator<Item = RichToken<'input>>` for streaming tokenization.
/// Errors are collected internally and retrieved via [`Lexer::take_errors`].
#[allow(
    clippy::struct_excessive_bools,
    reason = "state machine requires multiple boolean flags"
)]
pub struct Lexer<'input> {
    input: &'input str,
    /// Byte offset of current position in the input string.
    byte_pos: usize,
    /// Current flow depth (number of unclosed `{` or `[`)
    flow_depth: usize,
    /// Whether the previous token was a "JSON-like" value
    /// (quoted string, alias, flow end). After these, `:` is always
    /// a mapping indicator in flow context.
    prev_was_json_like: bool,
    /// Track if previous token was whitespace or line start - for comment validation.
    /// A `#` can only start a comment if preceded by whitespace or at line start.
    prev_was_separator: bool,
    /// Indentation stack for INDENT/DEDENT tokens (like Python).
    /// Starts with [0] representing the base indentation level.
    indent_stack: Vec<IndentLevel>,
    /// Pending tokens to be returned (used for multi-token constructs like quoted strings,
    /// and for INDENT/DEDENT tokens after `LineStart`)
    pending_tokens: VecDeque<RichToken<'input>>,
    /// Whether we're currently inside a quoted string (between `StringStart` and `StringEnd`).
    /// When true, we suppress INDENT/DEDENT emission for `LineStart` tokens.
    in_quoted_string: bool,
    /// Current phase of the iterator state machine
    phase: IteratorPhase,
    /// Collected errors during lexing
    errors: Vec<ParseError>,
    /// Current phase for multi-document stream handling.
    /// Determines whether directives are valid (in prologue) or not (in document).
    phase_state: LexerPhase,
    /// Whether we've seen a %YAML directive in the current document's prologue.
    /// Reset to false when entering a new document (on `DocEnd` or `DocStart` after content).
    has_yaml_directive: bool,
    /// Whether we've seen any directive in the current prologue (for "directive without document" error).
    has_directive_in_prologue: bool,
    /// Span of the first directive in the current prologue (for error reporting).
    first_directive_span: Option<Span>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            byte_pos: 0,
            flow_depth: 0,
            prev_was_json_like: false,
            prev_was_separator: true, // At start, we're at "line start"
            indent_stack: vec![0],    // Base indentation level
            pending_tokens: VecDeque::new(),
            in_quoted_string: false,
            phase: IteratorPhase::Initial,
            errors: Vec::new(),
            phase_state: LexerPhase::DirectivePrologue, // Start in directive prologue
            has_yaml_directive: false,
            has_directive_in_prologue: false,
            first_directive_span: None,
        }
    }

    /// Take collected errors.
    pub fn take_errors(&mut self) -> Vec<ParseError> {
        std::mem::take(&mut self.errors)
    }

    /// Record an error during lexing.
    fn add_error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError::new(kind, span));
    }

    fn mode(&self) -> LexMode {
        if self.flow_depth > 0 {
            LexMode::Flow
        } else {
            LexMode::Block
        }
    }

    /// Peek the current character without advancing.
    fn peek(&self) -> Option<char> {
        self.input.get(self.byte_pos..)?.chars().next()
    }

    /// Peek `n` characters ahead (0 = current character).
    fn peek_n(&self, n: usize) -> Option<char> {
        self.input.get(self.byte_pos..)?.chars().nth(n)
    }

    /// Advance to the next character and return the current one.
    fn advance(&mut self) -> Option<char> {
        let ch = self.input.get(self.byte_pos..)?.chars().next()?;
        self.byte_pos += ch.len_utf8();
        Some(ch)
    }

    fn current_span(&self, start: usize) -> Span {
        Span::from_usize_range(start..self.byte_pos)
    }

    /// Skip inline whitespace and return whether any tabs were found.
    fn skip_inline_whitespace_detecting_tabs(&mut self) -> bool {
        let mut has_tabs = false;
        while let Some(ch) = self.peek() {
            match ch {
                ' ' => {
                    self.advance();
                }
                '\t' => {
                    has_tabs = true;
                    self.advance();
                }
                _ => break,
            }
        }
        has_tabs
    }

    fn is_newline(ch: char) -> bool {
        matches!(ch, '\n' | '\r' | '\u{0085}' | '\u{2028}' | '\u{2029}')
    }

    fn is_flow_indicator(ch: char) -> bool {
        matches!(ch, ',' | '[' | ']' | '{' | '}')
    }

    /// Process a token after it's been produced: update lexer state.
    ///
    /// This handles flow depth tracking, quoted string context, and JSON-like detection.
    fn process_token(&mut self, token: &Token<'input>) {
        // Track flow depth
        match token {
            Token::FlowMapStart | Token::FlowSeqStart => {
                self.flow_depth += 1;
            }
            Token::FlowMapEnd | Token::FlowSeqEnd => {
                self.flow_depth = self.flow_depth.saturating_sub(1);
            }
            _ => {}
        }

        // Track quoted string context (between StringStart and StringEnd).
        // Inside quoted strings, we don't emit INDENT/DEDENT tokens.
        match token {
            Token::StringStart(_) => {
                self.in_quoted_string = true;
            }
            Token::StringEnd(_) => {
                self.in_quoted_string = false;
            }
            _ => {}
        }

        // Track if this token is "JSON-like" for colon indicator detection.
        // Whitespace, LineStart, and Comment tokens don't reset the flag -
        // they act as separators that preserve the "just saw JSON value" state.
        match token {
            Token::Whitespace
            | Token::WhitespaceWithTabs
            | Token::LineStart(_)
            | Token::Comment(_)
            | Token::StringStart(_)
            | Token::StringContent(_)
            | Token::Indent(_)
            | Token::Dedent
            | Token::YamlDirective(_)
            | Token::TagDirective(..)
            | Token::ReservedDirective(_) => {
                // Don't change prev_was_json_like
                // These are separators/structure tokens that allow comments to follow
                self.prev_was_separator = true;
            }
            Token::StringEnd(_) | Token::Alias(_) | Token::FlowMapEnd | Token::FlowSeqEnd => {
                self.prev_was_json_like = true;
                self.prev_was_separator = false;
            }
            _ => {
                self.prev_was_json_like = false;
                self.prev_was_separator = false;
            }
        }

        // Track lexer phase for multi-document streams.
        // Phase transitions:
        // - DirectivePrologue -> InDocument: when we see `---` or content
        // - InDocument -> DirectivePrologue: when we see `...` (document end)
        match self.phase_state {
            LexerPhase::DirectivePrologue => {
                match token {
                    Token::DocStart => {
                        // `---` starts a document - reset directive tracking for this doc
                        self.phase_state = LexerPhase::InDocument;
                        self.has_yaml_directive = false;
                        self.has_directive_in_prologue = false;
                        self.first_directive_span = None;
                    }
                    Token::YamlDirective(_)
                    | Token::TagDirective(..)
                    | Token::ReservedDirective(_)
                    | Token::Comment(_)
                    | Token::Whitespace
                    | Token::WhitespaceWithTabs
                    | Token::LineStart(_)
                    | Token::Indent(_)
                    | Token::Dedent => {
                        // These don't end the prologue
                    }
                    _ => {
                        // Any other content token starts an implicit document
                        // Reset directive tracking for this doc
                        self.phase_state = LexerPhase::InDocument;
                        self.has_yaml_directive = false;
                        self.has_directive_in_prologue = false;
                        self.first_directive_span = None;
                    }
                }
            }
            LexerPhase::InDocument => {
                if matches!(token, Token::DocEnd) {
                    // `...` ends the document, back to directive prologue
                    // Reset for the next document's prologue
                    self.phase_state = LexerPhase::DirectivePrologue;
                    self.has_yaml_directive = false;
                    self.has_directive_in_prologue = false;
                    self.first_directive_span = None;
                }
            }
        }
    }

    /// Queue INDENT/DEDENT tokens based on indentation change.
    /// Called after a LineStart(n) token is produced.
    /// Tokens are added to `pending_tokens` for subsequent iteration.
    ///
    /// Note: When `new_indent` doesn't match any level in the stack after dedenting,
    /// we push it as a new level without emitting an error. This is intentional because:
    /// - Block scalar content may have irregular indentation
    /// - Implicit block mappings inside sequences may not be tracked in the stack
    ///
    /// The parser is responsible for detecting semantically invalid indentation.
    fn queue_indent_dedent_tokens(&mut self, new_indent: IndentLevel, span: Span) {
        let current_indent = *self.indent_stack.last().unwrap_or(&0);

        if new_indent > current_indent {
            // Indent increased - push new level
            self.indent_stack.push(new_indent);
            self.pending_tokens
                .push_back(RichToken::new(Token::Indent(new_indent), span));
        } else if new_indent < current_indent {
            // Indent decreased - pop levels and emit Dedent for each
            while let Some(&top) = self.indent_stack.last() {
                if top <= new_indent {
                    break;
                }
                self.indent_stack.pop();
                self.pending_tokens
                    .push_back(RichToken::new(Token::Dedent, span));
            }

            // If new_indent doesn't match any level, push it as a new level.
            // This handles cases like block scalar content returning to parent level.
            let final_indent = *self.indent_stack.last().unwrap_or(&0);
            if new_indent != final_indent && new_indent > 0 {
                self.indent_stack.push(new_indent);
            }
        }
        // If new_indent == current_indent, no INDENT/DEDENT needed
    }

    /// Produce the next raw token (without INDENT/DEDENT processing).
    fn produce_next_token(&mut self) -> Option<RichToken<'input>> {
        let (token, span) = self.next_token()?;
        Some(RichToken::new(token, span))
    }

    /// Check if we're at column 0 (start of input or right after a newline).
    fn is_at_column_zero(&self) -> bool {
        if self.byte_pos == 0 {
            return true;
        }
        // Check if previous byte was a newline
        let prev_byte_pos = self.byte_pos.saturating_sub(1);
        if let Some(ch) = self.input.as_bytes().get(prev_byte_pos) {
            *ch == b'\n' || *ch == b'\r'
        } else {
            false
        }
    }

    // ========================================================================
    // Token lexing helpers - extracted from next_token() for clarity
    // ========================================================================

    /// Try to lex a document marker (`---` or `...`) at column 0.
    fn try_lex_document_marker(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        if !self.is_at_column_zero() {
            return None;
        }

        // Check for `---` followed by whitespace/newline/EOF
        if ch == '-' && self.peek_n(1) == Some('-') && self.peek_n(2) == Some('-') {
            let after = self.peek_n(3);
            if after.is_none()
                || after == Some(' ')
                || after == Some('\t')
                || Self::is_newline(after.unwrap_or('\n'))
            {
                self.advance(); // -
                self.advance(); // -
                self.advance(); // -
                let span = self.current_span(start);
                // Document markers are invalid in flow context
                if self.mode() == LexMode::Flow {
                    self.add_error(ErrorKind::DocumentMarkerInFlow, span);
                }
                return Some((Token::DocStart, span));
            }
        }

        // Check for `...` followed by whitespace/newline/EOF
        if ch == '.' && self.peek_n(1) == Some('.') && self.peek_n(2) == Some('.') {
            let after = self.peek_n(3);
            if after.is_none()
                || after == Some(' ')
                || after == Some('\t')
                || Self::is_newline(after.unwrap_or('\n'))
            {
                self.advance(); // .
                self.advance(); // .
                self.advance(); // .
                let span = self.current_span(start);
                // Document markers are invalid in flow context
                if self.mode() == LexMode::Flow {
                    self.add_error(ErrorKind::DocumentMarkerInFlow, span);
                }
                return Some((Token::DocEnd, span));
            }
        }

        None
    }

    /// Try to lex a directive (`%YAML`, `%TAG`, or reserved).
    ///
    /// Directives start with `%` at column 0 and continue to end of line.
    /// Returns the appropriate directive token.
    #[allow(
        clippy::string_slice,
        reason = "All positions are from byte-level scanning at UTF-8 boundaries"
    )]
    fn try_lex_directive(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        // Directives start with `%` at column 0, only in directive prologue
        if ch != '%'
            || !self.is_at_column_zero()
            || self.phase_state != LexerPhase::DirectivePrologue
        {
            return None;
        }

        // In flow context, `%` is not a directive starter
        if self.flow_depth > 0 {
            return None;
        }

        // Consume the `%`
        self.advance();

        // Read the directive name
        let name_start = self.byte_pos;
        while let Some(peek_ch) = self.peek() {
            if peek_ch.is_whitespace() {
                break;
            }
            self.advance();
        }
        let name = &self.input[name_start..self.byte_pos];

        // Skip whitespace after directive name
        while matches!(self.peek(), Some(' ' | '\t')) {
            self.advance();
        }

        // Read the directive value (rest of line, excluding comments)
        let value_start = self.byte_pos;
        while let Some(peek_ch) = self.peek() {
            if Self::is_newline(peek_ch) {
                break;
            }
            // Stop at comment (but only if preceded by whitespace)
            if peek_ch == '#' && self.byte_pos > value_start {
                let prev_byte = self.input.as_bytes().get(self.byte_pos - 1);
                if matches!(prev_byte, Some(b' ' | b'\t')) {
                    break;
                }
            }
            self.advance();
        }
        let value = self.input[value_start..self.byte_pos].trim();
        let span = self.current_span(start);

        // Track that we have a directive in this prologue (for "directive without document" check)
        if self.first_directive_span.is_none() {
            self.first_directive_span = Some(span);
        }
        self.has_directive_in_prologue = true;

        // Determine directive type
        let token = match name {
            "YAML" => {
                // Check for duplicate YAML directive in same document
                if self.has_yaml_directive {
                    self.add_error(ErrorKind::DuplicateDirective, span);
                }
                self.has_yaml_directive = true;

                // Validate YAML version format
                let is_valid = !value.is_empty()
                    && !value.contains(' ')
                    && !value.contains('\t')
                    && value.chars().all(|vc| vc.is_ascii_digit() || vc == '.');
                if !is_valid {
                    self.add_error(ErrorKind::InvalidDirective, span);
                }
                Token::YamlDirective(Cow::Borrowed(value))
            }
            "TAG" => {
                // `%TAG` directive: expects exactly two whitespace-separated parameters:
                // a tag handle (e.g. `!e!`) and a tag prefix (e.g. `tag:example,2000:`).
                let mut parts = value.split_whitespace();
                let handle = parts.next();
                let prefix = parts.next();
                let extra = parts.next();

                if let (Some(handle_str), Some(prefix_str), None) = (handle, prefix, extra) {
                    Token::TagDirective(handle_str, prefix_str)
                } else {
                    // Malformed TAG directive (wrong number of parameters).
                    // Report an error and treat it as a reserved/unknown directive
                    // so that it doesn't affect tag handle resolution.
                    self.add_error(ErrorKind::InvalidDirective, span);
                    let full_content = &self.input[name_start..self.byte_pos].trim_end();
                    Token::ReservedDirective(Cow::Borrowed(full_content))
                }
            }
            _ => {
                // Reserved directive: include the name in the value
                let full_content = &self.input[name_start..self.byte_pos].trim_end();
                Token::ReservedDirective(Cow::Borrowed(full_content))
            }
        };

        Some((token, span))
    }

    /// Try to lex a newline and subsequent indentation.
    fn try_lex_newline(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        if !Self::is_newline(ch) {
            return None;
        }

        self.advance();
        // Handle \r\n
        if ch == '\r' && self.peek() == Some('\n') {
            self.advance();
        }

        // Count indentation spaces only (tabs are NOT valid for indentation in YAML)
        let mut indent = 0;
        while self.peek() == Some(' ') {
            self.advance();
            indent += 1;
        }

        Some((Token::LineStart(indent), self.current_span(start)))
    }

    /// Try to lex inline whitespace.
    /// Returns `Token::Whitespace` for spaces only, or `Token::WhitespaceWithTabs` if tabs are present.
    fn try_lex_whitespace(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        if ch != ' ' && ch != '\t' {
            return None;
        }
        // Check if first char is a tab
        let first_is_tab = ch == '\t';
        // Continue consuming whitespace, detecting tabs
        let rest_has_tabs = self.skip_inline_whitespace_detecting_tabs();
        let has_tabs = first_is_tab || rest_has_tabs;
        let token = if has_tabs {
            Token::WhitespaceWithTabs
        } else {
            Token::Whitespace
        };
        Some((token, self.current_span(start)))
    }

    /// Try to lex a comment.
    fn try_lex_comment(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        if ch != '#' {
            return None;
        }

        if !self.prev_was_separator {
            // `#` without preceding whitespace is invalid - report error
            self.advance();
            let span = self.current_span(start);
            self.add_error(ErrorKind::InvalidComment, span);
            // Try to recover by consuming rest of line as if it were a comment
            while let Some(peek_ch) = self.peek() {
                if Self::is_newline(peek_ch) {
                    break;
                }
                self.advance();
            }
            return Some((Token::Comment(Cow::Borrowed("")), self.current_span(start)));
        }

        self.advance(); // consume #
        let content_start = self.byte_pos;
        while let Some(peek_ch) = self.peek() {
            if Self::is_newline(peek_ch) {
                break;
            }
            self.advance();
        }
        // Borrow directly from input (zero-copy)
        // Byte positions are always at UTF-8 boundaries (maintained by advance())
        let content = self
            .input
            .get(content_start..self.byte_pos)
            .unwrap_or_default();
        Some((
            Token::Comment(Cow::Borrowed(content)),
            self.current_span(start),
        ))
    }

    /// Try to lex a flow indicator (`{}[],`).
    fn try_lex_flow_indicator(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        match ch {
            '{' => {
                self.advance();
                Some((Token::FlowMapStart, self.current_span(start)))
            }
            '}' => {
                self.advance();
                Some((Token::FlowMapEnd, self.current_span(start)))
            }
            '[' => {
                self.advance();
                Some((Token::FlowSeqStart, self.current_span(start)))
            }
            ']' => {
                self.advance();
                Some((Token::FlowSeqEnd, self.current_span(start)))
            }
            ',' if self.mode() == LexMode::Flow => {
                self.advance();
                Some((Token::Comma, self.current_span(start)))
            }
            _ => None,
        }
    }

    /// Try to lex a block indicator (`-` or `?` followed by whitespace).
    fn try_lex_block_indicator(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        // Block sequence indicator: - followed by whitespace/newline
        if ch == '-' {
            if let Some(next) = self.peek_n(1) {
                if next == ' ' || next == '\t' || Self::is_newline(next) {
                    self.advance();
                    return Some((Token::BlockSeqIndicator, self.current_span(start)));
                }
            } else {
                // - at EOF
                self.advance();
                return Some((Token::BlockSeqIndicator, self.current_span(start)));
            }
        }

        // Explicit key indicator: ? followed by whitespace/newline
        if ch == '?' {
            if let Some(next) = self.peek_n(1) {
                if next == ' ' || next == '\t' || Self::is_newline(next) {
                    self.advance();
                    return Some((Token::MappingKey, self.current_span(start)));
                }
            } else {
                self.advance();
                return Some((Token::MappingKey, self.current_span(start)));
            }
        }

        None
    }

    /// Try to lex a colon as a mapping value indicator.
    fn try_lex_colon(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        if ch != ':' {
            return None;
        }

        let next = self.peek_n(1);
        let is_indicator = if self.prev_was_json_like && self.mode() == LexMode::Flow {
            // After a JSON-like value, : is ALWAYS an indicator in flow context
            true
        } else {
            match self.mode() {
                LexMode::Flow => {
                    // In flow context, : is indicator if followed by:
                    // - whitespace, newline, EOF, or flow indicator
                    next.is_none()
                        || next == Some(' ')
                        || next == Some('\t')
                        || next.is_some_and(Self::is_newline)
                        || next.is_some_and(Self::is_flow_indicator)
                }
                LexMode::Block => {
                    // In block context, : is indicator only if followed by whitespace/newline/EOF
                    next.is_none()
                        || next == Some(' ')
                        || next == Some('\t')
                        || next.is_some_and(Self::is_newline)
                }
            }
        };

        // Note: : starts a plain scalar if not an indicator - let caller handle it
        is_indicator.then(|| {
            self.advance();
            (Token::Colon, self.current_span(start))
        })
    }

    /// Try to lex an anchor (`&name`) or alias (`*name`).
    fn try_lex_anchor_or_alias(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        // Anchors: &name
        if ch == '&'
            && let Some(next) = self.peek_n(1)
            && is_anchor_char(next)
        {
            self.advance(); // consume &
            let name = self.consume_anchor_name();
            return Some((Token::Anchor(name), self.current_span(start)));
        }

        // Aliases: *name
        if ch == '*'
            && let Some(next) = self.peek_n(1)
            && is_anchor_char(next)
        {
            self.advance(); // consume *
            let name = self.consume_anchor_name();
            return Some((Token::Alias(name), self.current_span(start)));
        }

        None
    }

    /// Try to lex a block scalar header (`|` or `>`).
    fn try_lex_block_scalar_header(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<Spanned<Token<'input>>> {
        if ch == '|' {
            self.advance();
            let header = self.consume_block_header();
            return Some((Token::LiteralBlockHeader(header), self.current_span(start)));
        }
        if ch == '>' {
            self.advance();
            let header = self.consume_block_header();
            return Some((Token::FoldedBlockHeader(header), self.current_span(start)));
        }
        None
    }

    /// Try to lex a quoted scalar (`'...'` or `"..."`).
    fn try_lex_quoted_scalar(&mut self, start: usize, ch: char) -> Option<Spanned<Token<'input>>> {
        if ch == '\'' {
            return Some(self.consume_single_quoted(start));
        }
        if ch == '"' {
            return Some(self.consume_double_quoted(start));
        }
        None
    }

    /// Get the next token.
    ///
    /// This method dispatches to specialized helper methods for each token type.
    fn next_token(&mut self) -> Option<Spanned<Token<'input>>> {
        let start = self.byte_pos;
        let ch = self.peek()?;

        // Try each token type in order of precedence
        if let Some(token) = self.try_lex_document_marker(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_directive(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_newline(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_whitespace(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_comment(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_flow_indicator(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_block_indicator(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_colon(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_anchor_or_alias(start, ch) {
            return Some(token);
        }

        // Tags: !, !!type, !<uri>
        if ch == '!' {
            return Some(self.consume_tag(start));
        }

        if let Some(token) = self.try_lex_block_scalar_header(start, ch) {
            return Some(token);
        }
        if let Some(token) = self.try_lex_quoted_scalar(start, ch) {
            return Some(token);
        }

        // In directive prologue, content (other than directives/doc markers) is only valid at column 0.
        // Content NOT at column 0 is invalid trailing content after `...` on the same line.
        if self.phase_state == LexerPhase::DirectivePrologue && !self.is_at_column_zero() {
            // This is invalid trailing content after document end marker (e.g., "... invalid")
            // Consume to end of line and emit error
            while let Some(peek_ch) = self.peek() {
                if Self::is_newline(peek_ch) {
                    break;
                }
                self.advance();
            }
            let span = self.current_span(start);
            self.add_error(ErrorKind::TrailingContent, span);
            // Return a plain scalar token (with error attached) so parsing can continue
            #[allow(
                clippy::string_slice,
                reason = "byte positions are guaranteed to be on char boundaries"
            )]
            let content = &self.input[start..self.byte_pos];
            return Some((Token::Plain(Cow::Borrowed(content)), span));
        }

        // Default: plain scalar
        Some(self.consume_plain_scalar(start))
    }

    /// Consume an anchor name, borrowing directly from input (zero-copy).
    fn consume_anchor_name(&mut self) -> &'input str {
        // According to YAML 1.2 spec, anchor names (ns-anchor-char+) can contain:
        // - Any non-whitespace character except c-flow-indicator ([]{},)
        // This includes colons and other special characters!
        let name_start = self.byte_pos;
        while let Some(peek_ch) = self.peek() {
            if is_anchor_char(peek_ch) {
                self.advance();
            } else {
                break;
            }
        }
        // Borrow directly from input (zero-copy)
        // Byte positions are always at UTF-8 boundaries (maintained by advance())
        self.input
            .get(name_start..self.byte_pos)
            .unwrap_or_default()
    }

    fn consume_tag(&mut self, start: usize) -> Spanned<Token<'input>> {
        let tag_start = start; // Position of the '!'
        self.advance(); // consume !

        // Check for !! or !<
        match self.peek() {
            Some('!') => {
                // Secondary tag like !!str - can borrow the whole thing from input
                self.advance();
                while let Some(peek_ch) = self.peek() {
                    if peek_ch.is_whitespace() || Self::is_flow_indicator(peek_ch) {
                        break;
                    }
                    self.advance();
                }
                // Borrow from input: includes both ! characters
                #[allow(
                    clippy::string_slice,
                    reason = "tag_start and byte_pos are valid UTF-8 boundaries"
                )]
                let tag_slice = &self.input[tag_start..self.byte_pos];
                (
                    Token::Tag(Cow::Borrowed(tag_slice)),
                    self.current_span(start),
                )
            }
            Some('<') => {
                // Verbatim tag !<uri>
                // Mark with a leading '\0' so the parser knows not to expand it.
                // This internal marker is stripped by expand_tag().
                self.advance(); // consume '<'
                let mut tag = String::from("\0");
                while let Some(peek_ch) = self.peek() {
                    if peek_ch == '>' {
                        self.advance();
                        break;
                    }
                    tag.push(peek_ch);
                    self.advance();
                }
                (Token::Tag(Cow::Owned(tag)), self.current_span(start))
            }
            Some(ch) if Self::is_valid_tag_start_char(ch) => {
                // Regular tag !name - can borrow the whole thing from input
                while let Some(peek_ch) = self.peek() {
                    if peek_ch.is_whitespace() || Self::is_flow_indicator(peek_ch) {
                        break;
                    }
                    self.advance();
                }
                // Borrow from input: includes the ! prefix
                #[allow(
                    clippy::string_slice,
                    reason = "tag_start and byte_pos are valid UTF-8 boundaries"
                )]
                let tag_slice = &self.input[tag_start..self.byte_pos];
                (
                    Token::Tag(Cow::Borrowed(tag_slice)),
                    self.current_span(start),
                )
            }
            // ! followed by whitespace, EOF, or flow indicator is the non-specific tag
            Some(ch) if ch.is_whitespace() || Self::is_flow_indicator(ch) => {
                // Empty tag (non-specific tag `!`)
                // Borrow the single '!' from input
                #[allow(
                    clippy::string_slice,
                    reason = "tag_start and byte_pos are valid UTF-8 boundaries"
                )]
                let tag_slice = &self.input[tag_start..self.byte_pos];
                (
                    Token::Tag(Cow::Borrowed(tag_slice)),
                    self.current_span(start),
                )
            }
            None => {
                // ! at end of input is also a valid non-specific tag
                // Borrow the single '!' from input
                #[allow(
                    clippy::string_slice,
                    reason = "tag_start and byte_pos are valid UTF-8 boundaries"
                )]
                let tag_slice = &self.input[tag_start..self.byte_pos];
                (
                    Token::Tag(Cow::Borrowed(tag_slice)),
                    self.current_span(start),
                )
            }
            _ => {
                // `!` followed by non-tag character (like `!"#$%...`)
                // This is NOT a tag - treat the `!` and following content as plain scalar
                // Note: # is only a comment if preceded by whitespace, so we include it here
                let mut plain = String::from("!");
                let in_flow = self.mode() == LexMode::Flow;
                while let Some(peek_ch) = self.peek() {
                    // Stop at whitespace
                    if peek_ch.is_whitespace() {
                        break;
                    }
                    // Flow indicators only terminate in flow mode
                    if in_flow && Self::is_flow_indicator(peek_ch) {
                        break;
                    }
                    // Colon ends plain scalar if followed by whitespace (or EOF)
                    if peek_ch == ':' {
                        let next = self.peek_n(1);
                        if next.is_none() || next.is_some_and(char::is_whitespace) {
                            break;
                        }
                    }
                    plain.push(peek_ch);
                    self.advance();
                }
                // Tag fallback to plain scalar requires owned string
                (Token::Plain(Cow::Owned(plain)), self.current_span(start))
            }
        }
    }

    /// Check if a character is valid at the start of a tag name.
    /// Valid tag characters are URI characters: alphanumerics and certain punctuation.
    fn is_valid_tag_start_char(ch: char) -> bool {
        ch.is_alphanumeric()
            || matches!(
                ch,
                '-' | '_' | '.' | '~' | '%' | '/' | ':' | '@' | '&' | '=' | '+' | '$' | ',' | ';'
            )
    }

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
                if !saw_whitespace {
                    // Comment without preceding whitespace is invalid
                    has_invalid_content = true;
                }
                // Consume comment regardless (for error recovery)
                while let Some(ch) = self.peek() {
                    if Self::is_newline(ch) {
                        break;
                    }
                    self.advance();
                }
                break;
            }
            // Invalid content on same line as block scalar indicator
            has_invalid_content = true;
            self.advance();
        }

        if has_invalid_content {
            let span = Span::from_usize_range(error_start..self.byte_pos);
            self.add_error(ErrorKind::ContentOnSameLine, span);
        }

        BlockScalarHeader { indent, chomping }
    }

    /// Check if the current position has a forbidden document marker (`---` or `...`)
    /// at column 0 followed by whitespace/newline/EOF (YAML spec production c-forbidden [206]).
    /// Returns true if a forbidden marker was detected, and reports an error.
    fn check_forbidden_marker_in_quoted(&mut self) -> bool {
        // Only check at column 0 (indent 0, no leading spaces consumed yet)
        // The caller should check that we're at column 0 before calling this.

        // Check for `---` or `...` followed by whitespace/newline/EOF
        let ch0 = self.peek();
        let ch1 = self.peek_n(1);
        let ch2 = self.peek_n(2);
        let ch3 = self.peek_n(3);

        let is_marker = matches!(
            (ch0, ch1, ch2),
            (Some('-'), Some('-'), Some('-')) | (Some('.'), Some('.'), Some('.'))
        );

        if !is_marker {
            return false;
        }

        // Check that the marker is followed by whitespace, newline, or EOF
        let followed_by_break = ch3.is_none()
            || ch3 == Some(' ')
            || ch3 == Some('\t')
            || Self::is_newline(ch3.unwrap_or('\0'));

        if followed_by_break {
            // Report the error - this is a forbidden marker inside a quoted string
            let span = Span::from_usize_range(self.byte_pos..self.byte_pos + 3);
            self.add_error(ErrorKind::DocumentMarkerInScalar, span);
            return true;
        }

        false
    }

    /// Shared tail logic for handling a newline in quoted strings.
    ///
    /// Assumes any per-style content handling (such as trimming) has already
    /// been applied and the current `content` flushed if needed.
    ///
    /// Consumes the newline and indentation, emits a `LineStart` token, and
    /// returns the new `content_start` position.
    fn finish_quoted_newline(&mut self) -> usize {
        // Consume newline
        let newline_start = self.byte_pos;
        let ch = self.advance().unwrap();
        if ch == '\r' && self.peek() == Some('\n') {
            self.advance();
        }

        // Count indentation (spaces only per YAML spec s-indent)
        let mut indent = 0;
        while self.peek() == Some(' ') {
            self.advance();
            indent += 1;
        }

        // In flow folding, skip the entire "s-separate-in-line" prefix.
        // This is "s-white+" which includes BOTH spaces and tabs (interleaved).
        // Note: escaped tabs (\t) are processed before newline handling, so they
        // end up in the previous StringContent, not here.
        while matches!(self.peek(), Some(' ' | '\t')) {
            self.advance();
        }

        // Check for forbidden document markers at column 0 (c-forbidden [206])
        if indent == 0 {
            self.check_forbidden_marker_in_quoted();
        }

        // Emit LineStart token
        let line_span = Span::from_usize_range(newline_start..self.byte_pos);
        self.pending_tokens
            .push_back(RichToken::new(Token::LineStart(indent), line_span));

        self.byte_pos
    }

    /// Handle a newline within a quoted string.
    ///
    /// Emits the current content as a `StringContent` token (if non-empty),
    /// then delegates to `finish_quoted_newline` to consume the newline and
    /// emit the corresponding `LineStart` token. Returns the new
    /// `content_start` position.
    fn handle_quoted_newline(&mut self, content: &mut String, content_start: usize) -> usize {
        // Emit current content before newline
        if !content.is_empty() {
            let content_span = Span::from_usize_range(content_start..self.byte_pos);
            // Quoted strings always use Cow::Owned (escape processing)
            self.pending_tokens.push_back(RichToken::new(
                Token::StringContent(Cow::Owned(std::mem::take(content))),
                content_span,
            ));
        }

        self.finish_quoted_newline()
    }

    /// Handle newline in double-quoted strings, trimming trailing whitespace while preserving escaped content.
    ///
    /// `protected_len` is the content length that should not be trimmed (includes escaped characters).
    /// Trailing whitespace beyond this length will be trimmed.
    fn handle_quoted_newline_trimmed(
        &mut self,
        content: &mut String,
        content_start: usize,
        protected_len: usize,
    ) -> usize {
        // Trim trailing whitespace, but only beyond the protected length.
        // The protected_len is always at a character boundary (set after push_str).
        #[allow(
            clippy::string_slice,
            reason = "protected_len is always at char boundary"
        )]
        if content.len() > protected_len {
            let trimmable = &content[protected_len..];
            let trimmed = trimmable.trim_end_matches([' ', '\t']);
            let new_len = protected_len + trimmed.len();
            content.truncate(new_len);
        }

        // Emit current content before newline
        if !content.is_empty() {
            let content_span = Span::from_usize_range(content_start..self.byte_pos);
            self.pending_tokens.push_back(RichToken::new(
                Token::StringContent(Cow::Owned(std::mem::take(content))),
                content_span,
            ));
        }

        self.finish_quoted_newline()
    }

    /// Emit final string tokens after the main loop.
    ///
    /// Emits the remaining content (if any) and the `StringEnd` token.
    /// Reports an error if the string was not terminated.
    fn finalize_quoted_string(
        &mut self,
        start: usize,
        content: String,
        content_start: usize,
        terminated: bool,
        style: QuoteStyle,
    ) {
        // Emit final content segment if any
        if !content.is_empty() {
            let content_span = Span::from_usize_range(content_start..self.byte_pos);
            // Quoted strings always use Cow::Owned (escape processing)
            self.pending_tokens.push_back(RichToken::new(
                Token::StringContent(Cow::Owned(content)),
                content_span,
            ));
        }

        // Emit StringEnd
        let end_span = Span::from_usize_range(self.byte_pos.saturating_sub(1)..self.byte_pos);
        if !terminated {
            let full_span = self.current_span(start);
            self.add_error(ErrorKind::UnterminatedString, full_span);
        }
        self.pending_tokens
            .push_back(RichToken::new(Token::StringEnd(style), end_span));
    }

    /// Consume a single-quoted string, emitting `StringStart`, `StringContent`, `LineStart` and `StringEnd` tokens.
    /// Pushes tokens to `pending_tokens` and returns the first token.
    fn consume_single_quoted(&mut self, start: usize) -> Spanned<Token<'input>> {
        let start_span = Span::from_usize_range(start..start + 1);
        self.advance(); // consume opening '

        let mut content = String::new();
        let mut content_start = self.byte_pos;
        let mut terminated = false;

        loop {
            match self.peek() {
                None => break, // Unterminated
                Some('\'') => {
                    self.advance();
                    // Check for escaped quote ''
                    if self.peek() == Some('\'') {
                        content.push('\'');
                        self.advance();
                    } else {
                        terminated = true;
                        break;
                    }
                }
                Some('\n' | '\r') => {
                    content_start = self.handle_quoted_newline(&mut content, content_start);
                }
                Some(ch) => {
                    content.push(ch);
                    self.advance();
                }
            }
        }

        self.finalize_quoted_string(
            start,
            content,
            content_start,
            terminated,
            QuoteStyle::Single,
        );

        // Return StringStart as the immediate token
        (Token::StringStart(QuoteStyle::Single), start_span)
    }

    /// Consume a double-quoted string, emitting `StringStart`, `StringContent`, `LineStart` and `StringEnd` tokens.
    /// Pushes tokens to `pending_tokens` and returns the first token.
    fn consume_double_quoted(&mut self, start: usize) -> Spanned<Token<'input>> {
        let start_span = Span::from_usize_range(start..start + 1);
        self.advance(); // consume opening "

        let mut content = String::new();
        let mut content_start = self.byte_pos;
        let mut terminated = false;
        // Track the content length after the last non-escape character.
        // When we see a newline, we trim trailing whitespace but only up to this position.
        // This preserves escaped whitespace like \t while trimming literal trailing whitespace.
        let mut escape_protected_len = 0usize;

        loop {
            match self.peek() {
                None => break,
                Some('"') => {
                    self.advance();
                    terminated = true;
                    break;
                }
                Some('\\') => {
                    let escape_start = self.byte_pos;
                    self.advance();
                    if let Some(escaped) = self.consume_escape_sequence(escape_start) {
                        content.push_str(&escaped);
                        // The entire content including this escape is protected
                        escape_protected_len = content.len();
                    }
                }
                Some('\n' | '\r') => {
                    content_start = self.handle_quoted_newline_trimmed(
                        &mut content,
                        content_start,
                        escape_protected_len,
                    );
                    escape_protected_len = 0;
                }
                Some(ch) => {
                    content.push(ch);
                    // Track protection: non-whitespace chars protect all content before them
                    if !ch.is_ascii_whitespace() {
                        escape_protected_len = content.len();
                    }
                    // Whitespace doesn't update protection - can be trimmed if at end
                    self.advance();
                }
            }
        }

        self.finalize_quoted_string(
            start,
            content,
            content_start,
            terminated,
            QuoteStyle::Double,
        );

        // Return StringStart as the immediate token
        (Token::StringStart(QuoteStyle::Double), start_span)
    }

    fn consume_escape_sequence(&mut self, start_byte_pos: usize) -> Option<String> {
        let ch = self.advance()?;
        let result = match ch {
            '0' => String::from("\0"),
            'a' => String::from("\x07"),
            'b' => String::from("\x08"),
            't' | '\t' => String::from("\t"),
            'n' => String::from("\n"),
            'v' => String::from("\x0B"),
            'f' => String::from("\x0C"),
            'r' => String::from("\r"),
            'e' => String::from("\x1B"),
            ' ' => String::from(" "),
            '"' => String::from("\""),
            '/' => String::from("/"),
            '\\' => String::from("\\"),
            'N' => String::from("\u{0085}"),
            '_' => String::from("\u{00A0}"),
            'L' => String::from("\u{2028}"),
            'P' => String::from("\u{2029}"),
            'x' => self.consume_hex_escape(2),
            'u' => self.consume_hex_escape(4),
            'U' => self.consume_hex_escape(8),
            '\n' | '\r' => {
                // Line continuation - skip whitespace on next line
                while matches!(self.peek(), Some(' ' | '\t' | '\n' | '\r')) {
                    self.advance();
                }
                String::new()
            }
            _ => {
                // Invalid escape sequence - report error
                let span = Span::from_usize_range(start_byte_pos..self.byte_pos);
                self.add_error(ErrorKind::InvalidEscape(ch), span);
                // Still return the escaped char for error recovery
                ch.to_string()
            }
        };
        Some(result)
    }

    fn consume_hex_escape(&mut self, digits: usize) -> String {
        let mut hex = String::new();
        for _ in 0..digits {
            if let Some(peek_ch) = self.peek() {
                if peek_ch.is_ascii_hexdigit() {
                    hex.push(peek_ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }
        if let Ok(code) = u32::from_str_radix(&hex, 16)
            && let Some(ch) = char::from_u32(code)
        {
            return ch.to_string();
        }
        format!("\\x{hex}") // Invalid escape - keep as-is
    }

    /// Consume a plain scalar, respecting the current mode.
    ///
    /// Optimized for zero-copy: borrows directly from the input string and
    /// trims trailing whitespace by adjusting the end position rather than
    /// allocating a new string.
    fn consume_plain_scalar(&mut self, start: usize) -> Spanned<Token<'input>> {
        let content_start = self.byte_pos;
        let mut at_start = true;
        // Track the end of non-whitespace content for trailing whitespace trimming
        let mut content_end_non_ws = content_start;

        while let Some(ch) = self.peek() {
            // Always stop at newlines
            if Self::is_newline(ch) {
                break;
            }

            // Handle special indicators at start: -, ?, :
            // These can only start a plain scalar if followed by a "safe" character
            // In flow context, flow indicators are not safe
            if at_start && (ch == '-' || ch == '?' || ch == ':') {
                let next = self.peek_n(1);
                // Check if next is "safe" for plain scalar start
                let is_safe = match next {
                    None => false, // EOF not safe
                    Some(n) => {
                        !(n.is_whitespace()
                            || Self::is_newline(n)
                            || (self.mode() == LexMode::Flow && Self::is_flow_indicator(n)))
                    }
                };
                if !is_safe {
                    // Cannot start plain scalar with this character - emit error and skip it
                    let span = self.current_span(start);
                    // In flow mode, block indicators like `-` are invalid
                    let error_kind = if self.mode() == LexMode::Flow {
                        ErrorKind::BlockIndicatorInFlow
                    } else {
                        ErrorKind::InvalidCharacter
                    };
                    self.add_error(error_kind, span);
                    self.advance(); // Consume the invalid character to avoid infinite loop
                    return (Token::Plain(Cow::Borrowed("")), self.current_span(start));
                }
            }

            // Handle colon - behavior differs between block and flow mode
            if ch == ':' {
                if self.mode() == LexMode::Flow {
                    // In flow mode:
                    // - At the start of plain scalar, : can be included if followed by
                    //   non-whitespace and non-flow-indicator (e.g., :x, ://, etc.)
                    // - In the middle, : terminates if followed by whitespace/flow-indicator/EOF
                    let next = self.peek_n(1);
                    let colon_terminates = next.is_none()
                        || next == Some(' ')
                        || next == Some('\t')
                        || next.is_some_and(Self::is_newline)
                        || next.is_some_and(Self::is_flow_indicator);

                    if colon_terminates && !at_start {
                        // In the middle and : is followed by terminator -> stop
                        break;
                    } else if colon_terminates && at_start {
                        // At start but : is followed by terminator -> empty scalar, stop
                        break;
                    }
                    // Otherwise, : is followed by non-terminator, consume it
                } else {
                    // In block mode, only stop if followed by whitespace/newline/EOF
                    if let Some(next) = self.peek_n(1) {
                        if next == ' ' || next == '\t' || Self::is_newline(next) {
                            break;
                        }
                    } else {
                        // : at EOF
                        break;
                    }
                }
            }

            // Space followed by # is a comment start
            if (ch == ' ' || ch == '\t') && self.peek_n(1) == Some('#') {
                break;
            }

            // Flow indicators - depends on mode
            if Self::is_flow_indicator(ch) && self.mode() == LexMode::Flow {
                break;
            }
            // In block mode, flow indicators are part of plain scalar

            self.advance();
            at_start = false;

            // Track end of non-whitespace for trailing whitespace trimming
            if !ch.is_whitespace() {
                content_end_non_ws = self.byte_pos;
            }
        }

        // Zero-copy: borrow directly from input, trimmed at content_end_non_ws
        // byte_pos is always at UTF-8 boundaries (maintained by advance())
        let content = self
            .input
            .get(content_start..content_end_non_ws)
            .unwrap_or("");
        // Use content_end_non_ws for span end to match the trimmed content
        // (don't include trailing whitespace that was consumed but not part of the value)
        (
            Token::Plain(Cow::Borrowed(content)),
            Span::from_usize_range(start..content_end_non_ws),
        )
    }
}

/// Iterator implementation for streaming tokenization.
///
/// The lexer yields tokens one at a time. Errors are collected internally
/// and can be retrieved via [`Lexer::take_errors`]. This enables streaming
/// processing without buffering all tokens in memory.
impl<'input> Iterator for Lexer<'input> {
    type Item = RichToken<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.phase {
                IteratorPhase::Initial => {
                    // Emit initial LineStart(0)
                    self.phase = IteratorPhase::Running;
                    return Some(RichToken::new(
                        Token::LineStart(0),
                        Span::from_usize_range(0..0),
                    ));
                }

                IteratorPhase::Running => {
                    // Check pending tokens first (INDENT/DEDENT, string parts)
                    if let Some(pending) = self.pending_tokens.pop_front() {
                        // Process state for pending tokens too (e.g., StringEnd sets prev_was_json_like)
                        self.process_token(&pending.token);
                        return Some(pending);
                    }

                    // Try to produce a new token
                    if let Some(rich_token) = self.produce_next_token() {
                        // Update state based on token
                        self.process_token(&rich_token.token);

                        // If this is a LineStart in block context, queue INDENT/DEDENT tokens
                        if let Token::LineStart(indent) = &rich_token.token
                            && self.flow_depth == 0
                            && !self.in_quoted_string
                        {
                            self.queue_indent_dedent_tokens(*indent, rich_token.span);
                        }

                        return Some(rich_token);
                    }

                    // No more tokens from input, move to final dedents
                    self.phase = IteratorPhase::FinalDedents;
                }

                IteratorPhase::FinalDedents => {
                    // Emit final Dedent tokens for remaining indent stack
                    if self.flow_depth == 0 && self.indent_stack.len() > 1 {
                        self.indent_stack.pop();
                        let end_span = Span::from_usize_range(self.byte_pos..self.byte_pos);
                        return Some(RichToken::new(Token::Dedent, end_span));
                    }

                    // All done
                    self.phase = IteratorPhase::Done;
                }

                IteratorPhase::Done => {
                    return None;
                }
            }
        }
    }
}

/// Tokenize document content with context awareness.
///
/// Returns `RichToken`s wrapping each token. All tokens (including `Whitespace`,
/// `WhitespaceWithTabs`, `Comment`) are kept as real tokens in the stream.
///
/// Errors are collected internally by the lexer and returned separately.
///
/// For streaming usage, iterate directly over `Lexer::new(input)`.
#[cfg(test)]
pub(crate) fn tokenize_document(input: &str) -> (Vec<RichToken<'_>>, Vec<ParseError>) {
    let mut lexer = Lexer::new(input);
    let tokens: Vec<RichToken<'_>> = lexer.by_ref().collect();
    let errors = lexer.take_errors();

    (tokens, errors)
}

#[cfg(test)]
#[allow(
    clippy::indexing_slicing,
    clippy::min_ident_chars,
    clippy::shadow_reuse,
    reason = "Tests benefit from direct indexing, short identifiers, and variable shadowing for readability"
)]
mod tests {
    use super::*;

    fn get_tokens(input: &str) -> Vec<Token<'_>> {
        let (tokens, _errors) = tokenize_document(input);
        tokens
            .into_iter()
            .filter(|rt| {
                !matches!(
                    rt.token,
                    Token::Whitespace | Token::WhitespaceWithTabs | Token::LineStart(_)
                )
            })
            .map(|rt| rt.token)
            .collect()
    }

    #[test]
    fn test_colon_in_block_plain_scalar() {
        // In block context, :foo is a plain scalar
        let tokens = get_tokens(":foo");
        assert_eq!(tokens, vec![Token::Plain(":foo".into())]);
    }

    #[test]
    fn test_colon_in_flow_context() {
        // In flow context, key: value is a mapping. key:value (no space) is also
        // parsed as key + : + value because : followed by a flow indicator or
        // end of content terminates the key.
        // But key:value where : is followed by non-whitespace non-flow-indicator
        // is the key as the whole "key:value" plain scalar!
        // Actually, let's check: {key:value} - the colon is followed by 'v' which
        // is not whitespace/flow-indicator, so "key:value" is ONE plain scalar.
        let tokens = get_tokens("{key:value}");
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::Plain("key:value".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_colon_with_space_in_flow() {
        // With a space after colon, it's a mapping
        let tokens = get_tokens("{key: value}");
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::Plain("key".into()),
                Token::Colon,
                Token::Plain("value".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_colon_adjacent_in_flow() {
        // {:value} - the colon is followed by 'v', not whitespace, so :value is
        // a plain scalar starting with colon
        let tokens = get_tokens("{:value}");
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::Plain(":value".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_colon_as_indicator_in_flow() {
        // {: value} - colon followed by space is an indicator (empty key)
        let tokens = get_tokens("{: value}");
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::Colon,
                Token::Plain("value".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_quoted_key_adjacent_value() {
        // After quoted key, :value should be : + value (not :value as scalar)
        // This is test C2DT from yaml-test-suite
        let tokens = get_tokens("{\"adjacent\":value}");
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::StringStart(QuoteStyle::Double),
                Token::StringContent("adjacent".into()),
                Token::StringEnd(QuoteStyle::Double),
                Token::Colon,
                Token::Plain("value".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_comma_in_block_plain_scalar() {
        // In block context, comma is part of plain scalar
        let tokens = get_tokens("a,b,c");
        assert_eq!(tokens, vec![Token::Plain("a,b,c".into())]);
    }

    #[test]
    fn test_comma_in_flow_context() {
        // In flow context, comma is separator
        let tokens = get_tokens("[a,b,c]");
        assert_eq!(
            tokens,
            vec![
                Token::FlowSeqStart,
                Token::Plain("a".into()),
                Token::Comma,
                Token::Plain("b".into()),
                Token::Comma,
                Token::Plain("c".into()),
                Token::FlowSeqEnd,
            ]
        );
    }

    #[test]
    fn test_brackets_in_block_plain_scalar() {
        // In block context, [] are part of plain scalar...
        // Actually, no - [ and ] start/end flow sequences even in block context.
        // The difference is that , is only a separator in flow context.
        let tokens = get_tokens("key: [value]");
        // This should still recognize [] as flow sequence
        assert!(tokens.contains(&Token::FlowSeqStart));
    }

    #[test]
    fn test_percent_in_document() {
        // % in document content is just a regular character
        let tokens = get_tokens("foo%bar");
        assert_eq!(tokens, vec![Token::Plain("foo%bar".into())]);
    }

    #[test]
    fn test_percent_at_line_start_in_flow() {
        // % at line start inside flow mapping should be plain scalar content
        let tokens = get_tokens("{ matches\n% : 20 }");
        // Should have: { matches <newline> % : 20 }
        // The % is part of multiline plain scalar, then : 20 is value
        assert!(tokens.contains(&Token::FlowMapStart));
        assert!(tokens.contains(&Token::FlowMapEnd));
        // Should NOT have a reserved directive
        assert!(
            !tokens
                .iter()
                .any(|token| matches!(token, Token::ReservedDirective(_)))
        );
    }

    #[test]
    fn test_colon_value_in_flow() {
        // {x: :x} - value is :x (plain scalar starting with colon)
        let tokens = get_tokens("{x: :x}");
        // Should be: { x : :x }
        assert_eq!(
            tokens,
            vec![
                Token::FlowMapStart,
                Token::Plain("x".into()),
                Token::Colon,
                Token::Plain(":x".into()),
                Token::FlowMapEnd,
            ]
        );
    }

    #[test]
    fn test_url_in_flow() {
        // URL in flow context
        let tokens = get_tokens("{url: http://example.org}");
        // Should recognize http://example.org as a plain scalar
        assert!(tokens.contains(&Token::Plain("http://example.org".into())));
    }

    // Tests for tokenize_document
    //
    // Note: Whitespace, WhitespaceWithTabs, and Comment are real tokens in the stream
    // because comments have semantic meaning in YAML (they terminate plain scalars).

    #[test]
    fn test_simple_mapping_tokens() {
        // Test token stream for simple mapping
        let (tokens, errors) = tokenize_document("key: value");
        assert!(errors.is_empty());

        // Should have 5 tokens: LineStart(0), "key", ":", Whitespace, "value"
        assert_eq!(tokens.len(), 5);

        // Check token types
        assert!(matches!(tokens[0].token, Token::LineStart(0)));
        assert_eq!(tokens[1].token, Token::Plain("key".into()));
        assert_eq!(tokens[2].token, Token::Colon);
        assert_eq!(tokens[3].token, Token::Whitespace);
        assert_eq!(tokens[4].token, Token::Plain("value".into()));
    }

    #[test]
    fn test_comment_as_token() {
        // Test that comments are real tokens
        let (tokens, errors) = tokenize_document("key: value # this is a comment");
        assert!(errors.is_empty());

        // Comment should be a real token in the stream
        let comment_token = tokens
            .iter()
            .find(|t| matches!(&t.token, Token::Comment(s) if s.contains("this is a comment")));
        assert!(comment_token.is_some(), "Should find comment token");
    }

    #[test]
    fn test_multiline_with_comments() {
        // Test multiline document with comments
        let input = "# header comment\nkey: value";
        let (tokens, errors) = tokenize_document(input);
        assert!(errors.is_empty());

        // First token is LineStart(0)
        assert!(matches!(tokens[0].token, Token::LineStart(0)));

        // Comment should be a real token
        let comment_token = tokens
            .iter()
            .find(|t| matches!(&t.token, Token::Comment(s) if s.contains("header")));
        assert!(comment_token.is_some(), "Should find header comment token");
    }

    #[test]
    fn test_empty_input() {
        let (tokens, errors) = tokenize_document("");
        assert!(errors.is_empty());
        // Empty input still gets initial LineStart(0) token
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0].token, Token::LineStart(0)));
    }

    #[test]
    fn test_comment_only() {
        // Only comments
        let (tokens, errors) = tokenize_document("# just a comment");
        assert!(errors.is_empty());
        // LineStart(0) and Comment are separate tokens
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::LineStart(0)));
        assert!(matches!(&tokens[1].token, Token::Comment(s) if s.contains("just a comment")));
    }

    #[test]
    fn test_tab_as_whitespace_with_tabs_token() {
        // Test that tabs are WhitespaceWithTabs tokens (for error detection)
        let input = "key:\n\tvalue";
        let (tokens, errors) = tokenize_document(input);
        assert!(errors.is_empty());

        // WhitespaceWithTabs should be a real token
        let tab_token = tokens
            .iter()
            .find(|t| matches!(t.token, Token::WhitespaceWithTabs));
        assert!(
            tab_token.is_some(),
            "Should find WhitespaceWithTabs token for tab"
        );
    }

    #[test]
    fn test_multi_document_with_directives() {
        // Test that Lexer correctly handles multi-document streams
        // with directives, document markers, and phase transitions
        let input = "%YAML 1.2\n---\ndoc1\n...\n%TAG !e! tag:example,2000:\n---\ndoc2\n";
        let tokens = get_tokens(input);

        // Should have: YamlDirective, DocStart, Plain(doc1), DocEnd,
        //              TagDirective, DocStart, Plain(doc2)
        assert!(
            tokens.iter().any(|t| matches!(t, Token::YamlDirective(_))),
            "Should have YAML directive"
        );
        assert!(
            tokens
                .iter()
                .any(|token| matches!(token, Token::TagDirective(..))),
            "Should have TAG directive after ..."
        );
        assert_eq!(
            tokens
                .iter()
                .filter(|t| matches!(t, Token::DocStart))
                .count(),
            2,
            "Should have two DocStart markers"
        );
        assert_eq!(
            tokens.iter().filter(|t| matches!(t, Token::DocEnd)).count(),
            1,
            "Should have one DocEnd marker"
        );
    }
}
