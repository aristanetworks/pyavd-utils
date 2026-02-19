// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Modal lexer for YAML.
//!
//! This lexer uses internal modes to properly handle context-sensitive tokenization:
//! - **Stream mode**: Between documents, handles directives (`%YAML`, `%TAG`)
//! - **Document mode**: Inside documents, tracks quotes and flow depth
//!
//! The key improvement over the previous layered architecture is that the lexer
//! maintains full context, so it knows when `...` is inside a quoted string vs
//! being a document end marker.

use crate::error::{ErrorKind, ParseError};
use crate::lexer::{BlockScalarHeader, Chomping, QuoteStyle, Token};
use crate::span::{Span, Spanned};

use chumsky::span::Span as _;

/// Lexer mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexerMode {
    /// Between documents - can see directives and document markers
    Stream,
    /// Inside a document - tracks flow depth, quotes, etc.
    Document,
}

/// Flow context mode (block vs flow).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlowMode {
    /// Block context - flow indicators can be in plain scalars
    Block,
    /// Flow context - flow indicators are delimiters
    Flow,
}

/// Modal lexer state.
pub struct ModalLexer<'a> {
    input: &'a str,
    chars: Vec<char>,
    pos: usize,
    byte_pos: usize,

    /// Current lexer mode
    mode: LexerMode,

    /// Flow depth (number of unclosed `{` or `[`)
    flow_depth: usize,

    /// Indentation stack for INDENT/DEDENT tokens
    indent_stack: Vec<usize>,

    /// Whether previous token was whitespace/line start (for comment validation)
    prev_was_separator: bool,

    /// Whether previous token was "JSON-like" (quoted string, alias, flow end)
    prev_was_json_like: bool,

    /// Collected errors
    errors: Vec<ParseError>,

    /// Pending tokens to emit (for INDENT/DEDENT after LineStart)
    pending_tokens: Vec<Spanned<Token>>,

    /// Track if we've seen content in current document (for bare documents)
    seen_doc_content: bool,

    /// Track if current document has an explicit start marker
    has_explicit_start: bool,

    /// Track if we've seen a `...` document end marker on the current line
    /// (for validating that no content follows `...` on the same line)
    seen_doc_end_on_line: bool,

    /// Track if we've seen a %YAML directive in the current document prolog
    seen_yaml_directive: bool,
}

impl<'a> ModalLexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.chars().collect(),
            pos: 0,
            byte_pos: 0,
            mode: LexerMode::Stream,
            flow_depth: 0,
            indent_stack: vec![0],
            prev_was_separator: true,
            prev_was_json_like: false,
            errors: Vec::new(),
            pending_tokens: Vec::new(),
            seen_doc_content: false,
            has_explicit_start: false,
            seen_doc_end_on_line: false,
            seen_yaml_directive: false,
        }
    }

    /// Tokenize the entire input and return tokens with spans and errors.
    pub fn tokenize(mut self) -> (Vec<Spanned<Token>>, Vec<ParseError>) {
        let mut tokens = Vec::new();

        // Emit initial LineStart(0)
        tokens.push((Token::LineStart(0), Span::new((), 0..0)));

        while let Some(tok_span) = self.next_token() {
            // Handle flow depth tracking
            match &tok_span.0 {
                Token::FlowMapStart | Token::FlowSeqStart => {
                    self.flow_depth += 1;
                }
                Token::FlowMapEnd | Token::FlowSeqEnd => {
                    self.flow_depth = self.flow_depth.saturating_sub(1);
                }
                _ => {}
            }

            // Track separator state for comment validation
            match &tok_span.0 {
                Token::Whitespace | Token::LineStart(_) | Token::Comment(_) => {
                    self.prev_was_separator = true;
                }
                Token::StringEnd(_) | Token::Alias(_)
                | Token::FlowMapEnd | Token::FlowSeqEnd => {
                    self.prev_was_json_like = true;
                    self.prev_was_separator = false;
                }
                _ => {
                    self.prev_was_json_like = false;
                    self.prev_was_separator = false;
                }
            }

            // Emit INDENT/DEDENT after LineStart in block context
            if let Token::LineStart(n) = &tok_span.0 {
                tokens.push(tok_span.clone());
                if self.flow_depth == 0 {
                    self.emit_indent_dedent(*n, tok_span.1, &mut tokens);
                }
            } else {
                tokens.push(tok_span);
            }

            // Drain any pending tokens
            tokens.append(&mut self.pending_tokens);
        }

        // Emit final Dedent tokens
        let end_span = Span::new((), self.byte_pos..self.byte_pos);
        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            tokens.push((Token::Dedent, end_span));
        }

        (tokens, self.errors)
    }

    // === Helper methods ===

    fn flow_mode(&self) -> FlowMode {
        if self.flow_depth > 0 {
            FlowMode::Flow
        } else {
            FlowMode::Block
        }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.chars.len()
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn peek_n(&self, n: usize) -> Option<char> {
        self.chars.get(self.pos + n).copied()
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.chars.get(self.pos) {
            self.pos += 1;
            self.byte_pos += c.len_utf8();
            Some(*c)
        } else {
            None
        }
    }

    fn current_span(&self, start: usize) -> Span {
        Span::new((), start..self.byte_pos)
    }

    fn add_error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError {
            kind,
            span,
            expected: Vec::new(),
            found: None,
        });
    }

    fn is_newline(c: char) -> bool {
        matches!(c, '\n' | '\r' | '\u{0085}' | '\u{2028}' | '\u{2029}')
    }

    fn is_flow_indicator(c: char) -> bool {
        matches!(c, ',' | '[' | ']' | '{' | '}')
    }

    /// Check if current position is at column 0 (start of line).
    fn is_at_column_zero(&self) -> bool {
        if self.byte_pos == 0 {
            return true;
        }
        // Check if previous char was a newline
        if self.pos > 0 {
            if let Some(&prev_char) = self.chars.get(self.pos - 1) {
                return Self::is_newline(prev_char);
            }
        }
        false
    }

    /// Emit INDENT/DEDENT tokens based on indentation change.
    fn emit_indent_dedent(&mut self, new_indent: usize, span: Span, tokens: &mut Vec<Spanned<Token>>) {
        let current_indent = *self.indent_stack.last().unwrap_or(&0);

        if new_indent > current_indent {
            self.indent_stack.push(new_indent);
            tokens.push((Token::Indent(new_indent), span));
        } else if new_indent < current_indent {
            while let Some(&top) = self.indent_stack.last() {
                if top <= new_indent {
                    break;
                }
                self.indent_stack.pop();
                tokens.push((Token::Dedent, span));
            }

            let final_indent = *self.indent_stack.last().unwrap_or(&0);
            if new_indent != final_indent && new_indent > 0 {
                self.indent_stack.push(new_indent);
            }
        }
    }

    fn skip_inline_whitespace(&mut self) {
        while matches!(self.peek(), Some(' ' | '\t')) {
            self.advance();
        }
    }

    // === Main token dispatch ===

    fn next_token(&mut self) -> Option<Spanned<Token>> {
        if self.is_eof() {
            return None;
        }

        match self.mode {
            LexerMode::Stream => self.next_token_stream_mode(),
            LexerMode::Document => self.next_token_document_mode(),
        }
    }

    /// Stream mode: handles directives and document markers.
    fn next_token_stream_mode(&mut self) -> Option<Spanned<Token>> {
        let start = self.byte_pos;
        let c = self.peek()?;

        // Skip whitespace and newlines in stream mode
        if Self::is_newline(c) {
            self.advance();
            if c == '\r' && self.peek() == Some('\n') {
                self.advance();
            }
            // Reset seen_doc_end_on_line - we're on a new line now
            self.seen_doc_end_on_line = false;
            // Count indentation
            let mut indent = 0;
            while self.peek() == Some(' ') {
                self.advance();
                indent += 1;
            }
            return Some((Token::LineStart(indent), self.current_span(start)));
        }

        if c == ' ' || c == '\t' {
            self.skip_inline_whitespace();
            return Some((Token::Whitespace, self.current_span(start)));
        }

        // Comments in stream mode
        if c == '#' {
            let mut content = String::new();
            self.advance();
            while let Some(ch) = self.peek() {
                if Self::is_newline(ch) {
                    break;
                }
                content.push(ch);
                self.advance();
            }
            return Some((Token::Comment(content), self.current_span(start)));
        }

        // Check for directives
        if c == '%' {
            return Some(self.consume_directive(start));
        }

        // Check for document start marker `---`
        if c == '-' && self.peek_n(1) == Some('-') && self.peek_n(2) == Some('-') {
            let after = self.peek_n(3);
            if after.is_none() || after == Some(' ') || after == Some('\t') || Self::is_newline(after.unwrap()) {
                self.advance();
                self.advance();
                self.advance();
                self.mode = LexerMode::Document;
                self.has_explicit_start = true;
                self.seen_doc_content = false;
                self.seen_doc_end_on_line = false;  // Reset on new document
                self.seen_yaml_directive = false;  // Reset for next document prolog
                return Some((Token::DocStart, self.current_span(start)));
            }
        }

        // Check for document end marker `...`
        if c == '.' && self.peek_n(1) == Some('.') && self.peek_n(2) == Some('.') {
            let after = self.peek_n(3);
            if after.is_none() || after == Some(' ') || after == Some('\t') || Self::is_newline(after.unwrap()) {
                self.advance();
                self.advance();
                self.advance();
                self.seen_doc_end_on_line = true;  // Mark we've seen `...` on this line
                // Stay in stream mode after ...
                return Some((Token::DocEnd, self.current_span(start)));
            }
        }

        // After `...` on the SAME LINE, only comments are allowed, not content
        // (Bare documents starting on a NEW line after `...` are valid)
        if self.seen_doc_end_on_line {
            // Consume the invalid content until we reach a newline
            let mut content = String::new();
            while let Some(ch) = self.peek() {
                if Self::is_newline(ch) {
                    break;
                }
                content.push(ch);
                self.advance();
            }
            let span = self.current_span(start);
            self.add_error(ErrorKind::UnexpectedToken, span);
            // Return the invalid content as a plain token so we can continue parsing
            return Some((Token::Plain(content), span));
        }

        // Any other content starts a bare document (no explicit ---)
        self.mode = LexerMode::Document;
        self.has_explicit_start = false;
        self.seen_doc_content = false;
        self.next_token_document_mode()
    }

    /// Document mode: full content tokenization with context tracking.
    fn next_token_document_mode(&mut self) -> Option<Spanned<Token>> {
        let start = self.byte_pos;
        let c = self.peek()?;

        // Handle newlines -> produce LineStart with indentation
        if Self::is_newline(c) {
            self.advance();
            if c == '\r' && self.peek() == Some('\n') {
                self.advance();
            }
            // Count indentation
            let mut indent = 0;
            while self.peek() == Some(' ') {
                self.advance();
                indent += 1;
            }
            return Some((Token::LineStart(indent), self.current_span(start)));
        }

        // Check for document markers at column 0
        // Per YAML spec, `---` and `...` at column 0 ALWAYS act as document markers,
        // even inside flow contexts (which would be an error - unclosed flow)
        if self.is_at_column_zero() {
            // Check for `---`
            if c == '-' && self.peek_n(1) == Some('-') && self.peek_n(2) == Some('-') {
                let after = self.peek_n(3);
                if after.is_none()
                    || after == Some(' ')
                    || after == Some('\t')
                    || Self::is_newline(after.unwrap())
                {
                    // If in flow context, this is an error - unclosed flow
                    if self.flow_depth > 0 {
                        let span = Span::new((), start..start + 3);
                        self.add_error(ErrorKind::UnexpectedToken, span);
                        self.flow_depth = 0;  // Reset flow depth
                    }
                    // This starts a new document - switch to stream mode
                    self.mode = LexerMode::Stream;
                    self.advance();
                    self.advance();
                    self.advance();
                    self.has_explicit_start = true;
                    self.seen_doc_content = false;
                    return Some((Token::DocStart, self.current_span(start)));
                }
            }
            // Check for `...`
            if c == '.' && self.peek_n(1) == Some('.') && self.peek_n(2) == Some('.') {
                let after = self.peek_n(3);
                if after.is_none()
                    || after == Some(' ')
                    || after == Some('\t')
                    || Self::is_newline(after.unwrap())
                {
                    // If in flow context, this is an error - unclosed flow
                    if self.flow_depth > 0 {
                        let span = Span::new((), start..start + 3);
                        self.add_error(ErrorKind::UnexpectedToken, span);
                        self.flow_depth = 0;  // Reset flow depth
                    }
                    self.mode = LexerMode::Stream;
                    self.seen_doc_end_on_line = true;  // Mark we've seen `...` on this line
                    self.advance();
                    self.advance();
                    self.advance();
                    return Some((Token::DocEnd, self.current_span(start)));
                }
            }
        }

        // Whitespace
        if c == ' ' || c == '\t' {
            self.skip_inline_whitespace();
            return Some((Token::Whitespace, self.current_span(start)));
        }

        // Comments
        if c == '#' {
            if !self.prev_was_separator {
                self.advance();
                let span = self.current_span(start);
                self.add_error(ErrorKind::UnexpectedToken, span);
                while let Some(ch) = self.peek() {
                    if Self::is_newline(ch) {
                        break;
                    }
                    self.advance();
                }
                return Some((Token::Comment(String::new()), self.current_span(start)));
            }
            let mut content = String::new();
            self.advance();
            while let Some(ch) = self.peek() {
                if Self::is_newline(ch) {
                    break;
                }
                content.push(ch);
                self.advance();
            }
            return Some((Token::Comment(content), self.current_span(start)));
        }

        // Flow indicators
        match c {
            '{' => {
                self.advance();
                return Some((Token::FlowMapStart, self.current_span(start)));
            }
            '}' => {
                self.advance();
                return Some((Token::FlowMapEnd, self.current_span(start)));
            }
            '[' => {
                self.advance();
                return Some((Token::FlowSeqStart, self.current_span(start)));
            }
            ']' => {
                self.advance();
                return Some((Token::FlowSeqEnd, self.current_span(start)));
            }
            ',' if self.flow_mode() == FlowMode::Flow => {
                self.advance();
                return Some((Token::Comma, self.current_span(start)));
            }
            _ => {}
        }

        // Block sequence indicator
        if c == '-' {
            if let Some(next) = self.peek_n(1) {
                if next == ' ' || next == '\t' || Self::is_newline(next) {
                    self.advance();
                    return Some((Token::BlockSeqIndicator, self.current_span(start)));
                }
            } else {
                self.advance();
                return Some((Token::BlockSeqIndicator, self.current_span(start)));
            }
        }

        // Explicit key indicator
        if c == '?' {
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

        // Colon (mapping value indicator)
        if c == ':' {
            let next = self.peek_n(1);
            let is_indicator = if self.prev_was_json_like && self.flow_mode() == FlowMode::Flow {
                true
            } else {
                match self.flow_mode() {
                    FlowMode::Flow => {
                        next.is_none()
                            || next == Some(' ')
                            || next == Some('\t')
                            || next.map(Self::is_newline).unwrap_or(false)
                            || next.map(Self::is_flow_indicator).unwrap_or(false)
                    }
                    FlowMode::Block => {
                        next.is_none()
                            || next == Some(' ')
                            || next == Some('\t')
                            || next.map(Self::is_newline).unwrap_or(false)
                    }
                }
            };
            if is_indicator {
                self.advance();
                return Some((Token::Colon, self.current_span(start)));
            }
        }

        // Anchors and aliases
        if c == '&' {
            if let Some(next) = self.peek_n(1) {
                if Self::is_anchor_char(next) {
                    self.advance();
                    let name = self.consume_anchor_name();
                    return Some((Token::Anchor(name), self.current_span(start)));
                }
            }
        }
        if c == '*' {
            if let Some(next) = self.peek_n(1) {
                if Self::is_anchor_char(next) {
                    self.advance();
                    let name = self.consume_anchor_name();
                    return Some((Token::Alias(name), self.current_span(start)));
                }
            }
        }

        // Tags
        if c == '!' {
            return Some(self.consume_tag(start));
        }

        // Block scalar headers
        if c == '|' {
            self.advance();
            let header = self.consume_block_header();
            return Some((Token::LiteralBlockHeader(header), self.current_span(start)));
        }
        if c == '>' {
            self.advance();
            let header = self.consume_block_header();
            return Some((Token::FoldedBlockHeader(header), self.current_span(start)));
        }

        // Quoted scalars
        if c == '\'' {
            return Some(self.consume_single_quoted(start));
        }
        if c == '"' {
            return Some(self.consume_double_quoted(start));
        }

        // Plain scalar
        self.seen_doc_content = true;
        Some(self.consume_plain_scalar(start))
    }

    // === Consumer methods ===

    fn is_anchor_char(c: char) -> bool {
        !c.is_whitespace() && !matches!(c, '[' | ']' | '{' | '}' | ',')
    }

    fn consume_anchor_name(&mut self) -> String {
        let mut name = String::new();
        while let Some(c) = self.peek() {
            if Self::is_anchor_char(c) {
                name.push(c);
                self.advance();
            } else {
                break;
            }
        }
        name
    }

    fn consume_directive(&mut self, start: usize) -> Spanned<Token> {
        self.advance(); // consume %
        let mut directive = String::new();
        while let Some(c) = self.peek() {
            if c.is_whitespace() || Self::is_newline(c) {
                break;
            }
            directive.push(c);
            self.advance();
        }
        // Skip any trailing whitespace on same line
        self.skip_inline_whitespace();

        // Parse the directive value (everything up to newline/comment)
        let mut value = String::new();
        while let Some(c) = self.peek() {
            if Self::is_newline(c) {
                break;
            }
            if c == '#' {
                // Skip comment
                while let Some(ch) = self.peek() {
                    if Self::is_newline(ch) {
                        break;
                    }
                    self.advance();
                }
                break;
            }
            value.push(c);
            self.advance();
        }
        let value = value.trim().to_string();

        if directive == "YAML" {
            // Check for duplicate YAML directive
            if self.seen_yaml_directive {
                let span = self.current_span(start);
                self.add_error(ErrorKind::UnexpectedToken, span);
            }
            self.seen_yaml_directive = true;

            // Validate YAML directive: should only be version number like "1.2"
            // Check for extra content after version
            let parts: Vec<&str> = value.split_whitespace().collect();
            if parts.len() > 1 {
                // Extra content after version number
                let span = self.current_span(start);
                self.add_error(ErrorKind::UnexpectedToken, span);
            } else if parts.len() == 1 {
                // Validate version format (should be like "1.2")
                let version = parts[0];
                let is_valid_version = version.chars().all(|c| c.is_ascii_digit() || c == '.');
                if !is_valid_version {
                    let span = self.current_span(start);
                    self.add_error(ErrorKind::UnexpectedToken, span);
                }
            }
            (Token::YamlDirective(value), self.current_span(start))
        } else if directive == "TAG" {
            (Token::TagDirective(value), self.current_span(start))
        } else {
            // Unknown directive - emit as reserved
            (Token::ReservedDirective(directive), self.current_span(start))
        }
    }

    /// Check if a character is valid at the start of a tag name.
    fn is_valid_tag_start_char(c: char) -> bool {
        c.is_alphanumeric()
            || matches!(
                c,
                '-' | '_' | '.' | '~' | '%' | '/' | ':' | '@' | '&' | '=' | '+' | '$' | ',' | ';'
            )
    }

    fn consume_tag(&mut self, start: usize) -> Spanned<Token> {
        let mut tag = String::new();
        self.advance(); // consume !

        match self.peek() {
            Some('!') => {
                tag.push('!');
                self.advance();
                // Named tag like !!str
                while let Some(c) = self.peek() {
                    if c.is_whitespace() || Self::is_flow_indicator(c) {
                        break;
                    }
                    tag.push(c);
                    self.advance();
                }
            }
            Some('<') => {
                self.advance();
                // Verbatim tag !<uri>
                while let Some(c) = self.peek() {
                    if c == '>' {
                        self.advance();
                        break;
                    }
                    tag.push(c);
                    self.advance();
                }
            }
            Some(c) if Self::is_valid_tag_start_char(c) => {
                // Regular tag !name
                while let Some(c) = self.peek() {
                    if c.is_whitespace() || Self::is_flow_indicator(c) {
                        break;
                    }
                    tag.push(c);
                    self.advance();
                }
            }
            Some(c) if c.is_whitespace() || Self::is_flow_indicator(c) => {
                // Empty tag (non-specific tag `!`)
            }
            None => {
                // ! at end of input is also a valid non-specific tag
            }
            _ => {
                // `!` followed by non-tag character - treat as plain scalar
                let mut plain = String::from("!");
                let in_flow = self.flow_depth > 0;
                while let Some(c) = self.peek() {
                    if c.is_whitespace() {
                        break;
                    }
                    if in_flow && Self::is_flow_indicator(c) {
                        break;
                    }
                    if c == ':' {
                        let next = self.peek_n(1);
                        if next.is_none() || next.map(|n| n.is_whitespace()).unwrap_or(false) {
                            break;
                        }
                    }
                    plain.push(c);
                    self.advance();
                }
                return (Token::Plain(plain), self.current_span(start));
            }
        }

        (Token::Tag(tag), self.current_span(start))
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
                Some(c) if c.is_ascii_digit() && c != '0' => {
                    indent = Some((c as u8) - b'0');
                    self.advance();
                }
                _ => break,
            }
        }

        // After block header, only whitespace and comments are allowed
        let error_start = self.byte_pos;
        let mut has_invalid_content = false;
        let mut saw_whitespace = false;
        while let Some(c) = self.peek() {
            if Self::is_newline(c) {
                break;
            }
            if c == ' ' || c == '\t' {
                saw_whitespace = true;
                self.advance();
                continue;
            }
            if c == '#' {
                if !saw_whitespace {
                    has_invalid_content = true;
                }
                while let Some(ch) = self.peek() {
                    if Self::is_newline(ch) {
                        break;
                    }
                    self.advance();
                }
                break;
            }
            has_invalid_content = true;
            self.advance();
        }

        if has_invalid_content {
            let span = Span::new((), error_start..self.byte_pos);
            self.add_error(ErrorKind::UnexpectedToken, span);
        }

        BlockScalarHeader { indent, chomping }
    }

    /// Consume a single-quoted string, emitting StringStart, StringContent, LineStart, and StringEnd tokens.
    /// Pushes tokens to pending_tokens and returns the first token.
    fn consume_single_quoted(&mut self, start: usize) -> Spanned<Token> {
        let start_span = Span::new((), start..start + 1);
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
                    // Emit current content before newline
                    if !content.is_empty() {
                        let content_span = Span::new((), content_start..self.byte_pos);
                        self.pending_tokens
                            .push((Token::StringContent(content), content_span));
                        content = String::new();
                    }

                    // Consume newline
                    let newline_start = self.byte_pos;
                    let c = self.advance().unwrap();
                    if c == '\r' && self.peek() == Some('\n') {
                        self.advance();
                    }

                    // Count indentation
                    let mut indent = 0;
                    while self.peek() == Some(' ') {
                        self.advance();
                        indent += 1;
                    }

                    // Emit LineStart token
                    let line_span = Span::new((), newline_start..self.byte_pos);
                    self.pending_tokens
                        .push((Token::LineStart(indent), line_span));
                    content_start = self.byte_pos;
                }
                Some(c) => {
                    content.push(c);
                    self.advance();
                }
            }
        }

        // Emit final content segment if any
        if !content.is_empty() {
            let content_span = Span::new((), content_start..self.byte_pos);
            self.pending_tokens
                .push((Token::StringContent(content), content_span));
        }

        // Emit StringEnd
        let end_span = Span::new((), self.byte_pos - 1..self.byte_pos);
        if !terminated {
            let full_span = self.current_span(start);
            self.add_error(ErrorKind::UnterminatedString, full_span);
        }
        self.pending_tokens
            .push((Token::StringEnd(QuoteStyle::Single), end_span));

        // Return StringStart as the immediate token
        (Token::StringStart(QuoteStyle::Single), start_span)
    }

    /// Consume a double-quoted string, emitting StringStart, StringContent, LineStart, and StringEnd tokens.
    /// Pushes tokens to pending_tokens and returns the first token.
    fn consume_double_quoted(&mut self, start: usize) -> Spanned<Token> {
        let start_span = Span::new((), start..start + 1);
        self.advance(); // consume opening "

        let mut content = String::new();
        let mut content_start = self.byte_pos;
        let mut terminated = false;

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
                    }
                }
                Some('\n' | '\r') => {
                    // Emit current content before newline
                    if !content.is_empty() {
                        let content_span = Span::new((), content_start..self.byte_pos);
                        self.pending_tokens
                            .push((Token::StringContent(content), content_span));
                        content = String::new();
                    }

                    // Consume newline
                    let newline_start = self.byte_pos;
                    let c = self.advance().unwrap();
                    if c == '\r' && self.peek() == Some('\n') {
                        self.advance();
                    }

                    // Count indentation
                    let mut indent = 0;
                    while self.peek() == Some(' ') {
                        self.advance();
                        indent += 1;
                    }

                    // Emit LineStart token
                    let line_span = Span::new((), newline_start..self.byte_pos);
                    self.pending_tokens
                        .push((Token::LineStart(indent), line_span));
                    content_start = self.byte_pos;
                }
                Some(c) => {
                    content.push(c);
                    self.advance();
                }
            }
        }

        // Emit final content segment if any
        if !content.is_empty() {
            let content_span = Span::new((), content_start..self.byte_pos);
            self.pending_tokens
                .push((Token::StringContent(content), content_span));
        }

        // Emit StringEnd
        let end_span = Span::new((), self.byte_pos - 1..self.byte_pos);
        if !terminated {
            let full_span = self.current_span(start);
            self.add_error(ErrorKind::UnterminatedString, full_span);
        }
        self.pending_tokens
            .push((Token::StringEnd(QuoteStyle::Double), end_span));

        // Return StringStart as the immediate token
        (Token::StringStart(QuoteStyle::Double), start_span)
    }

    fn consume_escape_sequence(&mut self, start_byte_pos: usize) -> Option<String> {
        let c = self.advance()?;
        let result = match c {
            '0' => "\0".to_string(),
            'a' => "\x07".to_string(),
            'b' => "\x08".to_string(),
            't' | '\t' => "\t".to_string(),
            'n' => "\n".to_string(),
            'v' => "\x0B".to_string(),
            'f' => "\x0C".to_string(),
            'r' => "\r".to_string(),
            'e' => "\x1B".to_string(),
            ' ' => " ".to_string(),
            '"' => "\"".to_string(),
            '/' => "/".to_string(),
            '\\' => "\\".to_string(),
            'N' => "\u{0085}".to_string(),
            '_' => "\u{00A0}".to_string(),
            'L' => "\u{2028}".to_string(),
            'P' => "\u{2029}".to_string(),
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
                let span = Span::new((), start_byte_pos..self.byte_pos);
                self.add_error(ErrorKind::InvalidEscape(c), span);
                c.to_string()
            }
        };
        Some(result)
    }

    fn consume_hex_escape(&mut self, digits: usize) -> String {
        let mut hex = String::new();
        for _ in 0..digits {
            if let Some(c) = self.peek() {
                if c.is_ascii_hexdigit() {
                    hex.push(c);
                    self.advance();
                } else {
                    break;
                }
            }
        }
        if let Ok(code) = u32::from_str_radix(&hex, 16) {
            if let Some(c) = char::from_u32(code) {
                return c.to_string();
            }
        }
        format!("\\x{hex}")
    }

    /// Consume a plain scalar, respecting flow/block context.
    fn consume_plain_scalar(&mut self, start: usize) -> Spanned<Token> {
        let mut content = String::new();
        let mut at_start = true;
        let in_flow = self.flow_depth > 0;

        while let Some(c) = self.peek() {
            // Always stop at newlines
            if Self::is_newline(c) {
                break;
            }

            // Handle special indicators at start: -, ?, :
            if at_start && (c == '-' || c == '?' || c == ':') {
                let next = self.peek_n(1);
                let is_safe = match next {
                    None => false,
                    Some(n) => {
                        !n.is_whitespace()
                            && !Self::is_newline(n)
                            && !(in_flow && Self::is_flow_indicator(n))
                    }
                };
                if !is_safe {
                    let span = self.current_span(start);
                    self.add_error(ErrorKind::UnexpectedToken, span);
                    self.advance();
                    return (Token::Plain(String::new()), self.current_span(start));
                }
            }

            // Handle colon
            if c == ':' {
                if in_flow {
                    let next = self.peek_n(1);
                    let colon_terminates = next.is_none()
                        || next == Some(' ')
                        || next == Some('\t')
                        || next.map(Self::is_newline).unwrap_or(false)
                        || next.map(Self::is_flow_indicator).unwrap_or(false);

                    if colon_terminates && !at_start {
                        break;
                    } else if colon_terminates && at_start {
                        break;
                    }
                } else {
                    // Block mode
                    if let Some(next) = self.peek_n(1) {
                        if next == ' ' || next == '\t' || Self::is_newline(next) {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }

            // Space followed by # is a comment start
            if c == ' ' || c == '\t' {
                if self.peek_n(1) == Some('#') {
                    break;
                }
            }

            // Flow indicators - depends on mode
            if Self::is_flow_indicator(c) {
                if in_flow {
                    break;
                }
            }

            content.push(c);
            self.advance();
            at_start = false;
        }

        (
            Token::Plain(content.trim_end().to_string()),
            self.current_span(start),
        )
    }
}
