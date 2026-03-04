// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Scalar parsing (plain, quoted, and block scalars).

use crate::error::ErrorKind;
use crate::span::{IndentLevel, Span, indent_to_usize};
use crate::token::{BlockScalarHeader, Chomping, QuoteStyle, Token};
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

impl<'tokens: 'input, 'input> Parser<'tokens, 'input> {
    /// Parse a simple scalar token.
    /// For mapping keys (typically single-line), uses `min_indent=0`.
    pub fn parse_scalar(&mut self) -> Option<Node<'input>> {
        self.parse_scalar_with_indent(0)
    }

    /// Parse a scalar token with a specified minimum indentation for continuations.
    /// This is used when parsing values where multi-line strings must respect indentation.
    pub fn parse_scalar_with_indent(&mut self, min_indent: IndentLevel) -> Option<Node<'input>> {
        let (tok, span) = self.peek()?;

        match tok {
            Token::Plain(string) => {
                let value = Self::scalar_to_value(string.to_string());
                self.advance();
                Some(Node::new(value, span))
            }
            Token::StringStart(_style) => self.parse_quoted_string(min_indent),
            _ => None,
        }
    }

    /// Parse a quoted string from `StringStart`/`StringContent`/`LineStart`/`StringEnd` tokens.
    /// Returns the assembled string as a Node, or None if not at a `StringStart` token.
    /// Validates that continuation lines have proper indentation (>= `min_indent`).
    ///
    /// `min_indent` is the minimum indentation required for continuation lines.
    /// For root-level strings (`min_indent=0`), continuation at column 0 is valid.
    /// For strings that are values in mappings, continuation must be >= `min_indent`.
    pub fn parse_quoted_string(&mut self, min_indent: IndentLevel) -> Option<Node<'input>> {
        let (first_token, start_span) = self.peek()?;

        let style = match first_token {
            Token::StringStart(style) => *style,
            _ => return None,
        };
        self.advance(); // consume StringStart

        let mut content = String::new();
        let mut end_span = start_span;
        let mut first_content = true;

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
                    if !first_content && !content.is_empty() {
                        // Multi-line string: previous content ended, new content starts
                        // YAML spec: folded newlines become spaces (for flow scalars)
                        content.push(' ');
                    }
                    content.push_str(string);
                    first_content = false;
                    end_span = span;
                    self.advance();
                }
                Token::LineStart(indent) => {
                    // Validate continuation indentation
                    // In YAML, quoted string continuation lines must be indented
                    // at least at the same level as the parent block structure.
                    // For root-level strings (min_indent=0), continuation at column 0 is valid.
                    // For strings in mappings/sequences, continuation must be >= min_indent.
                    //
                    // IMPORTANT: Empty lines (where the next token is LineStart or StringEnd)
                    // are allowed regardless of indentation. Only lines with actual content
                    // (StringContent) need to respect the indentation requirement.
                    let indent = *indent;
                    self.advance();

                    // Check if this is an empty line by peeking at the next token
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
                        // Mismatched quotes (shouldn't happen with proper lexing)
                        self.errors.push(crate::error::ParseError::new(
                            ErrorKind::MismatchedQuotes,
                            span,
                        ));
                    }
                    end_span = span;
                    self.advance();
                    break;
                }
                _ => {
                    // Unexpected token inside string - treat as unterminated
                    self.errors.push(crate::error::ParseError::new(
                        ErrorKind::UnterminatedString,
                        span,
                    ));
                    break;
                }
            }
        }

        let full_span = Span::from_usize_range(start_span.start_usize()..end_span.end_usize());
        Some(Node::new(Value::String(content.into()), full_span))
    }

    /// Convert a plain scalar string to an appropriate Value.
    pub fn scalar_to_value(scalar: String) -> Value<'static> {
        match scalar.as_str() {
            "null" | "Null" | "NULL" | "~" | "" => Value::Null,
            "true" | "True" | "TRUE" => Value::Bool(true),
            "false" | "False" | "FALSE" => Value::Bool(false),
            _ => {
                if let Ok(integer) = scalar.parse::<i64>() {
                    return Value::Int(integer);
                }
                if let Some(hex) = scalar
                    .strip_prefix("0x")
                    .or_else(|| scalar.strip_prefix("0X"))
                    && let Ok(integer) = i64::from_str_radix(hex, 16)
                {
                    return Value::Int(integer);
                }
                if let Some(oct) = scalar
                    .strip_prefix("0o")
                    .or_else(|| scalar.strip_prefix("0O"))
                    && let Ok(integer) = i64::from_str_radix(oct, 8)
                {
                    return Value::Int(integer);
                }
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
    pub fn parse_alias(&mut self) -> Option<Node<'input>> {
        let (tok, span) = self.advance()?;

        let name = if let Token::Alias(name) = tok {
            name.clone()
        } else {
            return None;
        };

        if !self.anchors.contains_key(name.as_ref()) {
            self.error(ErrorKind::UndefinedAlias, span);
        }
        Some(Node::new(Value::Alias(name), span))
    }

    /// Parse a literal block scalar: |
    pub fn parse_literal_block_scalar(&mut self, _min_indent: IndentLevel) -> Option<Node<'input>> {
        let (tok, span) = self.advance()?;
        let start = span.start_usize();

        let header = if let Token::LiteralBlockHeader(hdr) = tok {
            hdr.clone()
        } else {
            return None;
        };

        let (content, end) = self.collect_block_scalar_content(&header, true);
        Some(Node::new(
            Value::String(content.into()),
            Span::from_usize_range(start..end),
        ))
    }

    /// Parse a folded block scalar: >
    pub fn parse_folded_block_scalar(&mut self, _min_indent: IndentLevel) -> Option<Node<'input>> {
        let (tok, span) = self.advance()?;
        let start = span.start_usize();

        let header = if let Token::FoldedBlockHeader(hdr) = tok {
            hdr.clone()
        } else {
            return None;
        };

        let (content, end) = self.collect_block_scalar_content(&header, false);
        Some(Node::new(
            Value::String(content.into()),
            Span::from_usize_range(start..end),
        ))
    }

    /// Check if the line starting at the given position is empty.
    /// An empty line contains only `Dedent`/`Indent` tokens followed by another `LineStart` or EOF.
    fn is_line_empty_at(&self, start_pos: usize) -> bool {
        let mut check_pos = start_pos;
        while let Some(rt) = self.tokens.get(check_pos) {
            match &rt.token {
                Token::Dedent | Token::Indent(_) => check_pos += 1,
                _ => break,
            }
        }
        self.tokens
            .get(check_pos)
            .is_none_or(|rt| matches!(rt.token, Token::LineStart(_)))
    }

    /// Join lines for literal block scalar (`|`). Preserves newlines between lines.
    fn join_literal_lines(lines: &[String]) -> String {
        lines.join("\n")
    }

    /// Join lines for folded block scalar (`>`). Folds single newlines to spaces,
    /// but preserves multiple consecutive newlines.
    fn join_folded_lines(lines: &[String]) -> String {
        let mut result = String::new();
        let mut prev_empty = false;
        for line in lines {
            if line.is_empty() {
                result.push('\n');
                prev_empty = true;
            } else {
                if !result.is_empty() && !prev_empty {
                    result.push(' ');
                }
                result.push_str(line);
                prev_empty = false;
            }
        }
        result
    }

    /// Apply chomping rules to the final block scalar content.
    /// - `Strip`: Remove all trailing newlines.
    /// - `Clip`: Keep exactly one trailing newline.
    /// - `Keep`: Preserve all trailing newlines.
    fn apply_chomping(content: &mut String, chomping: Chomping) {
        match chomping {
            Chomping::Strip => {
                while content.ends_with('\n') {
                    content.pop();
                }
            }
            Chomping::Clip => {
                while content.ends_with("\n\n") {
                    content.pop();
                }
                if !content.is_empty() && !content.ends_with('\n') {
                    content.push('\n');
                }
            }
            Chomping::Keep => {
                if !content.is_empty() && !content.ends_with('\n') {
                    content.push('\n');
                }
            }
        }
    }

    /// Collect content tokens from a single line of a block scalar.
    /// Returns the line content and the end position of the last token consumed.
    fn collect_block_line_content(
        &mut self,
        extra_indent: usize,
        initial_end_pos: usize,
    ) -> (String, usize) {
        let mut line_content = String::new();
        let mut end_pos = initial_end_pos;

        // Add extra indentation as spaces
        for _ in 0..extra_indent {
            line_content.push(' ');
        }

        while let Some((tok, tok_span)) = self.peek() {
            match tok {
                Token::Dedent | Token::Indent(_) => {
                    self.advance();
                }
                Token::Plain(string) | Token::StringContent(string) => {
                    line_content.push_str(string);
                    end_pos = tok_span.end_usize();
                    self.advance();
                }
                Token::StringStart(style) => {
                    // In block scalar content, quote characters are literal content
                    let quote_char = match style {
                        QuoteStyle::Single => '\'',
                        QuoteStyle::Double => '"',
                    };
                    line_content.push(quote_char);
                    end_pos = tok_span.end_usize();
                    self.advance();
                }
                Token::StringEnd(style) => {
                    let quote_char = match style {
                        QuoteStyle::Single => '\'',
                        QuoteStyle::Double => '"',
                    };
                    line_content.push(quote_char);
                    end_pos = tok_span.end_usize();
                    self.advance();
                }
                Token::Whitespace | Token::WhitespaceWithTabs => {
                    line_content.push(' ');
                    end_pos = tok_span.end_usize();
                    self.advance();
                }
                Token::Comment(comment) => {
                    line_content.push('#');
                    line_content.push_str(comment);
                    end_pos = tok_span.end_usize();
                    self.advance();
                }
                Token::LineStart(_) | Token::DocStart | Token::DocEnd => break,
                _ => {
                    end_pos = tok_span.end_usize();
                    self.advance();
                }
            }
        }

        (line_content, end_pos)
    }

    /// Collect block scalar content, respecting indentation.
    pub fn collect_block_scalar_content(
        &mut self,
        header: &BlockScalarHeader,
        literal: bool,
    ) -> (String, usize) {
        let mut lines: Vec<String> = Vec::new();
        let mut content_indent: Option<usize> = header.indent.map(usize::from);
        let mut end_pos = self.current_span().end_usize();

        // Track whitespace-only lines before the first content line.
        // These must have at most as many spaces as the first content line's indentation.
        let mut empty_lines_before_content: Vec<(usize, Span)> = Vec::new();

        loop {
            // Skip any Dedent/Indent tokens
            while let Some((Token::Dedent | Token::Indent(_), _)) = self.peek() {
                self.advance();
            }

            let Some((Token::LineStart(indent_level), span)) = self.peek() else {
                break;
            };
            let n = indent_to_usize(*indent_level);
            let is_empty_line = self.is_line_empty_at(self.pos + 1);

            // Discover content indentation from first non-empty line
            if content_indent.is_none() && !is_empty_line {
                content_indent = Some(n);
                // Validate that all preceding empty lines have at most n spaces
                for (empty_indent, empty_span) in &empty_lines_before_content {
                    if *empty_indent > n {
                        self.error(ErrorKind::InvalidIndentation, *empty_span);
                    }
                }
            }

            let min_indent = content_indent.unwrap_or(1);

            // Content line with insufficient indentation ends the block scalar
            if !is_empty_line && n < min_indent {
                break;
            }

            self.advance(); // consume LineStart
            end_pos = span.end_usize();

            // Skip optional Indent token after LineStart
            if let Some((Token::Indent(_), _)) = self.peek() {
                self.advance();
            }

            // Check for tabs used as indentation (only when no space-based indent)
            if n == 0
                && let Some((Token::WhitespaceWithTabs, tab_span)) = self.peek()
            {
                self.error(ErrorKind::InvalidIndentation, tab_span);
            }

            if is_empty_line {
                if content_indent.is_none() {
                    empty_lines_before_content.push((n, span));
                }
                lines.push(String::new());
                continue;
            }

            // Collect content from this line
            let extra_indent = n.saturating_sub(min_indent);
            let (line_content, line_end) = self.collect_block_line_content(extra_indent, end_pos);
            end_pos = line_end;
            lines.push(line_content);
        }

        // Join lines and apply chomping
        let mut content = if literal {
            Self::join_literal_lines(&lines)
        } else {
            Self::join_folded_lines(&lines)
        };
        Self::apply_chomping(&mut content, header.chomping);

        (content, end_pos)
    }

    /// Convert multiline scalar content to final value.
    fn finalize_multiline_scalar(
        content: String,
        had_continuations: bool,
        start: usize,
        end: usize,
    ) -> Node<'static> {
        // If we had continuations, always treat as string (no type coercion)
        // If no continuations, apply normal scalar type detection
        let value = if had_continuations {
            Value::String(content.into())
        } else {
            Self::scalar_to_value(content)
        };
        Node::new(value, Span::from_usize_range(start..end))
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
            self.check_multiline_implicit_key(scalar.span.start_usize(), scalar.span.end_usize());

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
                return Some(scalar);
            }

            // This is actually a mapping - reparse
            let start = scalar.span.start_usize();
            let map_indent = self.column_of_position(start);
            let mut pairs: Vec<(Node, Node)> = Vec::new();

            let key = scalar;

            self.advance(); // consume ':'
            self.check_tabs_after_block_indicator();
            self.skip_ws();

            // Check for block sequence indicator on same line as key - invalid in YAML
            if let Some((Token::BlockSeqIndicator, span)) = self.peek() {
                self.error(ErrorKind::ContentOnSameLine, span);
            }

            // Parse the value
            let value_on_same_line = !matches!(self.peek(), Some((Token::LineStart(_), _)));
            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.check_tabs_as_indentation();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };

            let value_node = value.unwrap_or_else(|| Node::null(self.current_span()));

            // Check for trailing content after quoted scalars in block context
            if value_on_same_line && let Value::String(_) = &value_node.value {
                self.check_trailing_content_after_scalar();
            }

            pairs.push((key, value_node));

            // Continue parsing more key-value pairs at same indentation
            self.parse_remaining_mapping_entries(&mut pairs, map_indent);

            let end = pairs.last().map_or(start, |(_, val)| val.span.end_usize());
            Some(Node::new(
                Value::Mapping(pairs),
                Span::from_usize_range(start..end),
            ))
        } else {
            // Just a scalar - check for multiline continuation (plain scalars only)
            // Note: If we saw a comment after the scalar, it terminates the plain scalar
            // and we should NOT look for continuation lines (YAML spec section 7.3.3)
            if is_quoted_scalar || saw_comment {
                // Comment terminated the scalar - check for trailing content at root
                if min_indent == 0 && saw_comment {
                    self.check_trailing_content_at_root(0);
                }
                Some(scalar)
            } else {
                Some(self.consume_plain_scalar_continuations(scalar, min_indent))
            }
        }
    }

    /// Helper to parse remaining mapping entries at the same indentation level.
    fn parse_remaining_mapping_entries(
        &mut self,
        pairs: &mut Vec<(Node<'input>, Node<'input>)>,
        map_indent: IndentLevel,
    ) {
        loop {
            let loop_start_pos = self.pos;

            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            // Advance to next line at same indent, or break if we can't
            if !self.advance_to_same_indent(map_indent) {
                break;
            }

            // Try to parse a key-value pair
            let Some((key, value_node)) = self.parse_implicit_mapping_entry(map_indent) else {
                break;
            };

            pairs.push((key, value_node));

            // Ensure progress
            if self.pos == loop_start_pos && !self.is_eof() {
                break;
            }
        }
    }

    /// Advance to the next line at the specified indent level.
    /// Returns true if we're now at a line with the target indent, false otherwise.
    fn advance_to_same_indent(&mut self, target_indent: IndentLevel) -> bool {
        loop {
            match self.peek() {
                Some((Token::LineStart(n), _)) => {
                    if *n < target_indent {
                        return false;
                    }
                    if *n == target_indent {
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
                    self.advance();
                }
                Some((Token::Dedent, _)) => {
                    self.advance();
                }
                _ => return false,
            }
        }
    }

    /// Parse a single implicit mapping entry (key: value).
    /// Returns None if no valid entry could be parsed.
    fn parse_implicit_mapping_entry(
        &mut self,
        map_indent: IndentLevel,
    ) -> Option<(Node<'input>, Node<'input>)> {
        // Collect any anchor/tag properties before the key
        let key_props = self.collect_node_properties(NodeProperties::default());

        // Parse the key (scalar or alias)
        let key = self.parse_mapping_key_or_alias(key_props)?;

        self.skip_ws();

        // Expect colon after key
        if let Some((Token::Colon, _)) = self.peek() {
            self.check_multiline_implicit_key(key.span.start_usize(), key.span.end_usize());
            self.advance();
        } else {
            self.error(ErrorKind::MissingColon, key.span);
            return None;
        }

        self.skip_ws();

        // Check for block sequence indicator on same line as key - invalid
        if let Some((Token::BlockSeqIndicator, span)) = self.peek() {
            self.error(ErrorKind::ContentOnSameLine, span);
        }

        let value_on_same_line = !matches!(self.peek(), Some((Token::LineStart(_), _)));
        let value_node = self.parse_mapping_value(map_indent);

        if value_on_same_line && let Value::String(_) = &value_node.value {
            self.check_trailing_content_after_scalar();
        }

        Some((key, value_node))
    }

    /// Parse a mapping key that can be a scalar or alias, with optional properties.
    /// Returns None if no valid key could be parsed.
    fn parse_mapping_key_or_alias(
        &mut self,
        key_props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        if let Some((Token::Alias(name), span)) = self.peek() {
            // Alias key - properties on aliases are invalid
            let alias_name = name.clone();
            if !key_props.is_empty() {
                self.error(ErrorKind::PropertiesOnAlias, span);
            }
            self.advance();
            if !self.anchors.contains_key(alias_name.as_ref()) {
                self.error(ErrorKind::UndefinedAlias, span);
            }
            Some(Node::new(Value::Alias(alias_name), span))
        } else if let Some(key) = self.parse_scalar() {
            // Scalar key - apply properties if present
            if key_props.is_empty() {
                Some(key)
            } else {
                Some(self.apply_properties_and_register(key_props, key))
            }
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
    #[allow(
        clippy::too_many_lines,
        reason = "Complex plain scalar continuation logic, will be refactored later"
    )]
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
        initial: Node<'input>,
        block_min_indent: IndentLevel,
    ) -> Node<'input> {
        let start = initial.span.start_usize();
        let mut end = initial.span.end_usize();

        // For plain scalar continuations, the minimum indent for continuation lines is:
        // - At least 1 (continuation must be indented)
        // - Greater than or equal to block_min_indent (must be in valid block context)
        let min_indent = block_min_indent.max(1);

        // Extract the initial content
        let mut content = match &initial.value {
            Value::String(string) => string.clone().into_owned(),
            Value::Bool(boolean) => boolean.to_string(),
            Value::Int(integer) => integer.to_string(),
            Value::Float(float) => float.to_string(),
            Value::Null => String::new(),
            _ => return initial, // Not a scalar type, return as-is
        };

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
                        return Self::finalize_multiline_scalar(
                            content,
                            had_continuations,
                            start,
                            end,
                        );
                    }
                    LowIndentResult::Break => break,
                }
            }

            // Check if the following content is a mapping key
            let next_pos = self.pos + 1;
            if next_pos < self.tokens.len() {
                let mut content_pos = next_pos;
                while content_pos < self.tokens.len() {
                    match &self.tokens[content_pos].token {
                        Token::Whitespace | Token::WhitespaceWithTabs | Token::Indent(_) => {
                            content_pos += 1;
                        }
                        _ => break,
                    }
                }

                if self.is_mapping_key_at_position(content_pos) {
                    return Self::finalize_multiline_scalar(content, had_continuations, start, end);
                }
            }

            // Valid continuation, consume the LineStart
            self.advance();

            while let Some((Token::Indent(_) | Token::Whitespace | Token::WhitespaceWithTabs, _)) =
                self.peek()
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
        Self::finalize_multiline_scalar(content, had_continuations, start, end)
    }
}
