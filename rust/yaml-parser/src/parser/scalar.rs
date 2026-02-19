// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Scalar parsing (plain, quoted, and block scalars).

use chumsky::span::Span as _;

use crate::error::ErrorKind;
use crate::lexer::{BlockScalarHeader, Chomping, Token};
use crate::span::Span;
use crate::value::{Node, Value};

use super::Parser;

impl<'a> Parser<'a> {
    /// Parse a simple scalar token (single-line only, used for keys).
    pub fn parse_scalar(&mut self) -> Option<Node> {
        let (tok, span) = self.peek()?;
        let span = *span;

        match tok {
            Token::Plain(s) => {
                let value = self.scalar_to_value(s.clone());
                self.advance();
                Some(Node::new(value, span))
            }
            Token::SingleQuoted(s) | Token::DoubleQuoted(s) => {
                let value = Value::String(s.clone());
                self.advance();
                Some(Node::new(value, span))
            }
            _ => None,
        }
    }

    /// Convert a plain scalar string to an appropriate Value.
    pub fn scalar_to_value(&self, s: String) -> Value {
        match s.as_str() {
            "null" | "Null" | "NULL" | "~" | "" => Value::Null,
            "true" | "True" | "TRUE" => Value::Bool(true),
            "false" | "False" | "FALSE" => Value::Bool(false),
            _ => {
                if let Ok(i) = s.parse::<i64>() {
                    return Value::Int(i);
                }
                if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
                    if let Ok(i) = i64::from_str_radix(hex, 16) {
                        return Value::Int(i);
                    }
                }
                if let Some(oct) = s.strip_prefix("0o").or_else(|| s.strip_prefix("0O")) {
                    if let Ok(i) = i64::from_str_radix(oct, 8) {
                        return Value::Int(i);
                    }
                }
                if let Ok(f) = s.parse::<f64>() {
                    return Value::Float(f);
                }
                match s.as_str() {
                    ".inf" | ".Inf" | ".INF" => return Value::Float(f64::INFINITY),
                    "-.inf" | "-.Inf" | "-.INF" => return Value::Float(f64::NEG_INFINITY),
                    ".nan" | ".NaN" | ".NAN" => return Value::Float(f64::NAN),
                    _ => {}
                }
                Value::String(s)
            }
        }
    }

    /// Parse an alias: *name
    pub fn parse_alias(&mut self) -> Option<Node> {
        let (tok, span) = self.advance()?;
        let span = *span;

        let name = if let Token::Alias(name) = tok {
            name.clone()
        } else {
            return None;
        };

        if !self.anchors.contains_key(&name) {
            self.error(ErrorKind::UndefinedAlias, span);
        }
        Some(Node::new(Value::Alias(name), span))
    }

    /// Parse a literal block scalar: |
    pub fn parse_literal_block_scalar(&mut self, _min_indent: usize) -> Option<Node> {
        let (tok, span) = self.advance()?;
        let start = span.start;

        let header = if let Token::LiteralBlockHeader(h) = tok {
            h.clone()
        } else {
            return None;
        };

        let (content, end) = self.collect_block_scalar_content(&header, true);
        Some(Node::new(Value::String(content), Span::new((), start..end)))
    }

    /// Parse a folded block scalar: >
    pub fn parse_folded_block_scalar(&mut self, _min_indent: usize) -> Option<Node> {
        let (tok, span) = self.advance()?;
        let start = span.start;

        let header = if let Token::FoldedBlockHeader(h) = tok {
            h.clone()
        } else {
            return None;
        };

        let (content, end) = self.collect_block_scalar_content(&header, false);
        Some(Node::new(Value::String(content), Span::new((), start..end)))
    }

    /// Collect block scalar content, respecting indentation.
    pub fn collect_block_scalar_content(
        &mut self,
        header: &BlockScalarHeader,
        literal: bool,
    ) -> (String, usize) {
        let mut lines: Vec<String> = Vec::new();
        let mut content_indent: Option<usize> = header.indent.map(|i| i as usize);
        let mut end_pos = self.current_span().end;

        // Track whitespace-only lines before the first content line.
        // These must have at most as many spaces as the first content line's indentation.
        // Store (indentation_level, span) for each empty line before content.
        let mut empty_lines_before_content: Vec<(usize, Span)> = Vec::new();

        loop {
            while let Some((Token::Dedent | Token::Indent(_), _)) = self.peek() {
                self.advance();
            }

            let Some((Token::LineStart(n), span)) = self.peek() else {
                break;
            };
            let n = *n;
            let span = *span;

            let is_empty_line = {
                let mut check_pos = self.pos + 1;
                while check_pos < self.tokens.len() {
                    match &self.tokens[check_pos].0 {
                        Token::Dedent | Token::Indent(_) => check_pos += 1,
                        _ => break,
                    }
                }
                if check_pos >= self.tokens.len() {
                    true
                } else {
                    matches!(self.tokens[check_pos].0, Token::LineStart(_))
                }
            };

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

            if !is_empty_line && n < min_indent {
                break;
            }

            self.advance(); // consume LineStart
            end_pos = span.end;

            if let Some((Token::Indent(_), _)) = self.peek() {
                self.advance();
            }

            if is_empty_line {
                // Track this empty line's indentation if we haven't found content yet
                if content_indent.is_none() {
                    empty_lines_before_content.push((n, span));
                }
                lines.push(String::new());
                continue;
            }

            let mut line_content = String::new();
            let extra_indent = n.saturating_sub(min_indent);

            for _ in 0..extra_indent {
                line_content.push(' ');
            }

            while let Some((tok, tok_span)) = self.peek() {
                match tok {
                    Token::LineStart(_) => break,
                    Token::Dedent | Token::Indent(_) => {
                        self.advance();
                    }
                    Token::Plain(s) | Token::SingleQuoted(s) | Token::DoubleQuoted(s) => {
                        line_content.push_str(s);
                        end_pos = tok_span.end;
                        self.advance();
                    }
                    Token::Whitespace => {
                        line_content.push(' ');
                        end_pos = tok_span.end;
                        self.advance();
                    }
                    Token::Comment(c) => {
                        line_content.push('#');
                        line_content.push_str(c);
                        end_pos = tok_span.end;
                        self.advance();
                    }
                    Token::DocStart | Token::DocEnd => break,
                    _ => {
                        end_pos = tok_span.end;
                        self.advance();
                    }
                }
            }

            lines.push(line_content);
        }

        let mut content = if literal {
            lines.join("\n")
        } else {
            let mut result = String::new();
            let mut prev_empty = false;
            for line in &lines {
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
        };

        match header.chomping {
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

        (content, end_pos)
    }

    /// Convert multiline scalar content to final value.
    fn finalize_multiline_scalar(
        &self,
        content: String,
        had_continuations: bool,
        start: usize,
        end: usize,
    ) -> Node {
        // If we had continuations, always treat as string (no type coercion)
        // If no continuations, apply normal scalar type detection
        let value = if had_continuations {
            Value::String(content)
        } else {
            self.scalar_to_value(content)
        };
        Node::new(value, Span::new((), start..end))
    }

    /// Parse a scalar and check if it's actually a mapping key.
    pub fn parse_scalar_or_mapping(&mut self, _min_indent: usize) -> Option<Node> {
        // Remember if the scalar we're about to parse is a quoted scalar, and its start position
        let is_quoted_scalar = matches!(
            self.peek(),
            Some((Token::SingleQuoted(_) | Token::DoubleQuoted(_), _))
        );
        let scalar_start_pos = self.pos;

        let scalar = self.parse_scalar()?;

        self.skip_ws();

        // Check if this is a mapping key (followed by colon)
        if let Some((Token::Colon, colon_span)) = self.peek() {
            // Copy the span before any mutable borrows
            let colon_span = *colon_span;

            // Check for multiline implicit key (invalid in block context)
            self.check_multiline_implicit_key(scalar.span.start, scalar.span.end);

            // Check for invalid nested mapping on same line.
            // Pattern like `a: b: c` or `a: 'b': c` is invalid
            let is_nested_value_position = {
                let mut found_implicit_key_colon = false;
                for i in (0..scalar_start_pos).rev() {
                    match &self.tokens[i].0 {
                        Token::Colon => {
                            let mut has_key_before_colon = false;
                            for j in (0..i).rev() {
                                match &self.tokens[j].0 {
                                    Token::LineStart(_) | Token::MappingKey => {
                                        break;
                                    }
                                    Token::Whitespace | Token::Comment(_) => {
                                        continue;
                                    }
                                    Token::Plain(_)
                                    | Token::SingleQuoted(_)
                                    | Token::DoubleQuoted(_)
                                    | Token::FlowSeqEnd
                                    | Token::FlowMapEnd => {
                                        has_key_before_colon = true;
                                        break;
                                    }
                                    _ => continue,
                                }
                            }
                            if has_key_before_colon {
                                found_implicit_key_colon = true;
                            }
                        }
                        Token::LineStart(_) => {
                            break;
                        }
                        Token::Whitespace | Token::Comment(_) => continue,
                        _ => continue,
                    }
                }
                found_implicit_key_colon
            };

            if is_nested_value_position {
                self.error(ErrorKind::UnexpectedToken, colon_span);
                return Some(scalar);
            }

            // This is actually a mapping - reparse
            let start = scalar.span.start;
            let map_indent = self.column_of_position(scalar.span.start);
            let mut pairs: Vec<(Node, Node)> = Vec::new();

            let key = scalar;

            self.advance(); // consume ':'
            self.skip_ws();

            // Check for block sequence indicator on same line as key - invalid in YAML
            if let Some((Token::BlockSeqIndicator, span)) = self.peek() {
                let span = *span;
                self.error(ErrorKind::UnexpectedToken, span);
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

            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

            // Check for trailing content after quoted scalars in block context
            if value_on_same_line {
                if let Value::String(_) = &value.value {
                    self.check_trailing_content_after_scalar();
                }
            }

            pairs.push((key, value));

            // Continue parsing more key-value pairs at same indentation
            self.parse_remaining_mapping_entries(&mut pairs, map_indent);

            let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
            Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
        } else {
            // Just a scalar - check for multiline continuation (plain scalars only)
            if is_quoted_scalar {
                Some(scalar)
            } else {
                self.consume_plain_scalar_continuations(scalar, _min_indent)
            }
        }
    }

    /// Helper to parse remaining mapping entries at the same indentation level.
    fn parse_remaining_mapping_entries(
        &mut self,
        pairs: &mut Vec<(Node, Node)>,
        map_indent: usize,
    ) {
        loop {
            let loop_start_pos = self.pos;

            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            // Check for next line at same indent
            let at_same_indent = loop {
                if let Some((Token::LineStart(n), _)) = self.peek() {
                    if *n < map_indent {
                        break false;
                    }
                    if *n == map_indent {
                        self.advance();
                        while let Some((Token::Dedent, _)) = self.peek() {
                            self.advance();
                        }
                        break true;
                    }
                    self.advance();
                } else if let Some((Token::Dedent, _)) = self.peek() {
                    self.advance();
                } else {
                    break false;
                }
            };

            if !at_same_indent {
                break;
            }

            self.skip_ws();

            // Try to parse another key
            let key = match self.parse_scalar() {
                Some(k) => k,
                None => break,
            };

            self.skip_ws();

            if let Some((Token::Colon, _)) = self.peek() {
                self.check_multiline_implicit_key(key.span.start, key.span.end);
                self.advance();
            } else {
                self.error(ErrorKind::UnexpectedToken, key.span);
                break;
            }

            self.skip_ws();

            // Check for block sequence indicator on same line as key - invalid
            if let Some((Token::BlockSeqIndicator, span)) = self.peek() {
                let span = *span;
                self.error(ErrorKind::UnexpectedToken, span);
            }

            let value_on_same_line = !matches!(self.peek(), Some((Token::LineStart(_), _)));
            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };

            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

            if value_on_same_line {
                if let Value::String(_) = &value.value {
                    self.check_trailing_content_after_scalar();
                }
            }

            pairs.push((key, value));

            // Safety: ensure progress
            if self.pos == loop_start_pos && !self.is_eof() {
                break;
            }
        }
    }

    /// Consume multiline continuation lines for a plain scalar.
    ///
    /// Called after the first line of a plain scalar has been parsed.
    /// Continuation lines must be more indented than the scalar's starting column.
    pub fn consume_plain_scalar_continuations(
        &mut self,
        initial: Node,
        block_min_indent: usize,
    ) -> Option<Node> {
        let start = initial.span.start;
        let mut end = initial.span.end;

        // For plain scalar continuations, the minimum indent for continuation lines is:
        // - At least 1 (continuation must be indented)
        // - Greater than or equal to block_min_indent (must be in valid block context)
        let min_indent = block_min_indent.max(1);

        // Extract the initial content
        let mut content = match &initial.value {
            Value::String(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Null => String::new(),
            _ => return Some(initial), // Not a scalar type, return as-is
        };

        // Track consecutive empty lines for folding
        let mut consecutive_newlines = 0;
        let mut had_continuations = false;

        loop {
            match self.peek() {
                Some((Token::LineStart(n), _)) => {
                    let indent = *n;

                    // Check if this might be an empty line (LineStart(0) followed by LineStart)
                    if indent < min_indent {
                        // Could be an empty line - check if later content continues
                        let mut next_pos = self.pos + 1;
                        let mut found_continuation = false;
                        while next_pos < self.tokens.len() {
                            match &self.tokens[next_pos].0 {
                                Token::Dedent | Token::Indent(_) => {
                                    next_pos += 1;
                                    continue;
                                }
                                Token::LineStart(next_indent) if *next_indent >= min_indent => {
                                    consecutive_newlines += 1;
                                    had_continuations = true;
                                    self.advance();
                                    while matches!(self.peek(), Some((Token::Dedent, _))) {
                                        self.advance();
                                    }
                                    found_continuation = true;
                                    break;
                                }
                                Token::Whitespace => {
                                    // Tab at start of line followed by content
                                    let after_ws = next_pos + 1;
                                    if after_ws < self.tokens.len() {
                                        match &self.tokens[after_ws].0 {
                                            Token::Plain(_)
                                            | Token::Anchor(_)
                                            | Token::Tag(_)
                                            | Token::Alias(_)
                                            | Token::BlockSeqIndicator => {
                                                if consecutive_newlines == 0 {
                                                    content.push(' ');
                                                } else {
                                                    for _ in 0..consecutive_newlines {
                                                        content.push('\n');
                                                    }
                                                }

                                                self.advance(); // consume LineStart
                                                while matches!(
                                                    self.peek(),
                                                    Some((Token::Dedent, _))
                                                ) {
                                                    self.advance();
                                                }
                                                self.advance(); // consume Whitespace (tab)

                                                match self.peek() {
                                                    Some((Token::Plain(s), plain_span)) => {
                                                        let continuation = s.clone();
                                                        end = plain_span.end;
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
                                                        let line_start_pos = span.start;
                                                        let mut line_end = span.end;
                                                        while let Some((tok, tok_span)) =
                                                            self.peek()
                                                        {
                                                            match tok {
                                                                Token::LineStart(_) => break,
                                                                _ => {
                                                                    line_end = tok_span.end;
                                                                    self.advance();
                                                                }
                                                            }
                                                        }
                                                        let line_text =
                                                            &self.input[line_start_pos..line_end];
                                                        content.push_str(line_text.trim_end());
                                                        end = line_end;
                                                    }
                                                    _ => {}
                                                }

                                                had_continuations = true;
                                                consecutive_newlines = 0;
                                                found_continuation = true;
                                                break;
                                            }
                                            _ => {}
                                        }
                                    }
                                    return Some(self.finalize_multiline_scalar(
                                        content,
                                        had_continuations,
                                        start,
                                        end,
                                    ));
                                }
                                _ => {
                                    return Some(self.finalize_multiline_scalar(
                                        content,
                                        had_continuations,
                                        start,
                                        end,
                                    ));
                                }
                            }
                        }
                        if !found_continuation {
                            if next_pos >= self.tokens.len() {
                                break;
                            }
                        }
                        continue;
                    }

                    // Check if the following content is a mapping key
                    let next_pos = self.pos + 1;
                    if next_pos < self.tokens.len() {
                        let mut content_pos = next_pos;
                        while content_pos < self.tokens.len() {
                            match &self.tokens[content_pos].0 {
                                Token::Whitespace | Token::Indent(_) => content_pos += 1,
                                _ => break,
                            }
                        }

                        if content_pos < self.tokens.len() {
                            if let Token::Plain(_) = &self.tokens[content_pos].0 {
                                let mut lookahead = content_pos + 1;
                                while lookahead < self.tokens.len() {
                                    match &self.tokens[lookahead].0 {
                                        Token::Whitespace => lookahead += 1,
                                        Token::Colon => {
                                            return Some(self.finalize_multiline_scalar(
                                                content,
                                                had_continuations,
                                                start,
                                                end,
                                            ));
                                        }
                                        _ => break,
                                    }
                                }
                            }
                        }
                    }

                    // Valid continuation, consume the LineStart
                    self.advance();

                    while let Some((Token::Indent(_) | Token::Whitespace, _)) = self.peek() {
                        self.advance();
                    }

                    match self.peek() {
                        Some((Token::Plain(s), plain_span)) => {
                            let continuation = s.clone();
                            end = plain_span.end;
                            had_continuations = true;

                            if consecutive_newlines == 0 {
                                content.push(' ');
                            } else {
                                for _ in 0..consecutive_newlines {
                                    content.push('\n');
                                }
                            }
                            content.push_str(&continuation);
                            consecutive_newlines = 0;

                            self.advance();
                        }
                        Some((Token::LineStart(_), _)) => {
                            consecutive_newlines += 1;
                            had_continuations = true;
                            continue;
                        }
                        Some((
                            Token::Anchor(_)
                            | Token::Tag(_)
                            | Token::Alias(_)
                            | Token::BlockSeqIndicator,
                            span,
                        )) => {
                            let line_start_pos = span.start;
                            let mut line_end = span.end;

                            while let Some((tok, tok_span)) = self.peek() {
                                match tok {
                                    Token::LineStart(_) => break,
                                    _ => {
                                        line_end = tok_span.end;
                                        self.advance();
                                    }
                                }
                            }

                            let line_text = &self.input[line_start_pos..line_end];
                            let continuation = line_text.trim_end();

                            if !continuation.is_empty() {
                                had_continuations = true;

                                if consecutive_newlines == 0 {
                                    content.push(' ');
                                } else {
                                    for _ in 0..consecutive_newlines {
                                        content.push('\n');
                                    }
                                }
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
                _ => {
                    break;
                }
            }
        }

        Some(self.finalize_multiline_scalar(content, had_continuations, start, end))
    }
}
