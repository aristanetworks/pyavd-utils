// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML parser with error recovery.
//!
//! This module implements the second phase of parsing: converting
//! the token stream into an AST. The parser handles both block and
//! flow styles, tracking indentation for block structures.
//!
//! The parser properly treats anchors and tags as node properties rather
//! than value wrappers. This means `&anchor key: value` correctly parses
//! with the anchor attached to the key scalar, not wrapping the mapping.

mod block;
mod flow;
mod scalar;

use std::borrow::Cow;
use std::collections::HashMap;

use crate::error::{ErrorKind, ParseError};
use crate::rich_token::RichToken;
use crate::span::{Span, Spanned};
use crate::token::Token;
use crate::value::{Node, Properties, Value};

/// A stream of YAML documents.
pub type Stream<'input> = Vec<Node<'input>>;

/// Pending node properties (anchor, tag) collected before parsing the value.
///
/// The lifetime `'input` refers to the input string being parsed.
#[derive(Debug, Clone, Default)]
pub(crate) struct NodeProperties<'input> {
    pub anchor: Option<(Cow<'input, str>, Span)>,
    pub tag: Option<(Cow<'input, str>, Span)>,
    /// Whether we've crossed a line boundary while accumulating these properties.
    /// This is important for distinguishing between:
    /// - `&a &b value` (invalid: two anchors on same node)
    /// - `&a\n&b value` (valid: &a on containing structure, &b on nested value)
    pub crossed_line_boundary: bool,
}

impl<'input> NodeProperties<'input> {
    pub fn is_empty(&self) -> bool {
        self.anchor.is_none() && self.tag.is_none()
    }

    /// Apply these properties to a node, updating its span to include properties.
    pub fn apply_to(self, mut node: Node<'input>) -> Node<'input> {
        let anchor_val = self.anchor.as_ref().map(|(anchor, _)| anchor.clone());
        let tag_val = self.tag.as_ref().map(|(tag, _)| tag.clone());

        // Update span to include properties
        if let Some((_, anchor_span)) = &self.anchor {
            if anchor_span.start < node.span.start {
                node.span = Span::new(anchor_span.start as usize..node.span.end as usize);
            }
        }
        if let Some((_, tag_span)) = &self.tag {
            if tag_span.start < node.span.start {
                node.span = Span::new(tag_span.start as usize..node.span.end as usize);
            }
        }

        // Only allocate a box if there are actual properties
        if anchor_val.is_some() || tag_val.is_some() {
            node.properties = Some(Box::new(Properties {
                anchor: anchor_val,
                tag: tag_val,
            }));
        }

        node
    }
}

/// Parser state for tracking position and context.
///
/// The parser has two lifetimes:
/// - `'tokens` is the lifetime of the token slice
/// - `'input` is the lifetime of the input string (tokens borrow from input via `Cow`)
///
/// The bound `'tokens: 'input` ensures that tokens outlive the returned Node.
/// This allows the returned `Node<'input>` to borrow data from the tokens.
#[derive(Debug)]
pub(crate) struct Parser<'tokens: 'input, 'input> {
    pub tokens: &'tokens [RichToken<'input>],
    pub input: &'input str,
    pub pos: usize,
    pub errors: Vec<ParseError>,
    /// Map of anchor names to their nodes (for alias resolution)
    pub anchors: HashMap<Cow<'input, str>, Node<'input>>,
    /// Flow depth tracking (0 = block context, > 0 = inside flow collections)
    pub flow_depth: usize,
    /// Stack of columns where each flow context started.
    /// Used to validate that continuation lines are indented relative to the flow start.
    /// Empty when not in flow context.
    pub flow_context_columns: Vec<usize>,
    /// Indentation stack tracking active block structure levels.
    /// Each entry is the indentation level of an active block structure.
    /// Used to detect orphan indentation (content at levels not in the stack).
    pub indent_stack: Vec<usize>,
    /// Tag handles declared in this document's prolog via %TAG directives.
    /// Key: handle like "!prefix!", Value: prefix/URI like "tag:example.com,2011:"
    pub tag_handles: HashMap<String, String>,
}

impl<'tokens: 'input, 'input> Parser<'tokens, 'input> {
    pub fn new(tokens: &'tokens [RichToken<'input>], input: &'input str) -> Self {
        Self {
            tokens,
            input,
            pos: 0,
            errors: Vec::new(),
            anchors: HashMap::new(),
            flow_depth: 0,
            flow_context_columns: Vec::new(),
            indent_stack: vec![0], // Start with base level 0
            tag_handles: HashMap::new(),
        }
    }

    /// Check if we've reached the end of input.
    pub fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Peek at the current token without consuming it.
    /// Returns the token and its span for easy destructuring.
    pub fn peek(&self) -> Option<(&Token<'input>, Span)> {
        self.tokens.get(self.pos).map(|rt| (&rt.token, rt.span))
    }

    /// Consume the current token and return it.
    /// Returns the token and its span for easy destructuring.
    #[allow(clippy::indexing_slicing, reason = "Bounds checked by the condition")]
    pub fn advance(&mut self) -> Option<(&Token<'input>, Span)> {
        (self.pos < self.tokens.len()).then(|| {
            let rt = &self.tokens[self.pos];
            self.pos += 1;
            (&rt.token, rt.span)
        })
    }

    /// Skip whitespace and comments, but NOT line starts.
    /// Also skips `Indent` tokens (they follow `LineStart` and are informational).
    pub fn skip_ws(&mut self) {
        while let Some((tok, _)) = self.peek() {
            match tok {
                Token::Whitespace
                | Token::WhitespaceWithTabs
                | Token::Comment(_)
                | Token::Indent(_) => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    /// Skip whitespace, comments, AND line starts.
    /// Also checks for tabs used as indentation in block context.
    /// Also skips `Indent`/`Dedent` tokens which are structural markers that can appear
    /// between content tokens (e.g., from comment-only indented lines).
    /// In flow context, validates that continuation lines at column 0 are only allowed
    /// when the flow collection itself started at column 0.
    #[allow(
        clippy::indexing_slicing,
        reason = "Index 0 is safe when flow_context_columns is not empty"
    )]
    pub fn skip_ws_and_newlines(&mut self) {
        while let Some((tok, span)) = self.peek() {
            match tok {
                Token::LineStart(indent) => {
                    let indent = *indent;
                    self.advance();

                    // In flow context, check for invalid column 0 continuations
                    // Column 0 is only allowed if the outermost flow context started at column 0
                    if !self.flow_context_columns.is_empty() && indent == 0 {
                        // Get the outermost (first) flow context column
                        let outermost_flow_col = self.flow_context_columns[0];
                        if outermost_flow_col > 0 {
                            // Check if there's actual content after this LineStart
                            // Skip past Whitespace tokens (tabs used as indentation)
                            // and check the actual column of the content token
                            let mut peek_offset = 0;
                            while let Some(rt) = self.tokens.get(self.pos + peek_offset) {
                                if matches!(rt.token, Token::Whitespace | Token::WhitespaceWithTabs)
                                {
                                    peek_offset += 1;
                                } else {
                                    break;
                                }
                            }

                            if let Some(rt) = self.tokens.get(self.pos + peek_offset) {
                                let is_content = !matches!(
                                    rt.token,
                                    Token::LineStart(_)
                                        | Token::FlowSeqEnd
                                        | Token::FlowMapEnd
                                        | Token::Dedent
                                        | Token::DocEnd
                                );
                                // Check actual column of content token
                                let content_col = self.column_of_position(rt.span.start as usize);
                                if is_content && content_col == 0 {
                                    self.errors.push(crate::error::ParseError::new(
                                        ErrorKind::InvalidIndentationContext {
                                            expected: 1,
                                            found: 0,
                                        },
                                        span,
                                    ));
                                }
                            }
                        }
                    }

                    // After LineStart, check if next token is Whitespace containing tabs.
                    // This indicates tabs being used as indentation, which is invalid.
                    // In flow context, only tabs at column 0 are invalid (Y79Y-003).
                    // Tabs after spaces in flow are allowed (6HB6).
                    if self.flow_context_columns.is_empty() {
                        self.check_tabs_as_indentation();
                    } else {
                        self.check_tabs_at_column_zero_in_flow();
                    }
                }
                Token::Indent(_) | Token::Dedent => {
                    // `Indent`/`Dedent` tokens are structural markers - skip them when skipping whitespace
                    self.advance();
                }
                Token::Whitespace | Token::WhitespaceWithTabs | Token::Comment(_) => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    /// Get the column (0-based) of the current token by looking back to the last newline.
    pub fn current_token_column(&self) -> usize {
        if let Some((_, span)) = self.peek() {
            self.column_of_position(span.start as usize)
        } else {
            0
        }
    }

    /// Get the column (0-based) of a byte position by looking back to the last newline.
    #[allow(
        clippy::string_slice,
        reason = "Position is validated to be within input bounds"
    )]
    pub fn column_of_position(&self, pos: usize) -> usize {
        let before = &self.input[..pos];
        if let Some(newline_pos) = before.rfind('\n') {
            pos - newline_pos - 1
        } else {
            pos // No newline, column is the byte position from start of input
        }
    }

    /// Apply node properties to a node and register the anchor if present.
    pub fn apply_properties_and_register(
        &mut self,
        props: NodeProperties<'input>,
        node: Node<'input>,
    ) -> Node<'input> {
        let node_with_props = props.apply_to(node);
        // If the node has an anchor, register it in the anchors map
        if let Some(anchor_name) = node_with_props.anchor() {
            self.anchors
                .insert(anchor_name.clone(), node_with_props.clone());
        }
        node_with_props
    }

    /// Collect node properties (anchors/tags) from the current position.
    ///
    /// This helper loops over Anchor/Tag/Whitespace tokens, accumulating them
    /// into a `NodeProperties` struct. Duplicate anchors/tags emit errors.
    ///
    /// Returns the collected properties and stops when a non-property token is found.
    pub fn collect_node_properties(
        &mut self,
        mut props: NodeProperties<'input>,
    ) -> NodeProperties<'input> {
        loop {
            match self.peek() {
                Some((Token::Anchor(name), anchor_span)) => {
                    let anchor_name = name.clone();
                    self.advance();
                    self.skip_ws();
                    if props.anchor.is_some() {
                        self.error(ErrorKind::DuplicateAnchor, anchor_span);
                    }
                    props.anchor = Some((anchor_name, anchor_span));
                }
                Some((Token::Tag(name), tag_span)) => {
                    let tag_name = name.clone();
                    self.advance();
                    self.skip_ws();
                    if props.tag.is_some() {
                        self.error(ErrorKind::DuplicateTag, tag_span);
                    }
                    props.tag = Some((tag_name, tag_span));
                }
                Some((Token::Whitespace | Token::WhitespaceWithTabs, _)) => {
                    self.advance();
                }
                _ => break,
            }
        }
        props
    }

    /// Push an indentation level onto the stack when entering a block structure.
    pub fn push_indent(&mut self, indent: usize) {
        self.indent_stack.push(indent);
    }

    /// Pop an indentation level from the stack when exiting a block structure.
    pub fn pop_indent(&mut self) {
        if self.indent_stack.len() > 1 {
            self.indent_stack.pop();
        }
    }

    /// Check if there's whitespace (tabs) after `LineStart` that would indicate
    /// invalid tab indentation.
    ///
    /// Tabs are invalid for indentation in both block and flow contexts.
    /// Even inside flow collections, if a line starts with tabs before content,
    /// that's considered invalid indentation (test Y79Y-003).
    pub fn check_tabs_as_indentation(&mut self) {
        if self.pos == 0 {
            return;
        }

        // Check: is the previous token LineStart?
        // If so, check if the current token is WhitespaceWithTabs
        // (which would indicate tabs used for indentation)
        #[allow(
            clippy::indexing_slicing,
            reason = "pos - 1 is valid because we check pos == 0 above"
        )]
        let prev_tok = &self.tokens[self.pos - 1].token;
        if !matches!(prev_tok, Token::LineStart(_)) {
            return;
        }

        // Current token might be WhitespaceWithTabs (tabs as indentation)
        if let Some((Token::WhitespaceWithTabs, tab_span)) = self.peek() {
            // Look ahead to see what real content follows the whitespace
            // Tabs are allowed:
            // - before flow collection start/end indicators (entering/exiting flow)
            // - on blank lines (line contains only whitespace)
            // - at EOF (trailing whitespace)
            let mut look_ahead = self.pos + 1;
            while let Some(rt) = self.tokens.get(look_ahead) {
                match &rt.token {
                    Token::Whitespace | Token::WhitespaceWithTabs => look_ahead += 1,
                    // Tabs allowed before:
                    // - Flow collection start/end (entering/exiting flow)
                    // - Blank line (line has only whitespace)
                    Token::FlowMapStart
                    | Token::FlowMapEnd
                    | Token::FlowSeqStart
                    | Token::FlowSeqEnd
                    | Token::LineStart(_) => return,
                    // Any other content - tabs used for indentation, which is invalid
                    _ => break,
                }
            }
            // If we exhausted tokens (EOF), tabs are allowed (trailing whitespace)
            if self.tokens.get(look_ahead).is_none() {
                return;
            }
            // Content after tabs - report error
            self.error(ErrorKind::InvalidIndentation, tab_span);
        }
    }

    /// Check for tabs at column 0 in flow context.
    /// In flow context, tabs at the start of a line (column 0) are invalid (Y79Y-003),
    /// but tabs after spaces are allowed (6HB6).
    pub fn check_tabs_at_column_zero_in_flow(&mut self) {
        // Only check if current token is WhitespaceWithTabs at column 0
        if let Some((Token::WhitespaceWithTabs, tab_span)) = self.peek() {
            let col = self.column_of_position(tab_span.start as usize);
            if col == 0 {
                // Look ahead to check it's not a blank line
                let mut look_ahead = self.pos + 1;
                while let Some(rt) = self.tokens.get(look_ahead) {
                    match &rt.token {
                        Token::Whitespace | Token::WhitespaceWithTabs => look_ahead += 1,
                        // Blank line or flow end - tabs allowed
                        Token::LineStart(_) | Token::FlowMapEnd | Token::FlowSeqEnd => return,
                        // Content at column 0 with leading tab - error
                        _ => {
                            self.error(ErrorKind::InvalidIndentation, tab_span);
                            return;
                        }
                    }
                }
            }
        }
    }

    /// Check if the current token is whitespace containing tabs after `-` indicator.
    /// Tabs after `-` are only invalid when followed by block structure indicators.
    /// This is because `-\t-` would make the second `-` look like indentation.
    ///
    /// Note: Tabs after `:` and `?` are ALLOWED per YAML 1.2 spec, as they are
    /// separation spaces (s-separate-in-line), not indentation (s-indent).
    pub fn check_tabs_after_block_seq_indicator(&mut self) {
        // Tabs are only problematic in BLOCK context
        if self.flow_depth > 0 {
            return;
        }

        // Check if current token is WhitespaceWithTabs
        if let Some((Token::WhitespaceWithTabs, tab_span)) = self.peek() {
            // Tabs are invalid before block structure indicators (could look like indentation),
            // but allowed before scalar content
            let mut lookahead = self.pos + 1;
            while let Some(rt) = self.tokens.get(lookahead) {
                match &rt.token {
                    Token::Whitespace | Token::WhitespaceWithTabs => lookahead += 1,
                    // Block structure indicators after tab - error (ambiguous indentation)
                    Token::BlockSeqIndicator | Token::MappingKey | Token::Colon => {
                        self.error(ErrorKind::InvalidIndentation, tab_span);
                        return;
                    }
                    // Scalar content after tab - allowed (separation space)
                    _ => return,
                }
            }
        }
    }

    /// Check for tabs after block indicators `:` and `?`.
    /// Per YAML 1.2 spec, tabs ARE allowed here as separation spaces (s-separate-in-line)
    /// EXCEPT when followed by block structure indicators (`-`, `?`, `:`).
    /// In those cases, tabs would make the structure appear indented.
    pub fn check_tabs_after_block_indicator(&mut self) {
        // Tabs are only problematic in BLOCK context
        if self.flow_depth > 0 {
            return;
        }

        // Check if current token is WhitespaceWithTabs
        if let Some((Token::WhitespaceWithTabs, tab_span)) = self.peek() {
            // Tabs are invalid before block structure indicators (could look like indentation).
            // Also check if a scalar is immediately followed by a colon (like `key:`),
            // which creates an implicit mapping and is also ambiguous (Y79Y-008, Y79Y-009).
            let mut lookahead = self.pos + 1;
            while let Some(rt) = self.tokens.get(lookahead) {
                match &rt.token {
                    Token::Whitespace | Token::WhitespaceWithTabs => lookahead += 1,
                    // Block structure indicators after tab - error (ambiguous indentation)
                    Token::BlockSeqIndicator | Token::MappingKey | Token::Colon => {
                        self.error(ErrorKind::InvalidIndentation, tab_span);
                        return;
                    }
                    // Scalar followed by colon - this creates an implicit mapping (like `key:`)
                    // which is also ambiguous after tabs (Y79Y-008, Y79Y-009)
                    Token::Plain(_) => {
                        // Check if the scalar is followed by a colon
                        if self
                            .tokens
                            .get(lookahead + 1)
                            .is_some_and(|next| matches!(next.token, Token::Colon))
                        {
                            self.error(ErrorKind::InvalidIndentation, tab_span);
                            return;
                        }
                        // Simple scalar without colon - allowed
                        return;
                    }
                    // Other content after tab - allowed (separation space)
                    _ => return,
                }
            }
        }
    }

    /// Check for tabs after the sequence indicator `-`.
    /// Tabs are allowed before scalar content, but invalid before block structure.
    pub fn check_tabs_after_seq_indicator(&mut self) {
        self.check_tabs_after_block_seq_indicator();
    }

    /// Check for invalid content immediately after a flow collection in block context.
    pub fn check_content_after_flow(&mut self, flow_end: usize) {
        if let Some((tok, span)) = self.peek() {
            let is_content = matches!(
                tok,
                Token::Plain(_)
                    | Token::StringStart(_)
                    | Token::Anchor(_)
                    | Token::Alias(_)
                    | Token::Tag(_)
                    | Token::BlockSeqIndicator
            );
            if is_content && span.start as usize == flow_end {
                self.error(ErrorKind::ContentOnSameLine, span);
            }
        }
    }

    /// Check for trailing content at column 0 after a closed structure.
    /// This is used for flow collections and block sequences at the root level.
    /// For these structures, content at the same indentation level after them is invalid.
    pub fn check_trailing_content_at_root(&mut self, root_indent: usize) {
        // Save current position to look ahead without advancing
        let saved_pos = self.pos;

        // Skip whitespace, newlines, and dedent tokens
        while !self.is_eof() {
            match self.peek() {
                Some((
                    Token::Whitespace
                    | Token::WhitespaceWithTabs
                    | Token::Comment(_)
                    | Token::Dedent
                    | Token::LineStart(_),
                    _,
                )) => {
                    self.advance();
                }
                _ => break,
            }
        }

        // Check if there's content at or below the root indentation
        if let Some((tok, span)) = self.peek() {
            let is_content = matches!(
                tok,
                Token::Plain(_)
                    | Token::StringStart(_)
                    | Token::Anchor(_)
                    | Token::Alias(_)
                    | Token::Tag(_)
                    | Token::FlowSeqStart
                    | Token::FlowMapStart
                    | Token::BlockSeqIndicator
            );

            // Get the column of this token
            let col = self.current_token_column();

            if is_content && col <= root_indent {
                self.error(ErrorKind::TrailingContent, span);
            }
        }

        // Restore position - we only looked ahead to check
        self.pos = saved_pos;
    }

    /// Check if the first tokens form a block mapping or sequence.
    /// Used to detect invalid block collections on the `---` line.
    /// Per YAML spec, block collections require s-l-comments (newline) before them
    /// when following an explicit document start marker.
    #[allow(
        clippy::indexing_slicing,
        reason = "Token positions are validated by bounds checks"
    )]
    pub fn check_block_mapping_on_start_line(&self) -> bool {
        let mut check_pos = self.pos;

        // Skip anchors, tags, and whitespace
        while check_pos < self.tokens.len() {
            match &self.tokens[check_pos].token {
                Token::Anchor(_)
                | Token::Tag(_)
                | Token::Whitespace
                | Token::WhitespaceWithTabs => check_pos += 1,
                _ => break,
            }
        }

        // Check for block sequence indicator (- item)
        if let Some(rt) = self.tokens.get(check_pos)
            && matches!(rt.token, Token::BlockSeqIndicator)
        {
            return true;
        }

        // Check for scalar followed by Colon (key: value pattern)
        if let Some(rt) = self.tokens.get(check_pos)
            && matches!(rt.token, Token::Plain(_) | Token::StringStart(_))
        {
            // Look for Colon (:) after the scalar
            // May have whitespace between scalar and :
            let mut after_scalar = check_pos + 1;
            while after_scalar < self.tokens.len() {
                match &self.tokens[after_scalar].token {
                    Token::Whitespace | Token::WhitespaceWithTabs => after_scalar += 1,
                    Token::Colon => return true,
                    _ => break,
                }
            }
        }

        false
    }

    /// Check if a value used as an implicit key spans multiple lines.
    #[allow(
        clippy::string_slice,
        reason = "Positions are from lexer tokens and guaranteed to be on UTF-8 boundaries"
    )]
    #[allow(
        clippy::indexing_slicing,
        reason = "Token positions are validated by parser logic before access"
    )]
    pub fn check_multiline_implicit_key(&mut self, key_start: usize, key_end: usize) {
        let mut check_pos = self.pos;
        while check_pos < self.tokens.len() {
            match &self.tokens[check_pos].token {
                Token::Whitespace | Token::WhitespaceWithTabs => check_pos += 1,
                Token::Colon => break,
                _ => return,
            }
        }
        if check_pos >= self.tokens.len() {
            return;
        }
        let colon_span = self.tokens[check_pos].span;

        let key_text = &self.input[key_start..key_end.min(self.input.len())];
        if key_text.contains('\n') {
            self.error(ErrorKind::MultilineImplicitKey, colon_span);
            return;
        }

        for rt in &self.tokens[..self.pos] {
            if rt.span.start as usize > key_start
                && rt.span.end as usize <= key_end
                && matches!(rt.token, Token::LineStart(_))
            {
                self.error(ErrorKind::MultilineImplicitKey, colon_span);
                return;
            }
        }
    }

    /// Check for invalid trailing content after a scalar value in block context.
    pub fn check_trailing_content_after_scalar(&mut self) {
        self.skip_ws();

        if let Some((Token::Comment(_), _)) = self.peek() {
            return;
        }

        if let Some((Token::LineStart(_), _)) = self.peek() {
            return;
        }

        if self.is_eof() {
            return;
        }

        if let Some((Token::DocStart | Token::DocEnd, _)) = self.peek() {
            return;
        }

        if let Some((_, span)) = self.peek() {
            self.error(ErrorKind::TrailingContent, span);
        }
    }

    /// Get the current indentation level from the most recent `LineStart`.
    #[allow(
        clippy::indexing_slicing,
        reason = "Loop index i is guaranteed to be < self.pos which is <= tokens.len()"
    )]
    pub fn current_indent(&self) -> usize {
        for i in (0..self.pos).rev() {
            if let Token::LineStart(n) = self.tokens[i].token {
                return n;
            }
        }
        0
    }

    /// Get the span of the current position.
    pub fn current_span(&self) -> Span {
        self.peek()
            .map_or_else(|| Span::new(0..0), |(_, span)| span)
    }

    /// Check if current position is a mapping key pattern (scalar followed by colon).
    #[allow(
        clippy::indexing_slicing,
        reason = "Token positions are validated by bounds checks"
    )]
    pub fn is_mapping_key_pattern(&self) -> bool {
        let mut i = self.pos;
        match self.tokens.get(i) {
            Some(rt) if matches!(rt.token, Token::Plain(_)) => {
                i += 1;
            }
            Some(rt) if matches!(rt.token, Token::StringStart(_)) => {
                // Skip the entire quoted string (StringStart, StringContent*, LineStart*, StringEnd)
                i += 1;
                while i < self.tokens.len() {
                    match &self.tokens[i].token {
                        Token::StringEnd(_) => {
                            i += 1;
                            break;
                        }
                        Token::StringContent(_) | Token::LineStart(_) => {
                            i += 1;
                        }
                        _ => break, // Unexpected token
                    }
                }
            }
            _ => return false,
        }
        while let Some(rt) = self.tokens.get(i) {
            if !matches!(rt.token, Token::Whitespace | Token::WhitespaceWithTabs) {
                break;
            }
            i += 1;
        }
        self.tokens
            .get(i)
            .is_some_and(|rt| matches!(rt.token, Token::Colon))
    }

    /// Add an error.
    pub fn error(&mut self, kind: ErrorKind, span: Span) {
        self.errors.push(ParseError::new(kind, span));
    }

    /// Populate tag handles from directives provided by the stream lexer.
    /// This is the primary method for setting up tag handle validation.
    pub fn populate_tag_handles_from_directives(
        &mut self,
        directives: &[Spanned<crate::stream_lexer::Directive>],
    ) {
        self.tag_handles.clear();

        for (directive, _span) in directives {
            if let crate::stream_lexer::Directive::Tag(value) = directive {
                // Parse "!prefix! tag:example.com,2011:" into handle + prefix
                if let Some((handle, prefix)) = Self::parse_tag_directive_value(value) {
                    self.tag_handles.insert(handle, prefix);
                }
            }
        }
    }

    /// Parse a %TAG directive value like "!prefix! tag:example.com,2011:"
    /// into (handle, prefix) tuple.
    #[allow(
        clippy::indexing_slicing,
        reason = "Bounds checked by parts.len() == 2 condition"
    )]
    fn parse_tag_directive_value(value: &str) -> Option<(String, String)> {
        // value is like "!prefix! tag:example.com,2011:"
        // or "! !" for primary handle
        let parts: Vec<&str> = value.splitn(2, ' ').collect();
        (parts.len() == 2).then(|| (parts[0].to_owned(), parts[1].to_owned()))
    }

    /// Validate that a tag handle is declared in the current document.
    ///
    /// Tags that don't require declaration:
    /// - `!<uri>` - verbatim tag (explicit URI)
    /// - `!!type` - secondary handle (built-in, maps to tag:yaml.org,2002:)
    /// - `!type` - primary handle (local tag, no second `!`)
    ///
    /// Tags that require declaration:
    /// - `!name!suffix` - named handle (must have `%TAG !name! prefix`)
    #[allow(
        clippy::string_slice,
        reason = "bang_pos is from find('!') so slicing up to it is safe"
    )]
    pub fn validate_tag_handle(&mut self, tag: &str, span: Span) {
        // Note: The lexer strips the leading `!` from tags.
        // So:
        // - `!!type` becomes `!type` in the token
        // - `!prefix!suffix` becomes `prefix!suffix` in the token
        // - `!type` becomes `type` or empty string in the token
        // - `!<uri>` becomes the uri content (without !< and >)

        // Verbatim tag: the original was !<uri> - always valid
        // After lexer: tag contains the URI without !< and >
        // Note: Verbatim tags in the lexer are stored without !< and >.
        // We don't see them here since the token doesn't start with special char.
        // If it was verbatim, the lexer consumed !<...> and stored just the URI.

        // Secondary handle: original !!type, stored as !type - always valid (built-in)
        if tag.starts_with('!') {
            // This is a secondary tag like !!str -> stored as !str
            return;
        }

        // Empty tag (just `!`) is stored as empty string - always valid (non-specific tag)
        if tag.is_empty() {
            return;
        }

        // Check for named handle: original !name!suffix, stored as name!suffix
        // Look for '!' in the tag value (would be the second '!' from original)
        if let Some(bang_pos) = tag.find('!') {
            // Named handle like !prefix!suffix is stored as prefix!suffix
            // The handle is !prefix! which is "!" + tag[0..bang_pos+1]
            let handle = format!("!{}!", &tag[0..bang_pos]);
            if !self.tag_handles.contains_key(&handle) {
                self.error(ErrorKind::UndefinedTagHandle, span);
            }
        }

        // Primary handle: original !type, stored as type (no '!') - always valid (local tag)
        // No validation needed
    }

    /// Parse a complete YAML stream (multiple documents).
    pub fn parse_stream(&mut self) -> Stream<'input> {
        let mut documents = Vec::new();

        self.skip_ws_and_newlines();

        while !self.is_eof() {
            let start_pos = self.pos;

            let explicit_doc_start = if let Some((Token::DocStart, span)) = self.peek() {
                self.advance();
                self.skip_ws_and_newlines();
                Some(span)
            } else {
                None
            };

            if self.is_eof() || matches!(self.peek(), Some((Token::DocStart | Token::DocEnd, _))) {
                if explicit_doc_start.is_some() || !documents.is_empty() || self.pos > start_pos {
                    let span = explicit_doc_start.map_or_else(
                        || self.current_span(),
                        |span| Span::new(span.end as usize..span.end as usize),
                    );
                    documents.push(Node::null(span));
                }
            } else if let Some(node) = self.parse_value(0) {
                documents.push(node);
            }

            self.skip_ws_and_newlines();

            if let Some((Token::DocEnd, _)) = self.peek() {
                self.advance();
                self.skip_ws_and_newlines();
            }

            if self.pos == start_pos && !self.is_eof() {
                self.advance();
            }
        }

        documents
    }

    /// Parse a YAML value at the given minimum indentation level.
    pub fn parse_value(&mut self, min_indent: usize) -> Option<Node<'input>> {
        self.parse_value_with_properties(min_indent, NodeProperties::default())
    }

    /// Handle a flow collection (mapping or sequence) as a value.
    ///
    /// This is shared logic for flow mapping and flow sequence handling:
    /// - Checks for content after the flow
    /// - Detects if used as a mapping key (followed by `:`)
    /// - Checks for trailing content at root level
    fn handle_flow_collection_as_value(
        &mut self,
        flow_node: Node<'input>,
        start_pos: usize,
        min_indent: usize,
        props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        self.check_content_after_flow(flow_node.span.end as usize);
        self.check_multiline_implicit_key(start_pos, flow_node.span.end as usize);

        self.skip_ws();
        if let Some((Token::Colon, _)) = self.peek() {
            let key = self.apply_properties_and_register(props, flow_node);
            self.parse_block_mapping_starting_with_key(min_indent, key)
        } else {
            // Flow collection not used as mapping key - check for trailing content
            if min_indent == 0 {
                self.check_trailing_content_at_root(0);
            }
            Some(self.apply_properties_and_register(props, flow_node))
        }
    }

    /// Handle an anchor token in value parsing.
    ///
    /// Validates indentation, checks for duplicates, and either:
    /// - Tries to parse as a block mapping key (if crossed line boundary with existing anchor)
    /// - Accumulates the anchor as a property and continues parsing
    fn handle_anchor_in_value(
        &mut self,
        anchor_name: Cow<'input, str>,
        anchor_span: Span,
        min_indent: usize,
        mut props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        // Check anchor indentation - must be >= min_indent
        let anchor_col = self.column_of_position(anchor_span.start as usize);
        if anchor_col < min_indent {
            self.error(ErrorKind::InvalidIndentation, anchor_span);
            return None;
        }

        self.advance();
        self.skip_ws();

        if props.anchor.is_some() && !props.crossed_line_boundary {
            self.error(ErrorKind::DuplicateAnchor, anchor_span);
        }

        if props.crossed_line_boundary && props.anchor.is_some() {
            let inner_props = NodeProperties {
                anchor: Some((anchor_name.clone(), anchor_span)),
                tag: None,
                crossed_line_boundary: false,
            };
            if let Some(mapping) = self.parse_block_mapping_with_props(min_indent, inner_props) {
                Some(self.apply_properties_and_register(props, mapping))
            } else if props.anchor.is_some() {
                self.error(ErrorKind::DuplicateAnchor, anchor_span);
                props.anchor = Some((anchor_name, anchor_span));
                self.parse_value_with_properties(min_indent, props)
            } else {
                props.anchor = Some((anchor_name, anchor_span));
                self.parse_value_with_properties(min_indent, props)
            }
        } else {
            props.anchor = Some((anchor_name, anchor_span));
            self.parse_value_with_properties(min_indent, props)
        }
    }

    /// Handle a tag token in value parsing.
    ///
    /// Validates tag handles, checks for missing separators, handles duplicates,
    /// and either:
    /// - Tries to parse as a block mapping key (if crossed line boundary with existing tag)
    /// - Accumulates the tag as a property and continues parsing
    fn handle_tag_in_value(
        &mut self,
        tag_name: Cow<'input, str>,
        tag_span: Span,
        min_indent: usize,
        mut props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        self.advance();

        // Validate that named tag handles are declared in this document
        self.validate_tag_handle(&tag_name, tag_span);

        let tag_looks_legitimate = !tag_name.contains('"') && !tag_name.contains('`');
        let tag_end = tag_span.end;
        if tag_looks_legitimate && let Some((next_tok, next_span)) = self.peek() {
            let is_content = matches!(
                next_tok,
                Token::Plain(_)
                    | Token::StringStart(_)
                    | Token::FlowSeqStart
                    | Token::FlowMapStart
                    | Token::BlockSeqIndicator
            );
            if is_content && next_span.start == tag_end {
                self.error(ErrorKind::ContentOnSameLine, next_span);
            }
        }

        self.skip_ws();

        if props.tag.is_some() && !props.crossed_line_boundary {
            self.error(ErrorKind::DuplicateTag, tag_span);
        }

        if props.crossed_line_boundary && props.tag.is_some() {
            let inner_props = NodeProperties {
                anchor: None,
                tag: Some((tag_name.clone(), tag_span)),
                crossed_line_boundary: false,
            };
            if let Some(mapping) = self.parse_block_mapping_with_props(min_indent, inner_props) {
                Some(self.apply_properties_and_register(props, mapping))
            } else if props.tag.is_some() {
                self.error(ErrorKind::DuplicateTag, tag_span);
                props.tag = Some((tag_name, tag_span));
                self.parse_value_with_properties(min_indent, props)
            } else {
                props.tag = Some((tag_name, tag_span));
                self.parse_value_with_properties(min_indent, props)
            }
        } else {
            props.tag = Some((tag_name, tag_span));
            self.parse_value_with_properties(min_indent, props)
        }
    }

    /// Parse a value with already-collected node properties.
    #[allow(
        clippy::too_many_lines,
        reason = "Match arms for token dispatch are minimal; further extraction would reduce clarity"
    )]
    pub fn parse_value_with_properties(
        &mut self,
        min_indent: usize,
        props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        self.skip_ws();

        // If at EOF and we have properties (anchor/tag), return a null node with those properties.
        // This handles cases like `!` alone where the tag applies to an implicit null (UKK6-02).
        let Some((tok, span)) = self.peek() else {
            return if props.is_empty() {
                None
            } else {
                Some(self.apply_properties_and_register(props, Node::null(self.current_span())))
            };
        };

        match tok {
            // Flow mapping
            Token::FlowMapStart => {
                let start_pos = span.start as usize;
                self.parse_flow_mapping().and_then(|flow_node| {
                    self.handle_flow_collection_as_value(flow_node, start_pos, min_indent, props)
                })
            }
            // Flow sequence
            Token::FlowSeqStart => {
                let start_pos = span.start as usize;
                self.parse_flow_sequence().and_then(|flow_node| {
                    self.handle_flow_collection_as_value(flow_node, start_pos, min_indent, props)
                })
            }
            // Block sequence
            Token::BlockSeqIndicator => {
                if !props.is_empty() && !props.crossed_line_boundary {
                    if let Some((_, anchor_span)) = &props.anchor {
                        self.error(ErrorKind::ContentOnSameLine, *anchor_span);
                    }
                    if let Some((_, tag_span)) = &props.tag {
                        self.error(ErrorKind::ContentOnSameLine, *tag_span);
                    }
                }
                if let Some(n) = self.parse_block_sequence(min_indent) {
                    // Block sequence is a closed structure - check for trailing content
                    if min_indent == 0 {
                        self.check_trailing_content_at_root(0);
                    }
                    Some(self.apply_properties_and_register(props, n))
                } else {
                    None
                }
            }
            // Anchor - collect as property and continue parsing
            Token::Anchor(name) => {
                let anchor_name = name.clone();
                self.handle_anchor_in_value(anchor_name, span, min_indent, props)
            }
            // Alias
            Token::Alias(name) => {
                if !props.is_empty() && !props.crossed_line_boundary {
                    self.error(ErrorKind::PropertiesOnAlias, span);
                    self.parse_alias()
                } else {
                    let alias_name = name.clone();
                    let alias_span = span;
                    self.advance();
                    self.skip_ws();

                    if matches!(self.peek(), Some((Token::Colon, _))) {
                        Some(self.parse_alias_as_mapping_key(alias_name, alias_span, props))
                    } else {
                        if !self.anchors.contains_key(alias_name.as_ref()) {
                            self.error(ErrorKind::UndefinedAlias, alias_span);
                        }
                        Some(Node::new(Value::Alias(alias_name), alias_span))
                    }
                }
            }
            // Tag - collect as property and continue parsing
            Token::Tag(tag) => {
                let tag_name = tag.clone();
                self.handle_tag_in_value(tag_name, span, min_indent, props)
            }
            // Block scalars
            Token::LiteralBlockHeader(_) => self
                .parse_literal_block_scalar(min_indent)
                .map(|n| self.apply_properties_and_register(props, n)),
            Token::FoldedBlockHeader(_) => self
                .parse_folded_block_scalar(min_indent)
                .map(|n| self.apply_properties_and_register(props, n)),
            // Scalars
            Token::Plain(_) | Token::StringStart(_) => {
                if !props.is_empty() && self.is_mapping_key_pattern() {
                    self.parse_block_mapping_with_props(min_indent, props)
                } else {
                    self.parse_scalar_or_mapping(min_indent)
                        .map(|n| self.apply_properties_and_register(props, n))
                }
            }
            // Document markers
            Token::DocStart | Token::DocEnd => None,
            // Line start
            Token::LineStart(n) => {
                if *n >= min_indent {
                    self.advance();
                    while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                        self.advance();
                    }
                    self.check_tabs_as_indentation();
                    let mut new_props = props;
                    new_props.crossed_line_boundary = true;
                    self.parse_value_with_properties(min_indent, new_props)
                } else if !props.is_empty() {
                    Some(self.apply_properties_and_register(props, Node::null(span)))
                } else {
                    None
                }
            }
            // Skip comments
            Token::Comment(_) | Token::Indent(_) => {
                self.advance();
                self.parse_value_with_properties(min_indent, props)
            }
            // Mapping key indicator
            Token::MappingKey => {
                let node = self.parse_block_mapping(min_indent);
                Some(self.apply_properties_and_register(props, node))
            }
            // Empty key
            Token::Colon => {
                if props.is_empty() {
                    Some(self.parse_block_mapping_with_empty_key(min_indent))
                } else {
                    self.parse_block_mapping_with_tagged_null_key(min_indent, props)
                }
            }
            // Directives
            Token::YamlDirective(_) | Token::TagDirective(_) | Token::ReservedDirective(_) => {
                self.advance();
                self.skip_ws_and_newlines();
                self.parse_value_with_properties(min_indent, props)
            }
            // Indent
            // Dedent
            Token::Dedent => {
                if props.is_empty() {
                    None
                } else {
                    Some(self.apply_properties_and_register(props, Node::null(span)))
                }
            }
            // Invalid or unexpected
            _ => {
                self.error(ErrorKind::InvalidValue, span);
                self.advance();
                Some(self.apply_properties_and_register(props, Node::invalid(span)))
            }
        }
    }
}

/// Parse a token stream into a YAML stream (multiple documents).
///
/// Returns the parsed documents and any errors encountered.
/// Due to error recovery, partial values may be returned even when
/// errors are present.
pub fn parse_tokens<'input>(
    tokens: &'input [RichToken<'input>],
    input: &'input str,
) -> (Stream<'input>, Vec<ParseError>) {
    let mut parser = Parser::new(tokens, input);
    let stream = parser.parse_stream();
    (stream, parser.errors)
}

/// Parse a token stream as a single document (zero-copy API).
///
/// This is the **zero-copy parsing API**. The returned `Node<'input>` borrows string data
/// directly from the input string via `Cow<'input, str>`. Simple scalars that don't require
/// transformation (escapes, multiline folding) will be `Cow::Borrowed`, avoiding allocation.
///
/// Unlike `parse_tokens` which may produce multiple documents, this function:
/// 1. Parses exactly ONE value at indent 0
/// 2. Reports an error if there's remaining content that doesn't form a valid continuation
///
/// The `directives` parameter contains the %TAG and %YAML directives declared in this
/// document's prolog. These are used to validate tag handles.
///
/// Returns the parsed node (or None if empty) and any errors encountered.
///
/// # Lifetimes
///
/// - `'tokens`: lifetime of the token slice (must outlive `'input` for the bound)
/// - `'input`: lifetime of the input string (returned Node borrows from this)
///
/// The returned `Node<'input>` borrows string data from `input`. Call [`Node::into_owned()`]
/// to convert to `Node<'static>` with owned data when needed.
///
/// See `test_zero_copy_parsing` in the test module for a usage example.
pub fn parse_single_document<'tokens: 'input, 'input>(
    tokens: &'tokens [RichToken<'input>],
    input: &'input str,
    directives: &[Spanned<crate::stream_lexer::Directive>],
) -> (Option<Node<'input>>, Vec<ParseError>) {
    let mut parser = Parser::new(tokens, input);

    // Populate tag handles from the directives provided by the stream lexer
    parser.populate_tag_handles_from_directives(directives);

    parser.skip_ws_and_newlines();

    // Check for explicit document start marker `---`
    let has_doc_start = matches!(parser.peek(), Some((Token::DocStart, _)));
    let mut content_on_start_line = false;

    if has_doc_start {
        parser.advance(); // consume DocStart

        // Skip whitespace after `---` (but NOT newlines yet)
        while matches!(
            parser.peek(),
            Some((Token::Whitespace | Token::WhitespaceWithTabs, _))
        ) {
            parser.advance();
        }

        // Check if there's content on the same line as `---`
        // (i.e., NOT a newline/LineStart immediately after)
        if !parser.is_eof()
            && !matches!(
                parser.peek(),
                Some((Token::LineStart(_) | Token::DocEnd, _))
            )
        {
            content_on_start_line = true;
        }

        // Skip newlines after `---`
        parser.skip_ws_and_newlines();
    }

    // Check if the first token starts a block mapping on the --- line
    // This is invalid per YAML spec (block collections require s-l-comments before them)
    if content_on_start_line && !parser.is_eof() {
        let has_block_mapping_on_start_line = parser.check_block_mapping_on_start_line();
        if has_block_mapping_on_start_line {
            // Report error but continue parsing for recovery
            parser.error(ErrorKind::ContentOnSameLine, parser.current_span());
        }
    }

    let doc = if parser.is_eof() || matches!(parser.peek(), Some((Token::DocEnd, _))) {
        // If we had an explicit document start (---), we should return a null node
        // rather than None. An explicit document with no content is a null value.
        has_doc_start.then(|| Node::null(Span::new(0..0)))
    } else {
        parser.parse_value(0)
    };

    // After parsing the document, skip remaining whitespace, newlines, and Dedent tokens.
    loop {
        parser.skip_ws_and_newlines();
        if matches!(parser.peek(), Some((Token::Dedent, _))) {
            parser.advance();
        } else {
            break;
        }
    }

    // Check for document end marker `...`
    if matches!(parser.peek(), Some((Token::DocEnd, _))) {
        parser.advance(); // consume DocEnd
        parser.skip_ws_and_newlines();
    }

    // Check for remaining content that wasn't consumed
    while !parser.is_eof() {
        if matches!(parser.peek(), Some((Token::Dedent, _))) {
            parser.advance();
            continue;
        }
        // Stop at another DocStart (shouldn't happen in single doc, but be safe)
        if matches!(parser.peek(), Some((Token::DocStart, _))) {
            break;
        }

        let col = parser.current_token_column();

        if col > 0 {
            parser.error(ErrorKind::TrailingContent, parser.current_span());
            parser.advance();
            parser.skip_ws_and_newlines();
        } else {
            break;
        }
    }

    (doc, parser.errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context_lexer::tokenize_document;

    fn parse(input: &str) -> (Stream<'static>, Vec<ParseError>) {
        let (tokens, _) = tokenize_document(input);
        let (stream, errors) = parse_tokens(&tokens, input);
        // Convert to owned data since tokens is local
        (stream.into_iter().map(Node::into_owned).collect(), errors)
    }

    #[test]
    fn test_parse_simple_scalar() {
        let (docs, errors) = parse("hello");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        assert!(matches!(&docs.first().unwrap().value, Value::String(string) if string == "hello"));
    }

    #[test]
    fn test_parse_simple_mapping() {
        let (docs, errors) = parse("key: value");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        let value = &docs.first().unwrap().value;
        assert!(matches!(value, Value::Mapping(_)));
        if let Value::Mapping(pairs) = value {
            assert_eq!(pairs.len(), 1);
            let pair = pairs.first().unwrap();
            assert!(matches!(&pair.0.value, Value::String(string) if string == "key"));
            assert!(matches!(&pair.1.value, Value::String(string) if string == "value"));
        }
    }

    #[test]
    fn test_parse_flow_mapping() {
        let (docs, errors) = parse("{a: 1, b: 2}");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        let value = &docs.first().unwrap().value;
        assert!(matches!(value, Value::Mapping(_)));
        if let Value::Mapping(pairs) = value {
            assert_eq!(pairs.len(), 2);
        }
    }

    #[test]
    fn test_parse_flow_sequence() {
        let (docs, errors) = parse("[1, 2, 3]");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        let value = &docs.first().unwrap().value;
        assert!(matches!(value, Value::Sequence(_)));
        if let Value::Sequence(items) = value {
            assert_eq!(items.len(), 3);
        }
    }

    #[test]
    fn test_parse_block_sequence() {
        let (docs, errors) = parse("- a\n- b\n- c");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        let value = &docs.first().unwrap().value;
        assert!(matches!(value, Value::Sequence(_)));
        if let Value::Sequence(items) = value {
            assert_eq!(items.len(), 3);
        }
    }

    #[test]
    fn test_parse_null_values() {
        let (docs, errors) = parse("~");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        assert!(matches!(&docs.first().unwrap().value, Value::Null));
    }

    #[test]
    fn test_parse_boolean_values() {
        let (docs, errors) = parse("true");
        assert!(errors.is_empty());
        assert!(matches!(&docs.first().unwrap().value, Value::Bool(true)));
    }

    #[test]
    fn test_parse_number_values() {
        let (docs, errors) = parse("42");
        assert!(errors.is_empty());
        assert!(matches!(&docs.first().unwrap().value, Value::Int(42)));

        let (docs_, errors_) = parse("3.45");
        assert!(errors_.is_empty());
        assert_eq!(docs_.len(), 1);
        assert_eq!(docs_.first().unwrap().value, Value::Float(3.45));
    }

    #[test]
    fn test_parse_multi_document() {
        let (docs, _) = parse("---\na\n---\nb");
        assert!(docs.len() >= 2);
    }

    #[test]
    fn test_parse_anchor_alias() {
        let (docs, errors) = parse("a: &anchor 1\nb: *anchor");
        assert!(errors.is_empty());
        assert_eq!(docs.len(), 1);
        let value = &docs.first().unwrap().value;
        assert!(matches!(&docs.first().unwrap().value, Value::Mapping(_)));
        if let Value::Mapping(pairs) = value {
            assert_eq!(pairs.len(), 2);
            assert!(
                matches!(&pairs.last().unwrap().1.value, Value::Alias(name) if name == "anchor")
            );
        }
    }

    #[test]
    fn test_multiline_quoted_key_error() {
        let input = "\"c\n d\": 1";
        let (_, errors) = parse(input);
        assert!(!errors.is_empty());
    }
}
