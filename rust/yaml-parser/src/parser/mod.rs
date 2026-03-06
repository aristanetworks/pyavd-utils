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
mod emitter;
mod flow;
mod scalar;
mod streaming;

pub use emitter::Emitter;
pub use streaming::StreamingParser;

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

use crate::error::{ErrorKind, ParseError};
use crate::event::{CollectionStyle, Event, ScalarStyle};
use crate::lexer::{RichToken, Token};
use crate::span::{IndentLevel, Span, usize_to_indent};
#[cfg(test)]
use crate::value::Value;
use crate::value::{Node, Properties};

/// A stream of YAML documents.
pub type Stream<'input> = Vec<Node<'input>>;

/// Pending node properties (anchor, tag) collected before parsing the value.
///
/// The lifetime `'input` refers to the input string being parsed.
/// Anchors use `&'input str` (zero-copy), tags use `Cow` (may need transformation).
#[derive(Debug, Clone, Default)]
pub(crate) struct NodeProperties<'input> {
    pub anchor: Option<(&'input str, Span)>,
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

    /// Returns true if there are no actual properties (anchor/tag), ignoring flags.
    /// This is useful for checking if we have collected any real node properties
    /// regardless of the `crossed_line_boundary` flag.
    pub fn is_empty_excluding_flag(&self) -> bool {
        self.anchor.is_none() && self.tag.is_none()
    }

    /// Apply these properties to a node, updating its span to include properties.
    pub fn apply_to(self, mut node: Node<'input>) -> Node<'input> {
        let anchor_val = self.anchor.map(|(anchor, _)| Cow::Borrowed(anchor));
        let tag_val = self.tag.as_ref().map(|(tag, _)| tag.clone());

        // Update span to include properties
        if let Some((_, anchor_span)) = &self.anchor
            && anchor_span.start < node.span.start
        {
            node.span = Span::new(anchor_span.start..node.span.end);
        }
        if let Some((_, tag_span)) = &self.tag
            && tag_span.start < node.span.start
        {
            node.span = Span::new(tag_span.start..node.span.end);
        }

        // Only allocate a box if there are actual properties to add.
        // Important: Don't overwrite existing properties if we have nothing to add!
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
/// The bound `'tokens: 'input` is required due to Rust's variance rules. When the parser
/// is used behind `&mut self`, the compiler requires lifetime invariance. Without this
/// bound, recursive methods that pass `&mut self` fail to compile because the compiler
/// cannot prove that the lifetimes are compatible.
#[derive(Debug)]
pub(crate) struct Parser<'tokens: 'input, 'input> {
    pub tokens: &'tokens [RichToken<'input>],
    pub input: &'input str,
    pub pos: usize,
    pub errors: Vec<ParseError>,
    /// Set of anchor names defined in this document.
    /// Used to check for undefined anchor errors when aliases are encountered.
    /// The actual anchor values are reconstructed by EventParser from the event stream.
    pub anchors: HashSet<&'input str>,
    /// Flow depth tracking (0 = block context, > 0 = inside flow collections)
    pub flow_depth: usize,
    /// Stack of columns where each flow context started.
    /// Used to validate that continuation lines are indented relative to the flow start.
    /// Empty when not in flow context.
    pub flow_context_columns: Vec<IndentLevel>,
    /// Indentation stack tracking active block structure levels.
    /// Each entry is the indentation level of an active block structure.
    /// Used to detect orphan indentation (content at levels not in the stack).
    pub indent_stack: Vec<IndentLevel>,
    /// Tag handles declared in this document's prolog via %TAG directives.
    /// Key: handle like "!prefix!", Value: prefix/URI like "tag:example.com,2011:"
    pub tag_handles: HashMap<String, String>,
    /// Event buffer for the event-emitting mode.
    /// When populated, the parser emits events instead of (or in addition to) building nodes.
    pub events: Vec<Event<'input>>,
    /// Current line's indentation level, tracked from the most recent `LineStart` token.
    /// Updated by `advance()` when consuming `LineStart` tokens.
    /// This eliminates the need for backward-scanning in `current_indent()`.
    pub current_line_indent: IndentLevel,
}

impl<'tokens: 'input, 'input> Parser<'tokens, 'input> {
    pub fn new(tokens: &'tokens [RichToken<'input>], input: &'input str) -> Self {
        Self {
            tokens,
            input,
            pos: 0,
            errors: Vec::new(),
            anchors: HashSet::new(),
            flow_depth: 0,
            flow_context_columns: Vec::new(),
            indent_stack: vec![0], // Start with base level 0
            tag_handles: HashMap::new(),
            events: Vec::new(),
            current_line_indent: 0, // Start at column 0
        }
    }

    /// Emit an event to the event buffer.
    pub fn emit(&mut self, event: Event<'input>) {
        self.events.push(event);
    }

    /// Take the collected events, leaving an empty buffer.
    pub fn take_events(&mut self) -> Vec<Event<'input>> {
        std::mem::take(&mut self.events)
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

    /// Peek at the nth token ahead without consuming it (0 = current token).
    /// Returns the full `RichToken` for cases that need span or error info.
    ///
    /// This is the foundation for streaming: it abstracts lookahead access.
    /// Currently backed by slice indexing; can be replaced with buffer fill.
    pub fn peek_nth(&self, n: usize) -> Option<&RichToken<'input>> {
        self.tokens.get(self.pos + n)
    }

    /// Consume the current token and return it.
    /// Returns the token and its span for easy destructuring.
    ///
    /// Side effect: Updates `self.current_line_indent` when consuming `LineStart` tokens.
    pub fn advance(&mut self) -> Option<(&Token<'input>, Span)> {
        self.tokens.get(self.pos).map(|rt| {
            self.pos += 1;
            // Track indent level when we consume a LineStart token
            if let Token::LineStart(n) = &rt.token {
                self.current_line_indent = *n;
            }
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
    pub fn skip_ws_and_newlines(&mut self) {
        while let Some((tok, span)) = self.peek() {
            match tok {
                Token::LineStart(indent) => {
                    let indent = *indent;
                    self.advance();

                    // In flow context, check for invalid column 0 continuations
                    // Column 0 is only allowed if the outermost flow context started at column 0
                    if let Some(&outermost_flow_col) = self.flow_context_columns.first()
                        && indent == 0
                        && outermost_flow_col > 0
                    {
                        // Check if there's actual content after this LineStart
                        // Skip past Whitespace tokens (tabs used as indentation)
                        // and check the actual column of the content token
                        let mut peek_offset = 0;
                        while let Some(rt) = self.peek_nth(peek_offset) {
                            if matches!(rt.token, Token::Whitespace | Token::WhitespaceWithTabs) {
                                peek_offset += 1;
                            } else {
                                break;
                            }
                        }

                        if let Some(rt) = self.peek_nth(peek_offset) {
                            let is_content = !matches!(
                                rt.token,
                                Token::LineStart(_)
                                    | Token::FlowSeqEnd
                                    | Token::FlowMapEnd
                                    | Token::Dedent
                                    | Token::DocEnd
                            );
                            // Check actual column of content token
                            let content_col = self.column_of_position(rt.span.start_usize());
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
    pub fn current_token_column(&self) -> IndentLevel {
        if let Some((_, span)) = self.peek() {
            self.column_of_position(span.start_usize())
        } else {
            0
        }
    }

    /// Get the column (0-based) of a byte position by looking back to the last newline.
    #[allow(
        clippy::string_slice,
        reason = "Position is validated to be within input bounds"
    )]
    pub fn column_of_position(&self, pos: usize) -> IndentLevel {
        let before = &self.input[..pos];
        let col = if let Some(newline_pos) = before.rfind('\n') {
            pos - newline_pos - 1
        } else {
            pos // No newline, column is the byte position from start of input
        };
        usize_to_indent(col)
    }

    /// Get the end position of the last emitted event.
    ///
    /// Returns the end position from the last event's span, or 0 if no events have been emitted.
    /// This allows tracking span information through events instead of Node structures.
    pub fn last_event_end_position(&self) -> usize {
        self.events
            .last()
            .and_then(Event::span)
            .map_or(0, |span| span.end_usize())
    }

    /// Emit an empty scalar event and return its span.
    ///
    /// Use this whenever a null/empty value needs to be created without properties.
    /// This ensures the event stream stays in sync with the AST.
    /// Callers that need a Node can wrap with `Node::null(span)`.
    pub fn emit_null_scalar(&mut self) -> Span {
        let null_span = self.current_span();
        self.emit(Event::Scalar {
            style: ScalarStyle::Plain,
            value: Cow::Borrowed(""),
            anchor: None,
            tag: None,
            span: null_span,
        });
        null_span
    }

    /// Apply node properties to a node and register the anchor if present.
    pub fn apply_properties_and_register(
        &mut self,
        props: NodeProperties<'input>,
        node: Node<'input>,
    ) -> Node<'input> {
        // Extract anchor name before applying (to get the 'input lifetime)
        let anchor_name = props.anchor.as_ref().map(|(name, _)| *name);

        // If we have properties, update the relevant event to include them
        // This handles cases where scalars/collections were emitted before properties were known
        if !props.is_empty() {
            self.apply_properties_to_events(&props);
        }

        let node_with_props = props.apply_to(node);
        // If the node has an anchor, register it in the anchors map
        if let Some(name) = anchor_name {
            self.anchors.insert(name);
        }
        node_with_props
    }

    /// Apply properties (anchor/tag) to the appropriate event in the event buffer.
    ///
    /// If the last event is a Scalar or collection Start, update it directly.
    /// If the last event is a collection End, find the matching Start and update it.
    fn apply_properties_to_events(&mut self, props: &NodeProperties<'input>) {
        let Some(last_event) = self.events.last() else {
            return;
        };

        // Check if the last event is a collection end - if so, find the matching start
        let target_idx = match last_event {
            Event::SequenceEnd { .. } => self.find_matching_sequence_start(),
            Event::MappingEnd { .. } => self.find_matching_mapping_start(),
            _ => Some(self.events.len() - 1),
        };

        let Some(idx) = target_idx else {
            return;
        };

        let Some(target_event) = self.events.get_mut(idx) else {
            return;
        };

        match target_event {
            Event::Scalar { anchor, tag, .. }
            | Event::MappingStart { anchor, tag, .. }
            | Event::SequenceStart { anchor, tag, .. } => {
                if anchor.is_none() {
                    *anchor = props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp));
                }
                if tag.is_none() {
                    *tag = props.tag.clone();
                }
            }
            _ => {}
        }
    }

    /// Find the index of the matching SequenceStart for the last SequenceEnd.
    fn find_matching_sequence_start(&self) -> Option<usize> {
        let mut depth = 0;
        for (i, event) in self.events.iter().enumerate().rev() {
            match event {
                Event::SequenceEnd { .. } => depth += 1,
                Event::SequenceStart { .. } => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(i);
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Find the index of the matching MappingStart for the last MappingEnd.
    fn find_matching_mapping_start(&self) -> Option<usize> {
        let mut depth = 0;
        for (i, event) in self.events.iter().enumerate().rev() {
            match event {
                Event::MappingEnd { .. } => depth += 1,
                Event::MappingStart { .. } => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(i);
                    }
                }
                _ => {}
            }
        }
        None
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
                    let anchor_name = *name;
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
                    // Expand tag to full URI form
                    let expanded_tag: Cow<'input, str> =
                        Cow::Owned(self.expand_tag(&tag_name, tag_span));
                    props.tag = Some((expanded_tag, tag_span));
                }
                Some((Token::Whitespace | Token::WhitespaceWithTabs, _)) => {
                    self.advance();
                }
                _ => break,
            }
        }
        props
    }

    /// Parse the value after a colon in a mapping.
    ///
    /// Handles LineStart/Indent tokens before the value and returns a null node
    /// if no value is present.
    pub fn parse_mapping_value(&mut self, map_indent: IndentLevel) -> Node<'input> {
        let value = match self.peek() {
            Some((Token::LineStart(n), _)) => {
                // If the next line is at strictly lower indentation, value is implicitly null.
                // Don't advance past LineStart - let the caller handle dedent.
                if *n < map_indent {
                    let null_span = self.current_span();
                    // Emit empty scalar event for null value
                    self.emit(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        anchor: None,
                        tag: None,
                        span: null_span,
                    });
                    return Node::null(null_span);
                }
                self.advance();
                while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                    self.advance();
                }
                self.parse_value(map_indent + 1)
            }
            Some((Token::Indent(_), _)) => {
                self.advance();
                self.parse_value(map_indent + 1)
            }
            Some((Token::Anchor(_) | Token::Tag(_), _)) => {
                // Collect properties first, then check for block collection on next line
                let props = self.collect_node_properties(NodeProperties::default());

                // After properties, check if we have LineStart
                // If the next line is dedented, this is a null value with properties
                // (don't bridge to dedented content - the properties belong to this value)
                if let Some((Token::LineStart(n), _)) = self.peek() {
                    if *n < map_indent + 1 {
                        // Dedented - emit null scalar with properties
                        let null_span = self.current_span();
                        self.emit(Event::Scalar {
                            style: ScalarStyle::Plain,
                            value: Cow::Borrowed(""),
                            anchor: props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
                            tag: props.tag.clone(),
                            span: null_span,
                        });
                        let null_node = Node::null(null_span);
                        let node_with_props = props.clone().apply_to(null_node);
                        if let Some((name, _)) = &props.anchor {
                            self.anchors.insert(name);
                        }
                        return node_with_props;
                    }
                }

                // Normal case - continue parsing value with properties
                self.parse_value_with_properties(map_indent + 1, props)
            }
            _ => self.parse_value(map_indent + 1),
        };
        value.unwrap_or_else(|| {
            let null_span = self.current_span();
            // Emit empty scalar event for null value
            self.emit(Event::Scalar {
                style: ScalarStyle::Plain,
                value: Cow::Borrowed(""),
                anchor: None,
                tag: None,
                span: null_span,
            });
            Node::null(null_span)
        })
    }

    /// Push an indentation level onto the stack when entering a block structure.
    pub fn push_indent(&mut self, indent: IndentLevel) {
        self.indent_stack.push(indent);
    }

    /// Pop an indentation level from the stack when exiting a block structure.
    pub fn pop_indent(&mut self) {
        if self.indent_stack.len() > 1 {
            self.indent_stack.pop();
        }
    }

    /// Check if an indentation level is valid (exists in the indent stack).
    ///
    /// Returns `true` if the indent is in the stack or is greater than all stack entries
    /// (which indicates a new nested level). Returns `false` if the indent is an
    /// "orphan" level that falls between existing stack entries.
    pub fn is_valid_indent(&self, indent: IndentLevel) -> bool {
        // Check if indent matches any level in the stack
        if self.indent_stack.contains(&indent) {
            return true;
        }
        // Check if indent is greater than all stack entries (new nested level)
        if let Some(&top) = self.indent_stack.last() {
            if indent > top {
                return true;
            }
        }
        // Indent falls between stack entries - this is an orphan indentation error
        false
    }

    /// Check if there's whitespace (tabs) after `LineStart` that would indicate
    /// invalid tab indentation.
    ///
    /// Tabs are invalid for indentation in both block and flow contexts.
    /// Even inside flow collections, if a line starts with tabs before content,
    /// that's considered invalid indentation (test Y79Y-003).
    pub fn check_tabs_as_indentation(&mut self) {
        // Check: is the previous token LineStart?
        // If so, check if the current token is WhitespaceWithTabs
        // (which would indicate tabs used for indentation)
        let Some(prev_rt) = self.pos.checked_sub(1).and_then(|i| self.tokens.get(i)) else {
            return;
        };
        if !matches!(prev_rt.token, Token::LineStart(_)) {
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
            let col = self.column_of_position(tab_span.start_usize());
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
            if is_content && span.start_usize() == flow_end {
                self.error(ErrorKind::ContentOnSameLine, span);
            }
        }
    }

    /// Check for trailing content at column 0 after a closed structure.
    /// This is used for flow collections and block sequences at the root level.
    /// For these structures, content at the same indentation level after them is invalid.
    ///
    /// This uses peek-only lookahead (no position modification), enabling streaming.
    pub fn check_trailing_content_at_root(&mut self, root_indent: IndentLevel) {
        // Skip whitespace, newlines, and dedent tokens using peek-only
        let mut peek_offset = 0;
        while let Some(rt) = self.peek_nth(peek_offset) {
            match &rt.token {
                Token::Whitespace
                | Token::WhitespaceWithTabs
                | Token::Comment(_)
                | Token::Dedent
                | Token::LineStart(_) => {
                    peek_offset += 1;
                }
                _ => break,
            }
        }

        // Check if there's content at or below the root indentation
        if let Some(rt) = self.peek_nth(peek_offset) {
            let tok = &rt.token;
            let span = rt.span;

            // Extra flow collection end tokens are always an error (unmatched brackets)
            if matches!(tok, Token::FlowSeqEnd | Token::FlowMapEnd) {
                self.error(ErrorKind::UnmatchedBracket, span);
            } else {
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

                // Get the column of this token by its span position
                let col = self.column_of_position(span.start_usize());

                if is_content && col <= root_indent {
                    self.error(ErrorKind::TrailingContent, span);
                }
            }
        }
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
            if rt.span.start_usize() > key_start
                && rt.span.end_usize() <= key_end
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
    ///
    /// This returns the tracked `current_line_indent` value, which is updated
    /// by `advance()` whenever a `LineStart` token is consumed.
    /// This eliminates backward-scanning, enabling streaming parsing.
    pub fn current_indent(&self) -> IndentLevel {
        self.current_line_indent
    }

    /// Get the span of the current position.
    pub fn current_span(&self) -> Span {
        self.peek()
            .map_or_else(|| Span::from_usize_range(0..0), |(_, span)| span)
    }

    /// Check if current position is a mapping key pattern (scalar followed by colon).
    /// This is used to determine if properties (anchor/tag) apply to a key or mapping.
    fn is_mapping_key_pattern(&self) -> bool {
        let mut i = self.pos;
        match self.tokens.get(i) {
            Some(rt) if matches!(rt.token, Token::Plain(_)) => {
                i += 1;
            }
            Some(rt) if matches!(rt.token, Token::StringStart(_)) => {
                // Skip the entire quoted string (StringStart, StringContent*, LineStart*, StringEnd)
                i += 1;
                while let Some(inner) = self.tokens.get(i) {
                    match &inner.token {
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

    /// Populate tag handles from `TagDirective` tokens in the token stream.
    ///
    /// This scans the tokens for `TagDirective` tokens that appear before
    /// for this document's prologue.
    ///
    /// Scans from the CURRENT position (not beginning) to handle multi-document
    /// streams where each document may have its own directives.
    pub fn populate_tag_handles_from_tokens(&mut self) {
        self.tag_handles.clear();

        // Scan from current position, not from beginning
        for rich_token in self.tokens.get(self.pos..).unwrap_or_default() {
            match &rich_token.token {
                Token::TagDirective(value) => {
                    // Parse "!prefix! tag:example.com,2011:" into handle + prefix
                    if let Some((handle, prefix)) = Self::parse_tag_directive_value(value) {
                        self.tag_handles.insert(handle, prefix);
                    }
                }
                // Stop scanning when we hit document content
                Token::DocStart | Token::Plain(_) | Token::BlockSeqIndicator | Token::Colon => {
                    break;
                }
                // Skip whitespace, comments, YAML directives, and other directives
                Token::YamlDirective(_)
                | Token::ReservedDirective(_)
                | Token::Whitespace
                | Token::WhitespaceWithTabs
                | Token::LineStart(_)
                | Token::Comment(_)
                | Token::Indent(_)
                | Token::Dedent => {}
                // Any other token means we're in content
                _ => break,
            }
        }
    }

    /// Parse a %TAG directive value like "!prefix! tag:example.com,2011:"
    /// into (handle, prefix) tuple.
    fn parse_tag_directive_value(value: &str) -> Option<(String, String)> {
        // value is like "!prefix! tag:example.com,2011:"
        // or "! !" for primary handle
        let mut parts = value.splitn(2, ' ');
        match (parts.next(), parts.next()) {
            (Some(handle), Some(prefix)) => Some((handle.to_owned(), prefix.to_owned())),
            _ => None,
        }
    }

    /// Parse a single document and emit events (without StreamStart/StreamEnd).
    ///
    /// This is used by `parse_stream_from_tokens` to parse each document in a
    /// multi-document stream. It handles:
    /// - Optional `---` document start marker
    /// - Document content
    /// - Optional `...` document end marker
    ///
    /// Returns `true` if the document ended with explicit `...`, `false` otherwise.
    pub fn parse_document_events(&mut self) -> bool {
        // Check for explicit document start marker `---`
        let has_doc_start = matches!(self.peek(), Some((Token::DocStart, _)));
        let doc_start_span = self.current_span();
        let mut content_on_start_line = false;

        if has_doc_start {
            self.advance(); // consume DocStart

            // Skip whitespace after `---` (but NOT newlines yet)
            while matches!(
                self.peek(),
                Some((Token::Whitespace | Token::WhitespaceWithTabs, _))
            ) {
                self.advance();
            }

            // Check if there's content on the same line as `---`
            if !self.is_eof()
                && !matches!(self.peek(), Some((Token::LineStart(_) | Token::DocEnd, _)))
            {
                content_on_start_line = true;
            }

            // Skip newlines after `---`
            self.skip_ws_and_newlines();
        }

        // Check if the first token starts a block mapping on the --- line
        if content_on_start_line && !self.is_eof() {
            let has_block_mapping_on_start_line = self.check_block_mapping_on_start_line();
            if has_block_mapping_on_start_line {
                self.error(ErrorKind::ContentOnSameLine, self.current_span());
            }
        }

        // Emit DocumentStart
        self.emit(Event::DocumentStart {
            explicit: has_doc_start,
            span: doc_start_span,
        });

        // Parse document content if not empty
        // A document is empty if we hit EOF, `...`, or another `---` (next document)
        if self.is_eof() || matches!(self.peek(), Some((Token::DocEnd | Token::DocStart, _))) {
            // Empty document after explicit ---
            if has_doc_start {
                let null_span = Span::from_usize_range(0..0);
                self.emit(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor: None,
                    tag: None,
                    span: null_span,
                });
            }
        } else {
            // Parse document content
            let _ = self.parse_value(0);
        }

        // After parsing, skip remaining whitespace and Dedent tokens
        loop {
            self.skip_ws_and_newlines();
            if matches!(self.peek(), Some((Token::Dedent, _))) {
                self.advance();
            } else {
                break;
            }
        }

        // Consume any remaining content that belongs to this document
        // (content before the next document marker)
        while !self.is_eof() {
            if matches!(self.peek(), Some((Token::Dedent, _))) {
                self.advance();
                continue;
            }
            // Stop at document markers
            if matches!(self.peek(), Some((Token::DocStart | Token::DocEnd, _))) {
                break;
            }
            // Stop at directives (they belong to the next document's prologue)
            if matches!(
                self.peek(),
                Some((
                    Token::YamlDirective(_) | Token::TagDirective(_) | Token::ReservedDirective(_),
                    _
                ))
            ) {
                break;
            }

            // Check if this is orphan content that should trigger an error.
            // Skip whitespace, indent, and line start tokens without error.
            // Content tokens (Plain, StringStart, etc.) at ANY column are errors because
            // they weren't consumed by the normal parsing flow.
            if let Some((token, span)) = self.peek() {
                let is_content = matches!(
                    token,
                    Token::Plain(_)
                        | Token::StringStart(_)
                        | Token::Colon
                        | Token::MappingKey
                        | Token::BlockSeqIndicator
                        | Token::Anchor(_)
                        | Token::Tag(_)
                        | Token::Alias(_)
                        | Token::FlowMapStart
                        | Token::FlowSeqStart
                );
                if is_content {
                    // This is orphan content that wasn't consumed - report error
                    self.error(ErrorKind::TrailingContent, span);
                }
            }
            self.advance();
            self.skip_ws_and_newlines();
        }

        // Check for document end marker `...`
        let has_doc_end = matches!(self.peek(), Some((Token::DocEnd, _)));
        let doc_end_span = self.current_span();
        if has_doc_end {
            self.advance(); // consume DocEnd
            self.skip_ws_and_newlines();
        }

        // Emit DocumentEnd
        self.emit(Event::DocumentEnd {
            explicit: has_doc_end,
            span: doc_end_span,
        });

        // Skip trailing Dedent tokens
        while matches!(self.peek(), Some((Token::Dedent, _))) {
            self.advance();
        }

        has_doc_end
    }

    /// Expand a tag to its full URI form and validate handle declaration.
    ///
    /// Tag formats from lexer (leading `!` stripped):
    /// - `!type` → secondary handle `!!type` → expands to `tag:yaml.org,2002:type`
    /// - `prefix!suffix` → named handle `!prefix!suffix` → lookup and expand
    /// - `type` → primary/local tag `!type` → stored as `!type`
    /// - empty → non-specific tag `!` → stored as `!`
    /// - URI with `:` → verbatim tag → stored as-is
    ///
    /// Returns the expanded tag as an owned string.
    #[allow(
        clippy::string_slice,
        reason = "bang_pos is from find('!') so slicing up to it is safe"
    )]
    pub fn expand_tag(&mut self, tag: &str, span: Span) -> String {
        /// Decode percent-encoded characters in a tag suffix.
        /// E.g., `tag%21` → `tag!` (since %21 is '!')
        fn percent_decode(input: &str) -> String {
            let mut result = String::with_capacity(input.len());
            let mut chars = input.chars().peekable();
            while let Some(ch) = chars.next() {
                if ch == '%' {
                    // Try to read two hex digits
                    let hex: String = chars.by_ref().take(2).collect();
                    if hex.len() == 2
                        && let Ok(byte) = u8::from_str_radix(&hex, 16)
                    {
                        result.push(char::from(byte));
                        continue;
                    }
                    // Failed to decode, keep as-is
                    result.push('%');
                    result.push_str(&hex);
                } else {
                    result.push(ch);
                }
            }
            result
        }

        // Verbatim tag: marked with leading '\0' by lexer - return as-is (without marker)
        // The lexer strips !< and > so we just get the URI content
        if let Some(verbatim) = tag.strip_prefix('\0') {
            return verbatim.to_owned();
        }

        // Secondary handle: original !!type, stored as !type
        // Check if !! has been redefined via %TAG, otherwise use default
        if let Some(suffix) = tag.strip_prefix('!') {
            let decoded_suffix = percent_decode(suffix);
            if let Some(prefix) = self.tag_handles.get("!!") {
                return format!("{prefix}{decoded_suffix}");
            }
            // Default secondary handle expands to tag:yaml.org,2002:
            return format!("tag:yaml.org,2002:{decoded_suffix}");
        }

        // Empty tag (just `!`) - non-specific tag
        if tag.is_empty() {
            return "!".to_owned();
        }

        // Check for named handle: original !name!suffix, stored as name!suffix
        if let Some(bang_pos) = tag.find('!') {
            let handle = format!("!{}!", &tag[0..bang_pos]);
            let suffix = &tag[bang_pos + 1..];
            if let Some(prefix) = self.tag_handles.get(&handle) {
                let decoded_suffix = percent_decode(suffix);
                return format!("{prefix}{decoded_suffix}");
            }
            // Handle not declared - emit error and return unexpanded
            self.error(ErrorKind::UndefinedTagHandle, span);
            return format!("!{tag}");
        }

        // Primary handle: original !type, stored as type (no '!' in the value)
        // Check if primary handle `!` has been redefined via %TAG ! prefix
        if let Some(prefix) = self.tag_handles.get("!") {
            let decoded_tag = percent_decode(tag);
            return format!("{prefix}{decoded_tag}");
        }

        // No redefinition - use as local tag with leading !
        format!("!{tag}")
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
                    // Empty document - emit null scalar event
                    documents.push(Node::null(self.emit_null_scalar()));
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
    pub fn parse_value(&mut self, min_indent: IndentLevel) -> Option<Node<'input>> {
        self.parse_value_with_properties(min_indent, NodeProperties::default())
    }

    /// Check if a flow collection starting at the current position will be used as a mapping key.
    ///
    /// Scans ahead to find the matching close bracket and checks if it's followed by a colon.
    /// This is used to emit `MappingStart` BEFORE parsing the flow collection so events
    /// are in the correct order.
    fn flow_collection_is_mapping_key(&self) -> bool {
        let start_token = match self.peek() {
            Some((Token::FlowSeqStart, _)) => Token::FlowSeqStart,
            Some((Token::FlowMapStart, _)) => Token::FlowMapStart,
            _ => return false,
        };

        let (open_token, close_token) = match start_token {
            Token::FlowSeqStart => (Token::FlowSeqStart, Token::FlowSeqEnd),
            Token::FlowMapStart => (Token::FlowMapStart, Token::FlowMapEnd),
            _ => return false,
        };

        // Scan for matching close bracket
        let mut depth = 0i32;
        let mut pos = self.pos;

        while let Some(rich_tok) = self.tokens.get(pos) {
            match &rich_tok.token {
                tok if std::mem::discriminant(tok) == std::mem::discriminant(&open_token) => {
                    depth += 1;
                }
                tok if std::mem::discriminant(tok) == std::mem::discriminant(&close_token) => {
                    depth -= 1;
                    if depth == 0 {
                        // Found matching close - check what follows
                        pos += 1;
                        // Skip whitespace
                        while let Some(next_tok) = self.tokens.get(pos) {
                            if matches!(
                                next_tok.token,
                                Token::Whitespace | Token::WhitespaceWithTabs
                            ) {
                                pos += 1;
                            } else {
                                break;
                            }
                        }
                        // Check for colon
                        return matches!(
                            self.tokens.get(pos),
                            Some(RichToken {
                                token: Token::Colon,
                                ..
                            })
                        );
                    }
                }
                _ => {}
            }
            pos += 1;
        }

        false
    }

    /// Handle an anchor token in value parsing.
    ///
    /// Validates indentation, checks for duplicates, and either:
    /// - Tries to parse as a block mapping key (if crossed line boundary with existing anchor)
    /// - Accumulates the anchor as a property and continues parsing
    fn handle_anchor_in_value(
        &mut self,
        anchor_name: &'input str,
        anchor_span: Span,
        min_indent: IndentLevel,
        mut props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        // Check anchor indentation - must be >= min_indent
        let anchor_col = self.column_of_position(anchor_span.start_usize());
        if anchor_col < min_indent {
            self.error(ErrorKind::InvalidIndentation, anchor_span);
            return None;
        }

        self.advance();
        self.skip_ws();

        if props.anchor.is_some() && !props.crossed_line_boundary {
            self.error(ErrorKind::DuplicateAnchor, anchor_span);
        }

        // When we crossed a line boundary AND have existing properties (tag/anchor),
        // this anchor starts a NEW node (e.g., a mapping key), not a continuation.
        // For example: `!!map\n&a8 !!str key8: value7`
        //   - !!map on line 1 → tag for the mapping
        //   - &a8 !!str on line 2 → properties for the key
        // We try to parse this as a block mapping with the anchor on the key.
        if props.crossed_line_boundary && !props.is_empty_excluding_flag() {
            let inner_props = NodeProperties {
                anchor: Some((anchor_name, anchor_span)),
                tag: None,
                crossed_line_boundary: false,
            };
            if let Some(mapping) = self.parse_block_mapping_with_props(min_indent, inner_props) {
                return Some(self.apply_properties_and_register(props, mapping));
            }
            // If not a mapping, fall through to add anchor to props
            // This handles the case where we have !!tag\n&anchor value (single scalar)
            if props.anchor.is_some() {
                self.error(ErrorKind::DuplicateAnchor, anchor_span);
            }
        }

        props.anchor = Some((anchor_name, anchor_span));
        self.parse_value_with_properties(min_indent, props)
    }

    /// Handle a tag token in value parsing.
    ///
    /// Expands tag handles to full URIs, checks for missing separators, handles duplicates,
    /// and either:
    /// - Tries to parse as a block mapping key (if crossed line boundary with existing tag)
    /// - Accumulates the tag as a property and continues parsing
    #[allow(
        clippy::needless_pass_by_value,
        reason = "Cow is cloned from token anyway; takes ownership for potential future optimization"
    )]
    fn handle_tag_in_value(
        &mut self,
        tag_name: Cow<'input, str>,
        tag_span: Span,
        min_indent: IndentLevel,
        mut props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        self.advance();

        // Expand tag to full URI form (validates handles and reports errors)
        let expanded_tag: Cow<'input, str> = Cow::Owned(self.expand_tag(&tag_name, tag_span));

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
                tag: Some((expanded_tag.clone(), tag_span)),
                crossed_line_boundary: false,
            };
            if let Some(mapping) = self.parse_block_mapping_with_props(min_indent, inner_props) {
                Some(self.apply_properties_and_register(props, mapping))
            } else if props.tag.is_some() {
                self.error(ErrorKind::DuplicateTag, tag_span);
                props.tag = Some((expanded_tag, tag_span));
                self.parse_value_with_properties(min_indent, props)
            } else {
                props.tag = Some((expanded_tag, tag_span));
                self.parse_value_with_properties(min_indent, props)
            }
        } else {
            props.tag = Some((expanded_tag, tag_span));
            self.parse_value_with_properties(min_indent, props)
        }
    }

    /// Handle a block sequence indicator as a value.
    ///
    /// Note: `parse_block_sequence` emits all events and returns `bool`.
    /// This function returns a placeholder Node for API compatibility during refactoring.
    fn handle_block_seq_indicator(
        &mut self,
        min_indent: IndentLevel,
        props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        let crossed_line = props.crossed_line_boundary;
        if !props.is_empty() && !crossed_line {
            if let Some((_, anchor_span)) = &props.anchor {
                self.error(ErrorKind::ContentOnSameLine, *anchor_span);
            }
            if let Some((_, tag_span)) = &props.tag {
                self.error(ErrorKind::ContentOnSameLine, *tag_span);
            }
        }
        // parse_block_sequence emits all events; returns bool for success
        #[allow(clippy::if_then_some_else_none, reason = "side effects in the block")]
        if self.parse_block_sequence(min_indent, props) {
            // Only check trailing content at true root level - not when we bridged
            // indentation for properties (crossed_line_boundary indicates bridging)
            if min_indent == 0 && !crossed_line {
                self.check_trailing_content_at_root(0);
            }
            // Return placeholder Node - actual structure is in events
            // TODO: Remove this when parse_value returns bool
            Some(Node::null(self.current_span()))
        } else {
            None
        }
    }

    /// Handle an alias token as a value.
    fn handle_alias_in_value(
        &mut self,
        alias_name: &'input str,
        alias_span: Span,
        props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        if !props.is_empty() && !props.crossed_line_boundary {
            self.error(ErrorKind::PropertiesOnAlias, alias_span);
            // parse_alias emits the event and returns the span
            return self.parse_alias().map(Node::null);
        }

        self.advance();
        self.skip_ws();

        if matches!(self.peek(), Some((Token::Colon, _))) {
            self.parse_alias_as_mapping_key(alias_name, alias_span, &props);
            // Return placeholder Node - actual structure is in events
            Some(Node::null(alias_span))
        } else {
            if !self.anchors.contains(alias_name) {
                self.error(ErrorKind::UndefinedAlias, alias_span);
            }
            // Emit Alias event
            self.emit(Event::Alias {
                name: Cow::Borrowed(alias_name),
                span: alias_span,
            });
            Some(Node::null(alias_span))
        }
    }

    /// Handle a line start token as a value.
    fn handle_line_start_in_value(
        &mut self,
        indent: IndentLevel,
        span: Span,
        min_indent: IndentLevel,
        props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        if indent >= min_indent {
            self.advance();
            while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                self.advance();
            }
            self.check_tabs_as_indentation();
            let mut new_props = props;
            new_props.crossed_line_boundary = true;
            self.parse_value_with_properties(min_indent, new_props)
        } else if !props.is_empty() {
            // Check if we can bridge indentation: properties were collected AFTER crossing
            // a line boundary, AND the dedented content is a block collection.
            // This handles cases like SKE5:
            //   seq:
            //    &anchor     <- anchor at indent 1, after crossing line boundary
            //   - a          <- sequence at indent 0 receives the anchor
            //
            // Note: we only bridge when crossed_line_boundary is true. This avoids
            // incorrectly bridging cases like FH7J where the tag is on the same line
            // as a sequence entry and the next line is a sibling entry.
            if props.crossed_line_boundary {
                // Peek ahead to see if there's a block collection at the dedented level
                // Use peek-only lookahead (no position modification) to enable streaming
                let mut peek_offset = 1; // Skip past LineStart at position 0
                while let Some(rt) = self.peek_nth(peek_offset) {
                    if matches!(rt.token, Token::Dedent) {
                        peek_offset += 1;
                    } else {
                        break;
                    }
                }

                // Check if there's a block collection indicator
                if let Some(rt) = self.peek_nth(peek_offset)
                    && matches!(rt.token, Token::BlockSeqIndicator | Token::Colon)
                {
                    // There's a block collection - consume the tokens we peeked and parse it
                    for _ in 0..peek_offset {
                        self.advance();
                    }
                    let mut new_props = props;
                    new_props.crossed_line_boundary = true;
                    return self.parse_value_with_properties(indent, new_props);
                }
                // No block collection - don't consume tokens, return null with properties
            }
            // Emit empty scalar event BEFORE applying properties (so props go to this scalar, not a previous event)
            self.emit(Event::Scalar {
                style: ScalarStyle::Plain,
                value: Cow::Borrowed(""),
                anchor: props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
                tag: props.tag.clone(),
                span,
            });
            // Register anchor if present
            let null_node = Node::null(span);
            let node_with_props = props.clone().apply_to(null_node);
            if let Some((name, _)) = &props.anchor {
                self.anchors.insert(name);
            }
            Some(node_with_props)
        } else {
            None
        }
    }

    /// Parse a value with already-collected node properties.
    pub fn parse_value_with_properties(
        &mut self,
        min_indent: IndentLevel,
        props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        self.skip_ws();

        // If at EOF and we have properties (anchor/tag), return a null node with those properties.
        // This handles cases like `!` alone where the tag applies to an implicit null (UKK6-02).
        let Some((tok, span)) = self.peek() else {
            return if props.is_empty() {
                None
            } else {
                // Emit scalar event for the null with properties
                let null_span = self.current_span();
                self.emit(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor: props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
                    tag: props.tag.clone(),
                    span: null_span,
                });
                let null_node = Node::null(null_span);
                let node_with_props = props.clone().apply_to(null_node);
                if let Some((name, _)) = &props.anchor {
                    self.anchors.insert(name);
                }
                Some(node_with_props)
            };
        };

        // Clone token discriminant and span to avoid borrow issues
        let is_flow_map_start = matches!(tok, Token::FlowMapStart);
        let tok_span = span;

        match tok {
            // Flow mapping or sequence - check if used as mapping key first
            Token::FlowMapStart | Token::FlowSeqStart => {
                let start_pos = tok_span.start_usize();
                let is_mapping_key = self.flow_collection_is_mapping_key();

                if is_mapping_key {
                    // Emit MappingStart BEFORE parsing the flow collection
                    self.emit(Event::MappingStart {
                        style: CollectionStyle::Block,
                        anchor: None,
                        tag: None,
                        span: tok_span,
                    });
                }

                // Parse the flow collection (events emitted internally)
                let flow_parsed = if is_flow_map_start {
                    self.parse_flow_mapping()
                } else {
                    self.parse_flow_sequence()
                };

                if !flow_parsed {
                    return None;
                }

                // Get end position from last emitted event
                let flow_end = self.last_event_end_position();

                self.check_content_after_flow(flow_end);
                self.check_multiline_implicit_key(start_pos, flow_end);

                // Apply properties to the flow collection events
                self.apply_properties_to_events(&props);
                if let Some((name, _)) = &props.anchor {
                    self.anchors.insert(name);
                }

                if is_mapping_key {
                    // We already emitted MappingStart, now parse the value
                    self.skip_ws();
                    if matches!(self.peek(), Some((Token::Colon, _))) {
                        self.advance(); // consume ':'
                        self.skip_ws();
                    }

                    let map_indent = self.current_indent();
                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    // Emit null if no value
                    if value.is_none() {
                        self.emit_null_scalar();
                    }

                    let end = self.last_event_end_position();
                    self.emit(Event::MappingEnd {
                        span: Span::from_usize_range(end..end),
                    });
                    // Return placeholder Node
                    Some(Node::null(Span::from_usize_range(start_pos..end)))
                } else {
                    // Not used as mapping key
                    if min_indent == 0 {
                        self.check_trailing_content_at_root(0);
                    }
                    // Return placeholder Node
                    Some(Node::null(Span::from_usize_range(start_pos..flow_end)))
                }
            }
            // Block sequence
            Token::BlockSeqIndicator => self.handle_block_seq_indicator(min_indent, props),
            // Anchor - collect as property and continue parsing
            Token::Anchor(name) => {
                let anchor_name = *name;
                self.handle_anchor_in_value(anchor_name, span, min_indent, props)
            }
            // Alias
            Token::Alias(name) => {
                let alias_name = *name;
                self.handle_alias_in_value(alias_name, span, props)
            }
            // Tag - collect as property and continue parsing
            Token::Tag(tag) => {
                let tag_name = tag.clone();
                self.handle_tag_in_value(tag_name, span, min_indent, props)
            }
            // Block scalars - parse_*_block_scalar emits Scalar events
            Token::LiteralBlockHeader(_) => {
                self.parse_literal_block_scalar(min_indent)
                    .map(|(scalar_span, _content)| {
                        self.apply_properties_to_events(&props);
                        if let Some((name, _)) = &props.anchor {
                            self.anchors.insert(name);
                        }
                        Node::null(scalar_span)
                    })
            }
            Token::FoldedBlockHeader(_) => {
                self.parse_folded_block_scalar(min_indent)
                    .map(|(scalar_span, _content)| {
                        self.apply_properties_to_events(&props);
                        if let Some((name, _)) = &props.anchor {
                            self.anchors.insert(name);
                        }
                        Node::null(scalar_span)
                    })
            }
            // Scalars
            Token::Plain(_) | Token::StringStart(_) => {
                // Check if scalar is at sufficient indentation level.
                // After crossing a line boundary, the scalar must be at min_indent or more.
                let token_col = self.current_token_column();
                if token_col < min_indent {
                    // Scalar is at lower indentation - return null with any properties
                    return if props.is_empty() {
                        None
                    } else {
                        // Emit scalar event for the null with properties
                        self.emit(Event::Scalar {
                            style: ScalarStyle::Plain,
                            value: Cow::Borrowed(""),
                            anchor: props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
                            tag: props.tag.clone(),
                            span,
                        });
                        if let Some((name, _)) = &props.anchor {
                            self.anchors.insert(name);
                        }
                        Some(Node::null(span))
                    };
                }

                // If we have properties (anchor/tag) and see a mapping key pattern,
                // there are two cases:
                // 1. Props are on a SEPARATE LINE before the mapping (crossed_line_boundary=true)
                //    → Props apply to the mapping itself
                // 2. Props are on the SAME LINE as the first key (crossed_line_boundary=false)
                //    → Props apply to the first key
                if !props.is_empty()
                    && !props.crossed_line_boundary
                    && self.is_mapping_key_pattern()
                {
                    // Props on same line as content, and it's a mapping - props apply to first key
                    self.parse_block_mapping_with_props(min_indent, props)
                } else {
                    // Props on separate line, or no props, or not a mapping - apply to resulting node
                    self.parse_scalar_or_mapping(min_indent)
                        .map(|n| self.apply_properties_and_register(props, n))
                }
            }
            // Document markers
            Token::DocStart | Token::DocEnd => None,
            // Line start
            Token::LineStart(n) => self.handle_line_start_in_value(*n, span, min_indent, props),
            // Skip comments
            Token::Comment(_) | Token::Indent(_) => {
                self.advance();
                self.parse_value_with_properties(min_indent, props)
            }
            // Mapping key indicator
            Token::MappingKey => {
                // parse_block_mapping emits all events; returns ()
                self.parse_block_mapping(min_indent, props);
                // Return placeholder Node - actual structure is in events
                Some(Node::null(self.current_span()))
            }
            // Empty key
            Token::Colon => {
                if props.is_empty() {
                    // parse_block_mapping_with_empty_key emits events; returns ()
                    self.parse_block_mapping_with_empty_key(min_indent);
                } else {
                    // parse_block_mapping_with_tagged_null_key emits events; returns bool
                    if !self.parse_block_mapping_with_tagged_null_key(min_indent, props) {
                        return None;
                    }
                }
                // Return placeholder Node - actual structure is in events
                Some(Node::null(self.current_span()))
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
                    // Emit scalar event for the null with properties
                    self.emit(Event::Scalar {
                        style: ScalarStyle::Plain,
                        value: Cow::Borrowed(""),
                        anchor: props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
                        tag: props.tag.clone(),
                        span,
                    });
                    let null_node = Node::null(span);
                    let node_with_props = props.clone().apply_to(null_node);
                    if let Some((name, _)) = &props.anchor {
                        self.anchors.insert(name);
                    }
                    Some(node_with_props)
                }
            }
            // Invalid or unexpected
            _ => {
                self.error(ErrorKind::InvalidValue, span);
                self.advance();
                // Emit scalar event for the invalid node with properties
                self.emit(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor: props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
                    tag: props.tag.clone(),
                    span,
                });
                let invalid_node = Node::invalid(span);
                let node_with_props = props.clone().apply_to(invalid_node);
                if let Some((name, _)) = &props.anchor {
                    self.anchors.insert(name);
                }
                Some(node_with_props)
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

/// Parse a token stream from the unified lexer into events.
///
/// This is the entry point for the unified streaming architecture where
/// `DocumentLexer` handles the entire input (including directives and
/// document markers) in a single pass.
///
/// Returns: `(Vec<Event>, Vec<ParseError>)`
///
/// The event stream follows YAML Test Suite format:
/// `StreamStart`, (`DocumentStart`, content, `DocumentEnd`)*, `StreamEnd`
pub fn parse_stream_from_tokens<'tokens: 'input, 'input>(
    tokens: &'tokens [RichToken<'input>],
    input: &'input str,
) -> (Vec<Event<'input>>, Vec<ParseError>) {
    let mut parser = Parser::new(tokens, input);

    // Emit StreamStart
    parser.emit(Event::StreamStart);

    // Skip initial whitespace/newlines
    parser.skip_ws_and_newlines();

    // Parse documents until EOF
    loop {
        if parser.is_eof() {
            break;
        }

        // Skip any orphan DocEnd markers that don't belong to a document.
        // A `...` at the start of a stream (or after another `...`) doesn't create a document.
        while matches!(parser.peek(), Some((Token::DocEnd, _))) {
            parser.advance();
            parser.skip_ws_and_newlines();
        }

        // After skipping DocEnd markers, check if we've reached EOF
        if parser.is_eof() {
            break;
        }

        // Populate tag handles for this document (clears old handles and reads new ones from current position)
        // This ensures each document has its own tag scope
        parser.populate_tag_handles_from_tokens();

        // Track directives for "directive without document" error
        let mut has_directive_in_prologue = false;
        let mut first_directive_span = Span::from_usize_range(0..0);

        // Skip directive tokens (they've been processed by populate_tag_handles_from_tokens)
        while let Some((tok, span)) = parser.peek() {
            match tok {
                Token::YamlDirective(_) | Token::TagDirective(_) | Token::ReservedDirective(_) => {
                    if !has_directive_in_prologue {
                        first_directive_span = span;
                    }
                    has_directive_in_prologue = true;
                    parser.advance();
                    parser.skip_ws_and_newlines();
                }
                _ => break,
            }
        }

        // Check for "directive without document" error:
        // If we saw directives but now hit EOF or `...` (doc end), that's an error
        if has_directive_in_prologue {
            let at_doc_end_or_eof =
                parser.is_eof() || matches!(parser.peek(), Some((Token::DocEnd, _)));
            if at_doc_end_or_eof {
                parser.error(ErrorKind::TrailingContent, first_directive_span);
                // Continue to next iteration to skip the DocEnd or exit
                continue;
            }
        }

        // Parse one document; returns true if it ended with explicit `...`
        let _ended_with_doc_end = parser.parse_document_events();

        // After document, skip whitespace before next iteration
        parser.skip_ws_and_newlines();
    }

    // Emit StreamEnd
    parser.emit(Event::StreamEnd);

    (parser.take_events(), parser.errors)
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
/// Emits: `StreamStart`, `DocumentStart`, document content, `DocumentEnd`, `StreamEnd`
/// Parse a single YAML document and return events.
///
/// This is the core parsing function that produces an event stream.
/// Use `EventParser` to reconstruct the AST from the events.
///
/// Emits: `StreamStart`, `DocumentStart`, document content, `DocumentEnd`, `StreamEnd`
pub fn parse_single_document<'tokens: 'input, 'input>(
    tokens: &'tokens [RichToken<'input>],
    input: &'input str,
) -> (Vec<Event<'input>>, Vec<ParseError>) {
    let mut parser = Parser::new(tokens, input);

    // Emit StreamStart
    parser.emit(Event::StreamStart);

    // Populate tag handles from TagDirective tokens in the token stream
    parser.populate_tag_handles_from_tokens();

    parser.skip_ws_and_newlines();

    // Check for explicit document start marker `---`
    let has_doc_start = matches!(parser.peek(), Some((Token::DocStart, _)));
    let doc_start_span = parser.current_span();
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

    // Emit DocumentStart
    parser.emit(Event::DocumentStart {
        explicit: has_doc_start,
        span: doc_start_span,
    });

    if parser.is_eof() || matches!(parser.peek(), Some((Token::DocEnd, _))) {
        // If we had an explicit document start (---), emit empty scalar
        if has_doc_start {
            let null_span = Span::from_usize_range(0..0);
            parser.emit(Event::Scalar {
                style: ScalarStyle::Plain,
                value: Cow::Borrowed(""),
                anchor: None,
                tag: None,
                span: null_span,
            });
        }
    } else {
        // Parse document content (events are emitted during parsing)
        let _ = parser.parse_value(0);
    }

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
    let has_doc_end = matches!(parser.peek(), Some((Token::DocEnd, _)));
    let doc_end_span = parser.current_span();
    if has_doc_end {
        parser.advance(); // consume DocEnd
        parser.skip_ws_and_newlines();
    }

    // Emit DocumentEnd
    parser.emit(Event::DocumentEnd {
        explicit: has_doc_end,
        span: doc_end_span,
    });

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

    // Emit StreamEnd
    parser.emit(Event::StreamEnd);

    (parser.take_events(), parser.errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Use event-based parsing for tests - this correctly reconstructs AST from events
    /// as we remove Node construction from the parser methods.
    fn parse(input: &str) -> (Stream<'static>, Vec<ParseError>) {
        crate::parse_via_events(input)
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
        assert!(errors.is_empty(), "errors: {errors:?}");
        assert_eq!(docs.len(), 1);
        let value = &docs.first().unwrap().value;
        assert!(matches!(&docs.first().unwrap().value, Value::Mapping(_)));
        if let Value::Mapping(pairs) = value {
            assert_eq!(pairs.len(), 2, "expected 2 pairs but got {pairs:?}");
            // Check first pair's value has anchor
            let first_value = &pairs.first().unwrap().1;
            assert_eq!(
                first_value.anchor(),
                Some("anchor"),
                "First value should have anchor 'anchor'"
            );
            let alias_value = &pairs.last().unwrap().1.value;
            assert!(
                matches!(alias_value, Value::Alias(name) if name.as_ref() == "anchor"),
                "Expected Alias(\"anchor\"), got {alias_value:?}"
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
