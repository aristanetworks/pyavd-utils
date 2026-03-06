// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Block structure parsing (block sequences and mappings).

use std::borrow::Cow;

use crate::error::ErrorKind;
use crate::event::{CollectionStyle, Event, ScalarStyle};
use crate::lexer::Token;
use crate::span::{IndentLevel, Span};
use crate::value::Node;

use super::{NodeProperties, Parser};

impl<'tokens: 'input, 'input> Parser<'tokens, 'input> {
    /// Parse a block sequence: - item\n- item
    ///
    /// Emits: `SequenceStart` at entry, items recursively, `SequenceEnd` at exit.
    /// Returns `true` if parsing succeeded, `false` if no sequence was found.
    pub fn parse_block_sequence(
        &mut self,
        _min_indent: IndentLevel,
        props: NodeProperties<'input>,
    ) -> bool {
        let Some((_, start_span)) = self.peek() else {
            return false;
        };
        let start = start_span.start_usize();
        let seq_indent = self.current_token_column();

        // Emit SequenceStart event with properties
        self.emit(Event::SequenceStart {
            style: CollectionStyle::Block,
            anchor: props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
            tag: props.tag.clone(),
            span: start_span,
        });

        // Register anchor if present
        if let Some((name, _)) = &props.anchor {
            self.anchors.insert(name);
        }

        self.push_indent(seq_indent);

        while let Some((Token::BlockSeqIndicator, _)) = self.peek() {
            let item_col = self.current_token_column();
            if item_col < seq_indent {
                break;
            }
            if item_col > seq_indent {
                break;
            }

            self.advance(); // consume '-'
            self.check_tabs_after_seq_indicator();
            self.skip_ws();

            let parsed_item = if let Some((Token::LineStart(n), _)) = self.peek() {
                let line_indent = *n;
                // If the next line is at or below sequence indent, this entry is empty
                if line_indent <= seq_indent {
                    false
                } else {
                    self.advance();
                    if let Some((Token::Indent(_), _)) = self.peek() {
                        self.advance();
                    }
                    self.parse_value(line_indent).is_some()
                }
            } else {
                self.parse_value(seq_indent + 1).is_some()
            };

            if !parsed_item {
                // Emit empty scalar for null item
                let null_span = self.current_span();
                self.emit(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor: None,
                    tag: None,
                    span: null_span,
                });
            }

            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            loop {
                match self.peek() {
                    Some((Token::LineStart(n), span)) => {
                        let n = *n;
                        let span = span;
                        if n < seq_indent {
                            // Check for orphan indentation: n is not in the parser's indent stack
                            // and is between valid levels (e.g., indent 2 when stack is [0, 3])
                            if !self.is_valid_indent(n) {
                                self.error(ErrorKind::InvalidIndentation, span);
                            }
                            // Use last event's end position for span calculation
                            let end = self.last_event_end_position().max(start);
                            self.pop_indent();
                            // Emit SequenceEnd event
                            self.emit(Event::SequenceEnd {
                                span: Span::from_usize_range(end..end),
                            });
                            return true;
                        }
                        self.advance();
                    }
                    Some((
                        Token::Whitespace
                        | Token::WhitespaceWithTabs
                        | Token::Comment(_)
                        | Token::Indent(_)
                        | Token::Dedent,
                        _,
                    )) => {
                        self.advance();
                    }
                    _ => {
                        break;
                    }
                }
            }
        }

        // Check if we stopped because of unexpected content at seq_indent level
        // e.g., a plain scalar where we expected '-' or end of sequence.
        // But NOT if the content is a mapping key (followed by colon), which is valid
        // for a sibling mapping entry at the parent level.
        if let Some((tok, span)) = self.peek() {
            let col = self.current_token_column();
            // Only check at seq_indent level, not at lower indents (those are valid dedents)
            if col == seq_indent {
                let is_unexpected_content = match tok {
                    Token::Plain(_) => {
                        // Check if this might be a mapping key (followed by colon)
                        // If so, it's a valid sibling mapping entry
                        let next = self.tokens.get(self.pos + 1);
                        !matches!(next, Some(rt) if matches!(rt.token, Token::Colon))
                    }
                    Token::StringStart(_) => {
                        // Quoted strings could be mapping keys - skip this check
                        // since they'd need more complex lookahead to find StringEnd + Colon
                        false
                    }
                    // Anchors, aliases, and tags alone at seq_indent are unexpected
                    Token::Anchor(_) | Token::Alias(_) | Token::Tag(_) => {
                        // But if followed by content that makes a mapping entry, allow it
                        false
                    }
                    _ => false,
                };
                if is_unexpected_content {
                    self.error(ErrorKind::TrailingContent, span);
                }
            }
        }

        // Use last event's end position for span calculation
        let end = self.last_event_end_position().max(start);
        self.pop_indent();
        // Emit SequenceEnd event
        self.emit(Event::SequenceEnd {
            span: Span::from_usize_range(end..end),
        });

        true
    }

    /// Parse a mapping key (explicit with `?` or implicit).
    /// Returns the key node and whether an explicit key indicator was found.
    /// Parse a mapping key. Emits events for the key.
    /// Returns `true` if a key was parsed (including null keys for explicit/empty cases).
    fn parse_mapping_key(
        &mut self,
        map_indent: IndentLevel,
        key_props: &NodeProperties<'input>,
        explicit_key: bool,
        empty_key: bool,
    ) -> bool {
        // Helper to apply properties and register anchor
        let apply_props = |parser: &mut Self, props: &NodeProperties<'input>| {
            if !props.is_empty() {
                parser.apply_properties_to_events(props);
                if let Some((name, _)) = &props.anchor {
                    parser.anchors.insert(name);
                }
            }
        };

        // Track if we parsed a real value (vs null key)
        let key_parsed = if explicit_key {
            match self.peek() {
                Some((Token::MappingKey, _)) => false,
                // Colon after ? - this could be an empty key mapping (like ? : x)
                // or the value indicator (like ?:). Check if there's content after colon.
                Some((Token::Colon, _)) => {
                    // Peek ahead to see if there's content after colon (making it a mapping key)
                    let saved_pos = self.pos;
                    self.advance(); // consume :
                    self.skip_ws();
                    let has_content = matches!(
                        self.peek(),
                        Some((
                            Token::Plain(_)
                                | Token::StringStart(_)
                                | Token::Anchor(_)
                                | Token::Tag(_),
                            _
                        ))
                    );
                    self.pos = saved_pos; // restore position

                    if has_content {
                        // : followed by content = nested mapping as key
                        if self.parse_value(map_indent + 1).is_some() {
                            apply_props(self, key_props);
                            true
                        } else {
                            false
                        }
                    } else {
                        // : with no content = empty key, treat as value indicator
                        false
                    }
                }
                Some((Token::LineStart(_), _)) => {
                    self.advance();
                    if let Some((Token::Indent(_), _)) = self.peek() {
                        self.advance();
                    }
                    // If the next token is Colon, that's the value indicator.
                    // The key is null (possibly with properties collected earlier).
                    if matches!(self.peek(), Some((Token::Colon, _))) {
                        false
                    } else if self.parse_value(map_indent + 1).is_some() {
                        apply_props(self, key_props);
                        true
                    } else {
                        false
                    }
                }
                Some((Token::Indent(_), _)) => {
                    self.advance();
                    if self.parse_value(map_indent + 1).is_some() {
                        apply_props(self, key_props);
                        true
                    } else {
                        false
                    }
                }
                _ => {
                    if self.parse_value(map_indent + 1).is_some() {
                        apply_props(self, key_props);
                        true
                    } else {
                        false
                    }
                }
            }
        } else if empty_key {
            false
        } else {
            // Check if the key is an alias with properties (invalid)
            if let Some((Token::Alias(name), span)) = self.peek() {
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
                true // Alias is a valid key (properties error already reported)
            } else {
                // parse_scalar emits events
                if self.parse_scalar().is_some() {
                    apply_props(self, key_props);
                    true
                } else {
                    false
                }
            }
        };

        if key_parsed {
            true
        } else if explicit_key || empty_key {
            // For explicit keys with properties (e.g., "? &d"), emit null with properties
            let null_span = self.current_span();
            // Emit empty scalar event for the null key with properties
            self.emit(Event::Scalar {
                style: ScalarStyle::Plain,
                value: Cow::Borrowed(""),
                anchor: key_props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
                tag: key_props.tag.clone(),
                span: null_span,
            });
            if let Some((name, _)) = &key_props.anchor {
                self.anchors.insert(name);
            }
            true
        } else {
            false
        }
    }

    /// Look ahead after explicit key to find colon on next line.
    /// Returns true if a colon was found, restores position if not.
    /// Skips over comment-only lines between the key and the value indicator.
    fn find_colon_after_explicit_key(&mut self, map_indent: IndentLevel) -> bool {
        let saved_pos = self.pos;

        loop {
            match self.peek() {
                Some((Token::LineStart(n), _)) if *n >= map_indent => {
                    self.advance();
                }
                Some((Token::Indent(_) | Token::Dedent, _)) => {
                    self.advance();
                }
                Some((Token::Comment(_), _)) => {
                    // Skip comment and continue looking
                    self.advance();
                }
                Some((Token::Colon, _)) => {
                    return true;
                }
                _ => {
                    // Not a colon - restore position
                    self.pos = saved_pos;
                    return false;
                }
            }
        }
    }

    /// Skip tokens until next mapping entry or end of mapping.
    /// Returns `true` to continue parsing, `false` if the mapping should end.
    /// When returning `false`, the caller is responsible for calling `pop_indent()`.
    fn skip_to_next_mapping_entry(&mut self, map_indent: IndentLevel) -> bool {
        // Handle immediate Dedent at mapping level
        if let Some((Token::Dedent, _)) = self.peek() {
            self.advance();
            return false; // End mapping
        }

        while let Some((tok, span)) = self.peek() {
            match tok {
                Token::LineStart(n) => {
                    let n = *n;
                    let span = span;
                    if n < map_indent {
                        // Check for orphan indentation: n is not in the parser's indent stack
                        // and is between valid levels (e.g., indent 1 when stack is [0, 2])
                        if !self.is_valid_indent(n) {
                            self.error(ErrorKind::InvalidIndentation, span);
                        }
                        return false; // End mapping
                    }
                    if n == map_indent {
                        self.advance();
                        // Skip Dedent tokens that follow (from nested content)
                        while let Some((Token::Dedent, _)) = self.peek() {
                            self.advance();
                        }
                        // Check if there's a mapping entry token now.
                        // If not, continue the loop to handle blank lines.
                        if self.is_mapping_entry_token() {
                            return true; // Continue
                        }
                        // Otherwise continue - might be a blank line
                    } else {
                        // n > map_indent: this is an indented comment or blank line
                        // Skip the entire line (LineStart, Dedent, Comment, etc.) until next LineStart
                        self.advance();
                        self.skip_tokens_until_linestart();
                    }
                }
                Token::Indent(_) => {
                    self.advance();
                }
                Token::Dedent => {
                    // Only end mapping if we're at or below map level
                    // Otherwise, this Dedent is from nested content
                    self.advance();
                    // Continue looking for the next LineStart
                }
                _ => return true, // Continue
            }
        }
        true // Continue (or EOF)
    }

    /// Skip tokens until the next `LineStart` token (or EOF).
    fn skip_tokens_until_linestart(&mut self) {
        while let Some((tok, _)) = self.peek() {
            if matches!(tok, Token::LineStart(_)) {
                break;
            }
            self.advance();
        }
    }

    /// Check if current token can start a mapping entry.
    fn is_mapping_entry_token(&self) -> bool {
        matches!(
            self.peek(),
            Some((
                Token::Plain(_)
                    | Token::StringStart(_)
                    | Token::MappingKey
                    | Token::Colon
                    | Token::LiteralBlockHeader(_)
                    | Token::FoldedBlockHeader(_)
                    | Token::Anchor(_)
                    | Token::Alias(_)
                    | Token::Tag(_),
                _
            ))
        )
    }

    /// Parse a block mapping with explicit key indicator or implicit keys.
    ///
    /// Emits: `MappingStart` at entry, key/value pairs recursively, `MappingEnd` at exit.
    /// The AST is reconstructed by `EventParser` from the events.
    pub fn parse_block_mapping(&mut self, _min_indent: IndentLevel, props: NodeProperties<'input>) {
        let start_span = self.current_span();
        let start = start_span.start_usize();
        let map_indent = self.current_token_column();

        // Emit MappingStart event with properties
        self.emit(Event::MappingStart {
            style: CollectionStyle::Block,
            anchor: props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
            tag: props.tag.clone(),
            span: start_span,
        });

        // Register anchor if present
        if let Some((name, _)) = &props.anchor {
            self.anchors.insert(name);
        }

        self.push_indent(map_indent);

        let mut first_entry = true;

        while !self.is_eof() {
            let loop_start_pos = self.pos;

            if !first_entry && self.current_indent() < map_indent {
                break;
            }

            // Handle explicit key indicator `?`
            let explicit_key = matches!(self.peek(), Some((Token::MappingKey, _)));
            if explicit_key {
                self.advance();
                self.check_tabs_after_block_indicator();
                self.skip_ws();
            }

            let empty_key = !explicit_key && matches!(self.peek(), Some((Token::Colon, _)));

            // Collect properties and parse key
            let key_props = self.collect_node_properties(NodeProperties::default());
            // parse_mapping_key emits events; we only care if it succeeded
            if !self.parse_mapping_key(map_indent, &key_props, explicit_key, empty_key) {
                break;
            }

            self.skip_ws();

            // Check for colon (value indicator)
            let mut has_value = matches!(self.peek(), Some((Token::Colon, _)));
            if !has_value && explicit_key {
                has_value = self.find_colon_after_explicit_key(map_indent);
            }

            // Consume colon and parse value
            if has_value && matches!(self.peek(), Some((Token::Colon, _))) {
                self.advance();
                self.check_tabs_after_block_indicator();
                self.skip_ws();
            }

            if has_value {
                // parse_mapping_value emits events; ignore returned Node
                let _ = self.parse_mapping_value(map_indent);
            } else {
                // Emit empty scalar for null value
                let null_span = self.current_span();
                self.emit(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor: None,
                    tag: None,
                    span: null_span,
                });
            }

            first_entry = false;

            // Skip trailing whitespace and comments
            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            // Skip to next entry or end mapping
            if !self.skip_to_next_mapping_entry(map_indent) {
                // Mapping ended
                self.pop_indent();
                // Use last event's end position for span calculation
                let end = self.last_event_end_position().max(start);
                self.emit(Event::MappingEnd {
                    span: Span::from_usize_range(end..end),
                });
                return;
            }

            // Check if current token can start a new entry
            if !self.is_mapping_entry_token() {
                break;
            }

            // Infinite loop guard
            if self.pos == loop_start_pos && !self.is_eof() {
                self.advance();
                break;
            }
        }

        self.pop_indent();
        // Use last event's end position for span calculation
        let end = self.last_event_end_position().max(start);
        self.emit(Event::MappingEnd {
            span: Span::from_usize_range(end..end),
        });
    }

    /// Parse a block mapping where the first key already has properties (anchor/tag).
    ///
    /// Emits: `MappingStart` at entry (with properties), key/value pairs recursively, `MappingEnd` at exit.
    ///
    /// This function uses a lookahead approach: it first checks (without emitting events) whether
    /// there's actually a mapping here (key followed by colon). Only if confirmed does it emit
    /// `MappingStart` and parse the key with event emission.
    #[allow(clippy::too_many_lines, reason = "Complex mapping logic")]
    pub fn parse_block_mapping_with_props(
        &mut self,
        _min_indent: IndentLevel,
        first_key_props: NodeProperties<'input>,
    ) -> Option<Node<'input>> {
        let start_span = self.current_span();
        let start = start_span.start_usize();
        let map_indent = self.current_indent();

        // PHASE 1: Lookahead to confirm this is actually a mapping (key + colon).
        // We do NOT emit any events in this phase - if it's not a mapping, we return None
        // without having emitted MappingStart.
        // Important: Save/restore errors too to avoid spurious errors during speculative parse.
        {
            let saved_pos = self.pos;
            let saved_events_len = self.events.len();
            let saved_errors_len = self.errors.len();

            // Collect any additional properties (without permanently consuming them)
            let _key_props = self.collect_node_properties(first_key_props.clone());

            // Check if there's a key token
            let has_key = matches!(
                self.peek(),
                Some((
                    Token::Plain(_)
                        | Token::StringStart(_)
                        | Token::FlowSeqStart
                        | Token::FlowMapStart,
                    _
                ))
            );
            if !has_key {
                self.pos = saved_pos;
                self.events.truncate(saved_events_len);
                self.errors.truncate(saved_errors_len);
                return None;
            }

            // Parse the potential key (speculatively)
            let key_parsed = match self.peek() {
                Some((Token::Plain(_) | Token::StringStart(_), _)) => self.parse_scalar().is_some(),
                Some((Token::FlowSeqStart | Token::FlowMapStart, _)) => {
                    self.parse_flow_value().is_some()
                }
                _ => false,
            };
            if !key_parsed {
                self.pos = saved_pos;
                self.events.truncate(saved_events_len);
                self.errors.truncate(saved_errors_len);
                return None;
            }

            self.skip_ws();

            // Check for colon
            let has_colon = matches!(self.peek(), Some((Token::Colon, _)));
            if !has_colon {
                self.pos = saved_pos;
                self.events.truncate(saved_events_len);
                self.errors.truncate(saved_errors_len);
                return None;
            }

            // Confirmed! Restore position and proceed with actual parsing
            self.pos = saved_pos;
            self.events.truncate(saved_events_len);
            self.errors.truncate(saved_errors_len);
        }

        // PHASE 2: Actually parse the mapping, now that we've confirmed it's valid.

        // Collect any additional properties after the initial ones
        let key_props = self.collect_node_properties(first_key_props);

        // Emit MappingStart event - note: properties are on the KEY, not the mapping
        // The mapping itself doesn't have anchor/tag in this case.
        self.emit(Event::MappingStart {
            style: CollectionStyle::Block,
            anchor: None,
            tag: None,
            span: start_span,
        });

        // Parse first key (events emitted by parse_scalar/parse_flow_value)
        let first_key_parsed = match self.peek() {
            Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                if self.parse_scalar().is_some() {
                    self.apply_properties_to_events(&key_props);
                    if let Some((name, _)) = &key_props.anchor {
                        self.anchors.insert(name);
                    }
                    true
                } else {
                    false
                }
            }
            Some((Token::FlowSeqStart | Token::FlowMapStart, _)) => {
                if self.parse_flow_value().is_some() {
                    self.apply_properties_to_events(&key_props);
                    if let Some((name, _)) = &key_props.anchor {
                        self.anchors.insert(name);
                    }
                    true
                } else {
                    false
                }
            }
            _ => false,
        };
        if !first_key_parsed {
            return None;
        }

        self.skip_ws();

        // We already confirmed colon exists in phase 1
        debug_assert!(matches!(self.peek(), Some((Token::Colon, _))));
        self.advance(); // consume ':'
        self.skip_ws();

        let first_value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        // If no value, emit null scalar
        if first_value.is_none() {
            self.emit_null_scalar();
        }

        self.skip_ws();
        if let Some((Token::Comment(_), _)) = self.peek() {
            self.advance();
        }

        // Skip any Dedent tokens that might appear after descending from nested values
        while let Some((Token::Dedent, _)) = self.peek() {
            self.advance();
        }

        while let Some((Token::LineStart(n), _)) = self.peek() {
            if *n < map_indent {
                break;
            }
            if *n == map_indent {
                self.advance();

                // Skip any Dedent tokens that appear after LineStart
                while let Some((Token::Dedent, _)) = self.peek() {
                    self.advance();
                }

                // Handle explicit key indicator (?) - this is a different flow
                if let Some((Token::MappingKey, _)) = self.peek() {
                    self.advance();
                    self.skip_ws();
                    // Parse the key after ? (events emitted)
                    if self.parse_value(map_indent + 1).is_none() {
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
                    if let Some((Token::Colon, _)) = self.peek() {
                        self.advance();
                        self.skip_ws();
                        let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                            self.advance();
                            self.parse_value(map_indent + 1)
                        } else {
                            self.parse_value(map_indent + 1)
                        };
                        if value.is_none() {
                            self.emit_null_scalar();
                        }
                    }
                    continue;
                }

                // Parse key: scalar, alias, or anchored/tagged value
                let key_parsed = match self.peek() {
                    Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                        // parse_scalar emits events
                        self.parse_scalar().is_some()
                    }
                    Some((Token::Alias(name), span)) => {
                        let alias_name = *name;
                        let alias_span = span;
                        if !self.anchors.contains(alias_name) {
                            self.error(ErrorKind::UndefinedAlias, alias_span);
                        }
                        self.advance();
                        // Emit Alias event for the key
                        self.emit(Event::Alias {
                            name: Cow::Borrowed(alias_name),
                            span: alias_span,
                        });
                        true
                    }
                    Some((Token::Anchor(name), span)) => {
                        let anchor_name = *name;
                        let anchor_span = span;
                        self.advance();
                        self.skip_ws();
                        // parse_scalar emits events
                        if self.parse_scalar().is_some() {
                            let props = NodeProperties {
                                anchor: Some((anchor_name, anchor_span)),
                                tag: None,
                                crossed_line_boundary: false,
                            };
                            self.apply_properties_to_events(&props);
                            self.anchors.insert(anchor_name);
                            true
                        } else {
                            false
                        }
                    }
                    Some((Token::Tag(tag), span)) => {
                        let tag_name = tag.clone();
                        let tag_span = span;
                        self.advance();
                        self.skip_ws();
                        let expanded = self.expand_tag(&tag_name, tag_span);
                        // parse_scalar emits events
                        if self.parse_scalar().is_some() {
                            let props = NodeProperties {
                                anchor: None,
                                tag: Some((Cow::Owned(expanded), tag_span)),
                                crossed_line_boundary: false,
                            };
                            self.apply_properties_to_events(&props);
                            true
                        } else {
                            false
                        }
                    }
                    _ => false,
                };

                if key_parsed {
                    self.skip_ws();
                    if matches!(self.peek(), Some((Token::Colon, _))) {
                        self.advance();
                        self.skip_ws();
                        let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                            self.advance();
                            self.parse_value(map_indent + 1)
                        } else {
                            self.parse_value(map_indent + 1)
                        };
                        if value.is_none() {
                            self.emit_null_scalar();
                        }
                    }
                }
            } else {
                self.advance();
            }
        }

        // Use last event's end position for span calculation
        let end = self.last_event_end_position().max(start);
        self.emit(Event::MappingEnd {
            span: Span::from_usize_range(end..end),
        });
        // Return placeholder Node - actual structure is in events
        Some(Node::null(Span::from_usize_range(start..end)))
    }

    /// Parse a block mapping where the first key is a null with properties.
    ///
    /// Emits: `MappingStart` at entry, key/value pairs recursively, `MappingEnd` at exit.
    /// Returns `true` if a mapping was parsed, `false` if no mapping found.
    /// The AST is reconstructed by `EventParser` from the events.
    pub fn parse_block_mapping_with_tagged_null_key(
        &mut self,
        _min_indent: IndentLevel,
        key_props: NodeProperties<'input>,
    ) -> bool {
        let start_span = self.current_span();
        let start = start_span.start_usize();
        let map_indent = self.current_indent();

        if !matches!(self.peek(), Some((Token::Colon, _))) {
            return false;
        }

        // Now we know it's a mapping, emit MappingStart
        self.emit(Event::MappingStart {
            style: CollectionStyle::Block,
            anchor: None,
            tag: None,
            span: start_span,
        });

        // Emit scalar event for the null key with properties
        let key_span = self.current_span();
        self.emit(Event::Scalar {
            style: ScalarStyle::Plain,
            value: Cow::Borrowed(""),
            anchor: key_props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
            tag: key_props.tag.clone(),
            span: key_span,
        });
        if let Some((name, _)) = &key_props.anchor {
            self.anchors.insert(name);
        }

        self.advance();
        self.skip_ws();

        // parse_mapping_value emits events; ignore returned Node
        let _ = self.parse_mapping_value(map_indent);

        // Parse remaining entries
        loop {
            self.skip_ws();

            // Check for line continuation at same indent
            let Some((Token::LineStart(n), _)) = self.peek() else {
                break;
            };
            let n = *n;

            if n < map_indent {
                break;
            }
            if n > map_indent {
                self.advance();
                continue;
            }
            self.advance();

            // Skip indent/dedent tokens
            while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                self.advance();
            }

            // Try to parse the next entry
            if !self.parse_tagged_null_mapping_entry(map_indent) {
                break;
            }
        }

        // Use last event's end position for span calculation
        let end = self.last_event_end_position().max(start);
        self.emit(Event::MappingEnd {
            span: Span::from_usize_range(end..end),
        });
        true
    }

    /// Parse a single entry in a tagged-null-key mapping.
    /// Returns true if an entry was parsed, false to end the mapping.
    /// Emits key and value events; AST is reconstructed by `EventParser`.
    fn parse_tagged_null_mapping_entry(&mut self, map_indent: IndentLevel) -> bool {
        match self.peek() {
            // Plain scalar or quoted string key
            Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                // parse_scalar emits events
                if self.parse_scalar().is_none() {
                    return false;
                }
                self.skip_ws();

                if !matches!(self.peek(), Some((Token::Colon, _))) {
                    return false;
                }
                self.advance();
                self.skip_ws();

                // parse_mapping_value emits events; ignore returned Node
                let _ = self.parse_mapping_value(map_indent);
                true
            }
            // Key with tag or anchor
            Some((Token::Tag(_) | Token::Anchor(_), _)) => {
                let inner_props = self.collect_node_properties(NodeProperties::default());
                self.parse_propertied_key_entry(map_indent, inner_props)
            }
            // Empty key (colon at line start)
            Some((Token::Colon, _)) => {
                // Emit empty scalar for the null key
                self.emit_null_scalar();
                self.advance();
                self.check_tabs_after_block_indicator();
                self.skip_ws();

                // parse_mapping_value emits events; ignore returned Node
                let _ = self.parse_mapping_value(map_indent);
                true
            }
            _ => false,
        }
    }

    /// Parse a mapping entry where the key has properties (tag/anchor).
    /// Emits key and value events; AST is reconstructed by `EventParser`.
    fn parse_propertied_key_entry(
        &mut self,
        map_indent: IndentLevel,
        props: NodeProperties<'input>,
    ) -> bool {
        match self.peek() {
            // Scalar key with properties
            Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                // parse_scalar emits events
                if self.parse_scalar().is_none() {
                    return false;
                }
                // Apply properties to the last emitted event
                self.apply_properties_to_events(&props);
                if let Some((name, _)) = &props.anchor {
                    self.anchors.insert(name);
                }
                self.skip_ws();

                if !matches!(self.peek(), Some((Token::Colon, _))) {
                    return false;
                }
                self.advance();
                self.skip_ws();

                // parse_mapping_value emits events; ignore returned Node
                let _ = self.parse_mapping_value(map_indent);
                true
            }
            // Null key with properties (tag/anchor followed by colon)
            Some((Token::Colon, _)) => {
                // Emit scalar event for the null key with properties
                let key_span = self.current_span();
                self.emit(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor: props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
                    tag: props.tag.clone(),
                    span: key_span,
                });
                if let Some((name, _)) = &props.anchor {
                    self.anchors.insert(name);
                }
                self.advance();
                self.skip_ws();

                // parse_mapping_value emits events; ignore returned Node
                let _ = self.parse_mapping_value(map_indent);
                true
            }
            _ => false,
        }
    }

    /// Parse a block mapping starting with an empty key (colon at line start).
    /// Emits events for the mapping; AST is reconstructed by `EventParser`.
    pub fn parse_block_mapping_with_empty_key(&mut self, _min_indent: IndentLevel) {
        let start = self.current_span().start_usize();
        let start_span = self.current_span();
        let map_indent = self.current_indent();

        // Emit MappingStart event
        self.emit(Event::MappingStart {
            style: CollectionStyle::Block,
            anchor: None,
            tag: None,
            span: start_span,
        });

        while let Some((Token::Colon, _)) = self.peek() {
            let key_span = self.current_span();

            // Emit empty key scalar event
            self.emit(Event::Scalar {
                style: ScalarStyle::Plain,
                value: Cow::Borrowed(""),
                anchor: None,
                tag: None,
                span: key_span,
            });

            self.advance();
            self.check_tabs_after_block_indicator();
            self.skip_ws();

            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };

            // If no value was parsed, emit null scalar
            if value.is_none() {
                let null_span = self.current_span();
                self.emit(Event::Scalar {
                    style: ScalarStyle::Plain,
                    value: Cow::Borrowed(""),
                    anchor: None,
                    tag: None,
                    span: null_span,
                });
            }

            self.skip_ws();
            while let Some((Token::LineStart(n), _)) = self.peek() {
                if *n < map_indent {
                    // Use last event's end position for span calculation
                    let end = self.last_event_end_position().max(start);
                    let end_span = Span::from_usize_range(end..end);
                    self.emit(Event::MappingEnd { span: end_span });
                    return;
                }
                if *n == map_indent {
                    self.advance();
                    break;
                }
                self.advance();
            }

            if !matches!(self.peek(), Some((Token::Colon, _))) {
                break;
            }
        }

        // Use last event's end position for span calculation
        let end = self.last_event_end_position().max(start);
        let end_span = Span::from_usize_range(end..end);
        self.emit(Event::MappingEnd { span: end_span });
    }

    /// Parse a block mapping where the key is an alias.
    /// Emits `MappingStart`, key/value pair events, and `MappingEnd`.
    pub fn parse_alias_as_mapping_key(
        &mut self,
        alias_name: &'input str,
        alias_span: Span,
        props: &NodeProperties<'input>,
    ) {
        if !self.anchors.contains(alias_name) {
            self.error(ErrorKind::UndefinedAlias, alias_span);
        }

        // Emit MappingStart with props (anchor/tag apply to the mapping)
        self.emit(Event::MappingStart {
            style: CollectionStyle::Block,
            anchor: props.anchor.map(|(name, sp)| (Cow::Borrowed(name), sp)),
            tag: props.tag.clone(),
            span: alias_span,
        });

        // Register anchor if present
        if let Some((name, _)) = &props.anchor {
            self.anchors.insert(name);
        }

        // Emit Alias event for the key
        self.emit(Event::Alias {
            name: Cow::Borrowed(alias_name),
            span: alias_span,
        });

        self.advance(); // ':'
        self.skip_ws();

        let map_indent = self.column_of_position(alias_span.start_usize());

        // Parse value (events emitted by parse_value)
        let first_value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        // If no value, emit null scalar
        if first_value.is_none() {
            self.emit_null_scalar();
        }

        loop {
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

            // Parse key (events emitted by parse_scalar or emit(Alias))
            let key_parsed = match self.peek() {
                Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                    // parse_scalar emits events
                    self.parse_scalar().is_some()
                }
                Some((Token::Alias(name), span)) => {
                    let new_alias_name = *name;
                    let new_alias_span = span;
                    self.advance();
                    if !self.anchors.contains(new_alias_name) {
                        self.error(ErrorKind::UndefinedAlias, new_alias_span);
                    }
                    // Emit Alias event for the key
                    self.emit(Event::Alias {
                        name: Cow::Borrowed(new_alias_name),
                        span: new_alias_span,
                    });
                    true
                }
                _ => false,
            };

            if !key_parsed {
                break;
            }

            self.skip_ws();

            if !matches!(self.peek(), Some((Token::Colon, _))) {
                break;
            }
            self.advance();
            self.skip_ws();

            // Parse value (events emitted)
            let next_value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };
            if next_value.is_none() {
                self.emit_null_scalar();
            }
        }

        // Use last event's end position for span calculation
        let end = self.last_event_end_position().max(alias_span.start_usize());

        // Emit MappingEnd event
        self.emit(Event::MappingEnd {
            span: Span::from_usize_range(end..end),
        });
    }
}
