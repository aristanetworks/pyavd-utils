// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Block structure parsing (block sequences and mappings).

use chumsky::span::Span as _;

use crate::error::ErrorKind;
use crate::lexer::Token;
use crate::span::Span;
use crate::value::{Node, Value};

use super::{NodeProperties, Parser};

impl<'a> Parser<'a> {
    /// Parse a block sequence: - item\n- item
    pub fn parse_block_sequence(&mut self, _min_indent: usize) -> Option<Node> {
        let (_, start_span) = self.peek()?;
        let start = start_span.start;
        let seq_indent = self.current_token_column();
        let mut items: Vec<Node> = Vec::new();

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
            self.skip_ws();

            let item = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                if let Some((Token::Indent(_), _)) = self.peek() {
                    self.advance();
                }
                let content_col = self.current_token_column();
                self.parse_value(content_col)
            } else {
                self.parse_value(seq_indent + 1)
            };

            if let Some(item) = item {
                items.push(item);
            } else {
                items.push(Node::null(self.current_span()));
            }

            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            loop {
                match self.peek() {
                    Some((Token::Whitespace | Token::Comment(_) | Token::Indent(_), _)) => {
                        self.advance();
                    }
                    Some((Token::LineStart(n), _)) => {
                        let n = *n;
                        if n < seq_indent {
                            let end = items.last().map(|n| n.span.end).unwrap_or(start);
                            self.pop_indent();
                            return Some(Node::new(
                                Value::Sequence(items),
                                Span::new((), start..end),
                            ));
                        }
                        self.advance();
                    }
                    Some((Token::Dedent, _)) => {
                        self.advance();
                    }
                    Some((Token::BlockSeqIndicator, _)) => {
                        break;
                    }
                    _ => {
                        break;
                    }
                }
            }
        }

        let end = items.last().map(|n| n.span.end).unwrap_or(start);
        self.pop_indent();
        Some(Node::new(Value::Sequence(items), Span::new((), start..end)))
    }

    /// Parse a block mapping with explicit key indicator or implicit keys.
    pub fn parse_block_mapping(&mut self, _min_indent: usize) -> Option<Node> {
        let start = self.current_span().start;
        let map_indent = self.current_token_column();
        let mut pairs: Vec<(Node, Node)> = Vec::new();

        self.push_indent(map_indent);

        let mut first_entry = true;

        while !self.is_eof() {
            let loop_start_pos = self.pos;

            if !first_entry && self.current_indent() < map_indent {
                break;
            }

            let explicit_key = matches!(self.peek(), Some((Token::MappingKey, _)));
            if explicit_key {
                self.advance();
                self.skip_ws();
            }

            let empty_key = !explicit_key && matches!(self.peek(), Some((Token::Colon, _)));

            // Collect any anchor/tag properties before the key
            let mut key_props = NodeProperties::default();
            loop {
                match self.peek() {
                    Some((Token::Anchor(name), anchor_span)) => {
                        let name = name.clone();
                        let anchor_span = *anchor_span;
                        self.advance();
                        self.skip_ws();
                        if key_props.anchor.is_some() {
                            self.error(ErrorKind::DuplicateAnchor, anchor_span);
                        }
                        key_props.anchor = Some((name, anchor_span));
                    }
                    Some((Token::Tag(tag), tag_span)) => {
                        let tag = tag.clone();
                        let tag_span = *tag_span;
                        self.advance();
                        self.skip_ws();
                        if key_props.tag.is_some() {
                            self.error(ErrorKind::DuplicateTag, tag_span);
                        }
                        key_props.tag = Some((tag, tag_span));
                    }
                    Some((Token::Whitespace, _)) => {
                        self.advance();
                    }
                    _ => break,
                }
            }

            let key = if explicit_key {
                match self.peek() {
                    Some((Token::MappingKey | Token::Colon, _)) => None,
                    Some((Token::LineStart(_), _)) => {
                        self.advance();
                        if let Some((Token::Indent(_), _)) = self.peek() {
                            self.advance();
                        }
                        self.parse_value(map_indent + 1)
                    }
                    Some((Token::Indent(_), _)) => {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    }
                    _ => self.parse_value(map_indent + 1),
                }
            } else if empty_key {
                None
            } else {
                // Check if the key is an alias with properties (invalid)
                if let Some((Token::Alias(alias_name), span)) = self.peek() {
                    let alias_name = alias_name.clone();
                    let span = *span;
                    if !key_props.is_empty() {
                        self.error(ErrorKind::PropertiesOnAlias, span);
                    }
                    self.advance();
                    if !self.anchors.contains_key(&alias_name) {
                        self.error(ErrorKind::UndefinedAlias, span);
                    }
                    Some(Node::new(Value::Alias(alias_name), span))
                } else {
                    self.parse_scalar()
                }
            };

            let key = match key {
                Some(k) => {
                    // Apply properties to the key (unless it's an alias, which already errored)
                    if !key_props.is_empty() && !matches!(k.value, Value::Alias(_)) {
                        self.apply_properties_and_register(key_props, k)
                    } else {
                        k
                    }
                }
                None if explicit_key || empty_key => Node::null(self.current_span()),
                None => break,
            };

            self.skip_ws();

            let mut has_value = matches!(self.peek(), Some((Token::Colon, _)));
            if !has_value && explicit_key {
                if let Some((Token::LineStart(n), _)) = self.peek() {
                    if *n >= map_indent {
                        let saved_pos = self.pos;
                        self.advance();

                        while let Some((Token::Indent(_), _)) = self.peek() {
                            self.advance();
                        }

                        while let Some((Token::Dedent, _)) = self.peek() {
                            self.advance();
                        }

                        if matches!(self.peek(), Some((Token::Colon, _))) {
                            has_value = true;
                        } else {
                            self.pos = saved_pos;
                        }
                    }
                }
            }

            if has_value && !matches!(self.peek(), Some((Token::Colon, _))) {
                // We already advanced past LineStart above, now at colon
            }
            if has_value && matches!(self.peek(), Some((Token::Colon, _))) {
                self.advance();
                self.skip_ws();
            }

            let value = if has_value {
                match self.peek() {
                    Some((Token::LineStart(_), _)) => {
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
                    _ => self.parse_value(map_indent + 1),
                }
            } else {
                None
            };

            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

            pairs.push((key, value));
            first_entry = false;

            self.skip_ws();
            if let Some((Token::Comment(_), _)) = self.peek() {
                self.advance();
            }

            if let Some((Token::Dedent, _)) = self.peek() {
                self.advance();
                let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
                self.pop_indent();
                return Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)));
            }

            while let Some((tok, _)) = self.peek() {
                match tok {
                    Token::LineStart(n) => {
                        let n = *n;
                        if n < map_indent {
                            let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
                            self.pop_indent();
                            return Some(Node::new(
                                Value::Mapping(pairs),
                                Span::new((), start..end),
                            ));
                        }
                        if n == map_indent {
                            self.advance();
                            while let Some((Token::Dedent, _)) = self.peek() {
                                self.advance();
                            }
                            break;
                        }
                        self.advance();
                    }
                    Token::Indent(_) => {
                        self.advance();
                    }
                    Token::Dedent => {
                        self.advance();
                        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
                        self.pop_indent();
                        return Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)));
                    }
                    _ => break,
                }
            }

            if let Some((tok, _)) = self.peek() {
                match tok {
                    Token::Plain(_)
                    | Token::StringStart(_)
                    | Token::MappingKey
                    | Token::Colon
                    | Token::LiteralBlockHeader(_)
                    | Token::FoldedBlockHeader(_)
                    | Token::Anchor(_)
                    | Token::Alias(_)
                    | Token::Tag(_) => {}
                    _ => break,
                }
            }

            if self.pos == loop_start_pos && !self.is_eof() {
                self.advance();
                break;
            }
        }

        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
        self.pop_indent();
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a block mapping where the first key already has properties (anchor/tag).
    pub fn parse_block_mapping_with_props(
        &mut self,
        _min_indent: usize,
        first_key_props: NodeProperties,
    ) -> Option<Node> {
        let start = self.current_span().start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node, Node)> = Vec::new();

        let mut key_props = first_key_props;

        loop {
            match self.peek() {
                Some((Token::Tag(tag), tag_span)) => {
                    let tag = tag.clone();
                    let tag_span = *tag_span;
                    self.advance();
                    self.skip_ws();
                    if key_props.tag.is_some() {
                        self.error(ErrorKind::DuplicateTag, tag_span);
                    }
                    key_props.tag = Some((tag, tag_span));
                }
                Some((Token::Anchor(name), anchor_span)) => {
                    let name = name.clone();
                    let anchor_span = *anchor_span;
                    self.advance();
                    self.skip_ws();
                    if key_props.anchor.is_some() {
                        self.error(ErrorKind::DuplicateAnchor, anchor_span);
                    }
                    key_props.anchor = Some((name, anchor_span));
                }
                Some((Token::Whitespace, _)) => {
                    self.advance();
                }
                _ => break,
            }
        }

        let first_key = match self.peek() {
            Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                let scalar = self.parse_scalar()?;
                self.apply_properties_and_register(key_props, scalar)
            }
            Some((Token::FlowSeqStart | Token::FlowMapStart, _)) => {
                if let Some(n) = self.parse_flow_value() {
                    self.apply_properties_and_register(key_props, n)
                } else {
                    return None;
                }
            }
            _ => {
                return None;
            }
        };

        self.skip_ws();

        let has_value = matches!(self.peek(), Some((Token::Colon, _)));
        if !has_value {
            return None;
        }

        self.advance(); // consume ':'
        self.skip_ws();

        let first_value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let first_value = first_value.unwrap_or_else(|| Node::null(self.current_span()));

        pairs.push((first_key, first_value));

        self.skip_ws();
        if let Some((Token::Comment(_), _)) = self.peek() {
            self.advance();
        }

        while let Some((Token::LineStart(n), _)) = self.peek() {
            if *n < map_indent {
                break;
            }
            if *n == map_indent {
                self.advance();

                if let Some(key) = self.parse_scalar() {
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
                        let value = value.unwrap_or_else(|| Node::null(self.current_span()));
                        pairs.push((key, value));
                    }
                }
            } else {
                self.advance();
            }
        }

        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a block mapping when we already have the first key.
    pub fn parse_block_mapping_starting_with_key(
        &mut self,
        _min_indent: usize,
        first_key: Node,
    ) -> Option<Node> {
        let start = first_key.span.start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node, Node)> = Vec::new();

        if !matches!(self.peek(), Some((Token::Colon, _))) {
            return Some(first_key);
        }
        self.advance(); // consume ':'
        self.skip_ws();

        let first_value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let first_value = first_value.unwrap_or_else(|| Node::null(self.current_span()));

        pairs.push((first_key, first_value));

        loop {
            self.skip_ws();

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

            while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                self.advance();
            }

            match self.peek() {
                Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                    let key = self.parse_scalar()?;
                    self.skip_ws();

                    if !matches!(self.peek(), Some((Token::Colon, _))) {
                        break;
                    }
                    self.advance();
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value));
                }
                Some((Token::FlowSeqStart | Token::FlowMapStart, _)) => {
                    let key = if matches!(self.peek(), Some((Token::FlowSeqStart, _))) {
                        self.parse_flow_sequence()?
                    } else {
                        self.parse_flow_mapping()?
                    };
                    self.skip_ws();

                    if !matches!(self.peek(), Some((Token::Colon, _))) {
                        break;
                    }
                    self.advance();
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value));
                }
                _ => break,
            }
        }

        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a block mapping where the first key is a null with properties.
    pub fn parse_block_mapping_with_tagged_null_key(
        &mut self,
        _min_indent: usize,
        key_props: NodeProperties,
    ) -> Option<Node> {
        let start = self.current_span().start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node, Node)> = Vec::new();

        let first_key =
            self.apply_properties_and_register(key_props, Node::null(self.current_span()));

        if !matches!(self.peek(), Some((Token::Colon, _))) {
            return None;
        }
        self.advance();
        self.skip_ws();

        let first_value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let first_value = first_value.unwrap_or_else(|| Node::null(self.current_span()));

        pairs.push((first_key, first_value));

        loop {
            self.skip_ws();

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

            while let Some((Token::Indent(_) | Token::Dedent, _)) = self.peek() {
                self.advance();
            }

            match self.peek() {
                Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                    let key = self.parse_scalar()?;
                    self.skip_ws();

                    if !matches!(self.peek(), Some((Token::Colon, _))) {
                        break;
                    }
                    self.advance();
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value));
                }
                Some((Token::Tag(_) | Token::Anchor(_), _)) => {
                    let mut inner_props = NodeProperties::default();
                    loop {
                        match self.peek() {
                            Some((Token::Tag(tag), tag_span)) => {
                                let tag = tag.clone();
                                let tag_span = *tag_span;
                                self.advance();
                                self.skip_ws();
                                if inner_props.tag.is_some() {
                                    self.error(ErrorKind::DuplicateTag, tag_span);
                                }
                                inner_props.tag = Some((tag, tag_span));
                            }
                            Some((Token::Anchor(name), anchor_span)) => {
                                let name = name.clone();
                                let anchor_span = *anchor_span;
                                self.advance();
                                self.skip_ws();
                                if inner_props.anchor.is_some() {
                                    self.error(ErrorKind::DuplicateAnchor, anchor_span);
                                }
                                inner_props.anchor = Some((name, anchor_span));
                            }
                            Some((Token::Whitespace, _)) => {
                                self.advance();
                            }
                            _ => break,
                        }
                    }

                    match self.peek() {
                        Some((Token::Plain(_) | Token::StringStart(_), _)) => {
                            let key = self.parse_scalar()?;
                            let key = self.apply_properties_and_register(inner_props, key);
                            self.skip_ws();

                            if !matches!(self.peek(), Some((Token::Colon, _))) {
                                break;
                            }
                            self.advance();
                            self.skip_ws();

                            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                                self.advance();
                                self.parse_value(map_indent + 1)
                            } else {
                                self.parse_value(map_indent + 1)
                            };
                            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                            pairs.push((key, value));
                        }
                        Some((Token::Colon, _)) => {
                            let key = self.apply_properties_and_register(
                                inner_props,
                                Node::null(self.current_span()),
                            );
                            self.advance();
                            self.skip_ws();

                            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                                self.advance();
                                self.parse_value(map_indent + 1)
                            } else {
                                self.parse_value(map_indent + 1)
                            };
                            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                            pairs.push((key, value));
                        }
                        _ => break,
                    }
                }
                Some((Token::Colon, _)) => {
                    let key = Node::null(self.current_span());
                    self.advance();
                    self.skip_ws();

                    let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                        self.advance();
                        self.parse_value(map_indent + 1)
                    } else {
                        self.parse_value(map_indent + 1)
                    };
                    let value = value.unwrap_or_else(|| Node::null(self.current_span()));

                    pairs.push((key, value));
                }
                _ => break,
            }
        }

        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a block mapping starting with an empty key (colon at line start).
    pub fn parse_block_mapping_with_empty_key(&mut self, _min_indent: usize) -> Option<Node> {
        let start = self.current_span().start;
        let map_indent = self.current_indent();
        let mut pairs: Vec<(Node, Node)> = Vec::new();

        while let Some((Token::Colon, _)) = self.peek() {
            let key = Node::null(self.current_span());

            self.advance();
            self.skip_ws();

            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };

            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

            pairs.push((key, value));

            self.skip_ws();
            while let Some((Token::LineStart(n), _)) = self.peek() {
                if *n < map_indent {
                    let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
                    return Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)));
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

        let end = pairs.last().map(|(_, v)| v.span.end).unwrap_or(start);
        Some(Node::new(Value::Mapping(pairs), Span::new((), start..end)))
    }

    /// Parse a block mapping where the key is an alias.
    pub fn parse_alias_as_mapping_key(
        &mut self,
        alias_name: String,
        alias_span: Span,
        props: NodeProperties,
    ) -> Option<Node> {
        if !self.anchors.contains_key(&alias_name) {
            self.error(ErrorKind::UndefinedAlias, alias_span);
        }

        let key = Node::new(Value::Alias(alias_name), alias_span);

        self.advance(); // ':'
        self.skip_ws();

        let map_indent = self.column_of_position(alias_span.start);

        let value = if let Some((Token::LineStart(_), _)) = self.peek() {
            self.advance();
            self.parse_value(map_indent + 1)
        } else {
            self.parse_value(map_indent + 1)
        };
        let value = value.unwrap_or_else(|| Node::null(self.current_span()));

        let mut pairs = vec![(key, value)];

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

            let key = match self.peek() {
                Some((Token::Plain(_) | Token::StringStart(_), _)) => self.parse_scalar(),
                Some((Token::Alias(name), span)) => {
                    let name = name.clone();
                    let span = *span;
                    self.advance();
                    if !self.anchors.contains_key(&name) {
                        self.error(ErrorKind::UndefinedAlias, span);
                    }
                    Some(Node::new(Value::Alias(name), span))
                }
                _ => None,
            };

            let Some(key) = key else {
                break;
            };

            self.skip_ws();

            if !matches!(self.peek(), Some((Token::Colon, _))) {
                break;
            }
            self.advance();
            self.skip_ws();

            let value = if let Some((Token::LineStart(_), _)) = self.peek() {
                self.advance();
                self.parse_value(map_indent + 1)
            } else {
                self.parse_value(map_indent + 1)
            };
            let value = value.unwrap_or_else(|| Node::null(self.current_span()));

            pairs.push((key, value));
        }

        let end = pairs
            .last()
            .map(|(_, v)| v.span.end)
            .unwrap_or(alias_span.start);
        let mapping = Node::new(Value::Mapping(pairs), Span::new((), alias_span.start..end));

        Some(self.apply_properties_and_register(props, mapping))
    }
}
