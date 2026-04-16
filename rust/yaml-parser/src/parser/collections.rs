// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use crate::ast_event::AstEvent;
use crate::event::Event;
use crate::span::Span;
use crate::value::{MappingPair, Node, Properties as NodeProperties, SequenceItem, Value};

use super::Parser;

impl<'input, I> Parser<'input, I>
where
    I: Iterator,
    I::Item: Into<AstEvent<'input>>,
{
    /// Parse a mapping node.
    ///
    /// Span policy:
    /// - block mappings use `start..last_value.end`
    /// - flow mappings use `start..closing_brace.end`
    pub(super) fn parse_mapping(
        &mut self,
        props: Option<Box<NodeProperties<'input>>>,
        start_span: Span,
    ) -> Node<'input> {
        self.register_anchor(
            props
                .as_ref()
                .and_then(|event_props| event_props.anchor.as_ref()),
        );

        let mut pairs: Vec<MappingPair<'input>> = Vec::with_capacity(8);
        let mut end_span = start_span;

        loop {
            match self.peek() {
                Some(
                    AstEvent::Event(Event::MappingEnd { span })
                    | AstEvent::RichEvent {
                        event: Event::MappingEnd { span },
                        ..
                    },
                ) => {
                    end_span = *span;
                    self.advance();
                    break;
                }
                Some(
                    AstEvent::Event(Event::SequenceEnd { .. })
                    | AstEvent::RichEvent {
                        event: Event::SequenceEnd { .. },
                        ..
                    },
                ) => {
                    // Mismatched end marker - break to avoid infinite loop
                    break;
                }
                Some(AstEvent::MappingKey { .. }) => {
                    // Parse key
                    let Some(AstEvent::MappingKey {
                        pair_start,
                        key_event,
                        leading_comment,
                        trailing_comment,
                    }) = self.next_event()
                    else {
                        debug_assert!(false, "peeked MappingKey event disappeared");
                        break;
                    };
                    let key_node_result =
                        self.parse_node_from_event(key_event, leading_comment, trailing_comment);
                    // Preserve collection end markers so the outer mapping can
                    // unwind after recovering an orphan key.
                    if let Some(parsed_value) = self.parse_mapping_value_with_metadata()
                        && let Some(parsed_key) = key_node_result
                    {
                        let key_node = parsed_key.node;
                        let value = parsed_value.node;
                        let pair_span = Span::new(pair_start..value.span.end);
                        let mut pair = MappingPair::new(pair_span, key_node, value);
                        if let Some(comment) = parsed_value.leading_comment {
                            pair = pair.with_header_comment(comment);
                        }
                        pairs.push(pair);
                    }
                }
                Some(_) => {
                    let consumed_before = self.events_consumed;
                    let key = self.parse_node().unwrap_or_else(|| Node::null(start_span));
                    if let Some(parsed_value) = self.parse_mapping_value_with_metadata() {
                        let value = parsed_value.node;
                        let pair_span = Span::new(key.span.start..value.span.end);
                        let mut pair = MappingPair::new(pair_span, key, value);
                        if let Some(comment) = parsed_value.leading_comment {
                            pair = pair.with_header_comment(comment);
                        }
                        pairs.push(pair);
                    }
                    if self.events_consumed == consumed_before {
                        self.advance();
                    }
                }
                None => break,
            }
        }

        // Flow collections end at the closing brace. Block collections end at
        // the last successfully parsed value.
        let end = if end_span.start == end_span.end {
            // Block: use last value's end
            pairs
                .last()
                .map_or(start_span.start, |pair| pair.value.span.end)
        } else {
            // Flow: end_span covers the closing brace
            end_span.end
        };
        let span = Span::new(start_span.start..end);

        let mapping_node = Self::apply_properties(Node::new(Value::Mapping(pairs), span), props);
        self.store_anchor_node(&mapping_node);
        mapping_node
    }

    /// Parse a sequence node.
    ///
    /// Span policy:
    /// - block sequences use `start..last_item.end`
    /// - flow sequences use `start..closing_bracket.end`
    pub(super) fn parse_sequence(
        &mut self,
        props: Option<Box<NodeProperties<'input>>>,
        start_span: Span,
    ) -> Node<'input> {
        self.register_anchor(
            props
                .as_ref()
                .and_then(|event_props| event_props.anchor.as_ref()),
        );

        let mut items: Vec<SequenceItem<'input>> = Vec::with_capacity(16);
        let mut end_span = start_span;

        loop {
            match self.peek() {
                Some(
                    AstEvent::Event(Event::SequenceEnd { span })
                    | AstEvent::RichEvent {
                        event: Event::SequenceEnd { span },
                        ..
                    },
                ) => {
                    end_span = *span;
                    self.advance();
                    break;
                }
                Some(
                    AstEvent::Event(Event::MappingEnd { .. })
                    | AstEvent::RichEvent {
                        event: Event::MappingEnd { .. },
                        ..
                    },
                ) => {
                    // Mismatched end marker - break to avoid infinite loop
                    break;
                }
                Some(AstEvent::SequenceItem { .. }) => {
                    let Some(AstEvent::SequenceItem {
                        item_start,
                        event,
                        leading_comment,
                        trailing_comment,
                    }) = self.next_event()
                    else {
                        debug_assert!(false, "peeked SequenceItem event disappeared");
                        break;
                    };
                    if let Some(parsed) =
                        self.parse_node_from_event(event, leading_comment, trailing_comment)
                    {
                        let node = parsed.node;
                        let item_span = Span::new(item_start..node.span.end);
                        let mut item = SequenceItem::new(item_span, node);
                        if let Some(comment) = parsed.leading_comment {
                            item = item.with_header_comment(comment);
                        }
                        items.push(item);
                    }
                }
                Some(_) => {
                    let consumed_before = self.events_consumed;
                    if let Some(parsed) = self.parse_node_with_metadata() {
                        let node = parsed.node;
                        let item_span = node.span;
                        let mut item = SequenceItem::new(item_span, node);
                        if let Some(comment) = parsed.leading_comment {
                            item = item.with_header_comment(comment);
                        }
                        items.push(item);
                    }
                    if self.events_consumed == consumed_before {
                        self.advance();
                    }
                }
                None => break,
            }
        }

        // Flow collections end at the closing bracket. Block collections end
        // at the last successfully parsed item.
        let end = if end_span.start == end_span.end {
            // Block: use last item's end
            items
                .last()
                .map_or(start_span.start, |item| item.node.span.end)
        } else {
            // Flow: end_span covers the closing bracket
            end_span.end
        };
        let span = Span::new(start_span.start..end);

        let sequence_node = Self::apply_properties(Node::new(Value::Sequence(items), span), props);
        self.store_anchor_node(&sequence_node);
        sequence_node
    }
}
