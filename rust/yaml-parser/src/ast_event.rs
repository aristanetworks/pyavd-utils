// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use crate::{Comment, Event, span::BytePosition};

/// AST-oriented event stream that carries structural spans for collection entries.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AstEvent<'input> {
    /// A plain event with no attached comment trivia.
    Event(Event<'input>),
    /// A node-level event with attached comment trivia.
    RichEvent {
        event: Event<'input>,
        /// Comment captured on the structural header line before this node
        /// starts, such as after `key:` or `-` before a nested block value.
        ///
        /// Example:
        /// ```yaml
        /// key: # comment
        ///   value
        /// ```
        ///
        /// During AST construction this is not attached to the node itself.
        /// It is consumed by the parent structure:
        /// - mapping values become `MappingPair.header_comment`
        /// - sequence entries become `SequenceItem.header_comment`
        ///
        /// If no such structural owner exists, it is currently dropped.
        leading_comment: Option<Comment<'input>>,
        /// Same-line comment that trails this event's node source.
        ///
        /// Example:
        /// ```yaml
        /// key: value # comment
        /// ```
        ///
        /// During AST construction this is attached to `Node.trailing_comment`
        /// for scalar/alias nodes. For other event kinds it is transient and
        /// is currently dropped.
        trailing_comment: Option<Comment<'input>>,
    },
    /// The first event of a sequence item, annotated with the item start offset.
    SequenceItem {
        item_start: BytePosition,
        event: Event<'input>,
        /// Comment captured after `-` before a nested block item starts.
        ///
        /// Example:
        /// ```yaml
        /// - # comment
        ///   value
        /// ```
        ///
        /// During AST construction this becomes `SequenceItem.header_comment`.
        /// It is not attached to the child node itself.
        leading_comment: Option<Comment<'input>>,
        /// Same-line comment that trails the sequence item's child node source.
        ///
        /// Example:
        /// ```yaml
        /// - value # comment
        /// ```
        ///
        /// During AST construction this becomes `Node.trailing_comment` on the
        /// parsed child node when the child is a scalar or alias.
        trailing_comment: Option<Comment<'input>>,
    },
    /// The first event of a mapping pair key, annotated with the pair start offset.
    MappingKey {
        pair_start: BytePosition,
        key_event: Event<'input>,
        /// Comment captured on the structural header line before the wrapped
        /// key event starts.
        ///
        /// This is usually `None` for ordinary scalar keys, but it can be set
        /// when the wrapped event is a collection start from an implicit
        /// mapping. In that case it flows to the resulting `ParsedNode` as
        /// leading comment metadata and may become the parent pair's
        /// `MappingPair.header_comment`.
        leading_comment: Option<Comment<'input>>,
        /// Same-line comment that trails the key node source.
        ///
        /// Example:
        /// ```yaml
        /// key # comment
        /// : value
        /// ```
        ///
        /// During AST construction this becomes `Node.trailing_comment` on the
        /// parsed key node when the key is a scalar or alias.
        trailing_comment: Option<Comment<'input>>,
    },
}

impl<'input> From<Event<'input>> for AstEvent<'input> {
    fn from(value: Event<'input>) -> Self {
        Self::Event(value)
    }
}
