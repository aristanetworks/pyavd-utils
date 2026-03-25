// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use crate::{Event, span::BytePosition};

/// AST-oriented event stream that carries structural spans for collection entries.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AstEvent<'input> {
    /// An ordinary YAML node-level event.
    Event(Event<'input>),
    /// The first event of a sequence item, annotated with the item start offset.
    SequenceItem {
        item_start: BytePosition,
        event: Event<'input>,
    },
    /// The first event of a mapping pair key, annotated with the pair start offset.
    MappingKey {
        pair_start: BytePosition,
        key_event: Event<'input>,
    },
}

impl<'input> From<Event<'input>> for AstEvent<'input> {
    fn from(value: Event<'input>) -> Self {
        Self::Event(value)
    }
}
