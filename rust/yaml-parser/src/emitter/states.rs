// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use crate::{
    Span,
    event::{Properties as EventProperties, Property as EventProperty},
    lexer::{BlockScalarHeader, QuoteStyle},
    span::IndentLevel,
};

/// Sparse emitter-side property carrier.
///
/// Anchors and tags are rare in real-world YAML, so keeping the event-layer
/// `Properties` inline in hot emitter states makes the state stack much larger
/// than the common case needs. This wrapper keeps empty properties as `None`
/// and only materializes a boxed payload when a property is actually present.
#[derive(Debug, Clone, Default)]
pub(super) struct EmitterProperties<'input>(Option<Box<EventProperties<'input>>>);

impl<'input> EmitterProperties<'input> {
    #[must_use]
    pub(super) fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    #[must_use]
    pub(super) fn has_anchor(&self) -> bool {
        self.0.as_ref().is_some_and(|props| props.anchor.is_some())
    }

    #[must_use]
    pub(super) fn has_tag(&self) -> bool {
        self.0.as_ref().is_some_and(|props| props.tag.is_some())
    }

    pub(super) fn set_anchor(&mut self, anchor: EventProperty<'input>) {
        self.get_or_insert_mut().anchor = Some(anchor);
    }

    pub(super) fn set_tag(&mut self, tag: EventProperty<'input>) {
        self.get_or_insert_mut().tag = Some(tag);
    }

    #[must_use]
    pub(super) fn into_event_box(self) -> Option<Box<EventProperties<'input>>> {
        self.0
    }

    #[must_use]
    pub(super) fn into_inner(self) -> EventProperties<'input> {
        self.0.map_or_else(EventProperties::default, |props| *props)
    }

    #[must_use]
    pub(super) fn into_parts(
        self,
    ) -> (Option<EventProperty<'input>>, Option<EventProperty<'input>>) {
        let props = self.into_inner();
        (props.anchor, props.tag)
    }

    #[must_use]
    pub(super) fn from_parts(
        anchor: Option<EventProperty<'input>>,
        tag: Option<EventProperty<'input>>,
    ) -> Self {
        if anchor.is_none() && tag.is_none() {
            Self::default()
        } else {
            Self(Some(Box::new(EventProperties { anchor, tag })))
        }
    }

    pub(super) fn merge_from(&mut self, other: Self) {
        let Some(other_props) = other.0 else {
            return;
        };
        let props = self.get_or_insert_mut();
        if let Some(anchor) = other_props.anchor {
            props.anchor = Some(anchor);
        }
        if let Some(tag) = other_props.tag {
            props.tag = Some(tag);
        }
    }

    #[must_use]
    pub(super) fn merged(mut self, other: Self) -> Self {
        self.merge_from(other);
        self
    }

    pub(super) fn take(&mut self) -> Self {
        Self(self.0.take())
    }

    fn get_or_insert_mut(&mut self) -> &mut EventProperties<'input> {
        self.0
            .get_or_insert_with(|| Box::new(EventProperties::default()))
            .as_mut()
    }
}

impl<'input> From<EventProperties<'input>> for EmitterProperties<'input> {
    fn from(value: EventProperties<'input>) -> Self {
        if value.is_empty() {
            Self::default()
        } else {
            Self(Some(Box::new(value)))
        }
    }
}

/// Kind of value being parsed.
///
/// This captures the high-level context in which a value appears
/// (mapping key, mapping value, sequence entry, or top-level value).
#[derive(Debug, Clone, Copy)]
pub(super) enum ValueKind {
    /// An implicit block-mapping key such as `key:` or `"key":`.
    ///
    /// This context applies simple-key restrictions like multiline-key errors
    /// and suppresses recursive implicit block-mapping detection.
    ImplicitKey,
    /// An explicit key introduced by `?`.
    ///
    /// Explicit keys can be arbitrary YAML nodes, including nested implicit
    /// mappings like `? earth: blue`.
    ExplicitKey,
    MappingValue,
    SeqEntryValue,
    TopLevelValue,
}

/// Context for parsing a single value.
///
/// This groups together the indentation constraint and semantic kind of
/// the value, along with whether nested implicit mappings are allowed in
/// this position.
#[derive(Debug, Clone, Copy)]
pub(super) struct ValueContext {
    pub min_indent: IndentLevel,
    /// Column where content starts on the same line as the indicator, if known.
    /// `Some(col)` when content follows the indicator on the same line (e.g., `- - a` → col 2).
    /// `None` when content is on a subsequent line or not yet determined.
    /// Separate from `min_indent` because scalar continuation needs the lower
    /// bound (`entry_indent + 1`) while nested structures need the actual position.
    pub content_column: Option<IndentLevel>,
    pub kind: ValueKind,
    pub allow_implicit_mapping: bool,
    /// True when properties were carried from a prior Value iteration after an
    /// earlier line-boundary transition. `OR`ed with `initial_crossed_line` so
    /// that crossing history is preserved across re-dispatch.
    pub prior_crossed_line: bool,
}

/// Phase within a block sequence.
#[derive(Debug, Clone, Copy)]
pub(super) enum BlockSeqPhase {
    /// Scan trivia and line transitions before parsing the next entry.
    BeforeEntryScan,
    /// Dispatch the next entry/end action after line scanning is complete.
    BeforeEntryDispatch,
}

/// Phase within a block mapping.
#[derive(Debug, Clone, Copy)]
pub(super) enum BlockMapPhase {
    /// Scan trivia and line transitions before parsing the next key.
    /// `require_line_boundary`: If true, a new entry requires a line boundary.
    /// This is set to true after processing a key-value pair to prevent
    /// same-line entries like `a: b: c`.
    BeforeKeyScan {
        require_line_boundary: bool,
        crossed_line: bool,
    },
    /// Dispatch the next key/value action after line scanning is complete.
    BeforeKeyDispatch {
        require_line_boundary: bool,
        crossed_line: bool,
    },
    /// After key, expect `:` and value.
    /// `is_implicit_scalar_key`: If true, the key was an implicit scalar (like `key:`).
    /// Block sequences on the same line as such keys are invalid (`key: - item`).
    AfterKey {
        is_implicit_scalar_key: bool,
        /// Column after the key ends. Used to compute `content_column` after `:`.
        /// Set from span width for implicit keys; None for explicit keys (`:` is
        /// on a new line at `indent`).
        key_end_column: Option<IndentLevel>,
    },
    /// After value, check for next pair or end.
    AfterValue,
}

/// Phase within a flow sequence.
#[derive(Debug, Clone, Copy)]
pub(super) enum FlowSeqPhase {
    /// Before parsing an entry.
    BeforeEntry,
    /// After entry, expect `,` or `]`.
    AfterEntry,
    /// Emit empty key scalar for implicit mapping with empty key (e.g., `[ : value ]`).
    ImplicitMapEmptyKey { map_start_span: Span },
    /// After implicit mapping key, expect `:` then parse value.
    ImplicitMapValue,
    /// After implicit mapping value, emit `MappingEnd`.
    ImplicitMapEnd,
}

/// Phase within a flow mapping.
#[derive(Debug, Clone, Copy)]
pub(super) enum FlowMapPhase {
    /// Before parsing a key.
    BeforeKey,
    /// After key, expect `:`.
    AfterKey,
    /// After value, expect `,` or `}`.
    AfterValue,
}

/// A parsing state on the stack.
///
/// Each variant represents a construct being parsed and its current phase.
/// The stack replaces the call stack from recursive descent parsing.
#[derive(Debug, Clone)]
pub(super) enum ParseState<'input> {
    /// Parse any value: skip initial whitespace/newlines, update
    /// `ctx.content_column`, then transition to `ValueCollectProperties`.
    Value {
        ctx: ValueContext,
        /// Collected properties (anchor, tag) carried into the value.
        properties: EmitterProperties<'input>,
    },
    /// Collect properties (anchor, tag) for a value.
    /// `ctx.content_column` has been updated for the initial whitespace skip.
    /// Transitions to `ValueDispatch` after collecting.
    ValueCollectProperties {
        ctx: ValueContext,
        properties: EmitterProperties<'input>,
        initial_crossed_line: bool,
        crossed_property_line_boundary: bool,
        consumed_width: IndentLevel,
    },
    /// Dispatch a value after properties have been collected and
    /// `ctx.content_column` has been updated through each transition.
    ///
    /// This state owns the logic for:
    /// - Bridging properties across dedent to block collections
    /// - Emitting empty scalars when bridging is not allowed
    /// - The main token dispatch for scalars, collections, and aliases.
    ValueDispatch {
        ctx: ValueContext,
        /// Properties (anchor, tag) collected for this value.
        properties: EmitterProperties<'input>,
        initial_crossed_line: bool,
        prop_crossed_line: bool,
        property_indent: Option<IndentLevel>,
    },
    /// Continue value parsing after property-bridging decisions have been made.
    /// This state owns post-property line crossing, dedent re-entry, dedented
    /// empty values, and final token dispatch.
    ValueDispatchToken {
        ctx: ValueContext,
        properties: EmitterProperties<'input>,
        initial_crossed_line: bool,
        prop_crossed_line: bool,
    },
    /// Handle an alias token as a value, including potential complex-key
    /// behaviour when used as a mapping key.
    AliasValue {
        name: Cow<'input, str>,
        span: Span,
        properties: EmitterProperties<'input>,
        crossed_line_after_properties: bool,
    },

    /// Continue parsing a block-context plain scalar after the first line has
    /// already been consumed.
    PlainScalarBlock {
        first_line: Cow<'input, str>,
        properties: EmitterProperties<'input>,
        start_span: Span,
        end_span: Span,
        min_indent: IndentLevel,
        consecutive_newlines: usize,
        has_continuation: bool,
        content: Option<String>,
    },
    /// Continue parsing a flow-context plain scalar after the first line has
    /// already been consumed.
    PlainScalarFlow {
        first_line: Cow<'input, str>,
        properties: EmitterProperties<'input>,
        start_span: Span,
        end_span: Span,
        has_continuation: bool,
        content: Option<String>,
    },
    /// Continue parsing a quoted scalar after the opening quote and optional
    /// first content token have already been consumed.
    QuotedScalar {
        properties: EmitterProperties<'input>,
        quote_style: QuoteStyle,
        min_indent: IndentLevel,
        start_span: Span,
        parts: Vec<Cow<'input, str>>,
        end_span: Span,
        pending_newlines: usize,
        needs_trim: bool,
    },
    /// Parse block scalar content after the header has been consumed.
    BlockScalar {
        properties: EmitterProperties<'input>,
        header: BlockScalarHeader,
        start_span: Span,
        min_indent: IndentLevel,
        is_literal: bool,
    },

    /// Handle a flow collection start (`[` or `{`) as a value, including
    /// potential complex-key behaviour in block context.
    FlowCollectionValue {
        /// `true` for flow mappings (`{`), `false` for flow sequences (`[`).
        is_map: bool,
        span: Span,
        properties: EmitterProperties<'input>,
        kind: ValueKind,
        /// Column of the `[`/`{` token, tracked from the Value dispatch.
        content_column: Option<IndentLevel>,
    },

    /// Collect additional properties after a line boundary before a value.
    AdditionalPropertiesCollect {
        ctx: ValueContext,
        /// Properties collected before the line boundary ("outer" properties).
        outer: EmitterProperties<'input>,
        /// Properties currently being collected for the nested value.
        inner: EmitterProperties<'input>,
        crossed_line_boundary: bool,
        consumed_width: IndentLevel,
    },
    /// Dispatch a value after additional inner properties have been collected.
    AdditionalPropertiesValue {
        /// Value context for the nested value being dispatched.
        ctx: ValueContext,
        /// Properties collected before the line boundary ("outer" properties).
        outer: EmitterProperties<'input>,
        /// Properties collected for the nested value itself.
        inner: EmitterProperties<'input>,
    },
    /// Continue dispatch after the complex-key ownership decision has been made.
    /// This state owns the final token-based dispatch for nested additional
    /// properties.
    AdditionalPropertiesDispatchToken {
        ctx: ValueContext,
        /// Properties collected before the line boundary ("outer" properties).
        outer: EmitterProperties<'input>,
        /// Properties collected for the nested value itself.
        inner: EmitterProperties<'input>,
    },
    /// Block sequence parsing.
    BlockSeq {
        indent: IndentLevel,
        phase: BlockSeqPhase,
        start_span: Span,
        /// Properties to attach to the block-sequence start event.
        properties: EmitterProperties<'input>,
    },
    /// Block mapping parsing.
    BlockMap {
        indent: IndentLevel,
        phase: BlockMapPhase,
        start_span: Span,
        /// Properties to attach to the block-mapping start event.
        properties: EmitterProperties<'input>,
    },
    /// Flow sequence parsing.
    FlowSeq {
        phase: FlowSeqPhase,
        start_span: Span,
    },
    /// Flow mapping parsing.
    FlowMap {
        phase: FlowMapPhase,
        start_span: Span,
    },
}

/// Document-level state.
#[derive(Debug, Clone)]
pub(super) enum DocState {
    /// Ready to start a new document.
    Ready,
    /// About to emit `DocumentStart`.
    EmitDocStart {
        explicit: bool,
        span: Span,
        /// Tracked column of the first content token after document preparation.
        initial_col: IndentLevel,
    },
    /// Parsing document content.
    Content,
    /// About to emit `DocumentEnd`.
    EmitDocEnd { explicit: bool, span: Span },
    /// Stream ended.
    Done,
}
