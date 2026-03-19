// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use crate::{ScalarStyle, Span, event::Properties as EventProperties, span::IndentLevel};

/// Kind of value being parsed.
///
/// This captures the high-level context in which a value appears
/// (mapping key, mapping value, sequence entry, or top-level value).
#[derive(Debug, Clone, Copy)]
pub(super) enum ValueKind {
    Key,
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
    /// True when properties were carried from a prior Value iteration (e.g. via
    /// Dedent re-entry). `OR`ed with `initial_crossed_line` so that crossing
    /// history is preserved across re-dispatch.
    pub prior_crossed_line: bool,
}

/// Phase within a block sequence.
#[derive(Debug, Clone, Copy)]
pub(super) enum BlockSeqPhase {
    /// Emit `SequenceStart`, then transition to `BeforeEntry`.
    EmitStart,
    /// Before parsing an entry - check for `-` indicator.
    BeforeEntry,
    /// After parsing entry value - check for next entry or end.
    AfterEntry,
}

/// Phase within a block mapping.
#[derive(Debug, Clone, Copy)]
pub(super) enum BlockMapPhase {
    /// Emit `MappingStart`, then transition to `BeforeKey`.
    EmitStart,
    /// Before parsing a key.
    /// `require_line_boundary`: If true, a new entry requires a line boundary.
    /// This is set to true after processing a key-value pair to prevent
    /// same-line entries like `a: b: c`.
    BeforeKey { require_line_boundary: bool },
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
    /// Emit `SequenceStart`, then transition to `BeforeEntry`.
    EmitStart,
    /// Before parsing an entry.
    BeforeEntry,
    /// After entry, expect `,` or `]`.
    AfterEntry,
    /// Emit empty key scalar for implicit mapping with empty key (e.g., `[ : value ]`).
    ImplicitMapEmptyKey { map_start_span: Span },
    /// After implicit mapping key, expect `:` then parse value.
    ImplicitMapValue { map_start_span: Span },
    /// After implicit mapping value, emit `MappingEnd`.
    ImplicitMapEnd,
}

/// Phase within a flow mapping.
#[derive(Debug, Clone, Copy)]
pub(super) enum FlowMapPhase {
    /// Emit `MappingStart`, then transition to `BeforeKey`.
    EmitStart,
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
        properties: EventProperties<'input>,
    },
    /// Collect properties (anchor, tag) for a value.
    /// `ctx.content_column` has been updated for the initial whitespace skip.
    /// Transitions to `ValueDispatch` after collecting.
    ValueCollectProperties {
        ctx: ValueContext,
        properties: EventProperties<'input>,
        initial_crossed_line: bool,
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
        properties: EventProperties<'input>,
        initial_crossed_line: bool,
        prop_crossed_line: bool,
        property_indent: Option<IndentLevel>,
    },
    /// Handle an alias token as a value, including potential complex-key
    /// behaviour when used as a mapping key.
    AliasValue {
        name: Cow<'input, str>,
        span: Span,
        properties: EventProperties<'input>,
        crossed_line_after_properties: bool,
    },

    /// Handle a flow collection start (`[` or `{`) as a value, including
    /// potential complex-key behaviour in block context.
    FlowCollectionValue {
        /// `true` for flow mappings (`{`), `false` for flow sequences (`[`).
        is_map: bool,
        span: Span,
        properties: EventProperties<'input>,
        /// Column of the `[`/`{` token, tracked from the Value dispatch.
        content_column: Option<IndentLevel>,
    },

    /// Handle additional properties after a line boundary before a value,
    /// including complex key patterns where outer and inner properties have
    /// different ownership.
    AdditionalPropertiesValue {
        /// Value context is stored for future use as we gradually migrate
        /// more behaviour into state-driven handlers.
        ctx: ValueContext,
        /// Properties collected before the line boundary ("outer" properties).
        outer: EventProperties<'input>,
    },
    /// Block sequence parsing.
    BlockSeq {
        indent: IndentLevel,
        phase: BlockSeqPhase,
        start_span: Span,
        /// Properties to attach to `SequenceStart` (only used in `EmitStart` phase).
        properties: EventProperties<'input>,
    },
    /// Block mapping parsing.
    BlockMap {
        indent: IndentLevel,
        phase: BlockMapPhase,
        start_span: Span,
        /// Properties to attach to `MappingStart` (only used in `EmitStart` phase).
        properties: EventProperties<'input>,
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
    /// Emit `MappingEnd` for block mapping.
    EmitMapEnd { span: Span },
    /// Emit `SequenceStart` (for complex key scenarios where `MappingStart` is emitted first).
    EmitSeqStart {
        properties: EventProperties<'input>,
        span: Span,
    },
    /// Emit a scalar event (used for deferred key emission).
    EmitScalar {
        value: Cow<'input, str>,
        properties: EventProperties<'input>,
        span: Span,
        style: ScalarStyle,
    },
    /// Emit an alias event (used for deferred key emission when alias is a mapping key).
    EmitAlias { name: Cow<'input, str>, span: Span },
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
