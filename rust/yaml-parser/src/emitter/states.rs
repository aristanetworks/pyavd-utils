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
    pub kind: ValueKind,
    pub allow_implicit_mapping: bool,
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
    AfterKey { is_implicit_scalar_key: bool },
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
    /// Parse any value with an associated `ValueContext`.
    ///
    /// This state is responsible for initial whitespace skipping,
    /// indentation validation, and property collection. Once properties
    /// have been collected, control transitions to `ValueAfterProperties`
    /// for the main dispatch and bridging/empty-value decisions.
    Value {
        ctx: ValueContext,
        /// Collected properties (anchor, tag) carried into the value.
        properties: EventProperties<'input>,
    },
    /// Continue parsing a value after properties have been collected.
    ///
    /// This state owns the logic for:
    /// - Bridging properties across dedent to block collections
    /// - Emitting empty scalars when bridging is not allowed
    /// - The main token dispatch for scalars, collections, and aliases.
    ValueAfterProperties {
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
    EmitDocStart { explicit: bool, span: Span },
    /// Parsing document content.
    Content,
    /// About to emit `DocumentEnd`.
    EmitDocEnd { explicit: bool, span: Span },
    /// Stream ended.
    Done,
}
