// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Event-to-AST parser.
//!
//! This module implements the `Parser` that consumes events emitted by the
//! YAML emitter and builds the AST (`Node` / `Value`).
//!
//! # Architecture
//!
//! ```text
//! Lexer -> Emitter (Event stream) -> Parser -> AST
//! ```
//!
//! The `Parser` is much simpler than a token-based parser because
//! structural detection (indentation, flow/block contexts) is handled by the
//! `Emitter`. The `Parser` just needs to:
//! 1. Match start/end event pairs (mapping, sequence, document)
//! 2. Apply type inference to scalars
//! 3. Track anchors for alias validation
#![allow(
    clippy::multiple_inherent_impl,
    reason = "the parser is intentionally split across multiple files without changing the owning type"
)]

mod anchors;
mod collections;
mod nodes;
mod scalars;
mod stream;

use std::collections::{HashMap, HashSet};

use crate::ast_event::AstEvent;
use crate::error::ParseError;
use crate::value::{Comment, Node};

struct ParsedNode<'input> {
    node: Node<'input>,
    leading_comment: Option<Comment<'input>>,
}

/// Parser that builds AST from a streaming source of events.
///
/// This parser consumes events and builds the AST.
/// Structural complexity such as indentation and block/flow handling is
/// resolved earlier by the emitter.
///
/// The parser operates over any `Iterator<Item = Event<'input>>`, using an
/// internal one-element lookahead buffer.
pub(crate) struct Parser<'input, I>
where
    I: Iterator,
    I::Item: Into<AstEvent<'input>>,
{
    /// Underlying event iterator.
    events: I,
    /// Buffered lookahead event (result of the most recent `peek()`).
    peeked: Option<AstEvent<'input>>,
    /// Count of events that have been logically consumed via `advance()`.
    /// Used for progress tracking in recovery paths.
    events_consumed: usize,
    /// Collected errors
    errors: Vec<ParseError>,
    /// Set of registered anchor names (for alias validation)
    /// Uses owned strings because events may contain `Cow::Owned` values
    anchors: HashSet<String>,
    /// Completed anchored nodes available for alias resolution.
    anchor_nodes: HashMap<String, Node<'input>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NumericKind {
    Integer,
    Float,
    NotNumeric,
}

#[cfg(test)]
mod tests;
