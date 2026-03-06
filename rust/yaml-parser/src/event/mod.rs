// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML serialization tree events.
//!
//! This module defines the event types following the YAML 1.2 spec's
//! "Serialization Tree" layer. Events are emitted by the Parser during
//! parsing and represent structural elements that preserve presentation details.
//!
//! # Architecture
//!
//! ```text
//! Stream Lexer → Document Lexer → Parser (emits Events) → EventParser → AST
//! ```
//!
//! The parser emits events as it parses, which can be:
//! - Used directly for streaming/SAX-style processing
//! - Consumed by `EventParser` to build an AST
//!
//! # Zero-Copy Design
//!
//! Events use `Cow<'input, str>` for values, borrowing from the input
//! when possible. Use [`Event::into_owned`] to convert to `'static` lifetime.

mod types;

pub use types::{CollectionStyle, Event, ScalarStyle};
