// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Serde integration for `yaml-parser`.
//!
//! This module provides helper functions to deserialize YAML input using
//! `serde`, currently built on top of the parser's AST (`Node` / `Value`).
//!
//! An experimental, internal event-based backend lives in `event_de` and is
//! used only in tests for now.

mod de;
mod ser;
mod event_de;

pub use de::{
    DeError,
    StreamDeserializer,
    from_reader,
    from_str,
    stream_from_str_docs,
};
pub use ser::{SerError, to_string, to_writer};

/// Internal helper re-exported for benchmarks only.
///
/// This exposes the experimental event-based serde backend so that Criterion
/// benchmarks can compare it against the AST-backed `from_str` and
/// `serde_yaml`. It is **not** a stable public API and may change or be
/// removed at any time.
#[doc(hidden)]
pub use event_de::from_str_events_internal as bench_from_str_events_internal;
