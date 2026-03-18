// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Serde integration for `yaml-parser`.
//!
//! This module provides helper functions to deserialize YAML input using
//! `serde`. The public API now uses an event-based backend that drives serde
//! visitors directly from the YAML event stream without building an intermediate
//! AST, providing better performance and lower memory usage.
//!
//! An AST-backed reference implementation is kept internally for testing and
//! comparison purposes.

mod de;
mod event_de;
mod ser;

pub use de::{DeError, StreamDeserializer, from_reader, from_str, stream_from_str_docs};
pub use ser::{SerError, to_string, to_writer};

/// Internal helper re-exported for benchmarks only.
///
/// This exposes the experimental event-based serde backend so that Criterion
/// benchmarks can compare it against the AST-backed `from_str` and
/// `serde_yaml`. It is **not** a stable public API and may change or be
/// removed at any time.
#[doc(hidden)]
pub use event_de::from_str_events_internal as bench_from_str_events_internal;
