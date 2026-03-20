// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Serde integration for `yaml-parser`.
//!
//! This module provides helper functions to deserialize YAML input using
//! `serde`. The public API now uses an event-based backend that drives serde
//! visitors directly from the YAML event stream without building an intermediate
//! AST, providing better performance and lower memory usage.

mod de;
mod event_de;
mod ser;

pub use de::{DeError, StreamDeserializer, from_reader, from_str, stream_from_str_docs};
pub use ser::{SerError, to_string, to_writer};
