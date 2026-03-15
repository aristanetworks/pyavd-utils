// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Serde integration for `yaml-parser`.
//!
//! This module provides helper functions to deserialize YAML input using
//! `serde`, built on top of the parser's AST (`Node` / `Value`).

mod de;
mod ser;

pub use de::{DeError, StreamDeserializer, from_reader, from_str, stream_from_str_docs};
pub use ser::{SerError, to_string, to_writer};
