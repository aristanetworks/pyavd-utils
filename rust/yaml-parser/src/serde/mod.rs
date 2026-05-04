// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Serde integration for `yaml-parser`.
//!
//! This module exposes the crate's public serde entrypoints for both:
//! - deserialization from YAML text into `T`
//! - serialization from `T` back to YAML text
//!
//! Deserialization uses the shared lexer/emitter pipeline and drives serde
//! visitors directly from the YAML event stream instead of building an
//! intermediate AST first. Scalar semantics come from the same shared YAML 1.2
//! Core-schema resolver used by the AST parser, so plain untagged scalars,
//! quoted/block scalars, and explicit built-in tags behave consistently across
//! both entrypoints.
//!
//! Public contract:
//!
//! - [`from_str`] expects exactly one YAML document,
//! - [`stream_from_str_docs`] is the multi-document entry point,
//! - anchor state is scoped to each document when streaming,
//! - [`from_reader`] currently reads the full input into memory before parsing,
//! - serialization is aimed at common configuration-shaped data,
//! - non-finite floats are rejected during serialization.

mod de;
mod event_de;
mod ser;

pub use de::DeError;
pub use de::StreamDeserializer;
pub use de::from_reader;
pub use de::from_str;
pub use de::stream_from_str_docs;
pub use ser::SerError;
pub use ser::to_string;
pub use ser::to_writer;
