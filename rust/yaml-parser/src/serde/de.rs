// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::fmt;
use std::io::Read;

use serde::de::{self, DeserializeOwned};

use crate::ParseError;

/// Error type for serde-based deserialization from yaml-parser.
#[derive(Debug, derive_more::Display)]
pub enum DeError {
    /// YAML was syntactically invalid.
    #[display("YAML parse error: {}", _0)]
    Parse(ParseError),

    /// No document found in the input.
    #[display("expected at least one YAML document")]
    NoDocument,

    /// More than one document was found where a single document was expected.
    #[display("expected a single YAML document")]
    MultipleDocuments,

    /// A mapping contained a value without a corresponding key.
    #[display("value without corresponding key in mapping")]
    ValueWithoutKey,

    /// An alias referenced an unknown anchor name.
    #[display("unknown YAML alias '{}'", _0)]
    UnknownAlias(String),

    /// Enum deserialization expected a string representation.
    #[display("expected string for enum")]
    ExpectedEnumString,

    /// I/O error while reading YAML input.
    #[display("I/O error while reading YAML: {}", _0)]
    Io(std::io::Error),

    /// Generic serde error created via `serde::de::Error::custom`.
    #[display("serde custom error: {}", _0)]
    Custom(String),
}

impl std::error::Error for DeError {}

impl de::Error for DeError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Self::Custom(msg.to_string())
    }
}

impl From<ParseError> for DeError {
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}

/// Streaming deserializer over multiple YAML documents in a string.
///
/// This implementation uses the event-based serde backend, which drives serde
/// visitors directly from the YAML event stream without building an intermediate
/// AST. Each call to [`Iterator::next`] deserializes at most one additional
/// document.
pub type StreamDeserializer<'de, T> = super::event_de::EventStreamDeserializer<'de, T>;

/// Deserialize a single YAML document from a string into `T`.
///
/// This uses the event-based serde backend, which drives serde visitors
/// directly from the YAML event stream without building an intermediate AST.
pub fn from_str<T>(input: &str) -> Result<T, DeError>
where
    T: DeserializeOwned,
{
    super::event_de::from_str_internal(input)
}

/// Deserialize a single YAML document from a reader into `T`.
pub fn from_reader<R, T>(mut reader: R) -> Result<T, DeError>
where
    R: Read,
    T: DeserializeOwned,
{
    let mut buf = String::new();
    reader.read_to_string(&mut buf).map_err(DeError::Io)?;
    from_str(&buf)
}

/// Deserialize zero or more YAML documents from a string into a streaming
/// iterator of `T`.
///
/// This variant is fully streaming: it does not pre-parse the entire input
/// into a `Vec<Node>`, but instead parses and deserializes each document on
/// demand from the underlying event stream.
pub fn stream_from_str_docs<T>(input: &str) -> Result<StreamDeserializer<'_, T>, DeError>
where
    T: DeserializeOwned,
{
    Ok(StreamDeserializer::new(input))
}
