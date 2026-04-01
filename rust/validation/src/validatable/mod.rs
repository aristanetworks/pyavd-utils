// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Traits for abstracting over different value representations.
//!
//! This module defines traits that allow validation code to work with different
//! structured value representations, enabling reuse of validation logic across
//! parsers and in-memory data models.
//!
//! # Coercion Behavior
//!
//! Some accessor methods perform type coercion to handle YAML's loose typing:
//!
//! - [`as_str()`](ValidatableValue::as_str): Coerces int, float, and bool to string
//! - [`as_i64()`](ValidatableValue::as_i64): Coerces string to int (if parseable)
//! - [`as_bool()`](ValidatableValue::as_bool): No coercion (strict)

use std::borrow::Cow;

use crate::feedback::{SourceSpan, Type};

/// A value that can be validated against a schema.
///
/// This trait abstracts over the common operations needed during validation,
/// allowing the same validation logic to work with different value representations.
pub trait ValidatableValue: Sized {
    /// The mapping type returned by [`as_mapping`](Self::as_mapping).
    type Mapping<'a>: ValidatableMapping<'a, Value = Self>
    where
        Self: 'a;

    /// The sequence type returned by [`as_sequence`](Self::as_sequence).
    type Sequence<'a>: ValidatableSequence<'a, Value = Self>
    where
        Self: 'a;

    /// The output type after coercion.
    ///
    /// For `serde_json::Value`, this is `Value` (same type).
    type Coerced;

    // === Strict type checking (no coercion) ===

    /// Returns `true` if this value is null/None.
    fn is_null(&self) -> bool;

    /// Returns `true` if this value is a string type.
    fn is_str(&self) -> bool;

    /// Returns `true` if this value is an integer type.
    fn is_int(&self) -> bool;

    /// Returns `true` if this value is a boolean type.
    fn is_bool(&self) -> bool;

    // === Value extraction with coercion ===

    /// Try to get this value as a string, coercing if possible.
    ///
    /// Coerces:
    /// - Int → String (e.g., `42` → `"42"`)
    /// - Float → String (e.g., `3.14` → `"3.14"`)
    /// - Bool → String (e.g., `true` → `"true"`)
    ///
    /// Returns `Cow::Borrowed` if already a string, `Cow::Owned` if coerced.
    fn as_str(&self) -> Option<Cow<'_, str>>;

    /// Try to get this value as a 64-bit signed integer, coercing if possible.
    ///
    /// Coerces:
    /// - String → Int (if parseable, e.g., `"123"` → `123`)
    fn as_i64(&self) -> Option<i64>;

    /// Try to get this value as a boolean (strict, no coercion).
    fn as_bool(&self) -> Option<bool>;

    // === Structural access ===

    /// Try to get this value as a mapping (object/dict).
    fn as_mapping(&self) -> Option<Self::Mapping<'_>>;

    /// Try to get this value as a sequence (array/list).
    fn as_sequence(&self) -> Option<Self::Sequence<'_>>;

    // === Quick child access ===

    /// Get a child value by key, if this is a mapping.
    ///
    /// This is a convenience method for quick lookups like checking if a
    /// primary key exists. Returns `None` if this is not a mapping or if
    /// the key doesn't exist.
    fn get(&self, key: &str) -> Option<&Self>;

    // === Type information for error reporting ===

    /// Get the type of this value for error reporting.
    fn value_type(&self) -> Type;

    /// Convert to feedback::Value for use in CoercionNote.
    /// This is used to record what value was coerced from.
    fn to_feedback_value(&self) -> crate::feedback::Value;

    /// Returns true if this value is a float.
    fn is_float(&self) -> bool;

    /// Get the source span for this value, if available.
    fn source_span(&self) -> Option<SourceSpan> {
        None
    }

    // === Path navigation ===

    /// Check if a value exists at the given dot-separated path.
    ///
    /// This is used for checking deprecation conflicts where we need to
    /// verify if a replacement key exists.
    fn path_exists(&self, path: &str) -> bool {
        !self.walk_path(path).is_empty()
    }

    /// Walk a dot-separated path and return all matching values.
    ///
    /// Path traversal expands through sequences automatically, so a path like
    /// `items.name` will match the `name` key under every item in a list.
    /// An empty path returns the value itself for scalars/mappings, and all
    /// sequence items for sequences.
    ///
    /// Returns pairs of `(trail, value)` where `trail` is the concrete path
    /// from `self` to the matched value, including list indexes.
    fn walk_path<'a>(&'a self, path: &str) -> Vec<(Vec<String>, &'a Self)> {
        fn walk<'a, V: ValidatableValue>(
            value: &'a V,
            components: &[&str],
            trail: &mut Vec<String>,
            results: &mut Vec<(Vec<String>, &'a V)>,
        ) {
            if let Some(seq) = value.as_sequence() {
                for (index, item) in seq.iter().enumerate() {
                    trail.push(index.to_string());
                    walk(item, components, trail, results);
                    trail.pop();
                }
                return;
            }

            let Some((component, rest)) = components.split_first() else {
                results.push((trail.clone(), value));
                return;
            };

            if let Some(child) = value.get(component) {
                trail.push((*component).to_string());
                walk(child, rest, trail, results);
                trail.pop();
            }
        }

        let components: Vec<&str> = if path.is_empty() {
            Vec::new()
        } else {
            path.split('.').collect()
        };
        let mut results = Vec::new();
        let mut trail = Vec::new();
        walk(self, &components, &mut trail, &mut results);
        results
    }

    // === Coercion builders ===
    //
    // These methods construct coerced output values, preserving metadata
    // (like spans) from the original value.

    /// Create a coerced null value.
    fn coerce_null(&self) -> Self::Coerced;

    /// Create a coerced boolean value.
    fn coerce_bool(&self, value: bool) -> Self::Coerced;

    /// Create a coerced integer value.
    fn coerce_int(&self, value: i64) -> Self::Coerced;

    /// Create a coerced string value.
    fn coerce_str(&self, value: String) -> Self::Coerced;

    /// Create a coerced sequence from coerced items.
    fn coerce_sequence(&self, items: Vec<Self::Coerced>) -> Self::Coerced;

    /// Create a coerced mapping from coerced key-value pairs.
    fn coerce_mapping(&self, items: Vec<(String, Self::Coerced)>) -> Self::Coerced;

    /// Clone the value as-is without type coercion.
    /// Used when there's no schema to guide coercion.
    fn clone_to_coerced(&self) -> Self::Coerced;
}

/// A mapping (object/dict) that can be iterated and queried.
pub trait ValidatableMapping<'a> {
    /// The type of values in this mapping.
    type Value: ValidatableValue + 'a;

    /// The mapping pair type yielded by iteration.
    type Pair: ValidatableMappingPair<'a, Value = Self::Value>;

    /// Iterator type for mapping pairs.
    type Iter: Iterator<Item = Self::Pair>;

    /// Get a value by key.
    fn get(&self, key: &str) -> Option<&Self::Value>;

    /// Check if a key exists in the mapping.
    fn contains_key(&self, key: &str) -> bool;

    /// Iterate over key-value pairs.
    fn iter(&self) -> Self::Iter;

    /// Get the number of entries in the mapping.
    #[allow(dead_code, reason = "len() could be useful later")]
    fn len(&self) -> usize;

    /// Check if the mapping is empty.
    #[allow(
        dead_code,
        clippy::len_without_is_empty,
        reason = "is_empty() could be useful later"
    )]
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// A single mapping key/value pair.
pub trait ValidatableMappingPair<'a> {
    /// The type of values in this mapping pair.
    type Value: ValidatableValue + 'a;

    /// Get the key as a string.
    fn key(&self) -> Cow<'a, str>;

    /// Get the mapped value.
    fn value(&self) -> &'a Self::Value;

    /// Get the source span of the key, if available.
    fn key_span(&self) -> Option<SourceSpan> {
        None
    }
}

/// A sequence (array/list) that can be iterated.
pub trait ValidatableSequence<'a> {
    /// The type of values in this sequence.
    type Value: ValidatableValue + 'a;

    /// Iterator type for values.
    type Iter: Iterator<Item = &'a Self::Value>;

    /// Iterate over values.
    fn iter(&self) -> Self::Iter;

    /// Get the number of items in the sequence.
    fn len(&self) -> usize;

    /// Check if the sequence is empty.
    #[allow(dead_code, reason = "exposed for future use in validators")]
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

// === Implementations ===

mod serde_json_impl;

#[cfg(test)]
mod tests;
