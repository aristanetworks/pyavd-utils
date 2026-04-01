// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

pub(crate) mod any;
pub(crate) mod boolean;
pub(crate) mod dict;
pub(crate) mod int;
pub(crate) mod list;
pub(crate) mod store;
pub(crate) mod str;
pub(crate) mod valid_values;

use crate::{context::Context, validatable::ValidatableValue};

/// Trait for validating values against a schema.
///
/// Accepts any type implementing [`ValidatableValue`], allowing validation
/// of both `serde_json::Value` and `yaml_parser::Node`.
///
/// Optionally returns the coerced value with types corrected based on schema expectations.
/// For example, a string "123" validated against an Int schema returns an Int value.
/// When `ctx.configuration.return_coerced_data` is false, returns `None` to avoid allocations.
pub trait Validation {
    /// Validate any value implementing [`ValidatableValue`] against this schema.
    ///
    /// This method validates the value and optionally returns a coerced version with types
    /// adjusted based on the schema. It works with both `serde_json::Value` and
    /// `yaml_parser::Node`.
    ///
    /// Returns `Some(coerced)` when `ctx.configuration.return_coerced_data` is true,
    /// `None` otherwise (to avoid expensive allocations in validation-only use cases).
    ///
    /// The coerced value preserves metadata (like YAML spans) from the original.
    fn validate<V: ValidatableValue>(&self, value: &V, ctx: &mut Context) -> Option<V::Coerced>;
}

#[cfg(test)]
pub(crate) mod test_utils;
