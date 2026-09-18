// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

pub(crate) mod boolean;
pub(crate) mod dict;
pub(crate) mod int;
pub(crate) mod list;
pub(crate) mod store;
pub(crate) mod str;

use crate::context::Context;
use crate::context::ValidationState;
use crate::feedback::Type;
use crate::feedback::Violation;
use crate::validatable::ValidatableValue;

pub(crate) fn invalid_type<T, V: ValidatableValue>(
    value: &V,
    ctx: &mut Context,
    state: &ValidationState,
    expected: Type,
) -> NodeValidation<T> {
    if value.is_null() && !ctx.configuration.restrict_null_values {
        NodeValidation::Null
    } else {
        ctx.add_error_for(
            state,
            value,
            Violation::InvalidType {
                expected,
                found: value.value_type(),
            },
        );
        NodeValidation::Invalid
    }
}

/// Outcome of validating one node without recursively validating its children.
///
/// Container validators use this to separate node validation from traversal:
/// a caller may traverse [`Valid`](Self::Valid), preserve an accepted
/// [`Null`](Self::Null), or stop after [`Invalid`](Self::Invalid). Validation
/// diagnostics are added to the [`Context`] before `Invalid` is returned.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum NodeValidation<T> {
    /// The node has the expected type and may be traversed through this view.
    Valid(T),
    /// Null is accepted because `restrict_null_values` is disabled.
    Null,
    /// The node is invalid and the relevant diagnostic has already been added.
    Invalid,
}

#[cfg(test)]
pub(crate) mod test_utils;
