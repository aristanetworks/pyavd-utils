// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use crate::{context::Context, validatable::ValidatableValue};
use avdschema::any::AnySchema;

use super::Validation;

impl Validation for AnySchema {
    fn validate<V: ValidatableValue>(&self, value: &V, ctx: &mut Context) -> Option<V::Coerced> {
        match self {
            Self::Bool(schema) => schema.validate(value, ctx),
            Self::Int(schema) => schema.validate(value, ctx),
            Self::Str(schema) => schema.validate(value, ctx),
            Self::List(schema) => schema.validate(value, ctx),
            Self::Dict(schema) => schema.validate(value, ctx),
        }
    }
}
