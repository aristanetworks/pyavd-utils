// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use crate::{context::Context, validatable::ValidatableValue};
use avdschema::{any::AnySchema, delegate_anyschema_method};

use super::Validation;

impl Validation for AnySchema {
    fn validate<V: ValidatableValue>(&self, value: &V, ctx: &mut Context) -> Option<V::Coerced> {
        delegate_anyschema_method!(self, validate, value, ctx)
    }
}
