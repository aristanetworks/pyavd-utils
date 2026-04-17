// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::{any::AnySchema, boolean::Bool, resolve_ref};

use crate::{
    context::Context,
    feedback::{Type, Violation},
    validatable::ValidatableValue,
};

use super::Validation;

impl Validation for Bool {
    fn validate<V: ValidatableValue>(&self, value: &V, ctx: &mut Context) -> Option<V::Coerced> {
        if let Some(maybe_coerced) = validate_ref(self, value, ctx) {
            return maybe_coerced;
        }

        if let Some(v) = value.as_bool() {
            // Bool schema has no constraints to validate beyond type checking
            if ctx.configuration.return_coerced_data {
                Some(value.coerce_bool(v))
            } else {
                None
            }
        } else {
            Self::handle_invalid_type(value, ctx, Type::Bool)
        }
    }
}

/// Validate against a referenced schema (for unresolved $ref ending with #).
fn validate_ref<V: ValidatableValue>(
    schema: &Bool,
    value: &V,
    ctx: &mut Context,
) -> Option<Option<V::Coerced>> {
    if let Some(ref_) = schema.base.schema_ref.as_ref()
        && let Ok(AnySchema::Bool(ref_schema)) = resolve_ref(ref_, ctx.store)
    {
        return Some(ref_schema.validate(value, ctx));
    }
    None
}

#[cfg(test)]
mod tests {
    use serde_json::Value;

    use super::*;
    use crate::{
        feedback::{Feedback, Type, Violation},
        validation::test_utils::get_test_store,
    };

    #[test]
    fn validate_type_ok() {
        let schema = Bool::default();
        let input: Value = true.into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = Bool::default();
        let input = serde_json::json!([]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::InvalidType {
                    expected: Type::Bool,
                    found: Type::List,
                }
                .into(),
            }],
        );
    }
}
