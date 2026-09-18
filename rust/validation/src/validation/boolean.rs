// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::NodeValidation;
use super::invalid_type;
use crate::context::Context;
use crate::context::ValidationState;
use crate::feedback::Type;
use crate::validatable::ValidatableValue;

pub(crate) fn validate_node<V: ValidatableValue>(
    value: &V,
    ctx: &mut Context,
    state: &mut ValidationState,
) -> NodeValidation<bool> {
    if let Some(boolean) = value.as_bool() {
        // Boolean schemas have no constraints to validate beyond type checking.
        NodeValidation::Valid(boolean)
    } else {
        invalid_type(value, ctx, state, Type::Bool)
    }
}

#[cfg(test)]
mod tests {
    use avdschema::boolean::SourceBool;
    use serde_json::Value;

    use super::*;
    use crate::feedback::Feedback;
    use crate::feedback::Type;
    use crate::feedback::Violation;
    use crate::validation::test_utils::TestValidate as _;

    #[test]
    fn validate_type_ok() {
        let schema = SourceBool::default();
        let input: Value = true.into();
        let mut ctx = Context::new(None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = SourceBool::default();
        let input = serde_json::json!([]);
        let mut ctx = Context::new(None);
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
