// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::Store;

use crate::{
    feedback::{ErrorIssue, Feedback, InfoIssue, Path, Violation, WarningIssue},
    validatable::ValidatableValue,
};

/// The Context object is passed along during coercion and validation.
/// All coercions and violations will be registered in the context with the path carried in the context.
/// The store is used for looking up schema references.
#[derive(Debug)]
pub struct Context<'a> {
    pub configuration: Configuration,
    pub store: &'a Store,
    pub result: ValidationResult,
    pub(crate) state: State,
}

impl<'a> Context<'a> {
    pub fn new(store: &'a Store, configuration: Option<&'a Configuration>) -> Self {
        Self {
            configuration: configuration.cloned().unwrap_or_default(),
            store,
            result: Default::default(),
            state: Default::default(),
        }
    }
    pub(crate) fn add_error_for<V: ValidatableValue>(
        &mut self,
        value: &V,
        error: impl Into<ErrorIssue>,
    ) {
        self.add_error_with_span(value.source_span(), error);
    }

    pub(crate) fn add_error_with_span(
        &mut self,
        span: Option<crate::feedback::SourceSpan>,
        error: impl Into<ErrorIssue>,
    ) {
        self.result.errors.push(Feedback {
            path: self.state.path.to_owned(),
            span,
            issue: error.into(),
        });
    }

    pub(crate) fn add_warning_with_span(
        &mut self,
        span: Option<crate::feedback::SourceSpan>,
        warning: impl Into<WarningIssue>,
    ) {
        self.result.warnings.push(Feedback {
            path: self.state.path.to_owned(),
            span,
            issue: warning.into(),
        });
    }

    pub(crate) fn add_info_for<V: ValidatableValue>(
        &mut self,
        value: &V,
        info: impl Into<InfoIssue>,
    ) {
        self.result.infos.push(Feedback {
            path: self.state.path.to_owned(),
            span: value.source_span(),
            issue: info.into(),
        });
    }

    pub(crate) fn add_duplicate_violation_pair_for<A: ValidatableValue, B: ValidatableValue>(
        &mut self,
        value_a: &A,
        trail_a: &[String],
        value_b: &B,
        trail_b: &[String],
    ) {
        // Violation from A's perspective (A sees B as duplicate)
        let violation_a = Feedback {
            path: self.state.path.clone_with_slice(trail_a),
            span: value_a.source_span(),
            issue: Violation::ValueNotUnique {
                other_path: self.state.path.clone_with_slice(trail_b),
                other_span: value_b.source_span(),
            }
            .into(),
        };

        // Violation from B's perspective (B sees A as duplicate)
        let violation_b = Feedback {
            path: self.state.path.clone_with_slice(trail_b),
            span: value_b.source_span(),
            issue: Violation::ValueNotUnique {
                other_path: self.state.path.clone_with_slice(trail_a),
                other_span: value_a.source_span(),
            }
            .into(),
        };

        self.result.errors.extend([violation_a, violation_b]);
    }
}

/// Validation state set on Context during validation.
#[derive(Clone, Debug, Default)]
pub(crate) struct State {
    /// Don't validate required keys.
    /// Used for structured_config where we overload other config, and only the final result should be validated for required keys.
    pub(crate) relaxed_validation: bool,
    pub(crate) path: Path,
}

/// Configuration to use during validation.
#[derive(Clone, Debug, Default)]
pub struct Configuration {
    pub ignore_required_keys_on_root_dict: bool,
    pub return_coercion_infos: bool,
    /// By default Null/None values are ignored no matter which data type is expected.
    /// Setting this will instead emit type errors for Null values.
    pub restrict_null_values: bool,
    /// When validating avd_design, emit warnings for top-level keys that exist in eos_config
    /// but not in avd_design.
    pub warn_eos_config_keys: bool,
    /// When true, validation returns coerced data with types adjusted according to the schema.
    /// When false (default), validation returns a null placeholder to avoid expensive cloning.
    /// Set to true when you need the coerced output (e.g., for data transformation).
    /// Set to false for validation-only use cases (e.g., LSP diagnostics).
    pub return_coerced_data: bool,
}

#[derive(Clone, Debug, Default)]
pub struct ValidationResult {
    pub errors: Vec<Feedback<ValidationIssue>>,
    pub warnings: Vec<Feedback<WarningIssue>>,
    pub infos: Vec<Feedback<InfoIssue>>,
}
