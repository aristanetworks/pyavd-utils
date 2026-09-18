// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
use std::sync::Arc;

use avdschema::dict::DynamicKeyOverrides;

use crate::feedback::CoercionNote;
use crate::feedback::ErrorIssue;
use crate::feedback::Feedback;
use crate::feedback::InfoIssue;
use crate::feedback::Path;
use crate::feedback::SourceSpan;
use crate::feedback::StringLoweredNote;
use crate::feedback::Value;
use crate::feedback::Violation;
use crate::feedback::WarningIssue;
use crate::validatable::ValidatableValue;

/// Configuration and accumulated diagnostics for one validation.
#[derive(Debug)]
pub(crate) struct Context {
    pub configuration: Configuration,
    pub result: ValidationResult,
}

impl Context {
    pub(crate) fn new(configuration: Option<&Configuration>) -> Self {
        Self {
            configuration: configuration.cloned().unwrap_or_default(),
            result: Default::default(),
        }
    }
    pub(crate) fn add_error_for<V: ValidatableValue>(
        &mut self,
        state: &ValidationState,
        value: &V,
        error: impl Into<ErrorIssue>,
    ) {
        self.add_error_with_span(state, value.source_span(), error);
    }

    pub(crate) fn add_error_with_span(
        &mut self,
        state: &ValidationState,
        span: Option<SourceSpan>,
        error: impl Into<ErrorIssue>,
    ) {
        self.result.errors.push(Feedback {
            path: state.path.clone(),
            span,
            issue: error.into(),
        });
    }

    pub(crate) fn add_warning_with_span(
        &mut self,
        state: &ValidationState,
        span: Option<SourceSpan>,
        warning: impl Into<WarningIssue>,
    ) {
        self.result.warnings.push(Feedback {
            path: state.path.clone(),
            span,
            issue: warning.into(),
        });
    }

    pub(crate) fn add_info_for<V: ValidatableValue>(
        &mut self,
        state: &ValidationState,
        value: &V,
        info: impl Into<InfoIssue>,
    ) {
        self.result.infos.push(Feedback {
            path: state.path.clone(),
            span: value.source_span(),
            issue: info.into(),
        });
    }

    pub(crate) fn add_coercion_for<V: ValidatableValue>(
        &mut self,
        state: &ValidationState,
        value: &V,
        made: impl Into<Value>,
    ) {
        if self.configuration.return_coercion_infos {
            self.add_info_for(
                state,
                value,
                CoercionNote {
                    found: value.to_feedback_value(),
                    made: made.into(),
                },
            );
        }
    }

    pub(crate) fn add_string_lowered_for<V: ValidatableValue>(
        &mut self,
        state: &ValidationState,
        value: &V,
        found: &str,
        made: &str,
    ) {
        if self.configuration.return_coercion_infos {
            self.add_info_for(
                state,
                value,
                StringLoweredNote {
                    found: found.to_owned(),
                    made: made.to_owned(),
                },
            );
        }
    }

    pub(crate) fn add_duplicate_value_violation_pair_for<
        A: ValidatableValue,
        B: ValidatableValue,
    >(
        &mut self,
        state: &ValidationState,
        value_a: &A,
        trail_a: &[String],
        value_b: &B,
        trail_b: &[String],
    ) {
        // Violation from A's perspective (A sees B as duplicate)
        let violation_a = Feedback {
            path: state.path.clone_with_slice(trail_a),
            span: value_a.source_span(),
            issue: Violation::ValueNotUnique {
                other_path: state.path.clone_with_slice(trail_b),
                other_span: value_b.source_span(),
            }
            .into(),
        };

        // Violation from B's perspective (B sees A as duplicate)
        let violation_b = Feedback {
            path: state.path.clone_with_slice(trail_b),
            span: value_b.source_span(),
            issue: Violation::ValueNotUnique {
                other_path: state.path.clone_with_slice(trail_a),
                other_span: value_a.source_span(),
            }
            .into(),
        };

        self.result.errors.extend([violation_a, violation_b]);
    }
}

/// Short-lived traversal state passed alongside [`Context`] during validation.
///
/// Context owns configuration and the accumulated result. This state carries
/// only the location and mode of the node currently being validated.
#[derive(Clone, Debug, Default)]
pub(crate) struct ValidationState {
    /// Don't validate required keys.
    /// Used for `structured_config` where we overload other config, and only the final result should be validated for required keys.
    pub(crate) relaxed_validation: bool,
    pub(crate) path: Path,
}

#[cfg(test)]
impl ValidationState {
    pub(crate) fn with_path(path: Path) -> Self {
        Self {
            path,
            ..Default::default()
        }
    }
}

/// Configuration to use during validation.
#[derive(Clone, Debug, Default)]
pub struct Configuration {
    /// Optional caller-supplied dynamic key overrides keyed by concrete input key.
    /// The override value is the schema dynamic-key path that should be used for that key.
    /// This is used by the LSP when interpreting # comments on keys.
    /// Stored behind Arc so cloning Configuration for each new Context stays cheap.
    pub dynamic_key_overrides: Option<Arc<DynamicKeyOverrides>>,
    pub ignore_required_keys_on_root_dict: bool,
    /// By default Null/None values are ignored no matter which data type is expected.
    /// Setting this will instead emit type errors for Null values.
    pub restrict_null_values: bool,
    /// When true, validation returns coerced data with types adjusted according to the schema.
    /// When false (default), validation returns a null placeholder to avoid expensive cloning.
    pub return_coerced_data: bool,
    /// Set to true when you need the coerced output (e.g., for data transformation).
    /// Set to false for validation-only use cases (e.g., LSP diagnostics).
    pub return_coercion_infos: bool,
    /// When validating `avd_design`, emit warnings for top-level keys that exist in `eos_config`
    /// but not in `avd_design`.
    pub warn_eos_config_keys: bool,
}

#[derive(Clone, Debug, Default)]
pub struct ValidationResult {
    pub errors: Vec<Feedback<ErrorIssue>>,
    pub warnings: Vec<Feedback<WarningIssue>>,
    pub infos: Vec<Feedback<InfoIssue>>,
}
