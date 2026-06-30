// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
// TODO: Reevaluate the allow
#![allow(
    missing_docs,
    missing_debug_implementations,
    clippy::as_conversions,
    clippy::cast_possible_truncation,
    clippy::cast_precision_loss,
    clippy::default_trait_access,
    clippy::empty_structs_with_brackets,
    clippy::field_scoped_visibility_modifiers,
    clippy::float_cmp,
    clippy::from_iter_instead_of_collect,
    clippy::match_wildcard_for_single_variants,
    clippy::option_option,
    clippy::partial_pub_fields,
    clippy::struct_excessive_bools,
    clippy::trivially_copy_pass_by_ref,
    reason = "Existing validation models and tests predate workspace lint inheritance"
)]
#![deny(unused_crate_dependencies)]

mod context;
pub mod feedback;
mod validatable;
mod validation;

pub use self::context::Configuration;
pub use self::context::Context;
pub use self::context::ValidationResult;
pub use self::validation::Validation;
pub use self::validation::store::InputValidationResult;
pub use self::validation::store::StoreValidate;
pub use self::validation::store::StoreValidateError;
pub use self::validation::store::StoreValidateInput;
pub use self::validation::store::ValidationOutput;
pub use self::validation::store::YamlValidationResult;
