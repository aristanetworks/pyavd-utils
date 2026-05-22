// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
#![allow(
    clippy::empty_structs_with_brackets,
    clippy::field_scoped_visibility_modifiers,
    clippy::indexing_slicing,
    clippy::min_ident_chars,
    clippy::partial_pub_fields,
    clippy::shadow_unrelated,
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
pub use self::validation::store::StoreValidateInput;
pub use self::validation::store::ValidationOutput;
pub use self::validation::store::YamlValidationResult;
