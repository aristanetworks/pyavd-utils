// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
// TODO: Reevaluate the allow
#![allow(
    missing_docs,
    clippy::empty_structs_with_brackets,
    clippy::empty_enum_variants_with_brackets,
    clippy::iter_over_hash_type,
    clippy::impl_trait_in_params,
    clippy::needless_pass_by_value,
    clippy::min_ident_chars,
    clippy::module_name_repetitions,
    clippy::multiple_inherent_impl,
    clippy::partial_pub_fields,
    clippy::pub_underscore_fields,
    clippy::redundant_type_annotations,
    clippy::shadow_unrelated,
    clippy::used_underscore_binding,
    clippy::unwrap_used,
    clippy::tests_outside_test_module,
    reason = "Existing schema models and feature-gated shared test helpers predate workspace lint inheritance"
)]
#![deny(unused_crate_dependencies)]

#[cfg(test)]
use test_schema_store as _;

mod inherit;
mod resolve;
mod schema;
mod store;
mod utils;

pub use self::inherit::Inherit;
pub use self::resolve::resolve_ref::resolve_ref;
pub use self::resolve::resolve_schema;
pub use self::schema::any;
pub use self::schema::base;
pub use self::schema::boolean;
pub use self::schema::dict;
pub use self::schema::dict::DynamicKeyOverrides;
pub use self::schema::int;
pub use self::schema::list;
pub use self::schema::str;
pub use self::store::SchemaStoreError;
pub use self::store::Store;
pub use self::utils::dump::Dump;
pub use self::utils::load::Load;
#[cfg(feature = "dump_load_files")]
pub use self::utils::load::LoadFromFragments;
pub use self::utils::schema_data::SchemaDataMapping;
pub use self::utils::schema_data::SchemaDataSequence;
pub use self::utils::schema_data::SchemaDataValue;
pub use self::utils::schema_from_path::SchemaKeys;
pub use self::utils::schema_from_path::get_schema_from_path;
