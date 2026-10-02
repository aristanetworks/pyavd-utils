// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! AVD schema source models and compiled runtime views.
//!
//! [`StoreSource`] and the `Source*` schema types are the authoring representation
//! deserialized from JSON, YAML, and compressed schema sources. Compilation resolves
//! references and inherited layers into typed, deduplicated tables. [`Store`] owns or
//! memory-maps those tables and is the canonical representation used by validation and
//! schema navigation. Runtime nodes are exposed as borrowed [`SchemaView`] variants and
//! typed child views; a view cannot outlive its store.
//!
//! [`Store::from_file`] memory-maps compiled archives. [`Store::from_json`] and
//! [`Store::from_gz_bytes`] compile ad-hoc schema sources into process-owned
//! archive bytes for consumers such as language servers. Compiled archives are generated runtime
//! artifacts, not a stable interchange format between arbitrary library versions. They remain
//! uncompressed so memory-mapped pages can be shared by the operating system. The schema data is
//! immutable; only derived regular expressions are cached per process.
//!
//! Schema defaults remain borrowed metadata and are never inserted into validated input data.
//! Concrete dynamic keys depend on input data, so [`resolve_dynamic_keys`] resolves them per
//! operation instead of storing them in the archive.
// TODO: Reevaluate the allow
#![allow(
    clippy::empty_structs_with_brackets,
    clippy::empty_enum_variants_with_brackets,
    clippy::iter_over_hash_type,
    clippy::impl_trait_in_params,
    clippy::needless_pass_by_value,
    clippy::module_name_repetitions,
    clippy::multiple_inherent_impl,
    clippy::partial_pub_fields,
    clippy::pub_underscore_fields,
    clippy::redundant_type_annotations,
    clippy::used_underscore_binding,
    clippy::unwrap_used,
    clippy::tests_outside_test_module,
    reason = "Existing schema models and feature-gated shared test helpers predate workspace lint inheritance"
)]
#![deny(unused_crate_dependencies)]

// Enable the targeted Unicode Perl tables used by fancy-regex without enabling
// its broad Unicode property tables.
use regex_automata as _;
#[cfg(test)]
use test_schema_store as _;

mod compiled;
#[cfg(feature = "generation")]
mod generation;
#[allow(
    missing_docs,
    reason = "Legacy schema inheritance API predates missing-doc enforcement."
)]
mod inherit;
mod navigation;
#[allow(
    missing_docs,
    reason = "Legacy schema resolution API predates missing-doc enforcement."
)]
mod resolve;
#[allow(
    missing_docs,
    reason = "Legacy source schema model API predates missing-doc enforcement."
)]
mod schema;
#[allow(
    missing_docs,
    reason = "Legacy schema source loading API predates missing-doc enforcement."
)]
mod source_store;
mod store;
#[allow(
    missing_docs,
    reason = "Legacy schema serialization helpers predate missing-doc enforcement."
)]
mod utils;
mod views;

pub use self::compiled::CompileError;
pub use self::compiled::SchemaDiagnostic;
pub use self::compiled::SchemaDiagnostics;
pub use self::compiled::SchemaId;
#[cfg(feature = "generation")]
pub use self::generation::DocumentationGenerationError;
#[cfg(feature = "generation")]
pub use self::generation::GenerationError;
#[cfg(feature = "generation")]
pub use self::generation::generate_markdown_documentation;
#[cfg(feature = "generation")]
pub use self::generation::generate_python_models;
#[cfg(feature = "generation")]
pub use self::generation::generate_python_models_projection;
pub use self::inherit::Inherit;
pub use self::navigation::SchemaPathError;
pub use self::navigation::resolve_dynamic_keys;
pub use self::resolve::errors::SchemaResolverError;
pub use self::resolve::walker::SchemaWalkError;
pub use self::schema::any;
pub use self::schema::base;
pub use self::schema::boolean;
pub use self::schema::dict;
pub use self::schema::dict::DynamicKeyOverrides;
pub use self::schema::int;
pub use self::schema::list;
pub use self::schema::str;
pub use self::source_store::SchemaStoreError;
pub use self::source_store::StoreSource;
pub use self::store::Store;
pub use self::store::StoreError;
pub use self::utils::dump::Dump;
pub use self::utils::dump::DumpError;
pub use self::utils::load::Load;
pub use self::utils::load::LoadError;
#[cfg(feature = "dump_load_files")]
pub use self::utils::load::LoadFromFragments;
pub use self::utils::schema_data::SchemaDataMapping;
pub use self::utils::schema_data::SchemaDataSequence;
pub use self::utils::schema_data::SchemaDataValue;
pub use self::views::BoolView;
pub use self::views::CommonView;
pub use self::views::DeprecationView;
pub use self::views::DictView;
pub use self::views::DocumentationOptionsView;
pub use self::views::IntView;
pub use self::views::ListView;
pub use self::views::SchemaListValueView;
pub use self::views::SchemaObjectValueView;
pub use self::views::SchemaValueView;
pub use self::views::SchemaView;
pub use self::views::StrView;
pub use self::views::StringFormatView;
