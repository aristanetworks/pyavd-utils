// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Curated pyavd-utils API for the AVD language server.
//!
//! This crate is the language server's sole dependency and feature boundary
//! for pyavd-utils. Each module re-exports only the individual functions,
//! traits, and types imported by the language server. It deliberately does not
//! re-export complete source crates or their module trees.
//!
//! # Feature contract
//!
//! The default feature set is empty and is the contract used by the browser
//! WASM build. It includes schema navigation and validation plus parsing of the
//! user's YAML documents through [`yaml`].
//!
//! The optional `gzip` feature is used only by native helpers and tests to load
//! a schema store from compressed JSON bytes with [`schema::Store::from_gz_bytes`]. It
//! does not enable schema loading from the filesystem, YAML schema
//! deserialization, or directory traversal. Those are `avdschema` authoring
//! and CLI capabilities, not language-server runtime capabilities.
//!
//! Changes to these exports or features should be driven by an import in the
//! language server and covered by the contract tests below.

#![deny(unused_crate_dependencies)]

/// Schema-store loading, navigation, and schema types used by the language
/// server.
#[allow(
    clippy::module_name_repetitions,
    reason = "The facade keeps the runtime view type names while grouping them by API domain."
)]
pub mod schema {
    pub use avdschema::BoolView;
    pub use avdschema::DictView;
    pub use avdschema::IntView;
    pub use avdschema::ListView;
    pub use avdschema::SchemaPathError;
    pub use avdschema::SchemaValueView;
    pub use avdschema::SchemaView;
    pub use avdschema::Store;
    pub use avdschema::StoreError;
    pub use avdschema::StrView;
    pub use avdschema::StringFormatView;
    pub use avdschema::dict::DynamicKeyOverrides;
}

/// Validation entry points, configuration, results, and diagnostics used by
/// the language server.
pub mod validation {
    pub use validation::Configuration;
    pub use validation::StoreValidate;
    pub use validation::feedback::ErrorIssue;
    pub use validation::feedback::Feedback;
    pub use validation::feedback::InfoIssue;
    pub use validation::feedback::ParseDiagnostic;
    pub use validation::feedback::ParseDiagnosticSource;
    pub use validation::feedback::Path;
    pub use validation::feedback::SourceSpan;
    pub use validation::feedback::WarningIssue;
}

/// YAML document parsing, AST, and event APIs used by the language server.
///
/// This module parses user documents. It is unrelated to loading schema files
/// in YAML format, which is intentionally outside this crate's feature graph.
pub mod yaml {
    pub use yaml_parser::CollectionStyle;
    pub use yaml_parser::Event;
    pub use yaml_parser::Integer;
    pub use yaml_parser::MappingPair;
    pub use yaml_parser::Node;
    pub use yaml_parser::Span;
    pub use yaml_parser::Value;
    pub use yaml_parser::emit_events;
    pub use yaml_parser::parse;
}

#[cfg(test)]
mod tests {
    use super::schema::BoolView;
    use super::schema::DictView;
    use super::schema::DynamicKeyOverrides;
    use super::schema::IntView;
    use super::schema::ListView;
    use super::schema::SchemaPathError;
    use super::schema::SchemaValueView;
    use super::schema::SchemaView;
    use super::schema::Store;
    use super::schema::StoreError;
    use super::schema::StrView;
    use super::schema::StringFormatView;
    use super::validation::Configuration;
    use super::validation::ErrorIssue;
    use super::validation::Feedback;
    use super::validation::InfoIssue;
    use super::validation::ParseDiagnostic;
    use super::validation::Path;
    use super::validation::SourceSpan;
    use super::validation::WarningIssue;
    use super::yaml::CollectionStyle;
    use super::yaml::Event;
    use super::yaml::Integer;
    use super::yaml::MappingPair;
    use super::yaml::Node;
    use super::yaml::Span;
    use super::yaml::Value;
    use super::yaml::emit_events;
    use super::yaml::parse;

    #[test]
    fn lsp_api_contract_is_available() {
        let store = Store::from_json("{}");
        let (documents, _) = parse("key: value\n");
        let _ = emit_events("key: value\n");

        if let (Ok(store), Some(document)) = (store, documents.first()) {
            let _ = store.get_schema_from_path("schema", &[], &document.value, None);
        }

        // Keep the exact imported LSP surface type-checked, including traits
        // and types whose constructors are not part of the consumer contract.
        let _: Option<BoolView<'_>> = None;
        let _: Option<DictView<'_>> = None;
        let _: Option<DynamicKeyOverrides> = None;
        let _: Option<IntView<'_>> = None;
        let _: Option<ListView<'_>> = None;
        let _: Option<SchemaPathError> = None;
        let _: Option<SchemaValueView<'_>> = None;
        let _: Option<SchemaView<'_>> = None;
        let _: Option<StoreError> = None;
        let _: Option<StrView<'_>> = None;
        let _: Option<StringFormatView> = None;
        let _: Option<Configuration> = None;
        let _: Option<Feedback<ErrorIssue>> = None;
        let _: Option<Feedback<InfoIssue>> = None;
        let _: Option<Feedback<WarningIssue>> = None;
        let _: Option<ParseDiagnostic> = None;
        let _: Option<Path> = None;
        let _: Option<SourceSpan> = None;
        let _: Option<CollectionStyle> = None;
        let _: Option<Event<'_>> = None;
        let _: Option<Integer<'_>> = None;
        let _: Option<MappingPair<'_>> = None;
        let _: Option<Node<'_>> = None;
        let _: Option<Span> = None;
        let _: Option<Value<'_>> = None;
    }

    #[cfg(feature = "gzip")]
    #[test]
    fn gzip_contract_loads_schema_store_from_bytes() {
        const EMPTY_STORE_GZIP: &[u8] = &[
            0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0xab, 0xae, 0x05, 0x00,
            0x43, 0xbf, 0xa6, 0xa3, 0x02, 0x00, 0x00, 0x00,
        ];

        assert!(Store::from_gz_bytes(EMPTY_STORE_GZIP).is_ok());
    }
}
