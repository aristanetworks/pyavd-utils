// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::sync::Arc;

use schema_merge::MergeBackend;
use serde_json::Value;
use yaml_parser::Node;

use crate::AddFragmentError;
use crate::FragmentErrorKind;

/// YAML-encoded variables and the devices to which they apply.
#[derive(Debug)]
pub struct YamlFragment {
    /// A single YAML mapping document.
    pub yaml: String,
    /// Opaque source identifier used in diagnostics; it need not be unique.
    pub source: Arc<str>,
    /// Devices receiving this fragment, after their earlier fragments.
    pub devices: Vec<String>,
}

/// JSON-encoded variables and the devices to which they apply.
#[derive(Debug)]
pub struct JsonFragment {
    /// A JSON object.
    pub json: String,
    /// Opaque fragment source identifier. JSON values do not carry source spans.
    pub source: Arc<str>,
    /// Devices receiving this fragment, after their earlier fragments.
    pub devices: Vec<String>,
}

/// Decoded input to inventory collection.
///
/// The inventory retains the value and source, and records device membership in
/// its merge graph. Callers normally use `Inventory::add_fragment` directly.
#[derive(Debug)]
pub struct ParsedFragment<V: MergeBackend> {
    /// Decoded variables; inventory insertion requires a mapping root.
    pub value: V,
    /// Opaque source identifier for this fragment.
    pub source: Arc<str>,
    /// Devices receiving this fragment.
    pub devices: Vec<String>,
}

/// An input format and its corresponding merge backend.
///
/// An `Inventory<F>` accepts only `F`, keeping every fragment in the same backend.
pub trait InventoryFragment: Sized {
    /// Parsed value representation used by the merger and validator.
    type Value: MergeBackend;

    /// Decode this fragment. Inventory insertion calls this automatically.
    fn decode(self) -> Result<ParsedFragment<Self::Value>, AddFragmentError>;
}

impl InventoryFragment for YamlFragment {
    type Value = Node<'static>;

    fn decode(self) -> Result<ParsedFragment<Self::Value>, AddFragmentError> {
        let (mut documents, errors) =
            yaml_parser::parse_with_source(&self.yaml, Some(self.source.clone()));
        if !errors.is_empty() {
            return Err(AddFragmentError {
                source: self.source,
                kind: FragmentErrorKind::Yaml(errors),
            });
        }
        if documents.len() != 1 {
            return Err(AddFragmentError {
                source: self.source,
                kind: FragmentErrorKind::DocumentCount(documents.len()),
            });
        }
        // The document-count check establishes the index.
        let value = documents.remove(0);
        Ok(ParsedFragment {
            value,
            source: self.source,
            devices: self.devices,
        })
    }
}

impl InventoryFragment for JsonFragment {
    type Value = Value;

    fn decode(self) -> Result<ParsedFragment<Self::Value>, AddFragmentError> {
        let value = serde_json::from_str(&self.json).map_err(|error| AddFragmentError {
            source: self.source.clone(),
            kind: FragmentErrorKind::Json(error),
        })?;
        Ok(ParsedFragment {
            value,
            source: self.source,
            devices: self.devices,
        })
    }
}
