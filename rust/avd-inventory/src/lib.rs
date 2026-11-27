// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Typed fragment collection with shared merging and validation.
//!
//! Adding a fragment decodes it once and extends the merge graph. Calling `merge_and_validate` consumes
//! the inventory, merges common fragment histories once, and validates each distinct
//! device root in parallel. Results own JSON output and diagnostics carrying original
//! sources and spans, independent of the parsed fragments.
//!
//! ```
//! use avd_inventory::{Inventory, ValidationOptions, YamlFragment};
//! use avdschema::Store;
//!
//! let schema = Store::from_json(r#"{"test":{"type":"dict","keys":{
//!     "mtu":{"type":"int"}
//! }}}"#).expect("schema should compile");
//! let mut inventory = Inventory::<YamlFragment>::new();
//! inventory.add_fragment(YamlFragment {
//!     yaml: "mtu: '1500'".into(),
//!     source: "fixture-a".into(),
//!     devices: vec!["leaf1".into(), "leaf2".into()],
//!     merge: Default::default(),
//! })?;
//! let output = inventory.merge_and_validate(&schema, &ValidationOptions::new("test"))?;
//! assert!(!output.has_errors());
//! assert_eq!(output.devices["leaf1"].validated_data.as_deref(), Some(r#"{"mtu":1500}"#));
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! Formats cannot be mixed:
//!
//! ```compile_fail,E0308
//! use avd_inventory::{Inventory, JsonFragment, YamlFragment};
//! let mut inventory = Inventory::<YamlFragment>::new();
//! inventory.add_fragment(JsonFragment {
//!     json: "{}".into(), source: "fixture".into(), devices: vec![],
//!     merge: Default::default(),
//! });
//! ```

mod errors;
mod fragments;
mod graph;
mod json;
mod phases;

use std::num::NonZeroUsize;
use std::sync::Arc;

use avdschema::Store;
pub use errors::AddFragmentError;
pub use errors::FragmentErrorKind;
pub use errors::ValidationError;
pub use fragments::InventoryFragment;
pub use fragments::JsonFragment;
pub use fragments::ParsedFragment;
pub use fragments::YamlFragment;
use graph::MergeGraph;
use indexmap::IndexMap;
use phases::Inputs;
use phases::Validated;
use schema_merge::MergeOptions;
use schema_merge::MergeableValue as _;
use schema_merge::MergedValue;
use validation::Configuration;
use validation::ValidationResult;

/// Identity of a fragment within one inventory, independent of its source identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FragmentId(usize);

/// Inventory-wide schema and validation settings.
#[derive(Debug, Clone)]
pub struct ValidationOptions {
    /// Schema selected from the supplied schema store.
    pub schema_name: String,
    /// Validation behavior. Coerced output is always enabled internally.
    pub validation: Configuration,
    /// Maximum validation workers for this operation. `None` uses available parallelism.
    ///
    /// Set this to the Python service's validation worker limit. A value of one
    /// runs validation on the calling thread without creating a pool.
    pub validation_threads: Option<NonZeroUsize>,
}

impl ValidationOptions {
    /// Select a schema with default validation behavior and available parallelism.
    pub fn new(schema_name: &str) -> Self {
        Self {
            schema_name: schema_name.to_owned(),
            validation: Configuration::default(),
            validation_threads: None,
        }
    }
}

/// Owned merge and validation results, safe to return after parsed inputs are dropped.
#[derive(Debug)]
pub struct ValidationOutput {
    /// Per-device results. Devices with identical fragment histories share one result.
    pub devices: IndexMap<String, Arc<DeviceOutput>>,
}

impl ValidationOutput {
    /// Whether any device has input validation errors.
    ///
    /// Callers must gate their common build stage on this inventory-wide result.
    pub fn has_errors(&self) -> bool {
        self.devices
            .values()
            .any(|output| !output.validation.errors.is_empty())
    }
}

/// Validation feedback and JSON for one distinct merged device root.
#[derive(Debug)]
pub struct DeviceOutput {
    /// Errors, warnings and infos with original fragment sources and spans where available.
    pub validation: ValidationResult,
    /// Coerced JSON, present only when this root has no validation errors.
    pub validated_data: Option<String>,
}

/// A collecting inventory accepting only one fragment format.
///
/// `merge_and_validate` consumes the inventory. Merge and validation phases remain private;
/// no self-referential owner is needed because the parsed inputs stay local to `merge_and_validate`.
#[derive(Debug)]
pub struct Inventory<F: InventoryFragment> {
    inputs: Inputs<F>,
}

impl<F: InventoryFragment> Default for Inventory<F> {
    fn default() -> Self {
        Self::new()
    }
}

impl<F: InventoryFragment> Inventory<F> {
    /// Create an empty inventory.
    pub fn new() -> Self {
        Self {
            inputs: Inputs {
                fragments: Vec::new(),
                graph: MergeGraph::default(),
            },
        }
    }

    /// Decode and collect a fragment, preserving insertion order per device.
    ///
    /// Duplicate device names in one fragment count once. An empty device list
    /// applies to no devices. Repeated source identifiers are allowed. Rejected
    /// input leaves the inventory unchanged; recovered YAML trees are not accepted.
    pub fn add_fragment(&mut self, fragment: F) -> Result<FragmentId, AddFragmentError> {
        let parsed = fragment.decode()?;
        if !parsed.value.is_mapping() {
            return Err(AddFragmentError {
                source: parsed.source,
                kind: FragmentErrorKind::RootNotMapping,
            });
        }
        let id = FragmentId(self.inputs.fragments.len());
        self.inputs.graph.add(id, &parsed.devices);
        self.inputs.fragments.push(StoredFragment {
            value: parsed.value,
            source: parsed.source,
            merge: parsed.merge,
        });
        Ok(id)
    }

    /// Consume the inventory, merge and validate device variables, and encode JSON.
    ///
    /// Distinct roots are validated and encoded in parallel with a dedicated
    /// worker pool. Findings remain structured output; schema, execution and JSON
    /// encoding failures are execution errors. Call [`ValidationOutput::has_errors`] before
    /// continuing with a common build stage. No archive is published.
    /// Inputs cannot be added after this call because it consumes `self`.
    ///
    /// ```compile_fail,E0382
    /// # use avd_inventory::{Inventory, ValidationOptions, YamlFragment};
    /// # use avdschema::Store;
    /// # fn example(store: &Store, fragment: YamlFragment) {
    /// let mut inventory = Inventory::<YamlFragment>::new();
    /// let _ = inventory.merge_and_validate(store, &ValidationOptions::new("test"));
    /// inventory.add_fragment(fragment);
    /// # }
    /// ```
    pub fn merge_and_validate(
        self,
        store: &Store,
        options: &ValidationOptions,
    ) -> Result<ValidationOutput, ValidationError>
    where
        for<'a> MergedValue<'a, F::Value>: Send + Sync,
    {
        let inputs = self.seal();
        let merged = inputs.merge(store, options)?;
        let Validated { roots, devices } = merged.validate(store, options)?;
        let devices = devices
            .iter()
            .map(|(device, root)| {
                // Every device head is retained as a terminal root by the private merge graph.
                #[allow(
                    clippy::indexing_slicing,
                    reason = "Every device head is a retained terminal root"
                )]
                (device.clone(), Arc::clone(&roots[root]))
            })
            .collect();
        Ok(ValidationOutput { devices })
    }

    fn seal(self) -> Inputs<F> {
        self.inputs
    }
}

#[derive(Debug)]
pub(crate) struct StoredFragment<V> {
    value: V,
    source: Arc<str>,
    merge: MergeOptions,
}

#[cfg(test)]
mod tests;
