// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Typed fragment collection with shared merging and validation.
//!
//! Adding a fragment decodes it once and extends the merge graph. Building consumes
//! the inventory, merges common fragment histories once, and validates each distinct
//! device root. The current build discards the private coerced roots and feedback;
//! it does not publish or return inventory data.
//!
//! ```
//! use avd_inventory::{BuildOptions, Inventory, YamlFragment};
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
//! })?;
//! inventory.build(&schema, &BuildOptions::new("test"))?;
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
//! });
//! ```

mod errors;
mod fragments;
mod graph;
mod phases;

use std::sync::Arc;

use avdschema::Store;
use schema_merge::MergeOptions;
use schema_merge::MergeableValue as _;
use validation::Configuration;

pub use errors::AddFragmentError;
pub use errors::BuildError;
pub use errors::FragmentErrorKind;
pub use fragments::InventoryFragment;
pub use fragments::JsonFragment;
pub use fragments::ParsedFragment;
pub use fragments::YamlFragment;
use graph::MergeGraph;
use phases::Inputs;
use phases::Validated;

/// Identity of a fragment within one inventory, independent of its source identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FragmentId(usize);

/// Build-wide schema, merge, and validation settings.
#[derive(Debug, Clone)]
pub struct BuildOptions {
    /// Schema selected from the supplied schema store.
    pub schema_name: String,
    /// Uniform merge behavior for all devices and shared prefixes.
    pub merge: MergeOptions,
    /// Validation behavior. Coerced output is always enabled internally.
    pub validation: Configuration,
}

impl BuildOptions {
    /// Select a schema with default merge and validation behavior.
    pub fn new(schema_name: &str) -> Self {
        Self {
            schema_name: schema_name.to_owned(),
            merge: MergeOptions::default(),
            validation: Configuration::default(),
        }
    }
}

/// A collecting inventory accepting only one fragment format.
///
/// `build` consumes the inventory. Merge and validation phases remain private;
/// no self-referential owner is needed because the parsed inputs stay local to build.
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
        });
        Ok(id)
    }

    /// Consume the inventory, merge device variables, and run validation/coercion.
    ///
    /// This initial entrypoint discards both coerced data and validation feedback
    /// and returns `Ok(())` when the phases complete. Validation findings do not
    /// become build errors. Publication and returning the product are separate work.
    /// Inputs cannot be added after this call because it consumes `self`.
    ///
    /// ```compile_fail,E0382
    /// # use avd_inventory::{BuildOptions, Inventory, YamlFragment};
    /// # use avdschema::Store;
    /// # fn example(store: &Store, fragment: YamlFragment) {
    /// let mut inventory = Inventory::<YamlFragment>::new();
    /// let _ = inventory.build(store, &BuildOptions::new("test"));
    /// inventory.add_fragment(fragment);
    /// # }
    /// ```
    pub fn build(self, store: &Store, options: &BuildOptions) -> Result<(), BuildError> {
        let inputs = self.seal();
        let merged = inputs.merge(store, options)?;
        let Validated { roots, devices } = merged.validate(store, options)?;
        // The owned parsed inputs remain alive until both products have been dropped.
        drop((roots, devices));
        Ok(())
    }

    fn seal(self) -> Inputs<F> {
        self.inputs
    }
}

#[derive(Debug)]
pub(crate) struct StoredFragment<V> {
    value: V,
    source: Arc<str>,
}

#[cfg(test)]
mod tests;
