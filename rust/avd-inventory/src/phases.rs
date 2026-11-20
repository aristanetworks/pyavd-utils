// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use avdschema::Store;
use indexmap::IndexMap;
use schema_merge::MergedValue;
use schema_merge::StoreSchema;
use validation::StoreValidate as _;
use validation::ValidationOutput;

use crate::BuildError;
use crate::BuildOptions;
use crate::InventoryFragment;
use crate::StoredFragment;
use crate::graph::MergeGraph;
use crate::graph::StepId;

#[derive(Debug)]
pub(crate) struct Inputs<F: InventoryFragment> {
    pub fragments: Vec<StoredFragment<F::Value>>,
    pub graph: MergeGraph,
}

#[derive(Debug)]
pub(crate) struct Merged<'input, F: InventoryFragment> {
    pub roots: IndexMap<StepId, MergedValue<'input, F::Value>>,
    pub devices: &'input IndexMap<String, StepId>,
}

#[derive(Debug)]
pub(crate) struct Validated<'input, F: InventoryFragment> {
    pub roots: IndexMap<StepId, ValidationOutput<MergedValue<'input, F::Value>>>,
    pub devices: &'input IndexMap<String, StepId>,
}

impl<F: InventoryFragment> Inputs<F> {
    #[allow(
        clippy::indexing_slicing,
        clippy::expect_used,
        reason = "The private append-only graph references earlier steps and valid fragment IDs; parent results remain until their last child"
    )]
    pub(crate) fn merge(
        &self,
        store: &Store,
        options: &BuildOptions,
    ) -> Result<Merged<'_, F>, BuildError> {
        let schema = StoreSchema::new(store, &options.schema_name).map_err(BuildError::Schema)?;
        let mut terminal = vec![false; self.graph.steps.len()];
        for head in self.graph.devices.values() {
            terminal[*head] = true;
        }
        let mut remaining: Vec<_> = self.graph.steps.iter().map(|step| step.children).collect();
        let mut results: Vec<Option<MergedValue<'_, F::Value>>> =
            Vec::with_capacity(self.graph.steps.len());
        for step in &self.graph.steps {
            let fragment = &self.fragments[step.fragment.0];
            let incoming = MergedValue::new(&fragment.value);
            let result = if let Some(parent) = step.parent {
                // Move the final non-terminal consumer's input rather than retaining a
                // spare Arc that would force a copy of every participating container.
                let previous = if remaining[parent] == 1 && !terminal[parent] {
                    results[parent].take()
                } else {
                    results[parent].clone()
                }
                .expect("parent result must exist before its children are merged");
                let merged = schema_merge::merge(previous, [incoming], &schema, options.merge)
                    .map_err(|error| BuildError::Merge {
                        source: fragment.source.clone(),
                        error,
                    })?;
                remaining[parent] -= 1;
                merged
            } else {
                incoming
            };
            results.push(Some(result));
        }
        let roots = results
            .into_iter()
            .enumerate()
            .filter_map(|(index, value)| {
                terminal[index].then(|| (index, value.expect("terminal result must be retained")))
            })
            .collect();
        Ok(Merged {
            roots,
            devices: &self.graph.devices,
        })
    }
}

impl<'input, F: InventoryFragment> Merged<'input, F> {
    pub(crate) fn validate(
        self,
        store: &Store,
        options: &BuildOptions,
    ) -> Result<Validated<'input, F>, BuildError> {
        let mut configuration = options.validation.clone();
        configuration.return_coerced_data = true;
        let roots = self
            .roots
            .into_iter()
            .map(|(id, value)| {
                let output = store
                    .validate_value(&value, &options.schema_name, Some(&configuration))
                    .map_err(BuildError::Validation)?;
                Ok((id, output))
            })
            .collect::<Result<_, BuildError>>()?;
        Ok(Validated {
            roots,
            devices: self.devices,
        })
    }
}
