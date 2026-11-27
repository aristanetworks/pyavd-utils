// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::sync::Arc;

use avdschema::Store;
use indexmap::IndexMap;
use rayon::prelude::*;
use schema_merge::MergeInput;
use schema_merge::MergedValue;
use schema_merge::StoreSchema;
use validation::StoreValidate as _;

use crate::DeviceOutput;
use crate::InventoryFragment;
use crate::StoredFragment;
use crate::ValidationError;
use crate::ValidationOptions;
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
pub(crate) struct Validated<'input> {
    pub roots: IndexMap<StepId, Arc<DeviceOutput>>,
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
        options: &ValidationOptions,
    ) -> Result<Merged<'_, F>, ValidationError> {
        let schema =
            StoreSchema::new(store, &options.schema_name).map_err(ValidationError::Schema)?;
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
                let merged = schema_merge::merge_inputs(
                    previous,
                    [MergeInput {
                        value: incoming,
                        options: fragment.merge,
                    }],
                    &schema,
                )
                .map_err(|error| ValidationError::Merge {
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
        options: &ValidationOptions,
    ) -> Result<Validated<'input>, ValidationError>
    where
        MergedValue<'input, F::Value>: Send + Sync,
    {
        let mut configuration = options.validation.clone();
        configuration.return_coerced_data = true;
        let validate = |(id, value): (StepId, MergedValue<'input, F::Value>)| {
            let output = store
                .validate_value(&value, &options.schema_name, Some(&configuration))
                .map_err(ValidationError::Validation)?;
            let validated_data = if output.result.errors.is_empty() {
                output
                    .coerced
                    .as_ref()
                    .map(F::encode_json)
                    .transpose()
                    .map_err(ValidationError::Json)?
            } else {
                None
            };
            Ok((
                id,
                Arc::new(DeviceOutput {
                    validation: output.result,
                    validated_data,
                }),
            ))
        };
        // Collect into a Vec before parallel iteration so result order remains
        // deterministic without enabling IndexMap's optional Rayon support.
        let roots: Vec<_> = self.roots.into_iter().collect();
        let roots: Vec<_> = if options
            .validation_threads
            .is_some_and(|count| count.get() == 1)
            || roots.len() < 2
        {
            roots
                .into_iter()
                .map(validate)
                .collect::<Result<_, ValidationError>>()?
        } else {
            let workers = options
                .validation_threads
                .map_or_else(
                    || std::thread::available_parallelism().map_or(1, std::num::NonZeroUsize::get),
                    std::num::NonZeroUsize::get,
                )
                .min(roots.len());
            let pool = rayon::ThreadPoolBuilder::new()
                .num_threads(workers)
                .build()
                .map_err(ValidationError::WorkerPool)?;
            pool.install(|| {
                roots
                    .into_par_iter()
                    .map(validate)
                    .collect::<Result<_, ValidationError>>()
            })?
        };
        Ok(Validated {
            roots: roots.into_iter().collect(),
            devices: self.devices,
        })
    }
}
