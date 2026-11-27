// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::collections::HashMap;
use std::collections::HashSet;

use indexmap::IndexMap;

use crate::FragmentId;

pub(crate) type StepId = usize;

#[derive(Debug)]
pub(crate) struct Step {
    pub parent: Option<StepId>,
    pub fragment: FragmentId,
    pub children: usize,
}

#[derive(Debug, Default)]
pub(crate) struct MergeGraph {
    pub steps: Vec<Step>,
    pub devices: IndexMap<String, StepId>,
}

impl MergeGraph {
    #[allow(
        clippy::indexing_slicing,
        reason = "Private device heads only reference steps previously inserted into this graph"
    )]
    pub(crate) fn add(&mut self, fragment: FragmentId, devices: &[String]) {
        let mut seen = HashSet::new();
        let mut successors = HashMap::new();
        for device in devices {
            if !seen.insert(device.as_str()) {
                continue;
            }
            let parent = self.devices.get(device).copied();
            let next = *successors.entry(parent).or_insert_with(|| {
                let next = self.steps.len();
                self.steps.push(Step {
                    parent,
                    fragment,
                    children: 0,
                });
                if let Some(parent) = parent {
                    self.steps[parent].children += 1;
                }
                next
            });
            if let Some(head) = self.devices.get_mut(device) {
                *head = next;
            } else {
                self.devices.insert(device.clone(), next);
            }
        }
    }
}
