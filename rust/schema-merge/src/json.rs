// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde_json::Value;

use crate::MergeableEntry;
use crate::MergeableItem;
use crate::MergeableValue;
use crate::SemanticValue;

impl MergeableValue for Value {
    type Entry = (String, Self);
    type Item = Self;
    type Identity = SemanticValue;

    fn take_mapping(&mut self) -> Option<Vec<Self::Entry>> {
        self.as_object_mut()
            .map(|mapping| std::mem::take(mapping).into_iter().collect())
    }

    fn set_mapping(&mut self, entries: Vec<Self::Entry>) {
        *self = Self::Object(entries.into_iter().collect());
    }

    fn take_sequence(&mut self) -> Option<Vec<Self::Item>> {
        self.as_array_mut().map(std::mem::take)
    }

    fn set_sequence(&mut self, items: Vec<Self::Item>) {
        *self = Self::Array(items);
    }

    fn get(&self, key: &str) -> Option<&Self> {
        self.get(key)
    }

    fn identity(&self) -> Self::Identity {
        self.into()
    }
}

impl MergeableEntry for (String, Value) {
    type Value = Value;
    type Key = String;

    fn key(&self) -> Self::Key {
        self.0.clone()
    }

    fn key_name(&self) -> Option<&str> {
        Some(&self.0)
    }

    fn value(&self) -> &Self::Value {
        &self.1
    }

    fn value_mut(&mut self) -> &mut Self::Value {
        &mut self.1
    }

    fn take_value(&mut self) -> Self::Value {
        self.1.take()
    }
}

impl MergeableItem for Value {
    type Value = Self;

    fn value(&self) -> &Self::Value {
        self
    }

    fn value_mut(&mut self) -> &mut Self::Value {
        self
    }

    fn into_value(self) -> Self::Value {
        self
    }
}
