// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use yaml_parser::MappingPair;
use yaml_parser::Node;
use yaml_parser::SequenceItem;
use yaml_parser::Span;
use yaml_parser::Value;

use crate::MergeableEntry;
use crate::MergeableItem;
use crate::MergeableValue;
use crate::SemanticValue;

impl<'input> MergeableValue for Node<'input> {
    type Entry = MappingPair<'input>;
    type Item = SequenceItem<'input>;
    type Identity = SemanticValue;

    fn take_mapping(&mut self) -> Option<Vec<Self::Entry>> {
        match &mut self.value {
            Value::Mapping(pairs) => Some(std::mem::take(pairs)),
            _ => None,
        }
    }

    fn set_mapping(&mut self, entries: Vec<Self::Entry>) {
        self.value = Value::Mapping(entries);
    }

    fn take_sequence(&mut self) -> Option<Vec<Self::Item>> {
        match &mut self.value {
            Value::Sequence(items) => Some(std::mem::take(items)),
            _ => None,
        }
    }

    fn set_sequence(&mut self, items: Vec<Self::Item>) {
        self.value = Value::Sequence(items);
    }

    fn get(&self, key: &str) -> Option<&Self> {
        match &self.value {
            Value::Mapping(pairs) => pairs.iter().find_map(|pair| {
                matches!(&pair.key.value, Value::String(name) if name == key).then_some(&pair.value)
            }),
            _ => None,
        }
    }

    fn identity(&self) -> Self::Identity {
        self.into()
    }
}

impl<'input> MergeableEntry for MappingPair<'input> {
    type Value = Node<'input>;
    type Key = SemanticValue;

    fn key(&self) -> Self::Key {
        (&self.key).into()
    }

    fn key_name(&self) -> Option<&str> {
        match &self.key.value {
            Value::String(name) => Some(name),
            _ => None,
        }
    }

    fn value(&self) -> &Self::Value {
        &self.value
    }

    fn value_mut(&mut self) -> &mut Self::Value {
        &mut self.value
    }

    fn take_value(&mut self) -> Self::Value {
        std::mem::replace(&mut self.value, Node::null(Span::default(), None))
    }
}

impl<'input> MergeableItem for SequenceItem<'input> {
    type Value = Node<'input>;

    fn value(&self) -> &Self::Value {
        &self.node
    }

    fn value_mut(&mut self) -> &mut Self::Value {
        &mut self.node
    }

    fn into_value(self) -> Self::Value {
        self.node
    }
}
