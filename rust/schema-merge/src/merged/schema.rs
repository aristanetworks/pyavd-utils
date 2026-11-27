// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use schema_data::SchemaDataMapping;
use schema_data::SchemaDataSequence;
use schema_data::SchemaDataValue;
use validation::ValidatableMapping as _;
use validation::ValidatableSequence as _;
use validation::ValidatableValue as _;

use super::MergeBackend;
use super::MergedValue;
use super::validatable::Mapping;
use super::validatable::MappingIter;
use super::validatable::Pair;
use super::validatable::Sequence;
use super::validatable::SequenceIter;

/// Copyable schema-navigation value over original or expanded storage.
#[derive(Debug, Clone)]
pub enum DataValue<'a, 'input, V: MergeBackend> {
    /// Original fragment value.
    Original(&'input V),
    /// Existing merged value.
    Merged(&'a MergedValue<'input, V>),
}
impl<V: MergeBackend> Copy for DataValue<'_, '_, V> {}

/// Schema-navigation mapping adapter.
#[derive(Debug, Clone)]
pub struct DataMapping<'a, 'input, V: MergeBackend>(Mapping<'a, 'input, V>);
impl<V: MergeBackend> Copy for DataMapping<'_, '_, V> {}
impl<'a, 'input, V: MergeBackend> DataMapping<'a, 'input, V> {
    pub(super) fn new(mapping: Mapping<'a, 'input, V>) -> Self {
        Self(mapping)
    }
}

/// Schema-navigation sequence adapter.
#[derive(Debug)]
pub struct DataSequence<'a, 'input, V: MergeBackend>(Sequence<'a, 'input, V>);

/// Iterator over string schema keys.
#[derive(Debug)]
pub struct DataKeys<'a, 'input, V: MergeBackend + 'input>(MappingIter<'a, 'input, V>);
impl<'a, 'input: 'a, V: MergeBackend> Iterator for DataKeys<'a, 'input, V> {
    type Item = Cow<'a, str>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .find_map(|pair| V::key_name(pair.key()).map(Cow::Borrowed))
    }
}

/// Iterator over copyable schema-navigation list values.
#[derive(Debug)]
pub struct DataIter<'a, 'input, V: MergeBackend + 'input>(SequenceIter<'a, 'input, V>);
impl<'a, 'input: 'a, V: MergeBackend> Iterator for DataIter<'a, 'input, V> {
    type Item = DataValue<'a, 'input, V>;
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            SequenceIter::Original(iter) => {
                iter.next().map(|(_, value)| DataValue::Original(value))
            }
            SequenceIter::Expanded(iter) => iter.next().map(|item| DataValue::Merged(&item.value)),
        }
    }
}

impl<'a, 'input: 'a, V: MergeBackend + 'input> SchemaDataValue<'a> for DataValue<'a, 'input, V> {
    type Mapping = DataMapping<'a, 'input, V>;
    type Sequence = DataSequence<'a, 'input, V>;
    fn as_mapping(self) -> Option<Self::Mapping> {
        match self {
            Self::Original(value) if crate::MergeableValue::is_mapping(value) => {
                Some(DataMapping(Mapping::Original(value)))
            }
            Self::Merged(value) => value.as_mapping().map(DataMapping),
            Self::Original(_) => None,
        }
    }
    fn as_sequence(self) -> Option<Self::Sequence> {
        match self {
            Self::Original(value) if crate::MergeableValue::is_sequence(value) => {
                Some(DataSequence(Sequence::Original(value)))
            }
            Self::Merged(value) => value.as_sequence().map(DataSequence),
            Self::Original(_) => None,
        }
    }
    fn as_str(self) -> Option<&'a str> {
        match self {
            Self::Original(value) => value.borrowed_str(),
            Self::Merged(value) => value.original().borrowed_str(),
        }
    }
}
impl<'a, 'input: 'a, V: MergeBackend + 'input> SchemaDataMapping<'a>
    for DataMapping<'a, 'input, V>
{
    type Value = DataValue<'a, 'input, V>;
    type Keys = DataKeys<'a, 'input, V>;
    fn get(&self, key: &str) -> Option<Self::Value> {
        if let Mapping::Original(value) = self.0 {
            return value.get_child(key).map(DataValue::Original);
        }
        self.0.iter().find_map(|pair| {
            if V::key_name(pair.key()) != Some(key) {
                return None;
            }
            Some(match pair {
                Pair::Original(_, value) => DataValue::Original(value),
                Pair::Expanded(entry) => DataValue::Merged(&entry.value),
            })
        })
    }
    fn keys(&self) -> Self::Keys {
        DataKeys(self.0.iter())
    }
}
impl<'a, 'input: 'a, V: MergeBackend + 'input> SchemaDataSequence<'a>
    for DataSequence<'a, 'input, V>
{
    type Value = DataValue<'a, 'input, V>;
    type Iter = DataIter<'a, 'input, V>;
    fn iter(&self) -> Self::Iter {
        DataIter(self.0.iter())
    }
}
