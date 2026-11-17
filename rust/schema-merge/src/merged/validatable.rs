// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;

use validation::MappingDuplicateKey;
use validation::ValidatableMapping;
use validation::ValidatableMappingPair;
use validation::ValidatableSequence;
use validation::ValidatableValue;
use validation::feedback::SourceSpan;
use validation::feedback::Type;
use validation::feedback::Value as FeedbackValue;

use super::Entry;
use super::Item;
use super::MappingData;
use super::MergeBackend;
use super::MergedValue;
use super::Repr;
use super::SequenceData;
use super::View;
use super::schema::DataMapping;

impl<'input, V: MergeBackend> ValidatableValue for MergedValue<'input, V> {
    type View<'a>
        = View<'a, 'input, V>
    where
        Self: 'a;
    type Mapping<'a>
        = Mapping<'a, 'input, V>
    where
        Self: 'a;
    type Sequence<'a>
        = Sequence<'a, 'input, V>
    where
        Self: 'a;
    type Coerced = Self;
    type CoercedMappingItem = Entry<'input, V>;

    fn is_null(&self) -> bool {
        self.original().is_null()
    }
    fn is_str(&self) -> bool {
        self.original().is_str()
    }
    fn is_int(&self) -> bool {
        self.original().is_int()
    }
    fn is_bool(&self) -> bool {
        self.original().is_bool()
    }
    fn is_float(&self) -> bool {
        self.original().is_float()
    }
    fn as_str(&self) -> Option<Cow<'_, str>> {
        self.original().as_str()
    }
    fn as_i64(&self) -> Option<i64> {
        self.original().as_i64()
    }
    fn as_bool(&self) -> Option<bool> {
        self.original().as_bool()
    }
    fn value_type(&self) -> Type {
        self.original().value_type()
    }
    fn source(&self) -> Option<&Arc<str>> {
        self.original().source()
    }
    fn source_span(&self) -> Option<SourceSpan> {
        self.original().source_span()
    }

    fn as_mapping(&self) -> Option<Self::Mapping<'_>> {
        match &self.inner {
            Repr::Borrowed(value) if value.is_mapping() => Some(Mapping::Original(value)),
            Repr::Mapping(data) => Some(Mapping::Expanded(&data.entries)),
            _ => None,
        }
    }
    fn as_sequence(&self) -> Option<Self::Sequence<'_>> {
        match &self.inner {
            Repr::Borrowed(value) if value.is_sequence() => Some(Sequence::Original(value)),
            Repr::Sequence(data) => Some(Sequence::Expanded(&data.items)),
            _ => None,
        }
    }
    fn get(&self, key: &str) -> Option<Self::View<'_>> {
        self.as_mapping()?.get(key)
    }

    fn to_feedback_value(&self) -> FeedbackValue {
        match &self.inner {
            Repr::Borrowed(value) => value.to_feedback_value(),
            Repr::Scalar(value) => value.to_feedback_value(),
            Repr::Mapping(data) => FeedbackValue::Dict(
                data.entries
                    .iter()
                    .filter_map(|entry| {
                        V::feedback_key(entry.key)
                            .map(|key| (key.into_owned(), entry.value.to_feedback_value()))
                    })
                    .collect(),
            ),
            Repr::Sequence(data) => FeedbackValue::List(
                data.items
                    .iter()
                    .map(|item| item.value.to_feedback_value())
                    .collect(),
            ),
        }
    }

    fn coerce_null(&self) -> Self::Coerced {
        if self.is_null() {
            self.clone()
        } else {
            Self {
                inner: Repr::Scalar(Arc::new(self.original().make_null())),
            }
        }
    }
    fn coerce_bool(&self, value: bool) -> Self::Coerced {
        if self.is_bool() && self.as_bool() == Some(value) {
            self.clone()
        } else {
            Self {
                inner: Repr::Scalar(Arc::new(self.original().make_bool(value))),
            }
        }
    }
    fn coerce_int(&self, value: i64) -> Self::Coerced {
        if self.is_int() && self.as_i64() == Some(value) {
            self.clone()
        } else {
            Self {
                inner: Repr::Scalar(Arc::new(self.original().make_int(value))),
            }
        }
    }
    fn coerce_str(&self, value: Cow<'_, str>) -> Self::Coerced {
        if self.is_str() && self.as_str().as_deref() == Some(value.as_ref()) {
            self.clone()
        } else {
            Self {
                inner: Repr::Scalar(Arc::new(self.original().make_str(value.into_owned()))),
            }
        }
    }
    fn coerce_sequence(&self, items: Vec<Self::Coerced>) -> Self::Coerced {
        let mut metadata = self.item_metadata();
        Self {
            inner: Repr::Sequence(Arc::new(SequenceData {
                original: self.sequence_origin(),
                items: items
                    .into_iter()
                    .map(|value| Item {
                        original: metadata.next().flatten(),
                        value,
                    })
                    .collect(),
            })),
        }
    }
    fn coerce_mapping(&self, items: Vec<Self::CoercedMappingItem>) -> Self::Coerced {
        Self {
            inner: Repr::Mapping(Arc::new(MappingData {
                original: self.mapping_origin(),
                entries: items,
            })),
        }
    }
    fn clone_to_coerced(&self) -> Self::Coerced {
        self.clone()
    }
}

/// Validation mapping over original or expanded storage.
#[derive(Debug, Clone)]
pub enum Mapping<'a, 'input, V: MergeBackend> {
    /// Original fragment mapping.
    Original(&'input V),
    /// Expanded mapping entries.
    Expanded(&'a [Entry<'input, V>]),
}
impl<V: MergeBackend> Copy for Mapping<'_, '_, V> {}

/// Iterator over validation mapping pairs.
pub enum MappingIter<'a, 'input, V: MergeBackend + 'input> {
    /// Original entries.
    Original(V::Entries<'input>),
    /// Expanded entries.
    Expanded(std::slice::Iter<'a, Entry<'input, V>>),
}
impl<V: MergeBackend> std::fmt::Debug for MappingIter<'_, '_, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("MappingIter")
    }
}

/// Validation mapping pair retaining key provenance independently of its value.
#[derive(Debug)]
pub enum Pair<'a, 'input, V: MergeBackend> {
    /// Original key and mapped value.
    Original(&'input V::Key, &'input V),
    /// Expanded pair.
    Expanded(&'a Entry<'input, V>),
}
impl<'a, 'input: 'a, V: MergeBackend> Pair<'a, 'input, V> {
    pub(super) fn key(&self) -> &'input V::Key {
        match self {
            Self::Original(key, _) => key,
            Self::Expanded(entry) => entry.key,
        }
    }
}
impl<'a, 'input: 'a, V: MergeBackend> Iterator for MappingIter<'a, 'input, V> {
    type Item = Pair<'a, 'input, V>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Original(iter) => iter.next().map(|(key, value)| Pair::Original(key, value)),
            Self::Expanded(iter) => iter.next().map(Pair::Expanded),
        }
    }
}

impl<'a, 'input: 'a, V: MergeBackend + 'input> ValidatableMapping<'a> for Mapping<'a, 'input, V> {
    type Value = MergedValue<'input, V>;
    type SchemaDataMapping<'s>
        = DataMapping<'s, 'input, V>
    where
        Self: 's;
    type Pair = Pair<'a, 'input, V>;
    type Iter = MappingIter<'a, 'input, V>;

    fn get(&self, key: &str) -> Option<View<'a, 'input, V>> {
        self.iter()
            .find_map(|pair| (V::key_name(pair.key()) == Some(key)).then(|| pair.value()))
    }
    fn duplicate_keys(&self) -> Vec<MappingDuplicateKey<'a>> {
        let mut keys: Vec<MappingDuplicateKey<'a>> = Vec::new();
        let mut seen = HashMap::new();
        for pair in self.iter() {
            let Some(key) = V::key_name(pair.key()) else {
                continue;
            };
            let index = *seen.entry(key).or_insert_with(|| {
                keys.push(MappingDuplicateKey {
                    key,
                    locations: Vec::new(),
                });
                keys.len() - 1
            });
            if let Some(duplicate) = keys.get_mut(index) {
                duplicate
                    .locations
                    .push((V::key_source(pair.key()), V::key_span(pair.key())));
            }
        }
        keys.retain(|key| key.locations.len() > 1);
        keys
    }
    fn as_schema_data_mapping(&self) -> Self::SchemaDataMapping<'_> {
        DataMapping::new(*self)
    }
    fn contains_key(&self, key: &str) -> bool {
        self.get(key).is_some()
    }
    fn iter(&self) -> Self::Iter {
        match self {
            Self::Original(value) => MappingIter::Original(value.entries()),
            Self::Expanded(entries) => MappingIter::Expanded(entries.iter()),
        }
    }
    fn len(&self) -> usize {
        match self {
            Self::Original(value) => value.as_mapping().map_or(0, |mapping| mapping.len()),
            Self::Expanded(entries) => entries.len(),
        }
    }
}

impl<'a, 'input: 'a, V: MergeBackend + 'input> ValidatableMappingPair<'a> for Pair<'a, 'input, V> {
    type Value = MergedValue<'input, V>;
    fn schema_key(&self) -> Option<Cow<'a, str>> {
        V::key_name(self.key()).map(Cow::Borrowed)
    }
    fn display_key(&self) -> Cow<'a, str> {
        V::display_key(self.key())
    }
    fn value(&self) -> View<'a, 'input, V> {
        match self {
            Self::Original(_, value) => View::Original(MergedValue::new(value)),
            Self::Expanded(entry) => View::Borrowed(&entry.value),
        }
    }
    fn coerced_item(&self, value: Self::Value) -> Entry<'input, V> {
        let original_value = match self {
            Self::Original(_, original_value) => original_value,
            Self::Expanded(entry) => entry.original_value,
        };
        Entry {
            key: self.key(),
            original_value,
            value,
        }
    }
    fn key_source(&self) -> Option<&str> {
        V::key_source(self.key())
    }
    fn key_span(&self) -> Option<SourceSpan> {
        V::key_span(self.key())
    }
}

/// Validation sequence over original or expanded storage.
#[derive(Debug, Clone)]
pub enum Sequence<'a, 'input, V: MergeBackend> {
    /// Original sequence.
    Original(&'input V),
    /// Expanded items retaining original sequence metadata.
    Expanded(&'a [Item<'input, V>]),
}
impl<V: MergeBackend> Copy for Sequence<'_, '_, V> {}

/// Iterator over validation sequence values.
pub enum SequenceIter<'a, 'input, V: MergeBackend + 'input> {
    /// Original sequence items.
    Original(V::Elements<'input>),
    /// Expanded sequence items.
    Expanded(std::slice::Iter<'a, Item<'input, V>>),
}
impl<V: MergeBackend> std::fmt::Debug for SequenceIter<'_, '_, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("SequenceIter")
    }
}
impl<'a, 'input: 'a, V: MergeBackend> Iterator for SequenceIter<'a, 'input, V> {
    type Item = View<'a, 'input, V>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Original(iter) => iter
                .next()
                .map(|(_, value)| View::Original(MergedValue::new(value))),
            Self::Expanded(iter) => iter.next().map(|item| View::Borrowed(&item.value)),
        }
    }
}
impl<'a, 'input: 'a, V: MergeBackend + 'input> ValidatableSequence<'a> for Sequence<'a, 'input, V> {
    type Value = MergedValue<'input, V>;
    type Iter = SequenceIter<'a, 'input, V>;
    fn iter(&self) -> Self::Iter {
        match self {
            Self::Original(value) => SequenceIter::Original(value.elements()),
            Self::Expanded(items) => SequenceIter::Expanded(items.iter()),
        }
    }
    fn len(&self) -> usize {
        match self {
            Self::Original(value) => value.as_sequence().map_or(0, |sequence| sequence.len()),
            Self::Expanded(items) => items.len(),
        }
    }
}
