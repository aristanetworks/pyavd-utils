// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Copy-on-write merge trees borrowing immutable fragment data.

mod backends;
mod schema;
pub use schema::DataIter;
pub use schema::DataKeys;
pub use schema::DataMapping;
pub use schema::DataSequence;
pub use schema::DataValue;
pub use validatable::Mapping;
pub use validatable::MappingIter;
pub use validatable::Pair;
pub use validatable::Sequence;
pub use validatable::SequenceIter;
mod validatable;

use std::borrow::Borrow;
use std::borrow::Cow;
use std::fmt::Debug;
use std::sync::Arc;

use validation::ValidatableValue;
use validation::feedback::SourceSpan;

use crate::MergeError;
use crate::MergeOptions;
use crate::MergeSchema;
use crate::MergeableEntry;
use crate::MergeableItem;
use crate::MergeableValue;
use crate::SemanticValue;

/// Immutable backend access needed to borrow keys, values and structural metadata.
///
/// Iterators borrow original storage. They must not clone or materialize subtrees.
/// Implementations are provided for JSON values and YAML nodes.
pub trait MergeBackend:
    MergeableValue<Identity = SemanticValue> + ValidatableValue + Debug
{
    /// Original mapping entry metadata (including the key).
    type Key: Debug + ?Sized;
    /// Original sequence item metadata.
    type Element: Debug;
    /// Borrowed mapping entries, in input order (including duplicate YAML keys).
    type Entries<'a>: Iterator<Item = (&'a Self::Key, &'a Self)>
    where
        Self: 'a;
    /// Borrowed sequence items, in input order.
    type Elements<'a>: Iterator<Item = (&'a Self::Element, &'a Self)>
    where
        Self: 'a;

    /// Borrow mapping entries; callers first check the value's type.
    fn entries(&self) -> Self::Entries<'_>;
    /// Borrow sequence items; callers first check the value's type.
    fn elements(&self) -> Self::Elements<'_>;
    /// String key used for schema lookups, if applicable.
    fn key_name(key: &Self::Key) -> Option<&str>;
    /// Human-readable diagnostic key.
    fn display_key(key: &Self::Key) -> Cow<'_, str>;
    /// Scalar key representation used in feedback, excluding unsupported key shapes.
    fn feedback_key(key: &Self::Key) -> Option<Cow<'_, str>> {
        Some(Self::display_key(key))
    }
    /// Metadata-free identity of a mapping key.
    fn key_identity(key: &Self::Key) -> SemanticValue;
    /// Original key source.
    fn key_source(key: &Self::Key) -> Option<&str>;
    /// Original key span.
    fn key_span(key: &Self::Key) -> Option<SourceSpan>;
    /// Create a null scalar retaining the original provenance.
    #[must_use]
    fn make_null(&self) -> Self;
    /// Create a boolean scalar retaining the original provenance.
    #[must_use]
    fn make_bool(&self, value: bool) -> Self;
    /// Create an integer scalar retaining the original provenance.
    #[must_use]
    fn make_int(&self, value: i64) -> Self;
    /// Create a string scalar retaining the original provenance.
    #[must_use]
    fn make_str(&self, value: String) -> Self;
    /// Create an empty mapping header retaining the original provenance.
    #[must_use]
    fn empty_mapping(&self) -> Self;
    /// Create an empty sequence header retaining the original provenance.
    #[must_use]
    fn empty_sequence(&self) -> Self;
}

/// A merge result borrowing unchanged subtrees from immutable fragments.
///
/// Merging expands participating containers while borrowing original keys and children.
/// Coercion rebuilds container wrappers and owns converted scalars, retaining unchanged
/// fragment references. The fragments must outlive this value. Validation operates
/// directly on this representation. Clones share expanded containers and generated
/// scalars; merging copies only shared containers it changes.
#[derive(Debug, Clone)]
pub struct MergedValue<'input, V: MergeBackend> {
    inner: Repr<'input, V>,
}

#[derive(Debug, Clone)]
enum Repr<'input, V: MergeBackend> {
    Borrowed(&'input V),
    Mapping(Arc<MappingData<'input, V>>),
    Sequence(Arc<SequenceData<'input, V>>),
    // Only scalars: children of owned backend containers could not borrow for 'input.
    Scalar(Arc<V>),
}

#[derive(Debug, Clone)]
struct MappingData<'input, V: MergeBackend> {
    original: Origin<'input, V>,
    entries: Vec<Entry<'input, V>>,
}

#[derive(Debug, Clone)]
struct SequenceData<'input, V: MergeBackend> {
    original: Origin<'input, V>,
    items: Vec<Item<'input, V>>,
}

// Generated origins contain only empty container headers. Share them so cloning
// a container does not clone backend metadata or inflate borrowed child wrappers.
#[derive(Debug, Clone)]
enum Origin<'input, V: MergeBackend> {
    Borrowed(&'input V),
    Generated(Arc<V>),
}

impl<V: MergeBackend> Origin<'_, V> {
    fn value(&self) -> &V {
        match self {
            Self::Borrowed(value) => value,
            Self::Generated(value) => value,
        }
    }
}

/// Expanded mapping entry borrowing its original key and pair metadata.
#[derive(Debug)]
pub struct Entry<'input, V: MergeBackend> {
    key: &'input V::Key,
    original_value: &'input V,
    value: MergedValue<'input, V>,
}

/// Expanded sequence item borrowing its original structural metadata.
#[derive(Debug)]
pub struct Item<'input, V: MergeBackend> {
    original: Option<&'input V::Element>,
    value: MergedValue<'input, V>,
}

impl<V: MergeBackend> Clone for Entry<'_, V> {
    fn clone(&self) -> Self {
        Self {
            key: self.key,
            original_value: self.original_value,
            value: self.value.clone(),
        }
    }
}
impl<V: MergeBackend> Clone for Item<'_, V> {
    fn clone(&self) -> Self {
        Self {
            original: self.original,
            value: self.value.clone(),
        }
    }
}

/// A child view borrowing an existing merge wrapper or owning a temporary wrapper.
#[derive(Debug)]
pub enum View<'a, 'input, V: MergeBackend> {
    /// An existing child in an expanded container.
    Borrowed(&'a MergedValue<'input, V>),
    /// A lightweight wrapper around an original backend child.
    Original(MergedValue<'input, V>),
}

impl<'input, V: MergeBackend> Borrow<MergedValue<'input, V>> for View<'_, 'input, V> {
    fn borrow(&self) -> &MergedValue<'input, V> {
        match self {
            Self::Borrowed(value) => value,
            Self::Original(value) => value,
        }
    }
}

impl<'input, V: MergeBackend> std::ops::Deref for View<'_, 'input, V> {
    type Target = MergedValue<'input, V>;
    fn deref(&self) -> &Self::Target {
        self.borrow()
    }
}

impl<'input, V: MergeBackend> MergedValue<'input, V> {
    /// Borrow a fragment or an entire unchanged subtree without allocating.
    pub fn new(value: &'input V) -> Self {
        Self {
            inner: Repr::Borrowed(value),
        }
    }

    /// Own a generated scalar. Containers are returned unchanged as an error.
    ///
    /// Containers are built from borrowed or generated children by merging and
    /// validation, rather than by owning a complete backend subtree.
    pub fn from_scalar(value: V) -> Result<Self, V> {
        if value.is_mapping() || value.is_sequence() {
            Err(value)
        } else {
            Ok(Self {
                inner: Repr::Scalar(Arc::new(value)),
            })
        }
    }

    /// Return the original subtree when this value is still entirely borrowed.
    pub fn as_original(&self) -> Option<&'input V> {
        match self.inner {
            Repr::Borrowed(value) => Some(value),
            _ => None,
        }
    }

    fn original(&self) -> &V {
        match &self.inner {
            Repr::Borrowed(value) => value,
            Repr::Mapping(data) => data.original.value(),
            Repr::Sequence(data) => data.original.value(),
            Repr::Scalar(value) => value,
        }
    }

    fn mapping_origin(&self) -> Origin<'input, V> {
        match &self.inner {
            Repr::Borrowed(value) if value.is_mapping() => Origin::Borrowed(value),
            Repr::Mapping(data) => data.original.clone(),
            _ => Origin::Generated(Arc::new(self.original().empty_mapping())),
        }
    }

    fn sequence_origin(&self) -> Origin<'input, V> {
        match &self.inner {
            Repr::Borrowed(value) if value.is_sequence() => Origin::Borrowed(value),
            Repr::Sequence(data) => data.original.clone(),
            _ => Origin::Generated(Arc::new(self.original().empty_sequence())),
        }
    }

    fn item_metadata(&self) -> impl Iterator<Item = Option<&'input V::Element>> + '_ {
        let mut original = match &self.inner {
            Repr::Borrowed(value) => Some(value.elements()),
            _ => None,
        };
        let mut expanded = match &self.inner {
            Repr::Sequence(data) => Some(data.items.iter()),
            _ => None,
        };
        std::iter::from_fn(move || {
            if let Some(iter) = &mut original {
                iter.next().map(|(element, _)| Some(element))
            } else {
                expanded.as_mut()?.next().map(|item| item.original)
            }
        })
    }
}

/// Merge immutable fragments without cloning their trees.
///
/// Results borrow from all input fragments. Use [`crate::merge`] with an existing
/// `MergedValue` to continue merging without materializing the previous result.
pub fn merge_borrowed<'input, V, I, S>(
    base: &'input V,
    nexts: I,
    schema: &S,
    options: MergeOptions,
) -> Result<MergedValue<'input, V>, MergeError>
where
    V: MergeBackend,
    I: IntoIterator<Item = &'input V>,
    S: MergeSchema,
{
    crate::merge(
        MergedValue::new(base),
        nexts.into_iter().map(MergedValue::new),
        schema,
        options,
    )
}

impl<'input, V: MergeBackend> MergeableValue for MergedValue<'input, V> {
    type Entry = Entry<'input, V>;
    type Item = Item<'input, V>;
    type Identity = SemanticValue;

    fn is_mapping(&self) -> bool {
        matches!(self.inner, Repr::Mapping(_))
            || matches!(&self.inner, Repr::Borrowed(value) if value.is_mapping())
    }
    fn is_sequence(&self) -> bool {
        matches!(self.inner, Repr::Sequence(_))
            || matches!(&self.inner, Repr::Borrowed(value) if value.is_sequence())
    }
    fn take_mapping(&mut self) -> Option<Vec<Self::Entry>> {
        if let Repr::Borrowed(original) = self.inner {
            if !original.is_mapping() {
                return None;
            }
            let entries = original
                .entries()
                .map(|(key, value)| Entry {
                    key,
                    original_value: value,
                    value: Self::new(value),
                })
                .collect();
            self.inner = Repr::Mapping(Arc::new(MappingData {
                original: Origin::Borrowed(original),
                entries: Vec::new(),
            }));
            return Some(entries);
        }
        match &mut self.inner {
            Repr::Mapping(data) => Some(std::mem::take(&mut Arc::make_mut(data).entries)),
            _ => None,
        }
    }
    fn set_mapping(&mut self, entries: Vec<Self::Entry>) {
        if let Repr::Mapping(data) = &mut self.inner {
            Arc::make_mut(data).entries = entries;
        }
    }
    fn take_sequence(&mut self) -> Option<Vec<Self::Item>> {
        if let Repr::Borrowed(original) = self.inner {
            if !original.is_sequence() {
                return None;
            }
            let items = original
                .elements()
                .map(|(element, value)| Item {
                    original: Some(element),
                    value: Self::new(value),
                })
                .collect();
            self.inner = Repr::Sequence(Arc::new(SequenceData {
                original: Origin::Borrowed(original),
                items: Vec::new(),
            }));
            return Some(items);
        }
        match &mut self.inner {
            Repr::Sequence(data) => Some(std::mem::take(&mut Arc::make_mut(data).items)),
            _ => None,
        }
    }
    fn set_sequence(&mut self, items: Vec<Self::Item>) {
        if let Repr::Sequence(data) = &mut self.inner {
            Arc::make_mut(data).items = items;
        }
    }
    fn child_identity(&self, key: &str) -> Option<Self::Identity> {
        ValidatableValue::get(self, key).map(|view| Borrow::<Self>::borrow(&view).identity())
    }
    fn identity(&self) -> Self::Identity {
        match &self.inner {
            Repr::Borrowed(value) => value.identity(),
            Repr::Scalar(value) => value.identity(),
            Repr::Mapping(data) => SemanticValue::mapping(
                data.entries
                    .iter()
                    .map(|entry| (V::key_identity(entry.key), entry.value.identity()))
                    .collect(),
            ),
            Repr::Sequence(data) => SemanticValue::sequence(
                data.items
                    .iter()
                    .map(|item| item.value.identity())
                    .collect(),
            ),
        }
    }
}

impl<'input, V: MergeBackend> MergeableEntry for Entry<'input, V> {
    type Value = MergedValue<'input, V>;
    type Key = SemanticValue;
    fn key(&self) -> Self::Key {
        V::key_identity(self.key)
    }
    fn key_name(&self) -> Option<&str> {
        V::key_name(self.key)
    }
    fn value(&self) -> &Self::Value {
        &self.value
    }
    fn value_mut(&mut self) -> &mut Self::Value {
        &mut self.value
    }
    fn take_value(&mut self) -> Self::Value {
        std::mem::replace(&mut self.value, MergedValue::new(self.original_value))
    }
}

impl<'input, V: MergeBackend> Item<'input, V> {
    /// Original backend item, including YAML span/comments, if the item was sourced.
    pub fn original(&self) -> Option<&'input V::Element> {
        self.original
    }
}

impl<'input, V: MergeBackend> MergeableItem for Item<'input, V> {
    type Value = MergedValue<'input, V>;
    fn value(&self) -> &Self::Value {
        &self.value
    }
    fn value_mut(&mut self) -> &mut Self::Value {
        &mut self.value
    }
    fn into_value(self) -> Self::Value {
        self.value
    }
}
