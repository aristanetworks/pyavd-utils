// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
//! Schema-guided merge helpers for AVD structured data.
//!
//! This crate is designed for schema-guided merges of AVD data. Dynamic-key
//! schemas are non-blocking, but they are not fully supported: if a merge input
//! modifies data used to resolve dynamic keys during the same merge, nested
//! primary-key list merging may use stale schema resolution and produce
//! unexpected results.

#![cfg_attr(
    test,
    allow(
        clippy::indexing_slicing,
        clippy::unwrap_used,
        reason = "Focused unit tests use direct indexing and unwraps for compact assertions"
    )
)]

use std::collections::HashMap;
use std::collections::HashSet;
use std::str::FromStr;

use avdschema::SchemaDataValue;
use avdschema::Store;
use avdschema::any::AnySchema;
use avdschema::get_schema_from_path;
use avdschema::list::List;
use serde_json::Map;
use serde_json::Value;

/// Merge already-parsed values or JSON strings using schemas in an [`avdschema::Store`].
pub trait StoreSchemaMerge {
    /// Merge `nexts` into `base` in order using `schema_name` and return merged JSON.
    ///
    /// Dynamic-key schemas are not blocked, but they are not fully supported. If
    /// a merge input modifies data used to resolve dynamic keys during the same
    /// merge, nested primary-key list merging may use stale schema resolution.
    ///
    /// For lists with schema primary keys, append/prepend strategies deep-merge
    /// by primary key. Replace/keep retain their full-list semantics. Keep-merge
    /// deep-merges matching primary-key items while keeping the existing list.
    /// Items with a missing or null primary key follow the ordinary list strategy,
    /// including full-value deduplication for the unique strategies.
    fn merge_json<I, S>(
        &self,
        base: &str,
        nexts: I,
        schema_name: &str,
        list_merge: ListMerge,
    ) -> Result<String, SchemaMergeError>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>;

    /// Merge `nexts` into `base` in order using `schema_name` and return the merged value.
    ///
    /// Dynamic-key schemas are not blocked, but they are not fully supported. If
    /// a merge input modifies data used to resolve dynamic keys during the same
    /// merge, nested primary-key list merging may use stale schema resolution.
    ///
    /// For lists with schema primary keys, append/prepend strategies deep-merge
    /// by primary key. Replace/keep retain their full-list semantics. Keep-merge
    /// deep-merges matching primary-key items while keeping the existing list.
    /// Items with a missing or null primary key follow the ordinary list strategy,
    /// including full-value deduplication for the unique strategies.
    fn merge_value<I>(
        &self,
        base: Value,
        nexts: I,
        schema_name: &str,
        list_merge: ListMerge,
    ) -> Result<Value, SchemaMergeError>
    where
        I: IntoIterator<Item = Value>;
}

impl StoreSchemaMerge for Store {
    fn merge_json<I, S>(
        &self,
        base: &str,
        nexts: I,
        schema_name: &str,
        list_merge: ListMerge,
    ) -> Result<String, SchemaMergeError>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let base_value = serde_json::from_str(base).map_err(SchemaMergeError::InvalidBaseJson)?;
        let nexts = nexts
            .into_iter()
            .enumerate()
            .map(|(index, next)| {
                serde_json::from_str(next.as_ref())
                    .map_err(|source| SchemaMergeError::InvalidNextJson { index, source })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let merged = self.merge_value(base_value, nexts, schema_name, list_merge)?;
        serde_json::to_string(&merged).map_err(SchemaMergeError::Serialize)
    }

    fn merge_value<I>(
        &self,
        mut base: Value,
        nexts: I,
        schema_name: &str,
        list_merge: ListMerge,
    ) -> Result<Value, SchemaMergeError>
    where
        I: IntoIterator<Item = Value>,
    {
        self.get(schema_name).map_err(|err| {
            SchemaMergeError::SchemaPath(avdschema::GetSchemaFromPathError::StoreError(err))
        })?;
        for next in nexts {
            let root = values_contain_mergeable_sequences(&base, &next).then(|| base.clone());
            let context = MergeContext {
                schema_name,
                store: self,
                root: root.as_ref(),
                list_merge,
            };
            merge_value(&mut base, next, &context, &mut Vec::new())?;
        }
        Ok(base)
    }
}

/// A structured value representation that can be merged by the schema merge engine.
///
/// This trait intentionally stays separate from validation's value abstraction. Validation needs
/// read-only type checks, coercion builders, spans, and diagnostics. Merge needs owned and mutable
/// structural operations: taking mappings/sequences out of incoming values, mutating existing base
/// mappings/sequences, replacing incompatible values, and computing identity for unique-list
/// handling.
///
/// The merge algorithm assumes a recursive tree model where mapping and sequence children are the
/// same value type as their parent. This fits dynamic data representations such as
/// [`serde_json::Value`], YAML AST nodes, or a future AVD value enum. Strongly typed generated
/// structs may be better served by generated typed merge code, or by converting to a dynamic value
/// before using this engine.
///
/// `SchemaData` bridges to `avdschema::get_schema_from_path`, so schema path resolution can reuse
/// the existing `avdschema::SchemaDataValue` adapters instead of depending on validation traits.
trait MergeableValue: Clone + Sized {
    /// Mapping/object representation for this value type.
    type Mapping: MergeableMapping<Value = Self>;
    /// Sequence/list representation for this value type.
    type Sequence: MergeableSequence<Value = Self>;
    /// Semantic key used for primary-key indexes and unique list strategies.
    type DedupKey: Eq + std::hash::Hash;
    /// Read-only schema-data view used by `avdschema` path resolution.
    type SchemaData<'a>: SchemaDataValue<'a>
    where
        Self: 'a;

    /// Return this value as a mapping, if it is one.
    fn as_mapping(&self) -> Option<&Self::Mapping>;

    /// Return this value as a mutable mapping, if it is one.
    fn as_mapping_mut(&mut self) -> Option<&mut Self::Mapping>;

    /// Consume this value as a mapping without cloning, returning the original value on mismatch.
    fn into_mapping(self) -> Result<Self::Mapping, Self>;

    /// Rebuild a value from its mapping representation.
    fn from_mapping(mapping: Self::Mapping) -> Self;

    /// Return this value as a sequence, if it is one.
    fn as_sequence(&self) -> Option<&Self::Sequence>;

    /// Return this value as a mutable sequence, if it is one.
    fn as_sequence_mut(&mut self) -> Option<&mut Self::Sequence>;

    /// Consume this value as a sequence without cloning, returning the original value on mismatch.
    fn into_sequence(self) -> Result<Self::Sequence, Self>;

    /// Rebuild a value from its sequence representation.
    fn from_sequence(sequence: Self::Sequence) -> Self;

    /// Return a read-only view suitable for schema path resolution.
    fn as_schema_data(&self) -> Self::SchemaData<'_>;

    /// Return the semantic identity used for deduplication.
    ///
    /// JSON currently uses the full value. A YAML implementation may choose a normalized semantic
    /// key so comments, spans, or node properties do not affect uniqueness.
    fn dedup_key(&self) -> Self::DedupKey;

    /// Return whether this value represents null/missing data.
    fn is_null(&self) -> bool;
}

/// Mutable mapping operations required by schema merge.
///
/// Keys are strings because AVD schemas address mapping entries by string keys. Backends with
/// richer key types, such as YAML, should expose only schema-addressable keys here and handle key
/// construction/preservation inside their adapter.
trait MergeableMapping {
    /// Value type stored in this mapping.
    type Value: MergeableValue;
    /// Borrowing iterator over schema-addressable mapping entries.
    type Iter<'a>: Iterator<Item = (&'a str, &'a Self::Value)>
    where
        Self: 'a;
    /// Owning iterator used to consume incoming mappings without cloning.
    type IntoIter: Iterator<Item = (String, Self::Value)>;

    /// Get an entry by schema key.
    fn get(&self, key: &str) -> Option<&Self::Value>;

    /// Get a mutable entry by schema key.
    fn get_mut(&mut self, key: &str) -> Option<&mut Self::Value>;

    /// Insert an incoming entry into the base mapping.
    fn insert(&mut self, key: String, value: Self::Value);

    /// Iterate over entries for recursive merge detection.
    fn iter(&self) -> Self::Iter<'_>;

    /// Consume the mapping into owned key-value entries.
    fn into_iter(self) -> Self::IntoIter;
}

/// Mutable sequence operations required by schema merge.
///
/// The merge engine uses this for list strategies, primary-key item updates, and temporary prepend
/// buffers. Backends can wrap richer item representations while still exposing the child value type
/// expected by the generic algorithm.
trait MergeableSequence {
    /// Value type stored in this sequence.
    type Value: MergeableValue;
    /// Borrowing iterator over sequence values.
    type Iter<'a>: Iterator<Item = &'a Self::Value>
    where
        Self: 'a;
    /// Owning iterator used to consume incoming sequences without cloning.
    type IntoIter: Iterator<Item = Self::Value>;

    /// Create an empty sequence with room for at least `capacity` items.
    fn with_capacity(capacity: usize) -> Self;

    /// Iterate over sequence values.
    fn iter(&self) -> Self::Iter<'_>;

    /// Return the number of items in the sequence.
    fn len(&self) -> usize;

    /// Get a mutable item by index.
    fn get_mut(&mut self, index: usize) -> Option<&mut Self::Value>;

    /// Append one item to the sequence.
    fn push(&mut self, value: Self::Value);

    /// Move all items from `other` onto the end of this sequence.
    fn append(&mut self, other: &mut Self);

    /// Replace this sequence with `next`.
    fn replace(&mut self, next: Self);

    /// Consume the sequence into owned values.
    fn into_iter(self) -> Self::IntoIter;
}

impl MergeableValue for Value {
    type Mapping = Map<String, Value>;
    type Sequence = Vec<Value>;
    type DedupKey = Value;
    type SchemaData<'a>
        = &'a Value
    where
        Self: 'a;

    fn as_mapping(&self) -> Option<&Self::Mapping> {
        self.as_object()
    }

    fn as_mapping_mut(&mut self) -> Option<&mut Self::Mapping> {
        self.as_object_mut()
    }

    fn into_mapping(self) -> Result<Self::Mapping, Self> {
        match self {
            Self::Object(map) => Ok(map),
            value => Err(value),
        }
    }

    fn from_mapping(mapping: Self::Mapping) -> Self {
        Self::Object(mapping)
    }

    fn as_sequence(&self) -> Option<&Self::Sequence> {
        self.as_array()
    }

    fn as_sequence_mut(&mut self) -> Option<&mut Self::Sequence> {
        self.as_array_mut()
    }

    fn into_sequence(self) -> Result<Self::Sequence, Self> {
        match self {
            Self::Array(sequence) => Ok(sequence),
            value => Err(value),
        }
    }

    fn from_sequence(sequence: Self::Sequence) -> Self {
        Self::Array(sequence)
    }

    fn as_schema_data(&self) -> Self::SchemaData<'_> {
        self
    }

    fn dedup_key(&self) -> Self::DedupKey {
        self.clone()
    }

    fn is_null(&self) -> bool {
        Value::is_null(self)
    }
}

impl MergeableMapping for Map<String, Value> {
    type Value = Value;
    type Iter<'a> = JsonMapIter<'a>;
    type IntoIter = serde_json::map::IntoIter;

    fn get(&self, key: &str) -> Option<&Self::Value> {
        Map::get(self, key)
    }

    fn get_mut(&mut self, key: &str) -> Option<&mut Self::Value> {
        Map::get_mut(self, key)
    }

    fn insert(&mut self, key: String, value: Self::Value) {
        Map::insert(self, key, value);
    }

    fn iter(&self) -> Self::Iter<'_> {
        JsonMapIter {
            inner: Map::iter(self),
        }
    }

    fn into_iter(self) -> Self::IntoIter {
        <Self as IntoIterator>::into_iter(self)
    }
}

struct JsonMapIter<'a> {
    inner: serde_json::map::Iter<'a>,
}

impl<'a> Iterator for JsonMapIter<'a> {
    type Item = (&'a str, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(key, value)| (key.as_str(), value))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl ExactSizeIterator for JsonMapIter<'_> {}

impl MergeableSequence for Vec<Value> {
    type Value = Value;
    type Iter<'a> = std::slice::Iter<'a, Value>;
    type IntoIter = std::vec::IntoIter<Value>;

    fn with_capacity(capacity: usize) -> Self {
        Vec::with_capacity(capacity)
    }

    fn iter(&self) -> Self::Iter<'_> {
        <[Value]>::iter(self)
    }

    fn len(&self) -> usize {
        Vec::len(self)
    }

    fn get_mut(&mut self, index: usize) -> Option<&mut Self::Value> {
        <[Value]>::get_mut(self, index)
    }

    fn push(&mut self, value: Self::Value) {
        Vec::push(self, value);
    }

    fn append(&mut self, other: &mut Self) {
        Vec::append(self, other);
    }

    fn replace(&mut self, next: Self) {
        *self = next;
    }

    fn into_iter(self) -> Self::IntoIter {
        <Self as IntoIterator>::into_iter(self)
    }
}

struct MergeContext<'a, V> {
    schema_name: &'a str,
    store: &'a Store,
    root: Option<&'a V>,
    list_merge: ListMerge,
}

fn values_contain_mergeable_sequences<V: MergeableValue>(base: &V, next: &V) -> bool {
    if base.as_sequence().is_some() && next.as_sequence().is_some() {
        return true;
    }

    let (Some(base_map), Some(next_map)) = (base.as_mapping(), next.as_mapping()) else {
        return false;
    };

    for (key, next_value) in next_map.iter() {
        if let Some(base_value) = base_map.get(key)
            && values_contain_mergeable_sequences(base_value, next_value)
        {
            return true;
        }
    }
    false
}

fn merge_value<V: MergeableValue>(
    base: &mut V,
    next: V,
    context: &MergeContext<'_, V>,
    path: &mut Vec<String>,
) -> Result<(), SchemaMergeError> {
    let next = match next.into_mapping() {
        Ok(next_map) => {
            if let Some(base_map) = base.as_mapping_mut() {
                merge_maps(base_map, next_map, context, path)?;
                return Ok(());
            }
            V::from_mapping(next_map)
        }
        Err(next_value) => next_value,
    };

    let next = match next.into_sequence() {
        Ok(next_sequence) => {
            if let Some(base_sequence) = base.as_sequence_mut() {
                merge_sequences(base_sequence, next_sequence, context, path)?;
                return Ok(());
            }
            V::from_sequence(next_sequence)
        }
        Err(next_value) => next_value,
    };

    *base = next;
    Ok(())
}

fn merge_maps<V: MergeableValue>(
    base: &mut V::Mapping,
    next: V::Mapping,
    context: &MergeContext<'_, V>,
    path: &mut Vec<String>,
) -> Result<(), SchemaMergeError> {
    for (key, next_value) in next.into_iter() {
        if let Some(base_value) = base.get_mut(&key) {
            path.push(key.clone());
            merge_value(base_value, next_value, context, path)?;
            path.pop();
        } else {
            base.insert(key, next_value);
        }
    }
    Ok(())
}

fn merge_sequences<V: MergeableValue>(
    base: &mut V::Sequence,
    next: V::Sequence,
    context: &MergeContext<'_, V>,
    path: &mut Vec<String>,
) -> Result<(), SchemaMergeError> {
    match context.list_merge {
        ListMerge::Replace => {
            base.replace(next);
            return Ok(());
        }
        ListMerge::Keep => return Ok(()),
        ListMerge::KeepMerge
        | ListMerge::Append
        | ListMerge::AppendUnique
        | ListMerge::Prepend
        | ListMerge::PrependUnique => {}
    }

    if let Some(root) = context.root
        && let Some(primary_key) =
            primary_key_for_path(context.schema_name, context.store, root, path)?
    {
        merge_primary_key_sequence_items(base, next, primary_key, context, path)?;
        return Ok(());
    }

    merge_non_primary_key_sequence_items::<V>(base, next, context.list_merge);
    Ok(())
}

fn merge_non_primary_key_sequence_items<V: MergeableValue>(
    base: &mut V::Sequence,
    next: V::Sequence,
    list_merge: ListMerge,
) {
    match list_merge {
        ListMerge::Replace => {
            base.replace(next);
            return;
        }
        ListMerge::Keep | ListMerge::KeepMerge => return,
        ListMerge::Append
        | ListMerge::AppendUnique
        | ListMerge::Prepend
        | ListMerge::PrependUnique => {}
    }

    merge_remaining_sequence_items::<V>(base, next, list_merge);
}

fn primary_key_for_path<'store, V: MergeableValue>(
    schema_name: &str,
    store: &'store Store,
    root: &V,
    path: &[String],
) -> Result<Option<&'store str>, SchemaMergeError> {
    let Some(schema) = get_schema_from_path(schema_name, store, path, root.as_schema_data(), None)
        .map_err(SchemaMergeError::SchemaPath)?
    else {
        return Ok(None);
    };
    let AnySchema::List(List {
        primary_key,
        allow_duplicate_primary_key,
        ..
    }) = schema
    else {
        return Ok(None);
    };
    if allow_duplicate_primary_key.unwrap_or_default() {
        return Ok(None);
    }
    Ok(primary_key.as_deref())
}

fn merge_primary_key_sequence_items<V: MergeableValue>(
    base: &mut V::Sequence,
    next: V::Sequence,
    primary_key: &str,
    context: &MergeContext<'_, V>,
    path: &mut Vec<String>,
) -> Result<(), SchemaMergeError> {
    let prepend = matches!(
        context.list_merge,
        ListMerge::Prepend | ListMerge::PrependUnique
    );
    let mut item_index_by_primary_value = item_index_by_primary_value::<V>(base, primary_key);
    let mut prepend_items = V::Sequence::with_capacity(0);

    for next_value in next.into_iter() {
        if let Some(next_primary_value) =
            item_primary_value(&next_value, primary_key).map(MergeableValue::dedup_key)
        {
            if let Some(item_index) = item_index_by_primary_value
                .get(&next_primary_value)
                .copied()
            {
                let items = if prepend && item_index.is_prepend {
                    &mut prepend_items
                } else {
                    &mut *base
                };
                path.push(item_index.index.to_string());
                if let Some(item) = items.get_mut(item_index.index) {
                    merge_value(item, next_value, context, path)?;
                }
                path.pop();
                continue;
            }

            if context.list_merge == ListMerge::KeepMerge {
                continue;
            }

            let index = if prepend {
                prepend_items.len()
            } else {
                base.len()
            };
            item_index_by_primary_value.insert(
                next_primary_value,
                ItemIndex {
                    index,
                    is_prepend: prepend,
                },
            );
        } else {
            if context.list_merge == ListMerge::KeepMerge {
                continue;
            }
            if matches!(
                context.list_merge,
                ListMerge::AppendUnique | ListMerge::PrependUnique
            ) {
                let dedup_key = next_value.dedup_key();
                if sequence_contains_dedup_key::<V>(base, &dedup_key)
                    || sequence_contains_dedup_key::<V>(&prepend_items, &dedup_key)
                {
                    continue;
                }
            }
        }
        if prepend {
            prepend_items.push(next_value);
        } else {
            base.push(next_value);
        }
    }

    if prepend {
        prepend_items.append(base);
        base.replace(prepend_items);
    }
    Ok(())
}

#[derive(Clone, Copy)]
struct ItemIndex {
    index: usize,
    is_prepend: bool,
}

fn item_index_by_primary_value<V: MergeableValue>(
    base: &V::Sequence,
    primary_key: &str,
) -> HashMap<V::DedupKey, ItemIndex> {
    let mut item_index_by_primary_value = HashMap::with_capacity(base.len());
    for (index, item) in base.iter().enumerate() {
        if let Some(primary_value) = item_primary_value(item, primary_key) {
            item_index_by_primary_value
                .entry(primary_value.dedup_key())
                .or_insert(ItemIndex {
                    index,
                    is_prepend: false,
                });
        }
    }
    item_index_by_primary_value
}

fn item_primary_value<'a, V: MergeableValue>(item: &'a V, primary_key: &str) -> Option<&'a V> {
    item.as_mapping()?
        .get(primary_key)
        .filter(|value| !value.is_null())
}

fn sequence_contains_dedup_key<V: MergeableValue>(
    sequence: &V::Sequence,
    dedup_key: &V::DedupKey,
) -> bool {
    sequence.iter().any(|item| item.dedup_key().eq(dedup_key))
}

fn merge_remaining_sequence_items<V: MergeableValue>(
    base: &mut V::Sequence,
    next: V::Sequence,
    list_merge: ListMerge,
) {
    let remaining = next.into_iter();
    match list_merge {
        ListMerge::Append => {
            for item in remaining {
                base.push(item);
            }
        }
        ListMerge::AppendUnique => {
            let mut seen: HashSet<V::DedupKey> =
                base.iter().map(MergeableValue::dedup_key).collect();
            for item in remaining {
                if seen.insert(item.dedup_key()) {
                    base.push(item);
                }
            }
        }
        ListMerge::Prepend => {
            let mut prepended = V::Sequence::with_capacity(0);
            for item in remaining {
                prepended.push(item);
            }
            prepended.append(base);
            base.replace(prepended);
        }
        ListMerge::PrependUnique => {
            let mut prepended = V::Sequence::with_capacity(0);
            let mut seen: HashSet<V::DedupKey> =
                base.iter().map(MergeableValue::dedup_key).collect();
            for item in remaining {
                if seen.insert(item.dedup_key()) {
                    prepended.push(item);
                }
            }
            prepended.append(base);
            base.replace(prepended);
        }
        ListMerge::Replace | ListMerge::Keep | ListMerge::KeepMerge => {}
    }
}

/// List merge strategy.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ListMerge {
    /// Replace the base list with the next list when the list has no schema primary key.
    Replace,
    /// Append items when the list has no schema primary key.
    Append,
    /// Keep the existing base list when the list has no schema primary key.
    Keep,
    /// Deep-merge matching primary-key items while keeping the existing base list.
    KeepMerge,
    /// Prepend items when the list has no schema primary key.
    Prepend,
    /// Append unique items when the list has no schema primary key.
    AppendUnique,
    /// Prepend unique items when the list has no schema primary key.
    PrependUnique,
}

impl FromStr for ListMerge {
    type Err = InvalidListMergeStrategy;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "replace" => Ok(Self::Replace),
            "append" => Ok(Self::Append),
            "keep" => Ok(Self::Keep),
            "keep_merge" => Ok(Self::KeepMerge),
            "prepend" => Ok(Self::Prepend),
            "append_unique" => Ok(Self::AppendUnique),
            "prepend_unique" => Ok(Self::PrependUnique),
            _ => Err(InvalidListMergeStrategy {
                strategy: value.to_owned(),
            }),
        }
    }
}

/// Invalid list merge strategy.
#[derive(Debug, PartialEq, Eq, derive_more::Display)]
#[display("Invalid list merge strategy '{strategy}'.")]
pub struct InvalidListMergeStrategy {
    /// The invalid strategy.
    pub strategy: String,
}

impl std::error::Error for InvalidListMergeStrategy {}

/// Error returned while schema-merging values.
#[derive(Debug, derive_more::Display)]
pub enum SchemaMergeError {
    /// Base JSON parsing failed.
    #[display("Invalid JSON in base data: {_0}")]
    InvalidBaseJson(serde_json::Error),
    /// Next JSON parsing failed.
    #[display("Invalid JSON in next data at index {index}: {source}")]
    InvalidNextJson {
        /// Index of the invalid JSON document in the incoming nexts collection.
        index: usize,
        /// JSON parsing error.
        source: serde_json::Error,
    },
    /// JSON serialization failed.
    #[display("Unable to serialize merged data as JSON: {_0}")]
    Serialize(serde_json::Error),
    /// Schema lookup failed.
    #[display("Unable to look up schema path: {_0:?}")]
    SchemaPath(avdschema::GetSchemaFromPathError),
}

impl std::error::Error for SchemaMergeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InvalidBaseJson(err) | Self::Serialize(err) => Some(err),
            Self::InvalidNextJson { source, .. } => Some(source),
            Self::SchemaPath(_err) => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    fn test_store() -> Store {
        serde_json::from_value(json!({
            "eos_config": {
                "type": "dict",
                "keys": {
                    "ethernet_interfaces": {
                        "type": "list",
                        "primary_key": "name",
                        "items": {
                            "type": "dict",
                            "keys": {
                                "name": {"type": "str"},
                                "description": {"type": "str"},
                                "tags": {
                                    "type": "list",
                                    "items": {"type": "str"}
                                },
                                "vlans": {
                                    "type": "list",
                                    "primary_key": "id",
                                    "items": {
                                        "type": "dict",
                                        "keys": {
                                            "id": {"type": "int"},
                                            "name": {"type": "str"}
                                        }
                                    }
                                }
                            }
                        }
                    },
                    "servers": {
                        "type": "list",
                        "items": {"type": "str"}
                    },
                    "router_bgp": {
                        "type": "dict",
                        "keys": {
                            "as": {"type": "str"},
                            "router_id": {"type": "str"}
                        }
                    }
                }
            }
        }))
        .unwrap()
    }

    fn merge(base: Value, next: Value, list_merge: ListMerge) -> Value {
        test_store()
            .merge_value(base, vec![next], "eos_config", list_merge)
            .unwrap()
    }

    fn merge_many(base: Value, nexts: Vec<Value>, list_merge: ListMerge) -> Value {
        test_store()
            .merge_value(base, nexts, "eos_config", list_merge)
            .unwrap()
    }

    #[test]
    fn recursive_dict_merge_and_scalar_override() {
        let result = merge(
            json!({"router_bgp": {"as": "65000", "router_id": "1.1.1.1"}}),
            json!({"router_bgp": {"router_id": "2.2.2.2"}}),
            ListMerge::AppendUnique,
        );

        assert_eq!(
            result,
            json!({"router_bgp": {"as": "65000", "router_id": "2.2.2.2"}})
        );
    }

    #[test]
    fn list_replace_strategy_replaces_full_list() {
        let result = merge(
            json!({"servers": ["one"]}),
            json!({"servers": ["two", "three"]}),
            ListMerge::Replace,
        );

        assert_eq!(result, json!({"servers": ["two", "three"]}));
    }

    #[test]
    fn list_keep_strategy_keeps_base_list() {
        let result = merge(
            json!({"servers": ["one"]}),
            json!({"servers": ["two"]}),
            ListMerge::Keep,
        );

        assert_eq!(result, json!({"servers": ["one"]}));
    }

    #[test]
    fn list_keep_merge_strategy_keeps_non_primary_key_base_list() {
        let result = merge(
            json!({"servers": ["one"]}),
            json!({"servers": ["two"]}),
            ListMerge::KeepMerge,
        );

        assert_eq!(result, json!({"servers": ["one"]}));
    }

    #[test]
    fn list_append_and_prepend_strategies_keep_duplicates() {
        assert_eq!(
            merge(
                json!({"servers": ["one"]}),
                json!({"servers": ["one", "two"]}),
                ListMerge::Append,
            ),
            json!({"servers": ["one", "one", "two"]})
        );
        assert_eq!(
            merge(
                json!({"servers": ["one"]}),
                json!({"servers": ["two"]}),
                ListMerge::Prepend,
            ),
            json!({"servers": ["two", "one"]})
        );
    }

    #[test]
    fn list_append_unique_and_prepend_unique_strategies_skip_duplicates() {
        assert_eq!(
            merge(
                json!({"servers": ["one"]}),
                json!({"servers": ["one", "two"]}),
                ListMerge::AppendUnique,
            ),
            json!({"servers": ["one", "two"]})
        );
        assert_eq!(
            merge(
                json!({"servers": ["one"]}),
                json!({"servers": ["one", "two"]}),
                ListMerge::PrependUnique,
            ),
            json!({"servers": ["two", "one"]})
        );
    }

    #[test]
    fn primary_key_list_items_are_merged() {
        let result = merge(
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "base"}]}),
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "next"}, {"name": "Ethernet2"}]}),
            ListMerge::AppendUnique,
        );

        assert_eq!(
            result,
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "next"}, {"name": "Ethernet2"}]})
        );
    }

    #[test]
    fn nested_primary_key_list_items_are_merged() {
        let result = merge(
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "vlans": [{"id": 10, "name": "base"}]}]}),
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "vlans": [{"id": 10, "name": "next"}, {"id": 20}]}]}),
            ListMerge::AppendUnique,
        );

        assert_eq!(
            result,
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "vlans": [{"id": 10, "name": "next"}, {"id": 20}]}]})
        );
    }

    #[test]
    fn ethernet_interfaces_append_merges_nested_primary_key_lists() {
        let result = merge(
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet1", "description": "base"},
                    {
                        "name": "Ethernet2",
                        "description": "from base",
                        "vlans": [{"id": 10, "name": "base"}]
                    },
                    {"name": "Ethernet3"}
                ]
            }),
            json!({
                "ethernet_interfaces": [
                    {
                        "name": "Ethernet2",
                        "description": "from next",
                        "vlans": [
                            {"id": 10, "name": "next"},
                            {"id": 20, "name": "new"}
                        ]
                    },
                    {"name": "Ethernet4"}
                ]
            }),
            ListMerge::Append,
        );

        assert_eq!(
            result,
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet1", "description": "base"},
                    {
                        "name": "Ethernet2",
                        "description": "from next",
                        "vlans": [
                            {"id": 10, "name": "next"},
                            {"id": 20, "name": "new"}
                        ]
                    },
                    {"name": "Ethernet3"},
                    {"name": "Ethernet4"}
                ]
            })
        );
    }

    #[test]
    fn primary_key_list_prepend_unique_merges_existing_and_prepends_new_items() {
        let result = merge(
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet1", "description": "base"},
                    {"name": "Ethernet2", "description": "from base"}
                ]
            }),
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet2", "description": "from next"},
                    {"name": "Ethernet3", "description": "new"},
                    {"name": "Ethernet3", "description": "new"}
                ]
            }),
            ListMerge::PrependUnique,
        );

        assert_eq!(
            result,
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet3", "description": "new"},
                    {"name": "Ethernet1", "description": "base"},
                    {"name": "Ethernet2", "description": "from next"}
                ]
            })
        );
    }

    #[test]
    fn primary_key_list_replace_replaces_full_list() {
        let result = merge(
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet1", "description": "base-only"},
                    {"name": "Ethernet2", "description": "base"}
                ]
            }),
            json!({"ethernet_interfaces": [{"name": "Ethernet2", "description": "next"}, {"name": "Ethernet3"}]}),
            ListMerge::Replace,
        );

        assert_eq!(
            result,
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet2", "description": "next"},
                    {"name": "Ethernet3"}
                ]
            })
        );
    }

    #[test]
    fn primary_key_list_keep_keeps_base_list() {
        let result = merge(
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "base"}]}),
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "next"}, {"name": "Ethernet2"}]}),
            ListMerge::Keep,
        );

        assert_eq!(
            result,
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "base"}]})
        );
    }

    #[test]
    fn primary_key_list_keep_merge_merges_matches_and_keeps_base_list() {
        let result = merge(
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet1", "description": "base", "tags": ["base"]},
                    {"name": "Ethernet2", "description": "base-only"}
                ]
            }),
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet1", "description": "next", "tags": ["next"]},
                    {"name": "Ethernet3", "description": "next-only"},
                    {"description": "missing primary key"}
                ]
            }),
            ListMerge::KeepMerge,
        );

        assert_eq!(
            result,
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet1", "description": "next", "tags": ["base"]},
                    {"name": "Ethernet2", "description": "base-only"}
                ]
            })
        );
    }

    #[test]
    fn duplicate_base_primary_keys_merge_into_first_match() {
        let result = merge(
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet1", "description": "first"},
                    {"name": "Ethernet1", "description": "second"}
                ]
            }),
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "next"}]}),
            ListMerge::Append,
        );

        assert_eq!(
            result,
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet1", "description": "next"},
                    {"name": "Ethernet1", "description": "second"}
                ]
            })
        );
    }

    #[test]
    fn duplicate_incoming_primary_keys_merge_repeatedly_into_same_base_item() {
        let result = merge(
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "base"}]}),
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet1", "description": "next"},
                    {"name": "Ethernet1", "tags": ["blue"]}
                ]
            }),
            ListMerge::Append,
        );

        assert_eq!(
            result,
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "next", "tags": ["blue"]}]})
        );
    }

    #[test]
    fn unmatched_primary_key_items_are_prepended_with_prepend() {
        let result = merge(
            json!({"ethernet_interfaces": [{"name": "Ethernet3"}]}),
            json!({"ethernet_interfaces": [{"name": "Ethernet1"}, {"name": "Ethernet2"}]}),
            ListMerge::Prepend,
        );

        assert_eq!(
            result,
            json!({"ethernet_interfaces": [{"name": "Ethernet1"}, {"name": "Ethernet2"}, {"name": "Ethernet3"}]})
        );
    }

    #[test]
    fn duplicate_incoming_unmatched_primary_keys_merge_into_appended_item() {
        let result = merge(
            json!({"ethernet_interfaces": []}),
            json!({
                "ethernet_interfaces": [
                    {"name": "Ethernet1", "description": "first"},
                    {"name": "Ethernet1", "tags": ["blue"]}
                ]
            }),
            ListMerge::Append,
        );

        assert_eq!(
            result,
            json!({"ethernet_interfaces": [{"name": "Ethernet1", "description": "first", "tags": ["blue"]}]})
        );
    }

    #[test]
    fn primary_key_list_items_missing_primary_key_are_appended() {
        let result = merge(
            json!({"ethernet_interfaces": [{"name": "Ethernet1"}]}),
            json!({"ethernet_interfaces": [{"description": "missing primary key"}]}),
            ListMerge::Append,
        );

        assert_eq!(
            result,
            json!({"ethernet_interfaces": [{"name": "Ethernet1"}, {"description": "missing primary key"}]})
        );
    }

    #[test]
    fn null_primary_key_items_follow_append_prepend_and_keep_merge_semantics() {
        let base = json!({"ethernet_interfaces": [{"name": null, "description": "base"}]});
        let next = json!({"ethernet_interfaces": [{"name": null, "description": "next"}]});

        assert_eq!(
            merge(base.clone(), next.clone(), ListMerge::Append),
            json!({"ethernet_interfaces": [
                {"name": null, "description": "base"},
                {"name": null, "description": "next"}
            ]})
        );
        assert_eq!(
            merge(base.clone(), next.clone(), ListMerge::Prepend),
            json!({"ethernet_interfaces": [
                {"name": null, "description": "next"},
                {"name": null, "description": "base"}
            ]})
        );
        assert_eq!(merge(base.clone(), next, ListMerge::KeepMerge), base);
    }

    #[test]
    fn unique_strategies_deduplicate_primary_key_items_without_usable_keys() {
        let base = json!({"ethernet_interfaces": [{"description": "same"}]});
        let next = json!({"ethernet_interfaces": [
            {"description": "same"},
            {"description": "new"},
            {"description": "new"},
            {"name": null, "description": "null"},
            {"name": null, "description": "null"}
        ]});

        assert_eq!(
            merge(base.clone(), next.clone(), ListMerge::AppendUnique),
            json!({"ethernet_interfaces": [
                {"description": "same"},
                {"description": "new"},
                {"name": null, "description": "null"}
            ]})
        );
        assert_eq!(
            merge(base, next, ListMerge::PrependUnique),
            json!({"ethernet_interfaces": [
                {"description": "new"},
                {"name": null, "description": "null"},
                {"description": "same"}
            ]})
        );
    }

    #[test]
    fn append_unique_deduplicates_unmerged_object_values() {
        let result = merge(
            json!({"unknown_list": [{"id": 1}]}),
            json!({"unknown_list": [{"id": 1}, {"id": 2}, {"id": 2}]}),
            ListMerge::AppendUnique,
        );

        assert_eq!(result, json!({"unknown_list": [{"id": 1}, {"id": 2}]}));
    }

    #[test]
    fn prepend_unique_deduplicates_unmerged_object_values() {
        let result = merge(
            json!({"unknown_list": [{"id": 1}]}),
            json!({"unknown_list": [{"id": 1}, {"id": 2}, {"id": 2}, {"id": 3}]}),
            ListMerge::PrependUnique,
        );

        assert_eq!(
            result,
            json!({"unknown_list": [{"id": 2}, {"id": 3}, {"id": 1}]})
        );
    }

    #[test]
    fn multiple_nexts_are_merged_in_incoming_order() {
        let result = merge_many(
            json!({"router_bgp": {"as": "65000", "router_id": "1.1.1.1"}}),
            vec![
                json!({"router_bgp": {"router_id": "2.2.2.2"}}),
                json!({"router_bgp": {"router_id": "3.3.3.3"}}),
            ],
            ListMerge::AppendUnique,
        );

        assert_eq!(
            result,
            json!({"router_bgp": {"as": "65000", "router_id": "3.3.3.3"}})
        );
    }

    #[test]
    fn merge_json_reports_invalid_json() {
        let result = test_store().merge_json("{", ["{}"], "eos_config", ListMerge::AppendUnique);

        assert!(matches!(result, Err(SchemaMergeError::InvalidBaseJson(_))));
    }

    #[test]
    fn merge_json_reports_invalid_next_json_index() {
        let result =
            test_store().merge_json("{}", ["{}", "{"], "eos_config", ListMerge::AppendUnique);

        assert!(matches!(
            result,
            Err(SchemaMergeError::InvalidNextJson { index: 1, .. })
        ));
    }

    #[test]
    fn invalid_schema_name_is_reported() {
        for (base, next, list_merge) in [
            (
                json!({"router_bgp": {"as": "65000"}}),
                json!({"router_bgp": {"as": "65001"}}),
                ListMerge::AppendUnique,
            ),
            (json!(["one"]), json!(["two"]), ListMerge::Replace),
            (json!(["one"]), json!(["two"]), ListMerge::Keep),
        ] {
            let result = test_store().merge_value(base, [next], "invalid", list_merge);

            assert!(matches!(result, Err(SchemaMergeError::SchemaPath(_))));
        }
    }

    #[test]
    fn parses_list_merge_strategy() {
        assert_eq!("append_unique".parse(), Ok(ListMerge::AppendUnique));
        assert_eq!("keep_merge".parse(), Ok(ListMerge::KeepMerge));
        assert!("invalid".parse::<ListMerge>().is_err());
    }
}
