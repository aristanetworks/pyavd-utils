// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Shallow and schema-guided deep merging for JSON values and YAML nodes.
//!
//! Values move between inputs and the result. YAML replacements keep incoming metadata;
//! partially merged containers keep their original metadata, while their children retain
//! their individual provenance. A container's original span does not describe all contributors.
//!
//! Schema lookup uses [`Store::get_schema_info`], matching the Python AVD merger. Data-dependent
//! dynamic keys are not supported. Unknown paths use ordinary list merging.

#![doc = include_str!("../README.md")]

mod identity;
mod json;
mod yaml;

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;

use avdschema::SchemaPathError;
use avdschema::Store;
pub use identity::SemanticValue;

/// An owned tree that supports merging without discarding backend metadata.
pub trait MergeableValue: Clone + Sized {
    /// Mapping entry, including any key and structural metadata.
    type Entry: MergeableEntry<Value = Self>;
    /// Sequence entry, including any structural metadata.
    type Item: MergeableItem<Value = Self>;
    /// Semantic identity, excluding source locations, comments, anchors, and other metadata.
    type Identity: Eq + Hash;

    /// Remove a mapping's entries, leaving an empty mapping with the same metadata.
    fn take_mapping(&mut self) -> Option<Vec<Self::Entry>>;
    /// Set the entries on an existing mapping, preserving its metadata.
    fn set_mapping(&mut self, entries: Vec<Self::Entry>);
    /// Remove a sequence's entries, leaving an empty sequence with the same metadata.
    fn take_sequence(&mut self) -> Option<Vec<Self::Item>>;
    /// Set the entries on an existing sequence, preserving its metadata.
    fn set_sequence(&mut self, items: Vec<Self::Item>);
    /// Look up a string mapping key; distinguish missing keys from explicit nulls.
    fn get(&self, key: &str) -> Option<&Self>;
    /// Identity used for primary-key indexes and unique-list strategies.
    fn identity(&self) -> Self::Identity;
}

/// A mapping entry whose key metadata can be retained independently of its value.
pub trait MergeableEntry {
    /// Value stored in this entry.
    type Value: MergeableValue;
    /// Key identity, excluding source locations and other metadata.
    type Key: Eq + Hash;

    /// Return the semantic key identity.
    fn key(&self) -> Self::Key;
    /// Return the key name for schema navigation, if it is a string.
    fn key_name(&self) -> Option<&str>;
    /// Borrow the value.
    fn value(&self) -> &Self::Value;
    /// Borrow the value mutably.
    fn value_mut(&mut self) -> &mut Self::Value;
    /// Move out the value, leaving a disposable placeholder.
    fn take_value(&mut self) -> Self::Value;
}

/// A sequence entry carrying its backend's structural metadata.
pub trait MergeableItem {
    /// Value stored in this entry.
    type Value: MergeableValue;

    /// Borrow the value.
    fn value(&self) -> &Self::Value;
    /// Borrow the value mutably.
    fn value_mut(&mut self) -> &mut Self::Value;
    /// Consume the entry and return its value.
    fn into_value(self) -> Self::Value;
}

/// Depth at which dictionary values are combined.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum MergeMode {
    /// Update root keys, replacing the entire value of each incoming key (`dict.update`).
    Shallow,
    /// Recursively combine dictionaries and apply the configured list strategy.
    #[default]
    Deep,
}

/// List strategies named after the AVD Python merger's `list_merge` options.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum ListMerge {
    /// Replace the whole list, bypassing schema primary keys.
    Replace,
    /// Keep the base list, but still merge items with matching schema primary keys.
    Keep,
    /// Append unmatched incoming items.
    #[default]
    Append,
    /// Prepend unmatched incoming items.
    Prepend,
    /// Append incoming items absent from the base (`append_rp`).
    AppendUnique,
    /// Prepend incoming items, removing equivalent base items (`prepend_rp`).
    PrependUnique,
}

/// Strategy for scalar values and type conflicts during deep merging.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum SameKeyStrategy {
    /// Prefer the incoming value.
    #[default]
    Override,
    /// Keep the base value, including explicit null.
    UseExisting,
    /// Reject semantically different values.
    MustMatch,
}

/// Options shared by JSON and YAML merging.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct MergeOptions {
    /// Shallow root updates or recursive merging.
    pub mode: MergeMode,
    /// Strategy for lists, after merging matching schema primary keys.
    pub list_merge: ListMerge,
    /// Strategy for scalars and type conflicts.
    pub same_key_strategy: SameKeyStrategy,
}

/// Schema metadata provider for a merge operation.
///
/// Paths contain dictionary keys and numeric list indexes, as in the existing schema-path helper.
/// Keeping this separate lets the merger operate without depending on schema views.
pub trait MergeSchema {
    /// Return the list primary key at a path, or `None` when no primary key applies.
    fn primary_key(&self, path: &[String]) -> Result<Option<&str>, SchemaPathError>;
}

/// Use ordinary list merging without schema primary keys.
#[derive(Debug, Default, Clone, Copy)]
pub struct NoSchema;

impl MergeSchema for NoSchema {
    fn primary_key(&self, _path: &[String]) -> Result<Option<&str>, SchemaPathError> {
        Ok(None)
    }
}

/// Schema-path adapter using the same metadata helper as the Python AVD merger.
#[derive(Debug, Clone, Copy)]
pub struct StoreSchema<'a> {
    store: &'a Store,
    name: &'a str,
}

impl<'a> StoreSchema<'a> {
    /// Select an existing schema. Dynamic-key definitions are not resolved from merge inputs.
    pub fn new(store: &'a Store, name: &'a str) -> Result<Self, SchemaPathError> {
        if store.get(name).is_none() {
            return Err(SchemaPathError::InvalidSchemaName(name.to_owned()));
        }
        Ok(Self { store, name })
    }
}

impl MergeSchema for StoreSchema<'_> {
    fn primary_key(&self, path: &[String]) -> Result<Option<&str>, SchemaPathError> {
        self.store
            .get_schema_info(self.name, path)
            .map(|info| info.and_then(|info| info.primary_key))
    }
}

/// A merge failure with the data path at which it occurred.
#[derive(Debug)]
pub enum MergeError {
    /// A schema path lookup failed.
    Schema {
        /// Data path being resolved.
        path: Vec<String>,
        /// Underlying schema-path error.
        source: SchemaPathError,
    },
    /// The `must_match` strategy encountered different values.
    Conflict {
        /// Data path at which values differ.
        path: Vec<String>,
    },
}

impl fmt::Display for MergeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Schema { path, source } => {
                write!(
                    f,
                    "Unable to resolve schema at {}: {source}",
                    path.join(".")
                )
            }
            Self::Conflict { path } => {
                write!(f, "Values of {} do not match", path.join("."))
            }
        }
    }
}

impl std::error::Error for MergeError {}

/// Merge inputs in order into an owned base value.
///
/// No whole-document copy or JSON conversion is required. Primary-key indexes cover only the
/// original base list, so duplicate keys within incoming lists remain available for validation.
/// Explicit null primary keys match; missing primary keys use the ordinary list strategy.
/// Duplicate YAML mapping keys are retained rather than silently repaired by merging.
///
/// A shallow merge updates root mapping keys regardless of the deep-merge conflict strategy.
/// Non-mapping roots are replaced in shallow mode. Unlike Python's in-place wrapper, this API
/// returns the resulting root, including replacements and root lists.
pub fn merge<V, I, S>(
    mut base: V,
    nexts: I,
    schema: &S,
    options: MergeOptions,
) -> Result<V, MergeError>
where
    V: MergeableValue,
    I: IntoIterator<Item = V>,
    S: MergeSchema,
{
    for next in nexts {
        merge_value(&mut base, next, schema, options, &mut Vec::new(), true)?;
    }
    Ok(base)
}

fn merge_value<V: MergeableValue, S: MergeSchema>(
    base: &mut V,
    mut next: V,
    schema: &S,
    options: MergeOptions,
    path: &mut Vec<String>,
    root: bool,
) -> Result<(), MergeError> {
    if options.mode == MergeMode::Shallow && !root {
        *base = next;
        return Ok(());
    }
    if let Some(next_entries) = next.take_mapping() {
        if let Some(base_entries) = base.take_mapping() {
            let entries = merge_mapping::<V, S>(base_entries, next_entries, schema, options, path)?;
            base.set_mapping(entries);
            return Ok(());
        }
        next.set_mapping(next_entries);
    }
    if options.mode == MergeMode::Shallow {
        *base = next;
        return Ok(());
    }
    if let Some(next_items) = next.take_sequence() {
        if let Some(base_items) = base.take_sequence() {
            if options.list_merge == ListMerge::Replace {
                next.set_sequence(next_items);
                *base = next;
                return Ok(());
            }
            let items = merge_sequence::<V, S>(base_items, next_items, schema, options, path)?;
            base.set_sequence(items);
            return Ok(());
        }
        next.set_sequence(next_items);
    }
    // Full replacements transfer the entire node, including its source and span.
    match options.same_key_strategy {
        SameKeyStrategy::Override => *base = next,
        SameKeyStrategy::MustMatch if base.identity() != next.identity() => {
            return Err(MergeError::Conflict { path: path.clone() });
        }
        SameKeyStrategy::UseExisting | SameKeyStrategy::MustMatch => (),
    }
    Ok(())
}

fn merge_mapping<V: MergeableValue, S: MergeSchema>(
    mut base: Vec<V::Entry>,
    next: Vec<V::Entry>,
    schema: &S,
    options: MergeOptions,
    path: &mut Vec<String>,
) -> Result<Vec<V::Entry>, MergeError> {
    let mut indexes: HashMap<_, Vec<usize>> = HashMap::new();
    for (index, entry) in base.iter().enumerate() {
        indexes.entry(entry.key()).or_default().push(index);
    }
    let mut incoming_counts = HashMap::new();
    for entry in &next {
        *incoming_counts.entry(entry.key()).or_insert(0_usize) += 1;
    }
    for mut entry in next {
        let key = entry.key();
        // Preserve ambiguous YAML mappings for validation instead of hiding duplicate keys.
        let existing = indexes
            .get(&key)
            .filter(|positions| positions.len() == 1 && incoming_counts.get(&key) == Some(&1));
        if let Some(existing) = existing.and_then(|positions| positions.first())
            && let Some(base_entry) = base.get_mut(*existing)
        {
            let component = entry.key_name().unwrap_or("<non-string key>").to_owned();
            path.push(component);
            let incoming_value = entry.take_value();
            merge_value(
                base_entry.value_mut(),
                incoming_value,
                schema,
                options,
                path,
                false,
            )?;
            path.pop();
            // The incoming key keeps its own provenance even when the merged value contains
            // children from multiple inputs (or use_existing keeps the old value).
            std::mem::swap(base_entry.value_mut(), entry.value_mut());
            *base_entry = entry;
        } else {
            base.push(entry);
        }
    }
    Ok(base)
}

fn merge_sequence<V: MergeableValue, S: MergeSchema>(
    mut base: Vec<V::Item>,
    next: Vec<V::Item>,
    schema: &S,
    options: MergeOptions,
    path: &mut Vec<String>,
) -> Result<Vec<V::Item>, MergeError> {
    let primary_key = schema
        .primary_key(path)
        .map_err(|source| MergeError::Schema {
            path: path.clone(),
            source,
        })?;
    let mut unmatched = Vec::with_capacity(next.len());
    if let Some(primary_key) = primary_key {
        let mut indexes: HashMap<_, Vec<usize>> = HashMap::new();
        for (index, item) in base.iter().enumerate() {
            if let Some(identity) = item.value().get(primary_key).map(MergeableValue::identity) {
                indexes.entry(identity).or_default().push(index);
            }
        }
        for item in next {
            let matches = item
                .value()
                .get(primary_key)
                .and_then(|value| indexes.get(&value.identity()));
            if let Some(matches) = matches {
                // Multiple matches indicate invalid duplicate base keys. Keep all entries so
                // validation can report them, rather than silently collapsing them.
                let mut incoming = Some(item.into_value());
                let mut positions = matches.iter().peekable();
                while let Some(index) = positions.next() {
                    if let Some(base_item) = base.get_mut(*index) {
                        let next_value = if positions.peek().is_some() {
                            incoming.clone()
                        } else {
                            incoming.take()
                        };
                        if let Some(next_value) = next_value {
                            path.push(index.to_string());
                            merge_value(
                                base_item.value_mut(),
                                next_value,
                                schema,
                                options,
                                path,
                                false,
                            )?;
                            path.pop();
                        }
                    }
                }
            } else {
                unmatched.push(item);
            }
        }
    } else {
        unmatched = next;
    }
    match options.list_merge {
        ListMerge::Keep => (),
        ListMerge::Append => base.extend(unmatched),
        ListMerge::Prepend => {
            unmatched.extend(base);
            base = unmatched;
        }
        ListMerge::AppendUnique => {
            // AVD's append_rp compares against the original base, retaining repeated next items.
            let identities: HashSet<_> = base.iter().map(|item| item.value().identity()).collect();
            base.extend(
                unmatched
                    .into_iter()
                    .filter(|item| !identities.contains(&item.value().identity())),
            );
        }
        ListMerge::PrependUnique => {
            let identities: HashSet<_> = unmatched
                .iter()
                .map(|item| item.value().identity())
                .collect();
            unmatched.extend(
                base.into_iter()
                    .filter(|item| !identities.contains(&item.value().identity())),
            );
            base = unmatched;
        }
        ListMerge::Replace => base = unmatched,
    }
    Ok(base)
}
