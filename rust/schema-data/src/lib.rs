// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Storage-neutral data views used by schema navigation.
//!
//! These traits are shared by schema consumers and data backends so neither
//! needs to depend on the other. JSON views are provided here; YAML views live
//! in `yaml-parser`.

use std::borrow::Cow;

use ordermap::OrderMap;
use serde_json::Map;
use serde_json::Value;

/// A borrowed value supporting dictionary and list navigation.
pub trait SchemaDataValue<'a>: Sized + Copy {
    /// Borrowed dictionary view for this value backend.
    type Mapping: SchemaDataMapping<'a, Value = Self> + Copy;

    /// Borrowed list view for this value backend.
    type Sequence: SchemaDataSequence<'a, Value = Self>;

    /// Return a dictionary view, if this value is a dictionary.
    fn as_mapping(self) -> Option<Self::Mapping>;

    /// Return a list view, if this value is a list.
    fn as_sequence(self) -> Option<Self::Sequence>;

    /// Return the string contents, if this value is a string.
    fn as_str(self) -> Option<&'a str>;

    /// Walk a key path, branching across lists and optionally recording each trail.
    fn walk<'s>(
        self,
        mut path: impl Iterator<Item = &'s str> + Clone,
        mut trail: Option<&mut Vec<String>>,
    ) -> OrderMap<Vec<String>, Self> {
        if let Some(component) = path.next() {
            if let Some(trail) = &mut trail {
                trail.push(component.to_owned());
            }
            if let Some(mapping) = self.as_mapping()
                && let Some(value) = mapping.get(component)
            {
                return value.walk(path, trail);
            }

            self.as_sequence()
                .map(|sequence| {
                    sequence
                        .iter()
                        .enumerate()
                        .filter_map(|(index, element)| {
                            element
                                .as_mapping()
                                .and_then(|mapping| mapping.get(component))
                                .map(|el| (index, el))
                        })
                        .flat_map(|(index, value)| {
                            let mut forked_trail = trail.as_ref().map(|trail| {
                                let mut forked_trail = (*trail).clone();
                                forked_trail.push(index.to_string());
                                forked_trail
                            });
                            value.walk(path.clone(), forked_trail.as_mut())
                        })
                        .collect()
                })
                .unwrap_or_default()
        } else {
            OrderMap::from_iter([(
                trail.map(|trail| trail.to_owned()).unwrap_or_default(),
                self,
            )])
        }
    }
}

/// A borrowed dictionary with string keys.
pub trait SchemaDataMapping<'a>: Copy {
    /// Borrowed value returned by this view.
    type Value: SchemaDataValue<'a> + 'a;
    /// Iterator over dictionary keys.
    type Keys: Iterator<Item = Cow<'a, str>>;

    /// Return the value associated with a key.
    fn get(&self, key: &str) -> Option<Self::Value>;

    /// Iterate over the dictionary keys.
    fn keys(&self) -> Self::Keys;
}

/// A borrowed list of data values.
pub trait SchemaDataSequence<'a> {
    /// Borrowed value returned by this view.
    type Value: SchemaDataValue<'a> + 'a;
    /// Iterator over list values.
    type Iter: Iterator<Item = Self::Value>;

    /// Iterate over the list values.
    fn iter(&self) -> Self::Iter;
}

impl<'a> SchemaDataValue<'a> for &'a Value {
    type Mapping = &'a Map<String, Value>;
    type Sequence = &'a [Value];

    fn as_mapping(self) -> Option<Self::Mapping> {
        self.as_object()
    }

    fn as_sequence(self) -> Option<Self::Sequence> {
        self.as_array().map(Vec::as_slice)
    }

    fn as_str(self) -> Option<&'a str> {
        self.as_str()
    }
}

impl<'a> SchemaDataMapping<'a> for &'a Map<String, Value> {
    type Value = &'a Value;
    type Keys = std::iter::Map<serde_json::map::Keys<'a>, fn(&'a String) -> Cow<'a, str>>;

    fn get(&self, key: &str) -> Option<Self::Value> {
        Map::get(self, key)
    }

    fn keys(&self) -> Self::Keys {
        Map::keys(self).map(|key| Cow::Borrowed(key.as_str()))
    }
}

impl<'a> SchemaDataSequence<'a> for &'a [Value] {
    type Value = &'a Value;
    type Iter = std::slice::Iter<'a, Value>;

    fn iter(&self) -> Self::Iter {
        <[Value]>::iter(self)
    }
}
