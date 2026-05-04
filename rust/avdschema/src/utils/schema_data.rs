// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use ordermap::OrderMap;
use serde_json::Map;
use serde_json::Value;

pub trait SchemaDataValue<'a>: Sized + Copy {
    type Mapping: SchemaDataMapping<'a, Value = Self> + Copy;

    type Sequence: SchemaDataSequence<'a, Value = Self>;

    fn as_mapping(self) -> Option<Self::Mapping>;

    fn as_sequence(self) -> Option<Self::Sequence>;

    fn as_str(self) -> Option<&'a str>;

    fn walk<'s>(
        self,
        mut path: impl Iterator<Item = &'s str> + Clone,
        mut trail: Option<&mut Vec<String>>,
    ) -> OrderMap<Vec<String>, Self> {
        if let Some(component) = path.next() {
            if let Some(trail) = &mut trail {
                trail.push(component.to_string());
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
                                let mut forked_trail = trail.to_vec();
                                forked_trail.push(index.to_string());
                                forked_trail
                            });
                            value.walk(path.clone(), forked_trail.as_mut())
                        })
                        .collect()
                })
                .unwrap_or_default()
        } else {
            OrderMap::from_iter([(trail.map(|t| t.to_owned()).unwrap_or_default(), self)])
        }
    }
}

pub trait SchemaDataMapping<'a>: Copy {
    type Value: SchemaDataValue<'a> + 'a;

    fn get(&self, key: &str) -> Option<Self::Value>;
}

pub trait SchemaDataSequence<'a> {
    type Value: SchemaDataValue<'a> + 'a;
    type Iter: Iterator<Item = Self::Value>;

    fn iter(&self) -> Self::Iter;
}

impl<'a> SchemaDataValue<'a> for &'a Value {
    type Mapping = &'a serde_json::Map<String, Value>;
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

    fn get(&self, key: &str) -> Option<Self::Value> {
        Map::get(self, key)
    }
}

impl<'a> SchemaDataSequence<'a> for &'a [Value] {
    type Value = &'a Value;
    type Iter = std::slice::Iter<'a, Value>;

    fn iter(&self) -> Self::Iter {
        <[Value]>::iter(self)
    }
}
