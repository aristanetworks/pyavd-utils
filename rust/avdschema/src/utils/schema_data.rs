// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use ordermap::OrderMap;
use serde_json::{Map, Value};

pub trait SchemaDataValue: Sized {
    type Map<'a>: SchemaDataMap<'a, Value = Self> + Copy
    where
        Self: 'a;

    type Sequence<'a>: SchemaDataSequence<'a, Value = Self>
    where
        Self: 'a;

    fn as_map(&self) -> Option<Self::Map<'_>>;

    fn as_sequence(&self) -> Option<Self::Sequence<'_>>;

    fn as_str(&self) -> Option<&str>;

    fn walk<'a, 's>(
        &'a self,
        mut path: impl Iterator<Item = &'s str> + Clone,
        mut trail: Option<&mut Vec<String>>,
    ) -> OrderMap<Vec<String>, &'a Self> {
        if let Some(component) = path.next() {
            if let Some(trail) = &mut trail {
                trail.push(component.to_string());
            }
            if let Some(map) = self.as_map()
                && let Some(value) = map.get(component)
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
                                .as_map()
                                .and_then(|map| map.get(component))
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

pub trait SchemaDataMap<'a>: Copy {
    type Value: SchemaDataValue + 'a;

    fn get(&self, key: &str) -> Option<&'a Self::Value>;
}

pub trait SchemaDataSequence<'a> {
    type Value: SchemaDataValue + 'a;
    type Iter: Iterator<Item = &'a Self::Value>;

    fn iter(&self) -> Self::Iter;
}

impl SchemaDataValue for Value {
    type Map<'a> = &'a serde_json::Map<String, Value>;
    type Sequence<'a> = &'a [Value];

    fn as_map(&self) -> Option<Self::Map<'_>> {
        self.as_object()
    }

    fn as_sequence(&self) -> Option<Self::Sequence<'_>> {
        self.as_array().map(Vec::as_slice)
    }

    fn as_str(&self) -> Option<&str> {
        self.as_str()
    }
}

impl<'a> SchemaDataMap<'a> for &'a Map<String, Value> {
    type Value = Value;

    fn get(&self, key: &str) -> Option<&'a Self::Value> {
        Map::get(self, key)
    }
}

impl<'a> SchemaDataSequence<'a> for &'a Vec<Value> {
    type Value = Value;
    type Iter = std::slice::Iter<'a, Value>;

    fn iter(&self) -> Self::Iter {
        <[Value]>::iter(self)
    }
}

impl<'a> SchemaDataSequence<'a> for &'a [Value] {
    type Value = Value;
    type Iter = std::slice::Iter<'a, Value>;

    fn iter(&self) -> Self::Iter {
        <[Value]>::iter(self)
    }
}
