// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use ordermap::OrderMap;

use super::schema_data::SchemaDataValue;

pub trait Walker<'a>: SchemaDataValue {
    fn walk<'s>(
        &'a self,
        path: impl Iterator<Item = &'s str> + Clone,
        trail: Option<&mut Vec<String>>,
    ) -> OrderMap<Vec<String>, &'a Self>;
}

impl<'a, T> Walker<'a> for T
where
    T: SchemaDataValue,
{
    fn walk<'s>(
        &'a self,
        path: impl Iterator<Item = &'s str> + Clone,
        trail: Option<&mut Vec<String>>,
    ) -> OrderMap<Vec<String>, &'a Self> {
        <T as SchemaDataValue>::walk(self, path, trail)
    }
}
