// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use ordermap::OrderMap;

use crate::any::AnySchema;

pub type DefaultDynamicKeys = OrderMap<String, Vec<String>>;

/// Maps a concrete input key to the dynamic key schema path that should validate it.
///
/// Used when the dynamic key cannot be inferred from input/default schema data,
/// for example when LSP comments identify the intended dynamic-key source.
/// Callers resolving both static and dynamic schema keys should give static
/// schema keys precedence over these overrides.
pub type DynamicKeyOverrides = OrderMap<String, String>;
pub(super) type CachedDefaultDynamicKeys = Option<Box<DefaultDynamicKeys>>;

#[derive(Debug, Clone, PartialEq)]
pub struct DynamicKeyInfo<'a> {
    /// The dynamic key path defined in the schema that led to this dynamic key.
    pub dynamic_key_path: &'a str,
    /// The schema for this dynamic key.
    pub schema: &'a AnySchema,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DictKeyMatch<'a, 'b> {
    Static(&'a AnySchema),
    Dynamic(&'b DynamicKeyInfo<'a>),
    UnknownKey,
}

#[derive(Debug)]
pub struct ResolvedDictKeys<'a> {
    pub static_keys: Option<&'a OrderMap<String, AnySchema>>,
    pub dynamic_keys: Option<OrderMap<String, DynamicKeyInfo<'a>>>,
}

impl<'a> ResolvedDictKeys<'a> {
    pub fn resolve<'b>(&'b self, key: &str) -> DictKeyMatch<'a, 'b> {
        if let Some(static_keys) = self.static_keys
            && let Some(schema) = static_keys.get(key)
        {
            return DictKeyMatch::Static(schema);
        }

        if let Some(dynamic_keys) = &self.dynamic_keys
            && let Some(dynamic_key_info) = dynamic_keys.get(key)
        {
            return DictKeyMatch::Dynamic(dynamic_key_info);
        }

        DictKeyMatch::UnknownKey
    }
}
