// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use ordermap::OrderMap;

use crate::any::AnySchema;

use super::prefix_keys::{PrefixKeyMatch, PrefixMatchResult, ResolvedPrefixConfig};

pub type DefaultDynamicKeys = OrderMap<String, Vec<String>>;
pub(super) type CachedDefaultDynamicKeys = Option<Box<DefaultDynamicKeys>>;

#[derive(Debug, Clone, PartialEq)]
pub struct DynamicKeyInfo<'a> {
    /// The dynamic key path defined in the schema that led to this dynamic key.
    pub dynamic_key_path: String,
    /// The schema for this dynamic key.
    pub schema: &'a AnySchema,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DictKeyMatch<'a> {
    Static(&'a AnySchema),
    Dynamic(DynamicKeyInfo<'a>),
    Prefix(PrefixKeyMatch<'a>),
    PrefixInvalidSuffix,
}

impl<'a> DictKeyMatch<'a> {
    pub fn schema(&self) -> &'a AnySchema {
        match self {
            Self::Static(schema) => schema,
            Self::Dynamic(dynamic_key_info) => dynamic_key_info.schema,
            Self::Prefix(prefix_key_match) => prefix_key_match.dynamic_key_info.schema,
            Self::PrefixInvalidSuffix => unreachable!("invalid suffix does not carry a schema"),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedDictKeys<'a> {
    pub(super) static_keys: Option<&'a OrderMap<String, AnySchema>>,
    pub(super) dynamic_keys: Option<OrderMap<String, DynamicKeyInfo<'a>>>,
    pub(super) prefix_configs: Option<Vec<ResolvedPrefixConfig<'a>>>,
}

impl<'a> ResolvedDictKeys<'a> {
    pub fn resolve(&self, key: &str) -> Option<DictKeyMatch<'a>> {
        if let Some(static_keys) = self.static_keys
            && let Some(schema) = static_keys.get(key)
        {
            return Some(DictKeyMatch::Static(schema));
        }

        if let Some(dynamic_keys) = &self.dynamic_keys
            && let Some(dynamic_key_info) = dynamic_keys.get(key)
        {
            return Some(DictKeyMatch::Dynamic(dynamic_key_info.clone()));
        }

        self.prefix_configs.as_ref().and_then(|prefix_configs| {
            prefix_configs
                .iter()
                .find_map(|config| match config.resolve_match(key) {
                    Some(PrefixMatchResult::Valid(prefix_key_match)) => {
                        Some(DictKeyMatch::Prefix(prefix_key_match))
                    }
                    Some(PrefixMatchResult::InvalidSuffix) => {
                        Some(DictKeyMatch::PrefixInvalidSuffix)
                    }
                    None => None,
                })
        })
    }
}
