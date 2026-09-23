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
    /// AVD Design source classification used by post-validation consolidation.
    pub source: Option<DynamicKeySource>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DynamicKeySource {
    CustomNodeTypes,
    ConnectedEndpoints,
    CustomConnectedEndpoints,
    NetworkServices,
    NodeTypes,
}

impl DynamicKeySource {
    #[must_use]
    pub fn from_schema_path(path: &str) -> Option<Self> {
        match path {
            "custom_node_type_keys.key" => Some(Self::CustomNodeTypes),
            "connected_endpoints_keys.key" => Some(Self::ConnectedEndpoints),
            "custom_connected_endpoints_keys.key" => Some(Self::CustomConnectedEndpoints),
            "network_services_keys.name" => Some(Self::NetworkServices),
            "node_type_keys.key" => Some(Self::NodeTypes),
            _ => None,
        }
    }

    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::CustomNodeTypes => "custom_node_types",
            Self::ConnectedEndpoints => "connected_endpoints",
            Self::CustomConnectedEndpoints => "custom_connected_endpoints",
            Self::NetworkServices => "network_services",
            Self::NodeTypes => "node_types",
        }
    }

    #[must_use]
    pub const fn collection_key(self) -> &'static str {
        match self {
            Self::CustomNodeTypes | Self::NodeTypes => "node_types",
            Self::ConnectedEndpoints | Self::CustomConnectedEndpoints => "connected_endpoints",
            Self::NetworkServices => "network_services",
        }
    }
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
