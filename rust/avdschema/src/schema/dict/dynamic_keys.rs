// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Root dynamic-key declaration sources and caller-supplied concrete-key overrides.
//!
//! Source documents use [`DynamicKeySource`] as map keys, so unsupported declarations fail
//! during deserialization. The canonical strings remain the authored JSON/YAML names and the
//! keys in compiled schema tables. They describe paths through input data, not schema edges;
//! [`crate::SchemaPathElement::DynamicKey`] addresses the declaration itself.

use ordermap::OrderMap;

/// Supported root-level sources of dynamic-key schema declarations.
///
/// These sources identify node types, connected endpoints, and network services. Concrete input
/// key names remain unrestricted; this enum identifies the declaration supplying their schema,
/// not those names.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum DynamicKeySource {
    /// Standard node types.
    #[serde(rename = "node_type_keys.key")]
    NodeTypes,
    /// Custom node types.
    #[serde(rename = "custom_node_type_keys.key")]
    CustomNodeTypes,
    /// Standard connected endpoints.
    #[serde(rename = "connected_endpoints_keys.key")]
    ConnectedEndpoints,
    /// Custom connected endpoints.
    #[serde(rename = "custom_connected_endpoints_keys.key")]
    CustomConnectedEndpoints,
    /// Network services.
    #[serde(rename = "network_services_keys.name")]
    NetworkServices,
}

impl DynamicKeySource {
    /// Every supported source, used for parsing and authoring-schema generation.
    const ALL: [Self; 5] = [
        Self::NodeTypes,
        Self::CustomNodeTypes,
        Self::ConnectedEndpoints,
        Self::CustomConnectedEndpoints,
        Self::NetworkServices,
    ];

    /// Input root key and record field supplying concrete dynamic names.
    /// This describes the source data, not a schema address.
    pub const fn components(self) -> [&'static str; 2] {
        match self {
            Self::NodeTypes => ["node_type_keys", "key"],
            Self::CustomNodeTypes => ["custom_node_type_keys", "key"],
            Self::ConnectedEndpoints => ["connected_endpoints_keys", "key"],
            Self::CustomConnectedEndpoints => ["custom_connected_endpoints_keys", "key"],
            Self::NetworkServices => ["network_services_keys", "name"],
        }
    }

    /// Canonical input-data path used as the declaration's key in `dynamic_keys`.
    pub const fn path(self) -> &'static str {
        match self {
            Self::NodeTypes => "node_type_keys.key",
            Self::CustomNodeTypes => "custom_node_type_keys.key",
            Self::ConnectedEndpoints => "connected_endpoints_keys.key",
            Self::CustomConnectedEndpoints => "custom_connected_endpoints_keys.key",
            Self::NetworkServices => "network_services_keys.name",
        }
    }
}

#[cfg(feature = "metaschema")]
impl schemars::JsonSchema for DynamicKeySource {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        "DynamicKeySource".into()
    }

    fn json_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
        // Map-key generation recognizes a flat string enum, but not the `oneOf` form
        // produced by deriving JsonSchema on an enum with documented variants.
        schemars::json_schema!({
            "type": "string",
            "enum": Self::ALL.map(Self::path),
        })
    }
}

/// A schema or caller named a dynamic-key source outside the supported set.
#[derive(Clone, Debug, derive_more::Display)]
#[display("Unsupported dynamic-key source '{path}'")]
pub struct UnsupportedDynamicKeySource {
    /// Unrecognized source declaration path.
    pub path: String,
}

impl std::error::Error for UnsupportedDynamicKeySource {}

impl TryFrom<&str> for DynamicKeySource {
    type Error = UnsupportedDynamicKeySource;

    fn try_from(path: &str) -> Result<Self, Self::Error> {
        Self::ALL
            .into_iter()
            .find(|source| source.path() == path)
            .ok_or_else(|| UnsupportedDynamicKeySource {
                path: path.to_owned(),
            })
    }
}

/// Maps a concrete input key to the dynamic key schema path that should validate it.
///
/// Used when the dynamic key cannot be inferred from input/default schema data,
/// for example when LSP comments identify the intended dynamic-key source.
/// Callers resolving both static and dynamic schema keys should give static
/// schema keys precedence over these overrides.
pub type DynamicKeyOverrides = OrderMap<String, String>;

#[cfg(test)]
mod tests {
    use super::DynamicKeySource;
    use crate::Load as _;
    use crate::StoreSource;

    #[test]
    fn dynamic_sources_have_matching_wire_names_and_data_components() {
        for source in DynamicKeySource::ALL {
            let path = source.path();
            assert_eq!(DynamicKeySource::try_from(path).unwrap(), source);
            assert_eq!(source.components().join("."), path);
            let serialized = serde_json::to_string(&source).unwrap();
            assert_eq!(serialized, format!("\"{path}\""));
            assert_eq!(
                serde_json::from_str::<DynamicKeySource>(&serialized).unwrap(),
                source
            );

            let json = format!(
                r#"{{"model":{{"type":"dict","dynamic_keys":{{"{path}":{{"type":"str"}}}}}}}}"#
            );
            let store = StoreSource::from_json(&json).unwrap();
            let reserialized = serde_json::to_value(&store).unwrap();
            assert_eq!(
                reserialized,
                serde_json::from_str::<serde_json::Value>(&json).unwrap()
            );
        }
    }

    #[test]
    fn source_loading_rejects_unknown_dynamic_declarations_even_when_removed() {
        for removed in [false, true] {
            let json = serde_json::json!({
                "model": {
                    "type": "dict",
                    "dynamic_keys": {
                        "unknown.key": {"type": "str", "deprecation": {"removed": removed}}
                    }
                }
            })
            .to_string();
            let error = StoreSource::from_json(&json).unwrap_err().to_string();
            assert!(error.contains("unknown.key"), "{error}");
        }
    }

    #[cfg(feature = "yaml")]
    #[test]
    fn yaml_map_keys_use_the_same_dynamic_source_contract() {
        let yaml =
            "model:\n  type: dict\n  dynamic_keys:\n    node_type_keys.key:\n      type: str\n";
        let store: StoreSource = yaml_parser::serde::from_str(yaml).unwrap();
        let serialized = yaml_parser::serde::to_string(&store).unwrap();
        let reloaded: StoreSource = yaml_parser::serde::from_str(&serialized).unwrap();
        assert_eq!(
            serde_json::to_value(store).unwrap(),
            serde_json::to_value(reloaded).unwrap()
        );
        assert!(
            yaml_parser::serde::from_str::<StoreSource>(
                &yaml.replace("node_type_keys.key", "unknown.key")
            )
            .is_err()
        );
    }
}
