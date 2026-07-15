// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;
use serde_with::skip_serializing_none;

use super::any::AnySchema;
use super::base::documentation_options::DocumentationOptions;
use crate::any::Shortcuts;
use crate::base::Deprecation;
use crate::schema::base::Base;

/// AVD Schema for list data.
#[skip_serializing_none]
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct List {
    /// Schema for list items
    pub items: Option<Box<AnySchema>>,
    pub min_length: Option<u64>,
    pub max_length: Option<u64>,
    /// Name of a primary key, or composite primary-key components, in a list of dictionaries.
    /// Primary-key components without a default are implicitly required. The full primary key
    /// must be unique between list elements unless duplicate primary keys are explicitly allowed.
    pub primary_key: Option<PrimaryKey>,
    /// List of keys or dot-notation path keys that must be unique in addition to `primary_key`.
    pub unique_keys: Option<Vec<String>>,
    /// Set to True to allow duplicate `primary_key` values for a list of dicts.
    /// Useful when primary key is only used for triggering documentation.
    /// NOTE! Should only be used in `avd_design` inputs since we cannot merge on primary key if there are duplicate entries.
    pub allow_duplicate_primary_key: Option<bool>,
    #[serde(flatten)]
    pub base: Base<Vec<Value>>,
    pub documentation_options: Option<DocumentationOptions>,
}

/// Primary-key definition for a list of dictionaries.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_more::From)]
#[serde(untagged)]
pub enum PrimaryKey {
    /// A single primary-key field.
    Single(String),
    /// Composite primary-key fields.
    Composite(Vec<PrimaryKeyComponent>),
}

/// One component in a composite primary key.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_more::From)]
#[serde(untagged)]
pub enum PrimaryKeyComponent {
    /// A required primary-key path.
    Path(String),
    /// A primary-key path with optional identity-only default behavior.
    Definition(PrimaryKeyComponentDefinition),
}

/// Object form of a composite primary-key component.
#[skip_serializing_none]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PrimaryKeyComponentDefinition {
    pub path: String,
    pub default: Option<Value>,
}

impl PrimaryKey {
    /// Return the primary-key paths in configured order.
    pub fn fields(&self) -> Vec<&str> {
        match self {
            Self::Single(field) => vec![field.as_str()],
            Self::Composite(components) => {
                components.iter().map(PrimaryKeyComponent::path).collect()
            }
        }
    }

    /// Return `true` when this key is composed of multiple fields.
    pub fn is_composite(&self) -> bool {
        matches!(self, Self::Composite(_))
    }
}

impl PrimaryKeyComponent {
    /// Return the primary-key path.
    pub fn path(&self) -> &str {
        match self {
            Self::Path(path) => path,
            Self::Definition(definition) => &definition.path,
        }
    }

    /// Return the optional identity-only default for this primary-key path.
    pub fn default_value(&self) -> Option<&Value> {
        match self {
            Self::Path(_) => None,
            Self::Definition(definition) => definition.default.as_ref(),
        }
    }
}

impl From<&str> for PrimaryKeyComponent {
    fn from(value: &str) -> Self {
        Self::Path(value.to_owned())
    }
}

impl From<&str> for PrimaryKey {
    fn from(value: &str) -> Self {
        Self::Single(value.to_owned())
    }
}

impl std::fmt::Display for PrimaryKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Single(field) => f.write_str(field),
            Self::Composite(components) => {
                for (index, component) in components.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }
                    f.write_str(component.path())?;
                }
                Ok(())
            }
        }
    }
}

impl Shortcuts for List {
    fn is_required(&self) -> bool {
        self.base.required.unwrap_or_default()
    }

    fn deprecation(&self) -> &Option<Deprecation> {
        &self.base.deprecation
    }
    fn default_(&self) -> Option<Value> {
        self.base
            .default
            .as_ref()
            .map(|value| Value::Array(value.to_owned()))
    }
}

impl<'x> TryFrom<&'x AnySchema> for &'x List {
    type Error = &'static str;

    fn try_from(value: &'x AnySchema) -> Result<Self, Self::Error> {
        match value {
            AnySchema::List(list) => Ok(list),
            _ => Err("Unable to convert from AnySchema to List. Invalid Schema type."),
        }
    }
}

#[cfg(test)]
mod tests {
    use serde::Deserialize as _;
    use serde_json::json;

    use super::List;
    use super::PrimaryKey;
    use super::PrimaryKeyComponent;
    use super::PrimaryKeyComponentDefinition;
    use crate::any::AnySchema;
    use crate::dict::Dict;

    #[test]
    fn try_from_anyschema_ok() {
        let anyschema = &AnySchema::List(List::default());
        let result: Result<&List, _> = anyschema.try_into();
        assert!(result.is_ok());
    }
    #[test]
    fn try_from_anyschema_err() {
        let anyschema = &AnySchema::Dict(Dict::default());
        let result: Result<&List, _> = anyschema.try_into();
        assert!(result.is_err());
    }

    #[test]
    fn deserialize_list_without_primary_key_ok() {
        let schema = List::deserialize(json!({})).unwrap();
        assert_eq!(schema.primary_key, None);
    }

    #[test]
    fn deserialize_scalar_primary_key_ok() {
        let schema = List::deserialize(json!({"primary_key": "name"})).unwrap();
        assert_eq!(schema.primary_key, Some(PrimaryKey::Single("name".into())));
        assert_eq!(
            serde_json::to_value(schema).unwrap()["primary_key"],
            json!("name")
        );
    }

    #[test]
    fn deserialize_composite_primary_key_ok() {
        let schema = List::deserialize(json!({"primary_key": ["tenant", "vrf"]})).unwrap();
        assert_eq!(
            schema.primary_key,
            Some(PrimaryKey::Composite(vec![
                PrimaryKeyComponent::Path("tenant".into()),
                PrimaryKeyComponent::Path("vrf".into()),
            ]))
        );
        assert_eq!(
            serde_json::to_value(schema).unwrap()["primary_key"],
            json!(["tenant", "vrf"])
        );
    }

    #[test]
    fn deserialize_single_field_composite_primary_key_ok() {
        let result = List::deserialize(json!({"primary_key": ["name"]}));
        assert_eq!(
            result.unwrap().primary_key,
            Some(PrimaryKey::Composite(vec![PrimaryKeyComponent::Path(
                "name".into()
            )]))
        );
    }

    #[test]
    fn deserialize_composite_primary_key_with_component_defaults_ok() {
        let schema = List::deserialize(json!({
            "primary_key": [
                "host",
                {"path": "tls.enabled"},
                {"path": "tls.port", "default": 2083}
            ]
        }))
        .unwrap();

        assert_eq!(
            schema.primary_key,
            Some(PrimaryKey::Composite(vec![
                PrimaryKeyComponent::Path("host".into()),
                PrimaryKeyComponent::Definition(PrimaryKeyComponentDefinition {
                    path: "tls.enabled".into(),
                    default: None,
                }),
                PrimaryKeyComponent::Definition(PrimaryKeyComponentDefinition {
                    path: "tls.port".into(),
                    default: Some(json!(2083)),
                }),
            ]))
        );
        assert_eq!(
            serde_json::to_value(schema).unwrap()["primary_key"],
            json!([
                "host",
                {"path": "tls.enabled"},
                {"path": "tls.port", "default": 2083}
            ])
        );
    }
}
