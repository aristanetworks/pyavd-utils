// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde_json::Value;
use yaml_parser::Node;
use yaml_parser::Value as YamlValue;

/// Metadata-free identity used by the JSON and YAML merge adapters.
///
/// Mapping order is ignored; sequence order is significant. Numeric equality follows Python:
/// booleans and integral floats compare equal to the corresponding integer. YAML spans, source
/// identifiers, tags, anchors, and comments do not participate in comparison.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SemanticValue(Identity);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Identity {
    Null,
    Integer(String),
    Float(u64),
    String(String),
    Sequence(Vec<SemanticValue>),
    Mapping(Vec<(SemanticValue, SemanticValue)>),
}

impl SemanticValue {
    fn float(value: f64) -> Self {
        if value.is_finite() && value.fract() == 0.0 {
            // Formatting the actual float avoids rounding large integers through f64 before
            // comparing them. In particular, 2**53 + 1 must not equal float(2**53).
            let integer = if value == 0.0 {
                "0".to_owned()
            } else {
                format!("{value:.0}")
            };
            Self(Identity::Integer(integer))
        } else {
            Self(Identity::Float(value.to_bits()))
        }
    }

    fn mapping(mut pairs: Vec<(Self, Self)>) -> Self {
        pairs.sort_unstable();
        Self(Identity::Mapping(pairs))
    }
}

impl From<&Value> for SemanticValue {
    fn from(value: &Value) -> Self {
        match value {
            Value::Null => Self(Identity::Null),
            Value::Bool(value) => Self(Identity::Integer(u8::from(*value).to_string())),
            Value::Number(value) => {
                let text = value.to_string();
                if !text.contains(['.', 'e', 'E']) {
                    Self(Identity::Integer(if text == "-0" {
                        "0".to_owned()
                    } else {
                        text
                    }))
                } else if let Some(float) = value.as_f64() {
                    Self::float(float)
                } else {
                    Self(Identity::Integer(text))
                }
            }
            Value::String(value) => Self(Identity::String(value.clone())),
            Value::Array(items) => Self(Identity::Sequence(items.iter().map(Self::from).collect())),
            Value::Object(pairs) => Self::mapping(
                pairs
                    .iter()
                    .map(|(key, child)| (Self(Identity::String(key.clone())), Self::from(child)))
                    .collect(),
            ),
        }
    }
}

impl From<&Node<'_>> for SemanticValue {
    fn from(value: &Node<'_>) -> Self {
        match &value.value {
            YamlValue::Null => Self(Identity::Null),
            YamlValue::Bool(value) => Self(Identity::Integer(u8::from(*value).to_string())),
            YamlValue::Int(value) => {
                Self(Identity::Integer(value.to_decimal_string().into_owned()))
            }
            YamlValue::Float(value) => Self::float(*value),
            YamlValue::String(value) => Self(Identity::String(value.to_string())),
            YamlValue::Sequence(items) => Self(Identity::Sequence(
                items.iter().map(|item| Self::from(&item.node)).collect(),
            )),
            YamlValue::Mapping(pairs) => Self::mapping(
                pairs
                    .iter()
                    .map(|pair| (Self::from(&pair.key), Self::from(&pair.value)))
                    .collect(),
            ),
        }
    }
}
