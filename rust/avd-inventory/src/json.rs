// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! JSON boundary adapter; merging and validation retain their native provenance.

use schema_merge::MergeBackend;
use schema_merge::MergedValue;
use serde::Serialize;
use serde::Serializer;
use serde::ser::Error as _;
use serde::ser::SerializeMap as _;
use serde::ser::SerializeSeq as _;
use validation::ValidatableMapping as _;
use validation::ValidatableMappingPair as _;
use validation::ValidatableSequence as _;
use validation::ValidatableSequenceItem as _;
use validation::ValidatableValue as _;

pub(crate) trait JsonScalar: MergeBackend {
    fn serialize_scalar<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error>;
}

impl JsonScalar for serde_json::Value {
    fn serialize_scalar<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.serialize(serializer)
    }
}

impl JsonScalar for yaml_parser::Node<'_> {
    fn serialize_scalar<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match &self.value {
            yaml_parser::Value::Int(integer) => {
                // Preserve arbitrary-size YAML integers as JSON numbers, including
                // integers outside i128/u128, rather than stringifying them.
                let number: serde_json::Number = integer
                    .to_decimal_string()
                    .parse()
                    .map_err(S::Error::custom)?;
                number.serialize(serializer)
            }
            yaml_parser::Value::Float(value) if !value.is_finite() => Err(S::Error::custom(
                "non-finite YAML floats cannot be represented as JSON",
            )),
            value => value.serialize(serializer),
        }
    }
}

pub(crate) fn encode<V: JsonScalar>(
    value: &MergedValue<'_, V>,
) -> Result<String, serde_json::Error> {
    serde_json::to_string(&Json(value))
}

struct Json<'a, 'input, V: MergeBackend>(&'a MergedValue<'input, V>);

impl<V: JsonScalar> Serialize for Json<'_, '_, V> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        if let Some(mapping) = self.0.as_mapping() {
            let mut output = serializer.serialize_map(Some(mapping.len()))?;
            for pair in mapping.iter() {
                let key = pair.schema_key().ok_or_else(|| {
                    S::Error::custom("non-string YAML mapping keys cannot be represented as JSON")
                })?;
                output.serialize_entry(&key, &Json(&pair.value()))?;
            }
            output.end()
        } else if let Some(sequence) = self.0.as_sequence() {
            let mut output = serializer.serialize_seq(Some(sequence.len()))?;
            for item in sequence.iter() {
                output.serialize_element(&Json(&item.value()))?;
            }
            output.end()
        } else {
            self.0
                .as_scalar()
                .ok_or_else(|| S::Error::custom("expected a scalar"))?
                .serialize_scalar(serializer)
        }
    }
}
