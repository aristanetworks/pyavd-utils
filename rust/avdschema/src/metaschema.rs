// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! JSON Schema generation for the AVD source-schema authoring format.

use schemars::generate::SchemaSettings;
use schemars::transform::Transform;
use schemars::transform::transform_subschemas;
use serde_json::Value;

use crate::dict::root::SourceRootSchema;

const METASCHEMA_ID: &str = "http://avd.sh/development/schema-schema.json";
const COPYRIGHT: &str = "Copyright (c) 2026 Arista Networks, Inc. Use of this source code is governed by the Apache License 2.0 that can be found in the LICENSE file.";

/// Removes explicit null values admitted by `Option<T>` from the authoring schema.
///
/// Optional source-model fields may be omitted, but the current authoring format does not accept
/// null as a configured value. Rust deserialization remains more tolerant for compatibility with
/// historical schema documents.
#[derive(Clone, Debug)]
struct DisallowExplicitNull;

impl Transform for DisallowExplicitNull {
    fn transform(&mut self, schema: &mut schemars::Schema) {
        for keyword in ["anyOf", "oneOf"] {
            if let Some(variants) = schema.get_mut(keyword).and_then(Value::as_array_mut) {
                variants.retain(|variant| {
                    variant.as_object().and_then(|object| object.get("type"))
                        != Some(&Value::String("null".to_owned()))
                });
            }
        }

        if let Some(schema_type) = schema.get_mut("type")
            && let Some(types) = schema_type.as_array_mut()
            && types.len() > 1
        {
            types.retain(|type_name| type_name != "null");
            let only_type = (types.len() == 1).then(|| types.remove(0));
            if let Some(only_type) = only_type {
                *schema_type = only_type;
            }
        }

        transform_subschemas(self, schema);
    }
}

/// Generate a formatted Draft 7 JSON Schema describing an AVD schema document root.
///
/// The schema is intended for authoring assistance and validation of AVD source-schema
/// documents. Its field definitions are derived from the same Rust types used to deserialize
/// those documents.
pub fn generate_metaschema_json() -> Result<String, serde_json::Error> {
    // Source models omit `None` when serialized, so the serialization contract represents their
    // canonical authored form. Schemars still makes `Option<T>` nullable; the transform narrows
    // those fields without changing Serde's deliberately tolerant historical deserialization.
    let mut schema = SchemaSettings::draft07()
        .for_serialize()
        .with_transform(DisallowExplicitNull)
        .into_generator()
        .into_root_schema_for::<SourceRootSchema>();
    let object = schema.ensure_object();
    object.insert(
        "title".to_owned(),
        Value::String("Arista AVD Schema".to_owned()),
    );
    object.insert("$id".to_owned(), Value::String(METASCHEMA_ID.to_owned()));
    object.insert("$comment".to_owned(), Value::String(COPYRIGHT.to_owned()));

    let mut json = serde_json::to_string_pretty(&schema)?;
    json.push('\n');
    Ok(json)
}

#[cfg(test)]
mod tests {
    use serde_json::Value;

    use super::COPYRIGHT;
    use super::METASCHEMA_ID;
    use super::generate_metaschema_json;

    #[test]
    fn generated_metaschema_has_stable_root_metadata() {
        let json = generate_metaschema_json().unwrap();
        let schema: Value = serde_json::from_str(&json).unwrap();

        assert_eq!(schema["$schema"], "http://json-schema.org/draft-07/schema#");
        assert_eq!(schema["$id"], METASCHEMA_ID);
        assert_eq!(schema["$comment"], COPYRIGHT);
        assert_eq!(schema["title"], "Arista AVD Schema");
        assert!(schema["definitions"].is_object());
    }
}
