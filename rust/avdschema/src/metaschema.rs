// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! JSON Schema generation for the AVD source-schema authoring format.

use schemars::generate::SchemaSettings;
use serde_json::Value;

use crate::any::SourceSchema;

const METASCHEMA_ID: &str = "http://avd.sh/development/schema-schema.json";
const COPYRIGHT: &str = "Copyright (c) 2026 Arista Networks, Inc. Use of this source code is governed by the Apache License 2.0 that can be found in the LICENSE file.";

/// Generate a formatted Draft 7 JSON Schema describing [`SourceSchema`].
///
/// The schema is intended for authoring assistance and validation of AVD source-schema
/// documents. Its field definitions are derived from the same Rust types used to deserialize
/// those documents.
pub fn generate_metaschema_json() -> Result<String, serde_json::Error> {
    let mut schema = SchemaSettings::draft07()
        .into_generator()
        .into_root_schema_for::<SourceSchema>();
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
