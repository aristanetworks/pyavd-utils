// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde_json::json;

use super::{ValidatableSequence, ValidatableValue};

#[test]
fn test_serde_json_null() {
    let value = json!(null);
    assert!(value.is_null());
    assert!(value.as_str().is_none());
    assert!(value.as_mapping().is_none());
}

#[test]
fn test_serde_json_string() {
    let value = json!("hello");
    assert!(!value.is_null());
    assert_eq!(value.as_str(), Some("hello"));
    assert!(value.as_i64().is_none());
    assert!(value.as_bool().is_none());
}

#[test]
fn test_serde_json_integer() {
    let value = json!(42);
    // Native as_i64 works
    assert_eq!(value.as_i64(), Some(42));
    // Trait as_str coerces integer to string
    assert_eq!(ValidatableValue::as_str(&value).as_deref(), Some("42"));
}

#[test]
fn test_serde_json_float_to_str_coercion() {
    let value = json!(1.5);
    // Trait as_str coerces float to string
    assert_eq!(ValidatableValue::as_str(&value).as_deref(), Some("1.5"));
}

#[test]
fn test_serde_json_float_value_type_is_not_int() {
    let value = json!(1.5);
    assert_eq!(value.value_type(), crate::feedback::Type::Float);
}

#[test]
fn test_serde_json_bool() {
    let value_true = json!(true);
    let value_false = json!(false);
    assert_eq!(value_true.as_bool(), Some(true));
    assert_eq!(value_false.as_bool(), Some(false));
    // Trait as_str coerces bool to string (Title case to match Python behavior)
    assert_eq!(
        ValidatableValue::as_str(&value_true).as_deref(),
        Some("True")
    );
    assert_eq!(
        ValidatableValue::as_str(&value_false).as_deref(),
        Some("False")
    );
}

#[test]
fn test_serde_json_str_to_int_coercion() {
    let value = json!("123");
    // Trait as_i64 coerces string to int if parseable
    assert_eq!(ValidatableValue::as_i64(&value), Some(123));
    // Invalid string does not coerce
    let invalid = json!("not a number");
    assert!(ValidatableValue::as_i64(&invalid).is_none());
}

#[test]
fn test_serde_json_mapping() {
    let value = json!({
        "name": "Alice",
        "age": 30
    });

    let mapping = value.as_mapping().expect("should be a mapping");
    assert_eq!(mapping.len(), 2);
    assert!(!mapping.is_empty());
    assert!(mapping.contains_key("name"));
    assert!(!mapping.contains_key("missing"));

    let name = mapping.get("name").expect("should have name");
    assert_eq!(name.as_str(), Some("Alice"));

    let keys: Vec<_> = mapping.keys().collect();
    assert!(keys.contains(&&"name".to_owned()));
    assert!(keys.contains(&&"age".to_owned()));
}

#[test]
fn test_serde_json_sequence() {
    let value = json!([1, 2, 3]);

    let seq = value.as_sequence().expect("should be a sequence");
    assert_eq!(seq.len(), 3);
    assert!(!seq.is_empty());

    let items: Vec<i64> = seq.iter().filter_map(|v| v.as_i64()).collect();
    assert_eq!(items, vec![1, 2, 3]);
}

#[test]
fn test_serde_json_get() {
    let value = json!({
        "nested": {
            "key": "value"
        }
    });

    let nested = value.get("nested").expect("should have nested");
    let key = nested.get("key").expect("should have key");
    assert_eq!(key.as_str(), Some("value"));

    assert!(value.get("missing").is_none());
}

#[test]
fn test_serde_json_empty_mapping() {
    let value = json!({});
    let mapping = value.as_mapping().expect("should be a mapping");
    assert!(mapping.is_empty());
    assert_eq!(mapping.len(), 0);
}

#[test]
fn test_serde_json_empty_sequence() {
    let value = json!([]);
    let seq = value.as_sequence().expect("should be a sequence");
    assert!(seq.is_empty());
    assert_eq!(seq.len(), 0);
}
