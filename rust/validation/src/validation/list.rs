// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::collections::HashMap;

use avdschema::any::AnySchema;
use avdschema::list::List;
use avdschema::list::PrimaryKey;
use avdschema::list::PrimaryKeyComponent;
use avdschema::resolve_ref;

use crate::context::Context;
use crate::feedback::CompositePrimaryKeyValue;
use crate::feedback::Type;
use crate::feedback::Violation;
use crate::validatable::ValidatableSequence;
use crate::validatable::ValidatableValue;
use crate::validation::Validation;

impl Validation for List {
    fn validate<V: ValidatableValue>(&self, value: &V, ctx: &mut Context) -> Option<V::Coerced> {
        if let Some(ref_result) = validate_ref(self, value, ctx) {
            return ref_result;
        }

        if let Some(seq) = value.as_sequence() {
            validate_min_length(self, value, &seq, ctx);
            validate_max_length(self, value, &seq, ctx);
            validate_unique_keys(self, &seq, ctx);
            validate_primary_key_uniqueness(self, &seq, ctx);
            // Validate items and optionally collect coerced results
            let coerced_items = validate_items(self, &seq, ctx);
            coerced_items.map(|items| value.coerce_sequence(items))
        } else if value.is_null() && !ctx.configuration.restrict_null_values {
            ctx.configuration
                .return_coerced_data
                .then(|| value.coerce_null())
        } else {
            ctx.add_error_for(
                value,
                Violation::InvalidType {
                    expected: Type::List,
                    found: value.value_type(),
                },
            );
            None
        }
    }
}

/// Validate against a referenced schema (for unresolved $ref ending with #).
fn validate_ref<V: ValidatableValue>(
    schema: &List,
    value: &V,
    ctx: &mut Context,
) -> Option<Option<V::Coerced>> {
    if let Some(ref_) = schema.base.schema_ref.as_ref()
        && let Ok(AnySchema::List(ref_schema)) = resolve_ref(ref_, ctx.store)
    {
        return Some(ref_schema.validate(value, ctx));
    }
    None
}

/// Validate and optionally coerce sequence items.
/// Returns `Some(coerced_items)` when coercion is enabled, None otherwise.
fn validate_items<'a, S: ValidatableSequence<'a>>(
    schema: &List,
    input: &S,
    ctx: &mut Context,
) -> Option<Vec<<S::Value as ValidatableValue>::Coerced>> {
    let mut coerced = ctx
        .configuration
        .return_coerced_data
        .then(|| Vec::with_capacity(input.len()));
    let required_primary_key_paths = required_primary_key_paths(schema);

    for (i, item) in input.iter().enumerate() {
        ctx.state.path.push(i.to_string());
        validate_item_primary_key(&required_primary_key_paths, item, ctx);
        if let Some(ref mut items) = coerced {
            let coerced_item = validate_item_schema(schema, item, ctx);
            items.push(coerced_item);
        } else {
            validate_item_schema_only(schema, item, ctx);
        }
        ctx.state.path.pop();
    }
    coerced
}

fn validate_item_schema<V: ValidatableValue>(
    schema: &List,
    item: &V,
    ctx: &mut Context,
) -> V::Coerced {
    if let Some(item_schema) = &schema.items {
        // validate() returns Option, but we know return_coerced_data is true here
        item_schema
            .validate(item, ctx)
            .unwrap_or_else(|| item.clone_to_coerced())
    } else {
        // No item schema - preserve the value as-is
        item.clone_to_coerced()
    }
}

fn validate_item_schema_only<V: ValidatableValue>(schema: &List, item: &V, ctx: &mut Context) {
    if let Some(item_schema) = &schema.items {
        let _ = item_schema.validate(item, ctx);
    }
}

fn validate_min_length<'a, V: ValidatableValue, S: ValidatableSequence<'a>>(
    schema: &List,
    value: &V,
    input: &S,
    ctx: &mut Context,
) {
    if let Some(min_length) = schema.min_length {
        let length = input.len() as u64;
        if min_length > length {
            ctx.add_error_for(
                value,
                Violation::LengthBelowMinimum {
                    minimum: min_length,
                    found: length,
                },
            );
        }
    }
}

fn validate_max_length<'a, V: ValidatableValue, S: ValidatableSequence<'a>>(
    schema: &List,
    value: &V,
    input: &S,
    ctx: &mut Context,
) {
    if let Some(max_length) = schema.max_length {
        let length = input.len() as u64;
        if max_length < length {
            ctx.add_error_for(
                value,
                Violation::LengthAboveMaximum {
                    maximum: max_length,
                    found: length,
                },
            );
        }
    }
}

fn required_primary_key_paths(schema: &List) -> Vec<PrimaryKeyPath<'_>> {
    match &schema.primary_key {
        Some(PrimaryKey::Single(field)) => vec![PrimaryKeyPath::new(field)],
        Some(PrimaryKey::Composite(components)) => components
            .iter()
            .filter(|component| component.default_value().is_none())
            .map(|component| PrimaryKeyPath::new(component.path()))
            .collect(),
        None => Vec::new(),
    }
}

fn validate_item_primary_key<V: ValidatableValue>(
    required_primary_key_paths: &[PrimaryKeyPath<'_>],
    item: &V,
    ctx: &mut Context,
) {
    for path in required_primary_key_paths {
        validate_required_primary_key_path(path, item, ctx);
    }
}

fn validate_required_primary_key_path<V: ValidatableValue>(
    path: &PrimaryKeyPath,
    item: &V,
    ctx: &mut Context,
) {
    if get_path(item, path).is_none_or(ValidatableValue::is_null) {
        ctx.add_error_for(
            item,
            Violation::MissingRequiredKey {
                key: path.raw.to_owned(),
            },
        );
    }
}

fn validate_unique_keys<'a, S: ValidatableSequence<'a>>(
    schema: &List,
    items: &S,
    ctx: &mut Context,
) {
    for unique_key in schema.unique_keys.iter().flatten() {
        validate_unique_path(unique_key, items, ctx);
    }
}

fn validate_primary_key_uniqueness<'a, S: ValidatableSequence<'a>>(
    schema: &List,
    items: &S,
    ctx: &mut Context,
) {
    if schema.allow_duplicate_primary_key.unwrap_or_default() {
        return;
    }

    // Primary-key uniqueness needs the item schema to compare values using the
    // same coercion rules as normal validation. For example, an int key should
    // compare "1" and 1 as equal, while a bool key should not accept "true".
    let item_schema = schema.items.as_deref();

    match schema.primary_key.as_ref() {
        Some(PrimaryKey::Single(primary_key)) => {
            validate_primary_key_path_uniqueness(
                item_schema,
                &PrimaryKeyPath::new(primary_key),
                items,
                ctx,
            );
        }
        Some(PrimaryKey::Composite(components)) => {
            validate_composite_primary_key_uniqueness(item_schema, components, items, ctx);
        }
        None => {}
    }
}

fn validate_primary_key_path_uniqueness<'a, S: ValidatableSequence<'a>>(
    item_schema: Option<&AnySchema>,
    primary_key: &PrimaryKeyPath<'_>,
    items: &S,
    ctx: &mut Context,
) {
    type SeenItem<'a, T> = (Vec<String>, &'a T);

    let mut seen_items: HashMap<String, Vec<SeenItem<'a, S::Value>>> = HashMap::new();
    // Resolve once since scalar primary keys use the same schema for every item.
    let primary_key_schema = match schema_for_primary_key_path(item_schema, primary_key) {
        Ok(schema) => schema,
        Err(message) => {
            add_primary_key_schema_error(ctx, message);
            return;
        }
    };

    for (i, item) in items.iter().enumerate() {
        let Some((trail_suffix, value)) = get_path_with_trail(item, primary_key) else {
            continue;
        };
        if value.is_null() {
            continue;
        }

        let mut trail = vec![i.to_string()];
        trail.extend(trail_suffix);
        // None means the value is invalid for the key schema. Item validation
        // will report that type error, so uniqueness should ignore this item.
        let Some(value_str) = value_to_comparison_string(value, primary_key_schema) else {
            continue;
        };

        seen_items
            .entry(value_str)
            .and_modify(|seen_item_trails| {
                for (seen_item_trail, seen_value) in seen_item_trails.iter() {
                    ctx.add_duplicate_violation_pair_for(
                        *seen_value,
                        seen_item_trail,
                        value,
                        &trail,
                    );
                }
                seen_item_trails.push((trail.clone(), value));
            })
            .or_insert_with(|| vec![(trail, value)]);
    }
}

fn validate_unique_path<'a, S: ValidatableSequence<'a>>(
    unique_key: &str,
    items: &S,
    ctx: &mut Context,
) {
    type SeenItem<'a, T> = (Vec<String>, &'a T);

    // Map from stringified value to list of (trail, value) pairs.
    let mut seen_items: HashMap<String, Vec<SeenItem<'a, S::Value>>> = HashMap::new();

    for (i, item) in items.iter().enumerate() {
        for (trail_suffix, value) in item.walk_path(unique_key) {
            // Build the full trail
            let mut trail = vec![i.to_string()];
            trail.extend(trail_suffix);

            // Convert value to string for comparison
            let value_str = value_to_string(value);

            seen_items
                .entry(value_str)
                .and_modify(|seen_item_trails| {
                    // We found at least one other item, so we know we have a duplicate
                    // Add violations for all duplicates in both directions.
                    for (seen_item_trail, seen_value) in seen_item_trails.iter() {
                        ctx.add_duplicate_violation_pair_for(
                            *seen_value,
                            seen_item_trail,
                            value,
                            &trail,
                        );
                    }
                    seen_item_trails.push((trail.clone(), value));
                })
                .or_insert_with(|| vec![(trail, value)]);
        }
    }
}

fn validate_composite_primary_key_uniqueness<'a, S: ValidatableSequence<'a>>(
    item_schema: Option<&AnySchema>,
    components: &[PrimaryKeyComponent],
    items: &S,
    ctx: &mut Context,
) {
    type SeenItem<'a, T> = (Vec<String>, &'a T);

    let Some(component_schemas) =
        composite_primary_key_component_schemas(item_schema, components, ctx)
    else {
        return;
    };

    let mut seen_items: HashMap<Vec<String>, Vec<SeenItem<'a, S::Value>>> = HashMap::new();
    for (i, item) in items.iter().enumerate() {
        // Missing required components and invalid component values are handled
        // by item validation. Such items are skipped for duplicate detection.
        let Some(primary_key_item) = composite_primary_key_item(&component_schemas, item) else {
            continue;
        };
        let trail = vec![i.to_string()];

        seen_items
            .entry(primary_key_item.comparison_value)
            .and_modify(|seen_item_trails| {
                for (seen_item_trail, seen_value) in seen_item_trails.iter() {
                    ctx.add_composite_primary_key_duplicate_violation_pair_for(
                        *seen_value,
                        seen_item_trail,
                        item,
                        &trail,
                        &primary_key_item.feedback_value,
                    );
                }
                seen_item_trails.push((trail.clone(), item));
            })
            .or_insert_with(|| vec![(trail, item)]);
    }
}

struct PrimaryKeyPath<'a> {
    raw: &'a str,
    components: Vec<&'a str>,
}

impl<'a> PrimaryKeyPath<'a> {
    fn new(raw: &'a str) -> Self {
        Self {
            raw,
            components: raw.split('.').collect(),
        }
    }
}

struct PrimaryKeyComponentSchema<'a> {
    component: &'a PrimaryKeyComponent,
    path: PrimaryKeyPath<'a>,
    schema: &'a AnySchema,
}

struct CompositePrimaryKeyItem {
    // Ordered, schema-coerced component values used as the HashMap key.
    comparison_value: Vec<String>,
    // Original/default component values shown in the duplicate diagnostic.
    feedback_value: CompositePrimaryKeyValue,
}

/// Build the comparable tuple and diagnostic value for a composite primary key.
///
/// Returns `None` when any component is missing without a key-only default, or
/// when a component cannot be coerced according to its schema. Validation of
/// the item schema is responsible for reporting those required/type errors.
fn composite_primary_key_item<V: ValidatableValue>(
    component_schemas: &[PrimaryKeyComponentSchema<'_>],
    item: &V,
) -> Option<CompositePrimaryKeyItem> {
    let mut comparison_value = Vec::with_capacity(component_schemas.len());
    let mut feedback_value = ordermap::OrderMap::with_capacity(component_schemas.len());

    for component_schema in component_schemas {
        let value = composite_primary_key_component_value(component_schema, item)?;
        comparison_value.push(value.comparison_value);
        feedback_value.insert(component_schema.path.raw.to_owned(), value.feedback_value);
    }

    Some(CompositePrimaryKeyItem {
        comparison_value,
        feedback_value: feedback_value.into(),
    })
}

struct CompositePrimaryKeyComponentValue {
    // Schema-coerced value used for duplicate comparison.
    comparison_value: String,
    // Original input value, or the key-only default, used in diagnostics.
    feedback_value: crate::feedback::Value,
}

/// Extract one composite-key component from an item.
///
/// The component default is only for identity comparison. It is not injected
/// into the data and should not change rendered/coerced output.
fn composite_primary_key_component_value<V: ValidatableValue>(
    component_schema: &PrimaryKeyComponentSchema<'_>,
    item: &V,
) -> Option<CompositePrimaryKeyComponentValue> {
    if let Some(value) = get_path(item, &component_schema.path).filter(|value| !value.is_null()) {
        return Some(CompositePrimaryKeyComponentValue {
            comparison_value: value_to_comparison_string(value, component_schema.schema)?,
            feedback_value: value.to_feedback_value(),
        });
    }

    let default_value = component_schema.component.default_value()?;
    Some(CompositePrimaryKeyComponentValue {
        comparison_value: value_to_comparison_string(default_value, component_schema.schema)?,
        feedback_value: default_value.clone().into(),
    })
}

fn get_path<'a, V: ValidatableValue>(value: &'a V, path: &PrimaryKeyPath<'_>) -> Option<&'a V> {
    get_path_with_trail(value, path).map(|(_, value)| value)
}

/// Resolve a primary-key dot path through dictionaries only.
///
/// This is intentionally stricter than `walk_path`: primary-key identity must
/// resolve to one value per list item, so paths are not expanded through nested
/// lists like `unique_keys` paths are.
fn get_path_with_trail<'a, V: ValidatableValue>(
    value: &'a V,
    path: &PrimaryKeyPath<'_>,
) -> Option<(Vec<String>, &'a V)> {
    let mut current = value;
    let mut trail = Vec::new();

    for component in &path.components {
        current = current.get(component)?;
        trail.push((*component).to_owned());
    }

    Some((trail, current))
}

fn composite_primary_key_component_schemas<'a>(
    item_schema: Option<&'a AnySchema>,
    components: &'a [PrimaryKeyComponent],
    ctx: &mut Context,
) -> Option<Vec<PrimaryKeyComponentSchema<'a>>> {
    let mut component_schemas = Vec::with_capacity(components.len());
    let mut valid_schema = true;

    for component in components {
        let path = PrimaryKeyPath::new(component.path());
        match schema_for_primary_key_path(item_schema, &path) {
            Ok(schema) => component_schemas.push(PrimaryKeyComponentSchema {
                component,
                path,
                schema,
            }),
            Err(message) => {
                add_primary_key_schema_error(ctx, message);
                valid_schema = false;
            }
        }
    }

    valid_schema.then_some(component_schemas)
}

fn schema_for_primary_key_path<'a>(
    item_schema: Option<&'a AnySchema>,
    path: &PrimaryKeyPath<'_>,
) -> Result<&'a AnySchema, String> {
    let mut current_schema = item_schema.ok_or_else(|| {
        format!(
            "Invalid list primary key '{}': list items schema is missing.",
            path.raw
        )
    })?;

    for component in &path.components {
        let AnySchema::Dict(dict_schema) = current_schema else {
            return Err(format!(
                "Invalid list primary key '{}': component '{component}' traverses a '{}' schema instead of a dict schema.",
                path.raw,
                schema_type(current_schema)
            ));
        };
        let keys = dict_schema.keys.as_ref().ok_or_else(|| {
            format!(
                "Invalid list primary key '{}': component '{component}' traverses a dict schema without keys.",
                path.raw
            )
        })?;
        current_schema = keys.get(*component).ok_or_else(|| {
            format!(
                "Invalid list primary key '{}': component '{component}' was not found in the item schema.",
                path.raw
            )
        })?;
    }

    match current_schema {
        AnySchema::Bool(_) | AnySchema::Int(_) | AnySchema::Str(_) => Ok(current_schema),
        AnySchema::Dict(_) | AnySchema::List(_) => Err(format!(
            "Invalid list primary key '{}': primary-key path resolves to a '{}' schema instead of a scalar bool, int, or str schema.",
            path.raw,
            schema_type(current_schema)
        )),
    }
}

/// Convert a primary-key value into the value used for duplicate comparison.
///
/// When the schema is known, comparison follows the same coercion policy as
/// validation for that schema type. Returning `None` means uniqueness cannot
/// compare the value: either the value is invalid for the key schema, the key
/// schema is complex, or the key schema could not be resolved. Normal item
/// validation or schema-authoring checks own those cases.
fn value_to_comparison_string<V: ValidatableValue>(
    value: &V,
    schema: &AnySchema,
) -> Option<String> {
    match schema {
        AnySchema::Bool(_) => value.as_bool().map(|value| value.to_string()),
        AnySchema::Int(_) => value.as_i64().map(|value| value.to_string()),
        AnySchema::Str(schema) => {
            let value = value.as_str()?.into_owned();
            Some(if schema.convert_to_lower_case.unwrap_or_default() {
                value.to_lowercase()
            } else {
                value
            })
        }
        AnySchema::Dict(_) | AnySchema::List(_) => None,
    }
}

fn add_primary_key_schema_error(ctx: &mut Context, message: String) {
    ctx.add_error_with_span(None, crate::feedback::ErrorIssue::InternalError { message });
}

fn schema_type(schema: &AnySchema) -> String {
    String::from(schema)
}

/// Convert a `ValidatableValue` to a string for comparison purposes.
fn value_to_string<V: ValidatableValue>(value: &V) -> String {
    if let Some(string) = value.as_str() {
        return string.into_owned();
    }
    if value.is_null() {
        return "__NULL__".to_owned();
    }
    // For complex types, we can't easily compare
    "__COMPLEX__".to_owned()
}

#[cfg(test)]
mod tests {
    use avdschema::any::AnySchema;
    use avdschema::boolean::Bool;
    use avdschema::dict::Dict;
    use avdschema::int::Int;
    use avdschema::str::Str;
    use ordermap::OrderMap;
    use serde_json::Value;

    use super::*;
    use crate::Configuration;
    use crate::feedback::CoercionNote;
    use crate::feedback::ErrorIssue;
    use crate::feedback::Feedback;
    use crate::feedback::Value as FeedbackValue;
    use crate::validation::test_utils::get_test_store;

    #[test]
    fn validate_type_ok() {
        let schema = List::default();
        let input = serde_json::json!(["foo", "bar"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_type_err() {
        let schema = List::default();
        let input: Value = true.into();
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::InvalidType {
                    expected: Type::List,
                    found: Type::Bool
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_item_type_ok() {
        let schema = List {
            items: Some(AnySchema::Str(Str::default()).into()),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_item_type_err() {
        let schema = List {
            items: Some(Box::new(Str::default().into())),
            ..Default::default()
        };
        let input = serde_json::json!([{}, {}]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["0".into()].into(),
                    span: None,
                    issue: Violation::InvalidType {
                        expected: Type::Str,
                        found: Type::Dict
                    }
                    .into()
                },
                Feedback {
                    path: vec!["1".into()].into(),
                    span: None,
                    issue: Violation::InvalidType {
                        expected: Type::Str,
                        found: Type::Dict
                    }
                    .into()
                }
            ]
        );
    }

    #[test]
    fn validate_item_type_coercion_ok_err() {
        let schema = List {
            items: Some(Box::new(Str::default().into())),
            ..Default::default()
        };
        let input = serde_json::json!([1, []]);
        let store = get_test_store();
        let configuration = Configuration {
            return_coercion_infos: true,
            return_coerced_data: true,
            ..Default::default()
        };
        let mut ctx = Context::new(&store, Some(&configuration));
        let coerced = schema.validate(&input, &mut ctx);
        // Int 1 is coerced to String "1"
        assert_eq!(
            ctx.result.infos,
            vec![Feedback {
                path: vec!["0".into()].into(),
                span: None,
                issue: CoercionNote {
                    found: 1.into(),
                    made: "1".into()
                }
                .into()
            }]
        );
        // Second item [] is invalid (List, not Str)
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["1".into()].into(),
                span: None,
                issue: Violation::InvalidType {
                    expected: Type::Str,
                    found: Type::List
                }
                .into()
            },]
        );
        // Coerced output should have "1" for first item, original [] preserved for invalid second item
        assert_eq!(coerced, Some(serde_json::json!(["1", []])));
    }

    #[test]
    fn validate_min_length_ok() {
        let schema = List {
            min_length: Some(1),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_min_length_err() {
        let schema = List {
            min_length: Some(3),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::LengthBelowMinimum {
                    minimum: 3,
                    found: 2
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_max_length_ok() {
        let schema = List {
            max_length: Some(2),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_max_length_err() {
        let schema = List {
            max_length: Some(2),
            ..Default::default()
        };
        let input = serde_json::json!(["foo", "bar", "baz"]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec![].into(),
                span: None,
                issue: Violation::LengthAboveMaximum {
                    maximum: 2,
                    found: 3
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_primary_key_ok() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": "v1" }, { "foo": "v2" }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_primary_key_required_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": null }, { "foo": "v1" }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["0".into()].into(),
                span: None,
                issue: Violation::MissingRequiredKey { key: "foo".into() }.into()
            }]
        );
    }

    #[test]
    fn validate_primary_key_not_unique_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": "111" }, { "foo": "222" }, { "foo": "111" }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["0".into(), "foo".into()].into(),
                    span: None,
                    issue: Violation::ValueNotUnique {
                        other_path: vec!["2".into(), "foo".into()].into(),
                        other_span: None,
                    }
                    .into()
                },
                Feedback {
                    path: vec!["2".into(), "foo".into()].into(),
                    span: None,
                    issue: Violation::ValueNotUnique {
                        other_path: vec!["0".into(), "foo".into()].into(),
                        other_span: None,
                    }
                    .into()
                }
            ]
        );
    }

    #[test]
    fn validate_primary_key_three_duplicates_reports_all_pairs_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": "111" }, { "foo": "111" }, { "foo": "111" }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                value_not_unique_feedback(&["0", "foo"], &["1", "foo"]),
                value_not_unique_feedback(&["1", "foo"], &["0", "foo"]),
                value_not_unique_feedback(&["0", "foo"], &["2", "foo"]),
                value_not_unique_feedback(&["2", "foo"], &["0", "foo"]),
                value_not_unique_feedback(&["1", "foo"], &["2", "foo"]),
                value_not_unique_feedback(&["2", "foo"], &["1", "foo"]),
            ]
        );
    }

    #[test]
    fn validate_primary_key_uses_schema_coercion_for_comparison_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([("id".into(), Int::default().into())])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("id".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "id": "1" }, { "id": 1 }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![
                value_not_unique_feedback(&["0", "id"], &["1", "id"]),
                value_not_unique_feedback(&["1", "id"], &["0", "id"]),
            ]
        );
    }

    #[test]
    fn validate_primary_key_ignores_invalid_type_for_uniqueness_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([(
                        "enabled".into(),
                        Bool::default().into(),
                    )])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("enabled".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "enabled": "true" }, { "enabled": "true" }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![
                invalid_type_feedback(&["0", "enabled"], Type::Bool, Type::Str),
                invalid_type_feedback(&["1", "enabled"], Type::Bool, Type::Str),
            ]
        );
    }

    #[test]
    fn validate_primary_key_unresolved_schema_internal_err() {
        let schema = List {
            primary_key: Some("name".into()),
            ..Default::default()
        };
        let input = serde_json::json!([{ "name": "dup" }, { "name": "dup" }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![internal_error_feedback(
                &[],
                "Invalid list primary key 'name': list items schema is missing.",
            )]
        );
    }

    #[test]
    fn validate_allow_duplicate_primary_key_ok() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([("foo".into(), Str::default().into())])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some("foo".into()),
            allow_duplicate_primary_key: Some(true),
            ..Default::default()
        };
        let input = serde_json::json!([{ "foo": "111" }, { "foo": "222" }, { "foo": "111" }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert!(ctx.result.errors.is_empty());
    }

    #[test]
    fn validate_composite_primary_key_ok() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([
                        ("tenant".into(), Str::default().into()),
                        ("vrf".into(), Str::default().into()),
                    ])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some(PrimaryKey::Composite(vec!["tenant".into(), "vrf".into()])),
            ..Default::default()
        };
        let input = serde_json::json!([
            { "tenant": "t1", "vrf": "v1" },
            { "tenant": "t1", "vrf": "v2" }
        ]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_composite_primary_key_nested_paths_with_default_ok() {
        let schema = radius_server_schema();
        let input = serde_json::json!([
            { "host": "r1", "tls": { "enabled": true } },
            { "host": "r1", "tls": { "enabled": true, "port": 2084 } },
            { "host": "r2", "tls": { "enabled": true, "port": 2083 } }
        ]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.errors.is_empty() && ctx.result.infos.is_empty());
    }

    #[test]
    fn validate_composite_primary_key_nested_paths_with_default_not_unique_err() {
        let schema = radius_server_schema();
        let input = serde_json::json!([
            { "host": "r1", "tls": { "enabled": true } },
            { "host": "r1", "tls": { "enabled": true, "port": 2083 } }
        ]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["0".into()].into(),
                    span: None,
                    issue: Violation::CompositePrimaryKeyNotUnique {
                        value: CompositePrimaryKeyValue::from(OrderMap::from_iter([
                            ("host".into(), FeedbackValue::Str("r1".into())),
                            ("tls.enabled".into(), FeedbackValue::Bool(true)),
                            ("tls.port".into(), FeedbackValue::Int(2083)),
                        ])),
                        other_path: vec!["1".into()].into(),
                        other_span: None,
                    }
                    .into()
                },
                Feedback {
                    path: vec!["1".into()].into(),
                    span: None,
                    issue: Violation::CompositePrimaryKeyNotUnique {
                        value: CompositePrimaryKeyValue::from(OrderMap::from_iter([
                            ("host".into(), FeedbackValue::Str("r1".into())),
                            ("tls.enabled".into(), FeedbackValue::Bool(true)),
                            ("tls.port".into(), FeedbackValue::Int(2083)),
                        ])),
                        other_path: vec!["0".into()].into(),
                        other_span: None,
                    }
                    .into()
                }
            ]
        );
    }

    #[test]
    fn validate_composite_primary_key_nested_path_without_default_required_err() {
        let schema = radius_server_schema();
        let input = serde_json::json!([{ "host": "r1", "tls": { "port": 2083 } }]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["0".into()].into(),
                span: None,
                issue: Violation::MissingRequiredKey {
                    key: "tls.enabled".into()
                }
                .into()
            }]
        );
    }

    #[test]
    fn validate_composite_primary_key_not_unique_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([
                        ("tenant".into(), Str::default().into()),
                        ("vrf".into(), Str::default().into()),
                        ("id".into(), Int::default().into()),
                        ("enabled".into(), Bool::default().into()),
                    ])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some(PrimaryKey::Composite(vec![
                "tenant".into(),
                "vrf".into(),
                "id".into(),
                "enabled".into(),
            ])),
            ..Default::default()
        };
        let input = serde_json::json!([
            { "tenant": "t1", "vrf": "v1", "id": 1, "enabled": true },
            { "tenant": "t1", "vrf": "v2", "id": 1, "enabled": true },
            { "tenant": "t1", "vrf": "v1", "id": 1, "enabled": true }
        ]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["0".into()].into(),
                    span: None,
                    issue: Violation::CompositePrimaryKeyNotUnique {
                        value: CompositePrimaryKeyValue::from(OrderMap::from_iter([
                            ("tenant".into(), FeedbackValue::Str("t1".into())),
                            ("vrf".into(), FeedbackValue::Str("v1".into())),
                            ("id".into(), FeedbackValue::Int(1)),
                            ("enabled".into(), FeedbackValue::Bool(true)),
                        ])),
                        other_path: vec!["2".into()].into(),
                        other_span: None,
                    }
                    .into()
                },
                Feedback {
                    path: vec!["2".into()].into(),
                    span: None,
                    issue: Violation::CompositePrimaryKeyNotUnique {
                        value: CompositePrimaryKeyValue::from(OrderMap::from_iter([
                            ("tenant".into(), FeedbackValue::Str("t1".into())),
                            ("vrf".into(), FeedbackValue::Str("v1".into())),
                            ("id".into(), FeedbackValue::Int(1)),
                            ("enabled".into(), FeedbackValue::Bool(true)),
                        ])),
                        other_path: vec!["0".into()].into(),
                        other_span: None,
                    }
                    .into()
                }
            ]
        );
    }

    #[test]
    fn validate_composite_primary_key_three_duplicates_reports_all_pairs_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([
                        ("tenant".into(), Str::default().into()),
                        ("vrf".into(), Str::default().into()),
                    ])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some(PrimaryKey::Composite(vec!["tenant".into(), "vrf".into()])),
            ..Default::default()
        };
        let input = serde_json::json!([
            { "tenant": "t1", "vrf": "v1" },
            { "tenant": "t1", "vrf": "v1" },
            { "tenant": "t1", "vrf": "v1" }
        ]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                composite_primary_key_not_unique_feedback(&["0"], &["1"]),
                composite_primary_key_not_unique_feedback(&["1"], &["0"]),
                composite_primary_key_not_unique_feedback(&["0"], &["2"]),
                composite_primary_key_not_unique_feedback(&["2"], &["0"]),
                composite_primary_key_not_unique_feedback(&["1"], &["2"]),
                composite_primary_key_not_unique_feedback(&["2"], &["1"]),
            ]
        );
    }

    #[test]
    fn validate_composite_primary_key_uses_schema_coercion_for_comparison_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([
                        ("tenant".into(), Str::default().into()),
                        ("id".into(), Int::default().into()),
                    ])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some(PrimaryKey::Composite(vec!["tenant".into(), "id".into()])),
            ..Default::default()
        };
        let input = serde_json::json!([
            { "tenant": "t1", "id": "1" },
            { "tenant": "t1", "id": 1 }
        ]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![
                composite_primary_key_not_unique_feedback_with_value(
                    &["0"],
                    &["1"],
                    &[
                        ("tenant", FeedbackValue::Str("t1".into())),
                        ("id", FeedbackValue::Int(1)),
                    ],
                ),
                composite_primary_key_not_unique_feedback_with_value(
                    &["1"],
                    &["0"],
                    &[
                        ("tenant", FeedbackValue::Str("t1".into())),
                        ("id", FeedbackValue::Int(1)),
                    ],
                ),
            ]
        );
    }

    #[test]
    fn validate_composite_primary_key_ignores_invalid_type_for_uniqueness_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([
                        ("tenant".into(), Str::default().into()),
                        ("enabled".into(), Bool::default().into()),
                    ])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some(PrimaryKey::Composite(vec![
                "tenant".into(),
                "enabled".into(),
            ])),
            ..Default::default()
        };
        let input = serde_json::json!([
            { "tenant": "t1", "enabled": "true" },
            { "tenant": "t1", "enabled": "true" }
        ]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![
                invalid_type_feedback(&["0", "enabled"], Type::Bool, Type::Str),
                invalid_type_feedback(&["1", "enabled"], Type::Bool, Type::Str),
            ]
        );
    }

    fn radius_server_schema() -> List {
        List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([
                        ("host".into(), Str::default().into()),
                        (
                            "tls".into(),
                            Dict {
                                keys: Some(OrderMap::from_iter([
                                    ("enabled".into(), Bool::default().into()),
                                    ("port".into(), Int::default().into()),
                                ])),
                                ..Default::default()
                            }
                            .into(),
                        ),
                    ])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some(PrimaryKey::Composite(vec![
                "host".into(),
                avdschema::list::PrimaryKeyComponentDefinition {
                    path: "tls.enabled".into(),
                    default: None,
                }
                .into(),
                avdschema::list::PrimaryKeyComponentDefinition {
                    path: "tls.port".into(),
                    default: Some(serde_json::json!(2083)),
                }
                .into(),
            ])),
            ..Default::default()
        }
    }

    #[test]
    fn validate_composite_primary_key_required_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([
                        ("tenant".into(), Str::default().into()),
                        ("vrf".into(), Str::default().into()),
                    ])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some(PrimaryKey::Composite(vec!["tenant".into(), "vrf".into()])),
            ..Default::default()
        };
        let input = serde_json::json!([
            { "tenant": "t1", "vrf": null },
            { "vrf": "v1" }
        ]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["0".into()].into(),
                    span: None,
                    issue: Violation::MissingRequiredKey { key: "vrf".into() }.into()
                },
                Feedback {
                    path: vec!["1".into()].into(),
                    span: None,
                    issue: Violation::MissingRequiredKey {
                        key: "tenant".into()
                    }
                    .into()
                }
            ]
        );
    }

    #[test]
    fn validate_allow_duplicate_composite_primary_key_still_requires_fields_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([
                        ("tenant".into(), Str::default().into()),
                        ("vrf".into(), Str::default().into()),
                    ])),
                    ..Default::default()
                }
                .into(),
            )),
            primary_key: Some(PrimaryKey::Composite(vec!["tenant".into(), "vrf".into()])),
            allow_duplicate_primary_key: Some(true),
            ..Default::default()
        };
        let input = serde_json::json!([
            { "tenant": "t1", "vrf": "v1" },
            { "tenant": "t1", "vrf": "v1" },
            { "tenant": "t1" }
        ]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);
        assert!(ctx.result.infos.is_empty());
        assert_eq!(
            ctx.result.errors,
            vec![Feedback {
                path: vec!["2".into()].into(),
                span: None,
                issue: Violation::MissingRequiredKey { key: "vrf".into() }.into()
            }]
        );
    }

    #[test]
    fn validate_unique_keys_through_nested_list_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([(
                        "aliases".into(),
                        List {
                            items: Some(Box::new(
                                Dict {
                                    keys: Some(OrderMap::from_iter([(
                                        "name".into(),
                                        Str::default().into(),
                                    )])),
                                    ..Default::default()
                                }
                                .into(),
                            )),
                            ..Default::default()
                        }
                        .into(),
                    )])),
                    ..Default::default()
                }
                .into(),
            )),
            unique_keys: Some(vec!["aliases.name".into()]),
            ..Default::default()
        };
        let input = serde_json::json!([
            {"aliases": [{"name": "dup"}]},
            {"aliases": [{"name": "dup"}]}
        ]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![
                Feedback {
                    path: vec!["0".into(), "aliases".into(), "0".into(), "name".into()].into(),
                    span: None,
                    issue: Violation::ValueNotUnique {
                        other_path: vec!["1".into(), "aliases".into(), "0".into(), "name".into()]
                            .into(),
                        other_span: None,
                    }
                    .into()
                },
                Feedback {
                    path: vec!["1".into(), "aliases".into(), "0".into(), "name".into()].into(),
                    span: None,
                    issue: Violation::ValueNotUnique {
                        other_path: vec!["0".into(), "aliases".into(), "0".into(), "name".into()]
                            .into(),
                        other_span: None,
                    }
                    .into()
                }
            ]
        );
    }

    #[test]
    fn validate_unique_keys_three_duplicates_reports_all_pairs_err() {
        let schema = List {
            items: Some(Box::new(
                Dict {
                    keys: Some(OrderMap::from_iter([(
                        "name".into(),
                        Str::default().into(),
                    )])),
                    ..Default::default()
                }
                .into(),
            )),
            unique_keys: Some(vec!["name".into()]),
            ..Default::default()
        };
        let input = serde_json::json!([
            {"name": "dup"},
            {"name": "dup"},
            {"name": "dup"}
        ]);
        let store = get_test_store();
        let mut ctx = Context::new(&store, None);
        let _ = schema.validate(&input, &mut ctx);

        assert_eq!(
            ctx.result.errors,
            vec![
                value_not_unique_feedback(&["0", "name"], &["1", "name"]),
                value_not_unique_feedback(&["1", "name"], &["0", "name"]),
                value_not_unique_feedback(&["0", "name"], &["2", "name"]),
                value_not_unique_feedback(&["2", "name"], &["0", "name"]),
                value_not_unique_feedback(&["1", "name"], &["2", "name"]),
                value_not_unique_feedback(&["2", "name"], &["1", "name"]),
            ]
        );
    }

    fn value_not_unique_feedback(path: &[&str], other_path: &[&str]) -> Feedback<ErrorIssue> {
        Feedback {
            path: path.iter().copied().collect(),
            span: None,
            issue: Violation::ValueNotUnique {
                other_path: other_path.iter().copied().collect(),
                other_span: None,
            }
            .into(),
        }
    }

    fn invalid_type_feedback(path: &[&str], expected: Type, found: Type) -> Feedback<ErrorIssue> {
        Feedback {
            path: path.iter().copied().collect(),
            span: None,
            issue: Violation::InvalidType { expected, found }.into(),
        }
    }

    fn internal_error_feedback(path: &[&str], message: &str) -> Feedback<ErrorIssue> {
        Feedback {
            path: path.iter().copied().collect(),
            span: None,
            issue: ErrorIssue::InternalError {
                message: message.to_owned(),
            },
        }
    }

    fn composite_primary_key_not_unique_feedback(
        path: &[&str],
        other_path: &[&str],
    ) -> Feedback<ErrorIssue> {
        composite_primary_key_not_unique_feedback_with_value(
            path,
            other_path,
            &[
                ("tenant", FeedbackValue::Str("t1".into())),
                ("vrf", FeedbackValue::Str("v1".into())),
            ],
        )
    }

    fn composite_primary_key_not_unique_feedback_with_value(
        path: &[&str],
        other_path: &[&str],
        value: &[(&str, FeedbackValue)],
    ) -> Feedback<ErrorIssue> {
        Feedback {
            path: path.iter().copied().collect(),
            span: None,
            issue: Violation::CompositePrimaryKeyNotUnique {
                value: CompositePrimaryKeyValue::from(OrderMap::from_iter(
                    value
                        .iter()
                        .map(|(key, value)| ((*key).to_owned(), value.clone())),
                )),
                other_path: other_path.iter().copied().collect(),
                other_span: None,
            }
            .into(),
        }
    }
}
