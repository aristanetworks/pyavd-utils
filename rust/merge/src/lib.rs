// Copyright (c) 2025 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
#![deny(unused_crate_dependencies)]

//! Merge data in serde_json::Value based on the given AVD schema.

use std::collections::HashMap;

use avdschema::any::AnySchema;

/// Instruction on how to merge any lists at any level.
#[derive(Debug, PartialEq)]
pub enum ListMerge {
    /// Replace the full list without considering primary keys etc.
    Replace,
    /// For lists with primary_key first merge on primary_key and then append remaining items.
    /// For other lists append all items from the new list onto the old.
    Append,
    /// For lists with primary_key first merge on primary_key and then append remaining items.
    /// For other lists append items not already in the list.
    AppendUnique,
    /// Only use the new list if there is no existing list or existing list is `None`.
    KeepExisting,
    /// For lists with primary_key first merge on primary_key and then prepend remaining items.
    /// For other lists prepend all items from the new list onto the old.
    Prepend,
    /// For lists with primary_key first merge on primary_key and then prepend remaining items.
    /// For other lists prepend items not already in the list.
    PrependUnique,
}
impl TryFrom<&str> for ListMerge {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "replace" => Ok(Self::Replace),
            "append" => Ok(Self::Append),
            "append_unique" => Ok(Self::AppendUnique),
            "keep_existing" => Ok(Self::KeepExisting),
            "prepend" => Ok(Self::Prepend),
            "prepend_unique" => Ok(Self::PrependUnique),
            _ => Err(format!("Invalid list merge strategy: {}", value)),
        }
    }
}

/// Merge the nexts values into the base value based on the given schema.
/// The given ListMerge strategy is used for all lists.
/// Limitations:
/// - Does not support dynamic keys.
/// - Coercion should be done on both base and nexts values before running the merge.
pub fn merge(
    mut base: serde_json::Value,
    nexts: Vec<serde_json::Value>,
    schema: Option<&AnySchema>,
    list_merge: &ListMerge,
) -> serde_json::Value {
    for next in nexts {
        base = deepmerge_values(base, next, schema, list_merge);
    }
    base
}

fn deepmerge_values(
    base: serde_json::Value,
    next: serde_json::Value,
    schema: Option<&AnySchema>,
    list_merge: &ListMerge,
) -> serde_json::Value {
    match (base, next) {
        // Deepmerge nested models.
        (serde_json::Value::Array(base_array), serde_json::Value::Array(next_array)) => {
            serde_json::Value::Array(deepmerge_arrays(base_array, next_array, schema, list_merge))
        }
        (serde_json::Value::Object(base_map), serde_json::Value::Object(next_map)) => {
            serde_json::Value::Object(deepmerge_maps(base_map, next_map, schema, list_merge))
        }
        // Anything else overwrites.
        (_, next) => next,
    }
}

fn deepmerge_arrays(
    mut base: Vec<serde_json::Value>,
    next: Vec<serde_json::Value>,
    schema: Option<&AnySchema>,
    list_merge: &ListMerge,
) -> Vec<serde_json::Value> {
    // Handle list merge strategies that can quickly conclude and does not require any schema information.
    match list_merge {
        ListMerge::Append => (),
        ListMerge::AppendUnique => (),
        ListMerge::Prepend => (),
        ListMerge::PrependUnique => (),
        ListMerge::Replace => return next,
        ListMerge::KeepExisting => return base,
    }

    // Handle list merge strategies that requires schema information.
    if let Some(AnySchema::List(list_schema)) = schema
        && let (Some(primary_key), Some(items_schema)) =
            (&list_schema.primary_key, list_schema.items.as_ref())
    {
        return deepmerge_arrays_with_primary_key(
            base,
            next,
            items_schema,
            primary_key,
            list_merge,
        );
    }

    // Handle list merge strategies that does not require any schema information.
    match list_merge {
        ListMerge::Append => base.extend(next),
        ListMerge::AppendUnique => {
            for next_item in next {
                if !base.contains(&next_item) {
                    base.push(next_item)
                }
            }
        }
        ListMerge::Prepend => {
            let mut result = next;
            result.extend(base);
            return result;
        }
        ListMerge::PrependUnique => {
            let mut result = Vec::new();
            for next_item in next {
                if !base.contains(&next_item) {
                    result.push(next_item)
                }
            }
            result.extend(base);
            return result;
        }
        ListMerge::Replace => unreachable!(),
        ListMerge::KeepExisting => unreachable!(),
    }
    base
}

fn deepmerge_arrays_with_primary_key(
    mut base: Vec<serde_json::Value>,
    next: Vec<serde_json::Value>,
    items_schema: &AnySchema,
    primary_key: &str,
    list_merge: &ListMerge,
) -> Vec<serde_json::Value> {
    // Merge lists of dicts matching on primary key.
    // First build an index of existing primary keys.
    let mut primary_key_to_index_map: HashMap<serde_json::Value, usize> = Default::default();
    for (index, base_item) in base.iter().enumerate() {
        // If the item has the primary key set, add the index to the index map, so we can deepmerge it if needed.
        if let serde_json::Value::Object(base_map) = base_item
            && let Some(primary_key_value) = base_map.get(primary_key)
        {
            primary_key_to_index_map.insert(primary_key_value.clone(), index);
        }
    }

    let mut new_items = Vec::new();

    // Next merge each next item onto the base
    for next_item in next {
        // If the next_item is a map and we can find the primary key and the same primary key is in the existing data,
        // we will deepmerge the matching items.

        if let serde_json::Value::Object(ref next_map) = next_item
            && let Some(primary_key_value) = next_map.get(primary_key)
            && let Some(existing_index) = primary_key_to_index_map.get(primary_key_value)
        {
            let index = existing_index.to_owned();
            // Update existing item by first inserting a null item (placeholder) into the vec,
            let tmp_item = serde_json::Value::Null;
            let existing_item = core::mem::replace(&mut base[index], tmp_item);
            // Then do the merge
            let merged_item =
                deepmerge_values(existing_item, next_item, Some(items_schema), list_merge);
            // Now put back the merged item into the same position in the vec.
            _ = core::mem::replace(&mut base[index], merged_item);
            continue;
        };
        // For all other cases we just add to the list of new items which will be prepended or appended later.
        new_items.push(next_item)
    }
    match list_merge {
        ListMerge::Append => base.extend(new_items),
        ListMerge::AppendUnique => {
            for next_item in new_items {
                if !base.contains(&next_item) {
                    base.push(next_item)
                }
            }
        }
        ListMerge::Replace => unreachable!(),
        ListMerge::KeepExisting => unreachable!(),
        ListMerge::Prepend => {
            new_items.extend(base);
            return new_items;
        }
        ListMerge::PrependUnique => {
            let mut result = Vec::new();
            for next_item in new_items {
                if !result.contains(&next_item) {
                    result.push(next_item)
                }
            }
            result.extend(base);
            return result;
        }
    }
    base
}

fn deepmerge_maps(
    mut base: serde_json::Map<String, serde_json::Value>,
    next: serde_json::Map<String, serde_json::Value>,
    schema: Option<&AnySchema>,
    list_merge: &ListMerge,
) -> serde_json::Map<String, serde_json::Value> {
    let keys_schemas = schema.and_then(|schema| match schema {
        AnySchema::Dict(dict_schema) => dict_schema.keys.as_ref(),
        _ => None,
    });

    next.into_iter().for_each(|(key, next_value)| {
        match base.entry(&key) {
            serde_json::map::Entry::Occupied(mut entry) => {
                // Update existing value by first inserting a null item (placeholder) into the map,
                let tmp_item = serde_json::Value::Null;
                let existing_value = core::mem::replace(entry.get_mut(), tmp_item);
                // Then do the merge
                let merged_value = deepmerge_values(
                    existing_value,
                    next_value,
                    keys_schemas.and_then(|keys| keys.get(&key)),
                    list_merge,
                );
                // Now put back the merged item back into the map.
                _ = core::mem::replace(entry.get_mut(), merged_value);
            }
            serde_json::map::Entry::Vacant(entry) => {
                entry.insert(next_value);
            }
        }
    });
    base
}

#[cfg(test)]
mod tests {
    use std::sync::OnceLock;

    use avdschema::{Load as _, Store};
    use serde_json::json;

    use super::*;

    static STORE: OnceLock<Store> = OnceLock::new();

    fn get_store() -> &'static Store {
        STORE.get_or_init(|| {
            let file = test_schema_store::get_store_gz_path();
            Store::from_file(Some(file)).unwrap().as_resolved()
        })
    }

    #[test]
    fn merge_array_append() {
        let array1 = json!([1, 2, "three"]);
        let array2 = json!([2, "four"]);
        let merged_value = merge(array1, vec![array2], None, &ListMerge::Append);
        assert_eq!(merged_value, json!([1, 2, "three", 2, "four"]));
    }

    #[test]
    fn merge_array_append_unique() {
        let array1 = json!([1, 2, "three"]);
        let array2 = json!([2, "four"]);
        let merged_value = merge(array1, vec![array2], None, &ListMerge::AppendUnique);
        assert_eq!(merged_value, json!([1, 2, "three", "four"]));
    }

    #[test]
    fn merge_array_replace() {
        let array1 = json!([1, 2, "three"]);
        let array2 = json!([2, "four"]);
        let merged_value = merge(array1, vec![array2], None, &ListMerge::Replace);
        assert_eq!(merged_value, json!([2, "four"]));
    }

    #[test]
    fn merge_array_keep_existing() {
        let array1 = json!([1, 2, "three"]);
        let array2 = json!([2, "four"]);
        let merged_value = merge(array1, vec![array2], None, &ListMerge::KeepExisting);
        assert_eq!(merged_value, json!([1, 2, "three"]));
    }

    #[test]
    fn merge_array_prepend() {
        let array1 = json!([1, 2, "three"]);
        let array2 = json!([2, "four"]);
        let merged_value = merge(array1, vec![array2], None, &ListMerge::Prepend);
        assert_eq!(merged_value, json!([2, "four", 1, 2, "three"]));
    }

    #[test]
    fn merge_array_prepend_unique() {
        let array1 = json!([1, 2, "three"]);
        let array2 = json!([2, "four"]);
        let merged_value = merge(array1, vec![array2], None, &ListMerge::PrependUnique);
        assert_eq!(merged_value, json!(["four", 1, 2, "three"]));
    }

    #[test]
    fn merge_ethernet_interfaces_append() {
        let base = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet1",
                    "speed": "forced 10000full",
                },
                {
                    "name": "Ethernet2",
                    "description": "from array1",
                    "tx_queues": [
                        {"id": 1, "random_detect": {"ecn": {"count": true}}}
                    ]
                },
                {
                    "name": "Ethernet3",
                    "some_invalid_key": "merge_is_not_validation",
                },
            ]
        });
        let next = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet2",
                    "description": "from array2",
                    "shutdown": true,
                    "tx_queues": [
                        {"id": 1, "random_detect": {"ecn": {"count": false}}},
                        {"id": 2, "random_detect": {"ecn": {"count": true}}}
                    ]
                },
                {
                    "name": "Ethernet4",
                },
            ]
        });
        let store = get_store();
        let merged_value = merge(
            base,
            vec![next],
            Some(&store.eos_cli_config_gen),
            &ListMerge::Append,
        );
        assert_eq!(
            merged_value,
            json!({
                "ethernet_interfaces": [
                    {
                        "name": "Ethernet1",
                        "speed": "forced 10000full",
                    },
                    {
                        "name": "Ethernet2",
                        "description": "from array2",
                        "shutdown": true,
                        "tx_queues": [
                            {"id": 1, "random_detect": {"ecn": {"count": false}}},
                            {"id": 2, "random_detect": {"ecn": {"count": true}}}
                        ]
                    },
                    {
                        "name": "Ethernet3",
                        "some_invalid_key": "merge_is_not_validation",
                    },
                    {
                        "name": "Ethernet4",
                    },
                ]
            })
        );
    }
    #[test]
    fn merge_ethernet_interfaces_no_schema() {
        let base = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet1",
                    "speed": "forced 10000full",
                },
                {
                    "name": "Ethernet2",
                    "description": "from array1",
                    "tx_queues": [
                        {"id": 1, "random_detect": {"ecn": {"count": true}}}
                    ]
                },
                {
                    "name": "Ethernet3",
                    "some_invalid_key": "merge_is_not_validation",
                },
            ]
        });
        let next = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet2",
                    "description": "from array2",
                    "shutdown": true,
                    "tx_queues": [
                        {"id": 1, "random_detect": {"ecn": {"count": false}}},
                        {"id": 2, "random_detect": {"ecn": {"count": true}}}
                    ]
                },
                {
                    "name": "Ethernet4",
                },
            ]
        });
        let merged_value = merge(base, vec![next], None, &ListMerge::Append);
        assert_eq!(
            merged_value,
            json!({
                "ethernet_interfaces": [
                    {
                        "name": "Ethernet1",
                        "speed": "forced 10000full",
                    },
                    {
                        "name": "Ethernet2",
                        "description": "from array1",
                        "tx_queues": [
                            {"id": 1, "random_detect": {"ecn": {"count": true}}}
                        ]
                    },
                    {
                        "name": "Ethernet3",
                        "some_invalid_key": "merge_is_not_validation",
                    },
                    {
                        "name": "Ethernet2",
                        "description": "from array2",
                        "shutdown": true,
                        "tx_queues": [
                            {"id": 1, "random_detect": {"ecn": {"count": false}}},
                            {"id": 2, "random_detect": {"ecn": {"count": true}}}
                        ]
                    },
                    {
                        "name": "Ethernet4",
                    },
                ]
            })
        );
    }

    #[test]
    fn merge_ethernet_interfaces_append_unique() {
        let base = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet1",
                    "speed": "forced 10000full",
                },
                {
                    "name": "Ethernet2",
                    "description": "from base",
                },
            ]
        });
        let next = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet2",
                    "description": "from next",
                    "shutdown": true,
                },
                {
                    "name": "Ethernet3",
                },
                {
                    "name": "Ethernet3",
                    "description": "this is not validation"
                },
            ]
        });
        let store = get_store();
        let merged_value = merge(
            base,
            vec![next],
            Some(&store.eos_cli_config_gen),
            &ListMerge::AppendUnique,
        );
        assert_eq!(
            merged_value,
            json!({
                "ethernet_interfaces": [
                    {
                        "name": "Ethernet1",
                        "speed": "forced 10000full",
                    },
                    {
                        "name": "Ethernet2",
                        "description": "from next",
                        "shutdown": true,
                    },
                    {
                        "name": "Ethernet3",
                    },
                    {
                        "name": "Ethernet3",
                        "description": "this is not validation"
                    },
                ]
            })
        );
    }

    #[test]
    fn merge_ethernet_interfaces_prepend() {
        let base = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet1",
                    "speed": "forced 10000full",
                },
                {
                    "name": "Ethernet2",
                    "description": "from base",
                },
            ]
        });
        let next = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet2",
                    "description": "from next",
                    "shutdown": true,
                },
                {
                    "name": "Ethernet3",
                    "description": "this is not validation"
                },
            ]
        });
        let store = get_store();
        let merged_value = merge(
            base,
            vec![next],
            Some(&store.eos_cli_config_gen),
            &ListMerge::Prepend,
        );
        assert_eq!(
            merged_value,
            json!({
                "ethernet_interfaces": [
                    {
                        "name": "Ethernet3",
                        "description": "this is not validation"
                    },
                    {
                        "name": "Ethernet1",
                        "speed": "forced 10000full",
                    },
                    {
                        "name": "Ethernet2",
                        "description": "from next",
                        "shutdown": true,
                    },
                ]
            })
        );
    }

    #[test]
    fn merge_ethernet_interfaces_prepend_unique() {
        let base = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet1",
                    "speed": "forced 10000full",
                },
                {
                    "name": "Ethernet2",
                    "description": "from base",
                },
            ]
        });
        let next = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet2",
                    "description": "from next",
                    "shutdown": true,
                },
                {
                    "name": "Ethernet3",
                    "description": "this is not validation"
                },
                {
                    "name": "Ethernet3",
                    "description": "this is not validation"
                },
            ]
        });
        let store = get_store();
        let merged_value = merge(
            base,
            vec![next],
            Some(&store.eos_cli_config_gen),
            &ListMerge::PrependUnique,
        );
        assert_eq!(
            merged_value,
            json!({
                "ethernet_interfaces": [
                    {
                        "name": "Ethernet3",
                        "description": "this is not validation"
                    },
                    {
                        "name": "Ethernet1",
                        "speed": "forced 10000full",
                    },
                    {
                        "name": "Ethernet2",
                        "description": "from next",
                        "shutdown": true,
                    },
                ]
            })
        );
    }

    #[test]
    fn merge_ethernet_interfaces_replace() {
        let base = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet1",
                    "speed": "forced 10000full",
                },
                {
                    "name": "Ethernet2",
                    "description": "from base",
                },
            ]
        });
        let next = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet3",
                },
            ]
        });
        let store = get_store();
        let merged_value = merge(
            base,
            vec![next],
            Some(&store.eos_cli_config_gen),
            &ListMerge::Replace,
        );
        assert_eq!(
            merged_value,
            json!({
                "ethernet_interfaces": [
                    {
                        "name": "Ethernet3",
                    },
                ]
            })
        );
    }

    #[test]
    fn merge_ethernet_interfaces_keep_existing() {
        let base = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet1",
                    "speed": "forced 10000full",
                },
                {
                    "name": "Ethernet2",
                    "description": "from base",
                },
            ]
        });
        let next = json!({
            "ethernet_interfaces": [
                {
                    "name": "Ethernet3",
                },
            ]
        });
        let store = get_store();
        let merged_value = merge(
            base,
            vec![next],
            Some(&store.eos_cli_config_gen),
            &ListMerge::KeepExisting,
        );
        assert_eq!(
            merged_value,
            json!({
                "ethernet_interfaces": [
                    {
                        "name": "Ethernet1",
                        "speed": "forced 10000full",
                    },
                    {
                        "name": "Ethernet2",
                        "description": "from base",
                    },
                ]
            })
        );
    }

    #[test]
    fn list_merge_try_from_str() {
        assert_eq!(ListMerge::try_from("replace").unwrap(), ListMerge::Replace);
        assert_eq!(ListMerge::try_from("append").unwrap(), ListMerge::Append);
        assert_eq!(
            ListMerge::try_from("append_unique").unwrap(),
            ListMerge::AppendUnique
        );
        assert_eq!(
            ListMerge::try_from("keep_existing").unwrap(),
            ListMerge::KeepExisting
        );
        assert_eq!(ListMerge::try_from("prepend").unwrap(), ListMerge::Prepend);
        assert_eq!(
            ListMerge::try_from("prepend_unique").unwrap(),
            ListMerge::PrependUnique
        );
        assert!(ListMerge::try_from("invalid").is_err());
    }
}
