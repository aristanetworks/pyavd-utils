// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Shared backend behavior and YAML provenance tests.

#[cfg(test)]
#[allow(
    clippy::unwrap_used,
    clippy::indexing_slicing,
    clippy::panic,
    reason = "Tests assert known fixture structures directly"
)]
mod tests {
    use std::sync::Arc;

    use avdschema::Store;
    use schema_merge::ListMerge;
    use schema_merge::MergeError;
    use schema_merge::MergeMode;
    use schema_merge::MergeOptions;
    use schema_merge::MergeableValue as _;
    use schema_merge::NoSchema;
    use schema_merge::SameKeyStrategy;
    use schema_merge::StoreSchema;
    use schema_merge::merge;
    use serde_json::Value;
    use serde_json::json;
    use yaml_parser::MappingPair;
    use yaml_parser::Node;
    use yaml_parser::Value as YamlValue;
    use yaml_parser::parse_with_source;

    fn store() -> Store {
        Store::from_json(&json!({
            "test": {
                "type": "dict",
                "keys": {
                    "items": {
                        "type": "list", "primary_key": "name",
                        "allow_duplicate_primary_key": true,
                        "items": {"type": "dict", "keys": {
                            "name": {"type": "str"},
                            "children": {"type": "list", "primary_key": "id", "items": {"type": "dict"}}
                        }}
                    },
                    "interfaces": {"type": "list", "primary_key": "name", "items": {"type": "dict"}},
                    "scalar": {"type": "str"}
                },
                "dynamic_keys": {
                    "names": {"type": "dict", "keys": {
                        "items": {"type": "list", "primary_key": "name", "items": {"type": "dict"}}
                    }}
                }
            }
        }).to_string()).unwrap()
    }

    fn yaml(text: &str, source: &str) -> Node<'static> {
        let (mut documents, errors) = parse_with_source(text, Some(Arc::from(source)));
        assert!(errors.is_empty(), "{errors:?}");
        assert_eq!(documents.len(), 1);
        documents.remove(0)
    }

    #[allow(
        clippy::needless_pass_by_value,
        reason = "Fixture expectations are convenient owned JSON literals"
    )]
    fn check(base: Value, next: Value, options: MergeOptions, expected: Value) {
        let store = store();
        let schema = StoreSchema::new(&store, "test").unwrap();
        let yaml_base = yaml(&base.to_string(), "base");
        let yaml_next = yaml(&next.to_string(), "next");
        assert_eq!(merge(base, [next], &schema, options).unwrap(), expected);
        let merged = merge(yaml_base, [yaml_next], &schema, options).unwrap();
        assert_eq!(serde_json::to_value(&merged.value).unwrap(), expected);
    }

    #[test]
    fn shallow_update_replaces_complete_values() {
        check(
            json!({"nested": {"keep": true, "change": 1}, "items": [{"name": "a", "keep": true}], "keep": 3}),
            json!({"nested": {"change": 2}, "items": [{"name": "a"}], "new": 4}),
            MergeOptions {
                mode: MergeMode::Shallow,
                ..MergeOptions::default()
            },
            json!({"nested": {"change": 2}, "items": [{"name": "a"}], "keep": 3, "new": 4}),
        );
    }

    #[test]
    fn nested_primary_keys_keep_base_order_and_merge_at_the_correct_path() {
        check(
            json!({"items": [
                {"name": "a", "children": [{"id": 10, "old": true}, {"id": 30}]},
                {"name": "b", "keep": true}
            ]}),
            json!({"items": [
                {"name": "a", "children": [{"id": 10, "new": true}, {"id": 20}]},
                {"name": "c"}
            ]}),
            MergeOptions::default(),
            json!({"items": [
                {"name": "a", "children": [{"id": 10, "old": true, "new": true}, {"id": 30}, {"id": 20}]},
                {"name": "b", "keep": true}, {"name": "c"}
            ]}),
        );
    }

    #[test]
    fn list_strategies_match_avd_python() {
        // append_rp compares against base only; it does not remove duplicates inside next.
        for (strategy, expected) in [
            (ListMerge::Replace, json!([2, 3, 3])),
            (ListMerge::Keep, json!([1, 2, 2])),
            (ListMerge::Append, json!([1, 2, 2, 2, 3, 3])),
            (ListMerge::Prepend, json!([2, 3, 3, 1, 2, 2])),
            (ListMerge::AppendUnique, json!([1, 2, 2, 3, 3])),
            (ListMerge::PrependUnique, json!([2, 3, 3, 1])),
        ] {
            check(
                json!({"plain": [1, 2, 2]}),
                json!({"plain": [2, 3, 3]}),
                MergeOptions {
                    list_merge: strategy,
                    ..MergeOptions::default()
                },
                json!({"plain": expected}),
            );
        }
    }

    #[test]
    fn primary_keys_are_merged_before_each_list_strategy_except_replace() {
        let base = json!({"items": [{"name": "a", "old": true}, {"name": "b"}]});
        let next = json!({"items": [{"name": "a", "new": true}, {"name": "c"}]});
        for (strategy, expected) in [
            (ListMerge::Replace, next.clone()),
            (
                ListMerge::Keep,
                json!({"items": [{"name": "a", "old": true, "new": true}, {"name": "b"}]}),
            ),
            (
                ListMerge::Append,
                json!({"items": [{"name": "a", "old": true, "new": true}, {"name": "b"}, {"name": "c"}]}),
            ),
            (
                ListMerge::Prepend,
                json!({"items": [{"name": "c"}, {"name": "a", "old": true, "new": true}, {"name": "b"}]}),
            ),
            (
                ListMerge::AppendUnique,
                json!({"items": [{"name": "a", "old": true, "new": true}, {"name": "b"}, {"name": "c"}]}),
            ),
            (
                ListMerge::PrependUnique,
                json!({"items": [{"name": "c"}, {"name": "a", "old": true, "new": true}, {"name": "b"}]}),
            ),
        ] {
            check(
                base.clone(),
                next.clone(),
                MergeOptions {
                    list_merge: strategy,
                    ..MergeOptions::default()
                },
                expected,
            );
        }
    }

    #[test]
    fn missing_null_and_duplicate_primary_keys_are_not_silently_collapsed() {
        check(
            json!({"items": [{"name": null, "old": true}, {"old": true}]}),
            json!({"items": [{"name": null, "new": true}, {"new": true}, {"name": "duplicate", "first": true}, {"name": "duplicate", "second": true}]}),
            MergeOptions::default(),
            json!({"items": [{"name": null, "old": true, "new": true}, {"old": true}, {"new": true}, {"name": "duplicate", "first": true}, {"name": "duplicate", "second": true}]}),
        );
        check(
            json!({"items": [{"name": "a", "first": true}, {"name": "a", "second": true}]}),
            json!({"items": [{"name": "a", "new": true}]}),
            MergeOptions::default(),
            json!({"items": [{"name": "a", "first": true, "new": true}, {"name": "a", "second": true, "new": true}]}),
        );
    }

    #[test]
    fn scalar_conflicts_and_type_changes_follow_same_key_strategy() {
        for (strategy, expected) in [
            (
                SameKeyStrategy::Override,
                json!({"scalar": 2, "null": 3, "map": [4]}),
            ),
            (
                SameKeyStrategy::UseExisting,
                json!({"scalar": 1, "null": null, "map": {"old": true}}),
            ),
        ] {
            check(
                json!({"scalar": 1, "null": null, "map": {"old": true}}),
                json!({"scalar": 2, "null": 3, "map": [4]}),
                MergeOptions {
                    same_key_strategy: strategy,
                    ..MergeOptions::default()
                },
                expected,
            );
        }
        // An explicit list replacement is independent of the scalar conflict strategy.
        check(
            json!({"items": [{"name": "a", "old": true}]}),
            json!({"items": [{"name": "b"}]}),
            MergeOptions {
                list_merge: ListMerge::Replace,
                same_key_strategy: SameKeyStrategy::UseExisting,
                ..MergeOptions::default()
            },
            json!({"items": [{"name": "b"}]}),
        );
        let options = MergeOptions {
            same_key_strategy: SameKeyStrategy::MustMatch,
            ..MergeOptions::default()
        };
        let error = merge(
            json!({"outer": {"value": 1}}),
            [json!({"outer": {"value": 2}})],
            &NoSchema,
            options,
        )
        .unwrap_err();
        assert!(matches!(error, MergeError::Conflict { path } if path == ["outer", "value"]));
        check(
            json!({"scalar": 1}),
            json!({"scalar": 1}),
            options,
            json!({"scalar": 1}),
        );
    }

    #[test]
    fn schema_less_roots_and_multiple_inputs_are_supported() {
        assert_eq!(
            merge(
                json!({"a": 1}),
                [json!({"a": 2}), json!({"b": 3})],
                &NoSchema,
                MergeOptions::default()
            )
            .unwrap(),
            json!({"a": 2, "b": 3})
        );
        assert_eq!(
            merge(json!([1]), [json!([2])], &NoSchema, MergeOptions::default()).unwrap(),
            json!([1, 2])
        );
        assert_eq!(
            merge(
                json!(null),
                [json!({"a": 1})],
                &NoSchema,
                MergeOptions::default()
            )
            .unwrap(),
            json!({"a": 1})
        );
        assert_eq!(
            merge(
                json!({"a": 1}),
                [json!([2])],
                &NoSchema,
                MergeOptions {
                    mode: MergeMode::Shallow,
                    ..MergeOptions::default()
                }
            )
            .unwrap(),
            json!([2])
        );
    }

    #[test]
    fn unknown_schema_paths_and_dynamic_keys_use_ordinary_lists() {
        check(
            json!({"names": ["custom"], "custom": {"items": [{"name": "a", "old": true}]}}),
            json!({"custom": {"items": [{"name": "a", "new": true}]}}),
            MergeOptions::default(),
            json!({"names": ["custom"], "custom": {"items": [{"name": "a", "old": true}, {"name": "a", "new": true}]}}),
        );
        let store = store();
        assert!(StoreSchema::new(&store, "missing").is_err());
        let schema = StoreSchema::new(&store, "test").unwrap();
        let error = merge(
            json!({"scalar": {"nested": []}}),
            [json!({"scalar": {"nested": [1]}})],
            &schema,
            MergeOptions::default(),
        )
        .unwrap_err();
        assert!(matches!(error, MergeError::Schema { path, .. } if path == ["scalar", "nested"]));
    }

    fn pair<'a>(node: &'a Node<'static>, key: &str) -> &'a MappingPair<'static> {
        // All fixtures below are parsed into owned nodes.
        match &node.value {
            YamlValue::Mapping(pairs) => pairs
                .iter()
                .find(|pair| matches!(&pair.key.value, YamlValue::String(name) if name == key))
                .unwrap(),
            _ => panic!("expected a mapping"),
        }
    }

    #[test]
    fn yaml_partial_merges_preserve_key_value_and_sequence_provenance() {
        let base = yaml(
            "interfaces:\n  - name: Ethernet1\n    description: old # base\n    mtu: 1500\n    tags: [old]\n",
            "base-fixture",
        );
        let next = yaml(
            "interfaces:\n  - name: Ethernet1\n    description: new # next\n    tags: [new]\n  - name: Ethernet2\n",
            "next-fixture",
        );
        let base_original = base.clone();
        let next_original = next.clone();
        let store = store();
        let merged = merge(
            base,
            [next],
            &StoreSchema::new(&store, "test").unwrap(),
            MergeOptions::default(),
        )
        .unwrap();
        assert_eq!(merged.source, base_original.source);
        assert_eq!(merged.span, base_original.span);
        let YamlValue::Sequence(items) = &merged.get("interfaces").unwrap().value else {
            panic!("expected list")
        };
        let YamlValue::Sequence(base_items) = &base_original.get("interfaces").unwrap().value
        else {
            panic!("expected list")
        };
        let YamlValue::Sequence(next_items) = &next_original.get("interfaces").unwrap().value
        else {
            panic!("expected list")
        };
        assert_eq!(items[0].item_span, base_items[0].item_span);
        assert_eq!(
            pair(&items[0].node, "mtu"),
            pair(&base_items[0].node, "mtu")
        );
        assert_eq!(
            pair(&items[0].node, "description"),
            pair(&next_items[0].node, "description")
        );
        assert_eq!(items[1], next_items[1]);
        let YamlValue::Sequence(tags) = &items[0].get("tags").unwrap().value else {
            panic!("expected tags")
        };
        assert_eq!(tags[0].source.as_deref(), Some("base-fixture"));
        assert_eq!(tags[1].source.as_deref(), Some("next-fixture"));
        assert!(Arc::ptr_eq(
            tags[0].source.as_ref().unwrap(),
            base_original.source.as_ref().unwrap()
        ));
        assert!(Arc::ptr_eq(
            tags[1].source.as_ref().unwrap(),
            next_original.source.as_ref().unwrap()
        ));
        assert_eq!(
            pair(&merged, "interfaces").key,
            pair(&next_original, "interfaces").key
        );
        assert_eq!(
            merged.get("interfaces").unwrap().source.as_deref(),
            Some("base-fixture")
        );
    }

    #[test]
    fn yaml_replacement_keeps_incoming_node_metadata() {
        let base = yaml("interfaces: [{name: Ethernet1}]\n", "base");
        let next = yaml(
            "interfaces: # next header\n  - name: Ethernet2 # next comment\n",
            "next",
        );
        for options in [
            MergeOptions {
                mode: MergeMode::Shallow,
                ..MergeOptions::default()
            },
            MergeOptions {
                list_merge: ListMerge::Replace,
                ..MergeOptions::default()
            },
        ] {
            let merged = merge(base.clone(), [next.clone()], &NoSchema, options).unwrap();
            assert_eq!(pair(&merged, "interfaces"), pair(&next, "interfaces"));
        }
        let scalar_next = yaml("interfaces: scalar # replacing list\n", "next");
        let merged = merge(
            base,
            [scalar_next.clone()],
            &NoSchema,
            MergeOptions::default(),
        )
        .unwrap();
        assert_eq!(
            pair(&merged, "interfaces"),
            pair(&scalar_next, "interfaces")
        );
    }

    #[test]
    fn yaml_unique_strategies_ignore_metadata_and_retain_the_chosen_item() {
        let base = yaml("values:\n  - &old {a: 1, b: 2} # old\n", "base");
        let next = yaml("values:\n  - {b: 2, a: 1} # new\n", "next");
        assert_eq!(base.identity(), next.identity());
        for (strategy, chosen) in [
            (ListMerge::AppendUnique, &base),
            (ListMerge::PrependUnique, &next),
        ] {
            let merged = merge(
                base.clone(),
                [next.clone()],
                &NoSchema,
                MergeOptions {
                    list_merge: strategy,
                    ..MergeOptions::default()
                },
            )
            .unwrap();
            assert_eq!(
                merged.get("values").unwrap().value,
                chosen.get("values").unwrap().value
            );
        }
    }

    #[test]
    fn yaml_duplicate_mapping_keys_and_nonstring_keys_are_retained() {
        let base = yaml("duplicate: {old: true}\n1: {old: true}\n", "base");
        let next = yaml(
            "duplicate: first\nduplicate: second\n1: {new: true}\n",
            "next",
        );
        let merged = merge(base, [next], &NoSchema, MergeOptions::default()).unwrap();
        let YamlValue::Mapping(pairs) = merged.value else {
            panic!("expected mapping")
        };
        assert_eq!(pairs.len(), 4);
        assert_eq!(
            serde_json::to_value(&pairs[1].value.value).unwrap(),
            json!({"old": true, "new": true})
        );
        assert_eq!(pairs[2].key.source.as_deref(), Some("next"));
        assert_eq!(pairs[3].key.source.as_deref(), Some("next"));
    }

    #[test]
    fn numeric_identity_matches_python_without_rounding_large_integers() {
        check(
            json!({"plain": [1, 2, 9_007_199_254_740_993_u64]}),
            json!({"plain": [true, 2.0, 9_007_199_254_740_992.0]}),
            MergeOptions {
                list_merge: ListMerge::AppendUnique,
                ..MergeOptions::default()
            },
            json!({"plain": [1, 2, 9_007_199_254_740_993_u64, 9_007_199_254_740_992.0]}),
        );
    }

    #[test]
    fn merged_validation_errors_retain_each_fragments_source_and_span() {
        use validation::StoreValidate as _;
        use validation::feedback::SourceSpan;

        let store = Store::from_json(&json!({"test": {"type": "dict", "keys": {
            "interfaces": {"type": "list", "primary_key": "name", "items": {"type": "dict", "keys": {
                "name": {"type": "str"},
                "old": {"type": "int", "min": 0},
                "updated": {"type": "int", "min": 0},
                "new": {"type": "int", "min": 0}
            }}}
        }}}).to_string()).unwrap();
        let base = yaml(
            "interfaces:\n  - name: Ethernet1\n    old: -1\n    updated: -2\n",
            "base-fixture",
        );
        let next = yaml(
            "interfaces:\n  - name: Ethernet1\n    updated: -3\n    new: -4\n",
            "next-fixture",
        );
        let YamlValue::Sequence(base_items) = &base.get("interfaces").unwrap().value else {
            panic!("expected list")
        };
        let YamlValue::Sequence(next_items) = &next.get("interfaces").unwrap().value else {
            panic!("expected list")
        };
        let spans = [
            (
                "old",
                "base-fixture",
                base_items[0].get("old").unwrap().span,
            ),
            (
                "updated",
                "next-fixture",
                next_items[0].get("updated").unwrap().span,
            ),
            (
                "new",
                "next-fixture",
                next_items[0].get("new").unwrap().span,
            ),
        ];
        let merged = merge(
            base,
            [next],
            &StoreSchema::new(&store, "test").unwrap(),
            MergeOptions::default(),
        )
        .unwrap();
        let result = store.validate_value(&merged, "test", None).unwrap().result;
        assert_eq!(result.errors.len(), 3);
        for (key, source, span) in spans {
            let error = result
                .errors
                .iter()
                .find(|error| error.path.to_string() == format!("interfaces[0].{key}"))
                .unwrap();
            assert_eq!(error.source.as_deref(), Some(source));
            assert_eq!(
                error.span,
                Some(SourceSpan {
                    start: span.start_usize(),
                    end: span.end_usize()
                })
            );
        }
    }

    #[test]
    fn avd_acl_fixture_merges_nested_schema_primary_keys() {
        // Fixtures from aristanetworks/avd, commit 5af3f292dd6acba5bf1b1a42a6225c4d48f49353,
        // python-avd/tests/pyavd/utils/merge (Apache-2.0).
        let store = Store::from_json(&json!({"eos_config": {"type": "dict", "keys": {
            "access_lists": {"type": "list", "primary_key": "name", "items": {"type": "dict", "keys": {
                "sequence_numbers": {"type": "list", "primary_key": "sequence", "items": {"type": "dict"}}
            }}}
        }}}).to_string()).unwrap();
        let base = yaml(include_str!("fixtures/acl1.yml"), "acl1");
        let next = yaml(include_str!("fixtures/acl2.yml"), "acl2");
        let expected = yaml(include_str!("fixtures/acl_merged.yml"), "expected");
        let schema = StoreSchema::new(&store, "eos_config").unwrap();
        let json_base = serde_json::to_value(&base.value).unwrap();
        let json_next = serde_json::to_value(&next.value).unwrap();
        let merged = merge(base, [next], &schema, MergeOptions::default()).unwrap();
        assert_eq!(merged.identity(), expected.identity());
        assert_eq!(
            merge(json_base, [json_next], &schema, MergeOptions::default()).unwrap(),
            serde_json::to_value(&expected.value).unwrap()
        );
    }

    #[test]
    fn json_negative_zero_has_the_same_identity_as_zero() {
        let base: Value = serde_json::from_str(r#"{"values": [-0]}"#).unwrap();
        let next: Value = serde_json::from_str(r#"{"values": [0]}"#).unwrap();
        let options = MergeOptions {
            list_merge: ListMerge::AppendUnique,
            ..MergeOptions::default()
        };
        let merged_json = merge(base.clone(), [next.clone()], &NoSchema, options).unwrap();
        assert_eq!(merged_json, base);
        let merged_yaml = merge(
            yaml(&base.to_string(), "base"),
            [yaml(&next.to_string(), "next")],
            &NoSchema,
            options,
        )
        .unwrap();
        assert_eq!(merged_yaml.identity(), base.identity());
    }
}
