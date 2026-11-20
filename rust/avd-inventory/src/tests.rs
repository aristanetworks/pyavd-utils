// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

#![allow(
    clippy::unwrap_used,
    clippy::indexing_slicing,
    clippy::panic,
    reason = "Tests inspect known fixture graphs and parsed trees directly"
)]

use avdschema::Store;
use schema_merge::ListMerge;
use schema_merge::MergeMode;
use schema_merge::MergeableValue as _;
use schema_merge::SameKeyStrategy;
use serde_json::json;
use validation::ValidatableValue as _;

use super::*;

fn yaml(text: &str, source: &str, devices: &[&str]) -> YamlFragment {
    YamlFragment {
        yaml: text.to_owned(),
        source: source.into(),
        devices: devices.iter().map(|name| (*name).to_owned()).collect(),
    }
}

fn json_fragment(text: &str, devices: &[&str]) -> JsonFragment {
    JsonFragment {
        json: text.to_owned(),
        source: "json-fixture".into(),
        devices: devices.iter().map(|name| (*name).to_owned()).collect(),
    }
}

fn store() -> Store {
    Store::from_json(
        &json!({"test": {"type": "dict", "keys": {
            "shared": {"type": "dict", "allow_other_keys": true},
            "only": {"type": "int"},
            "mtu": {"type": "int"},
            "invalid": {"type": "int"},
            "items": {"type": "list", "primary_key": "name", "items": {
                "type": "dict", "allow_other_keys": true, "keys": {"name": {"type": "str"}}
            }}
        }}})
        .to_string(),
    )
    .unwrap()
}

#[test]
fn graph_is_built_during_insertion_and_groups_devices_by_their_current_head() {
    let mut inventory = Inventory::<YamlFragment>::new();
    let general = inventory
        .add_fragment(yaml("{}", "same-source", &["a", "b", "c"]))
        .unwrap();
    let site = inventory
        .add_fragment(yaml("{}", "same-source", &["a", "b", "b"]))
        .unwrap();
    let specific = inventory
        .add_fragment(yaml("{}", "same-source", &["a"]))
        .unwrap();
    let common = inventory
        .add_fragment(yaml("{}", "same-source", &["a", "b", "c", "b"]))
        .unwrap();
    assert_ne!(general, site);
    let inputs = inventory.seal();
    assert_eq!(inputs.fragments.len(), 4);
    assert_eq!(inputs.graph.steps.len(), 6);
    assert_eq!(inputs.graph.devices.len(), 3);
    for (device, previous) in [("a", specific), ("b", site), ("c", general)] {
        let head = inputs.graph.devices[device];
        let step = &inputs.graph.steps[head];
        assert_eq!(step.fragment, common);
        assert_eq!(inputs.graph.steps[step.parent.unwrap()].fragment, previous);
    }
}

#[test]
fn shared_execution_matches_independent_device_merges_in_both_formats() {
    let store = store();
    let fragments = [
        json!({"shared": {"base": 1}, "items": [{"name": "a", "old": 1}]}),
        json!({"shared": {"site": 2}, "items": [{"name": "a", "site": 2}]}),
        json!({"only": 3, "items": [{"name": "b"}]}),
        json!({"shared": {"last": 4}, "items": [{"name": "a", "last": 4}]}),
    ];
    let membership = [
        &["a", "b", "c"][..],
        &["a", "b"],
        &["a"],
        &["a", "b", "c", "a", "late"],
    ];
    for (mode, list_merge) in [
        (MergeMode::Deep, ListMerge::Append),
        (MergeMode::Deep, ListMerge::Replace),
        (MergeMode::Deep, ListMerge::AppendUnique),
        (MergeMode::Shallow, ListMerge::Append),
    ] {
        let mut options = BuildOptions::new("test");
        options.merge.mode = mode;
        options.merge.list_merge = list_merge;
        let mut json_inventory = Inventory::<JsonFragment>::new();
        let mut yaml_inventory = Inventory::<YamlFragment>::new();
        for (value, devices) in fragments.iter().zip(membership) {
            let text = value.to_string();
            json_inventory
                .add_fragment(json_fragment(&text, devices))
                .unwrap();
            yaml_inventory
                .add_fragment(yaml(&text, "fixture", devices))
                .unwrap();
        }
        let json_inputs = json_inventory.seal();
        let yaml_inputs = yaml_inventory.seal();
        let json_merged = json_inputs.merge(&store, &options).unwrap();
        let yaml_merged = yaml_inputs.merge(&store, &options).unwrap();
        let schema = schema_merge::StoreSchema::new(&store, "test").unwrap();
        for device in ["a", "b", "c", "late"] {
            let mut values = fragments
                .iter()
                .zip(membership)
                .filter(|(_, devices)| devices.contains(&device))
                .map(|(value, _)| value.clone());
            let expected =
                schema_merge::merge(values.next().unwrap(), values, &schema, options.merge)
                    .unwrap();
            let json_head = json_merged.devices[device];
            let yaml_head = yaml_merged.devices[device];
            assert_eq!(
                json_merged.roots[&json_head].identity(),
                expected.identity()
            );
            assert_eq!(
                yaml_merged.roots[&yaml_head].identity(),
                expected.identity()
            );
        }
    }
}

#[test]
fn shared_terminal_roots_and_shared_subtrees_survive_forks() {
    let store = store();
    let options = BuildOptions::new("test");
    let mut inventory = Inventory::<YamlFragment>::new();
    inventory
        .add_fragment(yaml("shared: {base: 1}", "general", &["a", "b", "c"]))
        .unwrap();
    inventory
        .add_fragment(yaml("shared: {site: 2}", "site", &["a", "b", "c"]))
        .unwrap();
    inventory
        .add_fragment(yaml("only: 3", "specific", &["a"]))
        .unwrap();
    let inputs = inventory.seal();
    let merged = inputs.merge(&store, &options).unwrap();
    assert_eq!(merged.roots.len(), 2);
    assert_eq!(merged.devices["b"], merged.devices["c"]);
    let specific = &merged.roots[&merged.devices["a"]];
    let general = &merged.roots[&merged.devices["b"]];
    assert_eq!(specific.get("only").unwrap().as_i64(), Some(3));
    assert!(general.get("only").is_none());
    let specific_shared = specific.get("shared").unwrap();
    let general_shared = general.get("shared").unwrap();
    let Some(schema_merge::MergedMapping::Expanded(specific_entries)) =
        specific_shared.as_mapping()
    else {
        panic!("expected merged shared mapping")
    };
    let Some(schema_merge::MergedMapping::Expanded(general_entries)) = general_shared.as_mapping()
    else {
        panic!("expected merged shared mapping")
    };
    assert!(std::ptr::eq(specific_entries, general_entries));
    let validated = merged.validate(&store, &options).unwrap();
    assert_eq!(validated.roots.len(), 2);
    assert_eq!(validated.devices["b"], validated.devices["c"]);
}

#[test]
fn validation_preserves_source_spans_and_borrows_inputs_after_merged_wrappers_are_dropped() {
    let store = store();
    let mut options = BuildOptions::new("test");
    options.validation.return_coercion_infos = true;
    // Default return_coerced_data=false must be overridden for the internal product.
    assert!(!options.validation.return_coerced_data);
    let mut inventory = Inventory::<YamlFragment>::new();
    inventory
        .add_fragment(yaml("mtu: '1500'\ninvalid: nope\n", "base", &["a", "b"]))
        .unwrap();
    inventory
        .add_fragment(yaml("mtu: '1600'\n", "specific", &["a"]))
        .unwrap();
    let inputs = inventory.seal();
    let validated = {
        let merged = inputs.merge(&store, &options).unwrap();
        merged.validate(&store, &options).unwrap()
    };
    for (device, mtu, source) in [("a", 1600, "specific"), ("b", 1500, "base")] {
        let result = &validated.roots[&validated.devices[device]];
        assert_eq!(result.result.errors.len(), 1);
        assert_eq!(result.result.infos.len(), 1);
        let original = inputs.fragments[0].value.get("invalid").unwrap();
        assert_eq!(result.result.errors[0].source.as_deref(), Some("base"));
        assert_eq!(result.result.errors[0].span, original.source_span());
        let coerced = result.coerced.as_ref().unwrap();
        let actual_mtu = coerced.get("mtu").unwrap();
        assert_eq!(actual_mtu.as_i64(), Some(mtu));
        assert_eq!(actual_mtu.source().unwrap().as_ref(), source);
        assert!(actual_mtu.source_span().is_some());
        assert!(std::ptr::eq(
            coerced.get("invalid").unwrap().as_original().unwrap(),
            original
        ));
    }
}

#[test]
fn rejected_fragments_do_not_modify_inventory() {
    let mut inventory = Inventory::<YamlFragment>::new();
    let first = inventory
        .add_fragment(yaml("mtu: 1500", "base", &["a"]))
        .unwrap();
    for text in ["", "[", "[]", "null", "---\nmtu: 1\n---\nmtu: 2\n"] {
        let error = inventory
            .add_fragment(yaml(text, "rejected", &["b"]))
            .unwrap_err();
        assert_eq!(error.source.as_ref(), "rejected");
    }
    let second = inventory
        .add_fragment(yaml("only: 1", "next", &["a"]))
        .unwrap();
    assert_ne!(first, second);
    let inputs = inventory.seal();
    assert_eq!(inputs.fragments.len(), 2);
    assert!(!inputs.graph.devices.contains_key("b"));
    let store = store();
    let merged = inputs.merge(&store, &BuildOptions::new("test")).unwrap();
    let root = &merged.roots[&merged.devices["a"]];
    assert_eq!(root.get("mtu").unwrap().as_i64(), Some(1500));
    assert_eq!(root.get("only").unwrap().as_i64(), Some(1));
}

#[test]
fn json_errors_and_nonobject_roots_are_rejected_with_the_source() {
    let mut inventory = Inventory::<JsonFragment>::new();
    let malformed = inventory
        .add_fragment(json_fragment("{", &["a"]))
        .unwrap_err();
    assert_eq!(malformed.source.as_ref(), "json-fixture");
    assert!(matches!(malformed.kind, FragmentErrorKind::Json(_)));
    let nonobject = inventory
        .add_fragment(json_fragment("[]", &["a"]))
        .unwrap_err();
    assert!(matches!(nonobject.kind, FragmentErrorKind::RootNotMapping));
    let inputs = inventory.seal();
    assert!(inputs.fragments.is_empty());
    assert!(inputs.graph.devices.is_empty());
}

#[test]
fn empty_device_lists_apply_nowhere_and_empty_inventories_can_build() {
    let store = store();
    let options = BuildOptions::new("test");
    Inventory::<YamlFragment>::new()
        .build(&store, &options)
        .unwrap();
    let mut inventory = Inventory::<JsonFragment>::new();
    inventory
        .add_fragment(json_fragment("{\"mtu\":1500}", &[]))
        .unwrap();
    let inputs = inventory.seal();
    let merged = inputs.merge(&store, &options).unwrap();
    assert!(merged.roots.is_empty());
    assert!(merged.devices.is_empty());
}

#[test]
fn build_errors_identify_missing_schemas_and_conflicting_fragments() {
    let store = store();
    let schema_error = Inventory::<JsonFragment>::new()
        .build(&store, &BuildOptions::new("missing"))
        .unwrap_err();
    assert!(matches!(schema_error, BuildError::Schema(_)));
    let mut inventory = Inventory::<YamlFragment>::new();
    inventory
        .add_fragment(yaml("mtu: 1500", "base", &["a"]))
        .unwrap();
    inventory
        .add_fragment(yaml("mtu: 1600", "conflict", &["a"]))
        .unwrap();
    let mut options = BuildOptions::new("test");
    options.merge.same_key_strategy = SameKeyStrategy::MustMatch;
    let error = inventory.build(&store, &options).unwrap_err();
    assert!(
        matches!(error, BuildError::Merge { source, error: schema_merge::MergeError::Conflict { path } } if source.as_ref() == "conflict" && path == ["mtu"])
    );
}

#[test]
fn public_build_completes_validation_without_publishing_a_product() {
    let store = store();
    let mut inventory = Inventory::<JsonFragment>::new();
    inventory
        .add_fragment(json_fragment(
            "{\"mtu\":\"1500\",\"invalid\":\"nope\"}",
            &["a", "b"],
        ))
        .unwrap();
    // Findings are data, not execution failures; this temporary API drops them.
    inventory.build(&store, &BuildOptions::new("test")).unwrap();
}
