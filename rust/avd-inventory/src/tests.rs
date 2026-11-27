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
        merge: MergeOptions::default(),
    }
}

fn json_fragment(text: &str, devices: &[&str]) -> JsonFragment {
    JsonFragment {
        json: text.to_owned(),
        source: "json-fixture".into(),
        devices: devices.iter().map(|name| (*name).to_owned()).collect(),
        merge: MergeOptions::default(),
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
        let options = ValidationOptions::new("test");
        let merge = MergeOptions {
            mode,
            list_merge,
            ..MergeOptions::default()
        };
        let mut json_inventory = Inventory::<JsonFragment>::new();
        let mut yaml_inventory = Inventory::<YamlFragment>::new();
        for (value, devices) in fragments.iter().zip(membership) {
            let text = value.to_string();
            let mut json_input = json_fragment(&text, devices);
            json_input.merge = merge;
            let mut yaml_input = yaml(&text, "fixture", devices);
            yaml_input.merge = merge;
            json_inventory.add_fragment(json_input).unwrap();
            yaml_inventory.add_fragment(yaml_input).unwrap();
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
                schema_merge::merge(values.next().unwrap(), values, &schema, merge).unwrap();
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
    let options = ValidationOptions::new("test");
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
fn validation_returns_original_source_spans_and_owns_feedback_after_inputs_are_dropped() {
    let store = store();
    let mut options = ValidationOptions::new("test");
    options.validation.return_coercion_infos = true;
    assert!(!options.validation.return_coerced_data);
    let text = "mtu: '1500'\ninvalid: nope\n";
    let mut inventory = Inventory::<YamlFragment>::new();
    inventory
        .add_fragment(yaml(text, "base", &["a", "b"]))
        .unwrap();
    inventory
        .add_fragment(yaml("mtu: '1600'\n", "specific", &["a"]))
        .unwrap();
    let output = inventory.merge_and_validate(&store, &options).unwrap();
    assert!(output.has_errors());
    for device in ["a", "b"] {
        let result = &output.devices[device];
        assert!(result.validated_data.is_none());
        assert_eq!(result.validation.errors.len(), 1);
        assert_eq!(result.validation.infos.len(), 1);
        let error = &result.validation.errors[0];
        assert_eq!(error.source.as_deref(), Some("base"));
        assert_eq!(
            error.span,
            Some(validation::feedback::SourceSpan { start: 21, end: 25 })
        );
        assert_eq!(Vec::<String>::from(error.path.clone()), ["invalid"]);
    }
    assert_eq!(
        output.devices["a"].validation.infos[0].source.as_deref(),
        Some("specific")
    );
    assert_eq!(
        output.devices["b"].validation.infos[0].source.as_deref(),
        Some("base")
    );
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
    let merged = inputs
        .merge(&store, &ValidationOptions::new("test"))
        .unwrap();
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
fn empty_device_lists_apply_nowhere_and_empty_inventories_can_validate() {
    let store = store();
    let options = ValidationOptions::new("test");
    Inventory::<YamlFragment>::new()
        .merge_and_validate(&store, &options)
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
fn execution_errors_identify_missing_schemas_and_conflicting_fragments() {
    let store = store();
    let schema_error = Inventory::<JsonFragment>::new()
        .merge_and_validate(&store, &ValidationOptions::new("missing"))
        .unwrap_err();
    assert!(matches!(schema_error, ValidationError::Schema(_)));
    let mut inventory = Inventory::<YamlFragment>::new();
    inventory
        .add_fragment(yaml("mtu: 1500", "base", &["a"]))
        .unwrap();
    let mut conflicting = yaml("mtu: 1600", "conflict", &["a"]);
    conflicting.merge.same_key_strategy = SameKeyStrategy::MustMatch;
    inventory.add_fragment(conflicting).unwrap();
    let options = ValidationOptions::new("test");
    let error = inventory.merge_and_validate(&store, &options).unwrap_err();
    assert!(
        matches!(error, ValidationError::Merge { source, error: schema_merge::MergeError::Conflict { path } } if source.as_ref() == "conflict" && path == ["mtu"])
    );
}

#[test]
fn any_device_input_error_gates_the_common_build() {
    let store = store();
    let mut inventory = Inventory::<JsonFragment>::new();
    inventory
        .add_fragment(json_fragment(
            "{\"mtu\":\"1500\",\"invalid\":\"nope\"}",
            &["a", "b"],
        ))
        .unwrap();
    inventory
        .add_fragment(json_fragment("{\"mtu\":\"1600\"}", &["c"]))
        .unwrap();
    let output = inventory
        .merge_and_validate(&store, &ValidationOptions::new("test"))
        .unwrap();
    assert!(output.has_errors());
    assert!(output.devices["a"].validated_data.is_none());
    assert!(Arc::ptr_eq(&output.devices["a"], &output.devices["b"]));
    assert_eq!(
        output.devices["c"].validated_data.as_deref(),
        Some(r#"{"mtu":1600}"#)
    );
}

#[test]
fn each_fragment_controls_merge_behavior_without_affecting_other_devices() {
    let store = store();
    let mut inventory = Inventory::<YamlFragment>::new();
    inventory
        .add_fragment(yaml(
            "shared: {base: 1}\nitems: [{name: a, base: 1}]",
            "base",
            &["a", "b"],
        ))
        .unwrap();
    let mut site = yaml(
        "shared: {site: 2}\nitems: [{name: a, site: 2}, {name: b}]",
        "site",
        &["a", "b"],
    );
    site.merge.list_merge = ListMerge::AppendUnique;
    inventory.add_fragment(site).unwrap();
    let mut replace = yaml("items: [{name: c}]", "replace", &["a"]);
    replace.merge.list_merge = ListMerge::Replace;
    inventory.add_fragment(replace).unwrap();
    let mut shallow = yaml("shared: {last: 3}", "shallow", &["a"]);
    shallow.merge.mode = MergeMode::Shallow;
    inventory.add_fragment(shallow).unwrap();
    let output = inventory
        .merge_and_validate(&store, &ValidationOptions::new("test"))
        .unwrap();
    assert!(!output.has_errors());
    for (device, expected) in [
        (
            "a",
            json!({"shared": {"last": 3}, "items": [{"name": "c"}]}),
        ),
        (
            "b",
            json!({"shared": {"base": 1, "site": 2}, "items": [{"name": "a", "base": 1, "site": 2}, {"name": "b"}]}),
        ),
    ] {
        let actual: serde_json::Value =
            serde_json::from_str(output.devices[device].validated_data.as_ref().unwrap()).unwrap();
        assert_eq!(actual, expected);
    }
}

#[test]
fn parallel_and_serial_validation_return_identical_ordered_json_and_diagnostics() {
    let store = store();
    let validate = |workers| {
        let mut inventory = Inventory::<YamlFragment>::new();
        inventory
            .add_fragment(yaml("mtu: '1500'", "base", &["a", "b", "c", "d"]))
            .unwrap();
        inventory
            .add_fragment(yaml("invalid: nope", "invalid", &["b"]))
            .unwrap();
        inventory
            .add_fragment(yaml("only: '42'", "specific", &["c", "d"]))
            .unwrap();
        let mut options = ValidationOptions::new("test");
        options.validation_threads = NonZeroUsize::new(workers);
        options.validation.return_coercion_infos = true;
        inventory.merge_and_validate(&store, &options).unwrap()
    };
    let serial = validate(1);
    let parallel = validate(2);
    assert_eq!(
        serial.devices.keys().collect::<Vec<_>>(),
        parallel.devices.keys().collect::<Vec<_>>()
    );
    for (device, serial_device) in &serial.devices {
        let parallel = &parallel.devices[device];
        assert_eq!(serial_device.validated_data, parallel.validated_data);
        assert_eq!(serial_device.validation.errors, parallel.validation.errors);
        assert_eq!(
            serial_device.validation.warnings,
            parallel.validation.warnings
        );
        assert_eq!(serial_device.validation.infos, parallel.validation.infos);
    }
    assert!(Arc::ptr_eq(&parallel.devices["c"], &parallel.devices["d"]));
}

#[test]
fn distinct_roots_run_inside_the_requested_worker_pool_and_shared_roots_encode_once() {
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;

    use schema_merge::MergedValue;

    static ENCODED: AtomicUsize = AtomicUsize::new(0);
    struct TrackedFragment(JsonFragment);
    impl InventoryFragment for TrackedFragment {
        type Value = serde_json::Value;
        fn decode(self) -> Result<ParsedFragment<Self::Value>, AddFragmentError> {
            self.0.decode()
        }
        #[allow(
            clippy::panic_in_result_fn,
            reason = "Test adapter asserts its executor before encoding"
        )]
        fn encode_json(value: &MergedValue<'_, Self::Value>) -> Result<String, serde_json::Error> {
            assert!(rayon::current_thread_index().is_some());
            assert_eq!(rayon::current_num_threads(), 2);
            ENCODED.fetch_add(1, Ordering::Relaxed);
            JsonFragment::encode_json(value)
        }
    }
    let store = store();
    let mut inventory = Inventory::<TrackedFragment>::new();
    inventory
        .add_fragment(TrackedFragment(json_fragment(
            "{\"mtu\":\"1500\"}",
            &["a", "b", "c", "d"],
        )))
        .unwrap();
    inventory
        .add_fragment(TrackedFragment(json_fragment("{\"only\":1}", &["b"])))
        .unwrap();
    inventory
        .add_fragment(TrackedFragment(json_fragment("{\"only\":2}", &["c"])))
        .unwrap();
    let mut options = ValidationOptions::new("test");
    options.validation_threads = NonZeroUsize::new(2);
    let output = inventory.merge_and_validate(&store, &options).unwrap();
    assert!(!output.has_errors());
    assert_eq!(ENCODED.load(Ordering::Relaxed), 3);
    assert!(Arc::ptr_eq(&output.devices["a"], &output.devices["d"]));
}

#[test]
fn yaml_json_output_keeps_large_numbers_floats_and_coerced_scalars() {
    let store = store();
    let text = "mtu: '1500'\nshared: {large: 12345678901234567890123456789012345678901234567890, unsigned: 18446744073709551615, fraction: 1.25, enabled: true, empty: null}";
    let mut inventory = Inventory::<YamlFragment>::new();
    inventory
        .add_fragment(yaml(text, "fixture", &["a"]))
        .unwrap();
    // Force an expanded custom-data mapping, exercising both borrowed and expanded output.
    inventory
        .add_fragment(yaml("shared: {new: value}", "extra", &["a"]))
        .unwrap();
    let output = inventory
        .merge_and_validate(&store, &ValidationOptions::new("test"))
        .unwrap();
    assert!(!output.has_errors());
    let json: serde_json::Value =
        serde_json::from_str(output.devices["a"].validated_data.as_ref().unwrap()).unwrap();
    assert_eq!(json["mtu"], 1500);
    assert_eq!(
        json["shared"]["large"].to_string(),
        "12345678901234567890123456789012345678901234567890"
    );
    assert_eq!(
        json["shared"]["unsigned"].to_string(),
        "18446744073709551615"
    );
    assert_eq!(json["shared"]["fraction"], 1.25);
    assert_eq!(json["shared"]["enabled"], true);
    assert!(json["shared"]["empty"].is_null());
    assert_eq!(json["shared"]["new"], "value");
}

#[test]
fn json_encoding_rejects_unrepresentable_yaml_instead_of_silently_changing_it() {
    let store = store();
    for text in ["shared: {value: .inf}", "shared: {1: value}"] {
        let mut inventory = Inventory::<YamlFragment>::new();
        inventory
            .add_fragment(yaml(text, "fixture", &["a"]))
            .unwrap();
        assert!(matches!(
            inventory.merge_and_validate(&store, &ValidationOptions::new("test")),
            Err(ValidationError::Json(_))
        ));
    }
}

#[test]
fn keyed_list_merges_keep_error_value_and_warning_key_spans_through_reordering_and_replacement() {
    let store = Store::from_json(
        &json!({"test": {"type": "dict", "keys": {
            "interfaces": {"type": "list", "primary_key": "name", "items": {
                "type": "dict", "keys": {
                    "name": {"type": "str"}, "mtu": {"type": "int"},
                    "legacy": {"type": "str", "deprecation": {"warning": true}}
                }
            }}
        }}})
        .to_string(),
    )
    .unwrap();
    let base = "interfaces:\n  - name: Ethernet1\n    mtu: bad-base\n    legacy: old\n";
    let next = "interfaces:\n  - name: Ethernet2\n    mtu: bad-next\n  - name: Ethernet1\n    legacy: new\n";
    for list_merge in [ListMerge::Prepend, ListMerge::Replace] {
        let mut inventory = Inventory::<YamlFragment>::new();
        inventory
            .add_fragment(yaml(base, "base", &["a", "b"]))
            .unwrap();
        let mut specific = yaml(next, "specific", &["a"]);
        specific.merge.list_merge = list_merge;
        inventory.add_fragment(specific).unwrap();
        let output = inventory
            .merge_and_validate(&store, &ValidationOptions::new("test"))
            .unwrap();
        let result = &output.devices["a"];
        let expected = if list_merge == ListMerge::Replace {
            vec![("specific", next, "bad-next", "0")]
        } else {
            vec![
                ("specific", next, "bad-next", "0"),
                ("base", base, "bad-base", "1"),
            ]
        };
        assert_eq!(result.validation.errors.len(), expected.len());
        for (error, (source, text, token, index)) in result.validation.errors.iter().zip(expected) {
            assert_eq!(error.source.as_deref(), Some(source));
            let start = text.find(token).unwrap();
            assert_eq!(
                error.span,
                Some(validation::feedback::SourceSpan {
                    start,
                    end: start + token.len()
                })
            );
            assert_eq!(
                Vec::<String>::from(error.path.clone()),
                ["interfaces", index, "mtu"]
            );
        }
        assert_eq!(result.validation.warnings.len(), 1);
        let warning = &result.validation.warnings[0];
        assert_eq!(warning.source.as_deref(), Some("specific"));
        let start = next.find("legacy").unwrap();
        assert_eq!(
            warning.span,
            Some(validation::feedback::SourceSpan {
                start,
                end: start + "legacy".len()
            })
        );
        assert_eq!(
            Vec::<String>::from(warning.path.clone()),
            ["interfaces", "1", "legacy"]
        );
        assert_eq!(
            output.devices["b"].validation.errors[0].source.as_deref(),
            Some("base")
        );
    }
}
