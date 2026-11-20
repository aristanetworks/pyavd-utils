// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Sequence-item metadata survives validation and explicit item reconstruction.

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use avdschema::Store;
    use serde_json::json;
    use validation::Configuration;
    use validation::StoreValidate as _;
    use validation::ValidatableSequenceItem as _;
    use validation::ValidatableValue as _;
    use yaml_parser::Node;
    use yaml_parser::Value;
    use yaml_parser::parse_with_source;

    fn input() -> Node<'static> {
        let (mut documents, errors) = parse_with_source(
            "items:\n  - # first item\n    value: \"12\"\n  - # second item\n    value: invalid\n  - # third item\n    value: 7\n",
            Some(Arc::from("fixture-a")),
        );
        assert!(errors.is_empty(), "{errors:?}");
        documents.pop().expect("input should contain a document")
    }

    #[test]
    fn yaml_validation_preserves_sequence_item_spans_and_header_comments() {
        for validate_items in [false, true] {
            let mut list_schema = json!({"type": "list"});
            if validate_items {
                list_schema["items"] = json!({"type": "dict", "keys": {
                    "value": {"type": "int"}
                }});
            }
            let store = Store::from_json(
                &json!({"test": {"type": "dict", "keys": {"items": list_schema}}}).to_string(),
            )
            .expect("schema should compile");
            let input = input();
            let configuration = Configuration {
                return_coerced_data: true,
                return_coercion_infos: true,
                ..Default::default()
            };
            let result = store
                .validate_value(&input, "test", Some(&configuration))
                .expect("schema should exist");
            // Invalid items remain in the output with their original metadata.
            assert_eq!(result.result.errors.len(), usize::from(validate_items));
            assert_eq!(result.result.infos.len(), usize::from(validate_items));
            let output = result.coerced.expect("coerced output should be returned");
            let original_items = input.get("items").expect("original list should exist");
            let original_items = original_items.as_sequence().expect("expected a list");
            let output_items = output.get("items").expect("output list should exist");
            let output_items = output_items.as_sequence().expect("expected a list");
            assert_eq!(output_items.len(), original_items.len());
            for (original, coerced) in original_items.iter().zip(output_items) {
                assert_ne!(original.item_span, original.node.span);
                assert!(original.header_comment.is_some());
                assert_eq!(coerced.item_span, original.item_span);
                assert_eq!(coerced.header_comment, original.header_comment);
                assert_eq!(coerced.node.span, original.node.span);
                assert_eq!(coerced.node.source, original.node.source);
            }
            let value = output_items
                .first()
                .expect("first item should exist")
                .node
                .get("value")
                .expect("value should exist");
            if validate_items {
                assert!(matches!(value.value, Value::Int(_)));
                assert_eq!(value.as_i64(), Some(12));
            } else {
                assert!(matches!(value.value, Value::String(_)));
            }
        }
    }

    #[test]
    fn yaml_item_builders_preserve_metadata_when_filtering_and_reordering() {
        let input = input();
        let list = input.get("items").expect("list should exist");
        let sequence = list.as_sequence().expect("expected a list");
        let mut items = sequence.iter();
        let first = items.next().expect("first item should exist");
        let third = items.nth(1).expect("third item should exist");
        // Drop the second item, reverse the remaining items and convert their values.
        let output = list.coerce_sequence(vec![
            third.coerced_item(third.value().coerce_int(30)),
            first.coerced_item(first.value().coerce_int(10)),
        ]);
        let output_items = output.as_sequence().expect("expected an output list");
        assert_eq!(output_items.len(), 2);
        for ((original, expected_value), coerced) in
            [(third, 30), (first, 10)].into_iter().zip(output_items)
        {
            assert_eq!(coerced.item_span, original.item_span);
            assert_eq!(coerced.header_comment, original.header_comment);
            assert_eq!(coerced.node.source, original.node.source);
            assert_eq!(coerced.node.span, original.node.span);
            assert_eq!(coerced.node.as_i64(), Some(expected_value));
        }
    }
}
