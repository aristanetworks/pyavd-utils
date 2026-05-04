// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

// ============================================================================
// Event Emitter Tests - Testing event generation directly
// ============================================================================
// ============================================================================
// Event Emitter Tests - Testing event generation directly
// ============================================================================

#[test]
fn test_emit_e76z() {
    // Test E76Z: Aliases as implicit block mapping keys
    // Input: &a a: &b b\n*b : *a\n
    // Expected: Both aliases (*b key and *a value) emit Alias events
    let input = "&a a: &b b\n*b : *a\n";
    let (events, _errors) = crate::emit_events(input);

    let alias_events: Vec<_> = events
        .iter()
        .filter(|event| matches!(event, crate::Event::Alias { .. }))
        .collect();
    assert_eq!(
        alias_events.len(),
        2,
        "Expected 2 Alias events (for *b key and *a value)"
    );
}

mod event_generation {
    use crate::error::ErrorKind;
    use crate::event::CollectionStyle;
    use crate::event::Event;
    use crate::event::ScalarStyle;

    /// Helper to get events from YAML input using the emitter
    fn events_from(input: &str) -> Vec<Event<'static>> {
        let (events, errors) = crate::emit_events(input);
        assert!(
            errors.is_empty(),
            "unexpected emitter errors for input:\n{input}\nerrors: {errors:?}",
        );
        events.into_iter().map(Event::into_owned).collect()
    }

    #[test]
    fn test_plain_scalar() {
        let events = events_from("hello");
        assert!(events.iter().any(|ev| matches!(
            ev,
            Event::Scalar {
                style: ScalarStyle::Plain,
                value,
                ..
            } if value == "hello"
        )));
    }

    #[test]
    fn test_flow_mapping() {
        let events = events_from("{a: 1}");

        let has_map_start = events.iter().any(|ev| {
            matches!(
                ev,
                Event::MappingStart {
                    style: CollectionStyle::Flow,
                    ..
                }
            )
        });
        let has_map_end = events
            .iter()
            .any(|ev| matches!(ev, Event::MappingEnd { .. }));

        assert!(has_map_start, "Expected MappingStart, got: {events:?}");
        assert!(has_map_end, "Expected MappingEnd, got: {events:?}");
    }

    #[test]
    fn test_flow_sequence() {
        let events = events_from("[1, 2, 3]");

        let has_seq_start = events.iter().any(|ev| {
            matches!(
                ev,
                Event::SequenceStart {
                    style: CollectionStyle::Flow,
                    ..
                }
            )
        });
        let has_seq_end = events
            .iter()
            .any(|ev| matches!(ev, Event::SequenceEnd { .. }));

        assert!(has_seq_start, "Expected SequenceStart, got: {events:?}");
        assert!(has_seq_end, "Expected SequenceEnd, got: {events:?}");
    }

    #[test]
    fn test_block_sequence() {
        let events = events_from("- a\n- b");

        let seq_starts: Vec<_> = events
            .iter()
            .filter(|ev| {
                matches!(
                    ev,
                    Event::SequenceStart {
                        style: CollectionStyle::Block,
                        ..
                    }
                )
            })
            .collect();

        // Should have exactly ONE block sequence start, not one per entry
        assert_eq!(
            seq_starts.len(),
            1,
            "Expected 1 SequenceStart for block sequence, got {}: {events:?}",
            seq_starts.len()
        );
    }

    #[test]
    fn test_block_mapping() {
        let events = events_from("a: 1\nb: 2");

        let map_starts: Vec<_> = events
            .iter()
            .filter(|ev| {
                matches!(
                    ev,
                    Event::MappingStart {
                        style: CollectionStyle::Block,
                        ..
                    }
                )
            })
            .collect();

        // Should have exactly ONE block mapping start
        assert_eq!(
            map_starts.len(),
            1,
            "Expected 1 MappingStart for block mapping, got {}: {events:?}",
            map_starts.len()
        );
    }

    #[test]
    fn test_block_mapping_with_comment_between_keys() {
        let input = "a: 1\n# comment between keys\nb: 2\n";
        let (raw_events, errors) = crate::emit_events(input);
        let events: Vec<Event<'static>> = raw_events.into_iter().map(Event::into_owned).collect();

        assert!(
            errors.is_empty(),
            "expected no errors for mapping with comment between keys, got: {errors:?}"
        );

        // We should still have exactly one block MappingStart and MappingEnd
        let map_starts = events
            .iter()
            .filter(|ev| {
                matches!(
                    ev,
                    Event::MappingStart {
                        style: CollectionStyle::Block,
                        ..
                    }
                )
            })
            .count();
        let map_ends = events
            .iter()
            .filter(|ev| matches!(ev, Event::MappingEnd { .. }))
            .count();

        assert_eq!(
            map_starts, 1,
            "expected exactly 1 MappingStart, got {map_starts}: {events:?}",
        );
        assert_eq!(
            map_ends, 1,
            "expected exactly 1 MappingEnd, got {map_ends}: {events:?}",
        );
    }

    #[test]
    fn test_quoted_string() {
        let events = events_from("\"hello world\"");

        let has_quoted = events.iter().any(|ev| {
            matches!(
                ev,
                Event::Scalar {
                    style: ScalarStyle::DoubleQuoted,
                    ..
                }
            )
        });
        assert!(has_quoted, "Expected double-quoted scalar, got: {events:?}");
    }

    #[test]
    fn test_document_markers() {
        let events = events_from("---\nhello\n...");

        let has_doc_start = events
            .iter()
            .any(|ev| matches!(ev, Event::DocumentStart { explicit: true, .. }));
        let has_doc_end = events
            .iter()
            .any(|ev| matches!(ev, Event::DocumentEnd { explicit: true, .. }));

        assert!(has_doc_start, "Expected DocumentStart, got: {events:?}");
        assert!(has_doc_end, "Expected DocumentEnd, got: {events:?}");
    }

    #[test]
    fn test_anchor_and_alias() {
        // Use a sequence so both anchor and alias are in the same document
        let events = events_from("- &anchor value\n- *anchor");

        // Check for anchored scalar
        let has_anchor = events.iter().any(|ev| {
            matches!(
                ev,
                Event::Scalar { properties, .. }
                    if properties
                        .as_ref()
                        .and_then(|event_props| event_props.anchor.as_ref())
                        .map(|prop| prop.value.as_ref())
                        == Some("anchor")
            )
        });
        assert!(has_anchor, "Expected scalar with anchor, got: {events:?}");

        // Check for alias
        let has_alias = events.iter().any(|ev| {
            matches!(
                ev,
                Event::Alias { name, .. } if name == "anchor"
            )
        });
        assert!(has_alias, "Expected alias, got: {events:?}");
    }

    #[test]
    fn test_tagged_scalar() {
        let events = events_from("!!str 42");

        // Check for tagged scalar with expanded tag
        let has_tag = events.iter().any(|ev| {
            matches!(
                ev,
                Event::Scalar { properties, .. }
                    if properties
                        .as_ref()
                        .and_then(|event_props| event_props.tag.as_ref())
                        .map(|prop| prop.value.as_ref())
                        == Some("tag:yaml.org,2002:str")
            )
        });
        assert!(
            has_tag,
            "Expected scalar with expanded tag, got: {events:?}"
        );
    }

    #[test]
    fn test_tagged_scalar_with_percent_encoded_unicode_suffix() {
        let events = events_from("!!caf%C3%A9 42");

        let has_tag = events.iter().any(|ev| {
            matches!(
                ev,
                Event::Scalar { properties, .. }
                    if properties
                        .as_ref()
                        .and_then(|event_props| event_props.tag.as_ref())
                        .map(|prop| prop.value.as_ref())
                        == Some("tag:yaml.org,2002:café")
            )
        });
        assert!(
            has_tag,
            "Expected scalar with decoded Unicode tag suffix, got: {events:?}"
        );
    }

    #[test]
    fn test_nested_block_structures() {
        // Test nested mapping inside sequence
        let events = events_from("- a: 1\n- b: 2");

        let seq_count = events
            .iter()
            .filter(|ev| matches!(ev, Event::SequenceStart { .. }))
            .count();
        let map_count = events
            .iter()
            .filter(|ev| matches!(ev, Event::MappingStart { .. }))
            .count();

        assert_eq!(
            seq_count, 1,
            "Expected 1 sequence, got {seq_count}: {events:?}"
        );
        assert!(
            map_count >= 2,
            "Expected at least 2 mappings, got {map_count}: {events:?}"
        );
    }

    #[test]
    fn test_nested_block_mapping_with_comment_between_keys() {
        let input = "outer:\n  a: 1\n  # comment between nested keys\n  b: 2\n";
        let (events, errors) = events_and_errors_from(input);

        assert!(
            errors.is_empty(),
            "expected no errors for nested mapping with comment between keys, got: {errors:?}",
        );

        let map_count = events
            .iter()
            .filter(|ev| matches!(ev, Event::MappingStart { .. }))
            .count();

        assert!(
            map_count >= 2,
            "expected at least 2 mappings (outer + nested), got {map_count}: {events:?}",
        );
    }

    // Helper to get events and errors using the emitter
    fn events_and_errors_from(input: &str) -> (Vec<Event<'static>>, Vec<crate::error::ParseError>) {
        let (events, errors) = crate::emit_events(input);
        (events.into_iter().map(Event::into_owned).collect(), errors)
    }

    #[test]
    fn test_tagged_flow_mapping_implicit_key_keeps_tag_on_key_node() {
        let input = "!!map\n!foo {a: b}: c\n";
        let (events, errors) = events_and_errors_from(input);

        assert!(
            errors.is_empty(),
            "expected tagged flow-map key to parse without errors: {errors:?}"
        );
        assert!(
            events.iter().any(|event| {
                matches!(
                    event,
                    Event::MappingStart { properties, .. }
                        if properties
                            .as_ref()
                            .and_then(|event_props| event_props.tag.as_ref())
                            .map(|tag| tag.value.as_ref())
                            == Some("!foo")
                )
            }),
            "expected tagged flow mapping key event, got: {events:?}"
        );
    }

    #[test]
    fn test_tagged_flow_sequence_implicit_key_keeps_tag_on_key_node() {
        let input = "!!map\n!foo [a, b]: c\n";
        let (events, errors) = events_and_errors_from(input);

        assert!(
            errors.is_empty(),
            "expected tagged flow-sequence key to parse without errors: {errors:?}"
        );
        assert!(
            events.iter().any(|event| {
                matches!(
                    event,
                    Event::SequenceStart { properties, .. }
                        if properties
                            .as_ref()
                            .and_then(|event_props| event_props.tag.as_ref())
                            .map(|tag| tag.value.as_ref())
                            == Some("!foo")
                )
            }),
            "expected tagged flow sequence key event, got: {events:?}"
        );
    }

    #[test]
    fn test_tagged_double_quoted_implicit_key_keeps_tag_on_key_scalar() {
        let input = "!!map\n!foo \"a, b\": c\n";
        let (events, errors) = events_and_errors_from(input);

        assert!(
            errors.is_empty(),
            "expected tagged double-quoted key to parse without errors: {errors:?}"
        );
        assert!(
            events.iter().any(|event| {
                matches!(
                    event,
                    Event::Scalar {
                        style: ScalarStyle::DoubleQuoted,
                        value,
                        properties,
                        ..
                    } if value == "a, b"
                        && properties
                            .as_ref()
                            .and_then(|event_props| event_props.tag.as_ref())
                            .map(|tag| tag.value.as_ref())
                            == Some("!foo")
                )
            }),
            "expected tagged double-quoted key event, got: {events:?}"
        );
    }

    #[test]
    fn test_tagged_single_quoted_implicit_key_keeps_tag_on_key_scalar() {
        let input = "!!map\n!foo 'a, b': c\n";
        let (events, errors) = events_and_errors_from(input);

        assert!(
            errors.is_empty(),
            "expected tagged single-quoted key to parse without errors: {errors:?}"
        );
        assert!(
            events.iter().any(|event| {
                matches!(
                    event,
                    Event::Scalar {
                        style: ScalarStyle::SingleQuoted,
                        value,
                        properties,
                        ..
                    } if value == "a, b"
                        && properties
                            .as_ref()
                            .and_then(|event_props| event_props.tag.as_ref())
                            .map(|tag| tag.value.as_ref())
                            == Some("!foo")
                )
            }),
            "expected tagged single-quoted key event, got: {events:?}"
        );
    }

    #[test]
    fn test_pending_event_single_slot_stress_cases() {
        let input = "\
---
[flow]: block
---
? []: x
---
{ first: Sammy, last: Sosa }:
  hr: 65
  avg: 0.278
---
&a a: &b b
*b : *a
---
!!str : bar
---
top1:
  key1: val1
top2
";
        let (events, errors) = events_and_errors_from(input);

        let flow_seq_starts = events
            .iter()
            .filter(|event| {
                matches!(
                    event,
                    Event::SequenceStart {
                        style: CollectionStyle::Flow,
                        ..
                    }
                )
            })
            .count();
        let flow_map_starts = events
            .iter()
            .filter(|event| {
                matches!(
                    event,
                    Event::MappingStart {
                        style: CollectionStyle::Flow,
                        ..
                    }
                )
            })
            .count();
        let alias_events = events
            .iter()
            .filter(|event| matches!(event, Event::Alias { .. }))
            .count();
        let empty_tagged_keys = events
            .iter()
            .filter(|event| {
                matches!(
                    event,
                    Event::Scalar {
                        style: ScalarStyle::Plain,
                        value,
                        properties,
                        ..
                    } if value.is_empty()
                        && properties
                            .as_ref()
                            .and_then(|event_props| event_props.tag.as_ref())
                            .map(|tag| tag.value.as_ref())
                            == Some("tag:yaml.org,2002:str")
                )
            })
            .count();

        assert!(
            flow_seq_starts >= 2,
            "expected at least 2 flow sequence starts, got {flow_seq_starts}: {events:?}"
        );
        assert!(
            flow_map_starts >= 1,
            "expected at least 1 flow mapping start, got {flow_map_starts}: {events:?}"
        );
        assert!(
            alias_events >= 2,
            "expected alias events from deferred alias-key path, got {alias_events}: {events:?}"
        );
        assert!(
            empty_tagged_keys >= 1,
            "expected tagged empty scalar key from deferred empty-key path, got {events:?}"
        );
        assert!(
            errors
                .iter()
                .any(|error| error.kind == ErrorKind::MissingColon),
            "expected MissingColon from stress input, got: {errors:?}"
        );
    }

    #[test]
    fn test_unclosed_flow_sequence_produces_error() {
        // Unclosed flow sequence should report error and auto-close
        let (events, errors) = events_and_errors_from("[a, b");

        // Should produce SequenceStart, scalars, and SequenceEnd (auto-closed)
        assert!(
            events
                .iter()
                .any(|event| matches!(event, Event::SequenceStart { .. })),
            "Should have SequenceStart: {events:?}"
        );
        assert!(
            events
                .iter()
                .any(|event| matches!(event, Event::SequenceEnd { .. })),
            "Should have SequenceEnd (auto-closed): {events:?}"
        );
        // Should report error
        assert!(
            errors
                .iter()
                .any(|err| matches!(err.kind, ErrorKind::UnexpectedEof)),
            "Should report UnexpectedEof error: {errors:?}"
        );
    }

    #[test]
    fn test_unclosed_flow_mapping_produces_error() {
        // Unclosed flow mapping should report error and auto-close
        let (events, errors) = events_and_errors_from("{a: 1");

        assert!(
            events
                .iter()
                .any(|event| matches!(event, Event::MappingStart { .. })),
            "Should have MappingStart: {events:?}"
        );
        assert!(
            events
                .iter()
                .any(|event| matches!(event, Event::MappingEnd { .. })),
            "Should have MappingEnd (auto-closed): {events:?}"
        );
        assert!(
            errors
                .iter()
                .any(|err| matches!(err.kind, ErrorKind::UnexpectedEof)),
            "Should report UnexpectedEof error: {errors:?}"
        );
    }

    #[test]
    fn test_mismatched_brackets_produces_error() {
        // Mismatched brackets: opened with [ but closed with }
        let (events, errors) = events_and_errors_from("[a, b}");

        // Should produce SequenceStart and SequenceEnd (correct type despite mismatch)
        let seq_starts = events
            .iter()
            .filter(|event| matches!(event, Event::SequenceStart { .. }))
            .count();
        let seq_ends = events
            .iter()
            .filter(|event| matches!(event, Event::SequenceEnd { .. }))
            .count();
        assert_eq!(seq_starts, 1, "Should have 1 SequenceStart: {events:?}");
        assert_eq!(seq_ends, 1, "Should have 1 SequenceEnd: {events:?}");
        // Should report some error for the invalid syntax
        // The exact error type may vary (MismatchedBrackets, MissingSeparator, UnexpectedEof, etc.)
        assert!(
            !errors.is_empty(),
            "Should report at least one error for mismatched brackets: {errors:?}"
        );
    }
}
