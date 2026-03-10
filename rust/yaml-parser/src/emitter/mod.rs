// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! YAML event emitter with error recovery.
//!
//! This module provides the YAML event emitter that converts
//! a token slice into YAML events via the `Iterator` interface.

mod cursor;
#[allow(
    clippy::module_inception,
    reason = "standard pattern for re-exporting main type"
)]
mod emitter;
mod states;

pub use emitter::Emitter;

#[cfg(test)]
mod tests {
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
        use crate::event::{CollectionStyle, Event, ScalarStyle};

        /// Helper to get events from YAML input using the emitter
        fn events_from(input: &str) -> Vec<Event<'static>> {
            let (events, _errors) = crate::emit_events(input);
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
                    Event::Scalar { anchor: Some((anchor_name, _)), .. } if anchor_name == "anchor"
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
                    Event::Scalar { tag: Some((tg, _)), .. } if tg == "tag:yaml.org,2002:str"
                )
            });
            assert!(
                has_tag,
                "Expected scalar with expanded tag, got: {events:?}"
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

        // Helper to get events and errors using the emitter
        fn events_and_errors_from(
            input: &str,
        ) -> (Vec<Event<'static>>, Vec<crate::error::ParseError>) {
            let (events, errors) = crate::emit_events(input);
            (events.into_iter().map(Event::into_owned).collect(), errors)
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
                    .any(|err| matches!(err.kind, crate::error::ErrorKind::UnexpectedEof)),
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
                    .any(|err| matches!(err.kind, crate::error::ErrorKind::UnexpectedEof)),
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
}
