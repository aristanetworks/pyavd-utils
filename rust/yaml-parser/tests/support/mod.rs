#![allow(
// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

    dead_code,
    reason = "test helpers are pulled in as modules by other tests"
)]

// Shared test helpers for integration tests in `tests/`.
//
// These helpers enforce the invariant that "positive" tests (those that
// expect successful parsing) must not ignore parser/emitter errors.

use yaml_parser::Event;
use yaml_parser::Stream;
use yaml_parser::emit_events;
use yaml_parser::parse;

/// Parse input as YAML and assert that the parser reports no errors.
///
/// Returns the parsed document stream for further inspection.
pub(crate) fn parse_ok(input: &str) -> Stream<'static> {
    let (docs, errors) = parse(input);
    assert!(
        errors.is_empty(),
        "expected no parse errors for input:\n{input}\nerrors: {errors:?}",
    );
    docs
}

/// Emit events from input and assert that the emitter reports no errors.
///
/// Returns owned events for convenience in tests.
pub(crate) fn emit_events_ok(input: &str) -> Vec<Event<'static>> {
    let (events, errors) = emit_events(input);
    assert!(
        errors.is_empty(),
        "expected no emit_events errors for input:\n{input}\nerrors: {errors:?}",
    );
    events.into_iter().map(Event::into_owned).collect()
}
