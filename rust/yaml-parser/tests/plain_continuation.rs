// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Plain scalar continuations keep quotes as literal content.

#![cfg(feature = "serde")]
#![allow(
    clippy::tests_outside_test_module,
    reason = "integration tests are top-level"
)]
#![allow(clippy::expect_used, reason = "expect is acceptable in tests")]

use yaml_serde::Value;

fn assert_document(input: &str, expected_yaml: &str) {
    let expected: Value =
        yaml_serde::from_str(expected_yaml).expect("the expected document should be valid YAML");
    let reference: Value =
        yaml_serde::from_str(input).expect("the reference reader should accept this YAML");
    assert_eq!(
        reference, expected,
        "the reference reader must agree on plain scalar semantics"
    );
    let decoded: Value = yaml_parser::serde::from_str(input)
        .expect("valid plain scalar continuation should deserialize");
    assert_eq!(
        decoded, expected,
        "from_str changed the document: {input:?}"
    );
    let reader_decoded: Value = yaml_parser::serde::from_reader(input.as_bytes())
        .expect("the production reader should accept the continuation");
    assert_eq!(
        reader_decoded, expected,
        "from_reader changed the document: {input:?}"
    );
}

#[test]
fn description_continuation_starting_with_quote() {
    assert_document(
        "description: Take action, regardless of state. Should be set to\n   'true' for MSS-G.\n",
        "description: Take action, regardless of state. Should be set to 'true' for MSS-G.\n",
    );
}

#[test]
fn continuation_quotes_and_backslashes_are_literal() {
    for continuation in [
        "'true' for MSS-G.",
        "\"true\" for MSS-G.",
        "'unmatched quote",
        "\"unmatched quote",
        r#""\n" stays literal"#,
        r#""\q" is not a quoted escape"#,
        "'it''s' stays literal",
        "'value' # ignored comment",
        "\"https://example.com/#fragment\" stays literal",
    ] {
        let expected = if continuation == "'value' # ignored comment" {
            "first 'value'".to_owned()
        } else {
            format!("first {continuation}")
        };
        assert_document(
            &format!("description: first\n  {continuation}\nnext: unchanged\n"),
            &format!("description: {expected}\nnext: unchanged\n"),
        );
    }
}

#[test]
fn continuation_preserves_blank_lines_and_crlf_folding() {
    for newline in ["\n", "\r\n"] {
        assert_document(
            &format!("description: first{newline}{newline}  'second'{newline}  \"third\"{newline}"),
            "description: |-\n  first\n  'second' \"third\"\n",
        );
    }
}

#[test]
fn continuation_ends_before_quoted_sibling_keys() {
    assert_document(
        "outer:\n  description: first\n    'second'\n  'next': \"quoted value\"\n",
        "outer: {description: first 'second', next: quoted value}\n",
    );
    assert_document(
        "- description: first\n    \"second\"\n  \"next\": 'quoted value'\n- 'quoted item'\n",
        "- description: first \"second\"\n  next: quoted value\n- quoted item\n",
    );
}

#[test]
fn flow_plain_continuation_keeps_quotes_literal() {
    assert_document(
        "{description: first\n  'second', next: \"quoted value\"}\n",
        "description: first 'second'\nnext: quoted value\n",
    );
    assert_document(
        "[first\n  \"second\", 'quoted item']\n",
        "- first \"second\"\n- quoted item\n",
    );
}

#[test]
fn quotes_after_comments_start_new_values() {
    assert_document(
        "- first # scalar ends here\n- \"second\"\n",
        "- first\n- second\n",
    );
    for input in [
        "description: first # scalar ends here\n  'second'\n",
        "description: first\n  # scalar ends here\n  \"second\"\n",
    ] {
        assert!(
            yaml_parser::serde::from_str::<Value>(input).is_err(),
            "a comment must terminate a plain scalar: {input:?}"
        );
    }
}

#[test]
fn compact_nested_sequences_keep_quoted_mapping_keys() {
    assert_document(
        "- - description: first\n    'next': 'quoted value'\n",
        "- - description: first\n    next: quoted value\n",
    );
    assert_document(
        "- - description: first\n      'second'\n    'next': 'quoted value'\n",
        "- - description: first 'second'\n    next: quoted value\n",
    );
}

#[test]
fn compact_sequence_with_extra_spaces_keeps_quoted_mapping_keys() {
    assert_document(
        "-    description: first\n     'next': 'quoted value'\n",
        "- description: first\n  next: quoted value\n",
    );
    assert_document(
        "-    description: first\n       \"second\"\n     \"next\": 'quoted value'\n",
        "- description: first \"second\"\n  next: quoted value\n",
    );
}

#[test]
fn compact_quoted_keys_keep_comment_punctuation_and_escapes() {
    for key in ["'a # b'", "'a'' # b'", r#""a\" # b""#] {
        assert_document(
            &format!("- - description: first\n    {key}: quoted value\n"),
            &format!("- - description: first\n    {key}: quoted value\n"),
        );
    }
}

#[test]
fn flow_continuation_keeps_nonseparating_colon_literal() {
    assert_document(
        "[first\n  'second':literal]\n",
        "- first 'second':literal\n",
    );
}
