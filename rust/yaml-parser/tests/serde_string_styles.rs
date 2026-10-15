// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! String style selection must preserve values and types across YAML readers.

#![cfg(feature = "serde")]
#![allow(
    clippy::tests_outside_test_module,
    reason = "integration tests in tests/ are top-level by design"
)]
#![allow(clippy::expect_used, reason = "expect is acceptable in tests")]

use std::collections::BTreeMap;

use yaml_parser::serde::from_str;
use yaml_parser::serde::to_string;

fn assert_string_preserved(input: &str, yaml: &str) {
    let parser_decoded: String =
        from_str(yaml).expect("yaml-parser should read the emitted string");
    assert_eq!(parser_decoded, input, "string changed in YAML {yaml:?}");
    let reference_decoded: yaml_serde::Value =
        yaml_serde::from_str(yaml).expect("the reference reader should accept the emitted YAML");
    assert_eq!(
        reference_decoded,
        yaml_serde::Value::String(input.to_owned()),
        "string type or content changed in YAML {yaml:?}"
    );
}

#[test]
fn safe_single_line_strings_use_plain_style() {
    for input in [
        "profiles",
        "default",
        "hello world",
        "http://example.com/path#fragment",
        "a:b",
        "-word",
        "?word",
        ":word",
        "O'Connor",
        "say \"hello\"",
        "C:\\path\\file",
        "café",
        "1.2.3",
        "1e",
        "0xnothex",
        "2026-10-13-not-a-date",
        "OFFice",
        "true-ish",
        "nullish",
        "123abc",
    ] {
        let yaml = to_string(&input).expect("string serialization should succeed");
        assert_eq!(
            yaml,
            format!("{input}\n"),
            "safe string should be plain: {input:?}"
        );
        assert_string_preserved(input, &yaml);
    }
}

#[test]
fn syntax_and_yaml_1_1_or_1_2_type_like_strings_stay_quoted() {
    for input in [
        "",
        " leading",
        "trailing ",
        "a: b",
        "a # comment",
        "value:",
        "[value]",
        "{value}",
        "a,b",
        "x[y]",
        "x{y}",
        "#comment",
        "&anchor",
        "*alias",
        "!tag",
        "|block",
        ">folded",
        "%directive",
        "@reserved",
        "`reserved",
        "'quoted'",
        "\"quoted\"",
        "-",
        "?",
        ":",
        "- item",
        "? key",
        ": value",
        "---",
        "...",
        "--- document",
        "... document",
        "true",
        "True",
        "FALSE",
        "null",
        "NULL",
        "~",
        "yes",
        "Yes",
        "YES",
        "no",
        "No",
        "NO",
        "on",
        "ON",
        "off",
        "Off",
        "OFF",
        "y",
        "Y",
        "n",
        "N",
        "<<",
        "=",
        "0",
        "42",
        "-42",
        "+42",
        "0123",
        "0xFF",
        "-0xFF",
        "+0o52",
        "0b1010",
        "1_000",
        "1.5",
        "1.",
        ".5",
        "1e3",
        "1e+3",
        "1e400",
        ".inf",
        "-.Inf",
        ".NAN",
        "18446744073709551616",
        "340282366920938463463374607431768211456",
        "1:20",
        "12:34:56",
        "12:34:56.7",
        "2026-10-13",
        "2026-10-13T12:34:56Z",
        "2026-1-3 12:34:56 +02:00",
        "a\tb",
        "first\nsecond\n",
        "a\rb",
    ] {
        let yaml = to_string(&input).expect("string serialization should succeed");
        assert!(
            yaml.starts_with('"'),
            "string should retain quoted fallback: {input:?}: {yaml:?}"
        );
        assert_string_preserved(input, &yaml);
    }
}

#[test]
fn quoted_fallback_escapes_controls_and_unicode_line_breaks() {
    for character in ('\0'..='\u{1f}')
        .chain('\u{7f}'..='\u{9f}')
        .chain(['\u{2028}', '\u{2029}', '\u{fffe}', '\u{ffff}'])
    {
        let input = format!("before{character}after");
        let yaml = to_string(&input).expect("string serialization should succeed");
        assert!(
            yaml.starts_with('"'),
            "nonprintable string must be quoted: {yaml:?}"
        );
        assert_string_preserved(&input, &yaml);
    }
}

#[test]
fn string_types_survive_as_mapping_keys_values_and_sequence_items() {
    let mapping = BTreeMap::from([
        ("plain".to_owned(), "yes".to_owned()),
        ("ON".to_owned(), "name".to_owned()),
        ("a: b".to_owned(), "1:20".to_owned()),
        ("true".to_owned(), "2026-10-13".to_owned()),
        ("<<".to_owned(), "default".to_owned()),
    ]);
    let yaml = to_string(&mapping).expect("mapping serialization should succeed");
    let parser_mapping: BTreeMap<String, String> =
        from_str(&yaml).expect("mapping should deserialize");
    assert_eq!(parser_mapping, mapping);
    let reference_mapping: BTreeMap<String, String> =
        yaml_serde::from_str(&yaml).expect("reference mapping should deserialize");
    assert_eq!(reference_mapping, mapping);

    let items = vec![
        "true",
        "0123",
        "yes",
        "hello world",
        "a,b",
        "http://example.com",
    ];
    let sequence_yaml = to_string(&items).expect("sequence serialization should succeed");
    let parser_items: Vec<String> = from_str(&sequence_yaml).expect("sequence should deserialize");
    assert_eq!(parser_items, items);
    let reference_items: Vec<String> =
        yaml_serde::from_str(&sequence_yaml).expect("reference sequence should deserialize");
    assert_eq!(reference_items, items);
}
