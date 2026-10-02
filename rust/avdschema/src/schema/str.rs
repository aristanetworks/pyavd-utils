// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::sync::OnceLock;

use fancy_regex::Error;
use fancy_regex::Regex;
use fancy_regex::RegexBuilder;
use serde::Deserialize;
use serde::Serialize;
use serde_with::skip_serializing_none;

use super::any::SourceSchema;
use super::base::Base;
use super::base::convert_types::ConvertTypes;
use super::base::documentation_options::DocumentationOptions;
use super::base::valid_values::ValidValues;

/// Enum for string formats allowed by the [`SourceStr`] schema.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum Format {
    Cidr,
    Ip,
    IpPool,
    Ipv4,
    Ipv4Cidr,
    Ipv4Pool,
    Ipv6,
    Ipv6Cidr,
    Ipv6Pool,
    Mac,
}

/// AVD Schema for string data.
#[skip_serializing_none]
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct SourceStr {
    /// Convert string value to lower case before performing validation
    pub convert_to_lower_case: Option<bool>,
    pub format: Option<Format>,
    pub max_length: Option<u64>,
    pub min_length: Option<u64>,
    /// An AVD regular expression which will be matched against the complete variable value.
    /// Perl shorthand classes, wildcards, and explicit character classes operate on Unicode
    /// strings. Unicode property and script tables beyond those needed by the Perl classes are
    /// not supported.
    /// Remember to use double escapes
    pub pattern: Option<Pattern>,
    #[serde(flatten)]
    pub base: Base<String>,
    #[serde(flatten)]
    #[cfg_attr(
        feature = "metaschema",
        schemars(with = "super::base::convert_types::StrConvertTypes")
    )]
    pub convert_types: ConvertTypes,
    #[serde(flatten)]
    pub valid_values: ValidValues<String>,
    pub documentation_options: Option<DocumentationOptions>,
}

impl<'x> TryFrom<&'x SourceSchema> for &'x SourceStr {
    type Error = &'static str;

    fn try_from(value: &'x SourceSchema) -> Result<Self, Self::Error> {
        match value {
            SourceSchema::Str(str) => Ok(str),
            _ => Err("Unable to convert from SourceSchema to SourceStr. Invalid Schema type."),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, derive_more::Display)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "metaschema", schemars(extend("format" = "regex")))]
#[display("{pattern}")]
#[serde(transparent)]
pub struct Pattern {
    pub pattern: String,
    #[serde(skip)]
    compiled_pattern: OnceLock<Result<Regex, Error>>,
}

impl Pattern {
    fn new(pattern: String) -> Self {
        Self {
            pattern,
            compiled_pattern: OnceLock::default(),
        }
    }
    pub fn get_compiled_pattern(&self) -> Result<&Regex, &Error> {
        self.compiled_pattern
            .get_or_init(|| {
                let anchored_pattern = format!("^(?:{})$", self.pattern);
                RegexBuilder::new(&anchored_pattern)
                    // Unicode mode keeps `.`, negated classes, and Perl shorthand complements
                    // valid for Rust UTF-8 strings. Only the Unicode Perl tables are enabled in
                    // Cargo, so broad properties and scripts remain unavailable.
                    .unicode_mode(true)
                    .build()
            })
            .as_ref()
    }
}
impl PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern
    }
}
impl From<&str> for Pattern {
    fn from(value: &str) -> Self {
        Self::new(value.to_owned())
    }
}

#[cfg(test)]
mod tests {
    use super::Pattern;
    use super::SourceStr;
    use crate::any::SourceSchema;
    use crate::boolean::SourceBool;

    #[test]
    fn try_from_anyschema_ok() {
        let anyschema = &SourceSchema::Str(SourceStr::default());
        let result: Result<&SourceStr, _> = anyschema.try_into();
        assert!(result.is_ok());
    }
    #[test]
    fn try_from_anyschema_err() {
        let anyschema = &SourceSchema::Bool(SourceBool::default());
        let result: Result<&SourceStr, _> = anyschema.try_into();
        assert!(result.is_err());
    }
    #[test]
    fn perl_classes_use_unicode_semantics() {
        let digits = Pattern::from(r"\d+");
        let digits = digits.get_compiled_pattern().unwrap();
        assert!(digits.is_match("123").unwrap());
        assert!(digits.is_match("١٢٣").unwrap());

        let whitespace = Pattern::from(r"\s+");
        let whitespace = whitespace.get_compiled_pattern().unwrap();
        assert!(whitespace.is_match(" \t\n").unwrap());
        assert!(whitespace.is_match("\u{2003}").unwrap());

        let word = Pattern::from(r"\w+");
        let word = word.get_compiled_pattern().unwrap();
        assert!(word.is_match("AVD_123").unwrap());
        assert!(word.is_match("café").unwrap());
    }

    #[test]
    fn word_boundaries_use_unicode_semantics() {
        let pattern = Pattern::from(r"\bcafé\b");
        let compiled_pattern = pattern.get_compiled_pattern().unwrap();

        assert!(compiled_pattern.is_match("café").unwrap());
    }

    #[test]
    fn complemented_perl_classes_use_unicode_semantics() {
        let non_digits = Pattern::from(r"\D+");
        let non_digits = non_digits.get_compiled_pattern().unwrap();
        assert!(non_digits.is_match("é").unwrap());
        assert!(!non_digits.is_match("1").unwrap());
        assert!(!non_digits.is_match("١").unwrap());

        let non_whitespace = Pattern::from(r"\S+");
        let non_whitespace = non_whitespace.get_compiled_pattern().unwrap();
        assert!(non_whitespace.is_match("a").unwrap());
        assert!(!non_whitespace.is_match(" ").unwrap());
        assert!(!non_whitespace.is_match("\u{2003}").unwrap());

        let non_word = Pattern::from(r"\W+");
        let non_word = non_word.get_compiled_pattern().unwrap();
        assert!(non_word.is_match("!").unwrap());
        assert!(!non_word.is_match("_").unwrap());
        assert!(!non_word.is_match("é").unwrap());
    }

    #[test]
    fn unicode_perl_classes_work_inside_character_classes() {
        let union = Pattern::from(r"[\dA]+");
        let union = union.get_compiled_pattern().unwrap();
        assert!(union.is_match("1A").unwrap());
        assert!(union.is_match("١").unwrap());

        let negated_complement = Pattern::from(r"[^\D]+");
        let negated_complement = negated_complement.get_compiled_pattern().unwrap();
        assert!(negated_complement.is_match("1").unwrap());
        assert!(negated_complement.is_match("١").unwrap());
        assert!(!negated_complement.is_match("A").unwrap());
    }

    #[test]
    fn dot_wildcard_compiles_and_matches_the_complete_value() {
        let pattern = Pattern::from("Ethernet.*");
        let compiled_pattern = pattern.get_compiled_pattern().unwrap();

        assert!(compiled_pattern.is_match("Ethernet1").unwrap());
        assert!(compiled_pattern.is_match("Etherneté").unwrap());
        assert!(!compiled_pattern.is_match("FastEthernet1").unwrap());
    }

    #[test]
    fn negated_character_class_compiles() {
        let pattern = Pattern::from("Ethernet[^/]+");
        let compiled_pattern = pattern.get_compiled_pattern().unwrap();

        assert!(compiled_pattern.is_match("Ethernet1").unwrap());
        assert!(compiled_pattern.is_match("Etherneté").unwrap());
        assert!(!compiled_pattern.is_match("Ethernet1/1").unwrap());
    }

    #[test]
    fn lookahead_compiles() {
        assert!(
            Pattern::from("(?=[a-z])(?=.*[0-9])[a-z0-9]+")
                .get_compiled_pattern()
                .is_ok()
        );
    }

    #[test]
    fn variable_lookbehind_compiles() {
        assert!(Pattern::from("(?<=a+)b").get_compiled_pattern().is_ok());
    }

    #[test]
    fn alternation_matches_the_complete_value() {
        let pattern = Pattern::from("foo|bar");
        let compiled_pattern = pattern.get_compiled_pattern().unwrap();

        assert!(compiled_pattern.is_match("foo").unwrap());
        assert!(compiled_pattern.is_match("bar").unwrap());
        assert!(!compiled_pattern.is_match("foobar").unwrap());
    }

    #[test]
    fn unicode_properties_are_rejected() {
        assert!(
            Pattern::from(r"\p{Script=Greek}+")
                .get_compiled_pattern()
                .is_err()
        );
        assert!(
            Pattern::from(r"\P{Script=Greek}+")
                .get_compiled_pattern()
                .is_err()
        );
    }

    #[test]
    fn escaped_perl_and_property_markers_remain_literals() {
        let pattern = Pattern::from(r"\\d\\p");
        let compiled_pattern = pattern.get_compiled_pattern().unwrap();

        assert!(compiled_pattern.is_match(r"\d\p").unwrap());
    }
}
