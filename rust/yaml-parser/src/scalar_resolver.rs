// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Shared YAML scalar semantic resolution.
//!
//! This module is the single source of truth for YAML 1.2 Core-schema scalar
//! resolution used by both the AST parser and the event-based serde backend.
//!
//! Contract:
//! - plain untagged scalars use YAML 1.2 Core implicit resolution
//! - quoted and block scalars remain strings unless an explicit built-in tag
//!   overrides that
//! - explicit built-in scalar tags override implicit resolution
//! - custom/local tags and the non-specific `!` tag disable implicit
//!   resolution and keep the scalar as a string
//! - invalid explicit built-in tag content reports an error to the caller,
//!   which decides whether and how to recover

use std::borrow::Cow;

use crate::event::ScalarStyle;
use crate::value::{Integer, Value};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ResolvedScalar<'input> {
    Null,
    Bool(bool),
    Int(Integer<'input>),
    Float(f64),
    String(Cow<'input, str>),
}

impl<'input> ResolvedScalar<'input> {
    #[must_use]
    pub(crate) fn into_value(self) -> Value<'input> {
        match self {
            Self::Null => Value::Null,
            Self::Bool(value) => Value::Bool(value),
            Self::Int(value) => Value::Int(value),
            Self::Float(value) => Value::Float(value),
            Self::String(value) => Value::String(value),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BuiltinScalarTag {
    Str,
    Null,
    Bool,
    Int,
    Float,
}

impl BuiltinScalarTag {
    #[cfg(feature = "serde")]
    #[must_use]
    pub(crate) const fn display_name(self) -> &'static str {
        match self {
            Self::Str => "!!str",
            Self::Null => "!!null",
            Self::Bool => "!!bool",
            Self::Int => "!!int",
            Self::Float => "!!float",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ScalarResolutionError<'input> {
    InvalidExplicitBuiltinTagValue {
        tag: BuiltinScalarTag,
        original_text: Cow<'input, str>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DecimalNumericKind {
    Integer,
    Float,
    NotNumeric,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Sign {
    Positive,
    Negative,
}

impl Sign {
    const fn prefix(self) -> &'static str {
        match self {
            Self::Positive => "",
            Self::Negative => "-",
        }
    }
}

enum ExplicitTag {
    Builtin(BuiltinScalarTag),
    NonSpecific,
    Custom,
}

#[allow(clippy::inline_always, reason = "Proven performance gain")]
#[inline(always)]
pub(crate) fn resolve_untagged_scalar(
    text: Cow<'_, str>,
    style: ScalarStyle,
) -> ResolvedScalar<'_> {
    if style == ScalarStyle::Plain {
        resolve_plain_implicit(text)
    } else {
        ResolvedScalar::String(text)
    }
}

#[inline]
pub(crate) fn resolve_tagged_scalar<'input>(
    text: Cow<'input, str>,
    explicit_tag: &str,
) -> Result<ResolvedScalar<'input>, ScalarResolutionError<'input>> {
    match parse_explicit_tag(explicit_tag) {
        ExplicitTag::Builtin(builtin_tag) => resolve_explicit_builtin(builtin_tag, text),
        ExplicitTag::NonSpecific | ExplicitTag::Custom => Ok(ResolvedScalar::String(text)),
    }
}

fn parse_explicit_tag(tag: &str) -> ExplicitTag {
    match tag {
        "!" => ExplicitTag::NonSpecific,
        "!!str" | "tag:yaml.org,2002:str" => ExplicitTag::Builtin(BuiltinScalarTag::Str),
        "!!null" | "tag:yaml.org,2002:null" => ExplicitTag::Builtin(BuiltinScalarTag::Null),
        "!!bool" | "tag:yaml.org,2002:bool" => ExplicitTag::Builtin(BuiltinScalarTag::Bool),
        "!!int" | "tag:yaml.org,2002:int" => ExplicitTag::Builtin(BuiltinScalarTag::Int),
        "!!float" | "tag:yaml.org,2002:float" => ExplicitTag::Builtin(BuiltinScalarTag::Float),
        _ => ExplicitTag::Custom,
    }
}

// These `resolve_*numeric` helpers are hot-path dispatchers for plain implicit
// scalars. They intentionally duplicate some checks from `parse_yaml_int` /
// `parse_yaml_float` to keep the common plain-scalar path locally cheap.
#[allow(clippy::inline_always, reason = "Proven performance gain")]
#[inline(always)]
fn resolve_plain_implicit(text: Cow<'_, str>) -> ResolvedScalar<'_> {
    let input = text.as_ref();
    let bytes = input.as_bytes();
    let first = bytes.first();

    match first {
        None => ResolvedScalar::Null,
        Some(b'~' | b'n' | b'N') if parse_yaml_null(input).is_some() => ResolvedScalar::Null,
        Some(b't') if input == "true" => ResolvedScalar::Bool(true),
        Some(b'T') if matches!(input, "True" | "TRUE") => ResolvedScalar::Bool(true),
        Some(b'f') if input == "false" => ResolvedScalar::Bool(false),
        Some(b'F') if matches!(input, "False" | "FALSE") => ResolvedScalar::Bool(false),
        Some(b'+' | b'-' | b'.' | b'0'..=b'9') => {
            match resolve_plain_numeric(text.clone(), first.unwrap()) {
                Some(resolved_scalar) => resolved_scalar,
                None => ResolvedScalar::String(text),
            }
        }
        Some(_) => ResolvedScalar::String(text),
    }
}

/// Fast dispatcher for plain-scalar numeric candidates; not a full YAML
/// numeric parser.
#[allow(clippy::inline_always, reason = "Proven performance gain")]
#[inline(always)]
fn resolve_plain_numeric<'a>(text: Cow<'a, str>, first: &'_ u8) -> Option<ResolvedScalar<'a>> {
    match first {
        b'0'..=b'9' => resolve_unsigned_numeric(text),
        b'+' | b'-' => resolve_signed_numeric(text),
        b'.' => resolve_dotted_numeric(text),
        _ => None,
    }
}

/// Unsigned plain-scalar fast path: cheap prefix handling before decimal
/// classification.
#[allow(clippy::indexing_slicing, reason = "bytes length tracked")]
#[allow(clippy::inline_always, reason = "Proven performance gain")]
#[inline(always)]
fn resolve_unsigned_numeric(text: Cow<'_, str>) -> Option<ResolvedScalar<'_>> {
    let input = text.as_ref();
    let bytes = input.as_bytes();
    if bytes.len() > 2 && bytes[0] == b'0' {
        let sign = Sign::Positive;
        match bytes[1] {
            b'o' => {
                return input
                    .strip_prefix("0o")
                    .and_then(|digits| parse_prefixed_int(sign, digits, 8))
                    .map(ResolvedScalar::Int);
            }
            b'x' => {
                return input
                    .strip_prefix("0x")
                    .and_then(|digits| parse_prefixed_int(sign, digits, 16))
                    .map(ResolvedScalar::Int);
            }
            _ => {}
        }
    }

    match classify_decimal_numeric(input) {
        DecimalNumericKind::Integer => {
            Some(ResolvedScalar::Int(parse_decimal_int(text, Sign::Positive)))
        }
        DecimalNumericKind::Float => parse_finite_f64(input).map(ResolvedScalar::Float),
        DecimalNumericKind::NotNumeric => None,
    }
}

/// Signed plain-scalar fast path: cheap reject/prefix handling before decimal
/// classification.
#[allow(clippy::string_slice, reason = "We matched the previous characters")]
#[allow(clippy::inline_always, reason = "Proven performance gain")]
#[inline(always)]
fn resolve_signed_numeric(text: Cow<'_, str>) -> Option<ResolvedScalar<'_>> {
    let input = text.as_ref();
    let (sign, unsigned) = split_sign(input);
    let bytes = unsigned.as_bytes();

    match bytes.first()? {
        b'0' => match bytes.get(1) {
            Some(b'o') => {
                return parse_prefixed_int(sign, &unsigned[2..], 8).map(ResolvedScalar::Int);
            }
            Some(b'x') => {
                return parse_prefixed_int(sign, &unsigned[2..], 16).map(ResolvedScalar::Int);
            }
            _ => {}
        },
        b'1'..=b'9' => {}
        b'.' => {
            if let Some(value) = parse_special_yaml_float(input) {
                return Some(ResolvedScalar::Float(value));
            }
        }
        _ => return None,
    }

    match classify_decimal_numeric(input) {
        DecimalNumericKind::Integer => Some(ResolvedScalar::Int(parse_decimal_int(text, sign))),
        DecimalNumericKind::Float => parse_finite_f64(input).map(ResolvedScalar::Float),
        DecimalNumericKind::NotNumeric => None,
    }
}

/// Dot-prefixed plain-scalar fast path for decimal floats and special YAML
/// float spellings such as `.inf` / `.nan`.
#[allow(
    clippy::needless_pass_by_value,
    reason = "Proven performance gain + borrow issues in calling function"
)]
#[allow(clippy::inline_always, reason = "Proven performance gain")]
#[inline(always)]
fn resolve_dotted_numeric(text: Cow<'_, str>) -> Option<ResolvedScalar<'_>> {
    let input = text.as_ref();
    let bytes = input.as_bytes();
    match bytes.get(1).copied()? {
        b'0'..=b'9' => parse_finite_f64(input).map(ResolvedScalar::Float),
        b'i' | b'I' | b'n' | b'N' => parse_special_yaml_float(input).map(ResolvedScalar::Float),
        _ => None,
    }
}

fn resolve_explicit_builtin(
    tag: BuiltinScalarTag,
    text: Cow<'_, str>,
) -> Result<ResolvedScalar<'_>, ScalarResolutionError<'_>> {
    match tag {
        BuiltinScalarTag::Str => Ok(ResolvedScalar::String(text)),
        BuiltinScalarTag::Null => {
            if parse_yaml_null(text.as_ref()).is_some() {
                Ok(ResolvedScalar::Null)
            } else {
                Err(ScalarResolutionError::InvalidExplicitBuiltinTagValue {
                    tag,
                    original_text: text,
                })
            }
        }
        BuiltinScalarTag::Bool => {
            if let Some(value) = parse_yaml_bool(text.as_ref()) {
                Ok(ResolvedScalar::Bool(value))
            } else {
                Err(ScalarResolutionError::InvalidExplicitBuiltinTagValue {
                    tag,
                    original_text: text,
                })
            }
        }
        BuiltinScalarTag::Int => {
            if let Some(value) = parse_yaml_int(text.clone()) {
                Ok(ResolvedScalar::Int(value))
            } else {
                Err(ScalarResolutionError::InvalidExplicitBuiltinTagValue {
                    tag,
                    original_text: text,
                })
            }
        }
        BuiltinScalarTag::Float => {
            if let Some(value) = parse_explicit_float(text.clone()) {
                Ok(ResolvedScalar::Float(value))
            } else {
                Err(ScalarResolutionError::InvalidExplicitBuiltinTagValue {
                    tag,
                    original_text: text,
                })
            }
        }
    }
}

#[inline]
fn parse_yaml_null(input: &str) -> Option<()> {
    matches!(input, "" | "~" | "null" | "Null" | "NULL").then_some(())
}

#[inline]
pub(crate) fn parse_yaml_bool(input: &str) -> Option<bool> {
    match input {
        "true" | "True" | "TRUE" => Some(true),
        "false" | "False" | "FALSE" => Some(false),
        _ => None,
    }
}

/// Full YAML float parser used for explicit `!!float` validation.
///
/// YAML permits explicit `!!float` tags on integer lexical forms, so this
/// accepts both float spellings and integer spellings that can be coerced to
/// finite `f64`.
fn parse_explicit_float(text: Cow<'_, str>) -> Option<f64> {
    let input = text.as_ref();

    match parse_special_yaml_float(input) {
        // Explicit YAML special float spellings such as `.inf` and `.nan`.
        Some(value) => Some(value),
        None => match classify_decimal_numeric(input) {
            // Decimal float and decimal integer spellings that fit as finite `f64`.
            DecimalNumericKind::Float | DecimalNumericKind::Integer => parse_finite_f64(input),
            // Non-decimal integer spellings such as `0x2A` and `0o52`.
            DecimalNumericKind::NotNumeric => {
                parse_yaml_int(text).and_then(|integer| integer_to_f64(&integer))
            }
        },
    }
}

#[inline]
fn parse_finite_f64(input: &str) -> Option<f64> {
    input.parse::<f64>().ok().filter(|value| value.is_finite())
}

/// Full YAML int parser used for explicit tag validation.
fn parse_yaml_int(text: Cow<'_, str>) -> Option<Integer<'_>> {
    let (sign, unsigned) = split_sign(text.as_ref());
    if let Some(digits) = unsigned.strip_prefix("0o") {
        return parse_prefixed_int(sign, digits, 8);
    }
    if let Some(digits) = unsigned.strip_prefix("0x") {
        return parse_prefixed_int(sign, digits, 16);
    }
    if classify_decimal_numeric(text.as_ref()) != DecimalNumericKind::Integer {
        return None;
    }

    Some(parse_decimal_int(text, sign))
}

fn parse_special_yaml_float(input: &str) -> Option<f64> {
    match input {
        ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" => Some(f64::INFINITY),
        "-.inf" | "-.Inf" | "-.INF" => Some(f64::NEG_INFINITY),
        ".nan" | ".NaN" | ".NAN" => Some(f64::NAN),
        _ => None,
    }
}

fn split_sign(input: &str) -> (Sign, &str) {
    if let Some(rest) = input.strip_prefix('-') {
        (Sign::Negative, rest)
    } else if let Some(rest) = input.strip_prefix('+') {
        (Sign::Positive, rest)
    } else {
        (Sign::Positive, input)
    }
}

fn parse_decimal_int(input: Cow<'_, str>, sign: Sign) -> Integer<'_> {
    match sign {
        Sign::Positive => {
            if let Ok(value) = input.parse::<i64>() {
                return Integer::I64(value);
            }
            if let Ok(value) = input.parse::<i128>() {
                return Integer::I128(value);
            }
            if let Ok(value) = input.parse::<u128>() {
                return Integer::U128(value);
            }
            Integer::BigIntStr(if let Some(digits) = input.as_ref().strip_prefix('+') {
                Cow::Owned(digits.to_owned())
            } else {
                input
            })
        }
        Sign::Negative => {
            if let Ok(value) = input.parse::<i64>() {
                return Integer::I64(value);
            }
            if let Ok(value) = input.parse::<i128>() {
                return Integer::I128(value);
            }
            Integer::BigIntStr(input)
        }
    }
}

fn parse_prefixed_int(sign: Sign, digits: &str, radix: u32) -> Option<Integer<'static>> {
    const I64_MAX_MAGNITUDE: u128 = (1_u128 << 63) - 1;
    const I64_MIN_MAGNITUDE: u128 = 1_u128 << 63;
    const I128_MAX_MAGNITUDE: u128 = (1_u128 << 127) - 1;
    const I128_MIN_MAGNITUDE: u128 = 1_u128 << 127;

    if digits.is_empty() || !digits.chars().all(|ch| ch.is_digit(radix)) {
        return None;
    }

    match sign {
        Sign::Positive => {
            if let Ok(value) = i64::from_str_radix(digits, radix) {
                return Some(Integer::I64(value));
            }
            if let Ok(value) = i128::from_str_radix(digits, radix) {
                return Some(Integer::I128(value));
            }
            if let Ok(value) = u128::from_str_radix(digits, radix) {
                return Some(Integer::U128(value));
            }
        }
        Sign::Negative => {
            if let Ok(magnitude) = u128::from_str_radix(digits, radix) {
                if magnitude <= I64_MAX_MAGNITUDE
                    && let Ok(value) = i64::try_from(magnitude)
                {
                    return Some(Integer::I64(-value));
                }
                if magnitude == I64_MIN_MAGNITUDE {
                    return Some(Integer::I64(i64::MIN));
                }
                if magnitude <= I128_MAX_MAGNITUDE
                    && let Ok(value) = i128::try_from(magnitude)
                {
                    return Some(Integer::I128(-value));
                }
                if magnitude == I128_MIN_MAGNITUDE {
                    return Some(Integer::I128(i128::MIN));
                }
            }
        }
    }

    let decimal = convert_radix_digits_to_decimal(digits, radix)?;
    let normalized = match sign {
        Sign::Positive => decimal,
        Sign::Negative => {
            let mut value = String::with_capacity(decimal.len() + 1);
            value.push_str(sign.prefix());
            value.push_str(&decimal);
            value
        }
    };

    Some(Integer::BigIntStr(Cow::Owned(normalized)))
}

/// Coerce integer to f64 accepting precision loss.
#[allow(clippy::cast_precision_loss, reason = "Accepted precision loss")]
fn integer_to_f64(value: &Integer<'_>) -> Option<f64> {
    let decimal = value.to_decimal_string();
    parse_finite_f64(decimal.as_ref())
}

fn classify_decimal_numeric(input: &str) -> DecimalNumericKind {
    let bytes = input.as_bytes();
    if bytes.is_empty() {
        return DecimalNumericKind::NotNumeric;
    }

    let mut idx = 0;
    let mut saw_digit_before_exp = false;
    let mut saw_digit_after_exp = false;
    let mut saw_dot = false;
    let mut saw_exp = false;

    if matches!(bytes.get(idx), Some(b'+' | b'-')) {
        idx += 1;
        if idx == bytes.len() {
            return DecimalNumericKind::NotNumeric;
        }
    }

    while let Some(byte) = bytes.get(idx) {
        match byte {
            b'0'..=b'9' => {
                if saw_exp {
                    saw_digit_after_exp = true;
                } else {
                    saw_digit_before_exp = true;
                }
            }
            b'.' if !saw_dot && !saw_exp => {
                saw_dot = true;
            }
            b'e' | b'E' if saw_digit_before_exp && !saw_exp => {
                saw_exp = true;
                if matches!(bytes.get(idx + 1), Some(b'+' | b'-')) {
                    idx += 1;
                }
            }
            _ => return DecimalNumericKind::NotNumeric,
        }
        idx += 1;
    }

    if !saw_digit_before_exp {
        return DecimalNumericKind::NotNumeric;
    }

    if saw_exp && !saw_digit_after_exp {
        return DecimalNumericKind::NotNumeric;
    }

    if saw_dot || saw_exp {
        DecimalNumericKind::Float
    } else {
        DecimalNumericKind::Integer
    }
}

fn convert_radix_digits_to_decimal(digits: &str, radix: u32) -> Option<String> {
    let mut decimal_digits = vec![0_u8];

    for ch in digits.chars() {
        let digit = ch.to_digit(radix)?;
        multiply_decimal_digits(&mut decimal_digits, radix)?;
        add_decimal_digit(&mut decimal_digits, digit)?;
    }

    Some(
        decimal_digits
            .into_iter()
            .rev()
            .map(|digit| char::from(b'0' + digit))
            .collect(),
    )
}

fn multiply_decimal_digits(decimal_digits: &mut Vec<u8>, multiplier: u32) -> Option<()> {
    let mut carry = 0_u32;

    for digit in decimal_digits.iter_mut() {
        let value = (u32::from(*digit) * multiplier) + carry;
        let (new_digit, new_carry) = split_decimal(value)?;
        *digit = new_digit;
        carry = new_carry;
    }

    while carry > 0 {
        let (new_digit, new_carry) = split_decimal(carry)?;
        decimal_digits.push(new_digit);
        carry = new_carry;
    }

    Some(())
}

fn add_decimal_digit(decimal_digits: &mut Vec<u8>, addend: u32) -> Option<()> {
    let mut carry = addend;
    let mut idx = 0;

    while carry > 0 {
        if let Some(current) = decimal_digits.get_mut(idx) {
            let value = u32::from(*current) + carry;
            let (new_digit, new_carry) = split_decimal(value)?;
            *current = new_digit;
            carry = new_carry;
        } else {
            let (new_digit, new_carry) = split_decimal(carry)?;
            decimal_digits.push(new_digit);
            carry = new_carry;
        }
        idx += 1;
    }

    Some(())
}

fn split_decimal(value: u32) -> Option<(u8, u32)> {
    let mut carry = 0;
    let mut remainder = value;

    while remainder >= 10 {
        remainder -= 10;
        carry += 1;
    }

    let digit = match remainder {
        0 => 0,
        1 => 1,
        2 => 2,
        3 => 3,
        4 => 4,
        5 => 5,
        6 => 6,
        7 => 7,
        8 => 8,
        9 => 9,
        _ => return None,
    };

    Some((digit, carry))
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use super::{
        BuiltinScalarTag, ResolvedScalar, ScalarResolutionError, resolve_tagged_scalar,
        resolve_untagged_scalar,
    };
    use crate::event::ScalarStyle;
    use crate::value::Integer;

    #[test]
    fn resolves_plain_core_schema_scalars() {
        assert!(matches!(
            resolve_untagged_scalar(Cow::Borrowed("0o52"), ScalarStyle::Plain),
            ResolvedScalar::Int(Integer::I64(42))
        ));
        assert!(matches!(
            resolve_untagged_scalar(Cow::Borrowed("0x2A"), ScalarStyle::Plain),
            ResolvedScalar::Int(Integer::I64(42))
        ));
        assert!(matches!(
            resolve_untagged_scalar(Cow::Borrowed("+.INF"), ScalarStyle::Plain),
            ResolvedScalar::Float(value) if value.is_infinite() && value.is_sign_positive()
        ));
        assert!(matches!(
            resolve_untagged_scalar(Cow::Borrowed("~foo"), ScalarStyle::Plain),
            ResolvedScalar::String(text) if text == "~foo"
        ));
    }

    #[test]
    fn explicit_builtin_tag_mismatch_returns_original_text_in_error() {
        let resolution = resolve_tagged_scalar(Cow::Borrowed("hello"), "tag:yaml.org,2002:int");
        assert_eq!(
            resolution,
            Err(ScalarResolutionError::InvalidExplicitBuiltinTagValue {
                tag: BuiltinScalarTag::Int,
                original_text: Cow::Borrowed("hello"),
            })
        );
    }

    #[test]
    fn explicit_null_accepts_core_null_spellings() {
        for input in ["", "~", "null", "Null", "NULL"] {
            assert_eq!(
                resolve_tagged_scalar(Cow::Borrowed(input), "!!null"),
                Ok(ResolvedScalar::Null),
                "expected explicit null tag to accept {input:?}"
            );
        }
    }

    #[test]
    fn explicit_float_accepts_integer_lexemes() {
        for input in ["42", "0o52", "0x2A"] {
            assert_eq!(
                resolve_tagged_scalar(Cow::Borrowed(input), "!!float"),
                Ok(ResolvedScalar::Float(42.0)),
                "expected explicit float tag to accept {input:?}"
            );
        }
    }

    #[test]
    fn negative_prefixed_int_min_i128_resolves_to_concrete_i128() {
        assert_eq!(
            resolve_untagged_scalar(
                Cow::Borrowed("-0x80000000000000000000000000000000"),
                ScalarStyle::Plain
            ),
            ResolvedScalar::Int(Integer::I128(i128::MIN))
        );
    }

    #[test]
    fn overflowing_decimal_float_is_not_accepted_implicitly() {
        assert!(matches!(
            resolve_untagged_scalar(Cow::Borrowed("1e400"), ScalarStyle::Plain),
            ResolvedScalar::String(text) if text == "1e400"
        ));
    }

    #[test]
    fn overflowing_decimal_float_is_not_accepted_for_explicit_float() {
        assert_eq!(
            resolve_tagged_scalar(Cow::Borrowed("1e400"), "!!float"),
            Err(ScalarResolutionError::InvalidExplicitBuiltinTagValue {
                tag: BuiltinScalarTag::Float,
                original_text: Cow::Borrowed("1e400"),
            })
        );
    }
}
