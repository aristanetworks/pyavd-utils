// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Conservative string presentation for Serde output, independent of numeric ranges.

use crate::event::ScalarStyle;

/// Use plain style only when syntax and implicit typing preserve a string.
///
/// The syntax check is conservative across block, flow, key, and root contexts.
/// YAML 1.1 type-like strings are quoted defensively alongside YAML 1.2 Core
/// spellings. Quoting an extra string is preferable to changing its value/type.
pub(crate) fn string_style(value: &str) -> ScalarStyle {
    if plain_syntax_is_safe(value) && !has_implicit_type(value) {
        ScalarStyle::Plain
    } else {
        ScalarStyle::DoubleQuoted
    }
}

fn plain_syntax_is_safe(value: &str) -> bool {
    if value.is_empty() || value.trim() != value {
        return false;
    }
    let mut characters = value.chars().peekable();
    if characters.peek().is_some_and(|character| {
        matches!(
            character,
            '!' | '&'
                | '*'
                | '#'
                | '{'
                | '}'
                | '['
                | ']'
                | ','
                | '|'
                | '>'
                | '\''
                | '"'
                | '%'
                | '@'
                | '`'
        )
    }) {
        return false;
    }
    if value.starts_with(['-', '?', ':']) && value.chars().nth(1).is_none_or(char::is_whitespace) {
        return false;
    }
    for marker in ["---", "..."] {
        if let Some(rest) = value.strip_prefix(marker)
            && (rest.is_empty() || rest.starts_with(char::is_whitespace))
        {
            return false;
        }
    }
    let mut previous_was_space = false;
    while let Some(character) = characters.next() {
        if character.is_control()
            || matches!(
                character,
                '[' | ']'
                    | '{'
                    | '}'
                    | ','
                    | '\u{2028}'
                    | '\u{2029}'
                    | '\u{feff}'
                    | '\u{fffe}'
                    | '\u{ffff}'
            )
            || (character == '#' && previous_was_space)
            || (character == ':' && characters.peek().is_none_or(|next| next.is_whitespace()))
        {
            return false;
        }
        previous_was_space = character.is_whitespace();
    }
    true
}

fn has_implicit_type(value: &str) -> bool {
    matches!(value, "~" | "<<" | "=")
        || ["null", "true", "false", "yes", "no", "on", "off", "y", "n"]
            .iter()
            .any(|word| value.eq_ignore_ascii_case(word))
        || looks_like_number(value)
        || looks_like_timestamp(value)
}

fn unsigned_text(value: &str) -> &str {
    value.strip_prefix(['+', '-']).unwrap_or(value)
}

fn looks_like_number(value: &str) -> bool {
    let unsigned = unsigned_text(value);
    if unsigned.eq_ignore_ascii_case(".inf") || unsigned.eq_ignore_ascii_case(".nan") {
        return true;
    }
    for (prefix, radix) in [("0b", 2), ("0o", 8), ("0x", 16)] {
        if let Some(digits) = unsigned.strip_prefix(prefix) {
            return !digits.is_empty()
                && digits
                    .chars()
                    .all(|character| character == '_' || character.is_digit(radix));
        }
    }
    if !unsigned
        .as_bytes()
        .first()
        .is_some_and(|first| first.is_ascii_digit() || *first == b'.')
    {
        return false;
    }
    if unsigned.contains(':') {
        return looks_like_sexagesimal(unsigned);
    }
    let mantissa = if let Some((mantissa, exponent)) = unsigned.split_once(['e', 'E']) {
        if !decimal_digits(unsigned_text(exponent)) {
            return false;
        }
        mantissa
    } else {
        unsigned
    };
    if let Some((whole, fraction)) = mantissa.split_once('.') {
        (whole.is_empty() || decimal_digits(whole))
            && (fraction.is_empty() || decimal_digits(fraction))
            && mantissa.bytes().any(|byte| byte.is_ascii_digit())
    } else {
        decimal_digits(mantissa)
    }
}

fn decimal_digits(value: &str) -> bool {
    !value.is_empty()
        && value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || byte == b'_')
}

fn ascii_digits(value: &str) -> bool {
    !value.is_empty() && value.bytes().all(|byte| byte.is_ascii_digit())
}

fn looks_like_sexagesimal(value: &str) -> bool {
    let mut parts = value.split(':').peekable();
    if !parts.next().is_some_and(decimal_digits) {
        return false;
    }
    while let Some(part) = parts.next() {
        let whole = if let Some((whole, fraction)) = part.split_once('.') {
            if parts.peek().is_some() || (!fraction.is_empty() && !decimal_digits(fraction)) {
                return false;
            }
            whole
        } else {
            part
        };
        if !ascii_digits(whole)
            || whole.len() > 2
            || (whole.len() == 2 && whole.as_bytes().first().is_some_and(|digit| *digit > b'5'))
        {
            return false;
        }
    }
    true
}

fn looks_like_timestamp(value: &str) -> bool {
    let Some((year, rest)) = value.split_once('-') else {
        return false;
    };
    let Some((month, rest)) = rest.split_once('-') else {
        return false;
    };
    if year.len() != 4 || !ascii_digits(year) || month.len() > 2 || !ascii_digits(month) {
        return false;
    }
    let day_length = rest.bytes().take_while(u8::is_ascii_digit).count();
    if !(1..=2).contains(&day_length) {
        return false;
    }
    let (_, suffix) = rest.split_at(day_length);
    suffix.is_empty() || suffix.starts_with(['T', 't']) || suffix.starts_with(char::is_whitespace)
}
