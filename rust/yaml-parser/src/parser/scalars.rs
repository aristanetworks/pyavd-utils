// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use crate::ast_event::AstEvent;
use crate::error::ErrorKind;
use crate::event::ScalarStyle;
use crate::span::Span;
use crate::value::{Integer, Node, Properties as NodeProperties, Value};

use super::{NumericKind, Parser};

impl<'input, I> Parser<'input, I>
where
    I: Iterator,
    I::Item: Into<AstEvent<'input>>,
{
    /// Build a scalar node with type inference.
    ///
    /// Plain scalars are inferred before explicit tags are interpreted as
    /// annotations, so `!!str 42` currently becomes `Int(42)` with a `!!str`
    /// tag attached to the node.
    pub(super) fn build_scalar(
        &mut self,
        style: ScalarStyle,
        value: Cow<'input, str>,
        props: Option<Box<NodeProperties<'input>>>,
        span: Span,
    ) -> Node<'input> {
        self.register_anchor(
            props
                .as_ref()
                .and_then(|event_props| event_props.anchor.as_ref()),
        );

        let invalid_explicit_null = style == ScalarStyle::Plain
            && props
                .as_ref()
                .and_then(|event_props| event_props.tag.as_ref())
                .is_some_and(|tag| {
                    tag.value == "tag:yaml.org,2002:null"
                        && !value.is_empty()
                        && value.as_ref() != "null"
                });

        // Type inference applies to plain scalars regardless of tag. We may
        // still record additional errors later if an explicit tag is
        // incompatible with the scalar's textual representation.
        let typed_value = if style == ScalarStyle::Plain {
            infer_scalar_type(value)
        } else {
            // Quoted/block scalars are always strings
            Value::String(value)
        };

        let base_node = Node::new(typed_value, span);
        let node = Self::apply_properties(base_node, props);
        self.store_anchor_node(&node);

        // Validate core-schema tags against the scalar's textual form.
        // For now we are conservative and only enforce additional rules for
        // the `!!null` tag. Other core tags currently continue to follow the
        // "type inference first, tags as annotations" behaviour.
        //
        // NOTE: We adopt a *mostly strict* policy for explicit `!!null` to
        // match the behaviour of `serde_yaml` and `saphyr` while still staying
        // compatible with the YAML test suite:
        // - For plain scalars tagged `!!null`, we accept either the canonical
        //   textual form `null` or an *empty* scalar as valid representations
        //   of `tag:yaml.org,2002:null`.
        // - Any other textual form (e.g. `Null`, `NULL`, `~`, `str`, `0`, ...)
        //   is treated as an invalid use of the null tag and recorded as a
        //   parse error.
        //
        // This keeps `!!null str` and similar cases aligned with
        // `serde_yaml`/`saphyr`, but allows "tags on empty scalars" such as
        // those in the FH7J YAML test to remain error-free.
        if invalid_explicit_null {
            // Recognised null tag whose textual content is neither
            // empty nor the canonical "null" is treated as an
            // invalid null scalar. We still keep the inferred
            // `Value::Null` in the AST so error-recovery consumers can
            // inspect partial data, but callers like `parse_ok` and
            // `serde::from_str` will see this as a parse error.
            self.error(ErrorKind::InvalidValue, span);
        }

        node
    }
}

/// Classify whether a scalar is a plausible integer, a plausible float, or
/// definitely not numeric using a single byte scan.
#[inline]
fn classify_numeric(input: &str) -> NumericKind {
    let bytes = input.as_bytes();
    if bytes.is_empty() {
        return NumericKind::NotNumeric;
    }

    let mut idx = 0;
    if matches!(bytes.get(idx), Some(b'+' | b'-')) {
        idx += 1;
        if idx == bytes.len() {
            return NumericKind::NotNumeric;
        }
    }

    let mut saw_digit = false;
    let mut saw_dot = false;
    let mut saw_exp = false;
    let mut expect_exp_digit = false;

    while idx < bytes.len() {
        match bytes.get(idx) {
            Some(b'0'..=b'9') => {
                saw_digit = true;
                expect_exp_digit = false;
            }
            Some(b'.') if !saw_dot && !saw_exp => {
                saw_dot = true;
            }
            Some(b'e' | b'E') if saw_digit && !saw_exp => {
                saw_exp = true;
                expect_exp_digit = true;
                saw_digit = false;
                if matches!(bytes.get(idx + 1), Some(b'+' | b'-')) {
                    idx += 1;
                }
            }
            _ => return NumericKind::NotNumeric,
        }
        idx += 1;
    }

    if !saw_digit || expect_exp_digit {
        return NumericKind::NotNumeric;
    }
    if saw_dot || saw_exp {
        NumericKind::Float
    } else {
        NumericKind::Integer
    }
}

/// Infer the type of a plain scalar value.
///
/// Per YAML 1.2 Core Schema:
/// - null: null, Null, NULL, ~, empty
/// - bool: true, True, TRUE, false, False, FALSE
/// - int: decimal integers
/// - float: decimal floats, .inf, -.inf, .nan
/// - everything else is a string
///
/// Takes a `Cow<'input, str>` to avoid unnecessary allocations when the value
/// is inferred as a string (can return the Cow as-is).
pub(crate) fn infer_scalar_type(value: Cow<'_, str>) -> Value<'_> {
    let text = value.as_ref();
    match text.as_bytes().first() {
        None => Value::Null,
        Some(b'~') if text.len() == 1 => Value::Null,
        Some(b'n') if matches!(text, "null") => Value::Null,
        Some(b'N') if matches!(text, "Null" | "NULL") => Value::Null,
        Some(b't') if text == "true" => Value::Bool(true),
        Some(b'T') if matches!(text, "True" | "TRUE") => Value::Bool(true),
        Some(b'f') if text == "false" => Value::Bool(false),
        Some(b'F') if matches!(text, "False" | "FALSE") => Value::Bool(false),
        Some(b'0'..=b'9' | b'+' | b'-' | b'.') => {
            match text {
                ".inf" | ".Inf" | ".INF" => return Value::Float(f64::INFINITY),
                "-.inf" | "-.Inf" | "-.INF" => return Value::Float(f64::NEG_INFINITY),
                ".nan" | ".NaN" | ".NAN" => return Value::Float(f64::NAN),
                _ => {}
            }

            match classify_numeric(text) {
                NumericKind::Float => {
                    if let Ok(float) = text.parse::<f64>() {
                        return Value::Float(float);
                    }
                }
                NumericKind::Integer => {
                    if let Ok(int) = text.parse::<i64>() {
                        return Value::Int(Integer::I64(int));
                    }
                    if let Ok(int) = text.parse::<i128>() {
                        return Value::Int(Integer::I128(int));
                    }
                    if let Ok(uint) = text.parse::<u128>() {
                        return Value::Int(Integer::U128(uint));
                    }
                    return Value::Int(Integer::BigIntStr(value));
                }
                NumericKind::NotNumeric => {}
            }

            Value::String(value)
        }
        _ => Value::String(value),
    }
}
