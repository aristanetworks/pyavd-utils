// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Helpers for converting the AST (`Node` / `Value`) into the internal
//! YAML event stream used by the writer and test tooling.
//!
//! This module is intentionally independent of serde. It is the common
//! building block for:
//! - Serializing an existing AST back to YAML via the event-based writer.
//! - Roundtrip tests that go `parse -> AST -> events -> writer -> parse`.

use crate::event::{CollectionStyle, Event, Properties as EventProperties, ScalarStyle};
use crate::{Node, Value};

/// Errors that can occur while converting an AST to events.
#[derive(Debug, Clone, Copy)]
pub enum AstToEventsError {
    /// Encountered a non-finite floating-point value.
    UnsupportedFloat(f64),
}

/// Convert a single AST node into a complete event stream representing a
/// one-document YAML stream.
pub fn node_to_events<'input>(
    node: &'input Node<'input>,
) -> Result<Vec<Event<'input>>, AstToEventsError> {
    let mut events = Vec::new();
    events.push(Event::StreamStart);
    let span = node.span;
    events.push(Event::DocumentStart {
        explicit: false,
        span,
    });
    emit_node_events(node, &mut events)?;
    events.push(Event::DocumentEnd {
        explicit: false,
        span,
    });
    events.push(Event::StreamEnd);
    Ok(events)
}

/// Emit events for a single AST node (and its children).
fn emit_node_events<'input>(
    node: &'input Node<'input>,
    events: &mut Vec<Event<'input>>,
) -> Result<(), AstToEventsError> {
    let props = node_properties_to_event_properties(node);
    match &node.value {
        Value::Null => {
            events.push(Event::Scalar {
                style: ScalarStyle::Plain,
                value: "null".to_owned().into(),
                properties: props.into_boxed_option(),
                span: node.span,
            });
            Ok(())
        }
        Value::Bool(bool_value) => {
            let text = if *bool_value { "true" } else { "false" };
            events.push(Event::Scalar {
                style: ScalarStyle::Plain,
                value: text.to_owned().into(),
                properties: props.into_boxed_option(),
                span: node.span,
            });
            Ok(())
        }
        Value::Int(number) => {
            let text = number.to_decimal_string();
            events.push(Event::Scalar {
                style: ScalarStyle::Plain,
                value: text,
                properties: props.into_boxed_option(),
                span: node.span,
            });
            Ok(())
        }
        Value::Float(float_value) => {
            if !float_value.is_finite() {
                return Err(AstToEventsError::UnsupportedFloat(*float_value));
            }
            events.push(Event::Scalar {
                style: ScalarStyle::Plain,
                value: float_value.to_string().into(),
                properties: props.into_boxed_option(),
                span: node.span,
            });
            Ok(())
        }
        Value::String(string_value) => {
            events.push(Event::Scalar {
                style: ScalarStyle::DoubleQuoted,
                value: string_value.clone(),
                properties: props.into_boxed_option(),
                span: node.span,
            });
            Ok(())
        }
        Value::Sequence(items) => {
            events.push(Event::SequenceStart {
                style: CollectionStyle::Block,
                properties: props.into_boxed_option(),
                span: node.span,
            });
            for item in items {
                emit_node_events(&item.node, events)?;
            }
            events.push(Event::SequenceEnd { span: node.span });
            Ok(())
        }
        Value::Mapping(pairs) => {
            events.push(Event::MappingStart {
                style: CollectionStyle::Block,
                properties: props.into_boxed_option(),
                span: node.span,
            });
            for pair in pairs {
                emit_node_events(&pair.key, events)?;
                emit_node_events(&pair.value, events)?;
            }
            events.push(Event::MappingEnd { span: node.span });
            Ok(())
        }
    }
}

/// Convert node properties (anchor, tag) into event-layer properties.
fn node_properties_to_event_properties<'input>(
    node: &'input Node<'input>,
) -> EventProperties<'input> {
    node.properties.as_deref().cloned().unwrap_or_default()
}
