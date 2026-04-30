// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::borrow::Cow;

use crate::ast_event::AstEvent;
use crate::error::ErrorKind;
use crate::event::ScalarStyle;
use crate::scalar_resolver::{
    ResolvedScalar, ScalarResolutionError, resolve_tagged_scalar, resolve_untagged_scalar,
};
use crate::span::Span;
use crate::value::{Node, Properties as NodeProperties};

use super::Parser;

impl<'input, I> Parser<'input, I>
where
    I: Iterator,
    I::Item: Into<AstEvent<'input>>,
{
    /// Build a scalar node with shared scalar semantic resolution.
    ///
    /// Plain untagged scalars follow YAML 1.2 Core implicit resolution.
    /// Quoted and block scalars stay as strings unless an explicit built-in tag
    /// overrides that. Explicit built-in tags are validated here so the AST and
    /// serde deserialization share the same scalar semantics.
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

        let resolved_scalar = if let Some(tag) = props
            .as_ref()
            .and_then(|event_props| event_props.tag.as_ref())
        {
            match resolve_tagged_scalar(value, style, tag.value.as_ref()) {
                Ok(resolved_scalar) => resolved_scalar,
                Err(ScalarResolutionError::InvalidExplicitBuiltinTagValue {
                    original_text,
                    ..
                }) => {
                    self.error(ErrorKind::InvalidValue, span);
                    ResolvedScalar::String(original_text)
                }
            }
        } else {
            resolve_untagged_scalar(value, style)
        };

        let base_node = Node::new(resolved_scalar.into_value(), span);
        let node = Self::apply_properties(base_node, props);
        self.store_anchor_node(&node);

        node
    }
}
