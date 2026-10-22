// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Resolution of typed schema edges against authored declarations.

use ordermap::OrderMap;

use crate::SchemaPathElement;
use crate::any::SourceLayer;
use crate::any::SourceSchema;
use crate::dict::SourceRootDict;

/// Follow structural schema edges; references are expanded separately by layer resolution.
pub(crate) trait Walker {
    fn walk(&self, path: &[SchemaPathElement]) -> Result<SourceLayer<'_>, SchemaWalkError>;
}

impl Walker for SourceSchema {
    fn walk(&self, path: &[SchemaPathElement]) -> Result<SourceLayer<'_>, SchemaWalkError> {
        let Some((element, remaining)) = path.split_first() else {
            return Ok(SourceLayer::schema(self));
        };
        match self {
            Self::List(schema) => match element {
                SchemaPathElement::Items => schema
                    .items
                    .as_deref()
                    .ok_or_else(|| SchemaWalkError::PathNotFound {
                        element: "items".to_owned(),
                    })?
                    .walk(remaining),
                _ => Err(SchemaWalkError::PathNotFound {
                    element: element_name(element),
                }),
            },
            Self::Dict(schema) => match element {
                SchemaPathElement::Key { key } => {
                    walk_mapping(schema.keys.as_ref(), key, "keys", key, remaining)
                }
                _ => Err(SchemaWalkError::InvalidPathElement {
                    element: element_name(element),
                }),
            },
            Self::Bool(_) | Self::Int(_) | Self::Str(_) => Err(SchemaWalkError::NotDictOrList),
        }
    }
}

impl Walker for SourceRootDict {
    fn walk(&self, path: &[SchemaPathElement]) -> Result<SourceLayer<'_>, SchemaWalkError> {
        let Some((element, remaining)) = path.split_first() else {
            return Ok(SourceLayer::root(self));
        };
        match element {
            SchemaPathElement::Key { key } => {
                walk_mapping(self.keys.as_ref(), key, "keys", key, remaining)
            }
            SchemaPathElement::DynamicKey { source } => walk_mapping(
                self.dynamic_keys.as_ref(),
                source,
                "dynamic_keys",
                source.path(),
                remaining,
            ),
            SchemaPathElement::Definition { name } => {
                walk_mapping(self.schema_defs.as_ref(), name, "$defs", name, remaining)
            }
            SchemaPathElement::Items => Err(SchemaWalkError::InvalidPathElement {
                element: "items".to_owned(),
            }),
        }
    }
}

fn element_name(element: &SchemaPathElement) -> String {
    match element {
        SchemaPathElement::Key { .. } => "keys",
        SchemaPathElement::DynamicKey { .. } => "dynamic_keys",
        SchemaPathElement::Items => "items",
        SchemaPathElement::Definition { .. } => "$defs",
    }
    .to_owned()
}

fn walk_mapping<'a, K: Eq + std::hash::Hash>(
    mapping: Option<&'a OrderMap<K, SourceSchema>>,
    key: &K,
    mapping_name: &str,
    display_key: &str,
    remaining: &[SchemaPathElement],
) -> Result<SourceLayer<'a>, SchemaWalkError> {
    mapping
        .ok_or_else(|| SchemaWalkError::PathNotFound {
            element: mapping_name.to_owned(),
        })?
        .get(key)
        .ok_or_else(|| SchemaWalkError::PathNotFound {
            element: display_key.to_owned(),
        })?
        .walk(remaining)
}

/// Structured failure encountered while walking a reference path.
#[derive(Debug, derive_more::Display)]
pub enum SchemaWalkError {
    #[display(
        "Invalid schema path. The element '{element}' is invalid. All path elements except the last must go via lists or dicts."
    )]
    InvalidPathElement { element: String },
    #[display(
        "Invalid schema path. An intermediate element pointed to a schema that is not a dict or list."
    )]
    NotDictOrList,
    #[display("Invalid schema path. The element '{element}' was not found.")]
    PathNotFound { element: String },
    /// The path ended at a mapping instead of selecting a schema from it.
    #[display("Invalid schema path. A path cannot end at the '{mapping}' schema mapping.")]
    PointingToMapping {
        /// Name of the terminal mapping, such as `keys`, `dynamic_keys`, or `$defs`.
        mapping: String,
    },
}
