// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use std::iter::Peekable;

use ordermap::OrderMap;

use crate::any::SourceSchema;

pub(crate) trait Walker {
    fn walk<'a, I>(&self, path: Peekable<I>) -> Result<&SourceSchema, SchemaWalkError>
    where
        I: Iterator<Item = &'a str> + std::fmt::Debug;
}

impl Walker for SourceSchema {
    fn walk<'a, I>(&self, mut path: Peekable<I>) -> Result<&SourceSchema, SchemaWalkError>
    where
        I: Iterator<Item = &'a str> + std::fmt::Debug,
    {
        let Some(element) = path.next() else {
            return Ok(self);
        };
        match self {
            Self::List(schema) => {
                if element != "items" {
                    return Err(SchemaWalkError::PathNotFound {
                        element: element.to_owned(),
                    });
                }
                schema
                    .items
                    .as_deref()
                    .ok_or_else(|| SchemaWalkError::PathNotFound {
                        element: element.to_owned(),
                    })?
                    .walk(path)
            }
            Self::Dict(schema) => match element {
                "keys" => walk_mapping(schema.keys.as_ref(), element, path),
                "dynamic_keys" => walk_mapping(schema.dynamic_keys.as_ref(), element, path),
                "$defs" => walk_mapping(schema.schema_defs.as_ref(), element, path),
                _ => Err(SchemaWalkError::InvalidPathElement {
                    element: element.to_owned(),
                }),
            },
            Self::Bool(_) | Self::Int(_) | Self::Str(_) => Err(SchemaWalkError::NotDictOrList),
        }
    }
}

fn walk_mapping<'schema, 'path, I>(
    mapping: Option<&'schema OrderMap<String, SourceSchema>>,
    mapping_name: &str,
    mut path: Peekable<I>,
) -> Result<&'schema SourceSchema, SchemaWalkError>
where
    I: Iterator<Item = &'path str> + std::fmt::Debug,
{
    let mapping = mapping.ok_or_else(|| SchemaWalkError::PathNotFound {
        element: mapping_name.to_owned(),
    })?;
    let Some(key) = path.next() else {
        return Err(SchemaWalkError::PointingToMapping {
            mapping: mapping_name.to_owned(),
        });
    };
    mapping
        .get(key)
        .ok_or_else(|| SchemaWalkError::PathNotFound {
            element: key.to_owned(),
        })?
        .walk(path)
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
