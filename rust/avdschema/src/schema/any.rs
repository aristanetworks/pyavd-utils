// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde::Deserialize;
use serde::Serialize;

use super::boolean::SourceBool;
use super::dict::SourceDict;
use super::dict::SourceRootDict;
use super::int::SourceInt;
use super::list::SourceList;
use super::str::SourceStr;
use crate::utils::dump::Dump;
use crate::utils::load::Load;

/// Enum covering recursive AVD schema types. Named schema roots use [`SourceRootDict`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_more::From)]
#[cfg_attr(feature = "metaschema", derive(schemars::JsonSchema))]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum SourceSchema {
    Bool(SourceBool),
    Int(SourceInt),
    Str(SourceStr),
    List(SourceList),
    Dict(SourceDict),
}

/// Borrowed source-schema layer used while resolving and compiling references.
///
/// Named roots have a stricter authoring model than recursive schemas, but a reference to an
/// entire named schema still contributes a dictionary layer at the reference occurrence.
#[derive(Debug, Clone, Copy)]
pub(crate) enum SourceLayer<'a> {
    Root(&'a SourceRootDict),
    Schema(&'a SourceSchema),
}

/// Discriminator shared by root and recursive source-schema layers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SourceType {
    Bool,
    Int,
    Str,
    List,
    Dict,
}

impl SourceType {
    pub(crate) const fn as_str(self) -> &'static str {
        match self {
            Self::Bool => "bool",
            Self::Int => "int",
            Self::Str => "str",
            Self::List => "list",
            Self::Dict => "dict",
        }
    }
}

impl<'a> SourceLayer<'a> {
    pub(crate) fn root(schema: &'a SourceRootDict) -> Self {
        Self::Root(schema)
    }

    pub(crate) fn schema(schema: &'a SourceSchema) -> Self {
        Self::Schema(schema)
    }

    pub(crate) fn identity(self) -> *const () {
        match self {
            Self::Root(schema) => std::ptr::from_ref(schema).cast(),
            Self::Schema(schema) => std::ptr::from_ref(schema).cast(),
        }
    }

    pub(crate) fn schema_type(self) -> SourceType {
        match self {
            Self::Root(_) | Self::Schema(SourceSchema::Dict(_)) => SourceType::Dict,
            Self::Schema(SourceSchema::Bool(_)) => SourceType::Bool,
            Self::Schema(SourceSchema::Int(_)) => SourceType::Int,
            Self::Schema(SourceSchema::Str(_)) => SourceType::Str,
            Self::Schema(SourceSchema::List(_)) => SourceType::List,
        }
    }

    pub(crate) fn schema_ref(self) -> Option<&'a str> {
        match self {
            Self::Root(schema) => schema.base.schema_ref.as_deref(),
            Self::Schema(SourceSchema::Bool(schema)) => schema.base.schema_ref.as_deref(),
            Self::Schema(SourceSchema::Int(schema)) => schema.base.schema_ref.as_deref(),
            Self::Schema(SourceSchema::Str(schema)) => schema.base.schema_ref.as_deref(),
            Self::Schema(SourceSchema::List(schema)) => schema.base.schema_ref.as_deref(),
            Self::Schema(SourceSchema::Dict(schema)) => schema.base.schema_ref.as_deref(),
        }
    }
}
impl Dump for SourceSchema {}
impl Load for SourceSchema {}
