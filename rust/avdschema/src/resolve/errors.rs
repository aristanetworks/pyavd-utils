// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use super::walker::SchemaWalkError;
use crate::source_store::SchemaStoreError;

/// Error encountered while resolving a schema reference.
#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum SchemaResolverError {
    #[display("Invalid syntax for schema $ref '{schema_ref}'.")]
    RefSyntax {
        schema_ref: String,
    },
    SchemaStore(SchemaStoreError),
    SchemaWalk(SchemaWalkError),
}
