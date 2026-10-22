// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Reference-layer expansion shared by compilation and build-time traversal.
//!
//! Layers are returned in precedence order: each declaration precedes the
//! schemas it references. This operation resolves only the layers at one
//! schema position; callers decide when to process its children. Runtime
//! validation reads compiled tables and does not use this source operation.

use crate::CompileError;
use crate::SchemaDiagnostic;
use crate::SchemaPath;
use crate::StoreSource;
use crate::any::SourceLayer;
use crate::resolve::resolve_ref::resolve_ref;

/// Expanded declarations and their parsed references, both in precedence order.
pub(crate) struct ExpandedLayers<'a> {
    /// Each declaration precedes the layers it references.
    pub layers: Vec<SourceLayer<'a>>,
    /// One address for each layer containing a reference, in matching layer order.
    /// Only build-time traversal retains these addresses after resolution.
    #[cfg(feature = "generation")]
    pub references: Vec<SchemaPath>,
}

/// Expand reference chains without merging properties or visiting children.
pub(crate) fn expand_layers<'a>(
    source: &'a StoreSource,
    declared: &[SourceLayer<'a>],
    schema_path: &SchemaPath,
) -> Result<ExpandedLayers<'a>, CompileError> {
    let Some(first_declared) = declared.first().copied() else {
        return Err(SchemaDiagnostic::TypeMismatch {
            expected: "schema",
            found: "no declared layers",
        }
        .into());
    };
    let mut result = Vec::new();
    #[cfg(feature = "generation")]
    let mut references = Vec::new();
    for declared_layer in declared {
        let mut layer = *declared_layer;
        let mut chain = Vec::new();
        loop {
            if first_declared.schema_type() != layer.schema_type() {
                return Err(SchemaDiagnostic::TypeMismatch {
                    expected: first_declared.schema_type().as_str(),
                    found: layer.schema_type().as_str(),
                }
                .into());
            }
            chain.push(layer.identity());
            result.push(layer);
            let Some(reference) = layer.schema_ref() else {
                break;
            };
            let resolved =
                resolve_ref(reference, source).map_err(|error| SchemaDiagnostic::Reference {
                    schema_path: schema_path.clone(),
                    reference: reference.to_owned(),
                    error,
                })?;
            layer = resolved.target;
            #[cfg(feature = "generation")]
            references.push(resolved.path);
            if chain.contains(&layer.identity()) {
                return Err(SchemaDiagnostic::ReferenceCycle {
                    schema_path: schema_path.clone(),
                    reference: reference.to_owned(),
                }
                .into());
            }
        }
    }
    Ok(ExpandedLayers {
        layers: result,
        #[cfg(feature = "generation")]
        references,
    })
}
