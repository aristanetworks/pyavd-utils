// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Shared occurrence-level context for documentation projections.

use crate::compiled::Common;

/// Resolve the effective documentation table for one schema occurrence.
///
/// Effective schema metadata takes precedence over ancestor context. An otherwise
/// unassigned top-level field starts a table named after its rendered key, with
/// dynamic-key brackets removed and underscores replaced by hyphens. Roots and
/// anonymous list items supply context too, but do not supply a fallback key.
///
/// This is the occurrence's grouping, not an exhaustive list of Markdown files
/// containing it: renderers may repeat ancestors or primary keys for context.
pub(super) fn documentation_table(
    common: &Common,
    parent_table: Option<&str>,
    top_level_key: Option<&str>,
) -> Option<String> {
    common
        .documentation_options
        .as_ref()
        .and_then(|options| options.table.clone())
        .or_else(|| parent_table.map(ToOwned::to_owned))
        .or_else(|| top_level_key.map(|key| key.replace(['<', '>'], "").replace('_', "-")))
}
