// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use ordermap::OrderMap;

/// Maps a concrete input key to the dynamic key schema path that should validate it.
///
/// Used when the dynamic key cannot be inferred from input/default schema data,
/// for example when LSP comments identify the intended dynamic-key source.
/// Callers resolving both static and dynamic schema keys should give static
/// schema keys precedence over these overrides.
pub type DynamicKeyOverrides = OrderMap<String, String>;
