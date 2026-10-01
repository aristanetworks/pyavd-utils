// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Schema traversal and artifact generators.

mod legacy_python;
mod traversal;

pub use self::legacy_python::GenerationError;
pub use self::legacy_python::generate_python_models;
pub use self::legacy_python::generate_python_models_projection;
