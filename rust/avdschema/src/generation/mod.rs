// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Schema traversal and artifact generators.

mod explorer;
mod legacy_python;
mod markdown;
mod traversal;

pub use self::explorer::SchemaExplorerRow;
pub use self::explorer::generate_schema_explorer_rows;
pub use self::legacy_python::GenerationError;
pub use self::legacy_python::generate_python_models;
pub use self::legacy_python::generate_python_models_projection;
pub use self::markdown::DocumentationGenerationError;
pub use self::markdown::generate_markdown_documentation;
