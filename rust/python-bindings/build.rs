// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
//! Build script adding the `PyO3` extension-module linker arguments where required.

fn main() {
    pyo3_build_config::add_extension_module_link_args();
}
