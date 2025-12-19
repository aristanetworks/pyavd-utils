// Copyright (c) 2025 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
#![deny(unused_crate_dependencies)]

// Feature sha512

#[cfg(feature = "sha512")]
mod sha512;

#[cfg(feature = "sha512")]
pub use crate::sha512::{InvalidSaltError, Sha512CryptError, sha512_crypt};

// Feature cbc

#[cfg(feature = "cbc")]
mod cbc;

#[cfg(feature = "cbc")]
pub use crate::cbc::{CbcError, cbc_check_password, cbc_decrypt, cbc_encrypt};
