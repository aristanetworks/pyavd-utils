// Copyright (c) 2025 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
#![deny(unused_crate_dependencies)]

mod errors;

#[cfg(feature = "sha512")]
mod sha512crypt;

#[cfg(feature = "cbc")]
mod cbc;

#[cfg(feature = "sha512")]
pub use crate::{
    errors::{InvalidSaltError, Sha512CryptError},
    sha512crypt::sha512_crypt,
};

#[cfg(feature = "cbc")]
pub use crate::{
    cbc::{cbc_check_password, cbc_decrypt, cbc_encrypt},
    errors::CbcError,
};

#[cfg(feature = "cbc")]
use cipher as _;
