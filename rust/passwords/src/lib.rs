// Copyright (c) 2025 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
#![deny(unused_crate_dependencies)]

// TODO: the crate is seen as unused, need to get why
use cipher as _;

mod cbc;
mod errors;
mod sha512crypt;

pub use cbc::{cbc_check_password, cbc_decrypt, cbc_encrypt};
pub use errors::{CbcError, InvalidSaltError, Sha512CryptError};
pub use sha512crypt::sha512_crypt;
