// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use base64ct::Base64ShaCrypt;
use base64ct::Encoding as _;
use sha_crypt::Params;
use sha_crypt::sha512_crypt as sha512_crypt_raw;

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum Sha512CryptError {
    // The errors from sha_crypt library should never happen in our case.
    #[display("SHA crypt library error: {_0:?}")]
    ShaCrypt(sha_crypt::Error),
    #[display("SHA crypt base64 invalid length error: {_0:?}")]
    Base64InvalidLength(base64ct::InvalidLengthError),
    #[display("Invalid Salt: {_0}")]
    InvalidSalt(InvalidSaltError),
}
impl std::error::Error for Sha512CryptError {}

#[derive(Debug, derive_more::Display)]
pub enum InvalidSaltError {
    #[display("Salt cannot be empty.")]
    IsEmpty,
    #[display("Salt contains an invalid character: '{_0}'")]
    InvalidCharacter(char),
}
impl std::error::Error for InvalidSaltError {}

pub fn sha512_crypt(password: &str, salt: &str) -> Result<String, Sha512CryptError> {
    // Setting rounds to 5000 which is the default for sha512crypt
    let params = Params::new(5_000)?;

    // Validate Salt
    validate_salt_characters(salt)?;

    // sha-crypt 0.6.0 removed the old sha512_crypt_b64 helper. Its high-level
    // MCF API cannot replace this because it emits "$6$rounds=5000$..." and
    // hashes over a base64-encoded salt, while EOS accepts the literal salt
    // format "$6$<salt>$<hash>". Keep using the low-level hash and reproduce
    // the SHA-512-crypt encoding step explicitly to preserve the public output.
    let raw_hash = sha512_crypt_raw(password.as_bytes(), salt.as_bytes(), params);
    let transposed_hash = transpose_sha512_hash(raw_hash);
    let mut encoded_hash = [0_u8; 86];
    let hashed_password = Base64ShaCrypt::encode(&transposed_hash, &mut encoded_hash)?;
    Ok(format!("$6${salt}${hashed_password}"))
}

#[allow(
    clippy::indexing_slicing,
    reason = "constant transposition table only contains valid SHA-512 digest indexes"
)]
fn transpose_sha512_hash(raw_hash: [u8; 64]) -> [u8; 64] {
    // SHA-512-crypt serializes the digest in this transposed byte order before
    // applying the crypt base64 alphabet. This table mirrors sha-crypt 0.6.0's
    // private MCF implementation; the public low-level API returns pre-
    // transposition bytes.
    const TRANSPOSITION_TABLE: [usize; 64] = [
        42, 21, 0, 1, 43, 22, 23, 2, 44, 45, 24, 3, 4, 46, 25, 26, 5, 47, 48, 27, 6, 7, 49, 28, 29,
        8, 50, 51, 30, 9, 10, 52, 31, 32, 11, 53, 54, 33, 12, 13, 55, 34, 35, 14, 56, 57, 36, 15,
        16, 58, 37, 38, 17, 59, 60, 39, 18, 19, 61, 40, 41, 20, 62, 63,
    ];

    let mut transposed_hash = [0_u8; 64];
    for (output_byte, input_index) in transposed_hash.iter_mut().zip(TRANSPOSITION_TABLE) {
        *output_byte = raw_hash[input_index];
    }
    transposed_hash
}

/// Verify that the salt is only composed of valid characters: [a-zA-Z0-9/.]
fn validate_salt_characters(salt: &str) -> Result<(), InvalidSaltError> {
    if salt.is_empty() {
        return Err(InvalidSaltError::IsEmpty);
    }

    for character in salt.chars() {
        if !(character.is_ascii_alphanumeric() || character == '/' || character == '.') {
            return Err(InvalidSaltError::InvalidCharacter(character));
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_salt() {
        let salt = "1234567890ABCDEF";
        let password = "LittleDropBobbyTable";

        let result = sha512_crypt(password, salt).unwrap();
        assert_eq!(
            result,
            "$6$1234567890ABCDEF$Em9R7hgj77mOWT2JjGxPzUQEXpe0HmEpcxlhR5W.cMjg48.AJ1L3qFxTKuvXdmsiisbVh04tvKKH1ab.15PaD1",
            "Incorrect hash returned."
        );
    }

    #[test]
    fn invalid_salt_bad_characters() {
        // invalid characters
        let salt = "🦀$;";
        let password = "LittleDropBobbyTable";

        let result = sha512_crypt(password, salt).unwrap_err();
        assert!(matches!(
            result,
            Sha512CryptError::InvalidSalt(InvalidSaltError::InvalidCharacter('🦀'))
        ));
    }

    #[test]
    fn invalid_salt_empty() {
        // empty salt
        let salt = "";
        let password = "LittleDropBobbyTable";

        let result = sha512_crypt(password, salt).unwrap_err();

        assert!(matches!(
            result,
            Sha512CryptError::InvalidSalt(InvalidSaltError::IsEmpty)
        ));
    }
}
