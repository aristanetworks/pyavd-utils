// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use rand::Rng;

const SIMPLE_7_SEED: &[u8] = b"dsfd;kfoA,.iyewrkldJKDHSUBsgvca69834ncxv9873254k;fg87";

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum Simple7Error {
    #[display("Invalid salt format in encrypted data")]
    InvalidSaltFormat(std::num::ParseIntError),
    #[display("Invalid hex encoding in encrypted data")]
    InvalidHexEncoding(hex::FromHexError),
    #[display("Decrypted data is not valid UTF-8")]
    InvalidUtf8(std::string::FromUtf8Error),
    #[display("Salt must be in the range 0-15, got {_0}")]
    InvalidSaltValue(u8),
    #[display("Encrypted data too short (minimum 2 characters required for salt)")]
    DataTooShort,
}
impl std::error::Error for Simple7Error {}

/// Decrypt (deobfuscate) a password from insecure type-7.
pub fn simple_7_decrypt(data: &str) -> Result<String, Simple7Error> {
    if data.len() < 2 {
        return Err(Simple7Error::DataTooShort);
    }

    let salt = data[0..2].parse::<usize>()?;

    // Validate salt is in valid range (0-15)
    if salt > 15 {
        return Err(Simple7Error::InvalidSaltValue(salt as u8));
    }

    let secret = hex::decode(&data[2..])?;

    let decrypted: Vec<u8> = secret
        .iter()
        .enumerate()
        .map(|(i, &byte)| byte ^ SIMPLE_7_SEED[(salt + i) % 53])
        .collect();

    Ok(String::from_utf8(decrypted)?)
}

/// Encrypt (obfuscate) a password with insecure type-7.
///
/// If `salt` is `None`, a random salt in the range 0-15 will be used.
/// Returns an error if the provided salt is not in the range 0-15.
pub fn simple_7_encrypt(data: &str, salt: Option<u8>) -> Result<String, Simple7Error> {
    let salt = match salt {
        Some(s) if s > 15 => return Err(Simple7Error::InvalidSaltValue(s)),
        Some(s) => s,
        None => rand::thread_rng().gen_range(0..16),
    };

    let cleartext = data.as_bytes();

    let encrypted: Vec<u8> = cleartext
        .iter()
        .enumerate()
        .map(|(i, &byte)| byte ^ SIMPLE_7_SEED[(salt as usize + i) % 53])
        .collect();

    Ok(format!("{:02}{}", salt, hex::encode_upper(encrypted)))
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_PASSWORD: &str = "foo";

    // (salt, encrypted_password) pairs for TEST_PASSWORD
    const VALID_ENCRYPT_DECRYPT_PAIRS: [(u8, &str); 7] = [
        (1, "0115090B"),
        (6, "0600002E"),
        (9, "094A4106"),
        (3, "03025404"),
        (12, "121F0A18"),
        (10, "10480616"),
        (15, "15140403"),
    ];

    // Invalid salt values for encryption
    const INVALID_SALT_VALUES: [u8; 3] = [16, 99, 255];

    #[test]
    fn test_simple_7_encrypt_ok() {
        for (salt, expected) in VALID_ENCRYPT_DECRYPT_PAIRS {
            let result = simple_7_encrypt(TEST_PASSWORD, Some(salt))
                .expect("Encryption failed");
            assert_eq!(result, expected, "Failed for salt {}", salt);
        }
    }

    #[test]
    fn test_simple_7_decrypt_ok() {
        for (salt, encrypted) in VALID_ENCRYPT_DECRYPT_PAIRS {
            let result = simple_7_decrypt(encrypted)
                .expect("Decryption failed");
            assert_eq!(result, TEST_PASSWORD, "Failed for salt {}", salt);
        }
    }

    #[test]
    fn test_simple_7_encrypt_decrypt_roundtrip() {
        let original = "test_password_123";
        let encrypted = simple_7_encrypt(original, Some(5))
            .expect("Encryption failed");
        let decrypted = simple_7_decrypt(&encrypted)
            .expect("Decryption failed");
        assert_eq!(decrypted, original);
    }

    #[test]
    fn test_simple_7_encrypt_random_salt() {
        let result = simple_7_encrypt(TEST_PASSWORD, None)
            .expect("Encryption with random salt failed");
        // Should be 2 chars for salt + hex encoded data
        assert!(result.len() >= 2);
        // Should be able to decrypt it back
        let decrypted = simple_7_decrypt(&result)
            .expect("Decryption failed");
        assert_eq!(decrypted, TEST_PASSWORD);
    }

    #[test]
    fn test_simple_7_encrypt_invalid_salt() {
        for salt in INVALID_SALT_VALUES {
            let result = simple_7_encrypt(TEST_PASSWORD, Some(salt));
            assert!(result.is_err(), "Expected error for salt {}", salt);
            assert!(
                matches!(result.unwrap_err(), Simple7Error::InvalidSaltValue(_)),
                "Expected InvalidSaltValue error for salt {}",
                salt
            );
        }
    }

    #[test]
    fn test_simple_7_decrypt_data_too_short() {
        let result = simple_7_decrypt("");
        assert!(matches!(result.unwrap_err(), Simple7Error::DataTooShort));

        let result = simple_7_decrypt("0");
        assert!(matches!(result.unwrap_err(), Simple7Error::DataTooShort));
    }

    #[test]
    fn test_simple_7_decrypt_invalid_hex() {
        let result = simple_7_decrypt("01GGGG");
        assert!(matches!(result.unwrap_err(), Simple7Error::InvalidHexEncoding(_)));
    }

    #[test]
    fn test_simple_7_decrypt_invalid_salt() {
        // Invalid salt format (not a number)
        let result = simple_7_decrypt("XX1234");
        assert!(matches!(result.unwrap_err(), Simple7Error::InvalidSaltFormat(_)));

        // Salt out of range (0-15)
        let result = simple_7_decrypt("161234");
        assert!(matches!(result.unwrap_err(), Simple7Error::InvalidSaltValue(16)));

        let result = simple_7_decrypt("991234");
        assert!(matches!(result.unwrap_err(), Simple7Error::InvalidSaltValue(99)));
    }
}
