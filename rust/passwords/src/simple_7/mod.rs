// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

const SIMPLE_7_SEED: &[u8] = b"dsfd;kfoA,.iyewrkldJKDHSUBsgvca69834ncxv9873254k;fg87";

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum Simple7Error {
    #[display("Invalid salt format in encrypted data")]
    InvalidSaltFormat(std::num::ParseIntError),
    #[display("Invalid hex encoding in encrypted data")]
    InvalidHexEncoding(hex::FromHexError),
    #[display("Failed to obtain random salt from the operating system")]
    RandomSourceUnavailable(getrandom::Error),
    #[display("Decrypted data is not valid UTF-8")]
    InvalidUtf8(std::string::FromUtf8Error),
    #[display("Salt must be in the range 0-15, got {_0}")]
    InvalidSaltValue(u8),
    #[display("Encrypted data too short (minimum 2 characters required for salt)")]
    DataTooShort,
    #[display("Password must not be empty")]
    EmptyPassword,
}
impl std::error::Error for Simple7Error {}

/// Decrypt (deobfuscate) a password from insecure type-7.
///
/// The salt prefix must contain exactly two ASCII decimal digits in the range 00-15.
pub fn simple_7_decrypt(data: &str) -> Result<String, Simple7Error> {
    if data.len() < 2 {
        return Err(Simple7Error::DataTooShort);
    }

    let mut characters = data.chars();
    let mut salt = 0_u8;
    // Parse each digit separately to reject signs and retain InvalidSaltFormat errors.
    // Advancing by characters avoids slicing inside a UTF-8 character on malformed input.
    for _ in 0..2 {
        let character = characters.next().ok_or(Simple7Error::DataTooShort)?;
        let mut buffer = [0; 4];
        let digit = character.encode_utf8(&mut buffer).parse::<u8>()?;
        salt = salt * 10 + digit;
    }

    // Validate salt is in valid range (0-15)
    if salt > 15 {
        return Err(Simple7Error::InvalidSaltValue(salt));
    }

    let secret = hex::decode(characters.as_str())?;

    let decrypted: Vec<u8> = secret
        .iter()
        .zip(SIMPLE_7_SEED.iter().cycle().skip(usize::from(salt)))
        .map(|(&byte, &seed_byte)| byte ^ seed_byte)
        .collect();

    Ok(String::from_utf8(decrypted)?)
}

/// Encrypt (obfuscate) a password with insecure type-7.
///
/// If `salt` is `None`, a random salt in the range 0-15 will be used.
/// Returns an error if the provided salt is not in the range 0-15, or if `data` is empty.
pub fn simple_7_encrypt(data: &str, salt: Option<u8>) -> Result<String, Simple7Error> {
    if data.is_empty() {
        return Err(Simple7Error::EmptyPassword);
    }
    let salt = match salt {
        Some(salt) if salt > 15 => return Err(Simple7Error::InvalidSaltValue(salt)),
        Some(salt) => salt,
        None => {
            let mut random_byte = [0_u8; 1];
            getrandom::fill(&mut random_byte)?;
            random_byte[0] & 0x0f
        }
    };

    let cleartext = data.as_bytes();

    let encrypted: Vec<u8> = cleartext
        .iter()
        .zip(SIMPLE_7_SEED.iter().cycle().skip(usize::from(salt)))
        .map(|(&byte, &seed_byte)| byte ^ seed_byte)
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
            let result = simple_7_encrypt(TEST_PASSWORD, Some(salt)).expect("Encryption failed");
            assert_eq!(result, expected, "Failed for salt {salt}");
        }
    }

    #[test]
    fn test_simple_7_decrypt_ok() {
        for (salt, encrypted) in VALID_ENCRYPT_DECRYPT_PAIRS {
            let result = simple_7_decrypt(encrypted).expect("Decryption failed");
            assert_eq!(result, TEST_PASSWORD, "Failed for salt {salt}");
        }
    }

    #[test]
    fn test_simple_7_encrypt_decrypt_roundtrip() {
        let original = "test_password_123";
        let encrypted = simple_7_encrypt(original, Some(5)).expect("Encryption failed");
        let decrypted = simple_7_decrypt(&encrypted).expect("Decryption failed");
        assert_eq!(decrypted, original);
    }

    #[test]
    fn test_simple_7_encrypt_random_salt() {
        let result =
            simple_7_encrypt(TEST_PASSWORD, None).expect("Encryption with random salt failed");
        // Should be 2 chars for salt + hex encoded data
        assert!(result.len() >= 2);
        // Should be able to decrypt it back
        let decrypted = simple_7_decrypt(&result).expect("Decryption failed");
        assert_eq!(decrypted, TEST_PASSWORD);
    }

    #[test]
    fn test_simple_7_encrypt_invalid_salt() {
        for salt in INVALID_SALT_VALUES {
            let result = simple_7_encrypt(TEST_PASSWORD, Some(salt));
            assert!(result.is_err(), "Expected error for salt {salt}");
            assert!(
                matches!(result.unwrap_err(), Simple7Error::InvalidSaltValue(_)),
                "Expected InvalidSaltValue error for salt {salt}"
            );
        }
    }

    #[test]
    fn test_simple_7_encrypt_empty_password() {
        let result = simple_7_encrypt("", Some(5));
        assert!(matches!(result.unwrap_err(), Simple7Error::EmptyPassword));
    }

    #[test]
    fn test_simple_7_decrypt_data_too_short() {
        let empty_result = simple_7_decrypt("");
        assert!(matches!(
            empty_result.unwrap_err(),
            Simple7Error::DataTooShort
        ));

        let short_result = simple_7_decrypt("0");
        assert!(matches!(
            short_result.unwrap_err(),
            Simple7Error::DataTooShort
        ));
    }

    #[test]
    fn test_simple_7_decrypt_invalid_hex() {
        let result = simple_7_decrypt("01GGGG");
        assert!(matches!(
            result.unwrap_err(),
            Simple7Error::InvalidHexEncoding(_)
        ));
    }

    #[test]
    fn test_simple_7_decrypt_invalid_salt() {
        // Invalid salt format (not a number)
        let invalid_format_result = simple_7_decrypt("XX1234");
        assert!(matches!(
            invalid_format_result.unwrap_err(),
            Simple7Error::InvalidSaltFormat(_)
        ));

        // Salt out of range (0-15)
        let salt_16_result = simple_7_decrypt("161234");
        assert!(matches!(
            salt_16_result.unwrap_err(),
            Simple7Error::InvalidSaltValue(16)
        ));

        let salt_99_result = simple_7_decrypt("991234");
        assert!(matches!(
            salt_99_result.unwrap_err(),
            Simple7Error::InvalidSaltValue(99)
        ));
    }

    #[test]
    fn test_simple_7_decrypt_malformed_salt_prefixes() {
        for data in [
            "é",
            "€",
            "🦀1234",
            "0é1234",
            "0€1234",
            "é01234",
            "１２1234",
            "+115090B",
            "-11234",
            " 11234",
            "1 1234",
        ] {
            let error = simple_7_decrypt(data).expect_err("Malformed salt must be rejected");
            assert!(matches!(error, Simple7Error::InvalidSaltFormat(_)));
            assert_eq!(error.to_string(), "Invalid salt format in encrypted data");
        }
    }

    #[test]
    fn test_simple_7_roundtrip_all_salts_with_unicode_password() {
        for salt in 0..=15 {
            let encrypted = simple_7_encrypt("päss€🦀", Some(salt)).expect("Encryption failed");
            assert_eq!(
                simple_7_decrypt(&encrypted).expect("Decryption failed"),
                "päss€🦀"
            );
        }
    }

    #[test]
    fn test_simple_7_decrypt_salt_only_and_non_ascii_payload() {
        assert_eq!(simple_7_decrypt("00").expect("Empty payload is valid"), "");
        assert!(matches!(
            simple_7_decrypt("00é"),
            Err(Simple7Error::InvalidHexEncoding(_))
        ));
        assert!(matches!(
            simple_7_decrypt("16é"),
            Err(Simple7Error::InvalidSaltValue(16))
        ));
    }
}
