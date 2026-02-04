// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Ansible Vault v1.1/v1.2 compatible encryption and decryption.
//!
//! Format without vault ID (v1.1):
//! ```text
//! $ANSIBLE_VAULT;1.1;AES256
//! <hex-encoded payload, 80 chars per line>
//! ```
//!
//! Format with vault ID (v1.2):
//! ```text
//! $ANSIBLE_VAULT;1.2;AES256;vault_id
//! <hex-encoded payload, 80 chars per line>
//! ```
//!
//! The payload is hex(salt || hmac || ciphertext) where:
//! - salt: 32 bytes random
//! - hmac: HMAC-SHA256 of ciphertext
//! - ciphertext: AES-256-CTR encrypted plaintext with PKCS7 padding

use aes::cipher::{KeyIvInit as _, StreamCipher as _};
use hmac::{Hmac, Mac as _};
use rand::Rng as _;
use sha2::Sha256;

use crate::EncryptError;

type Aes256Ctr = ctr::Ctr128BE<aes::Aes256>;
type HmacSha256 = Hmac<Sha256>;

const SALT_SIZE: usize = 32;
const KEY_SIZE: usize = 32;
const HMAC_SIZE: usize = 32;
const IV_SIZE: usize = 16;
const AES_BLOCK_SIZE: usize = 16;
const PBKDF2_ITERATIONS: u32 = 10000;
const VAULT_LINE_WIDTH: usize = 80;

/// Derive encryption keys from password and salt using PBKDF2-HMAC-SHA256.
/// Returns (cipher_key, hmac_key, iv) - 80 bytes total.
fn derive_keys(password: &[u8], salt: &[u8]) -> ([u8; KEY_SIZE], [u8; HMAC_SIZE], [u8; IV_SIZE]) {
    let mut derived = [0u8; KEY_SIZE + HMAC_SIZE + IV_SIZE];
    pbkdf2::pbkdf2_hmac::<Sha256>(password, salt, PBKDF2_ITERATIONS, &mut derived);

    let mut cipher_key = [0u8; KEY_SIZE];
    let mut hmac_key = [0u8; HMAC_SIZE];
    let mut iv = [0u8; IV_SIZE];

    cipher_key.copy_from_slice(&derived[..KEY_SIZE]);
    hmac_key.copy_from_slice(&derived[KEY_SIZE..KEY_SIZE + HMAC_SIZE]);
    iv.copy_from_slice(&derived[KEY_SIZE + HMAC_SIZE..]);

    (cipher_key, hmac_key, iv)
}

/// Add PKCS7 padding to data.
fn pkcs7_pad(data: &[u8]) -> Vec<u8> {
    let padding_len = AES_BLOCK_SIZE - (data.len() % AES_BLOCK_SIZE);
    let mut padded = data.to_vec();
    padded.extend(std::iter::repeat_n(padding_len as u8, padding_len));
    padded
}

/// Remove PKCS7 padding from data.
fn pkcs7_unpad(data: &[u8]) -> Result<Vec<u8>, EncryptError> {
    if data.is_empty() {
        return Err(EncryptError::InvalidPadding);
    }
    let padding_len = *data.last().unwrap() as usize;
    if padding_len == 0 || padding_len > AES_BLOCK_SIZE || padding_len > data.len() {
        return Err(EncryptError::InvalidPadding);
    }
    // Verify all padding bytes are correct
    for &byte in &data[data.len() - padding_len..] {
        if byte as usize != padding_len {
            return Err(EncryptError::InvalidPadding);
        }
    }
    Ok(data[..data.len() - padding_len].to_vec())
}

/// Encrypt data using Ansible Vault format.
///
/// If `vault_id` is `Some(id)` with a non-empty string, uses v1.2 format with the vault ID.
/// If `vault_id` is `None` or `Some("")`, uses v1.1 format without a vault ID.
///
/// Returns a string in Ansible Vault format ready to be written to a file.
pub fn vault_encrypt(plaintext: &[u8], password: &str, vault_id: Option<&str>) -> Result<String, EncryptError> {
    // Generate random salt
    let mut salt = [0u8; SALT_SIZE];
    rand::rng().fill(&mut salt);

    // Derive keys
    let (cipher_key, hmac_key, iv) = derive_keys(password.as_bytes(), &salt);

    // PKCS7 pad the plaintext
    let padded = pkcs7_pad(plaintext);

    // Encrypt with AES-256-CTR
    let mut ciphertext = padded;
    let mut cipher = Aes256Ctr::new(&cipher_key.into(), &iv.into());
    cipher.apply_keystream(&mut ciphertext);

    // Generate HMAC of ciphertext
    let mut mac = HmacSha256::new_from_slice(&hmac_key).expect("HMAC key size is valid");
    mac.update(&ciphertext);
    let hmac_result = mac.finalize().into_bytes();

    // Build payload: hex(salt) + "\n" + hex(hmac) + "\n" + hex(ciphertext)
    // This is the first level of hex encoding with newline separators
    let hex_salt = hex::encode(salt);
    let hex_hmac = hex::encode(hmac_result);
    let hex_ciphertext = hex::encode(&ciphertext);
    let inner_payload = format!("{}\n{}\n{}", hex_salt, hex_hmac, hex_ciphertext);

    // Hex encode again (second level) - this is what Ansible does
    let hex_hex_payload = hex::encode(inner_payload.as_bytes());

    // Split into 80-character lines
    let lines: Vec<&str> = hex_hex_payload
        .as_bytes()
        .chunks(VAULT_LINE_WIDTH)
        .map(|chunk| std::str::from_utf8(chunk).expect("hex is valid UTF-8"))
        .collect();

    // Build the vault file with appropriate header format
    let header = match vault_id {
        Some(id) if !id.is_empty() => format!("$ANSIBLE_VAULT;1.2;AES256;{}", id),
        _ => "$ANSIBLE_VAULT;1.1;AES256".to_string(),
    };
    Ok(format!("{}\n{}\n", header, lines.join("\n")))
}

/// Decrypt data from Ansible Vault v1.1/v1.2 format.
///
/// Returns the decrypted plaintext and the vault ID from the header (if present).
/// For v1.1 format (no vault ID), returns `None` for the vault ID.
/// For v1.2 format with a vault ID, returns `Some(vault_id)`.
pub fn vault_decrypt(vault_data: &str, password: &str) -> Result<(Vec<u8>, Option<String>), EncryptError> {
    let lines: Vec<&str> = vault_data.lines().collect();
    if lines.is_empty() {
        return Err(EncryptError::InvalidVaultFormat(
            "Empty vault data".to_string(),
        ));
    }

    // Parse header
    let header = lines[0];
    let parts: Vec<&str> = header.split(';').collect();
    if parts.len() < 3 || parts[0] != "$ANSIBLE_VAULT" {
        return Err(EncryptError::InvalidVaultFormat(
            "Invalid header format".to_string(),
        ));
    }

    let version = parts[1];
    if version != "1.1" && version != "1.2" {
        return Err(EncryptError::InvalidVaultFormat(format!(
            "Unsupported vault version: {}",
            version
        )));
    }

    let cipher = parts[2];
    if cipher != "AES256" {
        return Err(EncryptError::InvalidVaultFormat(format!(
            "Unsupported cipher: {}",
            cipher
        )));
    }

    // Vault ID is optional (only in v1.2)
    let vault_id = if parts.len() >= 4 && !parts[3].is_empty() {
        Some(parts[3].to_string())
    } else {
        None
    };

    // Join payload lines and decode hex twice
    let hex_hex_payload: String = lines[1..].concat();
    let hex_payload = hex::decode(&hex_hex_payload)?;
    let hex_payload_str =
        std::str::from_utf8(&hex_payload).map_err(|_| EncryptError::InvalidVaultFormat("Invalid hex payload".to_string()))?;

    // The inner hex string contains newlines separating salt, hmac, ciphertext
    let inner_lines: Vec<&str> = hex_payload_str.lines().collect();
    if inner_lines.len() < 3 {
        return Err(EncryptError::InvalidVaultFormat(
            "Payload must have salt, hmac, and ciphertext".to_string(),
        ));
    }

    let salt = hex::decode(inner_lines[0])?;
    let stored_hmac = hex::decode(inner_lines[1])?;
    let ciphertext = hex::decode(inner_lines[2])?;

    if salt.len() != SALT_SIZE {
        return Err(EncryptError::InvalidVaultFormat(format!(
            "Invalid salt size: expected {}, got {}",
            SALT_SIZE,
            salt.len()
        )));
    }

    // Derive keys from password and salt
    let (cipher_key, hmac_key, iv) = derive_keys(password.as_bytes(), &salt);

    // Verify HMAC
    let mut mac = HmacSha256::new_from_slice(&hmac_key).expect("HMAC key size is valid");
    mac.update(&ciphertext);
    let computed_hmac = mac.finalize().into_bytes();

    if computed_hmac.as_slice() != stored_hmac.as_slice() {
        return Err(EncryptError::HmacMismatch);
    }

    // Decrypt with AES-256-CTR
    let mut plaintext = ciphertext;
    let mut cipher = Aes256Ctr::new(&cipher_key.into(), &iv.into());
    cipher.apply_keystream(&mut plaintext);

    // Remove PKCS7 padding
    let unpadded = pkcs7_unpad(&plaintext)?;

    Ok((unpadded, vault_id))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vault_encrypt_decrypt_roundtrip_with_vault_id() {
        let plaintext = b"Hello, Ansible Vault!";
        let password = "secret_password";
        let vault_id = "my_vault";

        let encrypted = vault_encrypt(plaintext, password, Some(vault_id)).expect("Encryption failed");

        // Verify v1.2 header format with vault ID
        assert!(encrypted.starts_with("$ANSIBLE_VAULT;1.2;AES256;my_vault\n"));

        let (decrypted, returned_vault_id) =
            vault_decrypt(&encrypted, password).expect("Decryption failed");

        assert_eq!(decrypted, plaintext);
        assert_eq!(returned_vault_id, Some(vault_id.to_string()));
    }

    #[test]
    fn vault_encrypt_decrypt_roundtrip_without_vault_id() {
        let plaintext = b"Hello, Ansible Vault!";
        let password = "secret_password";

        let encrypted = vault_encrypt(plaintext, password, None).expect("Encryption failed");

        // Verify v1.1 header format without vault ID
        assert!(encrypted.starts_with("$ANSIBLE_VAULT;1.1;AES256\n"));

        let (decrypted, returned_vault_id) =
            vault_decrypt(&encrypted, password).expect("Decryption failed");

        assert_eq!(decrypted, plaintext);
        assert_eq!(returned_vault_id, None);
    }

    #[test]
    fn vault_decrypt_wrong_password_fails() {
        let plaintext = b"Secret data";
        let password = "correct_password";

        let encrypted = vault_encrypt(plaintext, password, Some("test")).unwrap();

        let result = vault_decrypt(&encrypted, "wrong_password");
        assert!(
            matches!(result, Err(EncryptError::HmacMismatch)),
            "Decryption should fail with wrong password"
        );
    }

    #[test]
    fn vault_format_has_80_char_lines() {
        let plaintext = b"This is a longer message that should produce multiple lines in the vault output.";
        let password = "test";

        let encrypted = vault_encrypt(plaintext, password, Some("test")).unwrap();
        let lines: Vec<&str> = encrypted.lines().collect();

        // First line is header
        assert!(lines[0].starts_with("$ANSIBLE_VAULT"));

        // Payload lines should be max 80 chars (except possibly the last)
        for line in &lines[1..lines.len() - 1] {
            assert_eq!(line.len(), 80, "Line should be 80 chars: {}", line);
        }
    }

    #[test]
    fn pkcs7_padding_works() {
        // Test data that needs padding
        let data = b"hello"; // 5 bytes, needs 11 bytes padding
        let padded = pkcs7_pad(data);
        assert_eq!(padded.len(), 16); // Rounded up to block size
        assert_eq!(&padded[..5], data);
        assert!(padded[5..].iter().all(|&b| b == 11)); // Padding value is 11

        let unpadded = pkcs7_unpad(&padded).unwrap();
        assert_eq!(unpadded, data);
    }

    #[test]
    fn pkcs7_padding_full_block() {
        // Test data that's already a multiple of block size
        let data = [0u8; 16];
        let padded = pkcs7_pad(&data);
        assert_eq!(padded.len(), 32); // Adds a full block of padding
        assert!(padded[16..].iter().all(|&b| b == 16));

        let unpadded = pkcs7_unpad(&padded).unwrap();
        assert_eq!(unpadded, data);
    }

    #[test]
    fn vault_empty_vault_id_uses_v1_1_format() {
        let plaintext = b"data";
        let password = "pass";

        // Empty string should produce v1.1 format
        let encrypted = vault_encrypt(plaintext, password, Some("")).unwrap();
        assert!(encrypted.starts_with("$ANSIBLE_VAULT;1.1;AES256\n"));

        let (decrypted, returned_vault_id) = vault_decrypt(&encrypted, password).unwrap();
        assert_eq!(decrypted, plaintext);
        assert_eq!(returned_vault_id, None);
    }

    #[test]
    fn vault_invalid_header() {
        let result = vault_decrypt("invalid header\npayload", "password");
        assert!(matches!(result, Err(EncryptError::InvalidVaultFormat(_))));
    }

    #[test]
    fn vault_empty_data() {
        let result = vault_decrypt("", "password");
        assert!(matches!(result, Err(EncryptError::InvalidVaultFormat(_))));
    }
}
