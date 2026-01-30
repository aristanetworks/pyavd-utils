// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! AES-256-GCM encryption and decryption utilities.
//!
//! This module provides functions for encrypting and decrypting data using
//! AES-256-GCM (Galois/Counter Mode). The encrypted output format is:
//!
//! ```text
//! ┌─────────────────┬──────────────────────────────────┐
//! │  Nonce (12B)    │  Ciphertext (includes 16B tag)   │
//! └─────────────────┴──────────────────────────────────┘
//! ```

use aes_gcm::aead::{Aead as _, AeadCore as _, KeyInit as _};

/// AES-GCM nonce size in bytes (96 bits)
pub const NONCE_SIZE: usize = 12;

/// AES-GCM authentication tag size in bytes (128 bits)
pub const TAG_SIZE: usize = 16;

/// Minimum size of encrypted data (nonce + tag + at least 1 byte of ciphertext)
pub const MIN_ENCRYPTED_SIZE: usize = NONCE_SIZE + TAG_SIZE + 1;

/// Encryption/decryption error type
#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum EncryptError {
    #[display("Encryption failed: {_0}")]
    AesGcmError(aes_gcm::Error),
    #[display("Encrypted data too short")]
    DataTooShort {},
    #[display("Invalid key length: expected 32 bytes, got {_0}")]
    InvalidKeyLength(usize),
}

/// Returns a random 256-bit key for AES-256-GCM encryption.
#[must_use]
pub fn generate_key() -> [u8; 32] {
    aes_gcm::Aes256Gcm::generate_key(aes_gcm::aead::OsRng).into()
}

/// Encrypt data using AES-256-GCM.
///
/// Returns the encrypted data in the format: nonce (12 bytes) || ciphertext || tag (16 bytes)
pub fn encrypt(plaintext: &[u8], key: &[u8; 32]) -> Result<Vec<u8>, EncryptError> {
    let cipher = aes_gcm::Aes256Gcm::new(key.into());
    let nonce = aes_gcm::Aes256Gcm::generate_nonce(aes_gcm::aead::OsRng);
    let ciphertext = cipher.encrypt(&nonce, plaintext)?;

    // Output format: nonce (12 bytes) || ciphertext (includes 16-byte auth tag)
    let mut output = Vec::with_capacity(nonce.len() + ciphertext.len());
    output.extend_from_slice(&nonce);
    output.extend(ciphertext);
    Ok(output)
}

/// Decrypt data using AES-256-GCM.
///
/// Expects data in the format: nonce (12 bytes) || ciphertext || tag (16 bytes)
pub fn decrypt(data: &[u8], key: &[u8; 32]) -> Result<Vec<u8>, EncryptError> {
    if data.len() < MIN_ENCRYPTED_SIZE {
        return Err(EncryptError::DataTooShort {});
    }

    let (nonce_bytes, ciphertext) = data.split_at(NONCE_SIZE);
    let nonce = aes_gcm::Nonce::from_slice(nonce_bytes);
    let cipher = aes_gcm::Aes256Gcm::new(key.into());

    Ok(cipher.decrypt(nonce, ciphertext)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encrypt_decrypt_roundtrip() {
        let key: [u8; 32] = [
            0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
            0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c,
            0x1d, 0x1e, 0x1f, 0x20,
        ];
        let plaintext = b"Hello, World! This is a test message.";

        let encrypted = encrypt(plaintext, &key).expect("Encryption failed");

        // Verify encrypted data has expected minimum size
        assert!(
            encrypted.len() >= MIN_ENCRYPTED_SIZE,
            "Encrypted data too short"
        );

        let decrypted = decrypt(&encrypted, &key).expect("Decryption failed");
        assert_eq!(decrypted, plaintext);
    }

    #[test]
    fn decrypt_with_wrong_key_fails() {
        let key: [u8; 32] = [0x01; 32];
        let wrong_key: [u8; 32] = [0x02; 32];
        let plaintext = b"Secret message";

        let encrypted = encrypt(plaintext, &key).unwrap();

        let result = decrypt(&encrypted, &wrong_key);
        assert!(result.is_err(), "Decryption should fail with wrong key");
    }

    #[test]
    fn decrypt_truncated_data_fails() {
        let key: [u8; 32] = [0x01; 32];

        // Data too short (less than nonce + tag + 1 byte)
        let short_data = vec![0u8; 20];
        let result = decrypt(&short_data, &key);
        assert!(
            matches!(result, Err(EncryptError::DataTooShort {})),
            "Should return DataTooShort error"
        );
    }

    #[test]
    fn decrypt_corrupted_data_fails() {
        let key: [u8; 32] = [0x01; 32];
        let plaintext = b"Secret message";

        let mut encrypted = encrypt(plaintext, &key).unwrap();

        // Corrupt the ciphertext (not the nonce)
        if encrypted.len() > 15 {
            encrypted[15] ^= 0xff;
        }

        let result = decrypt(&encrypted, &key);
        assert!(
            result.is_err(),
            "Decryption should fail with corrupted data"
        );
    }

    #[test]
    fn generate_key_produces_unique_keys() {
        let key1 = generate_key();
        let key2 = generate_key();
        assert_ne!(key1, key2, "Generated keys should be unique");
    }

    #[test]
    fn encrypt_produces_unique_ciphertexts() {
        let key: [u8; 32] = [0x01; 32];
        let plaintext = b"Same message";

        let encrypted1 = encrypt(plaintext, &key).unwrap();
        let encrypted2 = encrypt(plaintext, &key).unwrap();

        // Due to random nonce, ciphertexts should differ
        assert_ne!(
            encrypted1, encrypted2,
            "Encrypting same plaintext should produce different ciphertexts"
        );

        // But both should decrypt to the same plaintext
        let decrypted1 = decrypt(&encrypted1, &key).unwrap();
        let decrypted2 = decrypt(&encrypted2, &key).unwrap();
        assert_eq!(decrypted1, plaintext);
        assert_eq!(decrypted2, plaintext);
    }
}
