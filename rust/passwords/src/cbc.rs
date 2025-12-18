// Copyright (c) 2025 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use base64::{Engine as _, engine::general_purpose::STANDARD as B64};
use cbc::cipher::block_padding::NoPadding;
use cbc::cipher::{BlockDecryptMut, BlockEncryptMut, KeyIvInit};
use des::TdesEde3;

// Values used by Arista.
const SEED: [u8; 8] = [0xd5, 0xa8, 0xc9, 0x1e, 0xf5, 0xd5, 0x8a, 0x23];
const ENC_SIG: &[u8; 3] = b"\x4c\x88\xbb";
const PARITY_BITS: [u8; 128] = [
    0x01, 0x01, 0x02, 0x02, 0x04, 0x04, 0x07, 0x07, 0x08, 0x08, 0x0B, 0x0B, 0x0D, 0x0D, 0x0E, 0x0E,
    0x10, 0x10, 0x13, 0x13, 0x15, 0x15, 0x16, 0x16, 0x19, 0x19, 0x1A, 0x1A, 0x1C, 0x1C, 0x1F, 0x1F,
    0x20, 0x20, 0x23, 0x23, 0x25, 0x25, 0x26, 0x26, 0x29, 0x29, 0x2A, 0x2A, 0x2C, 0x2C, 0x2F, 0x2F,
    0x31, 0x31, 0x32, 0x32, 0x34, 0x34, 0x37, 0x37, 0x38, 0x38, 0x3B, 0x3B, 0x3D, 0x3D, 0x3E, 0x3E,
    0x40, 0x40, 0x43, 0x43, 0x45, 0x45, 0x46, 0x46, 0x49, 0x49, 0x4A, 0x4A, 0x4C, 0x4C, 0x4F, 0x4F,
    0x51, 0x51, 0x52, 0x52, 0x54, 0x54, 0x57, 0x57, 0x58, 0x58, 0x5B, 0x5B, 0x5D, 0x5D, 0x5E, 0x5E,
    0x61, 0x61, 0x62, 0x62, 0x64, 0x64, 0x67, 0x67, 0x68, 0x68, 0x6B, 0x6B, 0x6D, 0x6D, 0x6E, 0x6E,
    0x70, 0x70, 0x73, 0x73, 0x75, 0x75, 0x76, 0x76, 0x79, 0x79, 0x7A, 0x7A, 0x7C, 0x7C, 0x7F, 0x7F,
];

/// Convert the key to the proper format to give to CBC Encryptor and Decryptor.
fn derive_key(pw: &[u8]) -> [u8; 24] {
    let mut result = SEED;
    for (idx, &b) in pw.iter().enumerate() {
        result[idx & 7] ^= b;
    }

    let mut k8 = [0u8; 8];
    for i in 0..8 {
        k8[i] = PARITY_BITS[(result[i] & 0x7F) as usize];
    }

    let mut key_24 = [0u8; 24];
    key_24[0..8].copy_from_slice(&k8);
    key_24[8..16].copy_from_slice(&k8);
    key_24[16..24].copy_from_slice(&k8);
    key_24
}

pub fn cbc_encrypt(key: &[u8], data: &[u8]) -> Vec<u8> {
    let hashed_key = derive_key(key);
    let iv = [0u8; 8];
    let padding_len = (8 - ((data.len() + 4) % 8)) % 8;

    // ciphertext = ENC_SIG + bytes([padding * 16 + 0xE]) + data + bytes(padding)
    let buf_len = ENC_SIG.len() + 1 + data.len() + padding_len;
    let mut buf = Vec::with_capacity(buf_len);
    buf.extend_from_slice(ENC_SIG);
    buf.push((padding_len * 16 + 0xE) as u8);
    buf.extend_from_slice(data);
    buf.extend(std::iter::repeat_n(0, padding_len));

    let cipher = cbc::Encryptor::<TdesEde3>::new(&hashed_key.into(), &iv.into());

    let ct = cipher
        .encrypt_padded_mut::<NoPadding>(&mut buf, buf_len)
        .expect("Encryption failed: buffer was not block-aligned");

    B64.encode(ct).into_bytes()
}

pub fn cbc_decrypt(key: &[u8], b64_encrypted_data: &[u8]) -> Result<Vec<u8>, String> {
    let hashed_key = derive_key(key);
    let iv = [0u8; 8];
    let mut ciphertext = B64
        .decode(b64_encrypted_data)
        .map_err(|e| format!("Base64 error: {:?}", e))?;

    let cipher = cbc::Decryptor::<TdesEde3>::new(&hashed_key.into(), &iv.into());

    let pt = cipher
        .decrypt_padded_mut::<NoPadding>(&mut ciphertext)
        .map_err(|e| format!("DES decryption failed: {:?}", e))?;

    // Validate ENC SIGN
    if pt.len() < 4 || &pt[0..3] != ENC_SIG {
        return Err("Invalid Arista encryption signature inside ciphertext".to_string());
    }

    // Parse the Metadata byte (4th byte)
    let meta_byte = pt[3];
    if meta_byte < 0xE {
        return Err("Invalid metadata byte in ciphertext".to_string());
    }
    let padding_len = ((meta_byte as usize) - 0xE) / 16;

    // Layout: [SIG (3)] [META (1)] [DATA (len - 4 - padding)] [NULLS (padding)]
    let end_idx = pt.len() - padding_len;
    if end_idx < 4 {
        return Err("Decrypted block length is shorter than expected metadata".to_string());
    }

    Ok(pt[4..end_idx].to_vec())
}

pub fn cbc_check_password(key: &[u8], ciphertext: &[u8]) -> bool {
    cbc_decrypt(key, ciphertext).is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_PASSWORD: &[u8] = b"arista";

    // (key, ciphertext) for TEST_PASSWORD
    const VALID_PAIRS: [(&[u8], &[u8]); 2] = [
        (b"42.42.42.42_passwd", b"3QGcqpU2YTwKh2jVQ4Vj/A=="),
        (b"AVD-TEST_passwd", b"bM7t58t04qSqLHAfZR/Szg=="),
    ];

    const INVALID_PAIRS: [(&[u8], &[u8]); 2] = [
        (b"10.42.42.43_passwd", b"3QGcqpU2YTwKh2jVQ4Vj/A=="),
        (b"AVD-TEST-DUMMY_passwd", b"bM7t58t04qSqLHAfZR/Szg=="),
    ];

    #[test]
    fn test_cbc_encrypt_ok() {
        for (key, expected) in VALID_PAIRS {
            let result = cbc_encrypt(key, TEST_PASSWORD);
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_cbc_decrypt_ok() {
        for (key, ciphertext) in VALID_PAIRS {
            let decrypted = cbc_decrypt(key, ciphertext).expect("Decryption failed");
            assert_eq!(decrypted, TEST_PASSWORD);
        }
    }

    #[test]
    fn test_cbc_decrypt_failure() {
        let key: &[u8] = b"TOTO_passwd";
        let ciphertext: &[u8] = b"3QGcqpU2YTwKh2jVQ4Vj/A==";

        let result = cbc_decrypt(key, ciphertext);

        assert!(result.is_err());
    }

    #[test]
    fn test_cbc_check_password_ok() {
        for (key, ciphertext) in VALID_PAIRS {
            assert!(cbc_check_password(key, ciphertext));
        }
    }

    #[test]
    fn test_cbc_check_password_failure() {
        for (key, ciphertext) in INVALID_PAIRS {
            assert!(!cbc_check_password(key, ciphertext))
        }
    }
}
