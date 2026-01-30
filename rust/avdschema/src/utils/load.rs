// Copyright (c) 2025-2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

use serde::de::DeserializeOwned;
use std::io::BufReader;

#[cfg(feature = "dump_load_files")]
use std::{ffi::OsStr, fs::File, io, path::Path};

#[cfg(feature = "dump_load_files")]
use walkdir::WalkDir;

#[cfg(feature = "dump_load_files")]
use crate::Inherit;

pub trait Load
where
    Self: DeserializeOwned,
{
    fn from_json(json: &str) -> Result<Self, LoadError> {
        Ok(serde_json::from_str(json)?)
    }

    /// Decrypt and deserialize JSON data encrypted with AES-256-GCM.
    /// Expected format: nonce (12 bytes) || ciphertext (includes 16-byte auth tag)
    #[cfg(feature = "encryption")]
    fn from_encrypted_json(data: &[u8], key: &[u8; 32]) -> Result<Self, LoadError> {
        let plaintext = encrypt::decrypt(data, key)?;
        let json = std::str::from_utf8(&plaintext)?;
        Self::from_json(json)
    }

    #[cfg(feature = "dump_load_files")]
    fn from_file(input: Option<&Path>) -> Result<Self, LoadError> {
        // Read input from file / stdin
        match input {
            Some(path) => match path.extension().and_then(OsStr::to_str) {
                Some("yml" | "yaml") => Self::from_yaml_file(path),
                Some("json") => Self::from_json_file(path),
                #[cfg(feature = "xz2")]
                Some("xz2") => Self::from_xz2_file(path),
                Some("gz") => Self::from_gz_file(path),
                _ => Err(LoadError::InvalidExtension {}),
            },
            None => Self::from_stdin(),
        }
    }
    #[cfg(feature = "dump_load_files")]
    fn from_stdin() -> Result<Self, LoadError> {
        let reader = io::stdin();
        Ok(serde_yaml::from_reader(reader)?)
    }
    #[cfg(feature = "dump_load_files")]
    fn from_yaml_file(path: &Path) -> Result<Self, LoadError> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        Ok(serde_yaml::from_reader(reader)?)
    }
    #[cfg(feature = "xz2")]
    fn from_xz2_file(path: &Path) -> Result<Self, LoadError> {
        let file = File::open(path)?;
        let decompressor = xz2::read::XzDecoder::new(file);
        let reader = BufReader::new(decompressor);
        Ok(serde_json::from_reader(reader)?)
    }
    #[cfg(feature = "dump_load_files")]
    fn from_json_file(path: &Path) -> Result<Self, LoadError> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        Ok(serde_json::from_reader(reader)?)
    }
    #[cfg(feature = "xz2")]
    fn from_xz2_bytes(bytes: &[u8]) -> Result<Self, LoadError> {
        let decompressor = xz2::read::XzDecoder::new(bytes);
        let reader = BufReader::new(decompressor);
        Ok(serde_json::from_reader(reader)?)
    }
    #[cfg(feature = "dump_load_files")]
    fn from_gz_file(path: &Path) -> Result<Self, LoadError> {
        let file = File::open(path)?;
        let decompressor = flate2::read::GzDecoder::new(file);
        let reader = BufReader::new(decompressor);
        Ok(serde_json::from_reader(reader)?)
    }
    fn from_gz_bytes(bytes: &[u8]) -> Result<Self, LoadError> {
        let decompressor = flate2::read::GzDecoder::new(bytes);
        let reader = BufReader::new(decompressor);
        Ok(serde_json::from_reader(reader)?)
    }
}

#[cfg(feature = "dump_load_files")]
pub trait LoadFromFragments
where
    Self: Load + Inherit + DeserializeOwned,
{
    fn from_fragments(glob: &Path) -> Result<Self, LoadError> {
        let mut glob_iter = WalkDir::new(glob)
            .max_depth(1)
            .sort_by_file_name()
            .into_iter()
            .filter_map(Result::ok)
            .filter_map(|entry| {
                std::path::Path::new(entry.file_name().to_str()?)
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("yml"))
                    .then_some(entry)
            });
        let first_file = glob_iter.next().ok_or(LoadError::NoFilesFound {})?;
        let mut combined_data = Self::from_file(Some(first_file.path()))?;
        for file in glob_iter {
            let file_data = Self::from_file(Some(file.path()))?;
            combined_data.inherit(&file_data);
        }
        Ok(combined_data)
    }
}

#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum LoadError {
    JsonError(serde_json::Error),
    YamlError(serde_yaml::Error),
    #[cfg(feature = "dump_load_files")]
    IoError(std::io::Error),
    #[cfg(feature = "dump_load_files")]
    #[display("Invalid extension for input file.")]
    InvalidExtension {},
    #[cfg(feature = "dump_load_files")]
    #[display("No files found.")]
    NoFilesFound {},
    #[cfg(feature = "encryption")]
    #[display("Decryption error: {_0}")]
    DecryptionError(encrypt::EncryptError),
    #[cfg(feature = "encryption")]
    #[display("Invalid UTF-8 in decrypted data: {_0}")]
    Utf8Error(std::str::Utf8Error),
}

#[cfg(test)]
mod tests {
    use super::Load;
    use crate::Store;
    use crate::any::AnySchema;
    use crate::utils::test_utils::{get_test_dict_schema, get_test_store, get_tmp_file};

    #[test]
    fn load_yaml() {
        crate::utils::dump::tests::dump_yaml();
        let file_path = get_tmp_file("test_dump.yml");
        let schema = get_test_dict_schema();
        let result = AnySchema::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), schema);
    }
    #[test]
    fn load_json() {
        crate::utils::dump::tests::dump_json();
        let file_path = get_tmp_file("test_dump.json");
        let schema = get_test_dict_schema();
        let result = AnySchema::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), schema);
    }
    #[cfg(feature = "xz2")]
    #[test]
    fn load_xz2() {
        crate::utils::dump::tests::dump_xz2();
        let file_path = get_tmp_file("test_dump.xz2");
        let schema = get_test_dict_schema();
        let result = AnySchema::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), schema);
    }
    #[test]
    fn load_gz() {
        crate::utils::dump::tests::dump_gz();
        let file_path = get_tmp_file("test_dump.gz");
        let schema = get_test_dict_schema();
        let result = AnySchema::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), schema);
    }
    #[test]
    fn load_store_yaml() {
        crate::utils::dump::tests::dump_store_yaml();
        let file_path = get_tmp_file("test_dump_store.yml");
        let store = get_test_store();
        let result = Store::from_file(Some(&file_path));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), store);
    }

    #[cfg(feature = "encryption")]
    mod encryption_tests {
        use super::*;
        use crate::utils::dump::Dump;

        #[test]
        fn encrypt_decrypt_roundtrip_schema() {
            let key: [u8; 32] = [
                0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
                0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c,
                0x1d, 0x1e, 0x1f, 0x20,
            ];
            let schema = get_test_dict_schema();

            // Encrypt
            let encrypted = schema.to_encrypted_json(&key);
            assert!(encrypted.is_ok(), "Encryption failed: {:?}", encrypted.err());
            let encrypted_data = encrypted.unwrap();

            // Verify encrypted data has expected minimum size (12 nonce + 16 tag + some data)
            assert!(encrypted_data.len() >= 28, "Encrypted data too short");

            // Decrypt
            let decrypted: Result<AnySchema, _> = AnySchema::from_encrypted_json(&encrypted_data, &key);
            assert!(decrypted.is_ok(), "Decryption failed: {:?}", decrypted.err());
            assert_eq!(decrypted.unwrap(), schema);
        }

        #[test]
        fn encrypt_decrypt_roundtrip_store() {
            let key: [u8; 32] = [
                0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77,
                0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00, 0x11, 0x22, 0x33, 0x44, 0x55,
                0x66, 0x77, 0x88, 0x99,
            ];
            let store = get_test_store();

            // Encrypt
            let encrypted = store.to_encrypted_json(&key);
            assert!(encrypted.is_ok(), "Encryption failed: {:?}", encrypted.err());
            let encrypted_data = encrypted.unwrap();

            // Decrypt
            let decrypted: Result<Store, _> = Store::from_encrypted_json(&encrypted_data, &key);
            assert!(decrypted.is_ok(), "Decryption failed: {:?}", decrypted.err());
            assert_eq!(decrypted.unwrap(), store);
        }

        #[test]
        fn decrypt_with_wrong_key_fails() {
            let key: [u8; 32] = [0x01; 32];
            let wrong_key: [u8; 32] = [0x02; 32];
            let schema = get_test_dict_schema();

            // Encrypt with correct key
            let encrypted = schema.to_encrypted_json(&key).unwrap();

            // Decrypt with wrong key should fail
            let decrypted: Result<AnySchema, _> = AnySchema::from_encrypted_json(&encrypted, &wrong_key);
            assert!(decrypted.is_err(), "Decryption should fail with wrong key");
        }

        #[test]
        fn decrypt_truncated_data_fails() {
            let key: [u8; 32] = [0x01; 32];

            // Data too short (less than nonce + tag + 1 byte)
            let short_data = vec![0u8; 20];
            let result: Result<AnySchema, _> = AnySchema::from_encrypted_json(&short_data, &key);
            assert!(result.is_err(), "Decryption should fail with truncated data");
        }

        #[test]
        fn decrypt_corrupted_data_fails() {
            let key: [u8; 32] = [0x01; 32];
            let schema = get_test_dict_schema();

            // Encrypt
            let mut encrypted = schema.to_encrypted_json(&key).unwrap();

            // Corrupt the ciphertext (not the nonce)
            if encrypted.len() > 15 {
                encrypted[15] ^= 0xff;
            }

            // Decrypt should fail due to authentication failure
            let decrypted: Result<AnySchema, _> = AnySchema::from_encrypted_json(&encrypted, &key);
            assert!(decrypted.is_err(), "Decryption should fail with corrupted data");
        }
    }
}
