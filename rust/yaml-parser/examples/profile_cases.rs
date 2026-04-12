// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.
#![allow(missing_docs, reason = "local profiling helper")]
#![allow(clippy::print_stderr, reason = "helper script for debugging")]
use std::hint::black_box;
use std::process::ExitCode;

#[cfg(feature = "serde")]
use serde::Deserialize;

const LARGE_SEQUENCE: &str = include_str!("../benches/data/large_sequence.yml");
const LARGE_MAPPING: &str = include_str!("../benches/data/large_mapping.yml");
const BLOCK_SCALARS: &str = include_str!("../benches/data/block_scalars.yml");
const ANCHORS_ALIASES: &str = include_str!("../benches/data/anchors_aliases.yml");
const TAGS: &str = include_str!("../benches/data/tags.yml");
const DOUBLE_QUOTED: &str = r#"key1: "with \"escapes\""
key2: "newline\nhere"
key3: "tab\there"
"#;

#[cfg(feature = "serde")]
#[derive(Debug)]
struct OwnedYamlValue(
    #[allow(
        dead_code,
        reason = "holder type keeps serde target identical to benchmark"
    )]
    yaml_parser::Value<'static>,
);

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for OwnedYamlValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let borrowed: yaml_parser::Value<'de> = Deserialize::deserialize(deserializer)?;
        Ok(Self(borrowed.into_owned()))
    }
}

enum Mode {
    Parse,
    #[cfg(feature = "serde")]
    Serde,
}

fn dataset_input(name: &str) -> Option<&'static str> {
    match name {
        "block_scalars" => Some(BLOCK_SCALARS),
        "double_quoted" => Some(DOUBLE_QUOTED),
        "anchors_aliases" => Some(ANCHORS_ALIASES),
        "large_mapping" => Some(LARGE_MAPPING),
        "large_sequence" => Some(LARGE_SEQUENCE),
        "tags" => Some(TAGS),
        _ => None,
    }
}

fn usage() -> &'static str {
    "Usage: cargo run --profile bench --example profile_cases --features serde -- <parse|serde> <anchors_aliases|block_scalars|double_quoted|large_mapping|large_sequence|tags> <iterations>"
}

fn parse_mode(arg: &str) -> Option<Mode> {
    match arg {
        "parse" => Some(Mode::Parse),
        #[cfg(feature = "serde")]
        "serde" => Some(Mode::Serde),
        _ => None,
    }
}

fn parse_iterations(arg: &str) -> Option<usize> {
    arg.parse().ok().filter(|iterations| *iterations > 0)
}

fn main() -> ExitCode {
    let mut args = std::env::args().skip(1);
    let Some(mode_arg) = args.next() else {
        eprintln!("{}", usage());
        return ExitCode::FAILURE;
    };
    let Some(dataset_arg) = args.next() else {
        eprintln!("{}", usage());
        return ExitCode::FAILURE;
    };
    let Some(iterations_arg) = args.next() else {
        eprintln!("{}", usage());
        return ExitCode::FAILURE;
    };
    if args.next().is_some() {
        eprintln!("{}", usage());
        return ExitCode::FAILURE;
    }

    let Some(mode) = parse_mode(&mode_arg) else {
        eprintln!("{}", usage());
        return ExitCode::FAILURE;
    };
    let Some(input) = dataset_input(&dataset_arg) else {
        eprintln!("{}", usage());
        return ExitCode::FAILURE;
    };
    let Some(iterations) = parse_iterations(&iterations_arg) else {
        eprintln!("{}", usage());
        return ExitCode::FAILURE;
    };

    match mode {
        Mode::Parse => {
            for _ in 0..iterations {
                black_box(yaml_parser::parse(black_box(input)));
            }
        }
        #[cfg(feature = "serde")]
        Mode::Serde => {
            for _ in 0..iterations {
                let value: OwnedYamlValue = yaml_parser::serde::from_str(black_box(input)).unwrap();
                black_box(value);
            }
        }
    }

    ExitCode::SUCCESS
}
