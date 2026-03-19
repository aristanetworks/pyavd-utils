// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

// Check the size of Event enum

#![allow(clippy::print_stdout, reason = "examples are meant to print to stdout")]

use std::borrow::Cow;
use std::mem::size_of;
use yaml_parser::Event;

fn main() {
    println!("Size of Event: {} bytes", size_of::<Event>());
    println!("Size of Cow<str>: {} bytes", size_of::<Cow<str>>());
    println!(
        "Size of Option<Event>: {} bytes",
        size_of::<Option<Event>>()
    );
}
