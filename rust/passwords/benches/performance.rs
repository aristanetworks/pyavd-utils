// Copyright (c) 2026 Arista Networks, Inc.
// Use of this source code is governed by the Apache License 2.0
// that can be found in the LICENSE file.

//! Benchmarks for the password encryption and hashing helpers.

use std::hint::black_box;

use criterion::Criterion;
use passwords::cbc_check_password;
use passwords::cbc_decrypt;
use passwords::cbc_encrypt;
use passwords::sha512_crypt;
use passwords::simple_7_decrypt;
use passwords::simple_7_encrypt;

const PASSWORD: &str = "LittleDropBobbyTable";
const CBC_KEY: &[u8] = b"benchmark-fabric_passwd";

fn benchmark_simple_7(criterion: &mut Criterion) {
    let Ok(ciphertext) = simple_7_encrypt(PASSWORD, Some(7)) else {
        return;
    };

    criterion.bench_function("passwords/simple_7_encrypt", |bencher| {
        bencher.iter(|| simple_7_encrypt(black_box(PASSWORD), black_box(Some(7))));
    });
    criterion.bench_function("passwords/simple_7_decrypt", |bencher| {
        bencher.iter(|| simple_7_decrypt(black_box(&ciphertext)));
    });
}

fn benchmark_cbc(criterion: &mut Criterion) {
    let Ok(ciphertext) = cbc_encrypt(CBC_KEY, PASSWORD.as_bytes()) else {
        return;
    };

    criterion.bench_function("passwords/cbc_encrypt", |bencher| {
        bencher.iter(|| cbc_encrypt(black_box(CBC_KEY), black_box(PASSWORD.as_bytes())));
    });
    criterion.bench_function("passwords/cbc_decrypt", |bencher| {
        bencher.iter(|| cbc_decrypt(black_box(CBC_KEY), black_box(&ciphertext)));
    });
    criterion.bench_function("passwords/cbc_verify", |bencher| {
        bencher.iter(|| cbc_check_password(black_box(CBC_KEY), black_box(&ciphertext)));
    });
}

fn benchmark_sha512_crypt(criterion: &mut Criterion) {
    criterion.bench_function("passwords/sha512_crypt", |bencher| {
        bencher.iter(|| sha512_crypt(black_box(PASSWORD), black_box("1234567890ABCDEF")));
    });
}

fn run_benchmarks(criterion: &mut Criterion) {
    benchmark_simple_7(criterion);
    benchmark_cbc(criterion);
    benchmark_sha512_crypt(criterion);
}

#[cfg(codspeed)]
fn main() {
    let mut criterion = Criterion::new_instrumented();
    run_benchmarks(&mut criterion);
}

#[cfg(not(codspeed))]
fn main() {
    let mut criterion = Criterion::default().configure_from_args();
    run_benchmarks(&mut criterion);
}
