#!/usr/bin/env sh
cargo +1.61 test
cargo +nightly test
cargo +nightly test --no-default-features

cargo +nightly fmt --all -- --check
cargo +nightly clippy -- -D warnings
cargo +nightly miri test
cargo +nightly doc2readme --check
