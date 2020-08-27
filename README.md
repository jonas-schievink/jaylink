# A crate for controlling J-Link debug probes

[![crates.io](https://img.shields.io/crates/v/jaylink.svg)](https://crates.io/crates/jaylink)
[![docs.rs](https://docs.rs/jaylink/badge.svg)](https://docs.rs/jaylink/)
[![Build Status](https://travis-ci.org/jonas-schievink/jaylink.svg?branch=master)](https://travis-ci.org/jonas-schievink/jaylink)

This crate allows talking to J-Link debug probes attached via USB. The probe's
pins can be controlled and I/O operations using JTAG or SWD can be performed,
enabling control of target MCUs.

Please refer to the [changelog](CHANGELOG.md) to see what changed in the last
releases.

## Usage

Add an entry to your `Cargo.toml`:

```toml
[dependencies]
jaylink = "0.1.5"
```

Check the [API Documentation](https://docs.rs/jaylink/) for how to use the
crate's functionality.

## Rust version support

This crate supports the 3 latest stable Rust releases. Bumping the minimum
supported Rust version (MSRV) is not considered a breaking change as long as
these 3 versions are still supported.

The MSRV is also explicitly tested against in [.travis.yml](.travis.yml).
