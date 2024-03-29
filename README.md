# A crate for controlling J-Link debug probes

[![crates.io](https://img.shields.io/crates/v/jaylink.svg)](https://crates.io/crates/jaylink)
[![docs.rs](https://docs.rs/jaylink/badge.svg)](https://docs.rs/jaylink/)
![CI](https://github.com/jonas-schievink/jaylink/workflows/CI/badge.svg)

This crate allows talking to J-Link debug probes attached via USB. The probe's
pins can be controlled and I/O operations using JTAG or SWD can be performed,
enabling control of target MCUs.

Please refer to the [changelog](CHANGELOG.md) to see what changed in the last
releases.

## Usage

Add an entry to your `Cargo.toml`:

```toml
[dependencies]
jaylink = "0.3.0"
```

Check the [API Documentation](https://docs.rs/jaylink/) for how to use the
crate's functionality.

## Rust version support

This crate targets the latest stable Rust release.
