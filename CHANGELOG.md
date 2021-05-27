# Changelog

## Unreleased

No changes.

## [0.2.0 - 2021-05-27](https://github.com/jonas-schievink/jaylink/releases/tag/v0.2.0)

### New Features

- `HardwareType` now implements `Display`.
- Add untested and experimental support for additional target interfaces (BDM3,
  FINE, PIC32 ICSP, SPI, C2, cJTAG, and Microchip 2-wire JTAG).
- Redesigned the target interface API (breaking change):
  - `JayLink::available_interfaces` now returns the set of interfaces instead of an opaque iterator.
- Improved the capabilities API (breaking change):
  - A new `Capability` enum represents every capability the library knows about.
  - An opaque `Capabilities` struct represents a set of capabilities advertised by a probe.
- `swo_start_uart` is now called `swo_start` and handles future support for
  other encodings (breaking change).
- Add a `jtag_scan` example that enumerates a scan chain.

### Other Improvements

- Improved documentation.
- Improved the `list` example to list all interface speeds.
- Improved the error messages in the `swdump` example.
- Improved speed defaults in `swdump` and `swodump` examples.
- Eagerly fetch supported interfaces and capabilities.
- Update `rusb` to 0.8.
- Stop automatically selecting SWD when `swo_start` is called, to behave consistently.
- Redesign the speed info and configuration API to be easier to use:
  - `Speeds` is now `SpeedInfo`
  - `SwoSpeeds` is now `SwoSpeedInfo`
  - `CommunicationSpeed` is now `SpeedConfig`
  - `max_speed` getters were renamed `max_speed_hz`
  - A maximum speed `SpeedConfig` can be created via `SpeedInfo::max_speed_config`

### Bug Fixes

- Fix JTAG bitcounting logic.
- Fix `BitIter::split_off` logic.

## [0.1.5 - 2020-08-27](https://github.com/jonas-schievink/jaylink/releases/tag/v0.1.5)

- Improve error message on Windows, hinting at installing WinUSB.

## [0.1.4 - 2020-07-26](https://github.com/jonas-schievink/jaylink/releases/tag/v0.1.4)

### New Features

- Implement SWO capture support.
- Add functions to calculate the maximum supported transport speed.

## [0.1.3 - 2020-07-09](https://github.com/jonas-schievink/jaylink/releases/tag/v0.1.3)

Update rusb dependency to version 0.6.2.

## [0.1.2 - 2020-06-27](https://github.com/jonas-schievink/jaylink/releases/tag/v0.1.2)

Trim returned firmware version like the official tools.

## [0.1.1 - 2020-03-01](https://github.com/jonas-schievink/jaylink/releases/tag/v0.1.1)

Fix JTAG I/O command for old J-Links.

## [0.1.0 - 2019-12-08](https://github.com/jonas-schievink/jaylink/releases/tag/v0.1.0)

Initial release.
