# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Added the ability to specify the HSPI or VSPI ESP32 hardware interfaces when initializing the
  SPI Bus.
- Added support for the `spi:close/1` function.
- Added AVM_VERBOSE_ABORT CMake define, which when set to on, will print the C module and line
  number when a VM abort occurs.  This define is off by default.
- Added `spi:write/3` and `spi:write_read/3` functions to support generalized SPI transactions
  and arbitrary-length reads and writes from SPI devices.
- Added support for building ESP32 port with all currently supported versions of Espressif ESP-IDF,
  version 4.1.x through 4.4.x.

### Changed

- Changed the configuration model of the SPI driver, in order to allow for multiple "follower"
  devices to be attached to the same SPI Bus. IMPORTANT: These changes are source-incompatible with
  previous releases of AtomVM.

## [0.5.0] - 2022-03-22
