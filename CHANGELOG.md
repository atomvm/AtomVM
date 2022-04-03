# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Added the ability to specify the HSPI or VSPI ESP32 hardware interfaces when initializing the SPI
  Bus.
- Added support for the `spi:close/1` function.

### Changed

- Changed the configuration model of the SPI driver, in order to allow for multiple "follower"
  devices to be attached to the same SPI Bus. IMPORTANT: These changes are source-incompatible with
  previous releases of AtomVM.

## [0.5.0] - 2022-03-22
