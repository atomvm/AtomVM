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
- Added support for `controlling_process/2` in `gen_udp` and `gen_tcp` modules.
- Added ability to get the atomvm version via `erlang:system_info`.

### Changed

- Changed the configuration model of the SPI driver, in order to allow for multiple "follower"
  devices to be attached to the same SPI Bus. IMPORTANT: These changes are source-incompatible with
  previous releases of AtomVM.

## [0.5.1] - Unreleased
### Fixed
- Fix `gen_statem`: Cancel outstanding timers during state transitions in
  order to prevent spurious timeout messages from being sent to `gen_statem`
  process.
- Fix missing Elixir libraries: examvlib was not packed into atomvmlib.avm
- Fix `bs_context_to_binary`: match offset wasn't used, leading in certain situations to infinite loops
  while matching binaries.
- Fix how `start` option was handled from `bs_restore2` instruction: last saved match offset was
  used instead of match starting offset, causing some bytes being skipped.
- Fix another potential bug when doing pattern matching using code compiled with OTP 21.
- [ESP32] [UART]: Allow using different pins for rx, tx, cts and rts.
- [ESP32] [UART]: Replace custom UART handling with esp-idf UART event queues, hence other UARTs
  than UART0 are supported, with better performances and stability.

## [0.5.0] - 2022-03-22
