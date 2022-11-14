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
- Added `erlang:is_boolean/1` Bif.
- Added support for `esp:partition_erase_range/2`
- Added support for `i2c:close/1`
- Added support for `erlang:unregister/1`
- Added Elixir ESP32 LEDC driver and example
- Added support for `uart:close/1`
- Added Bitwise support for Elixir
- Added support for esp32-s2, esp32-s3, and esp32-c3 chips.
- Added the ability to specify the I2C port

### Breaking Changes

> IMPORTANT: These changes are incompatible with previous releases of AtomVM.

- Changed the configuration model of the SPI driver, in order to allow for multiple "follower"
  devices to be attached to the same SPI Bus.
- Changed the return value from `erlang:system_info(esp32_chip_info)` from a tuple to a map, with
additional information.

## [0.5.1] - Unreleased
### Added
- New function for atom comparison, useful when writing 3rd party components.
- New function for translating an atom term to an int value, according to a given translation table.
  This function can be used for translating an atom term to an enum const before doing a switch.
- New no-op `ATOM_STR(...)` macro for avoiding issues with clang-format.
- [ESP32] `REGISTER_PORT_DRIVER` for registering additional port drivers without editing any
  source file. This allows adding new components by just copying them to the components directory.
- [ESP32] `REGISTER_NIF_COLLECTION` for registering additional NIFs sets without editing any
  source file. This allows adding new NIFs by just copying them to the components directory.
- New function for getting a map or proplist value using an atom string without poluting the atom
  table.

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
- Fix binaries concat (`bs_append` instruction) that was adding some extra zeroes at the end of
  built binaries.
- Fixed a bug in gen_tcp that prevents an accepting socket from inheriting settings on the listening socket.
- Fixed a bug in packing and unpacking integers into and from binaries when the
  bit length is not a multiple of 8.
- Fixed `esp:deep_sleep/1` that did not accept values above 31 minutes.
- Fixed a bug that could cause processes to hang indefinitely when calling ports that have terminated.
- Fixed potential VM crash when parsing external terms.
- Fixed the enforcement of `min_free_space` process option.

## [0.5.0] - 2022-03-22
