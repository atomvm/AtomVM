# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Added the ability to specify the HSPI or VSPI ESP32 hardware interfaces when initializing the
  SPI Bus.
- Added support for the `spi:close/1` function.
- Added `AVM_VERBOSE_ABORT` CMake define, which when set to on, will print the C module and line
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
- Added Elixir I2C driver and example
- Added the ability to specify the I2C port
- Added support for the OTP `math` module
- Added support for `erlang:integer_to_list/2` and `erlang:integer_to_binary/2`
- Added functions `esp:sleep_enable_ext0_wakeup/2` and `esp:sleep_enable_ext1_wakeup/2.`
- Added support for FP opcodes 94-102 thus removing the need for `AVM_DISABLE_FP=On` with OTP-22+
- Added support for stacktraces
- Added support for `utf-8`, `utf-16`, and `utf-32` bit syntax modifiers (put and match)
- Added support for Erlang `gpio:close/1` and Elixir `GPIO.close/1` for ESP32
- Added support for the Erlang `gen_event` module
- Added `start_link` support for the `network` module
- Added support for `erlang:monotomic_time/1`
- Added `start_link` support for the `gen_statem` module
- Added support for serializing floats in erlang external term encoding
- Added support for the `SMALL_BIG_EXT` erlang external term encoding
- Added support for `erlang:memory(binary)`
- Added support for callbacks on SNTP updates
- Multithreading support (SMP)
- Added support for code:load_abs/1, code:load_binary/3
- Added support for loading / closing AVMPacks at runtime
- Added support for ESP-IDF v5.x
- Added support for `calendar:system_time_to_universal_time/2`
- Added support for `calendar:datetime_to_gregorian_seconds/1`
- Added support for Raspberry Pi Pico
- Added support for nodejs with Wasm
- Added support for a subset of the OTP logger interface
- Added `esp:partition_list/0` function

### Fixed
- Fixed issue with formatting integers with io:format() on STM32 platform
- Fixed a bug in the order of child initialization in the `supervisor` module
- Fixed a bug in the evaluation of `receive ... after infinity -> ...` expressions
- Fixed a bug in when putting integers in bit syntax with integer field sizes
- Fixed numerous bugs in memory allocations that could crash the VM
- Fixed SNTP support that had been broken in IDF 4.x builds
- Fixed `erlang:send/2` not sending to registered name

### Breaking Changes

> IMPORTANT: These changes are incompatible with previous releases of AtomVM.

- Changed the configuration model of the SPI driver, in order to allow for multiple "follower"
  devices to be attached to the same SPI Bus.
- Changed the return value from `erlang:system_info(esp32_chip_info)` from a tuple to a map, with
additional information.
- Changed the return type of the `network:start` function to return the tuple `{ok, Pid}` on a
successful call, instead of the bare atom `ok`.  Applications that use `network:start` and
check the return value will need to be modified.
- The return type of `i2c:read_bytes` has changed from returning just a binary to
returning the tuple `{ok, Binary}` when successful.
- The return type of many `i2c` operations under error conditions has changed from
`error` to `{error, Reason}`, for improved diagnostics.
- The eavmlib logger interface has been removed

### Removed
- ESP-IDF v3.x support.

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
- Fixed a bug in `gen_tcp` that prevents an accepting socket from inheriting settings on the listening socket.
- Fixed a bug in packing and unpacking integers into and from binaries when the
  bit length is not a multiple of 8.
- Fixed `esp:deep_sleep/1` that did not accept values above 31 minutes.
- Fixed a bug that could cause processes to hang indefinitely when calling ports that have terminated.
- Fixed potential VM crash when parsing external terms.
- Fixed the enforcement of `min_free_space` process option.

## [0.5.0] - 2022-03-22
