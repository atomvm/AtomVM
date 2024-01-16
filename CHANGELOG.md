# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.6.0-beta.0] - Unreleased

### Added
- Added `esp:get_default_mac/0` for retrieving the default MAC address on ESP32.
- Added support for `pico` and `poci` as an alternative to `mosi` and `miso` for SPI
- ESP32: Added support to SPI peripherals other than hspi and vspi
- Added `gpio:set_int/4`, with the 4th parameter being the pid() or registered name of the process to receive interrupt messages

### Changed

- Shorten SPI config options, such as `sclk_io_num` -> `sclk`
- Shorten I2C config options, such as `scl_io_num` -> `scl`
- Shorten UART config options, such as `tx_pin` -> `tx`
- Introduced support to non-integer peripheral names, `"i2c0"`, `"uart1"` (instead of just `0` and
- `1`, which now they are deprecated)
- New atom table, which uses less memory, has improved performances and better code.

### Fixed

- Fix several missing memory allocation checks in libAtomVM.
- Fixed a possible memory leak in libAtomVM/module.c `module_destroy`.

## [0.6.0-alpha.2] - 2023-12-10

### Fixed

- Fixed a bug where guards would raise exceptions instead of just being false
- Fixed support for big endian CPUs (such as some MIPS CPUs).
- Fixed STM32 not aborting when `AVM_ABORT()` is used
- Fixed a bug that would leave the STM32 trapped in a loop on hard faults, rather than aborting
- Fixed a bug that would make the VM to loop and failing to process selected fds on Linux
- Fixed classes of exceptions in estdlib.
- Fixed STM32 code that was hard coded to the default target device, now configured based on the `cmake -DDEVICE=` parameter
- Fixed hard fault on STM32 durung malloc on boards with more than one bank of sram
- Fixed invalid src_clk error on ESP-IDF >= 5.0
- Fixed changed default to `AVM_USE_32BIT_FLOAT=on` for STM32 platform to enable use of single precision hardware FPU on F4/F7 devices.
- Fixed a bug where emscripten `register_*_callback/1` functions would use x[1] as second argument
- Fixed precision of integers used with timers which could yield to halts and wait times smaller than expected
- Add support for ESP32-C6

### Changed

- Crypto functions on generic_unix platform now rely on MbedTLS instead of OpenSSL
- Platform function providing time used by timers was changed from `sys_monotonic_millis` to `sys_monotonic_time_u64`, `sys_monotonic_time_u64_to_ms` and `sys_monotonic_time_ms_to_u64`.
- Implement `atomvm:random/0` and `atomvm:rand_bytes/1` on top of `crypto:strong_rand_bytes/1` on
  generic_unix, ESP32 and RP2040 platforms.
- Performance improvements

### Added

- Added support for the OTP `socket` interface.
- Enhancd performance of STM32 by enabling flash cache and i-cache with branch prediction.
- Added cmake configuration option `AVM_CONFIG_REBOOT_ON_NOT_OK` for STM32
- New gpio driver for STM32 with nif and port support for read and write functions.
- Added support for interrupts to STM32 GPIO port driver.
- Added suppoprt for PicoW extra gpio pins (led) to the gpio driver.
- Added support for `net:getaddrinfo/1,2`
- Added minimal support for the OTP `ssl` interface.
- Added support for `crypto:one_time/4,5` on Unix and Pico as well as for `crypto:hash/2` on Pico
- Added ability to configure STM32 Nucleo boards onboard UART->USB-COM using the `-DBOARD=nucleo` cmake option
- Added STM32 cmake option `-DAVM_CFG_CONSOLE=` to select a different uart peripheral for the system console
- Added `crypto:strong_rand_bytes/1` using Mbed-TLS (only on generic_unix, ESP32 and RP2040
  platforms)
- Added support for setting the default receive buffer size for sockets via `socket:setopt/3`
- Added support for pattern matching binaries containing 32 and 64 bit floating point values, but
  only when aligned to byte boundaries (e.g. `<<0:4, F:32/float>> = Bin` is not supported).
- Added experimental backend to `get_tcp` and `get_udp` based on the new `socket` interface
- Added API for managing ESP32 watchdog (only on `esp-idf` >= v5.x)

### Removed

- OpenSSL support, Mbed-TLS is required instead.

## [0.6.0-alpha.1] - 2023-10-09

### Added

- Added erlang:spawn_link/1,3
- Added erlang:exit/2
- Added links to process_info/2
- Added lists:usort/1,2
- Added missing documentation and specifications for available nifs
- Added configurable logging macros to stm32 platform
- Added support for ULP wakeup on ESP32
- Added heap growth strategies as a fine-tuning option to `spawn_opt/2,4`
- Added `crypto:crypto_one_time/4,5` on ESP32
- Improved nif and port support on STM32
- Added support for `atomvm:posix_clock_settime/2`
- Added support for creations of binaries with unaligned strings
- Added `-h` and `-v` flags to generic_unix AtomVM command
- Removed support to ESP32 NVS from network module in order to make it generic. See also [UPDATING](UPDATING.md).
- Added initial support for Pico-W: on-board LED, Wifi (STA and AP modes).

### Changed

- Changed offset of atomvmlib and of program on Pico. See also [UPDATING](UPDATING.md).

### Fixed

- Fixed incorrect exit reason for exceptions of class exit
- Fixed several incorrect type specifications
- Fixed `esp:nvs_set_binary` functions.
- Fixed `monotonic_time/1` and `system_time/1` functions for Raspberry Pi Pico
- Fixed race conditions in atoms table.
- Fixed a bug in the STM32 port that caused the final result to never be returned.
- Fix bug when building a binary using a 64-bit integer on a 32-bit CPU.
- Fix (using 'auto' option)  SPI on ESP32 models other than ESP32, such as ESP32S2, ESP32C3, ...

## [0.6.0-alpha.0] - 2023-08-13

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
- Added support for `erlang:monotonic_time/1`
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
- Added `esp:nvs_fetch_binary/2` and `nvs_put_binary/3` functions (`esp:nvs_set_binary` and
functions that default to `?ATOMVM_NVS_NS` are deprecated now).
- Added most format possibilities to `io:format/2` and `io_lib:format/2`
- Added `unicode` module with `characters_to_list/1,2` and `characters_to_binary/1,2,3` functions
- Added support for `crypto:hash/2` (ESP32 and generic_unix with openssl)

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
