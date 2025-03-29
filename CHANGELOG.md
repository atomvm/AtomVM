# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Added
- Added a limited implementation of the OTP `ets` interface
- Added `code:all_loaded/0` and `code:all_available/0`
- Added menuconfig option for enabling USE_USB_SERIAL, eg. serial over USB for certain ESP32-S2 boards etc.
- Partial support for `erlang:fun_info/2`
- Added support for `registered_name` in `erlang:process_info/2` and `Process.info/2`
- Added `net:gethostname/0` on platforms with gethostname(3).
- Added `socket:getopt/2`
- Added `supervisor:terminate_child/2`, `supervisor:restart_child/2` and `supervisor:delete_child/2`
- Added support for 'erlang:--/2'.
- Added `esp:partition_read/3`, and documentation for `esp:partition_erase_range/2/3` and `esp:partition_write/3`
- Added support for list insertion in 'ets:insert/2'.
- Support to OTP-28
- Added support for `ets:update_counter/3` and `ets:update_counter/4`.
- Added `erlang:+/1`
- Added `lists:append/1` and `lists:append/2`
- Added `erlang:spawn_monitor/1`, `erlang:spawn_monitor/3`
- Added `lists:dropwhile/2`.
- Support for `float/1` BIF.
- Added `erlang:get/0` and `erlang:erase/0`.
- Added `erlang:unique_integer/0` and `erlang:unique_integer/1`

### Changed
- `binary_to_integer/1` doesn't accept anymore binaries such as `"0xFF"` or `"  123"`

### Fixed
- ESP32: improved sntp sync speed from a cold boot.
- Utilize reserved `phy_init` partition on ESP32 to store wifi calibration for faster connections.
- Support for zero count in `lists:duplicate/2`.

## [0.6.6] - Unreleased

### Added

- Added the ability to run beams from the CLI for Generic Unix platform (it was already possible
with nodejs and emscripten)
- Added preliminary support for ESP32P4 (no networking support yet).
- Added memory info in `out_of_memory` crash logs to help developers fix memory issues.
- Added documentation and function specs for uart driver
- Added `uart:read/2` with a timeout parameter.
- Missing `erlang:is_function/2` BIF

### Fixed

- Fixed specifications of nifs from `esp_adc` module
- ESP32: fix `gpio:init/1` on GPIO >= 32
- Adding missing check, passing a non numeric argument to a function expecting a floating point
might lead to a crash in certain situations.
- Fixed several bugs in `http_server` (#1366)
- Fixed generic\_unix `socket_driver` to return `{gen_tcp, closed}` when socket is closed on Linux
instead of `{gen_tcp, {recv, 104}}`
- Fixed a memory leak where modules were not properly destroyed when the global context is destroyd
- alisp: fix support to variables that are not binaries or integers.
- Fixed destruction of ssl-related resources
- Fixed corruption when dealing with specific situations that involve more than 16 x registers when
certain VM instructions are used.
- Fixed ESP32 GPIO interrupt trigger `none`
- Fixed an issue where a timeout would occur immediately in a race condition
- Fixed SPI close command
- Added missing lock on socket structure
- Fixed a race condition affecting multi-core MCUs where a timeout would not be properly cleared
- Fixed a double free when esp32 uart driver was closed, yielding an assert abort
- Fixed compilation with latest debian gcc-arm-none-eabi
- Fixed `network:stop/0` on ESP32 so the network can be started again
- Fixed a memory corruption caused by `binary:split/2,3`
- Fixed deadlock in socket code
- Fixed bug in opcode implementation (`select_val`): when selecting a value among many others a
shallow comparison was performed, so it was working just for plain values such as atoms and small
integers
- Fixed support for setting esp32 boot_path in NVS.
- Fixed race conditions in network:start/stop.
- Fixed crash calling network:sta_rssi(), when network not up.
- Fixed error handling when calling `min` and `max` with code compiled before OTP-26: there was a
bug when handling errors from BIFs used as NIFs (when called with `CALL_EXT` and similar opcodes)
- Fixed matching of binaries on unaligned boundaries for code compiled with older versions of OTP
- Added missing out of memory handling in binary_to_atom
- Fixed call to funs such as fun erlang:'not'/1, that make use of BIFs
- Fixed potential crashes or memory leaks caused by a mistake in calculation of reference counts
and a race condition in otp_socket code
- Fixed an out of memory issue by forcing GC to copy data from message fragments
- Fixed a bug where calling repeatedly `process_info` on a stopped process could cause an out of
memory error
- Fixed possible concurrency problems in ESP32 UART driver
- Fixed concurrency and memory leak related to links and monitors
- Fixed issues with parsing of line references for stack traces
- Fixed memory corruption issue with `erlang:make_tuple/2`
- Fix potential use after free with code generated from OTP <= 24
- Fix `is_function/2` guard
- Fixed segfault when calling `lists:reverse/1` (#1600)
- Fixed nif_atomvm_posix_read GC bug

### Changed

- ESP32 UART driver no longer aborts because of badargs in configuration, instead raising an error

## [0.6.5] - 2024-10-15

### Added

- ESP32: add a new Elixir release "flavor" with a bigger boot.avm partition that has room for
Elixir standard library modules
- ESP32: `--boot` option to mkimage.sh tool
- Add `erlang:atom_to_binary/1` that is equivalent to `erlang:atom_to_binary(Atom, utf8)`
- Support for Elixir `String.Chars` protocol, now functions such as `Enum.join` are able to take
also non string parameters (e.g. `Enum.join([1, 2], ",")`
- Support for Elixir `Enum.at/3`
- Add support for `is_bitstring/1` construct which is used in Elixir protocols runtime.
- Add support to Elixir `Enumerable` protocol also for `Enum.all?`, `Enum.any?`, `Enum.each`,
`Enum.filter`, `Enum.flat_map`, `Enum.reject`, `Enum.chunk_by` and `Enum.chunk_while`
- Support for `maps:merge_with/3`
- Support for `lists:last/1` and `lists:mapfoldl/3`
- Add support to Elixir for `Process.send/2` `Process.send_after/3/4` and `Process.cancel_timer/1`
- Add support for `handle_continue` callback in `gen_server`
- Support for Elixir `List.Chars` protocol
- Support for `gen_server:start_monitor/3,4`
- Support for `code:ensure_loaded/1`
- Support for `io_lib:latin1_char_list/1`
- Add support to Elixir for `Keyword.split/2`
- Support for `binary:split/3` and `string:find/2,3`
- Support for large tuples (more than 255 elements) in external terms.
- Support for `io:put_chars/2`
- Support for `lists:nthtail/2`
- Support for Elixir `IO.chardata_to_string/1`
- Support for Elixir `List.duplicate/2`
- Support for `binary:copy/1,2`
- Support for directory listing using POSIX APIs: (`atomvm:posix_opendir/1`,
`atomvm:posix_readdir/1`, `atomvm:posix_closedir/1`).
- ESP32: add support for `esp_adc` ADC driver, with Erlang and Elixir examples
- Add handler for ESP32 network driver STA mode `beacon_timeout` (event: 21), see issue
[#1100](https://github.com/atomvm/AtomVM/issues/1100)
- Support for mounting/unmounting storage on ESP32 (such as SD or internal flash) using
`esp:mount/4` and `esp:umount/1`
- Support for `binary_to_integer/2`
- Support for `binary:decode_hex/1` and `binary:encode_hex/1,2`
- Support for Elixir `Base.decode16/2` and `Base.encode16/2`
- Make external term serialize functions available without using `externalterm_to_binary` so terms
can be written directly to a buffer.
- Support for `erlang:list_to_integer/2`
- Add `externalterm_to_term_copy` that can be safely used from NIFs taking temporary buffers

### Changed

- ESP32: Elixir library is not shipped anymore with `esp32boot.avm`. Use `elixir_esp32boot.avm`
instead
- `Enum.find_index` and `Enum.find_value` support Enumerable and not just lists
- Install AtomVM libraries source code and binaries for better dialyzer integration
- Made the `device_config` properties list in `spi:open/1` optional (defaults to `[]`), so you can use the function with only a `bus_config`

### Fixed

- ESP32: content of `boot.avm` partition is not truncated anymore
- ESP32: `Fixed gpio:set_int` to accept any pin, not only pin 2
- Fix memory corruption in `unicode:characters_to_binary`
- Fix handling of large literal indexes and large extended literal indexes
- `unicode:characters_to_list`: fixed bogus out_of_memory error on some platforms such as ESP32
- Fix crash in Elixir library when doing `inspect(:atom)`
- General inspect() compliance with Elixir behavior (but there are still some minor differences)
- Fix several uses of free on prevously released memory on ESP32, under certain error condition using
`network:start/1`, that would lead to a hard crash of the VM.
- Fix a bug in ESP32 network driver where the low level driver was not being stopped and resoureces were not freed
when `network:stop/0` was used, see issue [#643](https://github.com/atomvm/AtomVM/issues/643)
- `uart:open/1,2` now works with uppercase peripheral names

## [0.6.4] - 2024-08-18

### Added

- Implement `gpio:init/1` on esp32 to initialize pins for GPIO usage, which some pins
require depending on default function and bootloader code
- Implement missing opcode 161 (raw_raise), that looks more likely to be generated with Elixir code
- Support for Elixir `Map.replace/3` and `Map.replace!/3`
- Support for Elixir `Kernel.struct` and `Kernel.struct!`
- Support for Elixir `IO.iodata_to_binary/1`
- Support for Elixir exceptions: `Exception` module and the other error related modules such as
`ArgumentError`, `UndefinedFunctionError`, etc...
- Support for Elixir `Enumerable` and `Collectable` protocol
- Support for Elixir `Enum` functions: `split_with`, `join`, `map_join`, `into`, `reverse`,
`slice` and `to_list`
- Support for Elixir `MapSet` module
- Support for Elixir `Range` module
- Support for Elixir `Kernel.min` and `Kernel.max`
- Support (as stub) for `erlang:error/3` (that is required from Elixir code)

## [0.6.3] - 2024-07-20

### Added

- Simple http client, that can be used for different use case such as downloading OTA updates
- Elixir support for `Keyword.merge` `Keyword.take` `Keyword.pop(!)` `Keyword.keyword?` `Keyword.has_key?` functions.
- Support for ESP32-H2
- lists:keytake/3 implemented.
- Support for setting channel used by network driver wifi access point.
- Support for `maps:iterator/2` and `~kp` with `io_lib:format/2` that were introduced with OTP26.
- Support for `erlang:apply/2`
- Support for `lists:keystore/4`
- Support for `erlang:size/1` bif
- Support for USB serial output on ESP32 (needs to be manually enabled)
- Support for `lists:filtermap/2`
- Support for standard library `queue` module
- Support for `maps:from_keys/2` NIF
- Support for standard library `sets` module

### Changed

- ESP32 network driver messages for event 40 (home channel change events) are now suppressed, but the
details for the channel changes can be observed in the console log if "debug" level logging is enabled
in ESP-IDF Kconfig options.
- Default size of ESP32 RTC slow memory from 4086 to 4096, except on ESP32-H2 where it's 3072
- Update `byte_size/1` and `bit_size/1` to implement OTP27 match context reuse optimization OTP-18987.

### Fixed

- Fix bug (with code compiled with OTP-21) with binary pattern matching: the fix introduced with
`02411048` was not completely right, and it was converting match context to bogus binaries.
- Fix creation of multiple links for the same process and not removing link at trapped exits.
See issue [#1193](https://github.com/atomvm/AtomVM/issues/1193).
- Fix error that is raised when a function is undefined
- Fix a bug that could yield crashes when functions are sent in messages
- Fix bug where failing guards would corrupt x0 and x1
- Fix a memory leak when raising out of memory error while executing PUT_MAP_ASSOC instruction

## [0.6.2] - 25-05-2024

### Added

- Support for DragonFly BSD (generic\_unix platform).
- Added guards `is_even` and `is_odd` to the `Integer` module
- Add a number of functions to proplists module, such as `delete/2`, `from/to_map/1`, etc...
- Add `esp:deep_sleep_enable_gpio_wakeup/2` to allow wakeup from deep sleep for ESP32C3 and ESP32C6.
- Obtain RSSI of the current connection with `network:sta_rssi/0` on ESP32.
- Pico-W support for `network:sta_rssi/0`.
- Add support to ESP32C2

### Fixed

- Fix invalid read after free in ssl code, see also issue
[#1115](https://github.com/atomvm/AtomVM/issues/1115).
- Fix semantic of `ssl:recv(Socket, 0)` to return all available bytes, matching what OTP does.
- Fix `binary` option handling in `ssl:connect/3` so `binary` can be used instead of
`{binary, true}`.
- Fix scheduling of trapped process that were wrongly immediately rescheduled before being signaled.
- Fix `gen_tcp` and `ssl` types.
- Fix documentation and specification of `esp:sleep_enable_ext0_wakeup/2` and `esp:sleep_enable_ext1_wakeup/2`.

### Changed
- Stacktraces are included by default on Pico devices.
- Changed ssl default from `{active, false}` to `{active, true}` in order to have same behavior as
OTP. Since active mode is not supported right now, `active` must be explicitly set to false:
`ssl:connect(..., ..., [{active, false}, ...])`, otherwise it will crash.

## [0.6.1] - 2024-04-17

### Added

- Added experimental optimized GC mode that makes use of C realloc instead of copying data around,
it can be enabled with `-DENABLE_REALLOC_GC=On`.

### Fixed

- Fix bug in `erlang:ref_to_list/1` and `erlang:display/1`: the unique integer was truncated on some
32-bit architectures
- Stop hardcoding `erl_eval` as module name in both display and fun_to_list
- Correctly display and convert to list funs such as `fun m:f/a`
- Fixed bug in STM32 cmake that could cause builds with multiple jobs to fail due to incorrect artifact dependency
- Fix crash on macOS due to missing call to `psa_crypto_init` for TLS 1.3
- Fix crypto test on rp2040

## [0.6.0] - 2024-03-05

### Fixed

- Fix a bug that broke sockets on ESP32-C3 and other single core ESP32 devices, that may also
cause other issues. The bug has been introduced with messages from tasks change between beta.1
and rc.0
- Fixed several issues related to Esp32 socket_driver that made it unreliable, especially with
single core MCUs

## [0.6.0-rc.0] - 2024-03-03

### Added

- `BOOTLOADER_OFFSET` for all current Esp32 models.
- Added API to send messages from FreeRTOS tasks or pthreads, typically to
easily support integration with Esp32 callbacks

### Fixed

- `BOOTLOADER_OFFSET` was incorrect for Esp32-C6 and Esp32-S2.
- Fixed a bug that would fail to set DHCP hostname in STA+AP mode on all ESP32 platforms.
- ESP32-S3: crash in network driver caused by a smaller stack size for scheduler threads, when
calling `esp_wifi_init()`. See also issue [#1059](https://github.com/atomvm/AtomVM/issues/1059).
- Fixed Esp32 network driver on non-SMP builds
- ESP32: fixed bug in `gpio:stop/0` and `gpio:close/1` that would cause the VM to crash.

## [0.6.0-beta.1] - 2024-02-28

### Added

- Support for utf8 encoding to `*_to_atom` and `atom_to_*` functions
- `binary_to_atom/1` and `atom_to_binary/1` that default to utf8 (they were introduced with OTP23)
- Added Pico cmake option `AVM_WAIT_BOOTSEL_ON_EXIT` (default `ON`) to allow tools to use automated `BOOTSEL` mode after main application exits
- Use UTF-8 encoding for atoms when using `erlang:term_to_binary/1`, in conformance with OTP-26
- Pico: Wait for USB serial connection `cmake` configuration option `AVM_USB_WAIT_SECONDS` added with 20 second default.
- Support for code that makes use of more than 16 live registers, such as functions with > 16
parameters and complex pattern matchings.

### Fixed

- ESP32: fix i2c_driver_acquire and i2c_driver_release functions, that were working only once.
- Sending messages to registered processes using the `!` operator now works.
- Fixed bug in `OP_SEND` that would accept sending a message to any integer or term without raising an error.
- `binary_to_term` checks atom encoding validity, and fix latin1 support (when non-ASCII chars are
used)
- ESP32: fixed bug in `gpio:set_pin_mode/2` and `gpio:set_direction/3` that would accept any atom for the mode parameter without an error.
- ESP32: GPIO driver fix bug that would accept invalid `pull` direction, and silently set `pull` direction to `floating` without issuing an error.
- ESP32: fixed bug in gpio driver that would accept invalid pin numbers (either negative, or too large)
- RP2040: fixed bug in `gpio:set_pin_pull/2` that would accept any parameter as a valid `pull` mode.
- Support to function with 10 or more parameters
- Very unlikely but possible corruption caused by generated code that uses 16 live registers

### Changed

- `binary_to_atom/2` validates utf8 strings
- `*_to_atom` and `atom_to_*` properly convert latin1 (not just ASCII) to utf8 and viceversa
- ESP32: use esp-idf v5.1.3 for building release binaries

## [0.6.0-beta.0] - 2024-02-08

### Added
- Added `esp:get_default_mac/0` for retrieving the default MAC address on ESP32.
- Added support for `pico` and `poci` as an alternative to `mosi` and `miso` for SPI
- ESP32: Added support to SPI peripherals other than hspi and vspi
- Added `gpio:set_int/4`, with the 4th parameter being the pid() or registered name of the process to receive interrupt messages
- Added support for `lists:split/2`
- Added ESP32 API for allowing coexistence of native and Erlang I2C drivers

### Changed

- Shorten SPI config options, such as `sclk_io_num` -> `sclk`
- Shorten I2C config options, such as `scl_io_num` -> `scl`
- Shorten UART config options, such as `tx_pin` -> `tx`
- Introduced support to non-integer peripheral names, `"i2c0"`, `"uart1"` (instead of just `0` and
- `1`, which now they are deprecated)
- New atom table, which uses less memory, has improved performances and better code.
- SPI: when gpio number is not provided for `miso` or `mosi` default to disabled
- Change port call tuple format to the same format as gen_server, so casts can be supported too

### Fixed

- Fix several missing memory allocation checks in libAtomVM.
- Fixed a possible memory leak in libAtomVM/module.c `module_destroy`.
- Fix possibile bug in random number generator on ESP32 and RPi2040
- Fixed interpretation of live for opcodes, thus altering GC semantics for nifs. See also [UPDATING](UPDATING.md).

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
