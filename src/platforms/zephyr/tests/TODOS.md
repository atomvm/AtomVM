# TODOS - Zephyr Platform APIs and Remaining Tests

This document tracks the missing Zephyr platform API surface and the remaining
test cases from ESP32 (`src/platforms/esp32/test/main/test_main.c`).

## 1. Zephyr Erlang Hardware APIs

`libs/avm_zephyr` packages the portable hardware HALs plus `zephyr.erl` for
platform NIFs (mount, socketpair, PM, task watchdog). This does not need to
mirror every module from a chip-specific library: modules such as `esp`, `pico`,
and ESP32 `ledc` expose vendor-specific functionality. However, the common
hardware interfaces supported by Zephyr should have Erlang APIs and tests.

Public platform subsystems are enabled by the normal board configuration and
inherited by tests. Application-specific ADC channels, SPI pinctrl, chip
selects, and other wiring remain in application/test overlays.

* [x] **ADC**
  * `adc.erl` uses Zephyr's devicetree-selected ADC channel.
  * The ESP32, ESP32-C3, and ESP32-S3 Wokwi diagrams provide a slide
    potentiometer for analog input testing.
  * The Wokwi test verifies averaged raw and millivolt readings.
* [x] **I2C**
  * `i2c.erl` implements `i2c_hal` on top of the Zephyr I2C NIF driver.
  * `native_sim` coverage uses Zephyr's I2C target emulator.
* [x] **UART**
  * `uart_driver.c` already implements the Zephyr resource NIFs (`init`,
    `deinit`, `read`, `write`, `abort`, and state/error queries).
  * `uart.erl` implements `uart_hal` with devicetree controller selection and
    scheduler-safe polling around the native driver's bounded operations.
  * Smoke coverage verifies both resource NIFs and the high-level API without
    transmitting data on the test console. Add loopback/emulator coverage when
    a dedicated non-console UART is available in the simulator configuration.
* [x] **GPIO**
  * `gpio.erl` implements `gpio_hal` using Zephyr GPIO controllers. Pins may
    use the chosen/default controller or explicitly select a controller with
    `{Controller, Pin}`.
  * Direction, pull, read, write, and GPIO interrupt registration have
    simulator smoke coverage. Add dedicated interrupt loopback coverage later.
  * Cover input, output, pull configuration, and interrupts.
* [x] **SPI**
  * `spi.erl` implements `spi_hal` over a Zephyr SPI resource NIF. Controllers
    and bus pins are selected through devicetree; optional software chip
    selects use the GPIO API.
  * Native simulator coverage verifies lifecycle and transaction validation.
    Add a Wokwi peripheral test for device-specific transfers later.
* [x] **Retention memory**
  * `retention.erl` exposes Zephyr's portable retention API.
  * ESP32, ESP32-C3, and ESP32-S3 tests reserve a checksummed 256-byte RTC RAM
    partition for data that survives deep sleep.
  * QEMU x86_64 exercises the portable RAM-backed implementation, and RP2040
    reserves the final 256 bytes of SRAM for build and Wokwi validation.
  * Normal AtomVM board fragments mirror the test configurations so retention
    is available in standard firmware builds, not only in the test image.
* [x] **PWM**
  * `pwm.erl` exposes Zephyr's portable PWM API (`set` in nanoseconds,
    `set_cycles`, and `get_cycles_per_sec`). Controllers are selected by
    `atomvm,pwm`, index (`pwm0`/`ledc0` as 0), or device name.
  * Native simulator coverage uses Zephyr's fake PWM driver.
* [ ] **DAC — medium priority**
  * Add an API using Zephyr's DAC subsystem on boards that expose a DAC.
* [ ] **USB CDC — lower priority**
  * Evaluate a common `usb_cdc` API over Zephyr's USB device stack. Do not add
    a separate module when the configured CDC ACM device is adequately exposed
    through the eventual UART API.
* [x] **Settings / NVS**
  * `zephyr:settings_get/2,3`, `settings_put/3`, and `settings_erase/2` store
    binary values under `Namespace/Key` via Zephyr Settings + NVS.
  * ESP32 boards use the existing `storage` partition. Pico W carves 24 KiB
    from the end of the 2 MiB flash (ESP-IDF default NVS size). native_sim uses
    the board's simulated-flash `storage` partition (16 KiB). This is not a
    clone of `esp:nvs_*`.
* [ ] **Flash map — lower priority**
  * Expose Zephyr's Flash Map API where the settings and file APIs are not
    sufficient. Do not copy ESP32 partition APIs verbatim.

## 2. High Priority Test Candidates (Ready to Port)

These tests check platform-independent core/NIF functionalities that are fully supported by Zephyr's subsystems.

* [x] **`test_select` (POSIX Select)**
  * *Purpose*: Verifies event multiplexing, eventfd, and socket notifications.
  * *Dependencies*: Zephyr's ZVFS and POSIX select APIs (`CONFIG_POSIX_API=y`).
* [x] **`test_file` (File I/O)**
  * *Purpose*: Tests file access, seeking, directory listing, and cleanup.
  * *Dependencies*: Zephyr VFS (`CONFIG_FILE_SYSTEM=y`), now enabled in `test_mount`.
* [x] **`test_md5` / `test_crypto` (Cryptography)**
  * *Purpose*: Verifies MD5, SHA, and symmetric encryption NIFs.
  * *Dependencies*: mbedtls / tinycrypt integration in the Zephyr build.
* [ ] **`atomvm_smp_0` (SMP Scheduling)**
  * *Purpose*: Verifies VM multi-scheduler concurrency across multiple physical CPU cores.
  * *Dependencies*: Zephyr SMP support (`CONFIG_SMP=y`).

---

## 3. Medium Priority Test Candidates (Network / Hardware Dependent)

These tests require network or Wi-Fi emulation or a physical device board to execute successfully (though they can be built as build-only tests on QEMU).

* [ ] **`test_socket` / `test_net` (Network Sockets)**
  * *test_socket*: Uses the deprecated/unsupported old `socket` port driver (not implemented for Zephyr, which uses modern socket NIFs instead).
  * *test_net*: Requires an active internet connection. It cannot be run as a standalone ZTEST before Wi-Fi is established, but it is successfully executed and verified inside `test_wifi_example` (after Wi-Fi connection is up).
* [x] **`test_ssl` (TLS client)**
  * *Purpose*: Verifies otp_ssl handshake, HTTP over TLS, and socket select against github.com.
  * *Dependencies*: Wi-Fi STA, TCP, DNS, and mbedTLS/PSA. Runs as its own ZTEST after the other Wi-Fi cases.
* [x] **`test_wifi_scan` / `test_wifi_managed` (Wi-Fi Driver)**
  * *Purpose*: Verifies Wi-Fi access point scanning, connection events, and SSID listing.
  * *Dependencies*: Zephyr Wi-Fi management (`CONFIG_WIFI=y`). Also tested via `test_wifi_example` (covering TCP sockets/DNS).

---

## 4. Low Priority / Platform Specific Test Candidates

These test ESP32-specific registers, partition managers, or RTC hardware features and will require adaptation or custom drivers on Zephyr.

* [ ] **`test_esp_partition` (Flash Partitions)**
  * *Adaptation*: Map to Zephyr's Flash Map API (`flash_area` / `CONFIG_FLASH_MAP=y`) or NVS.
* [x] **`test_deep_sleep_hold` (Power Management)**
  * *Adaptation*: Map to Zephyr's Power Management subsystem (`CONFIG_PM=y` / `pm_state`).
* [x] **`test_twdt` (Watchdog Timers)**
  * *Adaptation*: Map to Zephyr's standard Task Watchdog driver API (`CONFIG_TASK_WDT=y`).
