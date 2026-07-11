# TODOS - Zephyr Platform APIs and Remaining Tests

This document tracks the missing Zephyr platform API surface and the remaining
test cases from ESP32 (`src/platforms/esp32/test/main/test_main.c`).

## 1. Zephyr Erlang Hardware APIs

`libs/avm_zephyr` currently packages only `adc` and `i2c`. This does not need to
mirror every module from a chip-specific library: modules such as `esp`, `pico`,
and ESP32 `ledc` expose vendor-specific functionality. However, the common
hardware interfaces supported by Zephyr should have Erlang APIs and tests.

* [x] **ADC**
  * `adc.erl` uses Zephyr's devicetree-selected ADC channel.
  * The ESP32, ESP32-C3, and ESP32-S3 Wokwi diagrams provide a slide
    potentiometer for analog input testing.
  * The Wokwi test verifies averaged raw and millivolt readings.
* [x] **I2C**
  * `i2c.erl` implements `i2c_hal` on top of the Zephyr I2C NIF driver.
  * `native_sim` coverage uses Zephyr's I2C target emulator.
* [ ] **UART — high priority**
  * `uart_driver.c` already implements the Zephyr resource NIFs (`init`,
    `deinit`, `read`, `write`, `abort`, and state/error queries).
  * Add `libs/avm_zephyr/src/uart.erl`, implement the common `uart_hal` API,
    package it in `avm_zephyr.avm`, and add loopback/emulator coverage.
* [ ] **GPIO — high priority**
  * Add a Zephyr GPIO NIF driver and `gpio.erl` API using devicetree GPIO
    controllers rather than ESP32/RP2 pin-mux assumptions.
  * Cover input, output, pull configuration, and interrupts.
* [ ] **SPI — high priority**
  * Add a Zephyr SPI NIF driver and `spi.erl` implementation of `spi_hal`.
  * Select buses and chip selects through devicetree and add an emulator or
    Wokwi peripheral test.
* [ ] **PWM — medium priority**
  * Provide a portable Zephyr PWM API instead of copying the ESP32-specific
    `ledc` module.
* [ ] **DAC — medium priority**
  * Add an API using Zephyr's DAC subsystem on boards that expose a DAC.
* [ ] **USB CDC — lower priority**
  * Evaluate a common `usb_cdc` API over Zephyr's USB device stack. Do not add
    a separate module when the configured CDC ACM device is adequately exposed
    through the eventual UART API.
* [ ] **Flash map / storage — lower priority**
  * Expose Zephyr's Flash Map or NVS APIs where the generic file API is not
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

* [ ] **`test_socket` / `test_net` / `test_ssl` (Network Sockets)**
  * *test_socket*: Uses the deprecated/unsupported old `socket` port driver (not implemented for Zephyr, which uses modern socket NIFs instead).
  * *test_net*: Requires an active internet connection. It cannot be run as a standalone ZTEST before Wi-Fi is established, but it is successfully executed and verified inside `test_wifi_example` (after Wi-Fi connection is up).
  * *test_ssl*: Requires an active internet connection and Zephyr mbedTLS/PSA support, and is executed inside `test_wifi_example` after Wi-Fi connection is up.
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
