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
* [x] **DAC**
  * `dac.erl` exposes Zephyr's portable DAC API (`open`, `write` raw codes,
    `close`). Controllers are selected by `atomvm,dac`, index (`dac0`/`dac`
    as 0), or device name.
  * native_sim coverage uses `zephyr,dac-emul`. Classic ESP32 enables the
    on-chip DAC (GPIO25 channel 0). ESP32-C3 and ESP32-S3 have no DAC.
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
* [x] **Flash map**
  * `zephyr:flash_list/0`, `flash_read/3`, `flash_write/3`, and
    `flash_erase/2,3` wrap Zephyr's Flash Map (`flash_area`). Areas are
    selected by id or, with `CONFIG_FLASH_MAP_LABELS`, by partition label.
  * native_sim write/erase coverage uses `image-scratch` so Settings/NVS
    can keep `storage`. This is not a clone of `esp:partition_*`.
  * `zephyr:flash_mmap/3` maps flash through `spi_flash_mmap` on Espressif
    SoCs. Other boards return `{error, not_supported}`.

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
  * ESP32-C3 Wokwi is skipped: GitHub resets the TCP connection (`ECONNRESET`)
    mid-handshake after ~75s. ESP32-S3 Wokwi is skipped: handshake returns
    `{error,-2}` after ~7s. Classic ESP32 completes the same case.
* [x] **`test_wifi_scan` / `test_wifi_managed` (Wi-Fi Driver)**
  * *Purpose*: Verifies Wi-Fi access point scanning, connection events, and SSID listing.
  * *Dependencies*: Zephyr Wi-Fi management (`CONFIG_WIFI=y`). Also tested via `test_wifi_example` (covering TCP sockets/DNS).
* [x] **`test_wifi_ap` (SoftAP)**
  * `network:start([{ap, ...}])` enables Zephyr AP mode and waits for
    `ap_started`. The AP iface is given `192.168.4.1`. With
    `CONFIG_NET_DHCPV4_SERVER`, clients are leased from `192.168.4.2` and
    `sta_ip_assigned` is emitted. Station connect/disconnect events fire
    when a client joins or leaves. ESP32 uses `WIFI_USAGE_MODE_STA_AP`
    plus `WIFI_NM` so STA and AP can run on separate interfaces.
* [x] **`test_devmode` (SoftAP onboarding)**
  * `esp32devmode:start_network()` waits for AP, then binds the HTTP
    config page and ALISP console. Persistence uses `zephyr:settings_*`
    on Zephyr. Wokwi checks the `started` return, not a client join.
  * Default `gen_tcp`/`gen_udp` on Zephyr uses `{inet_backend, socket}`
    because there is no legacy `"socket"` port driver. The ZTEST is
    named `test_wifi_z_devmode` so it sorts after `test_ssl` and the
    other Wi-Fi cases; leftover AP sockets starve later STA.

---

## 4. Low Priority / Platform Specific Test Candidates

These test ESP32-specific registers, partition managers, or RTC hardware features and will require adaptation or custom drivers on Zephyr.

* [x] **`test_esp_partition` (Flash Partitions)**
  * *Adaptation*: `test_flash_map` covers Zephyr Flash Map list/read/write/erase
    on native_sim (`image-scratch`). Not an ESP-IDF partition-table clone.
* [x] **`test_deep_sleep_hold` (Power Management)**
  * *Adaptation*: Map to Zephyr's Power Management subsystem (`CONFIG_PM=y` / `pm_state`).
  * App-facing sleep is `zephyr:deep_sleep/0,1`, `sleep_enable_gpio_wakeup/2`,
    and `sleep_get_wakeup_cause/0`. ESP32 uses `sys_poweroff()` plus the
    Espressif timer/GPIO wakeup sources.
  * `test_deep_sleep` is the first ZTEST on ESP32: first boot sleeps 500 ms,
    reboot continues the suite after `sleep_get_wakeup_cause()` returns `timer`.
* [x] **`test_twdt` (Watchdog Timers)**
  * *Adaptation*: Map to Zephyr's standard Task Watchdog driver API (`CONFIG_TASK_WDT=y`).
