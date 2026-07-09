# TODOS - Porting Remaining ESP32 Tests to Zephyr

This document lists the remaining test cases from ESP32 (`src/platforms/esp32/test/main/test_main.c`) and assesses their feasibility for porting to the Zephyr platform runner.

## 1. High Priority Candidates (Ready to Port)

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

## 2. Medium Priority Candidates (Network / Hardware Dependent)

These tests require network or Wi-Fi emulation or a physical device board to execute successfully (though they can be built as build-only tests on QEMU).

* [ ] **`test_socket` / `test_net` / `test_ssl` (Network Sockets)**
  * *test_socket*: Uses the deprecated/unsupported old `socket` port driver (not implemented for Zephyr, which uses modern socket NIFs instead).
  * *test_net*: Requires an active internet connection. It cannot be run as a standalone ZTEST before Wi-Fi is established, but it is successfully executed and verified inside `test_wifi_example` (after Wi-Fi connection is up).
  * *test_ssl*: Requires an active internet connection and Zephyr mbedTLS/PSA support, and is executed inside `test_wifi_example` after Wi-Fi connection is up.
* [x] **`test_wifi_scan` / `test_wifi_managed` (Wi-Fi Driver)**
  * *Purpose*: Verifies Wi-Fi access point scanning, connection events, and SSID listing.
  * *Dependencies*: Zephyr Wi-Fi management (`CONFIG_WIFI=y`). Also tested via `test_wifi_example` (covering TCP sockets/DNS).

---

## 3. Low Priority / Platform Specific Candidates

These test ESP32-specific registers, partition managers, or RTC hardware features and will require adaptation or custom drivers on Zephyr.

* [ ] **`test_esp_partition` (Flash Partitions)**
  * *Adaptation*: Map to Zephyr's Flash Map API (`flash_area` / `CONFIG_FLASH_MAP=y`) or NVS.
* [x] **`test_deep_sleep_hold` (Power Management)**
  * *Adaptation*: Map to Zephyr's Power Management subsystem (`CONFIG_PM=y` / `pm_state`).
* [ ] **`test_twdt` (Watchdog Timers)**
  * *Adaptation*: Map to Zephyr's standard Watchdog driver API (`CONFIG_WATCHDOG=y`).
