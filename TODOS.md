<!---
  Copyright 2026 Peter M. <petermm@gmail.com>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# RTEMS port TODOs

The main remaining gaps are API completeness and hardware validation.
Networking, TLS/crypto, SMP, flash/NVS and basic peripherals already exist on
supported targets. Support varies by BSP; passing emulator tests does not
establish correct physical hardware behavior.

## Recently completed

- [x] Align S31 CPLL320 calibration, modem/low-power clocks, first-enable PHY
  calibration/wakeup/shutdown, PLL tracking and baseband watchdog settings with
  ESP-IDF. Preserve per-hart PMA mappings and apply S31 peripheral-access setup.
  Add recursive SMP-safe PHY/Wi-Fi/I2C critical sections in internal RAM.
  Emulator Wi-Fi, Erlang SMP and two-core nested-lock contention tests pass;
  physical S31 validation remains open.

- [x] Align C6 direct-boot PLL calibration/flash divider, modem clock dependencies,
  low-power clock selection, DMA access filters and first-enable combined-radio
  PHY lifecycle with ESP-IDF. Free calibration scratch storage, track PLL
  parameters and preserve nested Wi-Fi/PHY/I2C critical sections in internal RAM.
  C6 hardware validation remains open; only the emulator is available.

- [x] Align C3 modem/low-power clocks, first-enable PHY calibration, PLL tracking
  and RF shutdown with ESP-IDF; release calibration scratch storage. Correct the
  CPU source mask/APB frequency record, enable combined-radio PHY mode, preserve
  digital registers across disable/wakeup and retain nested PHY/Wi-Fi/I2C critical
  sections in internal instruction RAM. Physical C3 scan, WPA2, traffic and
  stop/restart testing passes with the aligned image.
- [x] Calibrate the C5 system PLL for its detected 40/48 MHz crystal during
  direct boot, and preserve default Wi-Fi radio arbitration priorities.
  Physical 48 MHz C5 scanning now works on both bands.
- [x] Align C5 modem clocks, PHY calibration/wakeup/shutdown, PLL tracking and
  DMA access initialization with ESP-IDF. Physical C5 testing passes WPA2,
  DHCP, RSSI, DNS, UDP/TCP traffic and stop/restart with default protocols.
  Free C5 calibration scratch data after use, matching ESP-IDF.

- [x] Measured station RSSI and active/passive Wi-Fi scanning on ESP32-C3/C5/C6/S31,
  including managed station mode, callbacks and cancellation.
- [x] Per-chip ADC, low-speed LEDC/PWM, RTC storage/counters and timer/GPIO sleep
  paths for C3/C5/C6/S31.
- [x] Remove synthetic ADC readings: conversion timeouts return `{error, timeout}`.
- [x] Replace delay/reset-based sleep with RTC/PMU hardware entry, RTEMS clock and
  timer recovery, and S31 second-CPU coordination. Physical validation remains open.

## Implementation reference

- **Boot and basic I/O:** ESP direct-boot images embed AVM applications; GRiSP 2
  can load its application from SD. UART, GPIO, I2C and SPI support depends on the
  BSP. Build instructions and the per-chip peripheral matrix are in the
  [RTEMS README](src/platforms/rtems/README.md).
- **Networking and time:** Station networking includes DHCP, DNS, UDP/TCP and
  passive sockets. [sntp.c](src/platforms/rtems/src/lib/sntp.c) updates the system
  clock and records RTC timekeeper state for restoration at boot.
- **TLS and crypto:** MbedTLS-backed `ssl` and `crypto` NIFs and platform entropy
  hooks already exist. See
  [otp_crypto_platform.c](src/platforms/rtems/src/lib/otp_crypto_platform.c),
  [otp_ssl_platform.c](src/platforms/rtems/src/lib/otp_ssl_platform.c) and
  [sys.c](src/platforms/rtems/src/lib/sys.c).
- **RTC persistence:** User binary storage and the timekeeper have CRC checks.
  RTC RAM starts at `0x50000000` on C3/C5/C6 and `0x2e000000` on S31; the
  timekeeper occupies a separate area at base + `0x1000`. See
  [rtc_slow_nif.c](src/platforms/rtems/src/lib/rtc_slow_nif.c) and the hardware
  counter/sleep backend in [esp_sleep_hw.c](src/platforms/rtems/src/lib/esp_sleep_hw.c).
- **Storage:** The default 4 MiB ESP flash layout reserves 128 KiB for NVS at
  `0x3c0000` and 128 KiB for JFFS2 at `0x3e0000`, mounted at `/flash`. NVS supports
  namespaces, binary values and CRC checks. Layout defaults are in
  [flash_driver.h](src/platforms/rtems/src/lib/flash_driver.h); implementations are
  in [nvs_driver.c](src/platforms/rtems/src/lib/nvs_driver.c) and
  [flash_fs.c](src/platforms/rtems/src/lib/flash_fs.c).
- **Watchdogs:** Task-watchdog user tracking and hardware watchdog handling
  already exist in [wdt.c](src/platforms/rtems/src/lib/wdt.c). These still need
  physical validation alongside sleep and peripheral activity.

## Peripheral behavior

- [ ] Implement timed LEDC fades, including step/cycle settings, wait modes and
  cancellation. The current fade API immediately applies the target duty.
  See [ledc_driver.c](src/platforms/rtems/src/lib/ledc_driver.c).
- [ ] Add eFuse-based ADC voltage calibration where supported on C3/C5/C6;
  current millivolt values are estimates. S31 returns raw samples and `undefined`
  voltage; voltage conversion needs a documented transfer function before it
  can be implemented. See [adc_driver.c](src/platforms/rtems/src/lib/adc_driver.c).
- [ ] Implement global deep-sleep pad hold on C5/C6/S31 where supported, or retain
  explicit unsupported errors. These calls currently return `{error, enotsup}`;
  individual pad holds are available. See
  [rtc_slow_nif.c](src/platforms/rtems/src/lib/rtc_slow_nif.c).

## Networking

- [ ] Add SoftAP configuration, lifecycle and events to the RTEMS network backend.
- [ ] Add mDNS integration and service discovery/advertising.
- [ ] Add scanning on LibBSD Wi-Fi targets. RSSI exists there, but scanning
  currently returns `{error, enotsup}`.

See [network_driver.c](src/platforms/rtems/src/lib/network_driver.c) and
[wifi.c](src/platforms/rtems/src/lib/wifi.c).

## Runtime file loading

- [ ] Implement `sys_open_avm_from_file()` for AVM packs and
  `sys_load_module_from_file()` for individual BEAM modules, including missing
  and malformed file handling. Both general filesystem hooks are stubbed.
  GRiSP 2 SD-card application loading at boot already works through a separate
  loader. See [sys.c](src/platforms/rtems/src/lib/sys.c).

## ESP API compatibility

- [ ] Add `esp:restart/0`.
- [ ] Add MAC-address access APIs.
- [ ] Add partition discovery and partition access APIs.

Compare the RTEMS [esp.erl](libs/avm_rtems/src/esp.erl) with the existing
[ESP32 module](libs/avm_esp32/src/esp.erl), preserving explicit errors for
operations a target cannot support.

## JIT and Bluetooth LE

- [ ] Add RTEMS JIT support, including executable memory allocation and instruction
  cache synchronization for supported architectures. JIT is currently forced off
  in [CMakeLists.txt](src/platforms/rtems/CMakeLists.txt).
- [ ] Integrate a BLE controller and host stack with AtomVM. Existing BSP Bluetooth
  interrupt/power support and chip capability flags do not provide a BLE stack.
  Wi-Fi now registers ESP-IDF's software coexistence adapter and runs
  `coex_pre_init()` from `esp_wifi_init()` (`libcoexist.a`, pinned to the HAL
  gitlink) instead of the disabled-coexistence stubs. A controller still has to
  register its own coexistence callbacks and share the PHY.

- [ ] Validate coexistence arbitration with a BLE controller. Wi-Fi alone
  exercises the adapter; the emulator Wi-Fi suites cover it on all four ESP32
  targets.

## Physical validation

- [ ] Validate GRiSP 2 boot, board/FDT compatibility, Ethernet/DHCP, peripherals,
  SD-card application replacement and embedded fallback on a real board.
- [ ] Validate Wi-Fi association, measured RSSI, scanning, cancellation,
  stop/restart and network traffic on physical C3/C5/C6/S31 boards.
  C3 testing passes active/passive scanning (two APs), association, DHCP,
  measured RSSI, DNS, UDP loopback, TCP/HTTP traffic and stop/restart.
  C5 testing passes active/passive scans (three APs across 2.4 GHz and 5 GHz),
  WPA2 association on 2.4 GHz, DHCP, measured RSSI, DNS, UDP loopback, TCP/HTTP
  and stop/restart after aligning initialization with ESP-IDF. Two consecutive
  final-image runs pass after freeing calibration scratch data; extend restart
  cycling under memory pressure. C5 5 GHz association and C6/S31 remain
  untested physically; scan cancellation also needs physical validation.
- [ ] Measure ADC accuracy and PWM frequency/duty on each chip, including pin
  ownership, alternate channels and peripheral coexistence.
- [ ] Validate light/deep sleep power transitions, timer and GPIO wakeup, reset
  reason, RTC retention, elapsed time and watchdog/timer behavior on each chip.
  Verify both S31 CPUs resume correctly after light sleep. Use the existing
  [deep-sleep test](src/platforms/rtems/tests/test_erl_sources/test_deep_sleep.erl)
  for reset and retention checks.
  C3 and C5 timer light sleep return, but an expired Erlang timer is not
  delivered within the test deadline and USB console continuity is unreliable.
  Clearing the previous RTC alarm fixes early C3 wakeup; timer recovery remains
  incomplete.
  RTC counters/storage, raw ADC conversion and PWM register readback work;
  analog accuracy, PWM waveforms and deep-sleep retention are unverified.

The sleep CI fixture substitutes the RTC counter and PMU entry point because
the pinned emulator does not model the necessary hardware behavior. It verifies
RTEMS integration, not physical sleep or power consumption. Board firmware must
use the normal hardware backend. See the
[RTEMS README](src/platforms/rtems/README.md) for test setup and limitations.
