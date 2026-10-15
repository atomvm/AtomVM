<!---
  Copyright 2026 Peter M. <petermm@gmail.com>

  SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# ESP32-C3 RTEMS Port: Remaining Work & Roadmap (TODOS)

This document tracks unimplemented features, architectural opportunities, and hardware peripheral support for the **ESP32-C3** (`riscv/esp32c3db`) target running under **RTEMS 7.0 (`main`)** in AtomVM.

---

## 1. Executive Summary & Status

The AtomVM RTEMS port for ESP32-C3 currently supports:
- **Direct-boot flash execution** (`AtomVM-esp32c3db.bin`, `.rodata` execution out of mapped SPI flash at `0x3c000000` to preserve internal SRAM).
- **Core OS & Memory Sizing**: Scaled RTEMS task stacks (~64 KiB init task, ~224 KiB free SRAM for AtomVM process heaps).
- **UART / Console**: Termios integration on UART0.
- **Wi-Fi & Networking**: Station mode association (`esp32-wifi-lib`), lwIP dual-stack integration, DHCP client, loopback, UDP, TCP client/server, passive sockets, and DNS resolver.
- **Time Synchronization**: SNTP RFC 4330 client updating POSIX `clock_settime(CLOCK_REALTIME)`.
- **Peripherals**: Native GPIO (input, output, open-drain, pull-up/down, IRQ 16 interrupt handler), I2C (`/dev/i2c-0`), and SPI (`/dev/spi-0` on GPSPI2).
- **Automated Emulation & CI**: Fast test runner under `esp-emulator` (`esp-emu v0.49.0`).

Below are the key missing subsystems, prioritized by implementation feasibility and impact.

---

## 2. Hardware Support Matrix

| Subsystem / Peripheral | ESP32-C3 Hardware | Current RTEMS State | Implementation Difficulty | Priority |
| :--- | :--- | :--- | :--- | :--- |
| **RTC Slow/Fast Memory** | 8 KiB SRAM at `0x5000_0000`, retained across deep sleep | **Supported** (`esp:rtc_slow_*` & `atomvm_rtems:rtc_slow_*`) | Low (direct register mapping) | **Done** |
| **RTC 64-bit Timer** | Continuous hardware counter in `RTC_CNTL` (`0x6000_8000`) | **Missing** (clock resets to 1988 on reboot until SNTP) | Low (MMIO register read/sync) | **High** |
| **Reset Reason & Wakeup** | `RTC_CNTL_RESET_CAUSE_PROCPU` (`0x6000_8034`) | **Missing** (`esp:reset_reason/0` unsupported) | Low (read MMIO register) | **High** |
| **Deep / Light Sleep** | `RTC_CNTL` power domain state machine, timer & GPIO wakeup | **Missing** (`esp:deep_sleep/1` unsupported) | Medium (register sequencing) | Medium |
| **MbedTLS / TLS / Crypto** | Hardware SHA, AES, RSA accelerators; software TLS | **Disabled** (`Could NOT find MbedTLS`) | Medium (cross-compile for `riscv-rtems7`) | **High** |
| **Hardware RNG** | True RNG at `0x6002_60B0` | Used internally by Wi-Fi, not exposed to Erlang `crypto` | Low (wire into `/dev/urandom` or crypto NIF) | Medium |
| **LEDC (Hardware PWM)** | 6 independent channels with configurable duty & frequency | **Missing** (`ledc.erl` unsupported on RTEMS) | Medium (MMIO register programming) | Medium |
| **SAR ADC** | 12-bit ADC, up to 6 channels on GPIO 0–4 | **Missing** (`esp_adc.erl` unsupported on RTEMS) | Medium (calibration & APB registers) | Medium |
| **NVS / Flash Storage** | SPI Flash partitions, wear-leveling, LittleFS | **Missing** (only direct-boot read-only flash mapped) | High (SPI flash controller driver) | Low |
| **Watchdog Timers (WDT)** | MWDT0, MWDT1, and RTC Watchdog (RWDT) | **Missing** (`esp:task_wdt_*` unsupported) | Low-Medium (timer interrupt or register reset) | Low |
| **Bluetooth LE (BLE 5.0)** | 2.4 GHz BLE controller in ROM & hardware baseband | **Missing** (no BLE host stack ported to RTEMS) | High (requires NimBLE / Zephyr controller glue) | Low |

---

## 3. Deep-Dive: RTC Subsystem & Power Management

### 3.1 RTC Slow/Fast Memory (`0x5000_0000`) — **Implemented**
- **Status**: **Done**.
- **Implementation**:
  - `src/platforms/rtems/src/lib/rtc_slow_nif.c` maps `struct esp32c3_rtc_slow_storage` directly to `0x50000000`.
  - Implements standard IEEE 802.3 CRC32 verification with magic initialization seed `0x55555555`.
  - Exposes `esp:rtc_slow_get_binary/0` / `esp:rtc_slow_set_binary/1` as well as `atomvm_rtems:rtc_slow_get_binary/0` / `atomvm_rtems:rtc_slow_set_binary/1`.
  - Verified under `esp-emulator` simulation (`test_boot.erl`).

### 3.2 Hardware RTC Timer & Offline Time Preservation
- **Background**:
  `RTC_CNTL_TIME0_REG` (`0x6000_800C`) and `RTC_CNTL_TIME1_REG` (`0x6000_8010`) latch a 64-bit slow-clock counter when `RTC_CNTL_TIME_UPDATE_REG` is written. This counter runs continuously even in deep sleep.
- **Action Items**:
  1. Create an RTC timekeeper module:
     - On SNTP sync, record the current UTC epoch along with the latched 64-bit RTC counter in RTC memory (`0x5000_0000`).
     - On boot / reset, read the RTC memory structure. If valid, compute:
       $$\text{Current UTC} = \text{Saved UTC} + \frac{\text{Current RTC Ticks} - \text{Saved RTC Ticks}}{F_{\text{RTC}}}$$
       and immediately call `clock_settime(CLOCK_REALTIME)` before any network connection is established.
  2. Expose `atomvm_rtems:get_rtc_time/0` or `esp:timer_get_time/0`.

### 3.3 Reset Reason & Wakeup Causes
- **Background**:
  Register `RTC_CNTL_RESET_CAUSE_PROCPU` (`0x6000_8034`) encodes:
  - `0x01`: Power-on reset (cold boot)
  - `0x03`: Software reset (`esp_restart()`)
  - `0x05`: Deep-sleep wakeup
  - `0x07` / `0x08`: Main watchdog resets (MWDT0 / MWDT1)
  - `0x09`: RTC watchdog reset (RWDT)
  - `0x0F`: Brownout reset
- **Action Items**:
  1. Implement `esp:reset_reason/0` returning atoms matching AtomVM conventions:
     - `chip_reset`, `software_reset`, `deep_sleep_wakeup`, `watchdog_reset`, `brownout_reset`, `unknown`.
  2. Implement `esp:sleep_get_wakeup_cause/0` reading `RTC_CNTL_WAKEUP_STATE_REG`.

### 3.4 Deep Sleep & Light Sleep
- **Background**:
  Entering deep sleep powers off the CPU and digital peripherals, keeping only the RTC domain alive. Wakeup can be triggered by timer or RTC GPIO.
- **Action Items**:
  1. Implement `esp:deep_sleep/1` (sleep milliseconds) by programming `RTC_CNTL_SLP_TIMER0/1_REG` and triggering the sleep state machine in `RTC_CNTL_STATE0_REG`.
  2. Implement `esp:deep_sleep_enable_gpio_wakeup/2`.

---

## 4. Cryptography, Security & TLS

### 4.1 MbedTLS 3.x Cross-Compilation
- **Background**:
  AtomVM has complete Erlang modules for `ssl` and `crypto`, but disabled on RTEMS ESP32-C3 because MbedTLS was not compiled for `riscv-rtems7`.
- **Action Items**:
  1. Update `src/platforms/rtems/docker/Dockerfile.rtems7-esp32c3` to clone and build `mbedtls` (v3.6.x LTS) with CMake:
     ```bash
     cmake -B build-mbedtls -S mbedtls \
       -DCMAKE_TOOLCHAIN_FILE=/opt/rtems/7/share/rtems/cmake/riscv-rtems7-esp32c3db.cmake \
       -DCMAKE_INSTALL_PREFIX=/opt/rtems/7/riscv-rtems7/esp32c3db \
       -DENABLE_TESTING=OFF -DENABLE_PROGRAMS=OFF
     ```
  2. Verify that CMake finds `MbedTLS` and automatically enables `ssl` and `crypto`.
  3. Validate TLS client handshake (`ahttp_client` over HTTPS) in simulation.

### 4.2 Hardware RNG Driver
- **Background**:
  ESP32-C3 contains a True Random Number Generator at `0x6002_60B0` (active when Wi-Fi/ADC is clocked).
- **Action Items**:
  1. Register `/dev/urandom` in IMFS or integrate with AtomVM's `sys_crypto_random_bytes()`.
  2. Expose `crypto:strong_rand_bytes/1` without requiring external entropy daemons.

---

## 5. Additional Hardware Peripherals

### 5.1 LEDC (Hardware PWM)
- **Background**:
  6 independent PWM channels driven by APB clock with hardware fading support.
- **Action Items**:
  1. Implement `ledc_driver.c` for RTEMS configuring `LEDC_CHn_CONF0_REG`, `LEDC_CHn_HPOINT_REG`, `LEDC_CHn_DUTY_REG`, and `LEDC_TIMERn_CONF_REG`.
  2. Support AtomVM's standard `ledc.erl` interface (`set_pin`, `set_frequency`, `set_duty`).

### 5.2 SAR ADC (12-bit Analog-to-Digital Converter)
- **Background**:
  ADC1 supports up to 6 channels (GPIO 0–4).
- **Action Items**:
  1. Implement `adc_driver.c` configuring `APB_SARADC_SAR1_PATT_TAB_REG` and reading `APB_SARADC_SAR1_DATA_REG`.
  2. Support AtomVM's `esp_adc.erl` interface (`adc:read/1`, `adc:read_mv/1` with factory eFuse calibration).

### 5.3 Non-Volatile Flash Storage & NVS
- **Background**:
  Direct flash boot runs from read-only SPI flash. To support file storage or persistent key-value configuration (`esp:nvs_*`), an SPI flash driver is needed.
- **Action Items**:
  1. Implement SPI flash erase/write routines using ROM SPI flash functions (`esp32c3.rom.api.ld`).
  2. Mount LittleFS or FATFS partition for Erlang file operations (`file:read_file/1`, `file:write_file/2`).
  3. Implement `esp:nvs_*` key-value primitives.

### 5.4 Watchdog Timers (WDT)
- **Background**:
  Provides hardware fault-recovery if the BEAM scheduler hangs or an ISR starves the system.
- **Action Items**:
  1. Implement `esp:task_wdt_init/1`, `esp:task_wdt_reset_user/1`, and `esp:task_wdt_deinit/0` using MWDT0.

---

## 6. Implementation Roadmap & Recommended Next Steps

```mermaid
flowchart TD
    subgraph Phase 1: RTC & State Retention
        A["1.1 RTC Slow Memory (0x50000000)<br>esp:rtc_slow_get/set_binary"] --> B["1.2 Reset Reason & Wakeup<br>esp:reset_reason"]
        B --> C["1.3 RTC Timer Sync<br>Preserve UTC time across resets"]
    end

    subgraph Phase 2: Security & Networking
        D["2.1 Cross-compile MbedTLS 3.x<br>for riscv-rtems7"] --> E["2.2 HTTPS & TLS Sockets<br>Enable ssl & crypto modules"]
        E --> F["2.3 Hardware RNG<br>crypto:strong_rand_bytes"]
    end

    subgraph Phase 3: Hardware Control
        G["3.1 LEDC (PWM Driver)<br>ledc:set_duty / set_freq"] --> H["3.2 SAR ADC (12-bit)<br>esp_adc:read"]
        H --> I["3.3 Deep Sleep Entry<br>esp:deep_sleep/1"]
    end

    subgraph Phase 4: Storage & Flash
        J["4.1 ROM Flash Write/Erase"] --> K["4.2 NVS Key-Value Storage"]
        K --> L["4.3 LittleFS Flash Mount"]
    end

    Phase 1 --> Phase 2
    Phase 2 --> Phase 3
    Phase 3 --> Phase 4
```
