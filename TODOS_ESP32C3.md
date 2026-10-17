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
| **RTC 64-bit Timer & SNTP Sync** | Continuous hardware counter in `RTC_CNTL` (`0x6000_8000`) & timekeeper | **Supported** (Latched counter in `0x6000_800C/10/14`, timekeeper at `0x5000_1000`, `esp:timer_get_time/0`) | Low (MMIO register read/sync) | **Done** |
| **Reset Reason & Wakeup** | `RTC_CNTL_RESET_CAUSE_PROCPU` (`0x6000_8034`) | **Supported** (`esp:reset_reason/0` and `atomvm_rtems:reset_reason/0`) | Low (ROM reset reason decode) | **Done** |
| **Deep / Light Sleep** | `RTC_CNTL` power domain state machine, timer & GPIO wakeup | **Supported** (`esp:deep_sleep/*`, `esp:light_sleep/0`, `esp:sleep_enable_*`, `gpio:deep_sleep_hold_*`) | Medium (register sequencing) | **Done** |
| **MbedTLS / TLS / Crypto** | Hardware SHA, AES, RSA accelerators; software TLS | **Supported** (MbedTLS 3.6.3 LTS installed into `riscv-rtems7/esp32c3db`, `crypto` and `ssl` NIFs enabled) | Medium | **Done** |
| **Hardware RNG** | True RNG at `0x6002_60B0` | **Supported** (Wired to MbedTLS entropy source & `crypto:strong_rand_bytes/1`) | Low | **Done** |
| **LEDC (Hardware PWM)** | 6 independent channels with configurable duty & frequency | **Supported** (`ledc:*`, `Elixir.LEDC:*`, 17 NIFs) | Medium (MMIO register programming) | **Done** |
| **SAR ADC** | 12-bit ADC, up to 6 channels on GPIO 0–4 | **Supported** (`esp_adc:*`, `Elixir.Esp.ADC:*`, gen_server) | Medium (calibration & APB registers) | **Done** |
| **NVS / Flash Storage** | SPI Flash partitions, wear-leveling, JFFS2 flash filesystem | **Supported** (ROM SPI flash driver, log-structured NVS partition at `0x3C0000`, JFFS2 filesystem mounted at `/flash`, `esp:flash_*`, `esp:nvs_*`, `file:*`) | Medium-High (ROM API & JFFS2 callbacks) | **Done** |
| **Watchdog Timers (WDT)** | MWDT0, MWDT1, and RTC Watchdog (RWDT) | **Supported** (Task WDT, MWDT0, RWDT, `esp:task_wdt_*`, `esp:wdt_feed/0`) | Low-Medium (timer interrupt or register reset) | **Done** |
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

### 3.2 Hardware RTC Timer & Offline Time Preservation — **Implemented**
- **Status**: **Done**.
- **Implementation**:
  - `esp32c3_rtc_get_counter()` commands the hardware update latch via `RTC_CNTL_TIME_UPDATE_REG` (`0x6000_800C`) and reads `RTC_CNTL_TIME0_REG` (`0x6000_8010`) / `RTC_CNTL_TIME1_REG` (`0x6000_8014`).
  - Packed `struct esp32c3_rtc_timekeeper` stored at dedicated RTC slow SRAM `0x5000_1000` (separate from user binary storage at `0x5000_0000`) protected with CRC32.
  - Automatically recorded upon successful SNTP sync in `sntp.c` (`esp32c3_rtc_timekeeper_record()`).
  - Automatically restored on early boot in `main.c` `Init()` via `esp32c3_rtc_timekeeper_restore()`: computes elapsed offline time $\Delta t = \frac{\Delta\text{ticks}}{F_{\text{RTC}}}$ and calls `clock_settime(CLOCK_REALTIME)` so the system clock is immediately valid before networking connects.
  - Exposes `esp:timer_get_time/0`, `esp:rtc_get_time_us/0`, `atomvm_rtems:get_rtc_counter/0`, `atomvm_rtems:rtc_timekeeper_record/2`, `atomvm_rtems:rtc_timekeeper_restore/0`, and `atomvm_rtems:rtc_timekeeper_status/0`.
  - Verified under `esp-emulator` simulation (`test_boot.erl`).

### 3.3 Reset Reason & Wakeup Causes — **Implemented**
- **Status**: **Done** (`esp:reset_reason/0` supported).
- **Implementation**:
  - `rtc_get_reset_reason(0)` reads ROM reset reason vector and decodes hardware status into standard AtomVM atoms: `esp_rst_poweron`, `esp_rst_sw`, `esp_rst_deepsleep`, `esp_rst_wdt`, `esp_rst_brownout`, and `esp_rst_unknown`.
  - Exposes `esp:reset_reason/0` and `atomvm_rtems:reset_reason/0`.
  - Verified under `esp-emulator` simulation (`test_boot.erl`).

### 3.4 Deep Sleep & Light Sleep — **Implemented**
- **Status**: **Done**.
- **Implementation**:
  - `esp32c3_sleep_enable_timer_wakeup(uint64_t usecs)` programs `RTC_CNTL_SLP_TIMER0_REG` (`0x6000_8004`), `RTC_CNTL_SLP_TIMER1_REG` (`0x6000_8008`), and sets `RTC_TIMER_TRIG_EN` in `RTC_CNTL_WAKEUP_STATE_REG` (`0x6000_803C`).
  - `esp32c3_deep_sleep_enable_gpio_wakeup(uint64_t mask, int mode)` configures RTC GPIO pins 0–5 via `GPIO_PINn_REG` (`0x6000_4074`) and enables `RTC_GPIO_TRIG_EN` in `RTC_CNTL_WAKEUP_STATE_REG`.
  - `esp32c3_deep_sleep_hold_en()` and `esp32c3_deep_sleep_hold_dis()` program `RTC_CNTL_PAD_HOLD_REG` (`0x6000_8064`) to freeze pin state during deep sleep.
  - `esp32c3_light_sleep()` puts the CPU into low power wait-for-interrupt (`wfi`) / nanosleep and restores execution, setting wakeup cause to `sleep_wakeup_timer` or `sleep_wakeup_gpio`.
  - `esp32c3_deep_sleep(uint64_t usecs)` saves the SNTP timekeeper and sleep state to RTC slow SRAM (`0x5000_1040`), programs `RTC_CNTL_STATE0_REG` (`0x6000_8018`) bit 31 (`RTC_CNTL_SLEEP_EN`), commands soft reset trigger `RTC_CNTL_OPTIONS0_REG` (`0x6000_8000`), and halts via `wfi`.
  - `esp32c3_sleep_get_wakeup_cause()` decodes the wakeup cause from ROM `rtc_get_wakeup_cause()` (`0x4000_0024`), `rtc_get_reset_reason(0)`, and RTC SRAM sleep state into standard AtomVM atoms (`sleep_wakeup_timer`, `sleep_wakeup_gpio`, `sleep_wakeup_ext0`, etc.).
  - Exposes `esp:deep_sleep/0`, `esp:deep_sleep/1`, `esp:light_sleep/0`, `esp:sleep_enable_timer_wakeup/1`, `esp:deep_sleep_enable_gpio_wakeup/2`, `esp:sleep_enable_gpio_wakeup/0`, `esp:sleep_get_wakeup_cause/0`, `esp:deep_sleep_hold_en/0`, `esp:deep_sleep_hold_dis/0`, `gpio:deep_sleep_hold_en/0`, and `gpio:deep_sleep_hold_dis/0`.
  - Verified under `esp-emulator` simulation (`test_boot.erl`).

---

## 4. Cryptography, Security & TLS — **Implemented**

### 4.1 MbedTLS 3.x Cross-Compilation
- **Status**: **Done**.
- **Implementation**:
  - Official MbedTLS 3.6.3 (`v3.6.3` LTS) cross-compiled for `riscv-rtems7` and installed into `${PREFIX}/riscv-rtems7/esp32c3db` (`libmbedcrypto.a`, `libmbedx509.a`, `libmbedtls.a`, and headers).
  - Built with `MBEDTLS_NO_PLATFORM_ENTROPY=ON`, `MBEDTLS_TIMING_C=OFF`, `MBEDTLS_NET_C=OFF`.
  - Toolchain CMake paths updated in `rtems-toolchain.cmake` (`CMAKE_PREFIX_PATH` and `CMAKE_FIND_ROOT_PATH`) to discover MbedTLS package.
  - Enabled `otp_crypto_platform.c` (`crypto:strong_rand_bytes/1`, `crypto:hash/2`, `crypto:crypto_one_time/4`) and `otp_ssl_platform.c`.
  - Added `atomvm:rand_bytes/1` and `atomvm:random/0` in RTEMS `platform_nifs.c`.

### 4.2 Hardware RNG Driver
- **Status**: **Done**.
- **Implementation**:
  - Polled ESP32-C3 hardware RNG register at `0x6002_60B0` in `sys.c`.
  - Registered as `MBEDTLS_ENTROPY_SOURCE_STRONG` source via `mbedtls_entropy_add_source()`.
  - Hooked into `sys_mbedtls_get_entropy_context_lock()` and `sys_mbedtls_get_ctr_drbg_context_lock()`.
  - Verified under `esp-emulator` simulation in `test_boot.erl` (`{crypto, strong_rand_bytes, ok}`).

---

## 5. Additional Hardware Peripherals

### 5.1 LEDC (Hardware PWM) — **Implemented**
- **Status**: **Done**.
- **Implementation**:
  - `ledc_driver.c` and `ledc_driver.h` implement MMIO hardware register control for the ESP32-C3 LEDC peripheral (`0x6001_9000`).
  - Supports 4 hardware timers (Timer 0–3) with 1–14 bit resolution and APB clock divider calculations ($\text{CLK\_DIV} = \frac{80,000,000 \cdot 256}{\text{freq\_hz} \cdot 2^{\text{duty\_res}}}$).
  - Supports 6 low-speed channels (Channel 0–5) with GPIO matrix signal routing (`LEDC_LS_SIG_OUT0_IDX` to `LEDC_LS_SIG_OUT5_IDX`, signals 45–50).
  - Direct control over peripheral clock gating and reset release via `SYSREG_PERIP_CLK_EN0_REG` / `SYSREG_PERIP_RST_EN0_REG` (bit 11), `HPOINT`, `DUTY`, and `CONF0` parameters.
  - Implements software & hardware fade sequencing (`set_fade_with_time`, `set_fade_with_step`, `fade_start`, `fade_stop`, `set_fade_time_and_start`, `set_fade_step_and_start`).
  - Implements all 17 NIFs required for standard AtomVM `ledc.erl`, `Elixir.LEDC`, and `atomvm_rtems:ledc_*`.
  - Verified under `esp-emulator` simulation (`test_boot.erl`).

### 5.2 SAR ADC (12-bit Analog-to-Digital Converter) — **Implemented**
- **Status**: **Done**.
- **Implementation**:
  - `adc_driver.c` and `adc_driver.h` implement MMIO hardware register control for the ESP32-C3 APB_SARADC peripheral (`0x6004_0000`).
  - Supports 12-bit analog sampling on ADC1 channels 0–4 (GPIO 0–4) and ADC2 channel 0 (GPIO 5).
  - Programs `SYSREG_PERIP_CLK_EN0_REG` / `SYSREG_PERIP_RST_EN0_REG` (bits 28 and 30) for peripheral clock gating and reset deassertion.
  - Programs `APB_SARADC_CLKM_CONF_REG`, `APB_SARADC_CTRL_REG`, `APB_SARADC_FSM_WAIT_REG`, and RTC power domains (`RTC_CNTL_ANA_CONF_REG` and `RTC_CNTL_SENSOR_CTRL_REG`).
  - Configures pin IO_MUX for analog input (disables digital input buffer `FUN_IE = 0`, disables pull-ups/pull-downs) and claims pin via `bsp_pin_claim`.
  - Performs one-shot conversions via `APB_SARADC_ONETIME_SAMPLE_REG` and reads digital output from `APB_SARADC_SAR1_DATA_STATUS_REG` / `SAR2`.
  - Converts raw readings to calibrated millivolts using Espressif's recommended voltage approximation for attenuations `db_0`, `db_2_5`, `db_6`, and `db_11` / `db_12`.
  - Implements resource management (`adc_unit_resource`, `adc_channel_resource`) and all standard NIFs: `esp_adc:init/0`, `esp_adc:deinit/1`, `esp_adc:acquire/4`, `esp_adc:acquire/2`, `esp_adc:release_channel/1`, `esp_adc:sample/3`, `esp_adc:sample/2` (with `Elixir.Esp.ADC` and `atomvm_rtems:adc_*` aliases).
  - Includes `esp_adc.erl` in `avm_rtems`, providing full `gen_server` convenience API (`start/0,1,2`, `read/1,2`, `stop/0,1`).
  - Verified under `esp-emulator` simulation (`test_boot.erl`).

### 5.3 Non-Volatile Flash Storage & NVS — **Implemented**
- **Status**: **Done**.
- **Implementation**:
  - `flash_driver.c` and `flash_driver.h`: Low-level SPI Flash driver using ESP32-C3 ROM APIs (`esp_rom_spiflash_read`, `esp_rom_spiflash_write`, `esp_rom_spiflash_erase_sector`). Automatically overrides ROM default 2 MiB chip size to 4 MiB (`4 * 1024 * 1024`). Handles unaligned reads and writes transparently via 4-byte aligned stack buffers. Protects flash cache coherence during erase/write by disabling local interrupts and invalidating cache via `Cache_Invalidate_ICache_All()`.
  - `nvs_driver.c` and `nvs_driver.h`: Log-structured key-value store in dedicated flash partition `0x003C0000` (128 KiB). Supports namespace isolation, key updates, wear-leveling in-place invalidation (`0xFF` -> `0xAA` -> `0x00`), CRC32 entry verification, and reformatting.
  - `flash_fs.c` and `flash_fs.h`: RTEMS JFFS2 flash filesystem control callbacks (`read`, `write`, `erase`) mapped to flash partition `0x003E0000` (128 KiB), mounted at `/flash`. Supports POSIX Erlang file operations (`file:read_file/1`, `file:write_file/2`).
  - Standard NIF APIs:
    - Flash: `esp:flash_get_size/0`, `esp:flash_read/2`, `esp:flash_write/2`, `esp:flash_erase_sector/1`, `esp:flash_erase_range/2` (with `atomvm_rtems:` aliases).
    - NVS: `esp:nvs_fetch_binary/2`, `esp:nvs_get_binary/1,2,3`, `esp:nvs_put_binary/3`, `esp:nvs_set_binary/2,3`, `esp:nvs_erase_key/1,2`, `esp:nvs_erase_all/0,1`, `esp:nvs_reformat/0` (with `atomvm_rtems:` aliases).
    - Filesystem: `esp:flash_fs_mount/0,1`, `esp:flash_fs_unmount/0,1`, `esp:flash_fs_is_mounted/0`, `file:read_file/1`, `file:write_file/2`.
  - Verified under `esp-emulator` simulation (`test_boot.erl`).

### 5.4 Watchdog Timers (WDT) — **Implemented**
- **Status**: **Done**.
- **Implementation**:
  - `wdt.c` implements the Task Watchdog Timer (TWDT) subsystem and hardware watchdog interface for ESP32-C3 under RTEMS 7.
  - Hardware MWDT0 programmed via `TIMG0_WDTCONFIG0..3_REG` (`0x6001_F048`), `TIMG0_WDTFEED_REG` (`0x6001_F060`), and write protect key `0x50D83AA1`.
  - Hardware RWDT (RTC Watchdog) fed via `RTC_CNTL_WDTFEED` (`0x6000_80A0`) and `RTC_CNTL_WDTWPROTECT` (`0x6000_80A4`).
  - RTEMS periodic timer runs in background to verify active user feed timestamps; triggers CPU reset via `RTC_CNTL_OPTIONS0_REG` if `trigger_panic` is true or logs warnings if false.
  - Automatically initializes with default `{5000, 0, false}` configuration at startup for seamless compatibility with standard ESP-IDF TWDT behavior.
  - Exposes `esp:task_wdt_init/1`, `esp:task_wdt_reconfigure/1`, `esp:task_wdt_deinit/0`, `esp:task_wdt_add_user/1`, `esp:task_wdt_reset_user/1`, `esp:task_wdt_delete_user/1`, and `esp:wdt_feed/0` (with `atomvm_rtems:` aliases).
  - Verified under `esp-emulator` simulation (`test_boot.erl`).

---

## 6. Implementation Roadmap & Recommended Next Steps

```mermaid
flowchart TD
    subgraph Phase 1: RTC & Power Management (Completed)
        A["1.1 RTC Slow Memory (0x50000000)<br>esp:rtc_slow_get/set_binary"] --> B["1.2 Reset Reason & Wakeup<br>esp:reset_reason"]
        B --> C["1.3 RTC Timer Sync<br>Preserve UTC time across resets"]
        C --> D["1.4 Deep / Light Sleep<br>esp:deep_sleep, esp:light_sleep"]
    end

    subgraph Phase 2: Security & Networking (Completed)
        E["2.1 Cross-compile MbedTLS 3.x<br>for riscv-rtems7"] --> F["2.2 HTTPS & TLS Sockets<br>Enable ssl & crypto modules"]
        F --> G["2.3 Hardware RNG<br>crypto:strong_rand_bytes"]
    end

    subgraph Phase 3: Hardware Control (Completed)
        H["3.1 LEDC (PWM Driver)<br>ledc:set_duty / set_freq"] --> I["3.2 SAR ADC (12-bit)<br>esp_adc:read / sample"]
    end

    subgraph Phase 4: Storage & Flash (Completed)
        J["4.1 ROM Flash Write/Erase<br>esp:flash_*"] --> K["4.2 NVS Key-Value Storage<br>esp:nvs_*"]
        K --> L["4.3 JFFS2 Flash Mount (/flash)<br>file:read_file / write_file"]
    end

    subgraph Phase 5: Bluetooth Low Energy
        M["5.1 BLE Controller Glue"] --> N["5.2 NimBLE / Host Stack"]
    end

    Phase 1 --> Phase 2
    Phase 2 --> Phase 3
    Phase 3 --> Phase 4
    Phase 4 --> Phase 5
```

