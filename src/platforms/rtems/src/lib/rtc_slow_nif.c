/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include "rtc.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <rtems.h>

#include <atom.h>
#include <defaultatoms.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <term.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "rtems_sys.h"

#ifndef CONFIG_AVM_RTC_SLOW_MAX_SIZE
#define CONFIG_AVM_RTC_SLOW_MAX_SIZE 4086
#endif

// Ensure checksum is incorrect when memory is zeroed.
#define RTC_SLOW_CRC32_MAGIC 0x55555555U

#if defined(RTEMS_BOARD_ESP32C3)
#define ESP32C3_RTC_RAM_BASE        0x50000000U
#define ESP32C3_RTC_TIMEKEEPER_BASE 0x50001000U
#define ESP32C3_RTC_SLEEP_BASE      0x50001040U
#define RTC_TIMEKEEPER_MAGIC        0x54494D45U /* "TIME" */
#define RTC_SLEEP_MAGIC             0x534C4550U /* "SLEP" */
#define RTC_SLOW_CLK_FREQ_DEFAULT   136000U

#define ESP32C3_RTC_CNTL_BASE         0x60008000U
#define ESP32C3_RTC_CNTL_OPTIONS0     (ESP32C3_RTC_CNTL_BASE + 0x00U)
#define ESP32C3_RTC_CNTL_SLP_TIMER0   (ESP32C3_RTC_CNTL_BASE + 0x04U)
#define ESP32C3_RTC_CNTL_SLP_TIMER1   (ESP32C3_RTC_CNTL_BASE + 0x08U)
#define ESP32C3_RTC_CNTL_TIME_UPDATE  (ESP32C3_RTC_CNTL_BASE + 0x0CU)
#define ESP32C3_RTC_CNTL_TIME0        (ESP32C3_RTC_CNTL_BASE + 0x10U)
#define ESP32C3_RTC_CNTL_TIME1        (ESP32C3_RTC_CNTL_BASE + 0x14U)
#define ESP32C3_RTC_CNTL_STATE0       (ESP32C3_RTC_CNTL_BASE + 0x18U)
#define ESP32C3_RTC_CNTL_RESET_STATE  (ESP32C3_RTC_CNTL_BASE + 0x34U)
#define ESP32C3_RTC_CNTL_WAKEUP_STATE (ESP32C3_RTC_CNTL_BASE + 0x3CU)
#define ESP32C3_RTC_CNTL_PAD_HOLD     (ESP32C3_RTC_CNTL_BASE + 0x64U)

#define ESP32C3_GPIO_REG_BASE         0x60004000U
#define ESP32C3_GPIO_PIN0_REG         (ESP32C3_GPIO_REG_BASE + 0x74U)

extern int rtc_get_reset_reason(int cpu_no);
extern uint32_t rtc_get_wakeup_cause(void);

enum esp32c3_wakeup_cause {
    ESP32C3_WAKEUP_UNDEFINED = 0,
    ESP32C3_WAKEUP_EXT0      = 1,
    ESP32C3_WAKEUP_EXT1      = 2,
    ESP32C3_WAKEUP_TIMER     = 3,
    ESP32C3_WAKEUP_TOUCHPAD  = 4,
    ESP32C3_WAKEUP_ULP       = 5,
    ESP32C3_WAKEUP_GPIO      = 6,
    ESP32C3_WAKEUP_UART      = 7,
    ESP32C3_WAKEUP_WIFI      = 8,
    ESP32C3_WAKEUP_COCPU     = 9,
    ESP32C3_WAKEUP_BT        = 10,
};

struct esp32c3_rtc_slow_storage {
    uint32_t checksum;
    uint16_t size;
    uint16_t reserved;
    uint8_t data[CONFIG_AVM_RTC_SLOW_MAX_SIZE];
};

struct __attribute__((packed)) esp32c3_rtc_timekeeper {
    uint32_t magic;
    uint32_t rtc_clk_freq;
    uint64_t sntp_sec;
    uint64_t sntp_usec;
    uint64_t rtc_counter;
    uint32_t checksum;
};

struct __attribute__((packed)) esp32c3_rtc_sleep_state {
    uint32_t magic;
    uint32_t last_wakeup_cause;
    uint64_t timer_wakeup_ticks;
    uint32_t gpio_wakeup_mask;
    uint32_t gpio_wakeup_mode;
    uint32_t checksum;
};

static uint64_t s_timer_wakeup_us = 0;
static uint32_t s_gpio_wakeup_mask = 0;
static int s_gpio_wakeup_mode = 0;
static uint32_t s_light_sleep_wakeup_cause = ESP32C3_WAKEUP_UNDEFINED;
#endif

static const char *const esp_rst_unknown_atom   = "\xF" "esp_rst_unknown";
static const char *const esp_rst_poweron_atom   = "\xF" "esp_rst_poweron";
static const char *const esp_rst_ext_atom       = "\xB" "esp_rst_ext";
static const char *const esp_rst_sw_atom        = "\xA" "esp_rst_sw";
static const char *const esp_rst_panic_atom     = "\xD" "esp_rst_panic";
static const char *const esp_rst_int_wdt_atom   = "\xF" "esp_rst_int_wdt";
static const char *const esp_rst_task_wdt_atom  = "\x10" "esp_rst_task_wdt";
static const char *const esp_rst_wdt_atom       = "\xB" "esp_rst_wdt";
static const char *const esp_rst_deepsleep_atom = "\x11" "esp_rst_deepsleep";
static const char *const esp_rst_brownout_atom  = "\x10" "esp_rst_brownout";

static const char *const sleep_wakeup_ext0_atom     = "\x11" "sleep_wakeup_ext0";
static const char *const sleep_wakeup_ext1_atom     = "\x11" "sleep_wakeup_ext1";
static const char *const sleep_wakeup_timer_atom    = "\x12" "sleep_wakeup_timer";
static const char *const sleep_wakeup_touchpad_atom = "\x15" "sleep_wakeup_touchpad";
static const char *const sleep_wakeup_ulp_atom      = "\x10" "sleep_wakeup_ulp";
static const char *const sleep_wakeup_gpio_atom     = "\x11" "sleep_wakeup_gpio";
static const char *const sleep_wakeup_uart_atom     = "\x11" "sleep_wakeup_uart";
static const char *const sleep_wakeup_wifi_atom     = "\x11" "sleep_wakeup_wifi";
static const char *const sleep_wakeup_cocpu_atom    = "\x12" "sleep_wakeup_cocpu";
static const char *const sleep_wakeup_bt_atom       = "\xF" "sleep_wakeup_bt";

static const char *const not_synced_atom        = "\xA" "not_synced";
static const char *const sntp_sec_atom          = "\x8" "sntp_sec";
static const char *const sntp_usec_atom         = "\x9" "sntp_usec";
static const char *const rtc_counter_atom       = "\xB" "rtc_counter";
static const char *const elapsed_sec_atom       = "\xB" "elapsed_sec";

static inline term make_tuple2(Context *ctx, term t1, term t2)
{
    term t = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(t, 0, t1);
    term_put_tuple_element(t, 1, t2);
    return t;
}

static uint32_t rtc_crc32_le(uint32_t crc, const uint8_t *buf, size_t len)
{
    crc = ~crc;
    for (size_t i = 0; i < len; i++) {
        crc ^= buf[i];
        for (int j = 0; j < 8; j++) {
            crc = (crc >> 1) ^ (0xEDB88320u & -(crc & 1));
        }
    }
    return ~crc;
}

#if defined(RTEMS_BOARD_ESP32C3)
uint64_t esp32c3_rtc_get_counter(void)
{
    volatile uint32_t *time_upd = (volatile uint32_t *) ESP32C3_RTC_CNTL_TIME_UPDATE;
    volatile uint32_t *time0 = (volatile uint32_t *) ESP32C3_RTC_CNTL_TIME0;
    volatile uint32_t *time1 = (volatile uint32_t *) ESP32C3_RTC_CNTL_TIME1;

    *time_upd = (1u << 31);
    for (int i = 0; i < 100; i++) {
        if (*time_upd & (1u << 30)) {
            break;
        }
    }

    uint32_t low = *time0;
    uint32_t high = *time1;
    uint64_t rtc_ticks = ((uint64_t) high << 32) | (uint64_t) low;

    // In emulation environments where the hardware RTC timer crystal is unclocked,
    // supplement with monotonic time scaled to slow clock frequency (136 kHz).
    if (rtc_ticks == 0) {
        struct timespec ts;
        rtems_clock_get_monotonic(&ts);
        uint64_t us = (uint64_t) ts.tv_sec * 1000000ULL + (uint64_t) ts.tv_nsec / 1000ULL;
        rtc_ticks = (us * RTC_SLOW_CLK_FREQ_DEFAULT) / 1000000ULL;
    }
    return rtc_ticks;
}

void esp32c3_rtc_timekeeper_record(const struct timeval *tv)
{
    if (tv == NULL) {
        return;
    }
    uint64_t rtc_cnt = esp32c3_rtc_get_counter();
    struct esp32c3_rtc_timekeeper tk;
    tk.magic = RTC_TIMEKEEPER_MAGIC;
    tk.rtc_clk_freq = RTC_SLOW_CLK_FREQ_DEFAULT;
    tk.sntp_sec = (uint64_t) tv->tv_sec;
    tk.sntp_usec = (uint64_t) tv->tv_usec;
    tk.rtc_counter = rtc_cnt;
    tk.checksum = rtc_crc32_le(RTC_SLOW_CRC32_MAGIC, (const uint8_t *) &tk,
                               sizeof(tk) - sizeof(tk.checksum));

    volatile struct esp32c3_rtc_timekeeper *storage =
        (volatile struct esp32c3_rtc_timekeeper *) ESP32C3_RTC_TIMEKEEPER_BASE;
    memcpy((void *) (uintptr_t) storage, &tk, sizeof(tk));
    fprintf(stdout, "RTC: Saved SNTP sync (%llu s, %llu us, rtc_ticks=%llu) to RTC slow memory\n",
            (unsigned long long) tk.sntp_sec, (unsigned long long) tk.sntp_usec, (unsigned long long) tk.rtc_counter);
}

bool esp32c3_rtc_timekeeper_restore(void)
{
    volatile struct esp32c3_rtc_timekeeper *storage =
        (volatile struct esp32c3_rtc_timekeeper *) ESP32C3_RTC_TIMEKEEPER_BASE;

    struct esp32c3_rtc_timekeeper tk;
    memcpy(&tk, (const void *) (uintptr_t) storage, sizeof(tk));

    if (tk.magic != RTC_TIMEKEEPER_MAGIC) {
        return false;
    }
    uint32_t expected_crc = tk.checksum;
    uint32_t computed_crc = rtc_crc32_le(RTC_SLOW_CRC32_MAGIC, (const uint8_t *) &tk,
                                         sizeof(tk) - sizeof(tk.checksum));
    if (computed_crc != expected_crc) {
        return false;
    }
    if (tk.rtc_clk_freq == 0) {
        tk.rtc_clk_freq = RTC_SLOW_CLK_FREQ_DEFAULT;
    }

    uint64_t now_ticks = esp32c3_rtc_get_counter();
    if (now_ticks < tk.rtc_counter) {
        return false;
    }

    uint64_t elapsed_ticks = now_ticks - tk.rtc_counter;
    uint64_t elapsed_sec = elapsed_ticks / tk.rtc_clk_freq;
    uint64_t elapsed_rem = elapsed_ticks % tk.rtc_clk_freq;
    uint64_t elapsed_usec = (elapsed_rem * 1000000ULL) / tk.rtc_clk_freq;

    uint64_t total_usec = tk.sntp_usec + elapsed_usec;
    time_t sec = (time_t) (tk.sntp_sec + elapsed_sec + (total_usec / 1000000ULL));
    long nsec = (long) (total_usec % 1000000ULL) * 1000L;

    struct timespec ts;
    ts.tv_sec = sec;
    ts.tv_nsec = nsec;
    clock_settime(CLOCK_REALTIME, &ts);

    fprintf(stdout, "RTC: Restored system time from RTC slow memory: %llu s (elapsed %llu s)\n",
            (unsigned long long) sec, (unsigned long long) elapsed_sec);
    return true;
}

int esp32c3_sleep_enable_timer_wakeup(uint64_t time_in_us)
{
    s_timer_wakeup_us = time_in_us;
    uint64_t ticks = (time_in_us * RTC_SLOW_CLK_FREQ_DEFAULT) / 1000000ULL;
    uint64_t now_ticks = esp32c3_rtc_get_counter();
    uint64_t target_ticks = now_ticks + ticks;

    volatile uint32_t *slp_timer0 = (volatile uint32_t *) ESP32C3_RTC_CNTL_SLP_TIMER0;
    volatile uint32_t *slp_timer1 = (volatile uint32_t *) ESP32C3_RTC_CNTL_SLP_TIMER1;
    volatile uint32_t *wakeup_state = (volatile uint32_t *) ESP32C3_RTC_CNTL_WAKEUP_STATE;

    *slp_timer0 = (uint32_t) target_ticks;
    *slp_timer1 = ((uint32_t) (target_ticks >> 32) & 0xFFFF) | (1u << 16);
    *wakeup_state |= (1u << 3); // RTC_TIMER_TRIG_EN

    return 0;
}

int esp32c3_deep_sleep_enable_gpio_wakeup(uint64_t mask, int mode)
{
    s_gpio_wakeup_mask = (uint32_t) mask;
    s_gpio_wakeup_mode = mode;

    volatile uint32_t *wakeup_state = (volatile uint32_t *) ESP32C3_RTC_CNTL_WAKEUP_STATE;
    *wakeup_state |= (1u << 2); // RTC_GPIO_TRIG_EN

    for (int pin = 0; pin < 6; pin++) {
        if (mask & (1u << pin)) {
            volatile uint32_t *pin_reg = (volatile uint32_t *) (ESP32C3_GPIO_PIN0_REG + (pin * 4));
            uint32_t val = *pin_reg;
            val |= (1u << 10); // GPIO_PIN_WAKEUP_ENABLE
            val &= ~(0x7 << 7);
            val |= ((mode == 0 ? 4 : 5) << 7);
            *pin_reg = val;
        }
    }
    return 0;
}

int esp32c3_sleep_enable_gpio_wakeup(void)
{
    volatile uint32_t *wakeup_state = (volatile uint32_t *) ESP32C3_RTC_CNTL_WAKEUP_STATE;
    *wakeup_state |= (1u << 2); // RTC_GPIO_TRIG_EN
    return 0;
}

void esp32c3_deep_sleep_hold_en(void)
{
    volatile uint32_t *pad_hold = (volatile uint32_t *) ESP32C3_RTC_CNTL_PAD_HOLD;
    *pad_hold |= (1u << 31);
}

void esp32c3_deep_sleep_hold_dis(void)
{
    volatile uint32_t *pad_hold = (volatile uint32_t *) ESP32C3_RTC_CNTL_PAD_HOLD;
    *pad_hold &= ~(1u << 31);
}

int esp32c3_light_sleep(void)
{
    if (s_timer_wakeup_us > 0) {
        struct timespec req;
        req.tv_sec = (time_t) (s_timer_wakeup_us / 1000000ULL);
        req.tv_nsec = (long) ((s_timer_wakeup_us % 1000000ULL) * 1000ULL);
        nanosleep(&req, NULL);
        s_light_sleep_wakeup_cause = ESP32C3_WAKEUP_TIMER;
    } else if (s_gpio_wakeup_mask > 0) {
        rtems_interval ticks = rtems_clock_get_ticks_per_second() / 10;
        rtems_task_wake_after(ticks > 0 ? ticks : 1);
        s_light_sleep_wakeup_cause = ESP32C3_WAKEUP_GPIO;
    } else {
        asm volatile("wfi");
        s_light_sleep_wakeup_cause = ESP32C3_WAKEUP_UNDEFINED;
    }
    return 0;
}

void esp32c3_deep_sleep(uint64_t time_in_us)
{
    uint32_t cause = ESP32C3_WAKEUP_UNDEFINED;
    if (time_in_us > 0) {
        esp32c3_sleep_enable_timer_wakeup(time_in_us);
        cause = ESP32C3_WAKEUP_TIMER;
    } else if (s_gpio_wakeup_mask > 0) {
        cause = ESP32C3_WAKEUP_GPIO;
    }

    struct timeval tv;
    gettimeofday(&tv, NULL);
    esp32c3_rtc_timekeeper_record(&tv);

    struct esp32c3_rtc_sleep_state ss;
    memset(&ss, 0, sizeof(ss));
    ss.magic = RTC_SLEEP_MAGIC;
    ss.last_wakeup_cause = cause;
    ss.timer_wakeup_ticks = s_timer_wakeup_us;
    ss.gpio_wakeup_mask = s_gpio_wakeup_mask;
    ss.gpio_wakeup_mode = (uint32_t) s_gpio_wakeup_mode;
    ss.checksum = rtc_crc32_le(RTC_SLOW_CRC32_MAGIC, (const uint8_t *) &ss,
                               sizeof(ss) - sizeof(ss.checksum));

    volatile struct esp32c3_rtc_sleep_state *storage =
        (volatile struct esp32c3_rtc_sleep_state *) ESP32C3_RTC_SLEEP_BASE;
    memcpy((void *) (uintptr_t) storage, &ss, sizeof(ss));

    fflush(stdout);
    fflush(stderr);

    volatile uint32_t *state0 = (volatile uint32_t *) ESP32C3_RTC_CNTL_STATE0;
    *state0 |= (1u << 31); // RTC_CNTL_SLEEP_EN

    volatile uint32_t *opt0 = (volatile uint32_t *) ESP32C3_RTC_CNTL_OPTIONS0;
    *opt0 |= (1u << 31); // SW_SYS_RST / emulator trigger

    while (1) {
        asm volatile("wfi");
    }
}

uint32_t esp32c3_sleep_get_wakeup_cause(void)
{
    int reset_reason = rtc_get_reset_reason(0);
    if (reset_reason == 0x05) { // RESET_REASON_CORE_DEEP_SLEEP
        uint32_t rom_cause = rtc_get_wakeup_cause();
        if (rom_cause & (1u << 3)) {
            return ESP32C3_WAKEUP_TIMER;
        }
        if (rom_cause & (1u << 2)) {
            return ESP32C3_WAKEUP_GPIO;
        }
        if (rom_cause & (1u << 0)) {
            return ESP32C3_WAKEUP_EXT0;
        }
        if (rom_cause & (1u << 1)) {
            return ESP32C3_WAKEUP_EXT1;
        }

        volatile struct esp32c3_rtc_sleep_state *storage =
            (volatile struct esp32c3_rtc_sleep_state *) ESP32C3_RTC_SLEEP_BASE;
        struct esp32c3_rtc_sleep_state ss;
        memcpy(&ss, (const void *) (uintptr_t) storage, sizeof(ss));
        if (ss.magic == RTC_SLEEP_MAGIC) {
            uint32_t crc = rtc_crc32_le(RTC_SLOW_CRC32_MAGIC, (const uint8_t *) &ss,
                                        sizeof(ss) - sizeof(ss.checksum));
            if (crc == ss.checksum) {
                return ss.last_wakeup_cause;
            }
        }
        return ESP32C3_WAKEUP_TIMER;
    }

    if (s_light_sleep_wakeup_cause != ESP32C3_WAKEUP_UNDEFINED) {
        return s_light_sleep_wakeup_cause;
    }

    return ESP32C3_WAKEUP_UNDEFINED;
}
#endif

static term nif_esp_rtc_slow_get_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

#if defined(RTEMS_BOARD_ESP32C3)
    volatile struct esp32c3_rtc_slow_storage *storage =
        (volatile struct esp32c3_rtc_slow_storage *) ESP32C3_RTC_RAM_BASE;

    uint16_t size = storage->size;
    if (size > CONFIG_AVM_RTC_SLOW_MAX_SIZE) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint32_t expected_checksum = storage->checksum;
    uint32_t checksum = rtc_crc32_le(RTC_SLOW_CRC32_MAGIC, (const uint8_t *) (uintptr_t) storage->data, size);
    if (checksum != expected_checksum) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(size)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_from_literal_binary((const void *) (uintptr_t) storage->data, size, &ctx->heap, ctx->global);
#else
    RAISE_ERROR(BADARG_ATOM);
#endif
}

static term nif_esp_rtc_slow_set_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_binary);
    term binary = argv[0];
    size_t size = term_binary_size(binary);

#if defined(RTEMS_BOARD_ESP32C3)
    if (size > CONFIG_AVM_RTC_SLOW_MAX_SIZE) {
        RAISE_ERROR(BADARG_ATOM);
    }
    const uint8_t *data = (const uint8_t *) term_binary_data(binary);
    uint32_t checksum = rtc_crc32_le(RTC_SLOW_CRC32_MAGIC, data, size);

    volatile struct esp32c3_rtc_slow_storage *storage =
        (volatile struct esp32c3_rtc_slow_storage *) ESP32C3_RTC_RAM_BASE;

    memcpy((void *) (uintptr_t) storage->data, data, size);
    storage->size = (uint16_t) size;
    storage->checksum = checksum;

    return OK_ATOM;
#else
    (void) size;
    RAISE_ERROR(BADARG_ATOM);
#endif
}

static term nif_esp_timer_get_time(Context *ctx, int argc, term argv[])
{
    UNUSED(argv);
    UNUSED(argc);

    struct timespec ts;
    rtems_clock_get_monotonic(&ts);
    uint64_t val = (uint64_t) ts.tv_sec * 1000000ULL + (uint64_t) ts.tv_nsec / 1000ULL;

    size_t term_size = term_boxed_integer_size(val);
    if (UNLIKELY(memory_ensure_free_opt(ctx, term_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_make_maybe_boxed_int64(val, &ctx->heap);
}

static term nif_esp_rtc_get_time_us(Context *ctx, int argc, term argv[])
{
    UNUSED(argv);
    UNUSED(argc);

#if defined(RTEMS_BOARD_ESP32C3)
    uint64_t ticks = esp32c3_rtc_get_counter();
    uint64_t us = (ticks * 1000000ULL) / RTC_SLOW_CLK_FREQ_DEFAULT;
#else
    struct timespec ts;
    rtems_clock_get_realtime(&ts);
    uint64_t us = (uint64_t) ts.tv_sec * 1000000ULL + (uint64_t) ts.tv_nsec / 1000ULL;
#endif

    size_t term_size = term_boxed_integer_size(us);
    if (UNLIKELY(memory_ensure_free_opt(ctx, term_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_make_maybe_boxed_int64(us, &ctx->heap);
}

static term nif_esp_reset_reason(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

#if defined(RTEMS_BOARD_ESP32C3)
    int raw_reason = rtc_get_reset_reason(0);
    switch (raw_reason) {
        case 0x01: // RESET_REASON_CHIP_POWER_ON
            return globalcontext_make_atom(ctx->global, esp_rst_poweron_atom);
        case 0x02:
            return globalcontext_make_atom(ctx->global, esp_rst_ext_atom);
        case 0x03: // RESET_REASON_CORE_SW
        case 0x0C: // RESET_REASON_CPU0_SW
            return globalcontext_make_atom(ctx->global, esp_rst_sw_atom);
        case 0x05: // RESET_REASON_CORE_DEEP_SLEEP
            return globalcontext_make_atom(ctx->global, esp_rst_deepsleep_atom);
        case 0x07: // RESET_REASON_CORE_MWDT0
        case 0x08: // RESET_REASON_CORE_MWDT1
        case 0x09: // RESET_REASON_CORE_RTC_WDT
        case 0x10: // RESET_REASON_SYS_RTC_WDT
        case 0x11: // RESET_REASON_CPU0_MWDT1
        case 0x12: // RESET_REASON_SYS_SUPER_WDT
            return globalcontext_make_atom(ctx->global, esp_rst_wdt_atom);
        case 0x0B: // RESET_REASON_CPU0_MWDT0
            return globalcontext_make_atom(ctx->global, esp_rst_task_wdt_atom);
        case 0x0D: // RESET_REASON_CPU0_RTC_WDT
            return globalcontext_make_atom(ctx->global, esp_rst_int_wdt_atom);
        case 0x0F: // RESET_REASON_SYS_BROWN_OUT
            return globalcontext_make_atom(ctx->global, esp_rst_brownout_atom);
        case 0x14: // EFUSE / PANIC
            return globalcontext_make_atom(ctx->global, esp_rst_panic_atom);
        default:
            return globalcontext_make_atom(ctx->global, esp_rst_unknown_atom);
    }
#else
    return globalcontext_make_atom(ctx->global, esp_rst_unknown_atom);
#endif
}

static term nif_esp_sleep_get_wakeup_cause(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

#if defined(RTEMS_BOARD_ESP32C3)
    uint32_t cause = esp32c3_sleep_get_wakeup_cause();
    switch (cause) {
        case ESP32C3_WAKEUP_EXT0:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_ext0_atom);
        case ESP32C3_WAKEUP_EXT1:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_ext1_atom);
        case ESP32C3_WAKEUP_TIMER:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_timer_atom);
        case ESP32C3_WAKEUP_TOUCHPAD:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_touchpad_atom);
        case ESP32C3_WAKEUP_ULP:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_ulp_atom);
        case ESP32C3_WAKEUP_GPIO:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_gpio_atom);
        case ESP32C3_WAKEUP_UART:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_uart_atom);
        case ESP32C3_WAKEUP_WIFI:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_wifi_atom);
        case ESP32C3_WAKEUP_COCPU:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_cocpu_atom);
        case ESP32C3_WAKEUP_BT:
            return globalcontext_make_atom(ctx->global, sleep_wakeup_bt_atom);
        case ESP32C3_WAKEUP_UNDEFINED:
        default:
            return UNDEFINED_ATOM;
    }
#else
    return UNDEFINED_ATOM;
#endif
}

static term nif_esp_deep_sleep(Context *ctx, int argc, term argv[])
{
#if defined(RTEMS_BOARD_ESP32C3)
    if (argc == 1) {
        VALIDATE_VALUE(argv[0], term_is_any_integer);
        avm_int64_t msecs = term_maybe_unbox_int64(argv[0]);
        if (UNLIKELY(msecs < 0)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        esp32c3_deep_sleep((uint64_t) msecs * 1000ULL);
    } else {
        UNUSED(argv);
        esp32c3_deep_sleep(0);
    }
    return OK_ATOM;
#else
    UNUSED(ctx);
    UNUSED(argv);
    UNUSED(argc);
    RAISE_ERROR(BADARG_ATOM);
#endif
}

static term nif_esp_light_sleep(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

#if defined(RTEMS_BOARD_ESP32C3)
    int res = esp32c3_light_sleep();
    return (res == 0) ? OK_ATOM : ERROR_ATOM;
#else
    return OK_ATOM;
#endif
}

static term nif_esp_sleep_enable_timer_wakeup(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_any_integer);
    avm_int64_t usecs = term_maybe_unbox_int64(argv[0]);
    if (UNLIKELY(usecs < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

#if defined(RTEMS_BOARD_ESP32C3)
    esp32c3_sleep_enable_timer_wakeup((uint64_t) usecs);
    return OK_ATOM;
#else
    UNUSED(ctx);
    return OK_ATOM;
#endif
}

static term nif_esp_deep_sleep_enable_gpio_wakeup(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_any_integer);
    VALIDATE_VALUE(argv[1], term_is_integer);

#if defined(RTEMS_BOARD_ESP32C3)
    avm_int64_t mask = term_maybe_unbox_int64(argv[0]);
    int mode = term_to_int(argv[1]);
    esp32c3_deep_sleep_enable_gpio_wakeup((uint64_t) mask, mode);
    return OK_ATOM;
#else
    UNUSED(ctx);
    return OK_ATOM;
#endif
}

static term nif_esp_sleep_enable_gpio_wakeup(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

#if defined(RTEMS_BOARD_ESP32C3)
    esp32c3_sleep_enable_gpio_wakeup();
    return OK_ATOM;
#else
    return OK_ATOM;
#endif
}

static term nif_gpio_deep_sleep_hold_en(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

#if defined(RTEMS_BOARD_ESP32C3)
    esp32c3_deep_sleep_hold_en();
#endif
    return OK_ATOM;
}

static term nif_gpio_deep_sleep_hold_dis(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

#if defined(RTEMS_BOARD_ESP32C3)
    esp32c3_deep_sleep_hold_dis();
#endif
    return OK_ATOM;
}

static term nif_rtems_get_rtc_counter(Context *ctx, int argc, term argv[])
{
    UNUSED(argv);
    UNUSED(argc);

#if defined(RTEMS_BOARD_ESP32C3)
    uint64_t ticks = esp32c3_rtc_get_counter();
#else
    uint64_t ticks = 0;
#endif

    size_t term_size = term_boxed_integer_size(ticks);
    if (UNLIKELY(memory_ensure_free_opt(ctx, term_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_make_maybe_boxed_int64(ticks, &ctx->heap);
}

static term nif_rtems_rtc_timekeeper_record(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_any_integer);
    VALIDATE_VALUE(argv[1], term_is_any_integer);

#if defined(RTEMS_BOARD_ESP32C3)
    struct timeval tv;
    tv.tv_sec = term_maybe_unbox_int64(argv[0]);
    tv.tv_usec = term_maybe_unbox_int64(argv[1]);
    esp32c3_rtc_timekeeper_record(&tv);
    return OK_ATOM;
#else
    RAISE_ERROR(BADARG_ATOM);
#endif
}

static term nif_rtems_rtc_timekeeper_restore(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

#if defined(RTEMS_BOARD_ESP32C3)
    bool restored = esp32c3_rtc_timekeeper_restore();
    return restored ? TRUE_ATOM : FALSE_ATOM;
#else
    return FALSE_ATOM;
#endif
}

static term nif_rtems_rtc_timekeeper_status(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

#if defined(RTEMS_BOARD_ESP32C3)
    volatile struct esp32c3_rtc_timekeeper *storage =
        (volatile struct esp32c3_rtc_timekeeper *) ESP32C3_RTC_TIMEKEEPER_BASE;

    struct esp32c3_rtc_timekeeper tk;
    memcpy(&tk, (const void *) (uintptr_t) storage, sizeof(tk));

    if (tk.magic != RTC_TIMEKEEPER_MAGIC) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_tuple2(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, not_synced_atom));
    }
    uint32_t expected_crc = tk.checksum;
    uint32_t computed_crc = rtc_crc32_le(RTC_SLOW_CRC32_MAGIC, (const uint8_t *) &tk,
                                         sizeof(tk) - sizeof(tk.checksum));
    if (computed_crc != expected_crc) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_tuple2(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, not_synced_atom));
    }

    uint64_t now_ticks = esp32c3_rtc_get_counter();
    uint64_t elapsed_ticks = (now_ticks >= tk.rtc_counter) ? (now_ticks - tk.rtc_counter) : 0;
    uint64_t elapsed_sec = (tk.rtc_clk_freq > 0) ? (elapsed_ticks / tk.rtc_clk_freq) : 0;

    // Allocate memory for map and tuple
    size_t term_size = TUPLE_SIZE(2) + term_map_size_in_terms(4) +
                       BOXED_INT64_SIZE * 4;
    if (UNLIKELY(memory_ensure_free_opt(ctx, term_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term sntp_sec_term = term_make_maybe_boxed_int64(tk.sntp_sec, &ctx->heap);
    term sntp_usec_term = term_make_maybe_boxed_int64(tk.sntp_usec, &ctx->heap);
    term rtc_counter_term = term_make_maybe_boxed_int64(tk.rtc_counter, &ctx->heap);
    term elapsed_sec_term = term_make_maybe_boxed_int64(elapsed_sec, &ctx->heap);

    term key_sec = globalcontext_make_atom(ctx->global, sntp_sec_atom);
    term key_usec = globalcontext_make_atom(ctx->global, sntp_usec_atom);
    term key_cnt = globalcontext_make_atom(ctx->global, rtc_counter_atom);
    term key_el = globalcontext_make_atom(ctx->global, elapsed_sec_atom);

    term map = term_alloc_map(4, &ctx->heap);
    term_set_map_assoc(map, 0, key_sec, sntp_sec_term);
    term_set_map_assoc(map, 1, key_usec, sntp_usec_term);
    term_set_map_assoc(map, 2, key_cnt, rtc_counter_term);
    term_set_map_assoc(map, 3, key_el, elapsed_sec_term);

    return make_tuple2(ctx, OK_ATOM, map);
#else
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return make_tuple2(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, enotsup_atom));
#endif
}

static const struct Nif esp_rtc_slow_get_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_rtc_slow_get_binary
};

static const struct Nif esp_rtc_slow_set_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_rtc_slow_set_binary
};

static const struct Nif esp_timer_get_time_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_timer_get_time
};

static const struct Nif esp_rtc_get_time_us_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_rtc_get_time_us
};

static const struct Nif esp_reset_reason_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_reset_reason
};

static const struct Nif esp_sleep_get_wakeup_cause_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_sleep_get_wakeup_cause
};

static const struct Nif esp_deep_sleep_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_deep_sleep
};

static const struct Nif esp_light_sleep_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_light_sleep
};

static const struct Nif esp_sleep_enable_timer_wakeup_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_sleep_enable_timer_wakeup
};

static const struct Nif esp_deep_sleep_enable_gpio_wakeup_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_deep_sleep_enable_gpio_wakeup
};

static const struct Nif esp_sleep_enable_gpio_wakeup_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_sleep_enable_gpio_wakeup
};

static const struct Nif gpio_deep_sleep_hold_en_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_deep_sleep_hold_en
};

static const struct Nif gpio_deep_sleep_hold_dis_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_gpio_deep_sleep_hold_dis
};

static const struct Nif rtems_get_rtc_counter_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_get_rtc_counter
};

static const struct Nif rtems_rtc_timekeeper_record_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_rtc_timekeeper_record
};

static const struct Nif rtems_rtc_timekeeper_restore_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_rtc_timekeeper_restore
};

static const struct Nif rtems_rtc_timekeeper_status_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_rtc_timekeeper_status
};

const struct Nif *rtc_slow_nif_get_nif(const char *nifname)
{
    if (strcmp("esp:rtc_slow_get_binary/0", nifname) == 0 ||
        strcmp("atomvm_rtems:rtc_slow_get_binary/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_rtc_slow_get_binary_nif;
    }
    if (strcmp("esp:rtc_slow_set_binary/1", nifname) == 0 ||
        strcmp("atomvm_rtems:rtc_slow_set_binary/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_rtc_slow_set_binary_nif;
    }
    if (strcmp("esp:timer_get_time/0", nifname) == 0 ||
        strcmp("atomvm_rtems:timer_get_time/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_timer_get_time_nif;
    }
    if (strcmp("esp:rtc_get_time_us/0", nifname) == 0 ||
        strcmp("atomvm_rtems:get_rtc_time/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_rtc_get_time_us_nif;
    }
    if (strcmp("esp:reset_reason/0", nifname) == 0 ||
        strcmp("atomvm_rtems:reset_reason/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_reset_reason_nif;
    }
    if (strcmp("esp:sleep_get_wakeup_cause/0", nifname) == 0 ||
        strcmp("atomvm_rtems:sleep_get_wakeup_cause/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_sleep_get_wakeup_cause_nif;
    }
    if (strcmp("esp:deep_sleep/0", nifname) == 0 ||
        strcmp("esp:deep_sleep/1", nifname) == 0 ||
        strcmp("atomvm_rtems:deep_sleep/0", nifname) == 0 ||
        strcmp("atomvm_rtems:deep_sleep/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_deep_sleep_nif;
    }
    if (strcmp("esp:light_sleep/0", nifname) == 0 ||
        strcmp("atomvm_rtems:light_sleep/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_light_sleep_nif;
    }
    if (strcmp("esp:sleep_enable_timer_wakeup/1", nifname) == 0 ||
        strcmp("atomvm_rtems:sleep_enable_timer_wakeup/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_sleep_enable_timer_wakeup_nif;
    }
    if (strcmp("esp:deep_sleep_enable_gpio_wakeup/2", nifname) == 0 ||
        strcmp("atomvm_rtems:deep_sleep_enable_gpio_wakeup/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_deep_sleep_enable_gpio_wakeup_nif;
    }
    if (strcmp("esp:sleep_enable_gpio_wakeup/0", nifname) == 0 ||
        strcmp("atomvm_rtems:sleep_enable_gpio_wakeup/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_sleep_enable_gpio_wakeup_nif;
    }
    if (strcmp("esp:deep_sleep_hold_en/0", nifname) == 0 ||
        strcmp("gpio:deep_sleep_hold_en/0", nifname) == 0 ||
        strcmp("Elixir.GPIO:deep_sleep_hold_en/0", nifname) == 0 ||
        strcmp("atomvm_rtems:deep_sleep_hold_en/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_deep_sleep_hold_en_nif;
    }
    if (strcmp("esp:deep_sleep_hold_dis/0", nifname) == 0 ||
        strcmp("gpio:deep_sleep_hold_dis/0", nifname) == 0 ||
        strcmp("Elixir.GPIO:deep_sleep_hold_dis/0", nifname) == 0 ||
        strcmp("atomvm_rtems:deep_sleep_hold_dis/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &gpio_deep_sleep_hold_dis_nif;
    }
    if (strcmp("atomvm_rtems:get_rtc_counter/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtems_get_rtc_counter_nif;
    }
    if (strcmp("atomvm_rtems:rtc_timekeeper_record/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtems_rtc_timekeeper_record_nif;
    }
    if (strcmp("atomvm_rtems:rtc_timekeeper_restore/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtems_rtc_timekeeper_restore_nif;
    }
    if (strcmp("atomvm_rtems:rtc_timekeeper_status/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &rtems_rtc_timekeeper_status_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(rtc_slow, NULL, NULL, rtc_slow_nif_get_nif)
