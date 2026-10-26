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

#if defined(RTEMS_BOARD_ESP32)
#include "esp_sleep_cache.h"
#include "esp_sleep_clock.h"
#include "esp_sleep_console.h"
#include "esp_sleep_pmu.h"
#include "esp_sleep_regs.h"
#include <bsp/linker-symbols.h>
#include <errno.h>
#include <rtems.h>

#if defined(RTEMS_BOARD_ESP32S31)
#define ESP_RTC_TIMER_BASE 0x20800000U
#define ESP_RTC_UPDATE_SHIFT 27
#elif defined(RTEMS_BOARD_ESP32P4)
#define ESP_RTC_TIMER_BASE 0x50112000U
#define ESP_RTC_UPDATE_SHIFT 28
#else
#define ESP_RTC_TIMER_BASE 0x600B0C00U
#if defined(RTEMS_BOARD_ESP32C5)
#define ESP_RTC_UPDATE_SHIFT 27
#else
#define ESP_RTC_UPDATE_SHIFT 28
#endif
#endif
#define RTC_COUNTER_MASK ((1ULL << 48) - 1)

RTEMS_INTERRUPT_LOCK_DEFINE(static, rtc_lock, "RTC snapshot")

/* The cache-off caller already has local interrupts disabled and the other
 * CPU parked. It must not enter RTEMS's flash-resident SMP locking code.
 */
static inline __attribute__((always_inline)) uint64_t rtc_counter_snapshot(void)
{
#if defined(RTEMS_BOARD_ESP32C3)
    volatile uint32_t *time_upd = (volatile uint32_t *) 0x6000800CU;
    *time_upd = (1U << 31);
    for (int i = 0; i < 100; i++) {
        if (*time_upd & (1U << 30)) {
            break;
        }
    }
    uint32_t low = *(volatile uint32_t *) 0x60008010U;
    uint32_t high = *(volatile uint32_t *) 0x60008014U;
#else
    /* ESP-IDF rtc_timer_ll_counter_snapshot(), LP_TIMER/RTC_TIMER buffer 0. */
    volatile uint32_t *update = (volatile uint32_t *) (ESP_RTC_TIMER_BASE + 0x10U);
    *update |= 1U << ESP_RTC_UPDATE_SHIFT;
    uint32_t low = *(volatile uint32_t *) (ESP_RTC_TIMER_BASE + 0x14U);
    uint32_t high = *(volatile uint32_t *) (ESP_RTC_TIMER_BASE + 0x18U);
#endif
    return ((uint64_t) (high & 0xFFFFU) << 32) | low;
}

uint64_t BSP_FAST_TEXT_SECTION rtems_esp_rtc_get_counter(void)
{
    rtems_interrupt_lock_context lock;
    rtems_interrupt_lock_acquire(&rtc_lock, &lock);
    uint64_t rtc_ticks = rtc_counter_snapshot();
    rtems_interrupt_lock_release(&rtc_lock, &lock);
    return rtc_ticks;
}

#if defined(RTEMS_BOARD_ESP32S31)
extern uint32_t Cache_Suspend_L1_CORE0_ICache(void);
extern uint32_t Cache_Suspend_L1_CORE1_ICache(void);
extern uint32_t Cache_Suspend_L1_DCache(void);
extern void Cache_Resume_L1_CORE0_ICache(uint32_t autoload);
extern void Cache_Resume_L1_CORE1_ICache(uint32_t autoload);
extern void Cache_Resume_L1_DCache(uint32_t autoload);
#elif defined(RTEMS_BOARD_ESP32P4)
extern uint32_t Cache_Suspend_L2_Cache(void);
extern void Cache_Resume_L2_Cache(uint32_t autoload);
#elif defined(RTEMS_BOARD_ESP32C5)
extern uint32_t Cache_Suspend_Cache(void);
extern void Cache_Resume_Cache(uint32_t autoload);
#else
extern uint32_t Cache_Suspend_ICache(void);
extern void Cache_Resume_ICache(uint32_t autoload);
#endif

struct sleep_cache_state
{
    uint32_t prediction;
    uint32_t autoload;
#if defined(RTEMS_BOARD_ESP32S31)
    uint32_t instruction0;
    uint32_t instruction1;
#endif
};

static inline __attribute__((always_inline)) void sleep_cache_suspend(struct sleep_cache_state *state)
{
    state->prediction = sleep_disable_branch_prediction();
#if defined(RTEMS_BOARD_ESP32S31)
    state->instruction0 = Cache_Suspend_L1_CORE0_ICache();
    state->instruction1 = Cache_Suspend_L1_CORE1_ICache();
    state->autoload = Cache_Suspend_L1_DCache();
#elif defined(RTEMS_BOARD_ESP32P4)
    /* Internal SRAM uses L1 on P4. Only suspend the external-memory L2. */
    state->autoload = Cache_Suspend_L2_Cache();
#elif defined(RTEMS_BOARD_ESP32C5)
    state->autoload = Cache_Suspend_Cache();
#else
    state->autoload = Cache_Suspend_ICache();
#endif
}

static inline __attribute__((always_inline)) void sleep_cache_resume(const struct sleep_cache_state *state)
{
#if defined(RTEMS_BOARD_ESP32S31)
    Cache_Resume_L1_DCache(state->autoload);
    Cache_Resume_L1_CORE1_ICache(state->instruction1);
    Cache_Resume_L1_CORE0_ICache(state->instruction0);
#elif defined(RTEMS_BOARD_ESP32P4)
    Cache_Resume_L2_Cache(state->autoload);
#elif defined(RTEMS_BOARD_ESP32C5)
    Cache_Resume_Cache(state->autoload);
#else
    Cache_Resume_ICache(state->autoload);
#endif
    sleep_restore_branch_prediction(state->prediction);
}

static inline __attribute__((always_inline)) void program_alarm(uint64_t target)
{
#if defined(RTEMS_BOARD_ESP32C3)
    REG(0x60008004U) = (uint32_t) target;
    REG(0x6000804CU) = BIT(10);
    REG(0x60008008U) = ((target >> 32) & 0xFFFFU) | BIT(16);
#else
    REG(ESP_RTC_TIMER_BASE + 0x34U) = BIT(31);
    REG(ESP_RTC_TIMER_BASE + 0x30U) |= BIT(31);
    REG(ESP_RTC_TIMER_BASE) = (uint32_t) target;
    REG(ESP_RTC_TIMER_BASE + 4U) = ((target >> 32) & 0xFFFFU) | BIT(31);
#endif
}

static int BSP_FAST_TEXT_SECTION __attribute__((noinline)) sleep_transition(bool deep, uint32_t wakeup,
    uint64_t start, uint64_t ticks, uint32_t minimum_ticks, uint32_t *cause,
    struct rtems_esp_sleep_pmu_state *pmu, const struct esp_sleep_clock_state *clock)
{
    struct sleep_cache_state cache;
    sleep_cache_suspend(&cache);
#if defined(RTEMS_BOARD_ESP32C3)
    REG(0x6000803CU) = (REG(0x6000803CU) & ~(0x1FFFFU << 15)) | (wakeup << 15);
    /* Both supported wake sources are in IDF's RTC_SLEEP_REJECT_MASK. */
    REG(0x60008068U) = (REG(0x60008068U) & ~(0x1FFFFU << 12)) | (deep ? 0 : wakeup << 12);
    REG(0x6000804CU) = 3U;
    REG(0x60008018U) |= BIT(1);
#else
    REG(SLEEP_MODE) = (REG(SLEEP_MODE) & ~1U) | (deep ? 1U : 0U);
#if defined(RTEMS_BOARD_ESP32S31)
    REG(PMU_BASE + PMU_CTRL + 4U) = deep ? 0 : wakeup;
    REG(PMU_BASE + PMU_CTRL) |= BIT(30); /* S31 moved SLP_REJECT_EN to CNTL0. */
#else
    REG(PMU_BASE + PMU_CTRL + 4U) = BIT(31) | (deep ? 0 : wakeup);
#endif
    REG(PMU_BASE + PMU_CTRL + 8U) = wakeup;
    REG(PMU_BASE + PMU_CLEAR) = BIT(30) | BIT(31);
    REG(PMU_BASE + PMU_CTRL + 16U) = BIT(31);
#endif
    rtems_esp_sleep_clock_before_entry(clock, deep);
    rtems_esp_sleep_pmu_before_entry(pmu);
    int result;
    bool requested = false;
    bool rejected = false;
    if ((wakeup & TIMER_WAKE)
        && (((rtc_counter_snapshot() - start) & RTC_COUNTER_MASK) + minimum_ticks >= ticks)) {
        result = ETIMEDOUT;
    } else {
        if (wakeup & TIMER_WAKE) {
            program_alarm(start + ticks);
        }
        requested = true;
#if defined(RTEMS_BOARD_ESP32C3)
        REG(0x60008018U) |= BIT(31);
        __asm__ volatile("fence iorw, iorw" ::: "memory");
        while ((REG(0x60008044U) & 3U) == 0) {
        }
        rejected = (REG(0x60008044U) & BIT(1)) != 0;
        uint32_t raw_cause = REG(0x600080F8U);
#else
#if defined(RTEMS_BOARD_ESP32P4)
        /* IDF pmu_sleep_cache_sync_items(): keep this inline at the actual
         * request boundary. A call/return after writeback can dirty the
         * internal-SRAM stack in L1 again. Both P4 layouts use these fields.
         */
        REG(0x3FF100A0U) = 0;
        REG(0x3FF100A4U) = 0;
        REG(0x3FF1009CU) = BIT(4); /* L1 D-cache */
        REG(0x3FF10098U) &= ~(15U << 5); /* SMMU_GID_DEFAULT */
        REG(0x3FF10098U) |= BIT(2); /* WRITEBACK */
        while (!(REG(0x3FF10098U) & BIT(4))) {
        }
#endif
        REG(PMU_BASE + PMU_CTRL) |= BIT(31);
        __asm__ volatile("fence iorw, iorw" ::: "memory");
        /* Like IDF, wait for hardware wake OR rejection. There is no safe
         * generic cancellation after setting sleep-enable; never resume
         * caches or callers while a transition remains unresolved.
         */
        while ((REG(PMU_BASE + PMU_RAW) & (BIT(30) | BIT(31))) == 0) {
        }
        rejected = (REG(PMU_BASE + PMU_RAW) & BIT(30)) != 0;
        uint32_t raw_cause = REG(PMU_BASE + PMU_WAKE_STATUS);
#endif
        result = rejected ? EAGAIN : (deep ? EIO : 0);
        if (!result) {
            *cause = raw_cause;
        }
    }
    rtems_esp_sleep_pmu_finish(pmu, requested, rejected);
    rtems_esp_sleep_clock_finish(clock);
#if defined(RTEMS_BOARD_ESP32C3)
    REG(0x6000804CU) = 3U;
#else
    REG(PMU_BASE + PMU_CLEAR) = BIT(30) | BIT(31);
#endif
    sleep_cache_resume(&cache);
    return result;
}

int rtems_esp_sleep_enter(bool deep, uint32_t wakeup, uint64_t usecs, uint32_t *cause)
{
    *cause = 0;
    uint64_t start = rtc_counter_snapshot();
    struct sleep_console_state console;
    struct esp_sleep_clock_state clock;
    struct rtems_esp_sleep_pmu_state pmu;
    rtems_esp_sleep_console_prepare(deep, &console);
    if (!rtems_esp_sleep_clock_prepare(&clock)) {
        rtems_esp_sleep_console_resume(deep, &console);
        return ENOTSUP;
    }
    int result = rtems_esp_sleep_pmu_prepare(&pmu, deep, wakeup, clock.xtal_freq_mhz);
    if (result) {
        rtems_esp_sleep_clock_finish(&clock);
    } else {
        /* Q19 division is done with caches ON. Splitting the numerator
         * avoids overflow for long deep-sleep intervals. The installed
         * RTEMS timecounter keeps its original scale; only this alarm and
         * PMU waits use the per-attempt oscillator calibration.
         */
        uint64_t ticks = ((usecs / pmu.slow_period) << 19)
            + (((usecs % pmu.slow_period) << 19) / pmu.slow_period);
        uint32_t minimum_ticks = (((uint64_t) pmu.minimum_entry_us << 19) + pmu.slow_period - 1) / pmu.slow_period;
        if (ticks > RTC_COUNTER_MASK) {
            rtems_esp_sleep_pmu_finish(&pmu, false, false);
            rtems_esp_sleep_clock_finish(&clock);
            result = EINVAL;
        } else {
            result = sleep_transition(deep, wakeup, start, ticks, minimum_ticks, cause, &pmu, &clock);
        }
    }
    rtems_esp_sleep_console_resume(deep, &console);
    return result;
}
#endif
