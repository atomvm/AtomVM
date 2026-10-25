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
#include "esp_sleep_regs.h"
#include <bsp/linker-symbols.h>
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

RTEMS_INTERRUPT_LOCK_DEFINE(static, rtc_lock, "RTC snapshot")

uint64_t BSP_FAST_TEXT_SECTION rtems_esp_rtc_get_counter(void)
{
    rtems_interrupt_lock_context lock;
    rtems_interrupt_lock_acquire(&rtc_lock, &lock);
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
    uint64_t rtc_ticks = ((uint64_t) (high & 0xFFFFU) << 32) | low;

    rtems_interrupt_lock_release(&rtc_lock, &lock);
    return rtc_ticks;
}

uint32_t BSP_FAST_TEXT_SECTION rtems_esp_sleep_enter(bool deep, uint32_t wakeup, uint64_t deadline)
{
#if defined(RTEMS_BOARD_ESP32C3)
    /* RTC memory, the slow clock and analog regulators remain powered. Deep
     * sleep powers down the digital wrapper and restarts through the ROM.
     */
    uint32_t power = REG(0x60008088U);
    uint32_t iso = REG(0x6000808CU);
    uint32_t options = REG(0x60008000U);
    uint32_t clock = REG(0x60008070U);
    REG(0x60008088U) = deep ? ((power & ~BIT(20)) | BIT(31)) : (power & ~(BIT(31) | BIT(29) | BIT(28)));
    if (deep) {
        REG(0x6000808CU) &= ~BIT(31); /* Release digital wrapper isolation to FSM. */
        REG(0x600080C0U) = 0; /* ROM boot, no RTC wake stub. */
    }
    REG(0x60008000U) |= BIT(13) | BIT(11); /* XTAL/PLL force on */
    REG(0x60008070U) |= BIT(28);
    REG(0x6000803CU) = (REG(0x6000803CU) & ~(0x1FFFFU << 15)) | (wakeup << 15);
    REG(0x60008068U) &= ~(0x1FFFFU << 12); /* No sleep rejection sources. */
    REG(0x6000804CU) = 3U;
    REG(0x60008018U) |= BIT(1); /* Clear prior reject cause. */
    REG(0x60008018U) |= BIT(31);
    __asm__ volatile("fence iorw, iorw" ::: "memory");
    while ((REG(0x60008044U) & 3U) == 0) {
        if (rtems_esp_rtc_get_counter() >= deadline) {
            REG(0x60008018U) &= ~BIT(31);
            REG(0x60008088U) = power;
            REG(0x6000808CU) = iso;
            REG(0x60008000U) = options;
            REG(0x60008070U) = clock;
            return 0;
        }
    }
    uint32_t status = REG(0x60008044U);
    uint32_t cause = REG(0x600080F8U);
    REG(0x6000804CU) = 3U;
    REG(0x60008088U) = power;
    REG(0x6000808CU) = iso;
    REG(0x60008000U) = options;
    REG(0x60008070U) = clock;
    return (status & BIT(1)) ? 0 : cause;
#else
    /* The HP active and sleep register groups share their layout. Copy only
     * power/clock/bias/regulator fields, excluding REGDMA backup controls.
     */
#if defined(RTEMS_BOARD_ESP32S31)
    static const uint8_t offsets[] = { 0, 4, 8, 12, 16, 20, 24, 28, 32, 48, 52, 56, 60 };
    const uint32_t sys = 0x18U, sysclk = 0x30U;
#else
    static const uint8_t offsets[] = { 0, 4, 8, 12, 16, 20, 24, 36, 40, 44, 48 };
    const uint32_t sys = 0x10U, sysclk = 0x24U;
#endif
    for (unsigned i = 0; i < sizeof(offsets); i++) {
        REG(PMU_BASE + PMU_SLEEP + offsets[i]) = REG(PMU_BASE + offsets[i]);
    }
    /* Pause watchdogs, stall CPUs, select the sleep system clock and gate it. */
    REG(PMU_BASE + PMU_SLEEP + sys) |= BIT(28) | BIT(29);
    REG(PMU_BASE + PMU_SLEEP + sysclk) = BIT(28) | BIT(29);
    uint32_t forces[PMU_FORCE_COUNT];
    uint32_t memory_force = REG(PMU_BASE + PMU_MEM_FORCE);
    if (deep) {
        REG(PMU_BASE + PMU_MEM_FORCE) &= ~(0xFU << 24);
        /* ROM direct boot leaves force-PU/no-reset/no-isolation asserted.
         * Hand the HP power domains to the PMU before requesting deep sleep.
         */
        for (unsigned i = 0; i < PMU_FORCE_COUNT; i++) {
            uintptr_t reg = PMU_BASE + PMU_FORCE0 + i * 4U;
            forces[i] = REG(reg);
#if defined(RTEMS_BOARD_ESP32C6)
            if (i == 3) { /* Reserved HPPERIPH domain on C6. */
                continue;
            }
#endif
            REG(reg) &= ~0x3FU;
        }
#if defined(RTEMS_BOARD_ESP32S31)
        REG(PMU_BASE + PMU_SLEEP) |= BIT(31) | BIT(30) | BIT(29) | BIT(28) | BIT(27) | BIT(21);
#else
        REG(PMU_BASE + PMU_SLEEP) |= BIT(31) | BIT(30) | BIT(29) | BIT(27) | (0xFU << 23);
#endif
        REG(PMU_BASE + PMU_SLEEP + sys) |= BIT(25) | BIT(26); /* Retain pad levels. */
    }
    REG(PMU_BASE + PMU_ICG_UPDATE) = BIT(28);
    REG(SLEEP_MODE) = (REG(SLEEP_MODE) & ~1U) | (deep ? 1U : 0U);
    REG(PMU_BASE + PMU_CTRL + 4U) = 0; /* Reject disabled. */
    REG(PMU_BASE + PMU_CTRL + 8U) = wakeup;
    REG(PMU_BASE + PMU_CLEAR) = BIT(30) | BIT(31);
    REG(PMU_BASE + PMU_CTRL + 16U) = BIT(31);
    REG(PMU_BASE + PMU_CTRL) |= BIT(31);
    __asm__ volatile("fence iorw, iorw" ::: "memory");
    bool timed_out = false;
    while ((REG(PMU_BASE + PMU_RAW) & (BIT(30) | BIT(31))) == 0) {
        if (rtems_esp_rtc_get_counter() >= deadline) {
            timed_out = true;
            break;
        }
    }
    if (deep) {
        REG(SLEEP_MODE) &= ~1U;
        REG(PMU_BASE + PMU_MEM_FORCE) = memory_force;
        for (unsigned i = 0; i < PMU_FORCE_COUNT; i++) {
            REG(PMU_BASE + PMU_FORCE0 + i * 4U) = forces[i];
        }
    }
    uint32_t status = REG(PMU_BASE + PMU_RAW);
    uint32_t cause = REG(PMU_BASE + PMU_CTRL + 32U);
    REG(PMU_BASE + PMU_CLEAR) = BIT(30) | BIT(31);
    return timed_out || (status & BIT(30)) ? 0 : cause;
#endif
}

#endif
