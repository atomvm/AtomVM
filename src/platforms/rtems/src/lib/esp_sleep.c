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
#include <bsp/irq.h>
#include <bsp/linker-symbols.h>
#include <errno.h>
#include <pthread.h>
#include <rtems.h>
#include <rtems/counter.h>
#include <rtems/score/smpimpl.h>
#include <rtems/score/threaddispatch.h>
#include <rtems/score/watchdogimpl.h>
#include <rtems/timecounter.h>
#include <sched.h>
#include <stdatomic.h>

#include "esp_sleep_cache.h"
#include "esp_sleep_regs.h"

static pthread_mutex_t sleep_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_once_t clock_once = PTHREAD_ONCE_INIT;
static pthread_once_t calibration_once = PTHREAD_ONCE_INIT;
static uint32_t rtc_frequency;

static uint32_t rtc_timecount(struct timecounter *tc)
{
    (void) tc;
    return (uint32_t) rtems_esp_rtc_get_counter();
}

static struct timecounter rtc_timecounter = {
    .tc_get_timecount = rtc_timecount,
    .tc_counter_mask = UINT32_MAX,
    .tc_quality = RTEMS_TIMECOUNTER_QUALITY_CLOCK_DRIVER + 1,
};

static void calibrate_rtc_clock(void)
{
    /* Calibrate against the crystal-derived SYSTIMER, without changing the
     * selected slow clock or resetting the counter retained across deep sleep.
     */
    uint64_t before = rtems_esp_rtc_get_counter();
    rtems_counter_ticks start = rtems_counter_read();
    rtems_counter_delay_nanoseconds(20000000);
    uint64_t elapsed = rtems_esp_rtc_get_counter() - before;
    rtems_counter_ticks cpu_elapsed = rtems_counter_difference(rtems_counter_read(), start);
    if (elapsed == 0 || cpu_elapsed == 0) {
        return;
    }
    rtc_frequency = (uint32_t) ((elapsed * rtems_counter_frequency() + cpu_elapsed / 2) / cpu_elapsed);
}

uint32_t rtems_esp_rtc_measure_frequency(void)
{
    pthread_once(&calibration_once, calibrate_rtc_clock);
    return rtc_frequency;
}

static void install_rtc_clock(void)
{
    uint32_t frequency = rtems_esp_rtc_measure_frequency();
    if (frequency == 0) {
        return;
    }
    rtc_timecounter.tc_frequency = frequency;
    rtems_timecounter_install(&rtc_timecounter);
}

uint32_t rtems_esp_rtc_frequency(void)
{
    pthread_once(&clock_once, install_rtc_clock);
    return rtc_frequency;
}

#if defined(RTEMS_SMP)
struct sleep_rendezvous
{
    atomic_bool ready;
    atomic_bool release;
    Per_CPU_Job_context context;
    Per_CPU_Job job;
    Per_CPU_Control *cpu;
};

static BSP_FAST_TEXT_SECTION void park_cpu(void *arg)
{
    struct sleep_rendezvous *state = arg;
    /* Executed by an RTEMS per-CPU job at interrupt level, after the other
     * processor has left interrupt locks. No scheduler or device locks are
     * acquired between this acknowledgement and the release.
     */
    uint32_t prediction = sleep_disable_branch_prediction();
    atomic_store_explicit(&state->ready, true, memory_order_release);
    while (!atomic_load_explicit(&state->release, memory_order_acquire)) {
        __asm__ volatile("nop");
    }
    sleep_restore_branch_prediction(prediction);
}
#endif

static void account_sleep(uint64_t elapsed)
{
    uint64_t ticks = (elapsed * 1000000ULL) / ((uint64_t) rtc_frequency * rtems_configuration_get_microseconds_per_tick());
    if (ticks == 0) {
        return;
    }
    /* A single hardware tick can represent many missed scheduler ticks.
     * Expire each CPU's tick, monotonic and realtime queues once at the new
     * time, rather than replaying millions of ISR calls after a long sleep.
     */
    _Watchdog_Ticks_since_boot += (rtems_interval) (ticks - 1);
    uint32_t count = rtems_scheduler_get_processor_maximum();
    for (uint32_t i = 0; i < count; i++) {
#ifdef RTEMS_SMP
        if (!_Processor_mask_Is_set(_SMP_Get_online_processors(), i)) {
            continue;
        }
#endif
        Per_CPU_Control *cpu = _Per_CPU_Get_by_index(i);
        cpu->Watchdog.ticks += ticks - 1;
        if (i == 0) {
            rtems_timecounter_tick();
        } else {
            _Watchdog_Tick(cpu);
        }
    }
}

struct gpio_sleep_state
{
    uint32_t select;
    uint32_t mux[GPIO_COUNT];
    uint32_t pin[GPIO_COUNT];
#if defined(RTEMS_BOARD_ESP32H2)
    uint32_t hold;
    uint32_t wakeup;
#endif
};

static void prepare_gpio(uint32_t mask, int mode, struct gpio_sleep_state *saved)
{
#if defined(RTEMS_BOARD_ESP32C3)
    saved->select = REG(0x60008110U);
    uint32_t wake = saved->select & ~((0x3FU << 26) | (0x3FFFFU << 8));
    for (unsigned pin = 0; pin < GPIO_COUNT; pin++) {
        if (mask & BIT(pin)) {
            wake |= BIT(31 - pin) | ((uint32_t) (mode ? 5 : 4) << (23 - pin * 3));
        }
    }
    REG(0x60008064U) |= BIT(31); /* Wakeup input filter. */
    REG(0x60008110U) = wake | BIT(7); /* GPIO_PIN_CLK_GATE */
#elif defined(RTEMS_BOARD_ESP32H2)
    /* ESP32-H2 ext1_wakeup_prepare(): the wake pads are ordinary digital
     * GPIOs held through sleep, and the always-on EXT1 comparator samples
     * the levels the LP_AON selection and polarity bits name. Only GPIO7-14
     * reach EXT1, so bit n of the mask is rtcio n, GPIO n + 7. AtomVM keeps
     * one wake mask for both sleep modes, so light sleep uses this source
     * too, which ESP-IDF allows.
     */
    if (mask) {
        saved->select = REG(LP_AON_GPIO_MUX);
        saved->hold = REG(LP_AON_GPIO_HOLD0);
        saved->wakeup = REG(LP_AON_EXT_WAKEUP);
        uint32_t hold = saved->hold;
        REG(LPPERI_CLK_EN) |= BIT(27); /* LP IO clock samples the wake pads. */
        for (unsigned pin = 0; pin < GPIO_COUNT; pin++) {
            if (!(mask & BIT(pin))) {
                continue;
            }
            uint32_t gpio = pin + GPIO_RTCIO_OFFSET;
            uintptr_t iomux = SLEEP_IO_MUX_BASE + (gpio + 1U) * 4U;
            saved->mux[pin] = REG(iomux);
            /* Digital GPIO function with the input enabled, pulled away from
             * the configured trigger level so a floating pad cannot wake
             * immediately.
             */
            uint32_t cfg = saved->mux[pin] & ~((7U << 12) | BIT(9) | BIT(8) | BIT(7));
            cfg |= (1U << 12) | BIT(9) | (mode ? BIT(7) : BIT(8));
            REG(iomux) = cfg;
            hold |= BIT(gpio);
        }
        REG(LP_AON_GPIO_MUX) = saved->select & ~mask; /* 0 routes the pad to digital GPIO. */
        REG(LP_AON_GPIO_HOLD0) = hold;
        REG(LP_AON_EXT_WAKEUP) = (saved->wakeup & ~((0xFFU << 15) | (0xFFU << 23) | BIT(31)))
            | ((mask & 0xFFU) << 15) | ((mode ? (mask & 0xFFU) : 0U) << 23) | BIT(31) | BIT(14);
    }
#else
#if !defined(RTEMS_BOARD_ESP32P4)
    saved->select = REG(LP_MUX_SELECT);
#endif
#if defined(RTEMS_BOARD_ESP32S31)
    REG(0x20710034U) |= BIT(30); /* LP IOMUX clock */
    REG(0x207133F8U) |= 1U;
#elif defined(RTEMS_BOARD_ESP32P4)
    REG(0x5012B000U) |= 1U; /* LP IOMUX clock */
    REG(0x5012A000U) |= 1U; /* Independent LP GPIO clock. */
    while (!(REG(0x5012A000U) & 1U)) {
    }
#else
    REG(0x600B2800U) |= BIT(27); /* LP IO clock */
#if defined(RTEMS_BOARD_ESP32C5)
    REG(0x600B47F8U) |= 1U;
#else
    REG(0x600B23FCU) |= BIT(31);
#endif
#endif
    for (unsigned pin = 0; pin < GPIO_COUNT; pin++) {
        saved->mux[pin] = REG(LP_MUX0 + pin * 4U);
        saved->pin[pin] = REG(LP_PIN0 + pin * 4U);
#if defined(RTEMS_BOARD_ESP32P4)
        /* P4 puts wake enable/type at 0/1, and mux/function/input in
         * each LP IOMUX PAD word at 4/5/10, on both chip revisions.
         */
        uint32_t cfg = saved->pin[pin] & ~(BIT(0) | (7U << 1));
        if (mask & BIT(pin)) {
            uint32_t mux = saved->mux[pin] & ~((3U << 5) | BIT(9));
            /* Input-only in sleep too, regardless of the previous PAD
             * sleep settings: SLP_SEL/SLP_IE/SLP_OE are at 7/8/9.
             */
            REG(LP_MUX0 + pin * 4U) = mux | BIT(4) | (1U << 5) | BIT(7) | BIT(8) | BIT(10);
            cfg |= BIT(0) | ((uint32_t) (mode ? 5 : 4) << 1);
        }
#else
        uint32_t cfg = saved->pin[pin] & ~(BIT(10) | (7U << 7));
        if (mask & BIT(pin)) {
            uint32_t mux = saved->mux[pin] & ~(7U << 12);
#if !defined(RTEMS_BOARD_ESP32C6)
            mux |= BIT(12); /* GPIO function 1; C6 uses function 0. */
#endif
            REG(LP_MUX0 + pin * 4U) = mux | BIT(9);
            cfg |= BIT(10) | ((uint32_t) (mode ? 5 : 4) << 7);
        }
#endif
        REG(LP_PIN0 + pin * 4U) = cfg;
    }
#if !defined(RTEMS_BOARD_ESP32P4)
    REG(LP_MUX_SELECT) |= mask;
#endif
#endif
}

static void restore_gpio(uint32_t mask, const struct gpio_sleep_state *saved)
{
#if defined(RTEMS_BOARD_ESP32C3)
    (void) mask;
    REG(0x60008110U) = saved->select;
#elif defined(RTEMS_BOARD_ESP32H2)
    if (mask) {
        for (unsigned pin = 0; pin < GPIO_COUNT; pin++) {
            if (mask & BIT(pin)) {
                REG(SLEEP_IO_MUX_BASE + (pin + GPIO_RTCIO_OFFSET + 1U) * 4U) = saved->mux[pin];
            }
        }
        REG(LP_AON_GPIO_MUX) = saved->select;
        REG(LP_AON_GPIO_HOLD0) = saved->hold;
        REG(LP_AON_EXT_WAKEUP) = saved->wakeup;
    }
#else
#if !defined(RTEMS_BOARD_ESP32P4)
    REG(LP_MUX_SELECT) = saved->select;
#endif
    for (unsigned pin = 0; pin < GPIO_COUNT; pin++) {
        REG(LP_PIN0 + pin * 4U) = saved->pin[pin];
        if (mask & BIT(pin)) {
            REG(LP_MUX0 + pin * 4U) = saved->mux[pin];
        }
    }
#endif
}

int rtems_esp_sleep_start(bool deep, uint64_t usecs, uint32_t gpio_mask, int mode, uint32_t *cause)
{
    *cause = 0;
    if (rtems_esp_rtc_frequency() == 0) {
        return ENODEV;
    }
    if (usecs / 1000000ULL > ((1ULL << 48) - 1) / rtc_frequency) {
        return EINVAL;
    }
    pthread_mutex_lock(&sleep_lock);
#ifdef RTEMS_SMP
    cpu_set_t saved, boot;
    if (rtems_task_get_affinity(RTEMS_SELF, sizeof(saved), &saved) != RTEMS_SUCCESSFUL) {
        pthread_mutex_unlock(&sleep_lock);
        return ENOTSUP;
    }
    CPU_ZERO(&boot);
    CPU_SET(0, &boot);
    rtems_status_code rc = rtems_task_set_affinity(RTEMS_SELF, sizeof(boot), &boot);
    if (rc != RTEMS_SUCCESSFUL) {
        pthread_mutex_unlock(&sleep_lock);
        return ENOTSUP;
    }
#endif
    Per_CPU_Control *self = _Thread_Dispatch_disable();
#ifdef RTEMS_SMP
    struct sleep_rendezvous other = { .ready = false, .release = false };
    if (rtems_scheduler_get_processor_maximum() > 1 && _Processor_mask_Is_set(_SMP_Get_online_processors(), 1)) {
        other.cpu = _Per_CPU_Get_by_index(1);
        other.context.handler = park_cpu;
        other.context.arg = &other;
        other.job.context = &other.context;
        _Per_CPU_Submit_job(other.cpu, &other.job);
        while (!atomic_load_explicit(&other.ready, memory_order_acquire)) {
            __asm__ volatile("nop");
        }
    }
#endif
    rtems_interrupt_level level;
    rtems_interrupt_local_disable(level);
    rtems_interrupt_vector_disable(SYSTIMER_TARGET0_INTR);
    struct gpio_sleep_state gpio_state = { 0 };
    prepare_gpio(gpio_mask, mode, &gpio_state);
    uint32_t raw_cause = 0;
    int result;
    /* Wind up the 32-bit RTC timecounter at least every hour, even for an
     * indefinite GPIO wait. This also bounds wakeup watchdog processing.
     */
    const uint64_t hour = 3600000000ULL;
    uint64_t remaining = usecs;
    do {
        uint64_t interval = (deep || (remaining && remaining < hour)) ? remaining : hour;
        uint64_t before = rtems_esp_rtc_get_counter();
        /* Hardware preparation may consume the available interval. Program
         * the absolute alarm late and reject an expired deadline BEFORE
         * requesting sleep, never by rolling back an unresolved PMU entry.
         */
        result = rtems_esp_sleep_enter(deep, (interval ? TIMER_WAKE : 0) | (gpio_mask ? GPIO_WAKE : 0),
            interval, &raw_cause);
        uint64_t elapsed = rtems_esp_rtc_get_counter() - before;
        account_sleep(elapsed);
        if (result || (raw_cause & GPIO_WAKE) || deep || (remaining && remaining <= interval)) {
            break;
        }
        if (remaining) {
            remaining -= interval;
        }
    } while (true);
    restore_gpio(gpio_mask, &gpio_state);
    REG(SYSTIMER_BASE_ADDR + 0x6CU) = 1U; /* Acknowledge the missed periodic IRQ. */
    rtems_interrupt_vector_enable(SYSTIMER_TARGET0_INTR);
    rtems_interrupt_local_enable(level);
#ifdef RTEMS_SMP
    if (other.cpu) {
        atomic_store_explicit(&other.release, true, memory_order_release);
        _Per_CPU_Wait_for_job(other.cpu, &other.job);
    }
#endif
    _Thread_Dispatch_enable(self);
#ifdef RTEMS_SMP
    rtems_task_set_affinity(RTEMS_SELF, sizeof(saved), &saved);
#endif
    pthread_mutex_unlock(&sleep_lock);
    *cause = rtems_esp_sleep_decode_wakeup_cause(raw_cause);
    return result;
}
#endif
