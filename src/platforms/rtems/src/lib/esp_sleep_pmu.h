/*
 * This file is part of AtomVM.
 * Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */
#ifndef AVM_RTEMS_ESP_SLEEP_PMU_H
#define AVM_RTEMS_ESP_SLEEP_PMU_H

#include <stdbool.h>
#include <stdint.h>

struct rtems_esp_sleep_pmu_state
{
    uint32_t minimum_entry_us;
    uint32_t slow_period; /* selected RTC_SLOW period, microseconds in Q19 */
    uint32_t fast_period; /* calibrated RC_FAST period, microseconds in Q19 */
    uint32_t active_bias;
    uint32_t active_regulator;
    uint32_t lp_active_regulator;
    uint32_t lp_active_driver;
    uint32_t chip_revision;
    uint32_t c3_analog;
    uint32_t c3_ulp;
    bool deep;
    bool prepared;
    bool entry_prepared;
};

/* Serialized with all RTC/analog-I2C/TIMG0 calibration users, interrupts off and
 * other CPU parked. Call with cache ON, AFTER the clock helper selects XTAL.
 * xtal_mhz is the clock helper's validated crystal frequency. Both selected
 * RTC_SLOW and RC_FAST (PMU work clock) are calibrated here on every attempt.
 * P4 caller must hand PVT regulation to PMU after switching to XTAL, and
 * reenable it after restoring clocks/cache on every returning path.
 * Baseline: no modem/PSRAM or other chips' PVT; TOP/CPU/peripherals/flash are
 * retained in light sleep, no retention DMA, RTC sources unchanged. Returns errno.
 * On error no finish is needed. The state must live in internal SRAM.
 */
int rtems_esp_sleep_pmu_prepare(struct rtems_esp_sleep_pmu_state *state,
    bool deep, uint32_t wakeup, uint32_t xtal_mhz);

/* Cache OFF and CPU on XTAL. Call after clock/MSPI save and immediately before
 * the parent's final deadline check and sleep-enable. Does NOT request sleep.
 * P4 performs pad selection and deep DCDC->LDO handoff here. Caller owns P4
 * register-only L1 writeback immediately before sleep-enable, after ALL
 * deadline/alarm accesses and stack spills (including return from here).
 */
void rtems_esp_sleep_pmu_before_entry(struct rtems_esp_sleep_pmu_state *state);

/* Call on EVERY successful prepare, including a pre-entry deadline abort.
 * sleep_requested=false: no request was issued (even if before_entry ran).
 * sleep_requested=true: parent observed wake/reject; rejected describes it.
 * Call cache OFF, BEFORE clock/MSPI restoration, then resume caches only after
 * clock restoration. Deep success never returns; a returning deep request is
 * rejected. Idempotent. No interrupt/cause clearing is performed here.
 * P4 clock_finish must call p4_wait_ready below after MPLL enable/cap settling,
 * before MPLL frequency and CPU restoration. PMU finish does not wait for P4
 * eFuse (it would otherwise run before MPLL had been reenabled).
 */
void rtems_esp_sleep_pmu_finish(struct rtems_esp_sleep_pmu_state *state,
    bool sleep_requested, bool rejected);

/* SRAM/ROM only. P4 clock_finish calls this after MPLL enable + rev3 cap=3
 * + 10 us, before MPLL configure/CPU restore. Releases pad sleep selection,
 * then waits for eFuse IDLE.
 * No-op on other chips. Call on abort/error too: pad-low is idempotent.
 */
void rtems_esp_sleep_pmu_p4_wait_ready(void);

/* SRAM/ROM only. Apply the ECO0 CPLL/SYSPLL OC_DIV correction AFTER MPLL
 * configuration, BEFORE restoring the saved CPU clock, in IDF order.
 */
void rtems_esp_sleep_pmu_p4_restore_pll_defaults(void);

#endif
