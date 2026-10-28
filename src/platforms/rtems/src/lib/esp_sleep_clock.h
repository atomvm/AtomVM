/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef AVM_RTEMS_ESP_SLEEP_CLOCK_H
#define AVM_RTEMS_ESP_SLEEP_CLOCK_H

#include <stdbool.h>
#include <stdint.h>

/* Private to the sleep implementation; keep this object in internal SRAM.
 * These are clock fields, not a backup of the peripheral/reset registers.
 */
struct esp_sleep_clock_state
{
    uint32_t source;
    uint32_t cpu;
    uint32_t ahb;
    uint32_t mem;
    uint32_t apb;
    uint32_t source_freq_mhz;
    uint32_t cpu_freq_mhz;
    uint32_t xtal_freq_mhz;
    uint32_t mpll_freq_mhz; /* P4/S31: zero when MPLL was disabled. */
};

/* With interrupts disabled and the other CPU parked, call prepare BEFORE
 * suspending the flash cache. false means unsupported clock configuration:
 * nothing was switched and finish must not be called. On success, finish
 * MUST run on every returning path (wake, rejection, or software timeout),
 * AFTER restoring PMU/BIAS, BEFORE resuming the flash cache or releasing
 * the other CPU/interrupts. finish and its complete call chain are SRAM/ROM,
 * including H2's shared CPU/flash PLL calibration. This also applies to a
 * returning deep-sleep attempt. The public prepare wrapper requires cache on.
 * P4 finish enables a saved MPLL, settles its rev3 cap, then calls the PMU's
 * p4_wait_ready eFuse barrier BEFORE MPLL configuration and CPU restoration.
 * P4 before_entry disables a saved MPLL for light sleep: no PSRAM or live
 * MPLL peripheral users are permitted (P4 flash never uses MPLL). Its BSP
 * frequency is adopted on the first prepare and must not be changed by
 * other code afterward; a powered PHY gate alone does not configure MPLL.
 *
 * Contract: the current RTEMS BSP's flash-only external-memory setup; no
 * initialized PSRAM. IDF's MSPI speed synchronization is a no-op for this
 * setup on all supported chips. PSRAM support requires its timing-tuning
 * state and low-speed switch BEFORE prepare, high-speed switch AFTER finish;
 * it cannot be inferred from the flash divider. PMU-chip prepare does not
 * software-disable PLLs (notably H2 FLASH_PLL).
 * C3 prepare DOES disable BBPLL, as IDF does: radio/USB/peripherals must be
 * quiesced first. S31 restores a previously enabled MPLL after PMU's eFuse
 * barrier, without the P4-specific software disable/cap-settling sequence.
 */
bool rtems_esp_sleep_clock_prepare(struct esp_sleep_clock_state *state);

/* Cache OFF, AFTER pmu_prepare, BEFORE pmu_before_entry and the parent's
 * final writeback.
 * P4 light sleep disables a saved MPLL here, not in prepare: PMU prepare's
 * rev3 bias workaround may reenable it. No-op on other chips/deep sleep.
 * SRAM/ROM only. finish is required even when the request is then aborted.
 */
void rtems_esp_sleep_clock_before_entry(const struct esp_sleep_clock_state *state, bool deep);
void rtems_esp_sleep_clock_finish(const struct esp_sleep_clock_state *state);

#endif
