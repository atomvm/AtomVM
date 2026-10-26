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

/* This fixture tests RTEMS clock/timer and SMP integration. It deliberately
 * replaces the unmodelled RTC/PMU hardware in esp-emu; it is never linked into
 * a normal firmware build and does not validate physical power transitions.
 */
#include <assert.h>
#include <errno.h>
#include <rtems/counter.h>
#include <rtems/score/cpuimpl.h>
#include <stdbool.h>
#include <stdint.h>

uint64_t rtems_esp_rtc_get_counter(void)
{
    return ((uint64_t) rtems_counter_read() * 136000) / rtems_counter_frequency();
}

int rtems_esp_sleep_enter(bool deep, uint32_t wakeup, uint64_t usecs, uint32_t *cause)
{
    assert(!deep);
    *cause = 0;
    if (usecs && usecs < 450) {
        return ETIMEDOUT;
    }
    /* Scheduler tick interrupts are masked, and the second CPU must already
     * be parked. The application timer must expire during this interval.
     */
    rtems_counter_delay_nanoseconds(50000000);
    *cause = wakeup;
    return 0;
}
