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

#ifndef AVM_RTEMS_ESP_SLEEP_CACHE_H
#define AVM_RTEMS_ESP_SLEEP_CACHE_H

#include <stdint.h>

/* ESP-IDF rv_utils.h: speculative fetches must stop on both harts before
 * suspending the external-memory cache. Always inline into the SRAM caller.
 */
static inline __attribute__((always_inline)) uint32_t sleep_disable_branch_prediction(void)
{
#if defined(RTEMS_BOARD_ESP32C5) || defined(RTEMS_BOARD_ESP32P4) || defined(RTEMS_BOARD_ESP32S31)
    uint32_t previous;
    const uint32_t mask = 0x1030U; /* MHCR RAS, JAL prediction and BPU. */
    __asm__ volatile(
        ".option push\n.option arch, +zicsr\n"
        "csrrc %0, 0x7c1, %1\n.option pop"
        : "=r"(previous)
        : "r"(mask)
        : "memory");
    return previous & mask;
#else
    return 0;
#endif
}

static inline __attribute__((always_inline)) void sleep_restore_branch_prediction(uint32_t saved)
{
#if defined(RTEMS_BOARD_ESP32C5) || defined(RTEMS_BOARD_ESP32P4) || defined(RTEMS_BOARD_ESP32S31)
    __asm__ volatile(
        ".option push\n.option arch, +zicsr\n"
        "csrs 0x7c1, %0\n.option pop"
        :
        : "r"(saved)
        : "memory");
#else
    (void) saved;
#endif
}

#endif
