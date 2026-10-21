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

#ifndef RTEMS_NPL_TEST_MOCK_RTEMS_H
#define RTEMS_NPL_TEST_MOCK_RTEMS_H
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
typedef uint32_t rtems_id;
typedef uint32_t rtems_option;
typedef uint32_t rtems_interval;
typedef uint32_t rtems_status_code;
typedef uint32_t rtems_interrupt_level;
#define RTEMS_INVALID_ID 0
#define RTEMS_BINARY_SEMAPHORE 1
#define RTEMS_SIMPLE_BINARY_SEMAPHORE 2
#define RTEMS_COUNTING_SEMAPHORE 4
#define RTEMS_PRIORITY 8
#define RTEMS_INHERIT_PRIORITY 16
#define RTEMS_SUCCESSFUL 0
#define RTEMS_NO_WAIT 0
#define RTEMS_WAIT 1
#define RTEMS_NO_TIMEOUT 0
static unsigned mock_task = 1;
static uint32_t mock_ticks;
static unsigned mock_obtains, mock_release_errors;
static void (*mock_wait_hook)(void);
static unsigned mock_wait_timeouts[1024];
static struct
{
    unsigned count, owner, attr;
} mock_sems[64];
static unsigned mock_next_id = 1;
static unsigned mock_timer_creates;
static bool mock_create_fail;
static bool mock_timer_arm_fail;
static void (*mock_release_hook)(void);
static unsigned mock_irq_disabled;
#define rtems_interrupt_disable(level) \
    do {                               \
        (level) = mock_irq_disabled;   \
        mock_irq_disabled = 1;         \
    } while (0)
#define rtems_interrupt_enable(level) \
    do {                              \
        mock_irq_disabled = (level);  \
    } while (0)
#define rtems_interrupt_local_disable(level) rtems_interrupt_disable(level)
#define rtems_interrupt_local_enable(level) rtems_interrupt_enable(level)
#define rtems_build_name(a, b, c, d) 0
static inline unsigned rtems_task_self(void)
{
    return mock_task;
}
static inline unsigned rtems_semaphore_create(unsigned n, unsigned c, unsigned a, unsigned p, unsigned *id)
{
    if (mock_create_fail) {
        return 1;
    }
    *id = mock_next_id++;
    mock_sems[*id].count = c;
    mock_sems[*id].attr = a;
    if ((a & RTEMS_BINARY_SEMAPHORE) && c == 0) {
        mock_sems[*id].owner = mock_task;
    }
    return 0;
}
static inline unsigned rtems_semaphore_delete(unsigned id) { return 0; }
static inline unsigned rtems_semaphore_release(unsigned id)
{
    assert(!mock_irq_disabled);
    if ((mock_sems[id].attr & RTEMS_BINARY_SEMAPHORE) && mock_sems[id].owner != mock_task) {
        mock_release_errors++;
        return 1;
    }
    mock_sems[id].owner = 0;
    if (mock_sems[id].attr & (RTEMS_BINARY_SEMAPHORE | RTEMS_SIMPLE_BINARY_SEMAPHORE)) {
        mock_sems[id].count = 1;
    } else {
        mock_sems[id].count++;
    }
    if (mock_release_hook) {
        mock_release_hook();
    }
    return 0;
}
static inline unsigned rtems_semaphore_obtain(unsigned id, unsigned o, unsigned t)
{
    mock_wait_timeouts[mock_obtains++] = t;
    if (mock_wait_hook) {
        mock_wait_hook();
    }
    if (!mock_sems[id].count) {
        if (t) {
            mock_ticks += t;
        }
        return 1;
    }
    mock_sems[id].count--;
    if (mock_sems[id].attr & RTEMS_BINARY_SEMAPHORE) {
        mock_sems[id].owner = mock_task;
    }
    return 0;
}
static inline unsigned rtems_clock_get_ticks_since_boot(void) { return mock_ticks; }
static inline unsigned rtems_task_wake_after(unsigned t) { return 0; }
static inline unsigned rtems_timer_create(unsigned n, unsigned *id)
{
    if (mock_create_fail) {
        return 1;
    }
    *id = ++mock_timer_creates;
    return 0;
}
static inline unsigned rtems_timer_cancel(unsigned id) { return 0; }
static inline unsigned rtems_timer_delete(unsigned id)
{
    assert(!mock_irq_disabled);
    return 0;
}
static inline unsigned rtems_timer_server_fire_after(unsigned id, unsigned t, void (*fn)(unsigned, void *), void *p) { return mock_timer_arm_fail ? 1 : 0; }
static inline bool rtems_interrupt_is_in_progress(void) { return false; }
#endif
