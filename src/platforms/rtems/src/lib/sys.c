/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Riccardo Binetti <rbino@gmx.com>
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

#include "rtems_sys.h"

#include <rtems.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#include <defaultatoms.h>
#include <utils.h>

// #define ENABLE_TRACE
#include <trace.h>

/* TODO: Needed because `defaultatoms_init` in libAtomVM/defaultatoms.c calls this function. */
void platform_defaultatoms_init(GlobalContext *glb)
{
    UNUSED(glb);
}

void sys_init_platform(GlobalContext *glb)
{
    struct RTEMSPlatformData *platform = malloc(sizeof(struct RTEMSPlatformData));
    if (IS_NULL_PTR(platform)) {
        AVM_ABORT();
    }
    platform->dummy = 0;
    glb->platform_data = platform;
}

void sys_free_platform(GlobalContext *glb)
{
    free(glb->platform_data);
    glb->platform_data = NULL;
}

static rtems_interval timeout_ms_to_ticks(int timeout_ms)
{
    rtems_interval ticks_per_second = rtems_clock_get_ticks_per_second();
    rtems_interval ticks = ((rtems_interval) timeout_ms * ticks_per_second + 999U) / 1000U;
    if (ticks == 0) {
        ticks = 1;
    }
    return ticks;
}

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
    UNUSED(glb);

    if (timeout_ms == SYS_POLL_EVENTS_DO_NOT_WAIT) {
        return;
    }

    rtems_interval ticks;
    if (timeout_ms == SYS_POLL_EVENTS_WAIT_FOREVER) {
        ticks = timeout_ms_to_ticks(100);
    } else {
        ticks = timeout_ms_to_ticks(timeout_ms);
    }
    rtems_task_wake_after(ticks);
}

void sys_listener_destroy(struct ListHead *item)
{
    UNUSED(item);
}

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

void sys_time(struct timespec *t)
{
    rtems_clock_get_realtime(t);
}

void sys_monotonic_time(struct timespec *t)
{
    rtems_clock_get_monotonic(t);
}

uint64_t sys_monotonic_time_u64(void)
{
    struct timespec ts;
    sys_monotonic_time(&ts);
    return ((uint_least64_t) ts.tv_sec * 1000000000) + (uint_least64_t) ts.tv_nsec;
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms)
{
    return ms * 1000000;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t t)
{
    return t / 1000000;
}

enum OpenAVMResult sys_open_avm_from_file(GlobalContext *global, const char *path, struct AVMPackData **data)
{
    UNUSED(global);
    UNUSED(path);
    UNUSED(data);
    TRACE("sys_open_avm_from_file: Going to open: %s\n", path);
    return AVM_OPEN_NOT_SUPPORTED;
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    UNUSED(global);
    UNUSED(path);
    return NULL;
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    return port_driver_create_port(driver_name, glb, opts);
}

term sys_get_info(Context *ctx, term key)
{
    UNUSED(ctx);
    UNUSED(key);
    return UNDEFINED_ATOM;
}
