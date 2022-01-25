/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Riccardo Binetti <rbino@gmx.com>
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

#include <sys.h>

#include "defaultatoms.h"
#include <avmpack.h>
#include <gpiodriver.h>
#include <scheduler.h>

// Monotonically increasing number of milliseconds from reset
// Overflows every 49 days
// TODO: use 64 bit (remember to take into account atomicity)
static volatile uint32_t system_millis;

// Called when systick fires
void sys_tick_handler()
{
    system_millis++;
}

// Sleep for delay milliseconds
static void msleep(uint32_t delay)
{
    // TODO: use a smarter sleep instead of busy waiting
    uint32_t wake = system_millis + delay;
    while (wake > system_millis)
        ;
}

static inline void sys_clock_gettime(struct timespec *t)
{
    t->tv_sec = system_millis / 1000;
    t->tv_nsec = (system_millis % 1000) * 1000000;
}

static int32_t timespec_diff_to_ms(struct timespec *timespec1, struct timespec *timespec2)
{
    return (timespec1->tv_sec - timespec2->tv_sec) * 1000 + (timespec1->tv_nsec - timespec2->tv_nsec) / 1000000;
}

void sys_init_platform(GlobalContext *glb)
{
    UNUSED(glb);
}

void sys_consume_pending_events(GlobalContext *glb)
{
    UNUSED(glb);
}

void sys_set_timestamp_from_relative_to_abs(struct timespec *t, int32_t millis)
{
    sys_clock_gettime(t);
    t->tv_sec += millis / 1000;
    t->tv_nsec += (millis % 1000) * 1000000;
}

void sys_time(struct timespec *t)
{
    sys_clock_gettime(t);
}

uint32_t sys_millis()
{
    return system_millis;
}

void sys_start_millis_timer()
{
}

void sys_stop_millis_timer()
{
}

Module *sys_load_module(GlobalContext *global, const char *module_name)
{
    const void *beam_module = NULL;
    uint32_t beam_module_size = 0;

    struct ListHead *item;
    LIST_FOR_EACH (item, &global->avmpack_data) {
        struct AVMPackData *avmpack_data = (struct AVMPackData *) item;
        if (avmpack_find_section_by_name(avmpack_data->data, module_name, &beam_module, &beam_module_size)) {
            break;
        }
    }

    if (IS_NULL_PTR(beam_module)) {
        fprintf(stderr, "Failed to open module: %s\n", module_name);
        return NULL;
    }

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    new_module->module_platform_data = NULL;

    return new_module;
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    Context *new_ctx = context_new(glb);

    if (!strcmp(driver_name, "gpio")) {
        gpiodriver_init(new_ctx);
    } else {
        context_destroy(new_ctx);
        return NULL;
    }

    return new_ctx;
}

term sys_get_info(Context *ctx, term key)
{
    return UNDEFINED_ATOM;
}

void sys_sleep(GlobalContext *glb)
{
    UNUSED(glb);
}
