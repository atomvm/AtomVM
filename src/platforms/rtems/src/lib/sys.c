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
#include <resources.h>
#include <utils.h>

#ifdef RTEMS_HAS_LIBBSD
#include <poll.h>
#endif

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
#ifdef RTEMS_HAS_LIBBSD
    platform->fds = NULL;
    platform->select_events_poll_count = -1;
#else
    platform->dummy = 0;
#endif
    glb->platform_data = platform;
}

void sys_free_platform(GlobalContext *glb)
{
#ifdef RTEMS_HAS_LIBBSD
    struct RTEMSPlatformData *platform = glb->platform_data;
    free(platform->fds);
#endif
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

#ifdef RTEMS_HAS_LIBBSD
static void sys_poll_events_with_poll(GlobalContext *glb, int timeout_ms)
{
    struct RTEMSPlatformData *platform = glb->platform_data;
    struct pollfd *fds = platform->fds;
    int select_events_poll_count = platform->select_events_poll_count;
    int fd_index;

    if (fds == NULL || select_events_poll_count < 0) {
        struct ListHead *select_events = synclist_wrlock(&glb->select_events);
        size_t select_events_new_count;
        if (select_events_poll_count < 0) {
            select_event_count_and_destroy_closed(select_events, NULL, NULL, &select_events_new_count, glb);
        } else {
            select_events_new_count = select_events_poll_count;
        }

        fds = realloc(fds, sizeof(struct pollfd) * (select_events_new_count == 0 ? 1 : select_events_new_count));
        platform->fds = fds;

        fd_index = 0;
        struct ListHead *item;
        LIST_FOR_EACH (item, select_events) {
            struct SelectEvent *select_event = GET_LIST_ENTRY(item, struct SelectEvent, head);
            if (select_event->read || select_event->write) {
                fds[fd_index].fd = select_event->event;
                fds[fd_index].events = (select_event->read ? POLLIN : 0) | (select_event->write ? POLLOUT : 0);
                fds[fd_index].revents = 0;
                fd_index++;
            }
        }
        synclist_unlock(&glb->select_events);

        select_events_poll_count = select_events_new_count;
        platform->select_events_poll_count = select_events_new_count;
    }

    if (select_events_poll_count == 0) {
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
        return;
    }

    int nb_descriptors = poll(fds, select_events_poll_count, timeout_ms);
    if (nb_descriptors <= 0) {
        return;
    }

    fd_index = 0;
    for (int i = 0; i < select_events_poll_count && nb_descriptors > 0; i++, fd_index++) {
        if (!(fds[fd_index].revents & (fds[fd_index].events | POLLHUP | POLLERR))) {
            continue;
        }
        bool is_read = fds[fd_index].revents & (POLLIN | POLLHUP | POLLERR);
        bool is_write = fds[fd_index].revents & (POLLOUT | POLLERR);
        fds[fd_index].revents = 0;
        nb_descriptors--;

        select_event_notify(fds[fd_index].fd, is_read, is_write, glb);
    }
}
#endif

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
#ifdef RTEMS_HAS_LIBBSD
    if (timeout_ms == SYS_POLL_EVENTS_DO_NOT_WAIT && synclist_is_empty(&glb->select_events)) {
        return;
    }
    sys_poll_events_with_poll(glb, timeout_ms);
#else
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
#endif
}

void sys_listener_destroy(struct ListHead *item)
{
    UNUSED(item);
}

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
#ifdef RTEMS_HAS_LIBBSD
    UNUSED(event);
    UNUSED(is_write);
    struct RTEMSPlatformData *platform = global->platform_data;
    platform->select_events_poll_count = -1;
#else
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
#endif
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
#ifdef RTEMS_HAS_LIBBSD
    UNUSED(event);
    UNUSED(is_write);
    struct RTEMSPlatformData *platform = global->platform_data;
    platform->select_events_poll_count = -1;
#else
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
#endif
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
