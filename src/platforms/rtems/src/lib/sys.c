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

#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
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
#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
    platform->fds = NULL;
    platform->select_events_poll_count = -1;
#else
    platform->dummy = 0;
#endif
    glb->platform_data = platform;
}

void sys_free_platform(GlobalContext *glb)
{
#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
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

#if defined(RTEMS_HAS_LWIP)
#include <sys/select.h>

int poll(struct pollfd fds[], nfds_t nfds, int timeout)
{
    fd_set readfds, writefds, exceptfds;
    FD_ZERO(&readfds);
    FD_ZERO(&writefds);
    FD_ZERO(&exceptfds);
    int maxfd = -1;
    for (nfds_t i = 0; i < nfds; i++) {
        fds[i].revents = 0;
        if (fds[i].fd < 0) {
            continue;
        }
        if (fds[i].events & (POLLIN | POLLRDNORM | POLLRDBAND)) {
            FD_SET(fds[i].fd, &readfds);
        }
        if (fds[i].events & (POLLOUT | POLLWRNORM | POLLWRBAND)) {
            FD_SET(fds[i].fd, &writefds);
        }
        if (fds[i].events & (POLLPRI | POLLERR)) {
            FD_SET(fds[i].fd, &exceptfds);
        }
        if (fds[i].fd > maxfd) {
            maxfd = fds[i].fd;
        }
    }
    struct timeval tv, *tvp = NULL;
    if (timeout >= 0) {
        tv.tv_sec = timeout / 1000;
        tv.tv_usec = (timeout % 1000) * 1000;
        tvp = &tv;
    }
    int res = select(maxfd + 1, &readfds, &writefds, &exceptfds, tvp);
    if (res <= 0) {
        return res;
    }
    int count = 0;
    for (nfds_t i = 0; i < nfds; i++) {
        if (fds[i].fd < 0) {
            continue;
        }
        if (FD_ISSET(fds[i].fd, &readfds)) {
            fds[i].revents |= POLLIN;
        }
        if (FD_ISSET(fds[i].fd, &writefds)) {
            fds[i].revents |= POLLOUT;
        }
        if (FD_ISSET(fds[i].fd, &exceptfds)) {
            fds[i].revents |= POLLERR;
        }
        if (fds[i].revents != 0) {
            count++;
        }
    }
    return count;
}
#endif

#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
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
        rtems_event_set out;
        rtems_event_receive(RTEMS_EVENT_0, RTEMS_WAIT | RTEMS_EVENT_ANY, ticks, &out);
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
    gpio_interrupt_drain(glb);
#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
    if (timeout_ms == SYS_POLL_EVENTS_DO_NOT_WAIT && synclist_is_empty(&glb->select_events)) {
        return;
    }
    sys_poll_events_with_poll(glb, timeout_ms);
    gpio_interrupt_drain(glb);
#else
    if (timeout_ms == SYS_POLL_EVENTS_DO_NOT_WAIT) {
        return;
    }

    rtems_interval ticks;
    if (timeout_ms == SYS_POLL_EVENTS_WAIT_FOREVER) {
        ticks = timeout_ms_to_ticks(100);
    } else {
        ticks = timeout_ms_to_ticks(timeout_ms);
    }
    rtems_event_set out;
    rtems_event_receive(RTEMS_EVENT_0, RTEMS_WAIT | RTEMS_EVENT_ANY, ticks, &out);
    gpio_interrupt_drain(glb);
#endif
}

void sys_listener_destroy(struct ListHead *item)
{
    UNUSED(item);
}

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
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
#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
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

#if defined(HAVE_MBEDTLS)
#include <sys_mbedtls.h>
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>

#if defined(RTEMS_BOARD_ESP32C3)
static int esp32c3_hardware_poll(void *data, unsigned char *output, size_t len, size_t *olen)
{
    (void) data;
    for (size_t i = 0; i < len; i += 4) {
        uint32_t r = *(volatile uint32_t *) 0x600260B0;
        size_t chunk = (len - i < 4) ? (len - i) : 4;
        memcpy(output + i, &r, chunk);
    }
    *olen = len;
    return 0;
}
#endif

int sys_mbedtls_entropy_func(void *entropy, unsigned char *buf, size_t size)
{
    return mbedtls_entropy_func(entropy, buf, size);
}

mbedtls_entropy_context *sys_mbedtls_get_entropy_context_lock(GlobalContext *global)
{
    (void) global;
    static mbedtls_entropy_context entropy_ctx;
    static bool entropy_initialized = false;
    if (!entropy_initialized) {
        mbedtls_entropy_init(&entropy_ctx);
#if defined(RTEMS_BOARD_ESP32C3)
        mbedtls_entropy_add_source(&entropy_ctx, esp32c3_hardware_poll, NULL, 32, MBEDTLS_ENTROPY_SOURCE_STRONG);
#endif
        entropy_initialized = true;
    }
    return &entropy_ctx;
}

void sys_mbedtls_entropy_context_unlock(GlobalContext *global)
{
    (void) global;
}

mbedtls_ctr_drbg_context *sys_mbedtls_get_ctr_drbg_context_lock(GlobalContext *global)
{
    (void) global;
    static mbedtls_ctr_drbg_context ctr_drbg_ctx;
    static bool drbg_initialized = false;
    if (!drbg_initialized) {
        mbedtls_ctr_drbg_init(&ctr_drbg_ctx);
        mbedtls_entropy_context *entropy = sys_mbedtls_get_entropy_context_lock(global);
        const char *seed = "AtomVM RTEMS initial entropy seed";
        int err = mbedtls_ctr_drbg_seed(&ctr_drbg_ctx, sys_mbedtls_entropy_func, entropy,
                                        (const unsigned char *) seed, strlen(seed));
        if (err != 0) {
            abort();
        }
        drbg_initialized = true;
    }
    return &ctr_drbg_ctx;
}

void sys_mbedtls_ctr_drbg_context_unlock(GlobalContext *global)
{
    (void) global;
}
#endif
