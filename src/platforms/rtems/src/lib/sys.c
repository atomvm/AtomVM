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

#include <errno.h>
#include <inttypes.h>
#include <pthread.h>
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
#endif
#ifndef AVM_NO_SMP
    pthread_mutex_init(&platform->poll_mutex, NULL);
    pthread_condattr_t cv_attr;
    pthread_condattr_init(&cv_attr);
    pthread_condattr_setclock(&cv_attr, CLOCK_MONOTONIC);
    pthread_cond_init(&platform->poll_cv, &cv_attr);
    pthread_condattr_destroy(&cv_attr);
#else
    platform->poll_task = rtems_task_self();
#endif
    platform->signaled = false;

#if defined(HAVE_MBEDTLS)
#ifndef AVM_NO_SMP
    platform->entropy_mutex = smp_mutex_create();
    platform->random_mutex = smp_mutex_create();
#endif
    platform->entropy_is_initialized = false;
    platform->random_is_initialized = false;
#endif

    glb->platform_data = platform;
}

void sys_free_platform(GlobalContext *glb)
{
    struct RTEMSPlatformData *platform = glb->platform_data;
    if (platform != NULL) {
#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
        free(platform->fds);
#endif
#if defined(HAVE_MBEDTLS)
#ifndef AVM_NO_SMP
        smp_mutex_destroy(platform->entropy_mutex);
        smp_mutex_destroy(platform->random_mutex);
#endif
#endif
#ifndef AVM_NO_SMP
        pthread_cond_destroy(&platform->poll_cv);
        pthread_mutex_destroy(&platform->poll_mutex);
#endif
        free(platform);
        glb->platform_data = NULL;
    }
}

#ifdef AVM_NO_SMP
static rtems_interval timeout_ms_to_ticks(int timeout_ms)
{
    rtems_interval ticks_per_second = rtems_clock_get_ticks_per_second();
    rtems_interval ticks = ((rtems_interval) timeout_ms * ticks_per_second + 999U) / 1000U;
    if (ticks == 0) {
        ticks = 1;
    }
    return ticks;
}
#endif

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

static void sys_poll_events_wait(struct RTEMSPlatformData *platform, int timeout_ms)
{
#ifndef AVM_NO_SMP
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    int wait_ms = (timeout_ms < 0) ? 100 : timeout_ms;
    ts.tv_sec += wait_ms / 1000;
    ts.tv_nsec += (long) (wait_ms % 1000) * 1000000L;
    if (ts.tv_nsec >= 1000000000L) {
        ts.tv_sec += 1;
        ts.tv_nsec -= 1000000000L;
    }
    if (!platform->signaled) {
        (void) pthread_cond_timedwait(&platform->poll_cv, &platform->poll_mutex, &ts);
    }
    platform->signaled = false;
#else
    rtems_interval ticks;
    if (timeout_ms < 0) {
        ticks = timeout_ms_to_ticks(100);
    } else {
        ticks = timeout_ms_to_ticks(timeout_ms);
    }
    (void) rtems_event_transient_receive(RTEMS_WAIT, ticks);
    platform->signaled = false;
#endif
}

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
        sys_poll_events_wait(platform, timeout_ms);
        return;
    }

    int effective_timeout = timeout_ms;
    if (effective_timeout < 0 || effective_timeout > 50) {
        effective_timeout = 50;
    }
#ifndef AVM_NO_SMP
    pthread_mutex_unlock(&platform->poll_mutex);
#endif
    int nb_descriptors = poll(fds, select_events_poll_count, effective_timeout);
#ifndef AVM_NO_SMP
    pthread_mutex_lock(&platform->poll_mutex);
#endif
    if (timeout_ms != SYS_POLL_EVENTS_DO_NOT_WAIT) {
        platform->signaled = false;
    }
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
    struct RTEMSPlatformData *platform = glb->platform_data;
    (void) platform;

#ifndef AVM_NO_SMP
    if (timeout_ms == SYS_POLL_EVENTS_DO_NOT_WAIT) {
        if (pthread_mutex_trylock(&platform->poll_mutex) != 0) {
            return;
        }
    } else {
        pthread_mutex_lock(&platform->poll_mutex);
        if (platform->signaled) {
            platform->signaled = false;
            timeout_ms = SYS_POLL_EVENTS_DO_NOT_WAIT;
        }
    }
#endif

#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
    if (timeout_ms == SYS_POLL_EVENTS_DO_NOT_WAIT && synclist_is_empty(&glb->select_events)) {
#ifndef AVM_NO_SMP
        pthread_mutex_unlock(&platform->poll_mutex);
#endif
        return;
    }
    sys_poll_events_with_poll(glb, timeout_ms);
    gpio_interrupt_drain(glb);
#else
    if (timeout_ms != SYS_POLL_EVENTS_DO_NOT_WAIT) {
        sys_poll_events_wait(platform, timeout_ms);
    }
    gpio_interrupt_drain(glb);
#endif

#ifndef AVM_NO_SMP
    pthread_mutex_unlock(&platform->poll_mutex);
#endif
}

#if !defined(AVM_NO_SMP) || defined(AVM_TASK_DRIVER_ENABLED)
void sys_signal(GlobalContext *glb)
{
    struct RTEMSPlatformData *platform = glb->platform_data;
    if (IS_NULL_PTR(platform)) {
        return;
    }
    platform->signaled = true;
#ifndef AVM_NO_SMP
    pthread_cond_signal(&platform->poll_cv);
#else
    rtems_event_transient_send(platform->poll_task);
#endif
}
#endif

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
#if !defined(AVM_NO_SMP) || defined(AVM_TASK_DRIVER_ENABLED)
    sys_signal(global);
#endif
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
#if !defined(AVM_NO_SMP) || defined(AVM_TASK_DRIVER_ENABLED)
    sys_signal(global);
#endif
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

#if defined(RTEMS_BOARD_ESP32)
/* Feature bits match the values exposed by ESP-IDF's esp_chip_info(). */
#define RTEMS_CHIP_FEATURE_EMB_FLASH (1U << 0)
#define RTEMS_CHIP_FEATURE_WIFI_BGN (1U << 1)
#define RTEMS_CHIP_FEATURE_BLE (1U << 4)
#define RTEMS_CHIP_FEATURE_BT (1U << 5)

static term chip_model_atom(GlobalContext *glb)
{
#if defined(RTEMS_BOARD_ESP32C3)
    return globalcontext_make_atom(glb, ATOM_STR("\x8", "esp32_c3"));
#elif defined(RTEMS_BOARD_ESP32C5)
    return globalcontext_make_atom(glb, ATOM_STR("\x8", "esp32_c5"));
#elif defined(RTEMS_BOARD_ESP32C6)
    return globalcontext_make_atom(glb, ATOM_STR("\x8", "esp32_c6"));
#elif defined(RTEMS_BOARD_ESP32H2)
    return globalcontext_make_atom(glb, ATOM_STR("\x8", "esp32_h2"));
#elif defined(RTEMS_BOARD_ESP32S31)
    return globalcontext_make_atom(glb, ATOM_STR("\x9", "esp32_s31"));
#elif defined(RTEMS_BOARD_ESP32P4)
    return globalcontext_make_atom(glb, ATOM_STR("\x8", "esp32_p4"));
#else
#error "Unsupported RTEMS ESP32 board"
#endif
}

static int chip_cores(void)
{
#if defined(RTEMS_BOARD_ESP32S31) || defined(RTEMS_BOARD_ESP32P4)
    return 2;
#else
    return 1;
#endif
}

static unsigned int chip_features(void)
{
#if defined(RTEMS_BOARD_ESP32P4)
    return 0;
#elif defined(RTEMS_BOARD_ESP32H2)
    /* Bluetooth LE only: this part has no Wi-Fi, and the port has no 802.15.4
     * driver to advertise. */
    return RTEMS_CHIP_FEATURE_BLE;
#else
    unsigned int features = RTEMS_CHIP_FEATURE_WIFI_BGN | RTEMS_CHIP_FEATURE_BLE;
#if defined(RTEMS_BOARD_ESP32S31)
    features |= RTEMS_CHIP_FEATURE_BT;
#endif
    return features;
#endif
}

static term chip_features_to_term(Context *ctx)
{
    GlobalContext *glb = ctx->global;
    unsigned int features = chip_features();
    term ret = term_nil();

    if (features & RTEMS_CHIP_FEATURE_EMB_FLASH) {
        ret = term_list_prepend(globalcontext_make_atom(glb, ATOM_STR("\x9", "emb_flash")), ret, &ctx->heap);
    }
    if (features & RTEMS_CHIP_FEATURE_WIFI_BGN) {
        ret = term_list_prepend(globalcontext_make_atom(glb, ATOM_STR("\x3", "bgn")), ret, &ctx->heap);
    }
    if (features & RTEMS_CHIP_FEATURE_BLE) {
        ret = term_list_prepend(globalcontext_make_atom(glb, ATOM_STR("\x3", "ble")), ret, &ctx->heap);
    }
    if (features & RTEMS_CHIP_FEATURE_BT) {
        ret = term_list_prepend(globalcontext_make_atom(glb, ATOM_STR("\x2", "bt")), ret, &ctx->heap);
    }

    return ret;
}

static term esp32_chip_info(Context *ctx)
{
    GlobalContext *glb = ctx->global;

    if (UNLIKELY(memory_ensure_free(ctx, term_map_size_in_terms(3) + 3 * 2) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }

    term ret = term_alloc_map(3, &ctx->heap);
    term_set_map_assoc(ret, 0, globalcontext_make_atom(glb, ATOM_STR("\x5", "cores")), term_from_int4(chip_cores()));
    term_set_map_assoc(ret, 1, globalcontext_make_atom(glb, ATOM_STR("\x8", "features")), chip_features_to_term(ctx));
    term_set_map_assoc(ret, 2, globalcontext_make_atom(glb, ATOM_STR("\x5", "model")), chip_model_atom(glb));
    return ret;
}
#endif

term sys_get_info(Context *ctx, term key)
{
#if defined(RTEMS_BOARD_ESP32)
    if (key == globalcontext_make_atom(ctx->global, ATOM_STR("\xF", "esp32_chip_info"))) {
        return esp32_chip_info(ctx);
    }
#endif
    UNUSED(ctx);
    UNUSED(key);
    return UNDEFINED_ATOM;
}

#if defined(HAVE_MBEDTLS)
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>
#include <sys_mbedtls.h>

#if defined(RTEMS_BOARD_ESP32)
static int esp32_hardware_poll(void *data, unsigned char *output, size_t len, size_t *olen)
{
    (void) data;
#if defined(RTEMS_BOARD_ESP32P4)
    uintptr_t rng_reg = 0x501101A4U; /* LP_SYSTEM_REG_RNG_DATA_REG */
#elif defined(RTEMS_BOARD_ESP32S31)
    uintptr_t rng_reg = 0x2081403CU; /* TRNG_SW_READ_REG */
#elif defined(RTEMS_BOARD_ESP32C6) || defined(RTEMS_BOARD_ESP32C5)
    uintptr_t rng_reg = 0x600B2808U;
#elif defined(RTEMS_BOARD_ESP32H2)
    /* LPPERI_RNG_DATA_REG: this part's LPPERI date moves it one word up. */
    uintptr_t rng_reg = 0x600B280CU;
#else
    uintptr_t rng_reg = 0x600260B0U;
#endif
    for (size_t i = 0; i < len; i += 4) {
        uint32_t r = *(volatile uint32_t *) rng_reg;
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
    struct RTEMSPlatformData *platform = global->platform_data;
    SMP_MUTEX_LOCK(platform->entropy_mutex);

    if (!platform->entropy_is_initialized) {
        mbedtls_entropy_init(&platform->entropy_ctx);
#if defined(RTEMS_BOARD_ESP32)
        mbedtls_entropy_add_source(&platform->entropy_ctx, esp32_hardware_poll, NULL, 32, MBEDTLS_ENTROPY_SOURCE_STRONG);
#endif
        platform->entropy_is_initialized = true;
    }
    return &platform->entropy_ctx;
}

void sys_mbedtls_entropy_context_unlock(GlobalContext *global)
{
    struct RTEMSPlatformData *platform = global->platform_data;
    (void) platform;
    SMP_MUTEX_UNLOCK(platform->entropy_mutex);
}

mbedtls_ctr_drbg_context *sys_mbedtls_get_ctr_drbg_context_lock(GlobalContext *global)
{
    struct RTEMSPlatformData *platform = global->platform_data;
    SMP_MUTEX_LOCK(platform->random_mutex);

    if (!platform->random_is_initialized) {
        mbedtls_ctr_drbg_init(&platform->random_ctx);
        mbedtls_entropy_context *entropy = sys_mbedtls_get_entropy_context_lock(global);
        sys_mbedtls_entropy_context_unlock(global);

        const char *seed = "AtomVM RTEMS initial entropy seed";
        int err = mbedtls_ctr_drbg_seed(&platform->random_ctx, sys_mbedtls_entropy_func, entropy,
            (const unsigned char *) seed, strlen(seed));
        if (err != 0) {
            abort();
        }
        platform->random_is_initialized = true;
    }
    return &platform->random_ctx;
}

void sys_mbedtls_ctr_drbg_context_unlock(GlobalContext *global)
{
    struct RTEMSPlatformData *platform = global->platform_data;
    (void) platform;
    SMP_MUTEX_UNLOCK(platform->random_mutex);
}
#endif
