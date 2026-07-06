/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
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

#include <avmpack.h>
#include <defaultatoms.h>
#include <scheduler.h>
#include <sys.h>
// #define ENABLE_TRACE
#include <trace.h>

#include <zephyr/kernel.h>
#include <stdlib.h>
#include "zephyros_sys.h"
#include "avm_log.h"
#include "platform_atomic.h"

#include "../../../../libAtomVM/resources.h"

#if defined(CONFIG_NET_SOCKETS)
#include <poll.h>
#include <sys/select.h>
#endif

#if defined(CONFIG_EVENTFD)
#include <sys/eventfd.h>
#include <unistd.h>
#endif




static Context *port_driver_create_port(const char *port_name, GlobalContext *global, term opts);

#if defined(CONFIG_NET_SOCKETS)
static void event_listener_add_to_polling_set(struct EventListener *listener, GlobalContext *glb)
{
    UNUSED(listener);
    struct ZephyrPlatformData *platform = glb->platform_data;
    if (platform) {
        platform->listeners_poll_count = -1;
    }
}

static void listener_event_remove_from_polling_set(listener_event_t event, GlobalContext *glb)
{
    UNUSED(event);
    struct ZephyrPlatformData *platform = glb->platform_data;
    if (platform) {
        platform->listeners_poll_count = -1;
    }
}

static bool event_listener_is_event(struct EventListener *listener, listener_event_t event)
{
    return listener->fd == event;
}

#include <listeners.h>
#endif

#define TAG "sys"

struct PortDriverDefListItem *port_driver_list;
struct NifCollectionDefListItem *nif_collection_list;

static inline void platform_clock_gettime(struct timespec *t)
{
    uint64_t now = sys_monotonic_time_u64();
    t->tv_sec = (time_t) now / 1000;
    t->tv_nsec = ((int32_t) now % 1000) * 1000000;
}

static int32_t timespec_diff_to_ms(struct timespec *timespec1, struct timespec *timespec2)
{
    return (int32_t) ((timespec1->tv_sec - timespec2->tv_sec) * 1000 + (timespec1->tv_nsec - timespec2->tv_nsec) / 1000000);
}

/* TODO: Needed because `defaultatoms_init` in libAtomVM/defaultatoms.c calls this function.
 * We should be able to remove this after `platform_defaulatoms.{c,h}` are removed on all platforms
 * and `defaultatoms_init` is no longer called.
 */
void platform_defaultatoms_init(GlobalContext *glb)
{
    UNUSED(glb);
}

void sys_init_platform(GlobalContext *glb)
{
    struct ZephyrPlatformData *platform = malloc(sizeof(struct ZephyrPlatformData));
    if (UNLIKELY(!platform)) {
        AVM_ABORT();
    }
#if defined(CONFIG_NET_SOCKETS)
    platform->fds = NULL;
    platform->listeners_poll_count = -1;
    platform->select_events_poll_count = -1;
#endif
#if defined(CONFIG_EVENTFD)
    platform->signal_fd = eventfd(0, EFD_NONBLOCK);
    if (platform->signal_fd < 0) {
        AVM_LOGE(TAG, "Failed to create eventfd");
        AVM_ABORT();
    }
#endif
    platform->zephyr_mounted_fs_resource_type = NULL;
    glb->platform_data = platform;
}

void sys_free_platform(GlobalContext *glb)
{
    struct ZephyrPlatformData *platform = glb->platform_data;
    if (platform) {
#if defined(CONFIG_EVENTFD)
        if (platform->signal_fd >= 0) {
            close(platform->signal_fd);
        }
#endif
#if defined(CONFIG_NET_SOCKETS)
        free(platform->fds);
#endif
        free(platform);
        glb->platform_data = NULL;
    }
}

void sys_signal(GlobalContext *glb)
{
#if defined(CONFIG_EVENTFD)
    struct ZephyrPlatformData *platform = glb->platform_data;
    if (platform && platform->signal_fd >= 0) {
        eventfd_t val = 1;
        (void) eventfd_write(platform->signal_fd, val);
    }
#else
    UNUSED(glb);
#endif
}

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
#if defined(CONFIG_NET_SOCKETS)
    struct ZephyrPlatformData *platform = glb->platform_data;
    if (UNLIKELY(!platform)) {
        return;
    }

    struct pollfd *fds = platform->fds;
    int listeners_poll_count = platform->listeners_poll_count;
    int select_events_poll_count = platform->select_events_poll_count;

    int signal_poll_count = 0;
#if defined(CONFIG_EVENTFD)
    signal_poll_count = 1;
#endif

    int fd_index;

    if (listeners_poll_count < 0 || select_events_poll_count < 0) {
        struct ListHead *select_events = synclist_wrlock(&glb->select_events);
        size_t select_events_new_count = 0;
        if (select_events_poll_count < 0) {
            select_event_count_and_destroy_closed(select_events, NULL, NULL, &select_events_new_count, glb);
        } else {
            select_events_new_count = select_events_poll_count;
        }

        size_t listeners_new_count = 0;
        struct ListHead *listeners = NULL;
        struct ListHead *item;
        if (listeners_poll_count < 0) {
            listeners = synclist_rdlock(&glb->listeners);
            LIST_FOR_EACH (item, listeners) {
                EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
                int listener_fd = listener->fd;
                if (listener_fd >= 0) {
                    listeners_new_count++;
                }
            }
        } else {
            listeners_new_count = listeners_poll_count;
        }

        size_t new_count = signal_poll_count + select_events_new_count + listeners_new_count;
        struct pollfd *new_fds = realloc(fds, sizeof(struct pollfd) * new_count);
        if (UNLIKELY(new_count > 0 && !new_fds)) {
            if (listeners_poll_count < 0) {
                synclist_unlock(&glb->listeners);
            }
            synclist_unlock(&glb->select_events);
            return;
        }
        fds = new_fds;
        platform->fds = fds;

#if defined(CONFIG_EVENTFD)
        fds[0].fd = platform->signal_fd;
        fds[0].events = POLLIN;
        fds[0].revents = 0;
#endif

        fd_index = signal_poll_count;
        if (listeners_poll_count < 0) {
            LIST_FOR_EACH (item, listeners) {
                EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
                int listener_fd = listener->fd;
                if (listener_fd >= 0) {
                    fds[fd_index].fd = listener_fd;
                    fds[fd_index].events = POLLIN;
                    fds[fd_index].revents = 0;
                    fd_index++;
                }
            }
            platform->listeners_poll_count = listeners_new_count;
            synclist_unlock(&glb->listeners);
        } else {
            fd_index += listeners_new_count;
        }

        LIST_FOR_EACH (item, select_events) {
            struct SelectEvent *select_event = GET_LIST_ENTRY(item, struct SelectEvent, head);
            if (select_event->read || select_event->write) {
                fds[fd_index].fd = select_event->event;
                fds[fd_index].events = (select_event->read ? POLLIN : 0) | (select_event->write ? POLLOUT : 0);
                fds[fd_index].revents = 0;
                fd_index++;
            }
        }
        platform->select_events_poll_count = select_events_new_count;
        synclist_unlock(&glb->select_events);

        listeners_poll_count = listeners_new_count;
        select_events_poll_count = select_events_new_count;
    }

    int poll_count = signal_poll_count + listeners_poll_count + select_events_poll_count;
    if (poll_count == 0) {
        if (timeout_ms > 0) {
            k_msleep(timeout_ms);
        }
        return;
    }

    int nb_descriptors = poll(fds, poll_count, timeout_ms);
    if (nb_descriptors <= 0) {
        return;
    }

    fd_index = 0;
#if defined(CONFIG_EVENTFD)
    if (nb_descriptors > 0) {
        if (fds[0].revents & POLLIN) {
            eventfd_t ignored;
            (void) eventfd_read(platform->signal_fd, &ignored);
            nb_descriptors--;
        }
        fd_index++;
    }
#endif

    if (nb_descriptors > 0) {
        struct ListHead *listeners = synclist_wrlock(&glb->listeners);
        struct ListHead *item = listeners->next;
        struct ListHead *previous = listeners;
        for (int i = 0; i < listeners_poll_count && nb_descriptors > 0; i++, fd_index++) {
            if (!(fds[fd_index].revents & fds[fd_index].events)) {
                continue;
            }
            fds[fd_index].revents = 0;
            nb_descriptors--;
            process_listener_handler(glb, fds[fd_index].fd, listeners, &item, &previous);
        }
        synclist_unlock(&glb->listeners);
    }

    for (int i = 0; i < select_events_poll_count && nb_descriptors > 0; i++, fd_index++) {
        if (!(fds[fd_index].revents & fds[fd_index].events)) {
            continue;
        }
        bool is_read = fds[fd_index].revents & POLLIN;
        bool is_write = fds[fd_index].revents & POLLOUT;
        fds[fd_index].revents = 0;
        nb_descriptors--;
        select_event_notify(fds[fd_index].fd, is_read, is_write, glb);
    }
#else
    if (timeout_ms > 0) {
        k_msleep(timeout_ms);
    }
#endif
}

void sys_register_listener(GlobalContext *global, struct EventListener *listener)
{
    struct ListHead *listeners = synclist_wrlock(&global->listeners);
    list_append(listeners, &listener->listeners_list_head);
#if defined(CONFIG_NET_SOCKETS)
    event_listener_add_to_polling_set(listener, global);
#endif
    synclist_unlock(&global->listeners);
}

void sys_unregister_listener(GlobalContext *global, struct EventListener *listener)
{
    synclist_remove(&global->listeners, &listener->listeners_list_head);
#if defined(CONFIG_NET_SOCKETS)
    struct ZephyrPlatformData *platform = global->platform_data;
    if (platform) {
        platform->listeners_poll_count = -1;
    }
#endif
}

#if !defined(CONFIG_NET_SOCKETS)
void sys_listener_destroy(struct ListHead *item)
{
    UNUSED(item);
}
#endif

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(event);
    UNUSED(is_write);
#if defined(CONFIG_NET_SOCKETS)
    struct ZephyrPlatformData *platform = global->platform_data;
    if (platform) {
        platform->select_events_poll_count = -1;
    }
#else
    UNUSED(global);
#endif
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(event);
    UNUSED(is_write);
#if defined(CONFIG_NET_SOCKETS)
    struct ZephyrPlatformData *platform = global->platform_data;
    if (platform) {
        platform->select_events_poll_count = -1;
    }
#else
    UNUSED(global);
#endif
}

void sys_time(struct timespec *t)
{
    platform_clock_gettime(t);
}

void sys_monotonic_time(struct timespec *t)
{
    platform_clock_gettime(t);
}

uint64_t sys_monotonic_time_u64()
{
    return k_uptime_get();
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms)
{
    return ms;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t t)
{
    return t;
}

enum OpenAVMResult sys_open_avm_from_file(
    GlobalContext *global, const char *path, struct AVMPackData **data)
{
    TRACE("sys_open_avm_from_file: Going to open: %s\n", path);

    // TODO
    AVM_LOGW(TAG, "Open from file not supported on this platform.");
    return AVM_OPEN_NOT_SUPPORTED;
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    // TODO
    return NULL;
}

Module *sys_load_module(GlobalContext *global, const char *module_name)
{
    const void *beam_module = NULL;
    uint32_t beam_module_size = 0;

    struct ListHead *avmpack_data_list = synclist_rdlock(&global->avmpack_data);
    struct ListHead *item;
    LIST_FOR_EACH (item, avmpack_data_list) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        avmpack_data->in_use = true;
        if (avmpack_find_section_by_name(avmpack_data->data, module_name, &beam_module, &beam_module_size)) {
            break;
        }
    }
    synclist_unlock(&global->avmpack_data);

    if (IS_NULL_PTR(beam_module)) {
        AVM_LOGE(TAG, "Failed to open module: %s.", module_name);
        return NULL;
    }

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    new_module->module_platform_data = NULL;

    return new_module;
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    Context *new_ctx = port_driver_create_port(driver_name, glb, opts);
    if (IS_NULL_PTR(new_ctx)) {
        AVM_LOGE(TAG, "Failed to load port \"%s\".  Ensure the port is configured properly in the build.", driver_name);
        new_ctx = NULL;
    }
    return new_ctx;
}

term sys_get_info(Context *ctx, term key)
{
    return UNDEFINED_ATOM;
}

void port_driver_init_all(GlobalContext *global)
{
    for (struct PortDriverDefListItem *item = port_driver_list; item != NULL; item = item->next) {
        if (item->def->port_driver_init_cb) {
            item->def->port_driver_init_cb(global);
        }
    }
}

void port_driver_destroy_all(GlobalContext *global)
{
    for (struct PortDriverDefListItem *item = port_driver_list; item != NULL; item = item->next) {
        if (item->def->port_driver_destroy_cb) {
            item->def->port_driver_destroy_cb(global);
        }
    }
}

static Context *port_driver_create_port(const char *port_name, GlobalContext *global, term opts)
{
    for (struct PortDriverDefListItem *item = port_driver_list; item != NULL; item = item->next) {
        if (strcmp(port_name, item->def->port_driver_name) == 0) {
            return item->def->port_driver_create_port_cb(global, opts);
        }
    }

    return NULL;
}

void nif_collection_init_all(GlobalContext *global)
{
    for (struct NifCollectionDefListItem *item = nif_collection_list; item != NULL; item = item->next) {
        if (item->def->nif_collection_init_cb) {
            item->def->nif_collection_init_cb(global);
        }
    }
}

void nif_collection_destroy_all(GlobalContext *global)
{
    for (struct NifCollectionDefListItem *item = nif_collection_list; item != NULL; item = item->next) {
        if (item->def->nif_collection_destroy_cb) {
            item->def->nif_collection_destroy_cb(global);
        }
    }
}

const struct Nif *nif_collection_resolve_nif(const char *name)
{
    for (struct NifCollectionDefListItem *item = nif_collection_list; item != NULL; item = item->next) {
        const struct Nif *res = item->def->nif_collection_resolve_nif_cb(name);
        if (res) {
            return res;
        }
    }

    return NULL;
}

bool platform_atomic_compare_exchange_weak_ptr(void **object, void **expected, void *desired)
{
    void *expected_value = *expected;
    bool exchanged = atomic_ptr_cas((atomic_ptr_t *) object, expected_value, desired);
    if (!exchanged) {
        *expected = atomic_ptr_get((atomic_ptr_t *) object);
    }
    return exchanged;
}

#ifndef AVM_NO_SMP
static struct k_spinlock atomic_ops_lock;

bool smp_atomic_compare_exchange_weak_int(void *object, void *expected, uint64_t desired, size_t desired_len)
{
    k_spinlock_key_t key = k_spin_lock(&atomic_ops_lock);

    bool result;
    switch (desired_len) {
        case sizeof(uint64_t): {
            uint64_t *object_ptr = (uint64_t *) object;
            uint64_t *expected_ptr = (uint64_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
        case sizeof(uint32_t): {
            uint32_t *object_ptr = (uint32_t *) object;
            uint32_t *expected_ptr = (uint32_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = (uint32_t) desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
        case sizeof(uint16_t): {
            uint16_t *object_ptr = (uint16_t *) object;
            uint16_t *expected_ptr = (uint16_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = (uint16_t) desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
        case sizeof(uint8_t): {
            uint8_t *object_ptr = (uint8_t *) object;
            uint8_t *expected_ptr = (uint8_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = (uint8_t) desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
        default:
            AVM_ABORT();
    }

    k_spin_unlock(&atomic_ops_lock, key);
    return result;
}

size_t smp_atomic_fetch_add_size(size_t *object, size_t delta)
{
    k_spinlock_key_t key = k_spin_lock(&atomic_ops_lock);
    size_t result = *object;
    *object += delta;
    k_spin_unlock(&atomic_ops_lock, key);
    return result;
}

size_t smp_atomic_fetch_sub_size(size_t *object, size_t delta)
{
    k_spinlock_key_t key = k_spin_lock(&atomic_ops_lock);
    size_t result = *object;
    *object -= delta;
    k_spin_unlock(&atomic_ops_lock, key);
    return result;
}

size_t smp_atomic_fetch_or_size(size_t *object, size_t mask)
{
    k_spinlock_key_t key = k_spin_lock(&atomic_ops_lock);
    size_t result = *object;
    *object |= mask;
    k_spin_unlock(&atomic_ops_lock, key);
    return result;
}
#endif

#ifdef HAVE_PSA_CRYPTO
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>
#include <zephyr/random/random.h>

#ifdef CONFIG_MBEDTLS_ENTROPY_C
static mbedtls_entropy_context entropy_ctx;
static bool entropy_is_initialized = false;
#endif
static mbedtls_ctr_drbg_context random_ctx;
static bool random_is_initialized = false;

int sys_mbedtls_entropy_func(void *entropy, unsigned char *buf, size_t size)
{
#ifdef CONFIG_MBEDTLS_ENTROPY_C
    return mbedtls_entropy_func(entropy, buf, size);
#else
    UNUSED(entropy);
    int rc = sys_csrand_get(buf, size);
    return rc == 0 ? 0 : -1;
#endif
}

mbedtls_entropy_context *sys_mbedtls_get_entropy_context_lock(GlobalContext *global)
{
    UNUSED(global);
#ifdef CONFIG_MBEDTLS_ENTROPY_C
    if (!entropy_is_initialized) {
        mbedtls_entropy_init(&entropy_ctx);
        entropy_is_initialized = true;
    }
    return &entropy_ctx;
#else
    return NULL;
#endif
}

void sys_mbedtls_entropy_context_unlock(GlobalContext *global)
{
    UNUSED(global);
}

mbedtls_ctr_drbg_context *sys_mbedtls_get_ctr_drbg_context_lock(GlobalContext *global)
{
    if (!random_is_initialized) {
        mbedtls_ctr_drbg_init(&random_ctx);
        mbedtls_entropy_context *entropy_ctx_ptr = sys_mbedtls_get_entropy_context_lock(global);
        const char *seed = "AtomVM Zephyr PSA Mbed-TLS initial seed.";
        int seed_len = strlen(seed);
        int seed_err = mbedtls_ctr_drbg_seed(&random_ctx, sys_mbedtls_entropy_func,
            entropy_ctx_ptr, (const unsigned char *) seed, seed_len);
        if (seed_err != 0) {
            AVM_ABORT();
        }
        random_is_initialized = true;
    }
    return &random_ctx;
}

void sys_mbedtls_ctr_drbg_context_unlock(GlobalContext *global)
{
    UNUSED(global);
}
#endif
