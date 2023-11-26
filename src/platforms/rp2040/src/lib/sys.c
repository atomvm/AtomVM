/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

// C11
#include <stdio.h>
#include <time.h>

// Pico SDK
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <hardware/rtc.h>
#include <pico/multicore.h>
#include <pico/time.h>
#include <pico/util/queue.h>
#include <sys/time.h>

#ifdef LIB_PICO_CYW43_ARCH
#include <pico/cyw43_arch.h>
#endif

#pragma GCC diagnostic pop

#ifdef LIB_PICO_CYW43_ARCH
#include <otp_socket.h>
#endif

#if defined(MBEDTLS_VERSION_NUMBER) && (MBEDTLS_VERSION_NUMBER >= 0x03000000)
#include <mbedtls/build_info.h>
#else
#include <mbedtls/config.h>
#endif

// libAtomVM
#include <avmpack.h>
#include <defaultatoms.h>
#include <utils.h>

#include "rp2040_sys.h"

// #define ENABLE_TRACE
#include "trace.h"

// Platform uses listeners
#include "listeners.h"

#ifndef AVM_NO_SMP
#define SMP_MUTEX_LOCK(mtx) smp_mutex_lock(mtx)
#define SMP_MUTEX_UNLOCK(mtx) smp_mutex_unlock(mtx)
#else
#define SMP_MUTEX_LOCK(mtx)
#define SMP_MUTEX_UNLOCK(mtx)
#endif

struct PortDriverDefListItem *port_driver_list;
struct NifCollectionDefListItem *nif_collection_list;

void sys_init_platform(GlobalContext *glb)
{
    struct RP2040PlatformData *platform = malloc(sizeof(struct RP2040PlatformData));
    glb->platform_data = platform;
#ifndef AVM_NO_SMP
    mutex_init(&platform->event_poll_mutex);
    cond_init(&platform->event_poll_cond);
    smp_init();
#endif
    queue_init(&platform->event_queue, sizeof(queue_t *), EVENT_QUEUE_LEN);

#ifdef LIB_PICO_CYW43_ARCH
    cyw43_arch_init();
    otp_socket_init(glb);
#endif

    platform->entropy_is_initialized = false;
    platform->random_is_initialized = false;
}

void sys_free_platform(GlobalContext *glb)
{
#ifdef LIB_PICO_CYW43_ARCH
    cyw43_arch_deinit();
#endif

    struct RP2040PlatformData *platform = glb->platform_data;
    queue_free(&platform->event_queue);

    if (platform->random_is_initialized) {
        mbedtls_ctr_drbg_free(&platform->random_ctx);
    }

    if (platform->entropy_is_initialized) {
        mbedtls_entropy_free(&platform->entropy_ctx);
    }

    free(platform);

#ifndef AVM_NO_SMP
    smp_free();
#endif
}

bool sys_try_post_listener_event_from_isr(GlobalContext *glb, listener_event_t listener_queue, const void *event)
{
    struct RP2040PlatformData *platform = glb->platform_data;
    if (UNLIKELY(!queue_try_add(listener_queue, event))) {
        fprintf(stderr, "Lost event from ISR as listener queue is full. System is overloaded or EVENT_QUEUE_LEN is too low\n");
        return false;
    }

#ifndef AVM_NO_SMP
    uint32_t owner;
    bool acquired_mutex = mutex_try_enter(&platform->event_poll_mutex, &owner);
    // We're from an ISR, so we cannot wait for the interrupted code (running
    // on the same core as we do) to release the mutex.
    if (!acquired_mutex) {
        // If this core is not the owner, wait for the other core to release
        // the mutex.
        // TODO: implement queue_try_remove_wait_timeout_ms in Pico SDK to
        // simplify this logic
        uint32_t caller = (uint32_t) lock_get_caller_owner_id(); // same cast exists in mutex_try_enter
        if (caller != owner) {
            mutex_enter_blocking(&platform->event_poll_mutex);
            acquired_mutex = true;
        }
    }
#endif
    if (UNLIKELY(!queue_try_add(&platform->event_queue, &listener_queue))) {
#ifndef AVM_NO_SMP
        if (acquired_mutex) {
            mutex_exit(&platform->event_poll_mutex);
        }
#endif
        fprintf(stderr, "Lost event from ISR as global event queue is full. System is overloaded or EVENT_QUEUE_LEN is too low\n");
        return false;
    }
#ifndef AVM_NO_SMP
    if (acquired_mutex) {
        mutex_exit(&platform->event_poll_mutex);
    }
#endif

#ifndef AVM_NO_SMP
    sys_signal(glb);
#endif

    return true;
}

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
    struct RP2040PlatformData *platform = glb->platform_data;
#ifndef AVM_NO_SMP
    if (timeout_ms != 0) {
        mutex_enter_blocking(&platform->event_poll_mutex);
        if (queue_is_empty(&platform->event_queue)) {
            if (timeout_ms > 0) {
                cond_wait_timeout_ms(&platform->event_poll_cond, &platform->event_poll_mutex, timeout_ms);
            } else {
                cond_wait(&platform->event_poll_cond, &platform->event_poll_mutex);
            }
        }
        mutex_exit(&platform->event_poll_mutex);
    }
#else
    UNUSED(timeout_ms);
#endif
    queue_t *event = NULL;
    while (queue_try_remove(&platform->event_queue, &event)) {
        struct ListHead *listeners = synclist_wrlock(&glb->listeners);
        if (!process_listener_handler(glb, event, listeners, NULL, NULL)) {
            TRACE("sys: handler not found for: %p\n", (void *) event);
        }
        synclist_unlock(&glb->listeners);
    }
}

#ifndef AVM_NO_SMP
void sys_signal(GlobalContext *glb)
{
    struct RP2040PlatformData *platform = glb->platform_data;
    cond_signal(&platform->event_poll_cond);
}
#endif

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
    struct timeval tv;
    gettimeofday(&tv, NULL);
    t->tv_sec = tv.tv_sec;
    t->tv_nsec = tv.tv_usec * 1000;
}

void sys_monotonic_time(struct timespec *t)
{
    absolute_time_t now = get_absolute_time();
    uint64_t usec = to_us_since_boot(now);
    t->tv_nsec = (usec % 1000000) * 1000;
    t->tv_sec = (usec / 1000000);
}

uint64_t sys_monotonic_time_u64()
{
    absolute_time_t now = get_absolute_time();
    return to_us_since_boot(now);
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms)
{
    return ms * 1000;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t t)
{
    return t / 1000;
}

enum OpenAVMResult sys_open_avm_from_file(
    GlobalContext *global, const char *path, struct AVMPackData **data)
{
    UNUSED(global);
    UNUSED(path);
    UNUSED(data);

    // TODO
    return AVM_OPEN_NOT_SUPPORTED;
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    UNUSED(global);
    UNUSED(path);

    // No file support on pico.
    return NULL;
}

Module *sys_load_module(GlobalContext *global, const char *module_name)
{
    const void *beam_module = NULL;
    uint32_t beam_module_size = 0;

    struct ListHead *item;
    struct ListHead *avmpack_data = synclist_rdlock(&global->avmpack_data);
    LIST_FOR_EACH (item, avmpack_data) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        if (avmpack_find_section_by_name(avmpack_data->data, module_name, &beam_module, &beam_module_size)) {
            break;
        }
    }
    synclist_unlock(&global->avmpack_data);

    if (IS_NULL_PTR(beam_module)) {
        fprintf(stderr, "Failed to open module: %s\n", module_name);
        return NULL;
    }

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    new_module->module_platform_data = NULL;

    return new_module;
}

Context *sys_create_port(GlobalContext *glb, const char *port_name, term opts)
{
    for (struct PortDriverDefListItem *item = port_driver_list; item != NULL; item = item->next) {
        if (strcmp(port_name, item->def->port_driver_name) == 0) {
            return item->def->port_driver_create_port_cb(glb, opts);
        }
    }

    return NULL;
}

term sys_get_info(Context *ctx, term key)
{
    UNUSED(ctx);
    UNUSED(key);
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

static void event_listener_add_to_polling_set(struct EventListener *listener, GlobalContext *glb)
{
    UNUSED(listener);
    UNUSED(glb);
}

static void listener_event_remove_from_polling_set(listener_event_t event, GlobalContext *glb)
{
    UNUSED(event);
    UNUSED(glb);
}

static bool event_listener_is_event(EventListener *listener, listener_event_t event)
{
    return listener->queue == event;
}

void sys_register_listener(GlobalContext *global, struct EventListener *listener)
{
    struct ListHead *listeners = synclist_wrlock(&global->listeners);
#ifndef AVM_NO_SMP
    sys_signal(global);
#endif
    list_append(listeners, &listener->listeners_list_head);
    synclist_unlock(&global->listeners);
}

void sys_unregister_listener(GlobalContext *global, struct EventListener *listener)
{
    struct ListHead *dummy = synclist_wrlock(&global->listeners);
    UNUSED(dummy);
#ifndef AVM_NO_SMP
    sys_signal(global);
#endif
    list_remove(&listener->listeners_list_head);
    synclist_unlock(&global->listeners);
}

void sys_unregister_listener_from_event(GlobalContext *global, listener_event_t event)
{
    struct ListHead *list = synclist_wrlock(&global->listeners);
#ifndef AVM_NO_SMP
    sys_signal(global);
#endif
    struct ListHead *item;
    LIST_FOR_EACH (item, list) {
        struct EventListener *listener = GET_LIST_ENTRY(item, struct EventListener, listeners_list_head);
        if (event_listener_is_event(listener, event)) {
            list_remove(&listener->listeners_list_head);
            break;
        }
    }
    synclist_unlock(&global->listeners);
}

// TODO: enable mbedtls threading support by defining MBEDTLS_THREADING_ALT
// and remove this function.
int sys_mbedtls_entropy_func(void *entropy, unsigned char *buf, size_t size)
{
#ifndef MBEDTLS_THREADING_C
    struct RP2040PlatformData *platform
        = CONTAINER_OF(entropy, struct RP2040PlatformData, entropy_ctx);
    SMP_MUTEX_LOCK(platform->entropy_mutex);
    int result = mbedtls_entropy_func(entropy, buf, size);
    SMP_MUTEX_UNLOCK(platform->entropy_mutex);

    return result;
#else
    return mbedtls_entropy_func(entropy, buf, size);
#endif
}

mbedtls_entropy_context *sys_mbedtls_get_entropy_context_lock(GlobalContext *global)
{
    struct RP2040PlatformData *platform = global->platform_data;

    SMP_MUTEX_LOCK(platform->entropy_mutex);

    if (!platform->entropy_is_initialized) {
        mbedtls_entropy_init(&platform->entropy_ctx);
        platform->entropy_is_initialized = true;
    }

    return &platform->entropy_ctx;
}

void sys_mbedtls_entropy_context_unlock(GlobalContext *global)
{
    struct RP2040PlatformData *platform = global->platform_data;
    SMP_MUTEX_UNLOCK(platform->entropy_mutex);
}

mbedtls_ctr_drbg_context *sys_mbedtls_get_ctr_drbg_context_lock(GlobalContext *global)
{
    struct RP2040PlatformData *platform = global->platform_data;

    SMP_MUTEX_LOCK(platform->random_mutex);

    if (!platform->random_is_initialized) {
        mbedtls_ctr_drbg_init(&platform->random_ctx);

        mbedtls_entropy_context *entropy_ctx = sys_mbedtls_get_entropy_context_lock(global);
        // Safe to unlock it now, sys_mbedtls_entropy_func will lock it again later
        sys_mbedtls_entropy_context_unlock(global);

        const char *seed = "AtomVM RP2040 Mbed-TLS initial seed.";
        int seed_len = strlen(seed);
        int seed_err = mbedtls_ctr_drbg_seed(&platform->random_ctx, sys_mbedtls_entropy_func,
            entropy_ctx, (const unsigned char *) seed, seed_len);
        if (UNLIKELY(seed_err != 0)) {
            abort();
        }
        platform->random_is_initialized = true;
    }

    return &platform->random_ctx;
}

void sys_mbedtls_ctr_drbg_context_unlock(GlobalContext *global)
{
    struct RP2040PlatformData *platform = global->platform_data;
    SMP_MUTEX_UNLOCK(platform->random_mutex);
}
