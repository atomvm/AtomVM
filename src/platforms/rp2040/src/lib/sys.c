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
#include <time.h>

// Pico SDK
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <hardware/rtc.h>
#include <pico/multicore.h>
#include <pico/time.h>
#include <sys/time.h>

#ifdef LIB_PICO_CYW43_ARCH
#include <pico/cyw43_arch.h>
#endif

#pragma GCC diagnostic pop

// libAtomVM
#include <avmpack.h>
#include <defaultatoms.h>
#include <utils.h>

#include "rp2040_sys.h"

struct PortDriverDefListItem *port_driver_list;
struct NifCollectionDefListItem *nif_collection_list;

void sys_init_platform(GlobalContext *glb)
{
    struct RP2040PlatformData *platform = malloc(sizeof(struct RP2040PlatformData));
    glb->platform_data = platform;
    mutex_init(&platform->event_poll_mutex);
    cond_init(&platform->event_poll_cond);

#ifdef LIB_PICO_CYW43_ARCH
    cyw43_arch_init();
#endif
}

void sys_free_platform(GlobalContext *glb)
{
#ifdef LIB_PICO_CYW43_ARCH
    cyw43_arch_deinit();
#endif

    struct RP2040PlatformData *platform = glb->platform_data;
    free(platform);
}

#ifndef AVM_NO_SMP
void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
    if (timeout_ms > 0) {
        struct RP2040PlatformData *platform = glb->platform_data;
        mutex_enter_blocking(&platform->event_poll_mutex);
        cond_wait_timeout_ms(&platform->event_poll_cond, &platform->event_poll_mutex, timeout_ms);
        mutex_exit(&platform->event_poll_mutex);
    }
}

void sys_signal(GlobalContext *glb)
{
    struct RP2040PlatformData *platform = glb->platform_data;
    cond_signal(&platform->event_poll_cond);
}
#else
void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
    UNUSED(glb);
    UNUSED(timeout_ms);
}
#endif

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

uint64_t sys_monotonic_millis()
{
    absolute_time_t now = get_absolute_time();
    uint64_t usec = to_us_since_boot(now);
    return usec / 1000;
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
        const struct Nif *res = item->def->nif_collection_resove_nif_cb(name);
        if (res) {
            return res;
        }
    }

    return NULL;
}
