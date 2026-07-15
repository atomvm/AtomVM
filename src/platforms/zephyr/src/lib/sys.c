/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Winford (Uncle Grumpy]) <winford@object.stream>
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
#include <zephyr/posix/time.h>

#include "avm_log.h"
#include "zephyros_sys.h"

#define TAG "sys"

struct PortDriverDefListItem *port_driver_list;
struct NifCollectionDefListItem *nif_collection_list;

static inline void sys_clock_gettime(struct timespec *t)
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
    UNUSED(glb);
}

void sys_free_platform(GlobalContext *glb)
{
    UNUSED(glb);
}

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
    UNUSED(glb);
    UNUSED(timeout_ms);
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
    sys_clock_gettime(t);
}

void sys_monotonic_time(struct timespec *t)
{
    sys_clock_gettime(t);
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
