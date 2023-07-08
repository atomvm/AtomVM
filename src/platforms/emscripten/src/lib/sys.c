/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

#include <avmpack.h>
#include <defaultatoms.h>
#include <scheduler.h>

#include <fcntl.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "trace.h"

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

void sys_time(struct timespec *t)
{
    if (UNLIKELY(clock_gettime(CLOCK_REALTIME, t))) {
        fprintf(stderr, "Failed clock_gettime.\n");
        AVM_ABORT();
    }
}

void sys_monotonic_time(struct timespec *t)
{
    if (UNLIKELY(clock_gettime(CLOCK_MONOTONIC, t))) {
        fprintf(stderr, "Failed clock_gettime.\n");
        AVM_ABORT();
    }
}

uint64_t sys_monotonic_millis()
{
    struct timespec ts;
    sys_monotonic_time(&ts);
    return (ts.tv_nsec / 1000000UL) + (ts.tv_sec * 1000UL);
}

struct AVMPackData *sys_open_avm_from_file(GlobalContext *global, const char *path)
{
    TRACE("sys_open_avm_from_file: Going to open: %s\n", path);

    UNUSED(global);

    int fd = open(path, O_RDONLY);
    if (UNLIKELY(fd < 0)) {
        return NULL;
    }

    off_t fsize = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);
    void *data = malloc(fsize);
    if (IS_NULL_PTR(data)) {
        close(fd);
        return NULL;
    }

    size_t r = read(fd, data, fsize);
    if (UNLIKELY(r != fsize)) {
        free(data);
        close(fd);
        return NULL;
    }

    struct ConstAVMPack *const_avm = malloc(sizeof(struct ConstAVMPack));
    if (IS_NULL_PTR(const_avm)) {
        return NULL;
    }
    avmpack_data_init(&const_avm->base, &const_avm_pack_info);
    const_avm->base.data = (const uint8_t *) data;

    return &const_avm->base;
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    UNUSED(global);
    UNUSED(path);

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
        fprintf(stderr, "Failed to open module: %s\n", module_name);
        return NULL;
    }

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    new_module->module_platform_data = NULL;

    return new_module;
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    UNUSED(glb);
    UNUSED(driver_name);
    UNUSED(opts);
    return NULL;
}

term sys_get_info(Context *ctx, term key)
{
    UNUSED(ctx);
    UNUSED(key);
    return UNDEFINED_ATOM;
}
