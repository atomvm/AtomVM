/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 by Paul Guyot <pguyot@kallisys.net>
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

#ifndef AVM_NO_JIT

#include "context.h"
#include "defaultatoms.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "jit.h"
#include "nifs.h"
#include "platform_defaultatoms.h"
#include "term.h"

#include <errno.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

#if defined(__APPLE__)
#include <libkern/OSCacheControl.h>
#endif

#if HAVE_MAP_JIT
#define ATOMVM_MAP_JIT MAP_JIT
#else
#define ATOMVM_MAP_JIT 0
#endif

static ErlNifResourceType *jit_stream_mmap_resource_type;
static void jit_stream_mmap_dtor(ErlNifEnv *caller_env, void *obj);

const ErlNifResourceTypeInit jit_stream_mmap_resource_type_init = {
    .members = 1,
    .dtor = jit_stream_mmap_dtor
};

struct JITStreamMMap
{
    uint8_t *stream_base;
    size_t stream_offset;
    size_t stream_size;
};

static term nif_jit_stream_mmap_new(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);

    size_t size = term_to_int(argv[0]);

    int prot = PROT_READ | PROT_WRITE | PROT_EXEC;
    int flags = MAP_PRIVATE | MAP_ANONYMOUS | ATOMVM_MAP_JIT;
    int fd = -1;
    off_t offset = 0;

    uint8_t *addr = (uint8_t *) mmap(0, size, prot, flags, fd, offset);
    if (addr == MAP_FAILED) {
        fprintf(stderr, "Could not allocate mmap for JIT: size=%zu, errno=%d\n", size, errno);
        RAISE_ERROR(BADARG_ATOM);
    }

    // Return a resource object
    struct JITStreamMMap *js = enif_alloc_resource(jit_stream_mmap_resource_type, sizeof(struct JITStreamMMap));
    if (IS_NULL_PTR(js)) {
        munmap(addr, size);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    js->stream_base = addr;
    js->stream_offset = 0;
    js->stream_size = size;

    term obj = enif_make_resource(erl_nif_env_from_context(ctx), js);
    enif_release_resource(js); // decrement refcount after enif_alloc_resource
    return obj;
}

static term nif_jit_stream_mmap_offset(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_mmap_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamMMap *js_obj = (struct JITStreamMMap *) js_obj_ptr;

    return term_from_int(js_obj->stream_offset);
}

static term nif_jit_stream_mmap_append(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_binary);
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_mmap_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamMMap *js_obj = (struct JITStreamMMap *) js_obj_ptr;

    size_t binary_size = term_binary_size(argv[1]);
    const uint8_t *binary_data = (const uint8_t *) term_binary_data(argv[1]);
    if (UNLIKELY(js_obj->stream_offset + binary_size > js_obj->stream_size)) {
        RAISE_ERROR(BADARG_ATOM);
    }

#if HAVE_PTHREAD_JIT_WRITE_PROTECT_NP
    pthread_jit_write_protect_np(0);
#endif
    memcpy(js_obj->stream_base + js_obj->stream_offset, binary_data, binary_size);
#if HAVE_PTHREAD_JIT_WRITE_PROTECT_NP
    pthread_jit_write_protect_np(1);
#endif
    js_obj->stream_offset += binary_size;

    return argv[0];
}

static term nif_jit_stream_mmap_replace(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_binary);
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_mmap_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamMMap *js_obj = (struct JITStreamMMap *) js_obj_ptr;

    size_t binary_size = term_binary_size(argv[2]);
    const uint8_t *binary_data = (const uint8_t *) term_binary_data(argv[2]);
    int offset = term_to_int(argv[1]);

    if (UNLIKELY(offset < 0 || binary_size + offset > js_obj->stream_offset)) {
        RAISE_ERROR(BADARG_ATOM);
    }

#if HAVE_PTHREAD_JIT_WRITE_PROTECT_NP
    pthread_jit_write_protect_np(0);
#endif
    memcpy(js_obj->stream_base + offset, binary_data, binary_size);
#if HAVE_PTHREAD_JIT_WRITE_PROTECT_NP
    pthread_jit_write_protect_np(1);
#endif

    return argv[0];
}

static term nif_jit_stream_mmap_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_mmap_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamMMap *js_obj = (struct JITStreamMMap *) js_obj_ptr;

    int offset = term_to_int(argv[1]);
    int len = term_to_int(argv[2]);

    if (UNLIKELY(memory_ensure_free_opt(ctx, TERM_BINARY_HEAP_SIZE(len), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    if (UNLIKELY(len <= 0 || offset < 0 || (size_t) (offset + len) > js_obj->stream_offset)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return term_from_literal_binary(js_obj->stream_base + offset, len, &ctx->heap, ctx->global);
}

static term nif_jit_stream_module(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    UNUSED(ctx);

    return JIT_STREAM_MMAP_ATOM;
}

static const struct Nif jit_stream_module_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_module
};
static const struct Nif jit_stream_mmap_new_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_mmap_new
};
static const struct Nif jit_stream_mmap_offset_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_mmap_offset
};
static const struct Nif jit_stream_mmap_append_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_mmap_append
};
static const struct Nif jit_stream_mmap_replace_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_mmap_replace
};
static const struct Nif jit_stream_mmap_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_mmap_read
};

ModuleNativeEntryPoint jit_stream_entry_point(Context *ctx, term jit_stream)
{
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), jit_stream, jit_stream_mmap_resource_type, &js_obj_ptr))) {
        return NULL;
    }
    struct JITStreamMMap *js_obj = (struct JITStreamMMap *) js_obj_ptr;

    if (IS_NULL_PTR(js_obj->stream_base)) {
        return NULL;
    }

#if defined(__APPLE__)
    sys_icache_invalidate(js_obj->stream_base, js_obj->stream_size);
#elif defined(__GNUC__)
    __builtin___clear_cache(js_obj->stream_base, js_obj->stream_base + js_obj->stream_size);
#endif
#if JIT_ARCH_TARGET == JIT_ARCH_ARMV6M
    // Set thumb bit for armv6m
    ModuleNativeEntryPoint result = (ModuleNativeEntryPoint) js_obj->stream_base + 1;
#else
    ModuleNativeEntryPoint result = (ModuleNativeEntryPoint) js_obj->stream_base;
#endif

    js_obj->stream_base = NULL;
    return result;
}

static void jit_stream_mmap_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct JITStreamMMap *js_obj = (struct JITStreamMMap *) obj;
    if (js_obj->stream_base) {
        munmap(js_obj->stream_base, js_obj->stream_size);
    }
}

//
// Entrypoints
//

const struct Nif *jit_stream_mmap_get_nif(const char *nifname)
{
    if (strcmp("jit:stream_module/0", nifname) == 0) {
        return &jit_stream_module_nif;
    }
    if (strncmp("jit_stream_mmap:", nifname, 16) == 0) {
        const char *rest = nifname + 16;
        if (strcmp("new/1", rest) == 0) {
            return &jit_stream_mmap_new_nif;
        }
        if (strcmp("offset/1", rest) == 0) {
            return &jit_stream_mmap_offset_nif;
        }
        if (strcmp("append/2", rest) == 0) {
            return &jit_stream_mmap_append_nif;
        }
        if (strcmp("replace/3", rest) == 0) {
            return &jit_stream_mmap_replace_nif;
        }
        if (strcmp("read/3", rest) == 0) {
            return &jit_stream_mmap_read_nif;
        }
    }
    return NULL;
}

void jit_stream_mmap_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    jit_stream_mmap_resource_type = enif_init_resource_type(&env, "jit_stream_mmap", &jit_stream_mmap_resource_type_init, ERL_NIF_RT_CREATE, NULL);
}

#endif
