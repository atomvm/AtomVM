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

#ifndef _ERL_NIF_PRIV_H_
#define _ERL_NIF_PRIV_H_

#include "context.h"
#include "memory.h"
#include "resources.h"

#ifdef __cplusplus
extern "C" {
#endif

struct ErlNifEnv
{
    GlobalContext *global;
    Heap heap;
    term *stack_pointer; // Context stack pointer, NULL for non-context envs
    term x[2];
};

_Static_assert(offsetof(struct ErlNifEnv, global) == offsetof(struct Context, global) ? 1 : 0,
    "ErlNifEnv.global doesn't match Context.global");
_Static_assert(offsetof(struct ErlNifEnv, heap) == offsetof(struct Context, heap) ? 1 : 0,
    "ErlNifEnv.heap doesn't match Context.heap");
_Static_assert(offsetof(struct ErlNifEnv, stack_pointer) == offsetof(struct Context, e) ? 1 : 0,
    "ErlNifEnv.stack_pointer doesn't match Context.e");
_Static_assert(offsetof(struct ErlNifEnv, x) == offsetof(struct Context, x) ? 1 : 0,
    "ErlNifEnv.x doesn't match Context.x");

static inline ErlNifEnv *erl_nif_env_from_context(Context *ctx)
{
    return (ErlNifEnv *) ctx;
}

static inline bool erl_nif_env_is_context(ErlNifEnv *env)
{
    return env->stack_pointer != NULL;
}

static inline void erl_nif_env_partial_init_from_globalcontext(ErlNifEnv *env, GlobalContext *global)
{
    env->global = global;
    env->heap.root = NULL;
    env->heap.heap_start = NULL;
    env->heap.heap_ptr = NULL;
    env->heap.heap_end = NULL;
    env->stack_pointer = NULL;
    env->x[0] = term_nil();
    env->x[1] = term_nil();
}

static inline void erl_nif_env_partial_init_from_resource(ErlNifEnv *env, void *resource)
{
    struct RefcBinary *refc = refc_binary_from_data(resource);
    env->global = refc->resource_type->global;
    env->heap.root = NULL;
    env->heap.heap_start = NULL;
    env->heap.heap_ptr = NULL;
    env->heap.heap_end = NULL;
    env->stack_pointer = NULL;
    env->x[0] = term_nil();
    env->x[1] = term_nil();
}

#ifdef __cplusplus
}
#endif

#endif // _ERL_NIF_PRIV_H_
