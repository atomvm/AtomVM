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

/**
 * @file erl_nif.h
 * @brief Public API for nifs, compatible with Erlang/OTP API
 */

#ifndef _ERL_NIF_H_
#define _ERL_NIF_H_

#include "term_typedef.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Opaque environment, passed to nifs.
 */
typedef struct ErlNifEnv ErlNifEnv;

/**
 * @brief A term.
 */
typedef term ERL_NIF_TERM;

/**
 * @brief a pid
 * @details We currently only handle local pids.
 */
typedef int32_t ErlNifPid;

/**
 * @brief Opaque resource type
 */
typedef struct ResourceType ErlNifResourceType;

/**
 * @brief Selectable event.
 */
typedef int ErlNifEvent;

/**
 * @brief Destructor callback
 */
typedef void ErlNifResourceDtor(ErlNifEnv *caller_env, void *obj);

/**
 * @brief Resource callbacks.
 * @details Members should be set to 0, 1 depending on provided callbacks.
 * Callbacks can also be NULL if not used.
 */
typedef struct
{
    int members;
    ErlNifResourceDtor *dtor;
} ErlNifResourceTypeInit;

/**
 * @brief resource flags
 */
typedef enum
{
    ERL_NIF_RT_CREATE = 1,
    // ERL_NIF_RT_TAKEOVER is not supported yet
} ErlNifResourceFlags;

/**
 * @brief Create or take over (code upgrade) a resource type.
 * @param env the current environment
 * @param init a structure describing the callbacks. Callbacks can be NULL if
 * not used.
 * @param name name of the resource (copied)
 * @param flags `ERL_NIF_RT_CREATE` or `ERL_NIF_RT_TAKEOVER` to create or
 * take over.
 * @param tried on output, updated to `ERL_NIF_RT_CREATE` or
 * `ERL_NIF_RT_TAKEOVER` depending on what has been done. On failure, updated
 * to flags. Can be NULL.
 * @return the resource type or `NULL` on failure.
 */
ErlNifResourceType *enif_init_resource_type(ErlNifEnv *env, const char *name, const ErlNifResourceTypeInit *init, ErlNifResourceFlags flags, ErlNifResourceFlags *tried);

/**
 * @brief Allocate a resource for given type for `size` bytes.
 * @param type a trype created by `enif_init_resource_type`.
 * @param size the size in bytes.
 * @return a pointer or `NULL` on failure.
 */
void *enif_alloc_resource(ErlNifResourceType *type, unsigned size);

/**
 * @brief Get a pointer to a resource from a term representing it.
 * @param env the current environment
 * @param t the term
 * @param type the resource type
 * @param objp on output the pointer to the resource
 * @return `true` on success, `false` on failure, if term is not a resource of
 * type `type`
 */
int enif_get_resource(ErlNifEnv *env, ERL_NIF_TERM t, ErlNifResourceType *type, void **objp);

/**
 * @brief Increment reference count of a resource
 * @param resource the resource to keep
 * @return `true`.
 */
int enif_keep_resource(void *resource);

/**
 * @brief Decrement reference count of a resource
 * @param resource the resource to release
 * @return `true`.
 */
int enif_release_resource(void *resource);

/**
 * @brief create a term from a resource
 * @details the term can be later passed to `enif_get_resource`.
 * The resource is typically released (by calling `enif_release_resource`)
 * just after calling this function to "transfer ownership" to Erlang code so
 * that it will be destroyed when garbage collected.
 * @param env current environment
 * @param obj resource
 * @return a new term representing the resource
 */
ERL_NIF_TERM enif_make_resource(ErlNifEnv *env, void *obj);

#ifdef __cplusplus
}
#endif

#endif // _ERL_NIF_H_
