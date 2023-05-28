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

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "context.h"
#include "defaultatoms.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "refc_binary.h"
#include "resources.h"
#include "sys.h"
#include "utils.h"

ErlNifResourceType *enif_init_resource_type(ErlNifEnv *env, const char *name, const ErlNifResourceTypeInit *init, ErlNifResourceFlags flags, ErlNifResourceFlags *tried)
{
    if (tried) {
        *tried = flags;
    }
    struct ResourceType *result = malloc(sizeof(struct ResourceType));
    if (UNLIKELY(result == NULL)) {
        return NULL;
    }
    result->name = strdup(name);
    result->global = env->global;
    list_init(&result->head);
    result->dtor = NULL;
    if (init->members >= 1) {
        result->dtor = init->dtor;
    }
    synclist_append(&env->global->resource_types, &result->head);

    return result;
}

void *enif_alloc_resource(ErlNifResourceType *type, unsigned size)
{
    struct RefcBinary *refc = refc_binary_create_resource(size, type);
    if (UNLIKELY(refc == NULL)) {
        return NULL;
    }
    // We add it now to the list of refc binaries, so resource is destroyed at
    // the latest when globalcontext is destroyed
    synclist_append(&type->global->refc_binaries, &refc->head);
    return (void *) refc_binary_get_data(refc);
}

int enif_get_resource(ErlNifEnv *env, ERL_NIF_TERM t, ErlNifResourceType *type, void **objp)
{
    UNUSED(env);

    if (UNLIKELY(!term_is_refc_binary(t))) {
        return false;
    }
    if (UNLIKELY(term_refc_binary_is_const(t))) {
        return false;
    }
    const term *boxed_value = term_to_const_term_ptr(t);
    struct RefcBinary *refc = (struct RefcBinary *) boxed_value[3];
    if (UNLIKELY(refc->resource_type != type)) {
        return false;
    }
    *objp = (void *) refc_binary_get_data(refc);
    return true;
}

int enif_keep_resource(void *resource)
{
    struct RefcBinary *refc = refc_binary_from_data(resource);
    refc_binary_increment_refcount(refc);
    return true;
}

int enif_release_resource(void *resource)
{
    struct RefcBinary *refc = refc_binary_from_data(resource);
    refc_binary_decrement_refcount(refc, refc->resource_type->global);
    return true;
}

static inline term resource_to_term(struct RefcBinary *refc, Heap *heap)
{
    term *boxed_value = memory_heap_alloc(heap, TERM_BOXED_REFC_BINARY_SIZE);
    boxed_value[0] = ((TERM_BOXED_REFC_BINARY_SIZE - 1) << 6) | TERM_BOXED_REFC_BINARY;
    boxed_value[1] = (term) 0; // binary size, this is pre ERTS 9.0 (OTP-20.0) behavior
    boxed_value[2] = (term) RefcNoFlags;
    term ret = ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
    boxed_value[3] = (term) refc;
    // Increment ref count and add the resource to the mso list
    refc_binary_increment_refcount(refc);
    heap->root->mso_list = term_list_init_prepend(boxed_value + 4, ret, heap->root->mso_list);
    return ret;
}

ERL_NIF_TERM enif_make_resource(ErlNifEnv *env, void *obj)
{
    struct RefcBinary *refc = refc_binary_from_data(obj);
    if (UNLIKELY(memory_erl_nif_env_ensure_free(env, TERM_BOXED_REFC_BINARY_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    return resource_to_term(refc, &env->heap);
}
