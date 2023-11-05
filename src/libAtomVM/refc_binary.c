/*
 * This file is part of AtomVM.
 *
 * Copyright 2021 Fred Dushin <fred@dushin.net>
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

#include <stdlib.h>

#include "refc_binary.h"

#include "context.h"
#include "dictionary.h"
#include "erl_nif_priv.h"
#include "memory.h"
#include "tempstack.h"
#include "utils.h"

// #define ENABLE_TRACE

#include "trace.h"

struct RefcBinary *refc_binary_create_resource(size_t size, struct ResourceType *resource_type)
{
    size_t n = sizeof(struct RefcBinary) + size;
    struct RefcBinary *refc = malloc(n);
    if (IS_NULL_PTR(refc)) {
        return NULL;
    }
    list_init(&refc->head);
    refc->ref_count = 1;
    refc->size = size;
    refc->resource_type = resource_type;

    return refc;
}

const char *refc_binary_get_data(const struct RefcBinary *refc)
{
    return (const char *) (&refc->data);
}

struct RefcBinary *refc_binary_from_data(void *ptr)
{
    return CONTAINER_OF(ptr, struct RefcBinary, data);
}

void refc_binary_increment_refcount(struct RefcBinary *refc)
{
    refc->ref_count++;
}

bool refc_binary_decrement_refcount(struct RefcBinary *refc, struct GlobalContext *global)
{
    if (--refc->ref_count == 0) {
        synclist_remove(&global->refc_binaries, &refc->head);
        refc_binary_destroy(refc, global);
        return true;
    }
    return false;
}

void refc_binary_destroy(struct RefcBinary *refc, struct GlobalContext *global)
{
    UNUSED(global);

    if (refc->resource_type) {
        if (refc->resource_type->down) {
            // There may be monitors associated with this resource.
            destroy_resource_monitors(refc, global);
        }
        if (refc->resource_type->dtor) {
            ErlNifEnv env;
            erl_nif_env_partial_init_from_globalcontext(&env, global);
            refc->resource_type->dtor(&env, (void *) &refc->data);
        }
    }
    free(refc);
}

term refc_binary_create_binary_info(Context *ctx)
{
    size_t len = 0;
    struct ListHead *item;
    struct ListHead *refc_binaries = synclist_wrlock(&ctx->global->refc_binaries);
    LIST_FOR_EACH (item, refc_binaries) {
        len++;
    }
    if (len == 0) {
        synclist_unlock(&ctx->global->refc_binaries);
        return term_nil();
    }
    if (memory_ensure_free(ctx, len * (2 + TUPLE_SIZE(2))) != MEMORY_GC_OK) {
        synclist_unlock(&ctx->global->refc_binaries);
        return term_invalid_term();
    }
    term ret = term_nil();
    LIST_FOR_EACH (item, refc_binaries) {
        struct RefcBinary *refc = GET_LIST_ENTRY(item, struct RefcBinary, head);
        term t = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(t, 0, term_from_int(refc->size));
        term_put_tuple_element(t, 1, term_from_int(refc->ref_count));
        ret = term_list_prepend(t, ret, &ctx->heap);
    }
    synclist_unlock(&ctx->global->refc_binaries);
    return ret;
}

size_t refc_binary_total_size(Context *ctx)
{
    size_t size = 0;
    struct ListHead *item;
    struct ListHead *refc_binaries = synclist_rdlock(&ctx->global->refc_binaries);
    LIST_FOR_EACH (item, refc_binaries) {
        struct RefcBinary *refc = GET_LIST_ENTRY(item, struct RefcBinary, head);
        size += refc->size;
    }
    synclist_unlock(&ctx->global->refc_binaries);
    return size;
}
