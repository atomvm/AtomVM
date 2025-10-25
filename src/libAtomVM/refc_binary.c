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
    refc->ref_count = 0;
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
        if (refc->resource_type) {
            if (refc->resource_type->down) {
                // There may be monitors associated with this resource.
                destroy_resource_monitors(refc, global);
                // After this point, the resource can no longer be found by
                // resource_type_fire_monitor
                // However, resource_type_fire_monitor may have incremented ref_count
                // to call the monitor handler.
                // So we check ref_count again. We're not affected by the ABA problem
                // here as the resource cannot (should not) be monitoring while it is
                // being destroyed, i.e. no resource monitor will be created now
                if (UNLIKELY(refc->ref_count != 0)) {
                    return false;
                }
            }
            // Remove the resource from the list of serialized resources
            // so it no longer can be unserialized.
            resource_unmark_serialized(refc->data, refc->resource_type);
        }
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

COLD_FUNC void refc_binary_dump_info(Context *ctx)
{
    struct ListHead *item;
    struct ListHead *refc_binaries = synclist_rdlock(&ctx->global->refc_binaries);

    // Note: This only counts non-const refc binaries (ones that allocate memory).
    // Const binaries (created by term_from_const_binary) point to existing data
    // and are never added to the global refc_binaries list, so they don't appear here.

    // First pass: count and calculate total size
    size_t count = 0;
    size_t total_size = 0;
    LIST_FOR_EACH (item, refc_binaries) {
        struct RefcBinary *refc = GET_LIST_ENTRY(item, struct RefcBinary, head);
        count++;
        total_size += refc->size;
    }

    fprintf(stderr, "refc_binary_count = %d\n", (int) count);
    fprintf(stderr, "refc_binary_total_size = %d\n", (int) total_size);

    if (count == 0) {
        synclist_unlock(&ctx->global->refc_binaries);
        return;
    }

// Find top 5 largest binaries
#define TOP_N 5
    struct RefcBinary *top[TOP_N] = { NULL };
    size_t top_indices[TOP_N] = { 0 };

    size_t index = 0;
    LIST_FOR_EACH (item, refc_binaries) {
        struct RefcBinary *refc = GET_LIST_ENTRY(item, struct RefcBinary, head);

        // Try to insert into top 5
        for (size_t i = 0; i < TOP_N; i++) {
            if (top[i] == NULL || refc->size > top[i]->size) {
                // Shift down
                for (size_t j = TOP_N - 1; j > i; j--) {
                    top[j] = top[j - 1];
                    top_indices[j] = top_indices[j - 1];
                }
                top[i] = refc;
                top_indices[i] = index;
                break;
            }
        }
        index++;
    }

    // Display top binaries
    fprintf(stderr, "\nTop %d largest refc binaries:\n", TOP_N);
    for (size_t i = 0; i < TOP_N && top[i] != NULL; i++) {
        struct RefcBinary *refc = top[i];
        fprintf(stderr, "  [%zu] size=%d bytes (%.1f%%), refcount=%d",
            top_indices[i],
            (int) refc->size,
            (double) refc->size * 100.0 / (double) total_size,
            (int) refc->ref_count);

        if (refc->resource_type) {
            fprintf(stderr, " [resource]");
        }

        // Print first 32 bytes as hex
        fprintf(stderr, "\n      data: ");
        size_t print_size = refc->size < 32 ? refc->size : 32;
        for (size_t j = 0; j < print_size; j++) {
            fprintf(stderr, "%02x", refc->data[j]);
            if (j % 4 == 3 && j < print_size - 1) {
                fprintf(stderr, " ");
            }
        }
        if (refc->size > 32) {
            fprintf(stderr, "...");
        }
        fprintf(stderr, "\n");
    }

    synclist_unlock(&ctx->global->refc_binaries);
}
