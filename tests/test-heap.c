/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

#include <assert.h>
#include <stdlib.h>

#include "context.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "memory.h"
#include "refc_binary.h"
#include "term.h"
#include "utils.h"

void test_memory_ensure_free(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;
    enum MemoryGCResult res = memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK);
    assert(res == MEMORY_GC_OK);
    size_t memory_size = memory_heap_memory_size(&ctx->heap);
    assert(memory_size == 0);

    res = memory_ensure_free(ctx, TUPLE_SIZE(3));
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == TUPLE_SIZE(3));

    term tuple = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(tuple, 0, OK_ATOM);
    term_put_tuple_element(tuple, 1, TRUE_ATOM);
    term_put_tuple_element(tuple, 2, FALSE_ATOM);

    res = memory_ensure_free_with_roots(ctx, 0, 1, &tuple, MEMORY_FORCE_SHRINK);
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == TUPLE_SIZE(3));

    res = memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK);
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == 0);
}

void test_gc_ref_count(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;
    enum MemoryGCResult res = memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK);
    assert(res == MEMORY_GC_OK);
    size_t memory_size = memory_heap_memory_size(&ctx->heap);
    assert(memory_size == 0);

    struct ListHead *refc_binaries = synclist_nolock(&glb->refc_binaries);
    assert(list_is_empty(refc_binaries));

    res = memory_ensure_free(ctx, TUPLE_SIZE(2) + 2 * TERM_BOXED_REFC_BINARY_SIZE);
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == TUPLE_SIZE(2) + 2 * TERM_BOXED_REFC_BINARY_SIZE);

    term refc_one = term_alloc_refc_binary(42, false, &ctx->heap, glb);
    struct RefcBinary *refc_one_ptr = term_refc_binary_ptr(refc_one);
    term refc_two = term_alloc_refc_binary(43, false, &ctx->heap, glb);
    struct RefcBinary *refc_two_ptr = term_refc_binary_ptr(refc_two);

    term tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple, 0, refc_one);
    term_put_tuple_element(tuple, 1, refc_two);

    term roots[2];
    roots[0] = tuple;
    roots[1] = refc_two;

    res = memory_ensure_free_with_roots(ctx, 0, 2, roots, MEMORY_FORCE_SHRINK);
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == TUPLE_SIZE(2) + 2 * TERM_BOXED_REFC_BINARY_SIZE);

    assert(refc_one_ptr->ref_count == 1);
    assert(refc_two_ptr->ref_count == 1);

    res = memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK);
    memory_size = memory_heap_memory_size(&ctx->heap);
    assert(res == MEMORY_GC_OK);
    assert(memory_size == 0);

    refc_binaries = synclist_nolock(&glb->refc_binaries);
    assert(list_is_empty(refc_binaries));
}

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_memory_ensure_free();
    test_gc_ref_count();

    return EXIT_SUCCESS;
}
