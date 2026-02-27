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

void test_generational_gc_basic(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;

    // Allocate a tuple and GC to set HWM
    enum MemoryGCResult res = memory_ensure_free(ctx, TUPLE_SIZE(2));
    assert(res == MEMORY_GC_OK);

    term tuple1 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple1, 0, term_from_int(42));
    term_put_tuple_element(tuple1, 1, term_from_int(43));

    term roots[2];
    roots[0] = tuple1;
    roots[1] = term_nil();

    // First GC sets HWM
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 2, roots, MEMORY_CAN_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.high_water_mark != NULL);
    tuple1 = roots[0];
    assert(term_get_tuple_arity(tuple1) == 2);
    assert(term_get_tuple_element(tuple1, 0) == term_from_int(42));
    assert(term_get_tuple_element(tuple1, 1) == term_from_int(43));

    // Allocate more data above HWM
    term tuple2 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple2, 0, term_from_int(100));
    term_put_tuple_element(tuple2, 1, tuple1);
    roots[1] = tuple2;

    // Second GC should be minor (HWM is set, gc_count < fullsweep_after)
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 2, roots, MEMORY_CAN_SHRINK);
    assert(res == MEMORY_GC_OK);
    tuple1 = roots[0];
    tuple2 = roots[1];

    // Verify data survived
    assert(term_get_tuple_arity(tuple1) == 2);
    assert(term_get_tuple_element(tuple1, 0) == term_from_int(42));
    assert(term_get_tuple_element(tuple1, 1) == term_from_int(43));
    assert(term_get_tuple_arity(tuple2) == 2);
    assert(term_get_tuple_element(tuple2, 0) == term_from_int(100));
    assert(term_get_tuple_element(tuple2, 1) == tuple1);

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

void test_generational_gc_promotion(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;

    // Allocate and GC to promote data to mature
    enum MemoryGCResult res = memory_ensure_free(ctx, TUPLE_SIZE(2));
    assert(res == MEMORY_GC_OK);

    term tuple1 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple1, 0, term_from_int(1));
    term_put_tuple_element(tuple1, 1, term_from_int(2));

    term roots[1];
    roots[0] = tuple1;

    // First GC: sets HWM
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, roots, MEMORY_CAN_SHRINK);
    assert(res == MEMORY_GC_OK);
    tuple1 = roots[0];

    // Allocate young data
    term tuple2 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple2, 0, term_from_int(3));
    term_put_tuple_element(tuple2, 1, tuple1);
    roots[0] = tuple2;

    // Second GC: minor GC should promote tuple1 to old heap
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, roots, MEMORY_NO_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.old_heap_start != NULL);
    assert(ctx->heap.old_heap_ptr > ctx->heap.old_heap_start);

    // Verify data is intact
    tuple2 = roots[0];
    assert(term_get_tuple_element(tuple2, 0) == term_from_int(3));
    tuple1 = term_get_tuple_element(tuple2, 1);
    assert(term_get_tuple_element(tuple1, 0) == term_from_int(1));
    assert(term_get_tuple_element(tuple1, 1) == term_from_int(2));

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

void test_generational_gc_major_on_force_shrink(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;

    // Build up an old heap
    enum MemoryGCResult res = memory_ensure_free(ctx, TUPLE_SIZE(2));
    assert(res == MEMORY_GC_OK);

    term tuple1 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple1, 0, term_from_int(1));
    term_put_tuple_element(tuple1, 1, term_from_int(2));

    term roots[1];
    roots[0] = tuple1;

    // First GC: sets HWM
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, roots, MEMORY_CAN_SHRINK);
    assert(res == MEMORY_GC_OK);
    tuple1 = roots[0];

    // Allocate young, then minor GC to promote
    term tuple2 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple2, 0, term_from_int(3));
    term_put_tuple_element(tuple2, 1, tuple1);
    roots[0] = tuple2;

    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, roots, MEMORY_NO_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.old_heap_start != NULL);

    // FORCE_SHRINK triggers major GC which frees old heap
    res = memory_ensure_free_with_roots(ctx, 0, 1, roots, MEMORY_FORCE_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.old_heap_start == NULL);

    // Data should still be intact
    tuple2 = roots[0];
    assert(term_get_tuple_element(tuple2, 0) == term_from_int(3));
    tuple1 = term_get_tuple_element(tuple2, 1);
    assert(term_get_tuple_element(tuple1, 0) == term_from_int(1));
    assert(term_get_tuple_element(tuple1, 1) == term_from_int(2));

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

void test_generational_gc_mso(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;

    struct ListHead *refc_binaries = synclist_nolock(&glb->refc_binaries);
    assert(list_is_empty(refc_binaries));

    // Allocate a refc binary and GC to set HWM
    enum MemoryGCResult res = memory_ensure_free(ctx, TERM_BOXED_REFC_BINARY_SIZE + TUPLE_SIZE(1));
    assert(res == MEMORY_GC_OK);

    term refc = term_alloc_refc_binary(42, false, &ctx->heap, glb);
    struct RefcBinary *refc_ptr = term_refc_binary_ptr(refc);
    assert(refc_ptr->ref_count == 1);

    term tuple1 = term_alloc_tuple(1, &ctx->heap);
    term_put_tuple_element(tuple1, 0, refc);

    term roots[2];
    roots[0] = tuple1;
    roots[1] = term_nil();

    // First GC: sets HWM, tuple1+refc are below HWM after this
    res = memory_ensure_free_with_roots(ctx, TERM_BOXED_REFC_BINARY_SIZE + TUPLE_SIZE(2), 2, roots, MEMORY_CAN_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(refc_ptr->ref_count == 1);
    tuple1 = roots[0];

    // Allocate a second refc binary (young) and a tuple referencing both
    term refc2 = term_alloc_refc_binary(43, false, &ctx->heap, glb);
    struct RefcBinary *refc2_ptr = term_refc_binary_ptr(refc2);

    term tuple2 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple2, 0, refc2);
    term_put_tuple_element(tuple2, 1, tuple1);
    roots[0] = tuple2;
    roots[1] = term_nil();

    // Minor GC: refc (via tuple1) should be promoted to old heap, refc2 stays young
    res = memory_ensure_free_with_roots(ctx, TERM_BOXED_REFC_BINARY_SIZE + TUPLE_SIZE(2), 2, roots, MEMORY_NO_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(refc_ptr->ref_count == 1);
    assert(refc2_ptr->ref_count == 1);
    assert(ctx->heap.old_heap_start != NULL);

    // Drop refc2: keep only tuple1 (from old heap) via a new young tuple
    tuple2 = roots[0];
    tuple1 = term_get_tuple_element(tuple2, 1);
    roots[0] = tuple1;
    roots[1] = term_nil();
    res = memory_ensure_free_with_roots(ctx, 1, 2, roots, MEMORY_NO_SHRINK);
    assert(res == MEMORY_GC_OK);

    // refc should still be alive in old heap
    assert(refc_ptr->ref_count == 1);

    // Major GC: drop everything
    roots[0] = term_nil();
    res = memory_ensure_free_with_roots(ctx, 0, 1, roots, MEMORY_FORCE_SHRINK);
    assert(res == MEMORY_GC_OK);

    refc_binaries = synclist_nolock(&glb->refc_binaries);
    assert(list_is_empty(refc_binaries));

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

void test_fullsweep_after_zero(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;
    ctx->fullsweep_after = 0;

    // Allocate and GC
    enum MemoryGCResult res = memory_ensure_free(ctx, TUPLE_SIZE(2));
    assert(res == MEMORY_GC_OK);

    term tuple1 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple1, 0, term_from_int(42));
    term_put_tuple_element(tuple1, 1, term_from_int(43));

    term roots[1];
    roots[0] = tuple1;

    // With fullsweep_after=0, GC should always be full, never creating old heap
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, roots, MEMORY_CAN_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.old_heap_start == NULL);

    tuple1 = roots[0];
    term tuple2 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple2, 0, term_from_int(100));
    term_put_tuple_element(tuple2, 1, tuple1);
    roots[0] = tuple2;

    // Second GC: still full sweep
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, roots, MEMORY_CAN_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.old_heap_start == NULL);

    // Verify data intact
    tuple2 = roots[0];
    assert(term_get_tuple_element(tuple2, 0) == term_from_int(100));
    tuple1 = term_get_tuple_element(tuple2, 1);
    assert(term_get_tuple_element(tuple1, 0) == term_from_int(42));
    assert(term_get_tuple_element(tuple1, 1) == term_from_int(43));

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_memory_ensure_free();
    test_gc_ref_count();
    test_generational_gc_basic();
    test_generational_gc_promotion();
    test_generational_gc_major_on_force_shrink();
    test_generational_gc_mso();
    test_fullsweep_after_zero();

    return EXIT_SUCCESS;
}
