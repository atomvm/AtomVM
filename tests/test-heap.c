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
#include <stdbool.h>
#include <stdint.h>
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
    ctx->fullsweep_after = 65535;
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
    ctx->fullsweep_after = 65535;
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
    ctx->fullsweep_after = 65535;

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

    // Verify heap is usable after GC
    term tuple3 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple3, 0, term_from_int(200));
    term_put_tuple_element(tuple3, 1, tuple2);

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

void test_generational_gc_promotion(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;
    ctx->fullsweep_after = 65535;

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

    // Verify heap is usable after GC
    term tuple3 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple3, 0, term_from_int(4));
    term_put_tuple_element(tuple3, 1, tuple2);

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

void test_generational_gc_major_on_force_shrink(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;
    ctx->fullsweep_after = 65535;

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

    // Verify heap is usable after minor GC
    term tuple3 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple3, 0, term_from_int(4));
    term_put_tuple_element(tuple3, 1, roots[0]);

    // FORCE_SHRINK triggers major GC which frees old heap
    roots[0] = tuple3;
    res = memory_ensure_free_with_roots(ctx, 0, 1, roots, MEMORY_FORCE_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.old_heap_start == NULL);

    // Data should still be intact
    tuple3 = roots[0];
    tuple2 = term_get_tuple_element(tuple3, 1);
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
    ctx->fullsweep_after = 65535;

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

    // Verify heap is usable after minor GC
    term tuple3 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple3, 0, tuple1);
    term_put_tuple_element(tuple3, 1, term_from_int(99));

    roots[0] = tuple3;
    roots[1] = term_nil();
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(1), 2, roots, MEMORY_NO_SHRINK);
    assert(res == MEMORY_GC_OK);

    // refc should still be alive in old heap
    assert(refc_ptr->ref_count == 1);

    // Verify heap is usable
    tuple3 = roots[0];
    term tuple4 = term_alloc_tuple(1, &ctx->heap);
    term_put_tuple_element(tuple4, 0, tuple3);

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

    // Verify heap is usable after GC
    term tuple3 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple3, 0, term_from_int(200));
    term_put_tuple_element(tuple3, 1, tuple2);

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

// Build a long list incrementally across many minor GCs, keeping the whole
// list live throughout. Each iteration conses a fresh young cell onto a list
// whose tail has already been promoted to the old generation, so the old
// generation accumulates and is re-scanned by every subsequent minor GC. This
// exercises repeated promotion, old-heap growth and the dual-scan fixpoint on
// a realistic structure, and would catch corruption (e.g. a list rewritten
// into a cycle) that the round-trip-only flag test cannot.
void test_generational_gc_incremental_list(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;
    // Large threshold so collections are minor (not full) sweeps.
    ctx->fullsweep_after = 1000000;

    const int count = 500;
    term list = term_nil();
    term roots[1];

    for (int i = 0; i < count; i++) {
        roots[0] = list;
        // CONS_SIZE words for the new cell, forcing a GC most iterations.
        // MEMORY_NO_SHRINK keeps the post-GC shrink pass (a full GC) from
        // collapsing the old generation we want to accumulate and re-scan.
        enum MemoryGCResult res = memory_ensure_free_with_roots(
            ctx, CONS_SIZE, 1, roots, MEMORY_NO_SHRINK);
        assert(res == MEMORY_GC_OK);
        list = roots[0];

        term cell = term_list_prepend(term_from_int(i), list, &ctx->heap);
        list = cell;
    }

    // After many minor GCs an old generation must exist.
    assert(ctx->heap.old_heap_start != NULL);

    // Walk the whole list: every element must be intact and in order, and the
    // walk must terminate (a corrupted tail would loop forever / read garbage).
    term l = list;
    for (int i = count - 1; i >= 0; i--) {
        assert(term_is_nonempty_list(l));
        assert(term_get_list_head(l) == term_from_int(i));
        l = term_get_list_tail(l);
    }
    assert(term_is_nil(l));

    // Force a full GC and re-verify, confirming the old generation collapses
    // back into the young heap without corruption.
    roots[0] = list;
    enum MemoryGCResult res = memory_ensure_free_with_roots(ctx, 0, 1, roots, MEMORY_FORCE_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.old_heap_start == NULL);
    list = roots[0];

    l = list;
    for (int i = count - 1; i >= 0; i--) {
        assert(term_is_nonempty_list(l));
        assert(term_get_list_head(l) == term_from_int(i));
        l = term_get_list_tail(l);
    }
    assert(term_is_nil(l));

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

// With MEMORY_CAN_SHRINK — the mode used by virtually every GC-capable
// opcode — the post-GC shrink pass must not collapse the old generation a
// minor GC just built: collapsing it forces the next minor GC to re-promote
// the entire live set, so every collection copies the whole heap twice and the
// generational GC degenerates into a pathologically slow full-sweep collector
// (observed as a ~10x slowdown of test_estdlib's test_json, timing out CI on
// slower runners).
void test_generational_gc_can_shrink_keeps_old_heap(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    // Default growth strategy (BoundedFreeHeapGrowth), generational GC on.
    ctx->fullsweep_after = 65535;

    // Allocate a large tuple that will become the mature data.
    enum MemoryGCResult res = memory_ensure_free(ctx, TUPLE_SIZE(200));
    assert(res == MEMORY_GC_OK);
    term tuple1 = term_alloc_tuple(200, &ctx->heap);
    for (int i = 0; i < 200; i++) {
        term_put_tuple_element(tuple1, i, term_from_int(i));
    }

    term roots[1];
    roots[0] = tuple1;

    // Full GC (FORCE_SHRINK) sets the high water mark: tuple1 is now mature.
    res = memory_ensure_free_with_roots(ctx, 0, 1, roots, MEMORY_FORCE_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.old_heap_start == NULL);
    tuple1 = roots[0];

    // Allocate young data referencing the mature tuple.
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, roots, MEMORY_NO_SHRINK);
    assert(res == MEMORY_GC_OK);
    tuple1 = roots[0];
    term tuple2 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple2, 0, term_from_int(1000));
    term_put_tuple_element(tuple2, 1, tuple1);
    roots[0] = tuple2;

    // This allocation request triggers a minor GC that promotes tuple1 to the
    // old generation. The old generation must still be there when
    // memory_ensure_free_with_roots returns, even with MEMORY_CAN_SHRINK.
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(50), 1, roots, MEMORY_CAN_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.old_heap_start != NULL);
    size_t old_used = ctx->heap.old_heap_ptr - ctx->heap.old_heap_start;
    assert(old_used >= TUPLE_SIZE(200));

    // Verify data is intact.
    tuple2 = roots[0];
    assert(term_get_tuple_element(tuple2, 0) == term_from_int(1000));
    tuple1 = term_get_tuple_element(tuple2, 1);
    for (int i = 0; i < 200; i++) {
        assert(term_get_tuple_element(tuple1, i) == term_from_int(i));
    }

    // A further minor GC must reuse the old generation: tuple2 (now mature)
    // is promoted into it, and tuple1 is not re-promoted.
    term *old_heap_start = ctx->heap.old_heap_start;
    term tuple3 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple3, 0, term_from_int(2000));
    term_put_tuple_element(tuple3, 1, tuple2);
    roots[0] = tuple3;

    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(50), 1, roots, MEMORY_CAN_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.old_heap_start == old_heap_start);
    assert((size_t) (ctx->heap.old_heap_ptr - ctx->heap.old_heap_start)
        == old_used + TUPLE_SIZE(2));

    // Verify data is intact through both generations.
    tuple3 = roots[0];
    assert(term_get_tuple_element(tuple3, 0) == term_from_int(2000));
    tuple2 = term_get_tuple_element(tuple3, 1);
    assert(term_get_tuple_element(tuple2, 0) == term_from_int(1000));
    tuple1 = term_get_tuple_element(tuple2, 1);
    for (int i = 0; i < 200; i++) {
        assert(term_get_tuple_element(tuple1, i) == term_from_int(i));
    }

    // A forced shrink still collapses the old generation.
    roots[0] = tuple3;
    res = memory_ensure_free_with_roots(ctx, 0, 1, roots, MEMORY_FORCE_SHRINK);
    assert(res == MEMORY_GC_OK);
    assert(ctx->heap.old_heap_start == NULL);

    tuple3 = roots[0];
    assert(term_get_tuple_element(tuple3, 0) == term_from_int(2000));
    tuple2 = term_get_tuple_element(tuple3, 1);
    assert(term_get_tuple_element(tuple2, 0) == term_from_int(1000));
    tuple1 = term_get_tuple_element(tuple2, 1);
    for (int i = 0; i < 200; i++) {
        assert(term_get_tuple_element(tuple1, i) == term_from_int(i));
    }

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

// A very large fullsweep_after must not be truncated into forcing a full GC on
// every collection. With a multiple of 2^32 (a regression case when the field
// was a 32-bit unsigned int) minor GCs must still happen, i.e. an old
// generation must be created.
void test_fullsweep_after_large(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;
#if SIZE_MAX > UINT32_MAX
    // 2^32: truncating to unsigned int yields 0, which would force full GC.
    ctx->fullsweep_after = ((size_t) 1) << 32;
#else
    // 2^32 is not representable in a 32-bit size_t; use the largest value to
    // check that a huge threshold still permits minor GCs.
    ctx->fullsweep_after = SIZE_MAX;
#endif
    // The value must survive round-tripping through the heap word size.
    assert(ctx->fullsweep_after != 0);

    enum MemoryGCResult res = memory_ensure_free(ctx, TUPLE_SIZE(2));
    assert(res == MEMORY_GC_OK);

    term tuple1 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple1, 0, term_from_int(1));
    term_put_tuple_element(tuple1, 1, term_from_int(2));

    term roots[1];
    roots[0] = tuple1;

    // First GC sets HWM.
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, roots, MEMORY_CAN_SHRINK);
    assert(res == MEMORY_GC_OK);

    // Allocate young data referencing the now-mature tuple, then GC again.
    tuple1 = roots[0];
    term tuple2 = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tuple2, 0, term_from_int(3));
    term_put_tuple_element(tuple2, 1, tuple1);
    roots[0] = tuple2;

    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, roots, MEMORY_NO_SHRINK);
    assert(res == MEMORY_GC_OK);
    // A minor GC must have run (data promoted), proving the threshold was not
    // truncated to 0.
    assert(ctx->heap.old_heap_start != NULL);

    tuple2 = roots[0];
    assert(term_get_tuple_element(tuple2, 0) == term_from_int(3));
    tuple1 = term_get_tuple_element(tuple2, 1);
    assert(term_get_tuple_element(tuple1, 0) == term_from_int(1));
    assert(term_get_tuple_element(tuple1, 1) == term_from_int(2));

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

// A fragment swap (memory_heap_alloc growing the heap into a fresh root
// fragment) strands high_water_mark in the swapped-out fragment, where it
// no longer delimits a mature region of the current root. The swap must
// clear it so the next minor collection runs with an empty mature region;
// comparing the stale pointer against the new root's range (as memory_gc used to do) is
// undefined behaviour and spuriously enabled a minor GC with a garbage
// mature region, corrupting the heap.
void test_generational_gc_fragment_swap(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ctx->heap_growth_strategy = MinimumHeapGrowth;
    ctx->fullsweep_after = 65535;

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

    // A no-GC allocation larger than the free space swaps in a new root
    // fragment (the path NIFs take when they cannot GC)
    size_t free_space = context_avail_free_memory(ctx);
    res = memory_ensure_free_opt(ctx, free_space + 16, MEMORY_NO_GC);
    assert(res == MEMORY_GC_OK);

    // high_water_mark no longer belongs to the root fragment: it must be
    // cleared so the next minor GC runs with an empty mature region
    assert(ctx->heap.high_water_mark == NULL);

    // The next collection must preserve the data reachable from roots
    res = memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 2, roots, MEMORY_CAN_SHRINK);
    assert(res == MEMORY_GC_OK);
    tuple1 = roots[0];
    assert(term_get_tuple_arity(tuple1) == 2);
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
    test_generational_gc_incremental_list();
    test_generational_gc_can_shrink_keeps_old_heap();
    test_fullsweep_after_large();
    test_generational_gc_fragment_swap();

    return EXIT_SUCCESS;
}
