/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "context.h"
#include "debug.h"
#include "dictionary.h"
#include "erl_nif_priv.h"
#include "globalcontext.h"
#include "list.h"
#include "memory.h"
#include "refc_binary.h"
#include "tempstack.h"
#include "term.h"

// #define ENABLE_TRACE

#include "trace.h"
#include "utils.h"

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

#ifdef ENABLE_REALLOC_GC
#define MEMORY_SHRINK memory_shrink
#else
#define MEMORY_SHRINK memory_gc
#endif

static void memory_scan_and_copy(HeapFragment *old_fragment, term *mem_start, const term *mem_end, term **new_heap_pos, term *mso_list, bool move);
static term memory_shallow_copy_term(HeapFragment *old_fragment, term t, term **new_heap, bool move);
static enum MemoryGCResult memory_gc(Context *ctx, size_t new_size, size_t num_roots, term *roots);
#ifdef ENABLE_REALLOC_GC
static enum MemoryGCResult memory_shrink(Context *ctx, size_t new_size, size_t num_roots, term *roots);
static void memory_scan_and_rewrite(size_t count, term *terms, const term *old_start, const term *old_end, intptr_t delta, bool is_heap);
static inline term *memory_rewrite_pointer(term *ptr, const term *old_start, const term *old_end, intptr_t delta);
#endif

enum MemoryGCResult memory_init_heap(Heap *heap, size_t size)
{
    HeapFragment *fragment = (HeapFragment *) malloc(sizeof(HeapFragment) + size * sizeof(term));
    if (IS_NULL_PTR(fragment)) {
        return MEMORY_GC_ERROR_FAILED_ALLOCATION;
    }
    memory_init_heap_root_fragment(heap, fragment, size);
    return MEMORY_GC_OK;
}

void memory_init_heap_root_fragment(Heap *heap, HeapFragment *root, size_t size)
{
    heap->root = root;
    root->next = NULL;
    root->mso_list = term_nil();
    heap->heap_start = root->storage;
    heap->heap_ptr = heap->heap_start;
    heap->heap_end = heap->heap_start + size;
}

#ifdef ENABLE_REALLOC_GC
static inline enum MemoryGCResult memory_realloc_heap_root(Heap *heap, size_t size)
{
    uintptr_t used_size = heap->heap_ptr - heap->heap_start;
    HeapFragment *new_root = (HeapFragment *) realloc(heap->root, sizeof(HeapFragment) + size * sizeof(term));
    if (IS_NULL_PTR(new_root)) {
        return MEMORY_GC_ERROR_FAILED_ALLOCATION;
    }
    heap->root = new_root;
    heap->heap_start = new_root->storage;
    heap->heap_ptr = heap->heap_start + used_size;
    heap->heap_end = heap->heap_start + size;
    return MEMORY_GC_OK;
}
#endif

static inline enum MemoryGCResult memory_heap_alloc_new_fragment(Heap *heap, size_t size)
{
    HeapFragment *root_fragment = heap->root;
    term *old_end = heap->heap_end;
    term mso_list = root_fragment->mso_list;
    if (UNLIKELY(memory_init_heap(heap, size) != MEMORY_GC_OK)) {
        TRACE("Unable to allocate memory fragment.  size=%u\n", (unsigned int) size);
        return MEMORY_GC_ERROR_FAILED_ALLOCATION;
    }
    // Convert root fragment to non-root fragment.
    root_fragment->heap_end = old_end; // used to hold mso_list when it was the root fragment
    heap->root->next = root_fragment;
    heap->root->mso_list = mso_list;
    return MEMORY_GC_OK;
}

enum MemoryGCResult memory_erl_nif_env_ensure_free(ErlNifEnv *env, size_t size)
{
    if (erl_nif_env_is_context(env)) {
        return memory_ensure_free_opt((Context *) env, size, MEMORY_NO_GC);
    } else {
        // Check if we have a heap.
        if (env->heap.root) {
            // We have no stack pointer, free memory is the difference.
            size_t free_space = env->heap.heap_end - env->heap.heap_ptr;
            if (free_space < size) {
                return memory_heap_alloc_new_fragment(&env->heap, size);
            }
        } else {
            if (UNLIKELY(memory_init_heap(&env->heap, size) != MEMORY_GC_OK)) {
                TRACE("Unable to allocate memory fragment.  size=%u\n", (unsigned int) size);
                return MEMORY_GC_ERROR_FAILED_ALLOCATION;
            }
        }
    }
    return MEMORY_GC_OK;
}

// Follow Erlang/OTP 18 fibonacci series.
static size_t next_fibonacci_heap_size(size_t size)
{
    static const size_t fib_seq[] = { 12, 38, 51, 90, 142, 233, 376, 610, 987, 1598, 2586, 4185, 6772, 10958,
        17731, 28690, 46422, 75113, 121536, 196650, 318187, 514838, 833026,
        1347865, 2180892, 3528758, 5709651 };
    for (size_t i = 0; i < sizeof(fib_seq) / sizeof(fib_seq[0]); i++) {
        if (size <= fib_seq[i]) {
            return fib_seq[i];
        }
    }
    return size + size / 5;
}

#define FIBONACCI_HEAP_GROWTH_REDUCTION_THRESHOLD 10000

enum MemoryGCResult memory_ensure_free_with_roots(Context *c, size_t size, size_t num_roots, term *roots, enum MemoryAllocMode alloc_mode)
{
    size_t free_space = context_avail_free_memory(c);
    if (alloc_mode == MEMORY_NO_GC) {
        if (free_space < size) {
            return memory_heap_alloc_new_fragment(&c->heap, size);
        }
    } else {
        // Target heap size depends on:
        // - alloc_mode (MEMORY_FORCE_SHRINK takes precedence)
        // - heap growth strategy
        bool should_gc = free_space < size || (alloc_mode == MEMORY_FORCE_SHRINK) || c->heap.root->next != NULL;
        size_t memory_size = 0;
        if (!should_gc) {
            switch (c->heap_growth_strategy) {
                case BoundedFreeHeapGrowth: {
                    size_t maximum_free_space = 2 * (size + MIN_FREE_SPACE_SIZE);
                    should_gc = ((alloc_mode == MEMORY_CAN_SHRINK) && free_space - size > maximum_free_space);
                } break;
                case MinimumHeapGrowth:
                    should_gc = ((alloc_mode == MEMORY_CAN_SHRINK) && free_space > size);
                    break;
                case FibonacciHeapGrowth: {
                    memory_size = memory_heap_memory_size(&c->heap);
                    should_gc = ((alloc_mode == MEMORY_CAN_SHRINK) && free_space - size > 3 * memory_size / 4);
                    break;
                }
            }
        }
        if (should_gc) {
            if (memory_size == 0) {
                memory_size = memory_heap_memory_size(&c->heap);
            }
            size_t target_size;
            switch (c->heap_growth_strategy) {
                case BoundedFreeHeapGrowth:
                    if (free_space < size) {
                        target_size = memory_size + size + MIN_FREE_SPACE_SIZE;
                    } else {
                        size_t maximum_free_space = 2 * (size + MIN_FREE_SPACE_SIZE);
                        target_size = memory_size - free_space + maximum_free_space;
                    }
                    break;
                case MinimumHeapGrowth:
                    target_size = memory_size - free_space + size;
                    break;
                case FibonacciHeapGrowth:
                    target_size = next_fibonacci_heap_size(memory_size - free_space + size);
                    break;
                default:
                    UNREACHABLE();
            }
            target_size = MAX(c->has_min_heap_size ? c->min_heap_size : 0, target_size);
            if (UNLIKELY(c->has_max_heap_size && (target_size > c->max_heap_size))) {
                return MEMORY_GC_DENIED_ALLOCATION;
            }
            if (UNLIKELY(memory_gc(c, target_size, num_roots, roots) != MEMORY_GC_OK)) {
                // TODO: handle this more gracefully
                TRACE("Unable to allocate memory for GC.  target_size=%zu\n", target_size);
                return MEMORY_GC_ERROR_FAILED_ALLOCATION;
            }
            should_gc = alloc_mode == MEMORY_FORCE_SHRINK;
            size_t new_memory_size = memory_heap_memory_size(&c->heap);
            size_t new_target_size = new_memory_size;
            size_t new_free_space = context_avail_free_memory(c);
            switch (c->heap_growth_strategy) {
                case BoundedFreeHeapGrowth: {
                    size_t maximum_free_space = 2 * (size + MIN_FREE_SPACE_SIZE);
                    should_gc = should_gc || (alloc_mode != MEMORY_NO_SHRINK && new_free_space > maximum_free_space);
                    if (should_gc) {
                        new_target_size = (new_memory_size - new_free_space) + maximum_free_space;
                    }
                } break;
                case MinimumHeapGrowth:
                    should_gc = should_gc || (alloc_mode != MEMORY_NO_SHRINK && new_free_space > 0);
                    if (should_gc) {
                        new_target_size = new_memory_size - new_free_space + size;
                    }
                    break;
                case FibonacciHeapGrowth:
                    should_gc = should_gc || (new_memory_size > FIBONACCI_HEAP_GROWTH_REDUCTION_THRESHOLD && new_free_space >= 3 * new_memory_size / 4);
                    if (should_gc) {
                        new_target_size = next_fibonacci_heap_size(new_memory_size - new_free_space + size);
                    }
                    break;
            }
            if (should_gc) {
                new_target_size = MAX(c->has_min_heap_size ? c->min_heap_size : 0, new_target_size);
                if (new_target_size != new_memory_size) {
                    if (UNLIKELY(MEMORY_SHRINK(c, new_target_size, num_roots, roots) != MEMORY_GC_OK)) {
                        TRACE("Unable to allocate memory for GC shrink.  new_memory_size=%zu new_free_space=%zu size=%u\n", new_memory_size, new_free_space, (unsigned int) size);
                        return MEMORY_GC_ERROR_FAILED_ALLOCATION;
                    }
                }
            }
        }
    }

    return MEMORY_GC_OK;
}

static inline void push_to_stack(term **stack, term value)
{
    *stack = (*stack) - 1;
    **stack = value;
}

static enum MemoryGCResult memory_gc(Context *ctx, size_t new_size, size_t num_roots, term *roots)
{
    TRACE("Going to perform gc on process %i\n", ctx->process_id);
    term old_mso_list = ctx->heap.root->mso_list;
    term *old_stack_ptr = context_stack_base(ctx);
    term *old_heap_end = ctx->heap.heap_end;
    HeapFragment *old_root_fragment = ctx->heap.root;

    if (UNLIKELY(memory_init_heap(&ctx->heap, new_size) != MEMORY_GC_OK)) {
        return MEMORY_GC_ERROR_FAILED_ALLOCATION;
    }
    // We need old heap fragment to only copy terms that were in the heap (as opposed to in messages)
    old_root_fragment->heap_end = old_heap_end;

    term *new_heap = ctx->heap.heap_start;
    TRACE("- Allocated %i words for new heap at address 0x%p\n", (int) new_size, (void *) new_heap);

    TRACE("- Running copy GC on stack (stack size: %i)\n", (int) (old_stack_ptr - ctx->e));
    term *stack_ptr = new_heap + new_size;
    while (old_stack_ptr > ctx->e) {
        term new_root = memory_shallow_copy_term(old_root_fragment, *(--old_stack_ptr), &ctx->heap.heap_ptr, true);
        push_to_stack(&stack_ptr, new_root);
    }
    ctx->e = stack_ptr;

    struct ListHead *item;
    TRACE("- Running copy GC on process dictionary\n");
    LIST_FOR_EACH (item, &ctx->dictionary) {
        struct DictEntry *entry = GET_LIST_ENTRY(item, struct DictEntry, head);
        entry->key = memory_shallow_copy_term(old_root_fragment, entry->key, &ctx->heap.heap_ptr, true);
        entry->value = memory_shallow_copy_term(old_root_fragment, entry->value, &ctx->heap.heap_ptr, true);
    }

    TRACE("- Running copy GC on process extended registers\n");
    LIST_FOR_EACH (item, &ctx->extended_x_regs) {
        struct ExtendedRegister *ext_reg = GET_LIST_ENTRY(item, struct ExtendedRegister, head);
        ext_reg->value = memory_shallow_copy_term(
            old_root_fragment, ext_reg->value, &ctx->heap.heap_ptr, true);
    }

    TRACE("- Running copy GC on exit reason\n");
    ctx->exit_reason = memory_shallow_copy_term(old_root_fragment, ctx->exit_reason, &ctx->heap.heap_ptr, true);

    TRACE("- Running copy GC on group leader\n");
    ctx->group_leader = memory_shallow_copy_term(old_root_fragment, ctx->group_leader, &ctx->heap.heap_ptr, true);

    TRACE("- Running copy GC on provided roots\n");
    for (size_t i = 0; i < num_roots; i++) {
        roots[i] = memory_shallow_copy_term(old_root_fragment, roots[i], &ctx->heap.heap_ptr, true);
    }

    term *temp_start = new_heap;
    term *temp_end = ctx->heap.heap_ptr;
    term new_mso_list = term_nil();
    do {
        term *next_end = temp_end;
        TRACE("- Running scan and copy GC from %p to %p\n", (void *) temp_start, (void *) temp_end);
        memory_scan_and_copy(old_root_fragment, temp_start, temp_end, &next_end, &new_mso_list, true);
        temp_start = temp_end;
        temp_end = next_end;
    } while (temp_start != temp_end);

    ctx->heap.heap_ptr = temp_end;

    memory_sweep_mso_list(old_mso_list, ctx->global, false);
    ctx->heap.root->mso_list = new_mso_list;

    memory_destroy_heap_fragment(old_root_fragment);

    return MEMORY_GC_OK;
}

#ifdef ENABLE_REALLOC_GC
static enum MemoryGCResult memory_shrink(Context *ctx, size_t new_size, size_t num_roots, term *roots)
{
    TRACE("Going to perform shrink on process %i\n", ctx->process_id);

    // First, move stack up.
    term *old_stack_ptr = context_stack_base(ctx);
    size_t stack_size = old_stack_ptr - ctx->e;
    term *new_ctx_e = ctx->heap.heap_start + new_size - stack_size;
    memmove(new_ctx_e, ctx->e, stack_size * sizeof(term));
    ctx->e = new_ctx_e;

    term *old_heap_root = ctx->heap.root->storage;
    term *old_end = ctx->heap.heap_ptr;

    if (UNLIKELY(memory_realloc_heap_root(&ctx->heap, new_size) != MEMORY_GC_OK)) {
        return MEMORY_GC_ERROR_FAILED_ALLOCATION;
    }

    term *new_heap_root = ctx->heap.root->storage;
    intptr_t delta = new_heap_root - old_heap_root;
    ctx->e += delta;

    if (delta == 0) {
        return MEMORY_GC_OK;
    }

    // Rewrite all boxed pointers...
    // ...in stack
    memory_scan_and_rewrite(ctx->heap.heap_end - ctx->e, ctx->e, old_heap_root, old_end, delta, false);
    // ...in heap
    memory_scan_and_rewrite(old_end - old_heap_root, new_heap_root, old_heap_root, old_end, delta, true);
    // ...in roots
    memory_scan_and_rewrite(num_roots, roots, old_heap_root, old_end, delta, true);
    // ...in process dictionary
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->dictionary) {
        struct DictEntry *entry = GET_LIST_ENTRY(item, struct DictEntry, head);
        memory_scan_and_rewrite(2, &entry->key, old_heap_root, old_end, delta, true);
    }
    // ...in extended x registers
    LIST_FOR_EACH (item, &ctx->extended_x_regs) {
        struct ExtendedRegister *ext_reg = GET_LIST_ENTRY(item, struct ExtendedRegister, head);
        memory_scan_and_rewrite(1, &ext_reg->value, old_heap_root, old_end, delta, true);
    }
    // ...exit_reason
    memory_scan_and_rewrite(1, &ctx->exit_reason, old_heap_root, old_end, delta, true);
    // ...group_leader
    memory_scan_and_rewrite(1, &ctx->group_leader, old_heap_root, old_end, delta, true);
    // ...and MSO list.
    term *mso_ptr = &ctx->heap.root->mso_list;
    while (!term_is_nil(*mso_ptr)) {
        term *list_ptr = term_get_list_ptr(*mso_ptr);
        term *new_list_ptr = memory_rewrite_pointer(list_ptr, old_heap_root, old_end, delta);
        if (list_ptr != new_list_ptr) {
            *mso_ptr = ((term) new_list_ptr) | 0x1;
        }
        // Process head.
        term head = new_list_ptr[1];
        if (UNLIKELY(!term_is_boxed(head))) {
            fprintf(stderr, "Expected a boxed term, got %" TERM_X_FMT "\n", head);
            AVM_ABORT();
        }
        term *boxed_ptr = term_to_term_ptr(head);
        term *new_boxed_ptr = memory_rewrite_pointer(boxed_ptr, old_heap_root, old_end, delta);
        if (boxed_ptr != new_boxed_ptr) {
            new_list_ptr[1] = ((term) new_boxed_ptr) | TERM_PRIMARY_BOXED;
        }
        // Loop with tail.
        mso_ptr = new_list_ptr;
    }

    return MEMORY_GC_OK;
}
#endif

static inline bool memory_is_moved_marker(term *t)
{
    // 0x2B is an unused tag
    return *t == 0x2B;
}

static inline void memory_replace_with_moved_marker(term *to_be_replaced, term replace_with)
{
    to_be_replaced[0] = 0x2B;
    to_be_replaced[1] = replace_with;
}

static inline term memory_dereference_moved_marker(const term *moved_marker)
{
    return moved_marker[1];
}

static term memory_copy_term_tree_internal(term **heap_ptr, term *mso_list, term t)
{
    TRACE("Copy term tree: %p, heap_ptr: %p\n", (void *) t, (void *) *heap_ptr);

    term *temp_start = *heap_ptr;
    term copied_term = memory_shallow_copy_term(NULL, t, heap_ptr, false);
    term *temp_end = *heap_ptr;

    do {
        term *next_end = temp_end;
        memory_scan_and_copy(NULL, temp_start, temp_end, &next_end, mso_list, false);
        temp_start = temp_end;
        temp_end = next_end;
    } while (temp_start != temp_end);

    *heap_ptr = temp_end;

    return copied_term;
}

term memory_copy_term_tree(Heap *new_heap, term t)
{
    return memory_copy_term_tree_internal(&new_heap->heap_ptr, &new_heap->root->mso_list, t);
}

term memory_copy_term_tree_to_storage(term *storage, term **heap_end, term t)
{
    term *heap_ptr = storage + STORAGE_HEAP_START_INDEX;
    storage[STORAGE_MSO_LIST_INDEX] = term_nil(); // mso_list
    term result = memory_copy_term_tree_internal(&heap_ptr, &storage[STORAGE_MSO_LIST_INDEX], t);
    *heap_end = heap_ptr;
    return result;
}

unsigned long memory_estimate_usage(term t)
{
    unsigned long acc = 0;

    struct TempStack temp_stack;
    if (UNLIKELY(temp_stack_init(&temp_stack) != TempStackOk)) {
        // TODO: handle failed malloc
        AVM_ABORT();
    }

    if (UNLIKELY(temp_stack_push(&temp_stack, t) != TempStackOk)) {
        // TODO: handle failed malloc
        AVM_ABORT();
    }

    while (!temp_stack_is_empty(&temp_stack)) {
        switch (t & TERM_PRIMARY_MASK) {
            case TERM_PRIMARY_IMMED:
                t = temp_stack_pop(&temp_stack);
                break;
            case TERM_PRIMARY_LIST:
                acc += 2;
                if (UNLIKELY(temp_stack_push(&temp_stack, term_get_list_tail(t)) != TempStackOk)) {
                    // TODO: handle failed malloc
                    AVM_ABORT();
                }
                t = term_get_list_head(t);
                break;
            case TERM_PRIMARY_BOXED: {
                const term *boxed_value = term_to_const_term_ptr(t);
                term boxed_value_0 = boxed_value[0];
                switch (boxed_value_0 & TERM_BOXED_TAG_MASK) {
                    case TERM_BOXED_TUPLE: {
                        int tuple_size = term_get_size_from_boxed_header(boxed_value_0);
                        acc += tuple_size + 1;

                        if (tuple_size > 0) {
                            for (int i = 1; i < tuple_size; i++) {
                                if (UNLIKELY(temp_stack_push(&temp_stack, term_get_tuple_element(t, i)) != TempStackOk)) {
                                    // TODO: handle failed malloc
                                    AVM_ABORT();
                                }
                            }
                            t = term_get_tuple_element(t, 0);

                        } else {
                            t = term_nil();
                        }
                    } break;

                    case TERM_BOXED_MAP: {
                        int map_size = term_get_map_size(t);
                        acc += term_map_size_in_terms(map_size);
                        if (map_size > 0) {
                            for (int i = 1; i < map_size; i++) {
                                if (UNLIKELY(temp_stack_push(&temp_stack, term_get_map_key(t, i)) != TempStackOk)) {
                                    // TODO: handle failed malloc
                                    AVM_ABORT();
                                }
                                if (UNLIKELY(temp_stack_push(&temp_stack, term_get_map_value(t, i)) != TempStackOk)) {
                                    // TODO: handle failed malloc
                                    AVM_ABORT();
                                }
                            }
                            if (UNLIKELY(temp_stack_push(&temp_stack, term_get_map_value(t, 0)) != TempStackOk)) {
                                // TODO: handle failed malloc
                                AVM_ABORT();
                            }
                            t = term_get_map_key(t, 0);

                        } else {
                            t = term_nil();
                        }
                    } break;

                    case TERM_BOXED_FUN: {
                        int boxed_size = term_get_size_from_boxed_header(boxed_value_0);
                        acc += boxed_size + 1;

                        // We skip the first two elements:
                        // First is either a module atom or a pointer to a Module
                        // Second is either a function atom or a function index
                        // Third would be arity as a term int (external function) or
                        // the first argument (if built with make_fun3) which we should
                        // estimate.
                        for (int i = 2; i < boxed_size; i++) {
                            if (UNLIKELY(temp_stack_push(&temp_stack, boxed_value[i + 1]) != TempStackOk)) {
                                // TODO: handle failed malloc
                                AVM_ABORT();
                            }
                        }
                        t = boxed_value[2];
                    } break;

                    case TERM_BOXED_SUB_BINARY: {
                        int boxed_size = term_get_size_from_boxed_header(boxed_value_0);
                        acc += boxed_size + 1;
                        t = term_get_sub_binary_ref(t);
                    } break;

                    default: {
                        // Default type of boxed terms
                        int boxed_size = term_get_size_from_boxed_header(boxed_value_0);
                        acc += boxed_size + 1;
                        t = temp_stack_pop(&temp_stack);
                    }
                }
            } break;

            default:
                UNREACHABLE();
        }
    }

    temp_stack_destroy(&temp_stack);

    return acc;
}

static void memory_scan_and_copy(HeapFragment *old_fragment, term *mem_start, const term *mem_end, term **new_heap_pos, term *mso_list, bool move)
{
    term *ptr = mem_start;
    term *new_heap = *new_heap_pos;

    while (ptr < mem_end) {
        term t = *ptr;
        switch (t & TERM_PRIMARY_MASK) {
            case TERM_PRIMARY_IMMED:
                TRACE("Found immediate (%" TERM_X_FMT ")\n", t);
                ptr++;
                break;
            case TERM_PRIMARY_CP: {
                TRACE("Found boxed header (%" TERM_X_FMT ")\n", t);

                size_t arity = term_get_size_from_boxed_header(t);
                switch (t & TERM_BOXED_TAG_MASK) {
                    case TERM_BOXED_TUPLE: {
                        TRACE("- Boxed is tuple (%" TERM_X_FMT "), arity: %i\n", t, (int) arity);

                        for (size_t i = 1; i <= arity; i++) {
                            TRACE("-- Elem: %" TERM_X_FMT "\n", ptr[i]);
                            ptr[i] = memory_shallow_copy_term(old_fragment, ptr[i], &new_heap, move);
                        }
                        break;
                    }

                    case TERM_BOXED_BIN_MATCH_STATE: {
                        TRACE("- Found bin match state.\n");
                        ptr[1] = memory_shallow_copy_term(old_fragment, ptr[1], &new_heap, move);
                        break;
                    }

                    case TERM_BOXED_POSITIVE_INTEGER:
                        TRACE("- Found boxed pos int.\n");
                        break;

                    case TERM_BOXED_NEGATIVE_INTEGER:
                        TRACE("- Found boxed neg int.\n");
                        break;

                    case TERM_BOXED_REF:
                        TRACE("- Found ref.\n");
                        break;

                    case TERM_BOXED_EXTERNAL_PID:
                        TRACE("- Found external pid.\n");
                        break;

                    case TERM_BOXED_EXTERNAL_PORT:
                        TRACE("- Found external port.\n");
                        break;

                    case TERM_BOXED_EXTERNAL_REF:
                        TRACE("- Found external ref.\n");
                        break;

                    case TERM_BOXED_FUN: {
                        TRACE("- Found fun, size: %i.\n", (int) arity);

                        // first term is the boxed header, followed by module and fun index.

                        for (size_t i = 3; i <= arity; i++) {
                            TRACE("-- Frozen: %" TERM_X_FMT "\n", ptr[i]);
                            ptr[i] = memory_shallow_copy_term(old_fragment, ptr[i], &new_heap, move);
                        }
                        break;
                    }

                    case TERM_BOXED_FLOAT:
                        TRACE("- Found float.\n");
                        break;

                    case TERM_BOXED_REFC_BINARY: {
                        TRACE("- Found refc binary.\n");
                        term ref = ((term) ptr) | TERM_PRIMARY_BOXED;
                        if (!term_refc_binary_is_const(ref)) {
                            *mso_list = term_list_init_prepend(ptr + REFC_BINARY_CONS_OFFSET, ref, *mso_list);
                            refc_binary_increment_refcount((struct RefcBinary *) term_refc_binary_ptr(ref));
                        }
                        break;
                    }

                    case TERM_BOXED_SUB_BINARY: {
                        TRACE("- Found sub binary.\n");
                        ptr[3] = memory_shallow_copy_term(old_fragment, ptr[3], &new_heap, move);
                        break;
                    }

                    case TERM_BOXED_HEAP_BINARY:
                        TRACE("- Found binary.\n");
                        break;

                    case TERM_BOXED_MAP: {
                        TRACE("- Found map.\n");
                        size_t map_size = arity - 1;
                        size_t keys_offset = term_get_map_keys_offset();
                        size_t value_offset = term_get_map_value_offset();
                        TRACE("-- Map keys: %" TERM_X_FMT "\n", ptr[keys_offset]);
                        ptr[keys_offset] = memory_shallow_copy_term(old_fragment, ptr[keys_offset], &new_heap, move);
                        for (size_t i = value_offset; i < value_offset + map_size; ++i) {
                            TRACE("-- Map Value: %" TERM_X_FMT "\n", ptr[i]);
                            ptr[i] = memory_shallow_copy_term(old_fragment, ptr[i], &new_heap, move);
                        }
                    } break;

                    default:
                        fprintf(stderr, "- Found unknown boxed type: %" TERM_X_FMT "\n", (t >> 2) & 0xF);
                        AVM_ABORT();
                }

                ptr += arity + 1;
                break;
            }
            case TERM_PRIMARY_LIST:
                TRACE("Found nonempty list (%p)\n", (void *) t);
                *ptr = memory_shallow_copy_term(old_fragment, t, &new_heap, move);
                ptr++;
                break;
            case TERM_PRIMARY_BOXED:
                TRACE("Found boxed (%p)\n", (void *) t);
                *ptr = memory_shallow_copy_term(old_fragment, t, &new_heap, move);
                ptr++;
                break;
            default:
                UNREACHABLE();
        }
    }

    *new_heap_pos = new_heap;
}

#ifdef ENABLE_REALLOC_GC
static inline term *memory_rewrite_pointer(term *ptr, const term *old_start, const term *old_end, intptr_t delta)
{
    if (ptr >= old_start && ptr < old_end) {
        return ptr + delta;
    }
    return ptr;
}

static void memory_scan_and_rewrite(size_t count, term *terms, const term *old_start, const term *old_end, intptr_t delta, bool is_heap)
{
    term *ptr = terms;
    term *end = terms + count;
    while (ptr < end) {
        term t = *ptr++;
        if (is_heap && (t & 0x3) == 0x0) {
            switch (t & TERM_BOXED_TAG_MASK) {
                case TERM_BOXED_TUPLE:
                    // Skip header and process elements
                    break;

                case TERM_BOXED_BIN_MATCH_STATE: {
                    // there is a boxed binary that needs to be rewritten and then it's integers.
                    term binary_or_state = *ptr;
                    if (UNLIKELY(!term_is_boxed(binary_or_state))) {
                        fprintf(stderr, "Expected a boxed term, got %" TERM_X_FMT "\n", binary_or_state);
                        AVM_ABORT();
                    }
                    term *boxed_ptr = term_to_term_ptr(binary_or_state);
                    term *new_boxed_ptr = memory_rewrite_pointer(boxed_ptr, old_start, old_end, delta);
                    if (boxed_ptr != new_boxed_ptr) {
                        *ptr = ((term) new_boxed_ptr) | TERM_PRIMARY_BOXED;
                    }
                    ptr += term_get_size_from_boxed_header(t);
                    break;
                }

                case TERM_BOXED_POSITIVE_INTEGER:
                    ptr += term_get_size_from_boxed_header(t);
                    break;

                case TERM_BOXED_NEGATIVE_INTEGER:
                    ptr += term_get_size_from_boxed_header(t);
                    break;

                case TERM_BOXED_REF:
                    ptr += term_get_size_from_boxed_header(t);
                    break;

                case TERM_BOXED_EXTERNAL_PID:
                    ptr += term_get_size_from_boxed_header(t);
                    break;

                case TERM_BOXED_EXTERNAL_PORT:
                    ptr += term_get_size_from_boxed_header(t);
                    break;

                case TERM_BOXED_EXTERNAL_REF:
                    ptr += term_get_size_from_boxed_header(t);
                    break;

                case TERM_BOXED_FUN:
                    // Skip header and module and process next terms
                    ptr++;
                    break;

                case TERM_BOXED_FLOAT:
                    ptr += term_get_size_from_boxed_header(t);
                    break;

                case TERM_BOXED_REFC_BINARY:
                    ptr += term_get_size_from_boxed_header(t);
                    break;

                case TERM_BOXED_SUB_BINARY:
                    // Skip to binary
                    ptr += 2;
                    break;

                case TERM_BOXED_HEAP_BINARY:
                    ptr += term_get_size_from_boxed_header(t);
                    break;

                case TERM_BOXED_MAP:
                    // Skip header and process next term
                    break;

                default:
                    fprintf(stderr, "- Found unknown boxed type: %" TERM_X_FMT "\n", t & TERM_BOXED_TAG_MASK);
                    AVM_ABORT();
            }
        } else if (term_is_nonempty_list(t)) {
            term *list_ptr = term_get_list_ptr(t);
            term *new_list_ptr = memory_rewrite_pointer(list_ptr, old_start, old_end, delta);
            if (list_ptr != new_list_ptr) {
                ptr[-1] = ((term) new_list_ptr) | 0x1;
            }

        } else if (term_is_boxed(t)) {
            term *boxed_ptr = term_to_term_ptr(t);
            term *new_boxed_ptr = memory_rewrite_pointer(boxed_ptr, old_start, old_end, delta);
            if (boxed_ptr != new_boxed_ptr) {
                ptr[-1] = ((term) new_boxed_ptr) | TERM_PRIMARY_BOXED;
            }
        }
    }
}
#endif

HOT_FUNC static inline bool memory_heap_fragment_contains_pointer(HeapFragment *old_fragment, term *ptr)
{
    do {
        if (ptr >= old_fragment->storage && ptr < old_fragment->heap_end) {
            return true;
        }
        old_fragment = old_fragment->next;
    } while (old_fragment);
    return false;
}

HOT_FUNC static term memory_shallow_copy_term(HeapFragment *old_fragment, term t, term **new_heap, bool move)
{
    switch (t & TERM_PRIMARY_MASK) {
        case TERM_PRIMARY_IMMED:
            return t;

        case TERM_PRIMARY_CP:
            // CP is valid only on stack
            // catch label is valid only on stack
            return t;

        case TERM_PRIMARY_BOXED: {
            term *boxed_value = term_to_term_ptr(t);
            // Do not GC terms from messages until the message is destroyed
            if (old_fragment != NULL && !memory_heap_fragment_contains_pointer(old_fragment, boxed_value)) {
                return t;
            }

            if (memory_is_moved_marker(boxed_value)) {
                return memory_dereference_moved_marker(boxed_value);
            }

            int boxed_size = term_boxed_size(t) + 1;

            // It must be an empty tuple, so we are not going to use moved markers.
            // Empty tuples memory is too small to store moved markers.
            // However it is also required to avoid boxed terms duplication.
            // So instead all empty tuples will reference the same boxed term.
            if (boxed_size == 1) {
                return ((term) &empty_tuple) | TERM_PRIMARY_BOXED;
            }

            term *dest = *new_heap;
            for (int i = 0; i < boxed_size; i++) {
                dest[i] = boxed_value[i];
            }
            *new_heap += boxed_size;

            term new_term = ((term) dest) | TERM_PRIMARY_BOXED;

            if (move) {
                memory_replace_with_moved_marker(boxed_value, new_term);
            }

            return new_term;
        }
        case TERM_PRIMARY_LIST: {
            term *list_ptr = term_get_list_ptr(t);
            if (old_fragment != NULL && !memory_heap_fragment_contains_pointer(old_fragment, list_ptr)) {
                return t;
            }

            if (memory_is_moved_marker(list_ptr)) {
                return memory_dereference_moved_marker(list_ptr);
            }

            term *dest = *new_heap;
            dest[0] = list_ptr[0];
            dest[1] = list_ptr[1];
            *new_heap += 2;

            term new_term = ((term) dest) | 0x1;

            if (move) {
                memory_replace_with_moved_marker(list_ptr, new_term);
            }

            return new_term;
        }
        default:
            UNREACHABLE();
    }
}

void memory_heap_append_fragment(Heap *heap, HeapFragment *fragment, term mso_list)
{
    // The fragment we are appending may have next fragments
    // So we take our current next and we add it to the tail of the passed list
    if (heap->root->next) {
        HeapFragment *tail = fragment;
        while (tail->next != NULL) {
            tail = tail->next;
        }
        tail->next = heap->root->next;
    }
    // The passed fragment is set as next, heap's root fragment is unmodified
    // as root fragment is different, holding the mso list
    heap->root->next = fragment;
    if (!term_is_nil(mso_list)) {
        // Suppose fragment mso_list is smaller and append heap mso at the end
        term old_mso = heap->root->mso_list;
        heap->root->mso_list = mso_list;
        do {
            term *list_ptr = term_get_list_ptr(mso_list);
            if (term_is_nonempty_list(*list_ptr)) {
                mso_list = *list_ptr;
            } else {
                *list_ptr = old_mso;
                break;
            }
        } while (true);
    }
}

void memory_sweep_mso_list(term mso_list, GlobalContext *global, bool from_task)
{
#ifndef AVM_TASK_DRIVER_ENABLED
    UNUSED(from_task)
#endif
    term l = mso_list;
    while (l != term_nil()) {
        term h = term_get_list_head(l);
        // the mso list only contains boxed values; each refc is unique
        TERM_DEBUG_ASSERT(term_is_boxed(h));
        term *boxed_value = term_to_term_ptr(h);
        if (memory_is_moved_marker(boxed_value)) {
            h = memory_dereference_moved_marker(boxed_value);
        }
        if (term_is_refc_binary(h) && !term_refc_binary_is_const(h)) {
            // unreferenced binary; decrement reference count
#ifdef AVM_TASK_DRIVER_ENABLED
            if (from_task) {
                globalcontext_refc_decrement_refcount_from_task(global, term_refc_binary_ptr(h));
            } else {
#endif
                refc_binary_decrement_refcount(term_refc_binary_ptr(h), global);
#ifdef AVM_TASK_DRIVER_ENABLED
            }
#endif
        }
        l = term_get_list_tail(l);
    }
}
