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

#include <stdlib.h>
#include <string.h>

#include "context.h"
#include "debug.h"
#include "dictionary.h"
#include "list.h"
#include "memory.h"
#include "refc_binary.h"
#include "tempstack.h"

//#define ENABLE_TRACE

#include "trace.h"

#define MAX(a, b) ((a) > (b) ? (a) : (b))

static void memory_scan_and_copy(term *mem_start, const term *mem_end, term **new_heap_pos, term *mso_list, int move);
static term memory_shallow_copy_term(term t, term **new_heap, int move);

HOT_FUNC term *memory_heap_alloc(Context *c, uint32_t size)
{
    term *allocated = c->heap_ptr;
    c->heap_ptr += size;

    return allocated;
}

MALLOC_LIKE term *memory_alloc_heap_fragment(Context *ctx, uint32_t fragment_size)
{
    struct ListHead *heap_fragment = malloc(fragment_size * sizeof(term) + sizeof(struct ListHead));
    if (IS_NULL_PTR(heap_fragment)) {
        return NULL;
    }
    list_append(&ctx->heap_fragments, heap_fragment);
    ctx->heap_fragments_size += fragment_size;
    return (term *) (heap_fragment + 1);
}

enum MemoryGCResult memory_ensure_free(Context *c, uint32_t size)
{
    size_t free_space = context_avail_free_memory(c);
    if (free_space < size + MIN_FREE_SPACE_SIZE) {
        size_t memory_size = context_memory_size(c);
        if (UNLIKELY(memory_gc(c, memory_size + size + MIN_FREE_SPACE_SIZE) != MEMORY_GC_OK)) {
            //TODO: handle this more gracefully
            TRACE("Unable to allocate memory for GC.  memory_size=%zu size=%u\n", memory_size, size);
            return MEMORY_GC_ERROR_FAILED_ALLOCATION;
        }
        size_t new_free_space = context_avail_free_memory(c);
        size_t new_minimum_free_space = 2 * (size + MIN_FREE_SPACE_SIZE);
        if (new_free_space > new_minimum_free_space) {
            size_t new_memory_size = context_memory_size(c);
            if (UNLIKELY(memory_gc(c, (new_memory_size - new_free_space) + new_minimum_free_space) != MEMORY_GC_OK)) {
                TRACE("Unable to allocate memory for GC shrink.  new_memory_size=%zu new_free_space=%zu new_minimum_free_space=%zu size=%u\n", new_memory_size, new_free_space, new_minimum_free_space, size);
                return MEMORY_GC_ERROR_FAILED_ALLOCATION;
            }
        }
    }

    return MEMORY_GC_OK;
}

enum MemoryGCResult memory_gc_and_shrink(Context *c)
{
    if (context_avail_free_memory(c) >= MIN_FREE_SPACE_SIZE * 2) {
        if (UNLIKELY(memory_gc(c, context_memory_size(c) - context_avail_free_memory(c) / 2) != MEMORY_GC_OK)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        }
    }

    return MEMORY_GC_OK;
}

static inline void push_to_stack(term **stack, term value)
{
    *stack = (*stack) - 1;
    **stack = value;
}

enum MemoryGCResult memory_gc(Context *ctx, int new_size)
{
    TRACE("Going to perform gc on process %i\n", ctx->process_id);

    new_size += ctx->heap_fragments_size;
    ctx->heap_fragments_size = 0;

    if (UNLIKELY(ctx->has_max_heap_size && (new_size > ctx->max_heap_size))) {
        return MEMORY_GC_DENIED_ALLOCATION;
    }

    term *new_heap = calloc(new_size, sizeof(term));
    if (IS_NULL_PTR(new_heap)) {
        return MEMORY_GC_ERROR_FAILED_ALLOCATION;
    }
    TRACE("- Allocated %i words for new heap at address 0x%x\n", new_size, (int) new_heap);
    term *new_stack = new_heap + new_size;

    term *heap_ptr = new_heap;
    term *stack_ptr = new_stack;

    TRACE("- Running copy GC on registers\n");
    for (int i = 0; i < ctx->avail_registers; i++) {
        term new_root = memory_shallow_copy_term(ctx->x[i], &heap_ptr, 1);
        ctx->x[i] = new_root;
    }

    term *stack = ctx->e;
    int stack_size = ctx->stack_base - ctx->e;
    TRACE("- Running copy GC on stack (stack size: %i)\n", stack_size);
    for (int i = stack_size - 1; i >= 0; i--) {
        term new_root = memory_shallow_copy_term(stack[i], &heap_ptr, 1);
        push_to_stack(&stack_ptr, new_root);
    }

    struct ListHead *item;
    TRACE("- Running copy GC on process dictionary\n");
    LIST_FOR_EACH (item, &ctx->dictionary) {
        struct DictEntry *entry = GET_LIST_ENTRY(item, struct DictEntry, head);
        entry->key = memory_shallow_copy_term(entry->key, &heap_ptr, 1);
        entry->value = memory_shallow_copy_term(entry->value, &heap_ptr, 1);
    }

    TRACE("- Running copy GC on exit reason\n");
    ctx->exit_reason = memory_shallow_copy_term(ctx->exit_reason, &heap_ptr, 1);

    term *temp_start = new_heap;
    term *temp_end = heap_ptr;
    term new_mso_list = term_nil();
    do {
        term *next_end = temp_end;
        TRACE("- Running scan and copy GC from 0x%lx to 0x%x\n", (int) temp_start, (int) temp_end);
        memory_scan_and_copy(temp_start, temp_end, &next_end, &new_mso_list, 1);
        temp_start = temp_end;
        temp_end = next_end;
    } while (temp_start != temp_end);

    heap_ptr = temp_end;
    memory_sweep_mso_list(ctx->mso_list);
    ctx->mso_list = new_mso_list;

    free(ctx->heap_start);

    struct ListHead *fragment;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (fragment, tmp, &ctx->heap_fragments) {
        free(fragment);
    }
    list_init(&ctx->heap_fragments);

    ctx->heap_start = new_heap;
    ctx->stack_base = ctx->heap_start + new_size;
    ctx->heap_ptr = heap_ptr;
    ctx->e = stack_ptr;

    return MEMORY_GC_OK;
}

static inline int memory_is_moved_marker(term *t)
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

term memory_copy_term_tree(term **new_heap, term t, term *mso_list)
{
    TRACE("Copy term tree: 0x%lx, heap: 0x%p\n", t, *new_heap);

    term *temp_start = *new_heap;
    term copied_term = memory_shallow_copy_term(t, new_heap, 0);
    term *temp_end = *new_heap;

    do {
        term *next_end = temp_end;
        memory_scan_and_copy(temp_start, temp_end, &next_end, mso_list, 0);
        temp_start = temp_end;
        temp_end = next_end;
    } while (temp_start != temp_end);

    *new_heap = temp_end;

    return copied_term;
}

unsigned long memory_estimate_usage(term t)
{
    unsigned long acc = 0;

    struct TempStack temp_stack;
    temp_stack_init(&temp_stack);

    temp_stack_push(&temp_stack, t);

    while (!temp_stack_is_empty(&temp_stack)) {
        if (term_is_atom(t)) {
            t = temp_stack_pop(&temp_stack);

        } else if (term_is_integer(t)) {
            t = temp_stack_pop(&temp_stack);

        } else if (term_is_nil(t)) {
            t = temp_stack_pop(&temp_stack);

        } else if (term_is_pid(t)) {
            t = temp_stack_pop(&temp_stack);

        } else if (term_is_nonempty_list(t)) {
            acc += 2;
            temp_stack_push(&temp_stack, term_get_list_tail(t));
            t = term_get_list_head(t);

        } else if (term_is_tuple(t)) {
            int tuple_size = term_get_tuple_arity(t);
            acc += tuple_size + 1;

            if (tuple_size > 0) {
                for (int i = 1; i < tuple_size; i++) {
                    temp_stack_push(&temp_stack, term_get_tuple_element(t, i));
                }
                t = term_get_tuple_element(t, 0);

            } else {
                t = term_nil();
            }

        } else if (term_is_map(t)) {
            int map_size = term_get_map_size(t);
            acc += term_map_size_in_terms(map_size);
            if (map_size > 0) {
                for (int i = 1; i < map_size; i++) {
                    temp_stack_push(&temp_stack, term_get_map_key(t, i));
                    temp_stack_push(&temp_stack, term_get_map_value(t, i));
                }
                temp_stack_push(&temp_stack, term_get_map_value(t, 0));
                t = term_get_map_key(t, 0);

            } else {
                t = term_nil();
            }

        } else if (term_is_boxed(t)) {
            acc += term_boxed_size(t) + 1;
            if (term_is_sub_binary(t)) {
                t = term_get_sub_binary_ref(t);
            } else {
                t = temp_stack_pop(&temp_stack);
            }

        } else {
            fprintf(stderr, "bug: found unknown term type: 0x%lx\n", t);
            if (term_is_boxed(t)) {
                const term *boxed_value = term_to_const_term_ptr(t);
                int boxed_size = term_boxed_size(t) + 1;
                fprintf(stderr, "boxed header: 0x%lx, size: %i\n", boxed_value[0], boxed_size);
            }
            abort();
        }
    }

    temp_stack_destory(&temp_stack);

    return acc;
}

static void memory_scan_and_copy(term *mem_start, const term *mem_end, term **new_heap_pos, term *mso_list, int move)
{
    term *ptr = mem_start;
    term *new_heap = *new_heap_pos;

    while (ptr < mem_end) {
        term t = *ptr;

        if (term_is_atom(t)) {
            TRACE("Found atom (%lx)\n", t);
            ptr++;

        } else if (term_is_integer(t)) {
            TRACE("Found integer (%lx)\n", t);
            ptr++;

        } else if (term_is_nil(t)) {
            TRACE("Found NIL (%lx)\n", t);
            ptr++;

        } else if (term_is_pid(t)) {
            TRACE("Found PID (%lx)\n", t);
            ptr++;

        } else if ((t & 0x3) == 0x0) {
            TRACE("Found boxed header (%lx)\n", t);

            switch (t & TERM_BOXED_TAG_MASK) {
                case TERM_BOXED_TUPLE: {
                    int arity = term_get_size_from_boxed_header(t);
                    TRACE("- Boxed is tuple (%lx), arity: %i\n", t, arity);

                    for (int i = 1; i <= arity; i++) {
                        TRACE("-- Elem: %lx\n", ptr[i]);
                        ptr[i] = memory_shallow_copy_term(ptr[i], &new_heap, move);
                    }
                    break;
                }

                case TERM_BOXED_BIN_MATCH_STATE: {
                    TRACE("- Found bin match state.\n");
                    ptr[1] = memory_shallow_copy_term(ptr[1], &new_heap, move);
                    break;
                }

                case TERM_BOXED_POSITIVE_INTEGER:
                    TRACE("- Found boxed pos int.\n");
                    break;

                case TERM_BOXED_REF:
                    TRACE("- Found ref.\n");
                    break;

                case TERM_BOXED_FUN: {
                    int fun_size = term_get_size_from_boxed_header(t);
                    TRACE("- Found fun, size: %i.\n", fun_size);

                    // first term is the boxed header, followed by module and fun index.

                    for (int i = 3; i <= fun_size; i++) {
                        TRACE("-- Frozen: %lx\n", ptr[i]);
                        ptr[i] = memory_shallow_copy_term(ptr[i], &new_heap, move);
                    }
                    break;
                }

                case TERM_BOXED_FLOAT:
                    TRACE("- Found float.\n");
                    break;

                case TERM_BOXED_REFC_BINARY: {
                    TRACE("- Found refc binary.\n");
                    term ref = ((term) ptr) | TERM_BOXED_VALUE_TAG;
                    if (!term_refc_binary_is_const(ref)) {
                        *mso_list = term_list_init_prepend(ptr + REFC_BINARY_CONS_OFFET, ref, *mso_list);
                    }
                    break;
                }

                case TERM_BOXED_SUB_BINARY: {
                    TRACE("- Found sub binary.\n");
                    ptr[3] = memory_shallow_copy_term(ptr[3], &new_heap, move);
                    break;
                }

                case TERM_BOXED_HEAP_BINARY:
                    TRACE("- Found binary.\n");
                    break;

                case TERM_BOXED_MAP: {
                    TRACE("- Found map.\n");
                    size_t map_size = term_get_size_from_boxed_header(t) - 1;
                    size_t keys_offset = term_get_map_keys_offset();
                    size_t value_offset = term_get_map_value_offset();
                    TRACE("-- Map keys: %lx\n", ptr[keys_offset]);
                    ptr[keys_offset] = memory_shallow_copy_term(ptr[keys_offset], &new_heap, move);
                    for (size_t i = value_offset; i < value_offset + map_size;  ++i) {
                        TRACE("-- Map Value: %lx\n", ptr[i]);
                        ptr[i] = memory_shallow_copy_term(ptr[i], &new_heap, move);
                    }
                }
                    break;

                default:
                    fprintf(stderr, "- Found unknown boxed type: %lx\n", (t >> 2) & 0xF);
                    abort();
            }

            ptr += term_get_size_from_boxed_header(t) + 1;

        } else if (term_is_nonempty_list(t)) {
            TRACE("Found nonempty list (%lx)\n", t);
            *ptr = memory_shallow_copy_term(t, &new_heap, move);
            ptr++;

        } else if (term_is_boxed(t)) {
            TRACE("Found boxed (%lx)\n", t);
            *ptr = memory_shallow_copy_term(t, &new_heap, move);
            ptr++;

        } else {
            fprintf(stderr, "bug: found unknown term type: 0x%lx\n", t);
            abort();
        }
    }

    *new_heap_pos = new_heap;
}

HOT_FUNC static term memory_shallow_copy_term(term t, term **new_heap, int move)
{
    if (term_is_atom(t)) {
        return t;

    } else if (term_is_integer(t)) {
        return t;

    } else if (term_is_nil(t)) {
        return t;

    } else if (term_is_pid(t)) {
        return t;

    } else if (term_is_cp(t)) {
        // CP is valid only on stack
        return t;

    } else if (term_is_catch_label(t)) {
        // catch label is valid only on stack
        return t;

    } else if (term_is_boxed(t)) {
        term *boxed_value = term_to_term_ptr(t);

        if (memory_is_moved_marker(boxed_value)) {
            return memory_dereference_moved_marker(boxed_value);
        }

        int boxed_size = term_boxed_size(t) + 1;

        // It must be an empty tuple, so we are not going to use moved markers.
        // Empty tuples memory is too small to store moved markers.
        // However it is also required to avoid boxed terms duplication.
        // So instead all empty tuples will reference the same boxed term.
        if (boxed_size == 1) {
            return ((term) &empty_tuple) | TERM_BOXED_VALUE_TAG;
        }

        term *dest = *new_heap;
        for (int i = 0; i < boxed_size; i++) {
            dest[i] = boxed_value[i];
        }
        *new_heap += boxed_size;

        term new_term = ((term) dest) | TERM_BOXED_VALUE_TAG;

        if (move) {
            memory_replace_with_moved_marker(boxed_value, new_term);
        } else if (term_is_refc_binary(t)) { // copy, not a move; increment refcount
            if (!term_refc_binary_is_const(t)) {
                refc_binary_increment_refcount((struct RefcBinary *) term_refc_binary_ptr(t));
            }
        }

        return new_term;

    } else if (term_is_nonempty_list(t)) {
        term *list_ptr = term_get_list_ptr(t);

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

    } else {
        fprintf(stderr, "Unexpected term. Term is: %lx\n", t);
        abort();
    }
}

void memory_sweep_mso_list(term mso_list)
{
    term l = mso_list;
    while (l != term_nil()) {
        term h = term_get_list_head(l);
        // the mso list only contains boxed values; each refc is unique
        TERM_DEBUG_ASSERT(term_is_boxed(h))
        term *boxed_value = term_to_term_ptr(h);
        if (memory_is_moved_marker(boxed_value)) {
            // it has been moved, so it is referenced
        } else if (term_is_refc_binary(h) && !term_refc_binary_is_const(h)) {
            // unreferenced binary; decrement reference count
            refc_binary_decrement_refcount((struct RefcBinary *) term_refc_binary_ptr(h));
        }
        l = term_get_list_tail(l);
    }
}
