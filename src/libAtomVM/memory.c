/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include <stdlib.h>
#include <string.h>

#include "context.h"
#include "debug.h"
#include "memory.h"

//#define ENABLE_TRACE

#include "trace.h"

#define MIN_FREE_SPACE_SIZE 16

#define MAX(a, b) ((a) > (b) ? (a) : (b))

static void memory_scan_and_copy(term *mem_start, const term *mem_end, term **new_heap_pos, int move);
static term memory_shallow_copy_term(term t, term **new_heap, int move);

HOT_FUNC term *memory_heap_alloc(Context *c, uint32_t size)
{
    term *allocated = c->heap_ptr;
    if (UNLIKELY(c->heap_ptr + size > c->e)) {
        fprintf(stderr, "Cannot allocate unavailable memory.\n");
        abort();
    }
    c->heap_ptr += size;

    return allocated;
}

enum MemoryGCResult memory_ensure_free(Context *c, uint32_t size)
{
    size_t free_space = context_avail_free_memory(c);
    if (free_space < size + MIN_FREE_SPACE_SIZE) {
        size_t memory_size = context_memory_size(c);
        if (UNLIKELY(memory_gc(c, memory_size + size + MIN_FREE_SPACE_SIZE) != MEMORY_GC_OK)) {
            //TODO: handle this more gracefully
            TRACE("Unable to allocate memory for GC\n");
            return MEMORY_GC_ERROR_FAILED_ALLOCATION;
        }
        size_t new_free_space = context_avail_free_memory(c);
        size_t new_minimum_free_space = 2 * (size + MIN_FREE_SPACE_SIZE);
        if (new_free_space > new_minimum_free_space) {
            size_t new_memory_size = context_memory_size(c);
            if (UNLIKELY(memory_gc(c, (new_memory_size - new_free_space) + new_minimum_free_space) != MEMORY_GC_OK)) {
                TRACE("Unable to allocate memory for GC shrink\n");
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
    TRACE("Going to perform gc\n");

    if (UNLIKELY(ctx->has_max_heap_size && (new_size > ctx->max_heap_size))) {
        return MEMORY_GC_DENIED_ALLOCATION;
    }

    term *new_heap = calloc(new_size, sizeof(term));
    if (IS_NULL_PTR(new_heap)) {
        return MEMORY_GC_ERROR_FAILED_ALLOCATION;
    }
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

    term *temp_start = new_heap;
    term *temp_end = heap_ptr;
    do {
        term *next_end = temp_end;
        memory_scan_and_copy(temp_start, temp_end, &next_end, 1);
        temp_start = temp_end;
        temp_end = next_end;
    } while (temp_start != temp_end);

    heap_ptr = temp_end;

    free(ctx->heap_start);

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

term memory_copy_term_tree(term **new_heap, term t)
{
    TRACE("Copy term tree: 0x%lx, heap: 0x%p\n", t, *new_heap);

    term *temp_start = *new_heap;
    term copied_term = memory_shallow_copy_term(t, new_heap, 0);
    term *temp_end = *new_heap;

    do {
        term *next_end = temp_end;
        memory_scan_and_copy(temp_start, temp_end, &next_end, 0);
        temp_start = temp_end;
        temp_end = next_end;
    } while (temp_start != temp_end);

    *new_heap = temp_end;

    return copied_term;
}

struct TempStack
{
    term *stack_end;
    term *stack_pos;
    int size;
};

static inline void temp_stack_init(struct TempStack *temp_stack)
{
    temp_stack->size = 8;
    temp_stack->stack_end = ((term *) malloc(temp_stack->size * sizeof(term))) + temp_stack->size;
    temp_stack->stack_pos = temp_stack->stack_end;
}

static inline void temp_stack_destory(struct TempStack *temp_stack)
{
    free(temp_stack->stack_end - temp_stack->size);
}

static void temp_stack_grow(struct TempStack *temp_stack)
{
    int old_used_size = temp_stack->stack_end - temp_stack->stack_pos;
    int new_size = temp_stack->size * 2;
    term *new_stack_end = ((term *) malloc(new_size * sizeof(term))) + new_size;
    term *new_stack_pos = new_stack_end - old_used_size;
    memcpy(new_stack_pos, temp_stack->stack_pos, old_used_size * sizeof(term));

    free(temp_stack->stack_end - temp_stack->size);
    temp_stack->stack_end = new_stack_end;
    temp_stack->stack_pos = new_stack_pos;
    temp_stack->size = new_size;
}

static inline int temp_stack_is_empty(const struct TempStack *temp_stack)
{
    return temp_stack->stack_end == temp_stack->stack_pos;
}

static inline void temp_stack_push(struct TempStack *temp_stack, term value)
{
    if (temp_stack->stack_end - temp_stack->stack_pos == temp_stack->size - 1) {
        temp_stack_grow(temp_stack);
    }

    temp_stack->stack_pos--;
    *temp_stack->stack_pos = value;
}

static inline term temp_stack_pop(struct TempStack *temp_stack)
{
    term value = *temp_stack->stack_pos;
    temp_stack->stack_pos++;

    return value;
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

        } else if (term_is_boxed(t)) {
            acc += term_boxed_size(t) + 1;
            t = temp_stack_pop(&temp_stack);

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

static void memory_scan_and_copy(term *mem_start, const term *mem_end, term **new_heap_pos, int move)
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

                case TERM_BOXED_HEAP_BINARY:
                    TRACE("- Found binary.\n");
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
        term *dest = *new_heap;
        for (int i = 0; i < boxed_size; i++) {
            dest[i] = boxed_value[i];
        }
        *new_heap += boxed_size;

        term new_term = ((term) dest) | TERM_BOXED_VALUE_TAG;

        if (move) {
            memory_replace_with_moved_marker(boxed_value, new_term);
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
