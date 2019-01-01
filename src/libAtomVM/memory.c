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

#define MIN_FREE_SPACE_SIZE 8

#define MAX(a, b) ((a) > (b) ? (a) : (b))

static void memory_scan_and_copy(term *mem_start, const term *mem_end, term **new_heap_pos);
static term memory_shallow_copy_term(term t, term **new_heap, int move);

HOT_FUNC term *memory_heap_alloc(Context *c, uint32_t size)
{
    term *allocated = c->heap_ptr;
    if (c->heap_ptr + size > c->e) {
        TRACE("GC is needed.\n");
        memory_ensure_free(c, size);
        allocated = c->heap_ptr;
    }
    c->heap_ptr += size;

    return allocated;
}

void memory_ensure_free(Context *c, uint32_t size)
{
    if (context_memory_size(c) > size) {
        if (UNLIKELY(memory_gc(c, context_memory_size(c)) != MEMORY_GC_OK)) {
            //TODO: handle this more gracefully
            fprintf(stderr, "Unable to allocate memory for GC\n");
            abort();
        }
    }

    if (context_avail_free_memory(c) < size + MIN_FREE_SPACE_SIZE) {
        if (UNLIKELY(memory_gc(c, MAX(context_memory_size(c) * 2, context_memory_size(c) + size)) != MEMORY_GC_OK)) {
            //TODO: handle this more gracefully
            fprintf(stderr, "Unable to allocate memory for GC\n");
            abort();
        }
    }
}

void memory_gc_and_shrink(Context *c)
{
    if (context_avail_free_memory(c) >= MIN_FREE_SPACE_SIZE * 2) {
        if (UNLIKELY(memory_gc(c, context_memory_size(c) - context_avail_free_memory(c) / 2) != MEMORY_GC_OK)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        }
    }
}

static inline term peek_stack(term **stack)
{
    term value = **stack;
    return value;
}

static inline void push_to_stack(term **stack, term value)
{
    *stack = (*stack) - 1;
    **stack = value;
}

static inline term pop_from_stack(term **stack)
{
    term value = **stack;
    *stack = (*stack) + 1;

    return value;
}

enum MemoryGCResult memory_gc(Context *ctx, int new_size)
{
    TRACE("Going to perform gc\n");
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
        memory_scan_and_copy(temp_start, temp_end, &next_end);
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


static inline int is_moved_marker(term t)
{
    return t == 0x2C;
}

static inline void replace_with_moved_marker(term *to_be_replaced, term replace_with)
{
    to_be_replaced[0] = 0x2C;
    to_be_replaced[1] = replace_with;
}

static inline void set_placeholder(term *placeholder_mem, term *previous_term_mem, term current)
{
    placeholder_mem[0] = (term) previous_term_mem;
    placeholder_mem[1] = current;
}

static inline term *get_placeholder_previous(term *placeholder)
{
    return (term *) placeholder[0];
}

static inline term get_placeholder_term(term *placeholder)
{
    return placeholder[1];
}

static inline int is_leaf_term(term t)
{
    return (!(term_is_nonempty_list(t) || term_is_tuple(t)));
}


term memory_copy_term_tree(term **new_heap, term t)
{
    TRACE("Copy term tree: 0x%lx, heap: 0x%p\n", t, *new_heap);

    term *temp_start = *new_heap;
    term copied_term = memory_shallow_copy_term(t, new_heap, 0);
    term *temp_end = *new_heap;

    do {
        term *next_end = temp_end;
        memory_scan_and_copy(temp_start, temp_end, &next_end);
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

        } else if (term_is_reference(t)) {
            acc += term_boxed_size(t) + 1;
            t = temp_stack_pop(&temp_stack);

        } else if (term_is_binary(t)) {
            //TODO: binaries might be shared outside process heap.
            acc += term_boxed_size(t) + 1;
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

        } else {
            abort();
        }
    }

    temp_stack_destory(&temp_stack);

    return acc;
}

static void memory_scan_and_copy(term *mem_start, const term *mem_end, term **new_heap_pos)
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

            switch ((t >> 2) & 0xF) {
                case 0: {
                    int arity = t >> 6;
                    TRACE("- Boxed is tuple (%lx), arity: %i\n", t, arity);

                    for (int i = 1; i <= arity; i++) {
                        TRACE("-- Elem: %lx\n", ptr[i]);
                        ptr[i] = memory_shallow_copy_term(ptr[i], &new_heap, 1);
                    }
                    break;
                }

                case 4:
                    TRACE("- Found ref.\n");
                    break;

                case 9:
                    TRACE("- Found binary.\n");
                    break;

                default:
                    fprintf(stderr, "- Found unknown boxed type: %lx\n", (t >> 2) & 0xF);
                    abort();
            }

            ptr += (t >> 6) + 1;

        } else if (term_is_nonempty_list(t)) {
            TRACE("Found nonempty list (%lx)\n", t);
            *ptr = memory_shallow_copy_term(t, &new_heap, 1);
            ptr++;

        } else if (term_is_boxed(t)) {
            TRACE("Found boxed (%lx)\n", t);
            *ptr = memory_shallow_copy_term(t, &new_heap, 1);
            ptr++;

        } else {
            TRACE("Found unknown term type: %lx\n", t);
        }
    }

    *new_heap_pos = new_heap;
}

static term memory_shallow_copy_term(term t, term **new_heap, int move)
{
    UNUSED(move);

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
        int boxed_size = term_boxed_size(t) + 1;
        const term *boxed_value = term_to_const_term_ptr(t);
        term *dest = *new_heap;
        for (int i = 0; i < boxed_size; i++) {
            dest[i] = boxed_value[i];
        }
        *new_heap += boxed_size;
        return ((term) dest) | TERM_BOXED_VALUE_TAG;

    } else if (term_is_nonempty_list(t)) {
        term *dest = *new_heap;
        const term *list_ptr = term_get_list_ptr(t);
        dest[0] = list_ptr[0];
        dest[1] = list_ptr[1];
        *new_heap += 2;
        return ((term) dest) | 0x1;

    } else {
        fprintf(stderr, "Unexpected term. Term is: %lx\n", t);
        abort();
    }
}
