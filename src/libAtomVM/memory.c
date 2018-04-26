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

#include "context.h"
#include "debug.h"
#include "memory.h"

//#define ENABLE_TRACE

#ifndef TRACE
    #ifdef ENABLE_TRACE
        #define TRACE printf
    #else
        #define TRACE(...)
    #endif
#endif

#define USED_BY_TRACE(x) \
    (void) (x)

term memory_copy_term_tree(term **new_heap, term **new_stack, term t, int move);

term *memory_heap_alloc(Context *c, uint32_t size)
{
    term *allocated = c->heap_ptr;
    if (c->heap_ptr + size >= c->e) {
        TRACE("GC is needed.\n");
        memory_gc(c, 1024);
    }
    c->heap_ptr += size;

    return allocated;
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

void memory_gc(Context *ctx, int new_size)
{
    TRACE("Going to perform gc\n");
    term *new_heap = calloc(new_size, sizeof(term));
    term *new_stack = new_heap + new_size;

    term *heap_ptr = new_heap;
    term *stack_ptr = new_stack;

    term *stack = ctx->e;
    int stack_size = (ctx->stack + DEFAULT_STACK_SIZE) - ctx->e;
    for (int i = 0; i < 16; i++) {
        term *old_stack_ptr = stack_ptr;
        term new_root = memory_copy_term_tree(&heap_ptr, &stack_ptr, ctx->x[i], 1);
        if (old_stack_ptr != stack_ptr) {
            abort();
        }
        ctx->x[i] = new_root;
    }
    for (int i = stack_size - 1; i >= 0; i--) {
        term *old_stack_ptr = stack_ptr;
        term new_root = memory_copy_term_tree(&heap_ptr, &stack_ptr, stack[i], 1);
        if (old_stack_ptr != stack_ptr) {
            abort();
        }
        push_to_stack(&stack_ptr, new_root);
    }

    free(ctx->stack);

    ctx->stack = new_heap;
    ctx->heap_ptr = heap_ptr;
    ctx->e = stack_ptr;
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
    return (!(term_is_list(t) || term_is_tuple(t)));
}


term memory_copy_term_tree(term **new_heap, term **new_stack, term t, int move)
{
    TRACE("Going to populate using %lx as root, with move = %i\n", t, move);

    if (t == 0) {
        return 0;
    }

    int going_back = 0;

    term *previous = NULL;
    term previous_term;

    do {
        if (!going_back) {
            if (term_is_tuple(t)) {
                int arity = term_get_tuple_arity(t);

                TRACE("- Found tuple (%lx) with %i arity.\n", t, arity);

                push_to_stack(new_stack, 0);

                set_placeholder((*new_heap), previous, t);

                term *tmp_previous = *new_heap;
                *new_heap += arity + 1;

                if (arity == 0) {
                    *new_heap += 1;

                    if (previous) {
                        previous = tmp_previous;
                        // Reached end of list, ascending back from leaf to root
                        going_back = 1;

                        continue;

                    } else {
                        TRACE("- Root is an empty tuple, just returning.\n");
                        pop_from_stack(new_stack);
                        return ((term) tmp_previous) | 0x2;
                    }

                } else {
                    previous = tmp_previous;
                }

                t = term_get_tuple_element(t, 0);

            } else if (term_is_nil(t)) {
                if (previous) {
                    TRACE("- Found end of list (NIL).\n");

                    previous_term = t;
                    // Reached end of list, ascending back from leaf to root
                    going_back = 1;
                    t = previous[1];

                } else {
                    TRACE("- Root is an empty list (NIL), just returning.\n");
                    return t;
                }

            } else if (term_is_list(t) && !is_moved_marker(*term_get_list_ptr(t))) {
                term tail = term_get_list_tail(t);
                term head = term_get_list_head(t);

                TRACE("- Found list (%lx) cons, previous was: %p, head: %lx, tail: %lx.\n", t, (const void *) previous, head, tail);

                if (!is_leaf_term(head) || (term_is_boxed(head) && is_moved_marker(*term_to_term_ptr(head))) ) {
                    TRACE("- List head (%lx) is not a leaf term.\n", head);
                    push_to_stack(new_stack, 0);
                }

                set_placeholder((*new_heap), previous, t);

                previous = *new_heap;
                *new_heap += 2;

                t = tail;

            } else if (term_is_list(t) && is_moved_marker(*term_get_list_ptr(t))) {
                TRACE("List moved marker found.\n");
                going_back = 1;
                previous_term = term_get_list_ptr(t)[1];
                if (previous) {
                    t = previous[1];
                } else {
                    TRACE("- Reached root moved list marker.\n");
                    return previous_term;
                }

            } else if (term_is_boxed(t) && is_moved_marker(*term_to_term_ptr(t))) {
                TRACE("Boxed moved marker found.\n");
                going_back = 1;
                previous_term = term_to_term_ptr(t)[1];
                if (previous) {
                    TRACE("- Reached moved term maker %lx\n, going back to %lx\n.", t, previous[1]);
                    t = previous[1];
                } else {
                    TRACE("- Reached root moved boxed marker.\n");
                    return previous_term;
                }

            } else if (term_is_binary(t)) {
                TRACE("- Found binary term (%lx).\n", t);

                term *tmp_previous = *new_heap;
                *new_heap += 2;

                //TODO: handle all types of binaries
                tmp_previous[0] = (term_binary_size(t) << 6) | 0x20; //refcounted binary
                tmp_previous[1] = (term) term_binary_data(t);

                if (previous) {
                    TRACE("- Found leaf binary, going back.\n");
                    t = get_placeholder_term(previous);
                    going_back = 1;

                    continue;

                } else {
                    TRACE("- Root is a binary, just returning.\n");
                    return ((term) tmp_previous) | 0x2;
                }

            } else {
                if (previous) {
                    TRACE("- Reached leaf value %lx, going back to root.\n", t);

                    previous_term = t;
                    t = get_placeholder_term(previous);
                    going_back = 1;

                    continue;
                } else {
                    TRACE("Found root leaf value, returning %lx\n", t);

                    return t;
                }
            }

        } else {
            if (term_is_tuple(t)) {
                term source_term = t;
                int tuple_elem = pop_from_stack(new_stack);
                tuple_elem++;

                if (term_get_tuple_arity(t) == 0) {
                    TRACE("- Empty tuple %lx found.\n", t);

                    term *dest_heap = previous;
                    previous = get_placeholder_previous(dest_heap);
                    if (previous) {
                        t = get_placeholder_term(previous);
                    }
                    dest_heap[0] = 0;
                    continue;
                }

                if (tuple_elem < term_get_tuple_arity(t)) {
                    TRACE("- Tuple %lx (arity = %i). Going to process elem %i subtree.\n", t, term_get_tuple_arity(t), tuple_elem);

                    push_to_stack(new_stack, previous_term);
                    push_to_stack(new_stack, tuple_elem);
                    t = term_get_tuple_element(t, tuple_elem);
                    going_back = 0;

                } else {
                    TRACE("- Done tuple %lx (arity = %i).\n", t, term_get_tuple_arity(t));

                    term *dest_heap = previous;

                    previous = get_placeholder_previous(dest_heap);
                    if (previous) {
                        t = get_placeholder_term(previous);
                    }

                    dest_heap[tuple_elem] = previous_term;
                    for (int i = tuple_elem - 1; i > 0; i--) {
                        dest_heap[i] = pop_from_stack(new_stack);
                    }
                    dest_heap[0] = tuple_elem << 6;

                    term new_term = ((term) dest_heap) | 0x2;

                    // leave a moved marker
                    if (move) {
                        term *source_mem = term_to_term_ptr(source_term);
                        replace_with_moved_marker(source_mem, new_term);
                    }

                    previous_term = new_term;
                }

            } else if (term_is_list(t)) {
                term *dest_heap = previous;
                term source_term = t;

                term head = term_get_list_head(source_term);
                term the_head;

                if (!is_leaf_term(head) || (term_is_boxed(head) && is_moved_marker(*term_to_term_ptr(head)))) {

                    int already_pushed_count = pop_from_stack(new_stack);

                    if (already_pushed_count == 0) {
                        TRACE("- List cons %lx head is not a leaf, going to visit tail first.\n", t);

                        push_to_stack(new_stack, previous_term);
                        push_to_stack(new_stack, 1);

                        t = head;
                        going_back = 0;

                        continue;

                    } else if (already_pushed_count == 1) {
                        TRACE("- List cons %lx head is not a leaf, going to visit head.\n", t);
                        the_head = previous_term;
                        previous_term = pop_from_stack(new_stack);
                    }

                } else {
                    the_head = head;
                }

                if (term_is_binary(the_head)) {
                    TRACE("- List head (%lx) is a binary.\n", the_head);
                    term new_head = ((term) (*new_heap)) | 0x2;
                    (*new_heap)[0] = (term_binary_size(the_head) << 6) | 0x20; //refcounted binary
                    (*new_heap)[1] = (term) term_binary_data(the_head);
                    *new_heap += 2;
                    the_head = new_head;
                }

                previous = get_placeholder_previous(dest_heap);
                if (previous) {
                    t = get_placeholder_term(previous);
                }

                TRACE("- Writing cons at address %p with head %lx and tail %lx.\n", (const void *) dest_heap, the_head, previous_term);
                dest_heap[0] = previous_term;
                dest_heap[1] = the_head;

                term new_term = ((term) dest_heap) | 0x1;

                // leave a moved marker
                if (move) {
                    term *source_mem = term_get_list_ptr(source_term);
                    replace_with_moved_marker(source_mem, new_term);
                }

                previous_term = new_term;

            } else {
                TRACE("Found a leaf term %lx while going back.\n", t);
                abort();
            }
        }
    } while (previous != NULL);

    return previous_term;
}
