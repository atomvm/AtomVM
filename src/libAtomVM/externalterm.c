/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
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

#include "externalterm.h"

#include "context.h"
#include "list.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "utils.h"

#define EXTERNAL_TERM_TAG 131
#define SMALL_INTEGER_EXT 97
#define INTEGER_EXT 98
#define ATOM_EXT 100
#define SMALL_TUPLE_EXT 104
#define NIL_EXT 106
#define STRING_EXT 107
#define LIST_EXT 108
#define BINARY_EXT 109

static term parse_external_terms(const uint8_t *external_term_buf, int *eterm_size, Context *ctx);
static int calculate_heap_usage(const uint8_t *external_term_buf, int *eterm_size, Context *ctx);

term externalterm_to_term(const void *external_term, Context *ctx)
{
    const uint8_t *external_term_buf = (const uint8_t *) external_term;

    if (UNLIKELY(external_term_buf[0] != EXTERNAL_TERM_TAG)) {
        fprintf(stderr, "External term format not supported\n");
        abort();
    }

    int eterm_size;
    int heap_usage = calculate_heap_usage(external_term_buf + 1, &eterm_size, ctx);

    struct ListHead *heap_fragment = malloc(heap_usage * sizeof(term) + sizeof(struct ListHead));
    if (IS_NULL_PTR(heap_fragment)) {
        return term_invalid_term();
    }
    list_append(&ctx->heap_fragments, heap_fragment);
    ctx->heap_fragments_size += heap_usage;
    term *external_term_heap = (term *) (heap_fragment + 1);

    // save the heap pointer and temporary switch to the newly created heap fragment
    // so all existing functions can be used on the heap fragment without any change.
    term *main_heap = ctx->heap_ptr;
    ctx->heap_ptr = external_term_heap;
    term result = parse_external_terms(external_term_buf + 1, &eterm_size, ctx);
    ctx->heap_ptr = main_heap;

    return result;
}

static term parse_external_terms(const uint8_t *external_term_buf, int *eterm_size, Context *ctx)
{
    switch (external_term_buf[0]) {
        case SMALL_INTEGER_EXT: {
            *eterm_size = 2;
            return term_from_int11(external_term_buf[1]);
        }

        case INTEGER_EXT: {
            int32_t value = READ_32_UNALIGNED(external_term_buf + 1);

            *eterm_size = 5;
            return term_from_int32(value);
        }

        case ATOM_EXT: {
            uint16_t atom_len = READ_16_UNALIGNED(external_term_buf + 1);

            int global_atom_id = globalcontext_insert_atom(ctx->global, (AtomString) (external_term_buf + 2));

            *eterm_size = 3 + atom_len;
            return term_from_atom_index(global_atom_id);
        }

        case SMALL_TUPLE_EXT: {
            uint8_t arity = external_term_buf[1];
            term tuple = term_alloc_tuple(arity, ctx);

            int buf_pos = 2;

            for (int i = 0; i < arity; i++) {
                int element_size;
                term put_value = parse_external_terms(external_term_buf + buf_pos, &element_size, ctx);
                term_put_tuple_element(tuple, i, put_value);

                buf_pos += element_size;
            }

            *eterm_size = buf_pos;
            return tuple;
        }

        case NIL_EXT: {
            *eterm_size = 1;
            return term_nil();
        }

        case STRING_EXT: {
            uint16_t string_size = READ_16_UNALIGNED(external_term_buf + 1);
            *eterm_size = 3 + string_size;
            return term_from_string((uint8_t *) external_term_buf + 3, string_size, ctx);
        }

        case LIST_EXT: {
            uint32_t list_len = READ_32_UNALIGNED(external_term_buf + 1);

            term list_begin = term_nil();
            term *prev_term = NULL;

            int buf_pos = 5;

            for (unsigned int i = 0; i < list_len; i++) {
                int item_size;
                term head = parse_external_terms(external_term_buf + buf_pos, &item_size, ctx);

                term *new_list_item = term_list_alloc(ctx);

                if (prev_term) {
                    prev_term[0] = term_list_from_list_ptr(new_list_item);
                } else {
                    list_begin = term_list_from_list_ptr(new_list_item);
                }

                prev_term = new_list_item;
                new_list_item[1] = head;

                buf_pos += item_size;
            }

            if (prev_term) {
                int tail_size;
                term tail = parse_external_terms(external_term_buf + buf_pos, &tail_size, ctx);
                if (tail != term_nil()) {
                    //TODO: add support for imporper lists
                    abort();
                }

                prev_term[0] = term_nil();
                buf_pos += tail_size;
            }

             *eterm_size = buf_pos;
             return list_begin;
        }

        case BINARY_EXT: {
            uint32_t binary_size = READ_32_UNALIGNED(external_term_buf + 1);
            *eterm_size = 5 + binary_size;
            return term_from_literal_binary((uint8_t *) external_term_buf + 5, binary_size, ctx);
        }

        default:
            fprintf(stderr, "Unknown term type: %i\n", (int) external_term_buf[0]);
            abort();
    }
}

static int calculate_heap_usage(const uint8_t *external_term_buf, int *eterm_size, Context *ctx)
{
    switch (external_term_buf[0]) {
        case SMALL_INTEGER_EXT: {
            *eterm_size = 2;
            return 0;
        }

        case INTEGER_EXT: {
            *eterm_size = 5;
            return 0;
        }

        case ATOM_EXT: {
            uint16_t atom_len = READ_16_UNALIGNED(external_term_buf + 1);
            *eterm_size = 3 + atom_len;
            return 0;
        }

        case SMALL_TUPLE_EXT: {
            uint8_t arity = external_term_buf[1];

            int heap_usage = 1;
            int buf_pos = 2;

            for (int i = 0; i < arity; i++) {
                int element_size;
                heap_usage += calculate_heap_usage(external_term_buf + buf_pos, &element_size, ctx) + 1;

                buf_pos += element_size;
            }

            *eterm_size = buf_pos;
            return heap_usage;
        }

        case NIL_EXT: {
            *eterm_size = 1;
            return 0;
        }

        case STRING_EXT: {
            uint16_t string_size = READ_16_UNALIGNED(external_term_buf + 1);
            *eterm_size = 3 + string_size;
            return string_size * 2;
        }

        case LIST_EXT: {
            uint32_t list_len = READ_32_UNALIGNED(external_term_buf + 1);

            int buf_pos = 5;
            int heap_usage = 0;

            for (unsigned int i = 0; i < list_len; i++) {
                int item_size;
                heap_usage += calculate_heap_usage(external_term_buf + buf_pos, &item_size, ctx) + 2;

                buf_pos += item_size;
            }

            int tail_size;
            heap_usage += calculate_heap_usage(external_term_buf + buf_pos, &tail_size, ctx);
            buf_pos += tail_size;

            *eterm_size = buf_pos;
            return heap_usage;
        }

        case BINARY_EXT: {
            uint32_t binary_size = READ_32_UNALIGNED(external_term_buf + 1);
            *eterm_size = 5 + binary_size;

            #if TERM_BYTES == 4
                int size_in_terms = ((binary_size + 4 - 1) >> 2);
            #elif TERM_BYTES == 8
                int size_in_terms = ((binary_size + 8 - 1) >> 3);
            #else
                #error
            #endif

            return 2 + size_in_terms;
        }

        default:
            fprintf(stderr, "Unknown term type: %i\n", (int) external_term_buf[0]);
            abort();
    }
}
