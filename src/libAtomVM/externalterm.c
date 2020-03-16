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

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "utils.h"

#define EXTERNAL_TERM_TAG 131
#define NEW_FLOAT_EXT 70
#define SMALL_INTEGER_EXT 97
#define INTEGER_EXT 98
#define ATOM_EXT 100
#define SMALL_TUPLE_EXT 104
#define NIL_EXT 106
#define STRING_EXT 107
#define LIST_EXT 108
#define BINARY_EXT 109
#define EXPORT_EXT 113

static term parse_external_terms(const uint8_t *external_term_buf, int *eterm_size, Context *ctx, int copy);
static int calculate_heap_usage(const uint8_t *external_term_buf, int *eterm_size, bool copy, Context *ctx);
static size_t compute_external_size(Context *ctx, term t);
static int externalterm_from_term(Context *ctx, uint8_t **buf, size_t *len, term t);
static int serialize_term(Context *ctx, uint8_t *buf, term t);

/**
 * @brief
 * @param   external_term   buffer containing external term
 * @param   ctx             current context in which terms may be stored
 * @param   use_heap_fragment whether to store parsed terms in a heap fragement.  If 0, terms
 *                          are stored in the context heap.
 * @param   bytes_read      the number of bytes read off external_term in order to yeild a term
 * @return  the parsed term
 */
static term externalterm_to_term_internal(const void *external_term, Context *ctx, int use_heap_fragment, size_t *bytes_read)
{
    const uint8_t *external_term_buf = (const uint8_t *) external_term;

    if (UNLIKELY(external_term_buf[0] != EXTERNAL_TERM_TAG)) {
        fprintf(stderr, "External term format not supported\n");
        abort();
    }

    int eterm_size;
    int heap_usage = calculate_heap_usage(external_term_buf + 1, &eterm_size, false, ctx);

    if (use_heap_fragment) {
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
        term result = parse_external_terms(external_term_buf + 1, &eterm_size, ctx, 0);
        *bytes_read = eterm_size + 1;
        ctx->heap_ptr = main_heap;

        return result;
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, heap_usage) != MEMORY_GC_OK)) {
            fprintf(stderr, "Unable to ensure %i free words in heap\n", eterm_size);
            return term_invalid_term();
        }
        term result = parse_external_terms(external_term_buf + 1, &eterm_size, ctx, 1);
        *bytes_read = eterm_size + 1;
        return result;
    }
}

term externalterm_to_term(const void *external_term, Context *ctx, int use_heap_fragment)
{
    size_t bytes_read = 0;
    return externalterm_to_term_internal(external_term, ctx, use_heap_fragment, &bytes_read);
}

enum ExternalTermResult externalterm_from_binary(Context *ctx, term *dst, term binary, size_t *bytes_read, size_t num_extra_terms)
{
    if (!term_is_binary(binary)) {
        return EXTERNAL_TERM_BAD_ARG;
    }
    //
    // Copy the binary data to a buffer (in case of GC)
    //
    size_t len = term_binary_size(binary);
    const uint8_t *data = (const uint8_t *) term_binary_data(binary);
    uint8_t *buf = malloc(len);
    if (UNLIKELY(IS_NULL_PTR(buf))) {
        fprintf(stderr, "Unable to allocate %zu bytes for binary buffer.\n", len);
        return EXTERNAL_TERM_MALLOC;
    }
    memcpy(buf, data, len);
    //
    // convert
    //
    *dst = externalterm_to_term_internal(buf, ctx, 0, bytes_read);
    free(buf);
    return EXTERNAL_TERM_OK;
}

static int externalterm_from_term(Context *ctx, uint8_t **buf, size_t *len, term t)
{
    *len = compute_external_size(ctx, t) + 1;
    *buf = malloc(*len);
    if (UNLIKELY(IS_NULL_PTR(*buf))) {
        fprintf(stderr, "Unable to allocate %zu bytes for externalized term.\n", *len);
        abort();
    }
    size_t k = serialize_term(ctx, *buf + 1, t);
    *buf[0] = EXTERNAL_TERM_TAG;
    return k + 1;
}

term externalterm_to_binary(Context *ctx, term t)
{
    //
    // convert
    //
    uint8_t *buf;
    size_t len;
    externalterm_from_term(ctx, &buf, &len, t);
    //
    // Ensure enough free space in heap for binary
    //
    int size_in_terms = term_binary_data_size_in_terms(len);
    if (UNLIKELY(memory_ensure_free(ctx, size_in_terms + 1) != MEMORY_GC_OK)) {
        fprintf(stderr, "Unable to ensure %i free words in heap\n", size_in_terms);
        return term_invalid_term();
    }
    //
    // create and return the binary
    //
    term binary = term_from_literal_binary((void *)buf, len, ctx);
    free(buf);
    return binary;
}

static size_t compute_external_size(Context *ctx, term t)
{
    return serialize_term(ctx, NULL, t);
}

static int serialize_term(Context *ctx, uint8_t *buf, term t)
{
    if (term_is_uint8(t)) {
        if (!IS_NULL_PTR(buf)) {
            buf[0] = SMALL_INTEGER_EXT;
            buf[1] = term_to_uint8(t);
        }
        return 2;

    } else if (term_is_integer(t)) {
        if (!IS_NULL_PTR(buf)) {
            int32_t val = term_to_int32(t);
            buf[0] = INTEGER_EXT;
            WRITE_32_UNALIGNED(buf + 1, val);
        }
        return 5;

    } else if (term_is_atom(t)) {
        AtomString atom_string = globalcontext_atomstring_from_term(ctx->global, t);
        size_t atom_len = atom_string_len(atom_string);
        if (!IS_NULL_PTR(buf)) {
            buf[0] = ATOM_EXT;
            WRITE_16_UNALIGNED(buf + 1, atom_len);
            int8_t *atom_data = (int8_t *) atom_string_data(atom_string);
            for (size_t i = 3;  i < atom_len + 3;  ++i) {
                buf[i] = (int8_t) atom_data[i - 3];
            }
        }
        return 3 + atom_len;

    } else if (term_is_tuple(t)) {
        size_t arity = term_get_tuple_arity(t);
        if (arity > 255) {
            fprintf(stderr, "Tuple arity greater than 255: %zu\n", arity);
            abort();
        }
        if (!IS_NULL_PTR(buf)) {
            buf[0] = SMALL_TUPLE_EXT;
            buf[1] = (int8_t) arity;
        }
        size_t k = 2;
        for (size_t i = 0;  i < arity;  ++i) {
            term e = term_get_tuple_element(t, i);
            k += serialize_term(ctx, IS_NULL_PTR(buf) ? NULL : buf + k, e);
        }
        return k;

    } else if (term_is_nil(t)) {
        if (!IS_NULL_PTR(buf)) {
            buf[0] = NIL_EXT;
        }
        return 1;

    } else if (term_is_string(t)) {
        if (!IS_NULL_PTR(buf)) {
            buf[0] = STRING_EXT;
        }
        size_t len = 0;
        size_t k = 3;
        term i = t;
        while (!term_is_nil(i)) {
            term e = term_get_list_head(i);
            if (!IS_NULL_PTR(buf)) {
                *(buf + k) = term_to_uint8(e);
            }
            ++k;
            i = term_get_list_tail(i);
            ++len;
        }
        if (!IS_NULL_PTR(buf)) {
            WRITE_16_UNALIGNED(buf + 1, len);
        }
        return k;

    } else if (term_is_list(t)) {
        if (!IS_NULL_PTR(buf)) {
            buf[0] = LIST_EXT;
        }
        size_t len = 0;
        size_t k = 5;
        term i = t;
        while (term_is_nonempty_list(i)) {
            term e = term_get_list_head(i);
            k += serialize_term(ctx, IS_NULL_PTR(buf) ? NULL : buf + k, e);
            i = term_get_list_tail(i);
            ++len;
        }
        k += serialize_term(ctx, IS_NULL_PTR(buf) ? NULL : buf + k, i);
        if (!IS_NULL_PTR(buf)) {
            WRITE_32_UNALIGNED(buf + 1, len);
        }
        return k;

    } else if (term_is_binary(t)) {
        if (!IS_NULL_PTR(buf)) {
            buf[0] = BINARY_EXT;
        }
        size_t len = term_binary_size(t);
        if (!IS_NULL_PTR(buf)) {
            const uint8_t *data = (const uint8_t *) term_binary_data(t);
            WRITE_32_UNALIGNED(buf + 1, len);
            memcpy(buf + 5, data, len);
        }
        return 5 + len;

    } else {
        fprintf(stderr, "Unknown term type: %li\n", t);
        abort();
    }
}

static term parse_external_terms(const uint8_t *external_term_buf, int *eterm_size, Context *ctx, int copy)
{
    switch (external_term_buf[0]) {
        case NEW_FLOAT_EXT: {
            #ifndef AVM_NO_FP
                union {
                    uint64_t intvalue;
                    double doublevalue;
                } v;
                v.intvalue = READ_64_UNALIGNED(external_term_buf + 1);

                *eterm_size = 9;
                return term_from_float(v.doublevalue, ctx);
            #else
                fprintf(stderr, "floating point support not enabled.\n");
                abort();
            #endif
        }

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

            int global_atom_id = globalcontext_insert_atom_maybe_copy(ctx->global, (AtomString) (external_term_buf + 2), copy);

            *eterm_size = 3 + atom_len;
            return term_from_atom_index(global_atom_id);
        }

        case SMALL_TUPLE_EXT: {
            uint8_t arity = external_term_buf[1];
            term tuple = term_alloc_tuple(arity, ctx);

            int buf_pos = 2;

            for (int i = 0; i < arity; i++) {
                int element_size;
                term put_value = parse_external_terms(external_term_buf + buf_pos, &element_size, ctx, copy);
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
                term head = parse_external_terms(external_term_buf + buf_pos, &item_size, ctx, copy);

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
                term tail = parse_external_terms(external_term_buf + buf_pos, &tail_size, ctx, copy);
                prev_term[0] = tail;
                buf_pos += tail_size;
            }

             *eterm_size = buf_pos;
             return list_begin;
        }

        case BINARY_EXT: {
            uint32_t binary_size = READ_32_UNALIGNED(external_term_buf + 1);
            *eterm_size = 5 + binary_size;
            if (copy) {
                return term_from_literal_binary((uint8_t *) external_term_buf + 5, binary_size, ctx);
            } else {
                return term_from_const_binary((uint8_t *) external_term_buf + 5, binary_size, ctx);
            }
        }

        case EXPORT_EXT: {
            int heap_usage = 1;
            int buf_pos = 1;
            int element_size;

            term m = parse_external_terms(external_term_buf + buf_pos, &element_size, ctx, copy);
            buf_pos += element_size;

            term f = parse_external_terms(external_term_buf + buf_pos, &element_size, ctx, copy);
            buf_pos += element_size;

            term a = parse_external_terms(external_term_buf + buf_pos, &element_size, ctx, copy);
            buf_pos += element_size;

            *eterm_size = buf_pos;
            return term_make_function_reference(m, f, a, ctx);
        }

        default:
            fprintf(stderr, "Unknown term type: %i\n", (int) external_term_buf[0]);
            abort();
    }
}

static int calculate_heap_usage(const uint8_t *external_term_buf, int *eterm_size, bool copy, Context *ctx)
{
    switch (external_term_buf[0]) {
        case NEW_FLOAT_EXT: {
            #ifndef AVM_NO_FP
                *eterm_size = 9;
                return FLOAT_SIZE;
            #else
                fprintf(stderr, "floating point support not enabled.\n");
                abort();
            #endif
        }

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
                heap_usage += calculate_heap_usage(external_term_buf + buf_pos, &element_size, copy, ctx) + 1;

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
                heap_usage += calculate_heap_usage(external_term_buf + buf_pos, &item_size, copy, ctx) + 2;

                buf_pos += item_size;
            }

            int tail_size;
            heap_usage += calculate_heap_usage(external_term_buf + buf_pos, &tail_size, copy, ctx);
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

            if (copy) {
                return 2 + size_in_terms;
            } else {
                return TERM_BOXED_REFC_BINARY_SIZE;
            }
        }

        case EXPORT_EXT: {
            int heap_usage = 1;
            int buf_pos = 1;
            for (int i = 0; i < 3; i++) {
                int element_size;
                heap_usage += calculate_heap_usage(external_term_buf + buf_pos, &element_size, copy, ctx) + 1;
                buf_pos += element_size;
            }

            *eterm_size = buf_pos;
            return FUNCTION_REFERENCE_SIZE;
        }

        default:
            fprintf(stderr, "Unknown term type: %i\n", (int) external_term_buf[0]);
            abort();
    }
}
