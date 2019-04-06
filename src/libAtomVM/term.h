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

/**
 * @file term.h
 * @brief term manipulation functions
 *
 * @details This header implements term manipulation functions.
 */

#ifndef _TERM_H_
#define _TERM_H_

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "utils.h"

#include "term_typedef.h"

#define TERM_BOXED_VALUE_TAG 0x2
#define TERM_CATCH_TAG 0x1B

#define TERM_BOXED_TAG_MASK 0x3F
#define TERM_BOXED_TUPLE 0x0
#define TERM_BOXED_REF 0x10
#define TERM_BOXED_FUN 0x14
#define TERM_BOXED_HEAP_BINARY 0x24

/**
 * @brief Gets a pointer to a term stored on the heap
 *
 * @details Casts a term to a term * that points to a value stored on the heap. Be aware: terms are assumed to be immutable.
 * @param t the term that will be casted, it must be valid.
 * @return a pointer to a term.
 */
static inline term *term_to_term_ptr(term t)
{
    return (term *) (t & ~0x3UL);
}

/**
 * @brief Gets a const pointer to a term stored on the heap
 *
 * @details Casts a term to a const term * that points to a value stored on the heap.
 * @param t the term that will be casted, it must be valid.
 * @return a const pointer to a term.
 */
static inline const term *term_to_const_term_ptr(term t)
{
    return (const term *) (t & ~0x3UL);
}

/**
 * @brief Checks if a term is an atom
 *
 * @details Returns 1 if a term is an atom, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_atom(term t)
{
    /* atom: | atom index | 00 10 11 */
    return ((t & 0x3F) == 0xB);
}

/**
 * @brief Check if a term is an invalid term
 *
 * @details Returns 1 if a term is an invalid term, otherwise 0 is returned.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_invalid_term(term t)
{
    return (t == 0);
}

/**
 * @brief Checks if a term is nil
 *
 * @details Returns 1 if a term is nil, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_nil(term t)
{
    /* nil: 11 10 11 */
    return ((t & 0x3F) == 0x3B);
}

/**
 * @brief Checks if a term is a non empty list
 *
 * @details Returns 1 if a term is a non empty list (cons), otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_nonempty_list(term t)
{
    /* list: 01 */
    return ((t & 0x3) == 0x1);
}

/**
 * @brief Checks if a term is a list
 *
 * @details Returns 1 if a term is a list (cons) or an empty list (nil term), otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_list(term t)
{
    /* list: 01 */
    return term_is_nonempty_list(t) || term_is_nil(t);
}

/**
 * @brief Checks if a term is a boxed value
 *
 * @details Returns 1 if a term is a boxed value stored on the heap, such as a tuple, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_boxed(term t)
{
    /* boxed: 10 */
    return ((t & 0x3) == 0x2);
}

/**
 * @brief Checks if a term is a movable boxed value
 *
 * @details Returns 1 if a term is a boxed value that can be safely copied with memcpy.
 * @param t the term that will checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_movable_boxed(term t)
{
    /* boxed: 10 */
    if ((t & 0x3) == 0x2) {
        const term *boxed_value = term_to_const_term_ptr(t);
        switch (boxed_value[0] & 0x3F) {
            case 0x10:
                return 1;

            default:
                return 0;
        }
    } else {
        return 0;
    }
}

/**
 * @brief Returns size of a boxed term from its header
 *
 * @details Returns the size that is stored in boxed term header most significant bits.
 * @param t the boxed term header.
 * @return the size of the boxed term that follows the header. 0 is returned if the boxed term is just the header.
 */
static inline int term_get_size_from_boxed_header(term header)
{
    return header >> 6;
}

/**
 * @brief Returns size of a boxed term
 *
 * @details Returns the size of a boxed term in term units.
 * @param t the boxed term.
 * @return size of given term.
 *
 */
static inline int term_boxed_size(term t)
{
    /* boxed: 10 */
    if ((t & 0x3) == 0x2) {
        const term *boxed_value = term_to_const_term_ptr(t);
        return term_get_size_from_boxed_header(*boxed_value);

    } else {
        //TODO: error here
        abort();
    }
}

/**
 * @brief Checks if a term is a binary
 *
 * @details Returns 1 if a term is a binary stored on the heap, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_binary(term t)
{
    /* boxed: 10 */
    if ((t & 0x3) == 0x2) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & 0x3F) == 0x24) {
            return 1;
        }
    }

    return 0;
}

/**
 * @brief Checks if a term is an integer value
 *
 * @details Returns 1 if a term is an integer value, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_integer(term t)
{
    /* integer: 11 11 */
    return ((t & 0xF) == 0xF);
}

static inline int term_is_catch_label(term t)
{
    return (t & 0x3F) == TERM_CATCH_TAG;
}

/**
 * @brief Checks if a term is a pid
 *
 * @details Returns 1 if a term is a process id, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_pid(term t)
{
    /* integer: 00 11 */
    return ((t & 0xF) == 0x3);
}

/**
 * @brief Checks if a term is a tuple
 *
 * @details Returns 1 if a term is a tuple, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_tuple(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & 0x3F) == 0) {
            return 1;
        }
    }

    return 0;
}

/**
 * @brief Checks if a term is a reference
 *
 * @details Returns 1 if a term is a reference, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_reference(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & 0x3F) == TERM_BOXED_REF) {
            return 1;
        }
    }

    return 0;
}

/**
 * @brief Checks if a term is a function
 *
 * @details Returns 1 if a term is a fun, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_function(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & 0x3F) == TERM_BOXED_FUN) {
            return 1;
        }
    }

    return 0;
}

/**
 * @brief Checks if a term is a saved CP
 *
 * @details Returns 1 if a term is a saved continuation pointer, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_cp(term t)
{
    return ((t & 0x3) == 0);
}

/**
 * @brief Gets invalid term
 *
 * @details Returns always an invalid term.
 * @return invalid term.
 */
static inline int term_invalid_term()
{
    return 0;
}

/**
 * @brief Gets nil value
 *
 * @details Returns always the nil value.
 * @return nil value term.
 */
static inline term term_nil()
{
    return 0x3B;
}

/**
 * @brief Gets global atom table index
 *
 * @details Returns atom table index for given atom term.
 * @param t the term that will be converted to atom table index. t must be a valid atom term.
 * @return a global atom table index.
 */
static inline int term_to_atom_index(term t)
{
    return t >> 6;
}

/**
 * @brief Term from global atom table index
 *
 * @details Returns a term from the given global atom table index.
 * @param atom_index global atoms table index.
 * @return a term that encapsulates the atom.
 */
static inline term term_from_atom_index(int atom_index)
{
    return (atom_index << 6) | 0xB;
}

/**
 * @brief Term to int32
 *
 * @details Returns an int32 for a given term. No overflow check is executed.
 * @param t the term that will be converted to int32, term type is checked.
 * @return a int32 value.
 */
static inline int32_t term_to_int32(term t)
{
    switch (t & 0xF) {
        case 0xF:
            return ((int32_t) t) >> 4;

        default:
            printf("term is not an integer: %lx\n", t);
            return 0;
    }
}

static inline int term_to_catch_label_and_module(term t, int *module_index)
{
    *module_index = t >> 24;
    return (t >> 6) & 0xFFFFF;
}

/**
 * @brief Gets process table index
 *
 * @details Returns local process table index for given atom term.
 * @param t the term that will be converted to local process table index, term type is checked.
 * @return a local process table index.
 */
static inline int32_t term_to_local_process_id(term t)
{
    switch (t & 0xF) {
        case 0x3:
            return t >> 4;

        default:
            printf("term is not a pid: %lx\n", t);
            return 0;
    }
}

/**
 * @brief Term from int4
 *
 * @details Returns a term for a given 4 bits integer value.
 * @param value the value that will be converted to a term.
 * @return a term that encapsulates the integer value.
 */
static inline term term_from_int4(int8_t value)
{
    return (value << 4) | 0xF;
}

/**
 * @brief Term from int11
 *
 * @details Returns a term for a given 11 bits integer value.
 * @param value the value that will be converted to a term.
 * @return a term that encapsulates the integer value.
 */
static inline term term_from_int11(int16_t value)
{
    return (value << 4) | 0xF;
}

/**
 * @brief Term from int32
 *
 * @details Returns a term for a given 32 bits integer value.
 * @param value the value that will be converted to a term.
 * @return a term that encapsulates the integer value.
 */
static inline term term_from_int32(int32_t value)
{
#if TERM_BITS == 32
    // 268435455 == 0x0FFFFFFF
    if (UNLIKELY((value > 268435455) || (value < -268435455))) {
        //TODO: unimplemented on heap integer value
        fprintf(stderr, "term_from_int32: unimplemented: term should be moved to heap.");
        abort();

    } else {
        return (value << 4) | 0xF;
    }

#elif TERM_BITS == 64
    return (value << 4) | 0xF;

#else
    #error "Wrong TERM_BITS define"
#endif
}

static inline term term_from_int64(int64_t value)
{
#if TERM_BITS == 32
    // 268435455 == 0x0FFFFFFF
    if (UNLIKELY((value > 268435455) || (value < -268435455))) {
        //TODO: unimplemented on heap integer value
        fprintf(stderr, "term_from_int64: unimplemented: term should be moved to heap.");
        abort();

    } else {
        return (value << 4) | 0xF;
    }

#elif TERM_BITS == 64
    // 1152921504606846975 = 0x0FFFFFFFFFFFFFFF
    if (UNLIKELY((value > 1152921504606846975) || (value < -1152921504606846975))) {
        //TODO: unimplemented on heap integer value
        fprintf(stderr, "unimplemented: term should be moved to heap.");
        abort();

    } else {
        return (value << 4) | 0xF;
    }

#else
    #error "Wrong TERM_BITS define"
#endif
}

static inline term term_from_catch_label(unsigned int module_index, unsigned int label)
{
    return (term) ((module_index << 24) | (label << 6) | TERM_CATCH_TAG);
}

/**
 * @brief Term from local process id
 *
 * @details Returns a term for a given local process table index.
 * @param local_process_id the local process table index that will be converted to a term.
 * @return a term that encapsulates a PID.
 */
static inline term term_from_local_process_id(uint32_t local_process_id)
{
    return (local_process_id << 4) | 0x3;
}

/**
 * @brief The count of terms needed to store the given amount of bytes
 *
 * @details Returns the count of terms needed to store the given size in bytes.
 * @param size the size in bytes
 * @return the count of terms
 */
static inline int term_binary_data_size_in_terms(uint32_t size)
{
#if TERM_BYTES == 4
    return ((size + 4 - 1) >> 2) + 1;
#elif TERM_BYTES == 8
    return ((size + 8 - 1) >> 3) + 1;
#else
    #error
#endif
}

/**
 * @brief Term from binary data
 *
 * @details Allocates a binary on the heap, and returns a term pointing to it.
 * @param data binary data.
 * @param size size of binary data buffer.
 * @param ctx the context that owns the memory that will be allocated.
 * @return a term pointing to the boxed binary pointer.
 */
static inline term term_from_literal_binary(void *data, uint32_t size, Context *ctx)
{
    int size_in_terms = term_binary_data_size_in_terms(size);

    term *boxed_value = memory_heap_alloc(ctx, size_in_terms + 1);
    boxed_value[0] = (size_in_terms << 6) | 0x24; // heap binary
    boxed_value[1] = size;

    memcpy(boxed_value + 2, data, size);

    return ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
}

/**
 * @brief Gets binary size
 *
 * @details Returns binary size for a given binary term.
 * @param t a term pointing to binary data. Fails if t is not a binary term.
 * @return binary size in bytes.
 */
static inline term term_binary_size(term t)
{
    const term *boxed_value = term_to_const_term_ptr(t);
    if (boxed_value[0] & 0x3F) {
        return boxed_value[1];
    } else {
        abort();
    }
}

/**
 * @brief Gets binary data
 *
 * @details Returns a pointer to stored binary data.
 * @param t a term pointing to binary data. Fails if t is not a binary term.
 * @return a const char * pointing to binary internal data.
 */
static inline const char *term_binary_data(term t)
{
    const term *boxed_value = term_to_const_term_ptr(t);
    if (boxed_value[0] & 0x3F) {
        return (const char *) (boxed_value + 2);
    } else {
        abort();
    }
}

/**
 * @brief Get a ref term from ref ticks
 *
 * @param ref_ticks an unique uint64 value that will be used to create ref term.
 * @param ctx the context that owns the memory that will be allocated.
 * @return a ref term created using given ref ticks.
 */
static inline term term_from_ref_ticks(uint64_t ref_ticks, Context *ctx)
{
    int ref_size = (sizeof(uint64_t) / sizeof(term));

    term *boxed_value = memory_heap_alloc(ctx, ref_size + 1);
    boxed_value[0] = (ref_size << 6) | TERM_BOXED_REF;

    if (ref_size == 1) {
        boxed_value[1] = (term) ref_ticks;

    } else if (ref_size == 2) {
        boxed_value[1] = (ref_ticks >> 4);
        boxed_value[2] = (ref_ticks & 0xFFFFFFFF);

    } else {
        abort();
    }

    return ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
}

static inline uint64_t term_to_ref_ticks(term rt)
{
    const term *boxed_value = term_to_const_term_ptr(rt);
    if ((boxed_value[0] & 0x3F) == TERM_BOXED_REF) {
        int ref_size = (sizeof(uint64_t) / sizeof(term));

        if (ref_size == 1) {
            return boxed_value[1];

        } else if (ref_size == 2) {
            return (boxed_value[1] << 4) | boxed_value[2];

        } else {
            abort();
        }

    } else {
        abort();
    }
}

/**
 * @brief Allocates a tuple on the heap
 *
 * @details Allocates an unitialized tuple on the heap with given arity.
 * @param size tuple arity (count of tuple elements).
 * @param ctx the context that owns the memory that will be allocated.
 * @return a term pointing on an empty tuple allocated on the heap.
 */
static inline term term_alloc_tuple(uint32_t size, Context *ctx)
{
    //TODO: write a real implementation
    //align constraints here
    term *boxed_value = memory_heap_alloc(ctx, 1 + size);
    boxed_value[0] = (size << 6); //tuple

    return ((term) boxed_value) | 0x2;
}

/**
 * @brief Replaces the content of a tuple element.
 *
 * @details Destructively replaces the nth element of an existing tuple, it should be used only on newly allocated tuples.
 * @param t the term pointing to the target tuple, fails if not a tuple.
 * @param elem_index the index of the element that will be replaced.
 * @param put_value the term that will be put on the nth tuple element.
 */
static inline void term_put_tuple_element(term t, uint32_t elem_index, term put_value)
{
    if (UNLIKELY(!term_is_boxed(t))) {
        abort();
    }

    term *boxed_value = term_to_term_ptr(t);
    if ( ((boxed_value[0] & 0x3F) == 0) && (elem_index < (boxed_value[0] >> 6)) )  {
        boxed_value[elem_index + 1] = put_value;
    } else {
        abort();
    }
}

/**
 * @brief Returns the nth tuple element
 *
 * @details Returns the nth element for a given tuple pointed by a term.
 * @param t a term that points to a tuple, fails otherwise.
 * @param elem_index index of the nth element that will be returned.
 * @return nth tuple term.
 */
static inline term term_get_tuple_element(term t, int elem_index)
{
    if (UNLIKELY(!term_is_boxed(t))) {
        abort();
    }

    const term *boxed_value = term_to_const_term_ptr(t);
    if ((boxed_value[0] & 0x3F) == 0) {
        return boxed_value[elem_index + 1];
    } else {
        abort();
    }
}

/*
 * @brief Returns count of tuple elements
 *
 * @details Returns tuple arity for the given term pointing to a tuple, fails otherwise.
 * @param t a term pointing to a tuple.
 * @return integer count of tuple elements (tuple arity).
 */
static inline int term_get_tuple_arity(term t)
{
    if (UNLIKELY(!term_is_boxed(t))) {
        abort();
    }

    const term *boxed_value = term_to_const_term_ptr(t);
    if ((boxed_value[0] & 0x3F) == 0) {
        return term_get_size_from_boxed_header(boxed_value[0]);
    } else {
        abort();
    }
}

/**
 * @brief Allocates a new list using string data
 *
 * @details Returns a term that points to a list (cons) that will be created using a string.
 * @param data a pointer to a string, it doesn't need to be NULL terminated.
 * @param size of the string/list that will be read and allocated.
 * @param ctx the context that owns the memory that will be allocated.
 * @return a term pointing to a list.
 */
static inline term term_from_string(const uint8_t *data, uint16_t size, Context *ctx)
{
    //TODO: write a real implementation
    //align constraints here
    term *list_cells = memory_heap_alloc(ctx, size * 2);
    for (int i = 0; i < size * 2; i += 2) {
        list_cells[i] = (term) &list_cells[i + 2] | 0x1;
        list_cells[i + 1] = term_from_int11(data[i / 2]);
    }
    list_cells[size * 2 - 2] = 0x3B;

    return ((term) list_cells) | 0x1;
}

/**
 * @brief Gets a term * pointing to a list
 *
 * @details Returns a term * pointer to a list (cons) from a given term.
 * @param t a term that points to a valid cons.
 * @return a term * pointing to the head of the first cell of a list.
 */
static inline term *term_get_list_ptr(term t)
{
    return (term *) (t & ~0x1);
}

/**
 * @brief Gets list term from pointer
 *
 * @details Return given list term from a list element pointer.
 * @param list_element a pointer to a list elment.
 * @return a list term
 */
static inline term term_list_from_list_ptr(term *list_elem)
{
    return ((term) list_elem) | 0x1;
}

/**
 * @brief Gets list head
 *
 * @details Returns given list head term
 * @param t a term pointing to a valid list (cons)
 * @return list head term
 */
static inline term term_get_list_head(term t)
{
    term *list_ptr = term_get_list_ptr(t);
    return list_ptr[1];
}

/**
 * @brief Gets list item tail
 *
 * @details Returns the tail, which is either a pointer to the next list item or nil, of the given list (that is not list tail).
 * @return list item tail term.
 */
static inline term term_get_list_tail(term t)
{
    term *list_ptr = term_get_list_ptr(t);
    return *list_ptr;
}

/**
 * @brief Allocate unitialized memory for a list item
 *
 * @details Allocates a memory area that will be used to store a list item.
 * @param ctx the context that owns the memory that will be allocated.
 * @return a pointer to a newly allocated memory area.
 */
MALLOC_LIKE static inline term *term_list_alloc(Context *ctx)
{
    return memory_heap_alloc(ctx, 2);
}

/**
 * @brief Prepends a term to an existing list
 *
 * @details Initializes a list item, set head to the given term and points tail to the given next item (that might be nil).
 * @param head term, the encapsulated list item value.
 * @param tail either nil or next list item.
 * @param list_elem the memory area that will be initialized.
 * @return a term pointing to the newly initialized list item.
 */
static inline term term_list_init_prepend(term *list_elem, term head, term tail)
{
    list_elem[0] = tail;
    list_elem[1] = head;

    return ((term) list_elem) | 0x1;
}

/**
 * @brief Prepends a term to an existing list
 *
 * @details Allocates a new list item, set head to the given term and points tail to the given next item (that might be nil).
 * @param head term, the encapsulated list item value.
 * @param tail either nil or next list item.
 * @param ctx the context that owns the memory that will be allocated.
 * @return a term pointing to the newly created list item.
 */
static inline term term_list_prepend(term head, term tail, Context *ctx)
{
    term *l = term_list_alloc(ctx);
    return term_list_init_prepend(l, head, tail);
}

/**
 * @brief Returns list length
 *
 * @details Counts the number of list items
 * @return number of list items
 */
static inline int term_list_length(term t)
{
    int len = 0;

    while (!term_is_nil(t)) {
        len++;
        t = term_get_list_tail(t);
    }

    return len;
}

/**
 * @brief Returns 1 if given terms are exactly equal.
 *
 * @details Compares 2 given terms and returns 1 if they are the same.
 * @param a first term
 * @param b second term
 * @return 1 if they are the same, 0 otherwise.
 */
static inline int term_exactly_equals(term a, term b)
{
    if (a == b) {
        return 1;

    } else if (term_is_boxed(a) && term_is_boxed(b)) {
        const term *boxed_a = term_to_const_term_ptr(a);
        const term *boxed_b = term_to_const_term_ptr(b);

        int a_size = term_boxed_size(a);
        int b_size = term_boxed_size(b);

        if (a_size == b_size) {
            return memcmp(boxed_a, boxed_b, (a_size + 1) * sizeof(term)) == 0;
        } else {
            return 0;
        }
    } else {
        //TODO: we might have to perform a deep comparison
        return 0;
    }
}

/**
 * @brief Returns 1 if given terms are equal.
 *
 * @details Compares 2 given terms and returns 1 if they are the same or they have same numeric value.
 * @param a first term
 * @param b second term
 * @return 1 if they are the same, 0 otherwise.
 */
static inline int term_equals(term a, term b)
{
    if (a == b) {
        return 1;

    } else if (term_is_boxed(a) && term_is_boxed(b)) {
        const term *boxed_a = term_to_const_term_ptr(a);
        const term *boxed_b = term_to_const_term_ptr(b);

        int a_size = term_boxed_size(a);
        int b_size = term_boxed_size(b);

        if (a_size == b_size) {
            return memcmp(boxed_a, boxed_b, (a_size + 1) * sizeof(term)) == 0;
        } else {
            return 0;
        }
    } else {
        //TODO: we might have to perform a deep comparison
        return 0;
    }
}

/**
 * @brief Prints a term to stdout
 *
 * @details Print any given term to the standard output.
 * @param t the term that will be printed.
 * @param ctx the context.
 */
void term_display(FILE *fd, term t, const Context *ctx);

#endif
