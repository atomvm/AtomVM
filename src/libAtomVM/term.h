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

/**
 * A value of any data type, types bigger than a machine word will require some additional space on heap.
 */
typedef unsigned long term;

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
 * @brief Checks if a term is a list
 *
 * @details Returns 1 if a term is a list (cons), otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_list(term t)
{
    /* list: 01 */
    return ((t & 0x3) == 0x1);
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
 * @brief Gets nil value
 *
 * @details Returns always the nil value.
 * @return nil value term.
 */
static inline int32_t term_nil()
{
    return 0x3B;
}

/**
 * @brief Gets atom table index
 *
 * @details Returns atom table index for given atom term.
 * @param t the term that will be converted to atom table index. t must be a valid atom term.
 * @return an atom table index.
 */

static inline int term_to_atom_index(term t)
{
    return t >> 6;
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
            return t >> 4;

        default:
            printf("term is not an integer: %lx\n", t);
            return 0;
    }
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
    //134217728 is 2^27
    //TODO: we don't have that "small" value on 64 bits CPUs
    if ((value > 134217728) || (value < -134217728)) {
        //TODO: unimplemented on heap integer value
        printf("unimplemented: term should be moved to heap.");
        abort();

    } else {
        return (value << 4) | 0xF;
    }
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
 * @brief Term from binary data
 *
 * @details Allocates a binary on the heap, and returns a term pointing to it.
 * @param data binary data.
 * @param size size of binary data buffer.
 * @return a term pointing to the boxed binary pointer.
 */
static inline term term_from_literal_binary(void *data, uint32_t size)
{
    //TODO: write a real implementation
    //align constraints here
    term *boxed_value = calloc(2, sizeof(term));
    boxed_value[0] = (size << 6) | 0x20; //refcounted binary
    boxed_value[1] = (term) data;

    return ((term) boxed_value) | 0x2;
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
        return boxed_value[0] >> 6;
    } else {
        abort();
    }
}

/**
 * @brief Allocates a tuple on the heap
 *
 * @details Allocates an unitialized tuple on the heap with given arity.
 * @param size tuple arity (count of tuple elements).
 * @return a term pointing on an empty tuple allocated on the heap.
 */
static inline term term_alloc_tuple(uint32_t size)
{
    //TODO: write a real implementation
    //align constraints here
    term *boxed_value = calloc(1 + size, sizeof(term));
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
    if (!term_is_boxed(t)) {
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
    if (!term_is_boxed(t)) {
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
    if (!term_is_boxed(t)) {
        abort();
    }

    const term *boxed_value = term_to_const_term_ptr(t);
    if ((boxed_value[0] & 0x3F) == 0) {
        return boxed_value[0] >> 6;
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
 * @return a term pointing to a list.
 */
static inline term term_from_string(const uint8_t *data, uint16_t size)
{
    //TODO: write a real implementation
    //align constraints here
    term *list_cells = calloc(size * 2, sizeof(term));
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
 * @brief Prepends a term to an existing list
 *
 * @details Allocates a new list item, set head to the given term and points tail to the given next item (that might be nil).
 * @param head term, the encapsulated list item value.
 * @param tail either nil or next list item.
 * @return a term pointing to the newly created list item.
 */
static inline term term_list_prepend(term head, term tail)
{
    term *list_elem = calloc(2, sizeof(term));
    list_elem[0] = tail;
    list_elem[1] = head;

    return ((term) list_elem) | 0x1;
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
        term *t_ptr = term_get_list_ptr(t);
        t = *t_ptr;
    }

    return len;
}

#endif
