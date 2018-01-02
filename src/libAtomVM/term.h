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

#ifndef _TERM_H_
#define _TERM_H_

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

typedef unsigned long term;

static inline term *term_to_term_ptr(term t)
{
    return (term *) (t & ~0x3UL);
}

static inline const term *term_to_const_term_ptr(term t)
{
    return (const term *) (t & ~0x3UL);
}

static inline int term_is_atom(term t)
{
    /* atom: | atom index | 00 10 11 */
    return ((t & 0x3F) == 0xB);
}

static inline int term_is_list(term t)
{
    /* list: 01 */
    return ((t & 0x3) == 0x1);
}

static inline int term_is_boxed(term t)
{
    /* boxed: 10 */
    return ((t & 0x3) == 0x2);
}

static inline int term_is_nil(term t)
{
    /* nil: 11 10 11 */
    return ((t & 0x3F) == 0x3B);
}

static inline int term_is_integer(term t)
{
    /* integer: 11 11 */
    return ((t & 0xF) == 0xF);
}

static inline int term_is_pid(term t)
{
    /* integer: 00 11 */
    return ((t & 0xF) == 0x3);
}

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

static inline int32_t term_nil()
{
    return 0x3B;
}

static inline int term_to_atom_index(term t)
{
    return t >> 6;
}

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

static inline term term_from_int4(int8_t value)
{
    return (value << 4) | 0xF;
}

static inline term term_from_int11(int16_t value)
{
    return (value << 4) | 0xF;
}

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

static inline term term_from_local_process_id(uint32_t local_process_id)
{
    return (local_process_id << 4) | 0x3;
}

static inline term term_from_literal_binary(void *data, uint32_t size)
{
    //TODO: write a real implementation
    //align constraints here
    term *boxed_value = calloc(2, sizeof(term));
    boxed_value[0] = (size << 6) | 0x20; //refcounted binary
    boxed_value[1] = (term) data;

    return ((term) boxed_value) | 0x2;
}

static inline term term_binary_size(term t)
{
    const term *boxed_value = term_to_const_term_ptr(t);
    if (boxed_value[0] & 0x3F) {
        return boxed_value[0] >> 6;
    } else {
        abort();
    }
}

static inline term term_alloc_tuple(uint32_t size)
{
    //TODO: write a real implementation
    //align constraints here
    term *boxed_value = calloc(1 + size, sizeof(term));
    boxed_value[0] = (size << 6); //tuple

    return ((term) boxed_value) | 0x2;
}

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

static inline term *term_get_list_ptr(term t)
{
    return (term *) (t & ~0x1);
}

static inline term term_get_list_head(term t)
{
    term *list_ptr = term_get_list_ptr(t);
    return list_ptr[1];
}

static inline term term_get_list_tail(term t)
{
    term *list_ptr = term_get_list_ptr(t);
    return *list_ptr;
}

static inline term term_list_prepend(term head, term tail)
{
    term *list_elem = calloc(2, sizeof(term));
    list_elem[0] = tail;
    list_elem[1] = head;

    return ((term) list_elem) | 0x1;
}

#endif
