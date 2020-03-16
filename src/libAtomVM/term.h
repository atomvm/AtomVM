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
#define TERM_INTEGER_TAG 0xF
#define TERM_CATCH_TAG 0x1B

#define TERM_BOXED_TAG_MASK 0x3F
#define TERM_BOXED_TUPLE 0x0
#define TERM_BOXED_POSITIVE_INTEGER 0x8
#define TERM_BOXED_REF 0x10
#define TERM_BOXED_FUN 0x14
#define TERM_BOXED_FLOAT 0x18
#define TERM_BOXED_REFC_BINARY 0x20
#define TERM_BOXED_HEAP_BINARY 0x24

#define TERM_BOXED_REFC_BINARY_SIZE 3

#define BINARY_HEADER_SIZE 2
#define FUNCTION_REFERENCE_SIZE 4
#define BOXED_INT_SIZE (BOXED_TERMS_REQUIRED_FOR_INT + 1)
#define BOXED_INT64_SIZE (BOXED_TERMS_REQUIRED_FOR_INT64 + 1)
#define FLOAT_SIZE (sizeof(float_term_t) / sizeof(term) + 1)

#define TERM_DEBUG_ASSERT(...)

#define TERM_FROM_ATOM_INDEX(atom_index) ((atom_index << 6) | 0xB)

/**
 * @brief All empty tuples will reference this
 */
extern const term empty_tuple;

/**
 * @brief Compares two terms
 *
 * @details Tells if first term is >, < or == to the second term.
 * @param t the first term
 * @param other the second term
 * @return 0 when given terms are equals, otherwise -1 when t < other, or 1 when t > other.
 */
int term_compare(term t, term other, Context *ctx);

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
    TERM_DEBUG_ASSERT((t & 0x3) == 0x2);

    const term *boxed_value = term_to_const_term_ptr(t);
    return term_get_size_from_boxed_header(*boxed_value);
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
        int masked_value = boxed_value[0] & 0x3F;
        switch (masked_value) {
            case TERM_BOXED_REFC_BINARY:
            case TERM_BOXED_HEAP_BINARY:
                return 1;
            default:
                return 0;
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

/**
 * @brief Checks if a term is a uint8_t
 *
 * @details Returns 1 if a term is an integer value in the [0, 255] range, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline uint8_t term_is_uint8(term t)
{
    return ((t & ~((term) 0xFF0)) == 0xF);
}

static inline int term_is_boxed_integer(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & 0x3F) == TERM_BOXED_POSITIVE_INTEGER) {
            return 1;
        }
    }

    return 0;
}

static inline int term_is_any_integer(term t)
{
    return term_is_integer(t) || term_is_boxed_integer(t);
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
    return TERM_FROM_ATOM_INDEX(atom_index);
}

/**
 * @brief Term to uint8
 *
 * @details Returns an uint8 for a given term. No overflow check is executed.
 * @param t the term that will be converted to uint8.
 * @return an uint8_t value.
 */
static inline uint8_t term_to_uint8(term t)
{
    TERM_DEBUG_ASSERT(term_is_uint8(t));

    return ((uint16_t) t) >> 4;
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
    TERM_DEBUG_ASSERT(term_is_integer(t));

    return ((int32_t) t) >> 4;
}

static inline avm_int_t term_to_int(term t)
{
    TERM_DEBUG_ASSERT(term_is_integer(t));

    return ((avm_int_t) t) >> 4;
}

static inline int term_to_catch_label_and_module(term t, int *module_index)
{
    *module_index = t >> 24;
    return (t >> 6) & 0x3FFFF;
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
    TERM_DEBUG_ASSERT(term_is_pid(t));

    return t >> 4;
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

static inline term term_from_int(avm_int_t value)
{
    return (value << 4) | 0xF;
}

static inline avm_int_t term_unbox_int(term boxed_int)
{
    TERM_DEBUG_ASSERT(term_is_boxed_integer(boxed_int));

    const term *boxed_value = term_to_const_term_ptr(boxed_int);

    return (avm_int_t) boxed_value[1];
}

static inline avm_int64_t term_unbox_int64(term boxed_long)
{
    TERM_DEBUG_ASSERT(term_is_boxed_integer(boxed_long));

    const term *boxed_value = term_to_const_term_ptr(boxed_long);

    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 1
        return (avm_int64_t) boxed_value[1];

    #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        #if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
            return (avm_int64_t) ((avm_uint64_t) boxed_value[1] | ((avm_uint64_t) boxed_value[2] << 32));
        #elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
            return (avm_int64_t) ((avm_uint64_t) boxed_value[1] << 32) | (avm_uint64_t) boxed_value[2];
        #else
            #error "unsupported endianess."
        #endif
    #else
        #error "unsupported configuration."
    #endif
}

static inline avm_int_t term_maybe_unbox_int(term maybe_boxed_int)
{
    if (term_is_boxed_integer(maybe_boxed_int)) {
        return term_unbox_int(maybe_boxed_int);
    } else {
        return term_to_int(maybe_boxed_int);
    }
}

static inline avm_int64_t term_maybe_unbox_int64(term maybe_boxed_int)
{
    if (term_is_boxed(maybe_boxed_int)) {
        if (term_boxed_size(maybe_boxed_int) == 1) {
            return term_unbox_int(maybe_boxed_int);
        } else {
            return term_unbox_int64(maybe_boxed_int);
        }
    } else {
        return term_to_int(maybe_boxed_int);
    }
}

static inline term term_make_boxed_int(avm_int_t value, Context *ctx)
{
    term *boxed_int = memory_heap_alloc(ctx, 1 + BOXED_TERMS_REQUIRED_FOR_INT);
    boxed_int[0] = (BOXED_TERMS_REQUIRED_FOR_INT << 6) | TERM_BOXED_POSITIVE_INTEGER; // OR sign bit
    boxed_int[1] = value;

    return ((term) boxed_int) | TERM_BOXED_VALUE_TAG;
}

static inline term term_make_boxed_int64(avm_int64_t large_int64, Context *ctx)
{
    term *boxed_int = memory_heap_alloc(ctx, 1 + BOXED_TERMS_REQUIRED_FOR_INT64);
    boxed_int[0] = (BOXED_TERMS_REQUIRED_FOR_INT64 << 6) | TERM_BOXED_POSITIVE_INTEGER; // OR sign bit
    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 1
        boxed_int[1] = large_int64;
    #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        #if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
            boxed_int[1] = large_int64;
            boxed_int[2] = large_int64 >> 32;
        #elif  __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
            boxed_int[2] = large_int64;
            boxed_int[1] = large_int64 >> 32;
        #else
            #error "unsupported endianess."
        #endif
    #else
        #error "unsupported configuration."
    #endif

    return ((term) boxed_int) | TERM_BOXED_VALUE_TAG;
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
static inline term term_from_literal_binary(const void *data, uint32_t size, Context *ctx)
{
    int size_in_terms = term_binary_data_size_in_terms(size);

    term *boxed_value = memory_heap_alloc(ctx, size_in_terms + 1);
    boxed_value[0] = (size_in_terms << 6) | 0x24; // heap binary
    boxed_value[1] = size;

    memcpy(boxed_value + 2, data, size);

    return ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
}

static inline term term_from_const_binary(const void *data, uint32_t size, Context *ctx)
{
    term *boxed_value = memory_heap_alloc(ctx, TERM_BOXED_REFC_BINARY_SIZE);
    boxed_value[0] = (2 << 6) | TERM_BOXED_REFC_BINARY;
    boxed_value[1] = size;
    boxed_value[2] = (term) data;

    return ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
}

/**
* @brief Create an unititialized binary.
*
* @details Allocates a binary on the heap, and returns a term pointing to it.
* Note that the data in teh binary is unitialized and could contain any garbage.
* Make sure to initialize before use, if needed (e.g., via memset).
* @param size size of binary data buffer.
* @param ctx the context that owns the memory that will be allocated.
* @return a term pointing to the boxed binary pointer.
*/
static inline term term_create_uninitialized_binary(uint32_t size, Context *ctx)
{
    int size_in_terms = term_binary_data_size_in_terms(size);

    term *boxed_value = memory_heap_alloc(ctx, size_in_terms + 1);
    boxed_value[0] = (size_in_terms << 6) | 0x24; // heap binary
    boxed_value[1] = size;

    return ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
}

/**
 * @brief Gets binary size
 *
 * @details Returns binary size for a given binary term.
 * @param t a term pointing to binary data. Fails if t is not a binary term.
 * @return binary size in bytes.
 */
static inline unsigned long term_binary_size(term t)
{
    TERM_DEBUG_ASSERT(term_is_binary(t));

    const term *boxed_value = term_to_const_term_ptr(t);
    return boxed_value[1];
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
    TERM_DEBUG_ASSERT(term_is_binary(t));

    const term *boxed_value = term_to_const_term_ptr(t);
    if (boxed_value[0] & 0x4) {
        return (const char *) (boxed_value + 2);
    } else {
        return ((const char *) boxed_value[2]);
    }
}

/**
* @brief Create an empty binary.  All bytes in the binary are initialized to 0x00
*
* @details Allocates a binary on the heap, and returns a term pointing to it.
* @param size size of binary data buffer.
* @param ctx the context that owns the memory that will be allocated.
* @return a term pointing to the boxed binary pointer.
*/
static inline term term_create_empty_binary(uint32_t size, Context *ctx)
{
    term t = term_create_uninitialized_binary(size, ctx);
    memset((char *) term_binary_data(t), 0x00, size);
    return t;
}

/**
* @brief Insert an integer into a binary (using bit syntax).
*
* @details Insert the low order n bits on value into the binary stored in t, starting
* at the bit position starting in offset.
* @param t a term pointing to binary data. Fails if t is not a binary term.
* @param offset the bitwise offset in t at which to start writing the integer value
* @param value the integer value to write
* @param n the number of low-order bits from value to write.
* @return 0 on success; non-zero value if:
*           t is not a binary term
*           n is greater than the number of bits in an integer
*           there is insufficient capacity in the binary to write these bits
* In general, none of these conditions should apply, if this function is being
* called in the context of generated bit syntax instructions.
*/
static inline int term_bs_insert_integer(term t, avm_int_t offset, avm_int_t value, avm_int_t n, avm_int_t flags)
{
    if (!term_is_binary(t)) {
        return -1;
    }
    if (n > sizeof(avm_int_t) * 8) {
        return -2;
    }
    size_t capacity = term_binary_size(t);
    if (8 * capacity < (offset + n)) {
        return -3;
    }
    // TODO optimize by xor'ing by byte (or mask on boundaries)
    // TODO support big/little endian flags
    for (int i = 0;  i < n;  ++i) {
        int k = (n - 1) - i;
        int bit_val = (value & (0x01 << k)) >> k;
        if (bit_val) {
            int bit_pos = offset + i;
            int byte_pos = bit_pos / 8;
            uint8_t *pos = (uint8_t *) (term_binary_data(t) + byte_pos);
            int shift = 7 - (bit_pos % 8);
            *pos ^= (0x01 << shift);
        }
    }
    return 0;
}

/**
* @brief Extract an integer from a binary (using bit syntax).
*
* @details Extract the next size * unit bits from src into dst, starting
* at the bit position starting in offset.
* @param dst the location of the integer value to write
* @param src a term pointing to binary data. Fails if src is not a binary term.
* @param n the number of low-order bits from value to read.
* @return 0 on success; non-zero value if:
*           src is not a binary term
*           there is insufficient capacity in the binary to read the desired number of bits
* In general, none of these conditions should apply, if this function is being
* called in the context of generated bit syntax instructions.
*/
static inline int term_bs_extract_integer(avm_int_t *dst, term src, size_t offset, avm_int_t n, avm_int_t flags)
{
    if (!term_is_binary(src)) {
        return -1;
    }
    size_t capacity = term_binary_size(src);
    if (8 * capacity - offset < n) {
        return -2;
    }
    *dst = 0;
    // TODO optimize by xor'ing by byte (or mask on boundaries)
    // TODO support big/little endian flags
    // TODO support big/little sign flags
    for (int i = 0;  i < n;  ++i) {
        int bit_pos = offset + i;
        int byte_pos = bit_pos / 8;
        uint8_t *pos = (uint8_t *) (term_binary_data(src) + byte_pos);
        int shift = 7 - (bit_pos % 8);

        uint8_t bit_val = ((0x01 << shift) & *pos) >> shift;
        if (bit_val) {
            *dst |= 0x01 << (n - i - 1);
        }
    }
    return 0;
}

/**
* @brief Insert an binary into a binary (using bit syntax).
*
* @details Insert the data from the input binary, starting
* at the bit position starting in offset.
* @param t a term pointing to binary data. Fails if t is not a binary term.
* @param offset the bitwise offset in t at which to start writing the integer value
* @param value the integer value to write
* @param n the bumber of low-order bits from value to write.
* @return 0 on success; non-zero value if:
*           t is not a binary term
*           n is greater than the number of bits in an integer
*           there is insufficient capacity in the binary to write these bits
* In general, none of these conditions should apply, if this function is being
* called in the context of generated bit syntax instructions.
*/
static inline int term_bs_insert_binary(term t, int offset, term src, int n)
{
    if (!term_is_binary(t)) {
        fprintf(stderr, "Target is not a binary\n");
        return -1;
    }
    if (!term_is_binary(src)) {
        fprintf(stderr, "Source is not a binary\n");
        return -2;
    }
    if (offset % 8 != 0) {
        fprintf(stderr, "Offset not aligned on a byte boundary\n");
        return -3;
    }
    size_t capacity = term_binary_size(t);
    if (capacity < (offset/8 + n)) {
        fprintf(stderr, "Insufficient capacity to write binary\n");
        return -4;
    }
    uint8_t *dst_pos = (uint8_t *) term_binary_data(t) + offset/8;
    uint8_t *src_pos = (uint8_t *) term_binary_data(src);
    memcpy(dst_pos, src_pos, n);
    return 0;
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

    #if TERM_BYTES == 8
        boxed_value[1] = (term) ref_ticks;

    #elif TERM_BYTES == 4
        boxed_value[1] = (ref_ticks >> 4);
        boxed_value[2] = (ref_ticks & 0xFFFFFFFF);

    #else
        #error "terms must be either 32 or 64 bit wide"
    #endif

    return ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
}

static inline uint64_t term_to_ref_ticks(term rt)
{
    TERM_DEBUG_ASSERT(term_is_reference(rt));

    const term *boxed_value = term_to_const_term_ptr(rt);

    #if TERM_BYTES == 8
        return boxed_value[1];

    #elif TERM_BYTES == 4
        return (boxed_value[1] << 4) | boxed_value[2];

    #else
        #error "terms must be either 32 or 64 bit wide"
    #endif
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
    TERM_DEBUG_ASSERT(term_is_tuple(t));

    term *boxed_value = term_to_term_ptr(t);

    TERM_DEBUG_ASSERT(((boxed_value[0] & 0x3F) == 0) && (elem_index < (boxed_value[0] >> 6)));

    boxed_value[elem_index + 1] = put_value;
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
    TERM_DEBUG_ASSERT(term_is_tuple(t));

    const term *boxed_value = term_to_const_term_ptr(t);

    TERM_DEBUG_ASSERT(((boxed_value[0] & 0x3F) == 0) && (elem_index < (boxed_value[0] >> 6)));

    return boxed_value[elem_index + 1];
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
    TERM_DEBUG_ASSERT(term_is_tuple(t));

    const term *boxed_value = term_to_const_term_ptr(t);
    return term_get_size_from_boxed_header(boxed_value[0]);
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
static inline int term_list_length(term t, int *proper)
{
    int len = 0;

    while (term_is_nonempty_list(t)) {
        len++;
        t = term_get_list_tail(t);
    }
    *proper = term_is_nil(t);

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
static inline int term_exactly_equals(term a, term b, Context *ctx)
{
    if (a == b) {
        return 1;
    } else {
        return term_compare(a, b, ctx) == 0;
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
static inline int term_equals(term a, term b, Context *ctx)
{
    if (a == b) {
        return 1;
    } else {
        //TODO: add parameter for exactly equals.
        return term_compare(a, b, ctx) == 0;
    }
}

#ifndef AVM_NO_FP

static inline int term_is_float(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        return (boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_FLOAT;

    } else {
        return 0;
    }
}

static inline term term_from_float(avm_float_t f, Context *ctx)
{
    term *boxed_value = memory_heap_alloc(ctx, FLOAT_SIZE);
    boxed_value[0] = ((FLOAT_SIZE - 1) << 6) | TERM_BOXED_FLOAT;

    float_term_t *boxed_float = (float_term_t *) (boxed_value + 1);
    boxed_float->f = f;

    return ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
}

static inline avm_float_t term_to_float(term t)
{
    const float_term_t *boxed_float = (float_term_t *) (term_to_const_term_ptr(t) + 1);
    return boxed_float->f;
}

static inline avm_float_t term_conv_to_float(term t)
{
    if (term_is_any_integer(t)) {
        return term_maybe_unbox_int64(t);
    } else {
        return term_to_float(t);
    }
}

#endif

static inline int term_is_number(term t)
{
    #ifndef AVM_NO_FP
       return term_is_any_integer(t) || term_is_float(t);
    #else
       return term_is_any_integer(t);
    #endif
}

/**
 * @brief Prints a term to stdout
 *
 * @details Print any given term to the standard output.
 * @param t the term that will be printed.
 * @param ctx the context.
 */
void term_display(FILE *fd, term t, const Context *ctx);

/**
 * @brief Checks if a term is a string (i.e., a list of characters)
 *
 * @details Returns 1 if a term is a proper (nil-terminated) list of characters
 * or an empty list (nil term), otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succedes, 0 otherwise.
 */
static inline int term_is_string(term t)
{
    while (term_is_nonempty_list(t)) {
        term e = term_get_list_head(t);
        if (!term_is_uint8(e)) {
            return 0;
        }
        t = term_get_list_tail(t);
    }
    return term_is_nil(t);
}

/**
 * @brief Checks to see if e is a member of list
 *
 * @details returns 1 if e is equal to an element of list; 0, otherwise
 * @param   list list term
 * @param   e element term
 * @return  1 if e is equal to a member of list; 0, otherwise
 */
static inline int term_list_member(term list, term e, Context *ctx)
{
    term t = list;
    while (term_is_nonempty_list(t)) {
        term head = term_get_list_head(t);
        if (term_equals(head, e, ctx)) {
            return 1;
        }
        t = term_get_list_tail(t);
    }
    return 0;
}


static inline term term_make_function_reference(term m, term f, term a, Context *ctx)
{
    if (memory_ensure_free(ctx, FUNCTION_REFERENCE_SIZE) != MEMORY_GC_OK) {
        return term_invalid_term();
    }
    term *boxed_func = memory_heap_alloc(ctx, FUNCTION_REFERENCE_SIZE);

    boxed_func[0] = ((FUNCTION_REFERENCE_SIZE - 1) << 6) | TERM_BOXED_FUN;
    boxed_func[1] = m;
    boxed_func[2] = f;
    boxed_func[3] = a;

    return ((term) boxed_func) | TERM_BOXED_VALUE_TAG;
}

#endif
