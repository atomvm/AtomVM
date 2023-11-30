/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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

/**
 * @file term.h
 * @brief term manipulation functions
 *
 * @details This header implements term manipulation functions.
 */

#ifndef _TERM_H_
#define _TERM_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "refc_binary.h"
#include "utils.h"

#include "term_typedef.h"

#define TERM_BOXED_VALUE_TAG 0x2
#define TERM_INTEGER_TAG 0xF
#define TERM_CATCH_TAG 0x1B

#define TERM_BOXED_TAG_MASK 0x3F
#define TERM_BOXED_TUPLE 0x0
#define TERM_BOXED_BIN_MATCH_STATE 0x4
#define TERM_BOXED_POSITIVE_INTEGER 0x8
#define TERM_BOXED_REF 0x10
#define TERM_BOXED_FUN 0x14
#define TERM_BOXED_FLOAT 0x18
#define TERM_BOXED_REFC_BINARY 0x20
#define TERM_BOXED_HEAP_BINARY 0x24
#define TERM_BOXED_MAP 0x3C
#define TERM_BOXED_SUB_BINARY 0x28

#define TERM_UNUSED 0x2B
#define TERM_RESERVED_MARKER(x) ((x << 6) | TERM_UNUSED)

#define TERM_BOXED_REFC_BINARY_SIZE 6
#define TERM_BOXED_BIN_MATCH_STATE_SIZE 4
#define TERM_BOXED_SUB_BINARY_SIZE 4
#define TERM_BOXED_RESOURCE_SIZE TERM_BOXED_REFC_BINARY_SIZE
#if TERM_BYTES == 8
    #define REFC_BINARY_MIN 64
    #define SUB_BINARY_MIN 16
#elif TERM_BYTES == 4
    #define REFC_BINARY_MIN 32
    #define SUB_BINARY_MIN 8
#else
    #error
#endif

#define BINARY_HEADER_SIZE 2
#define FUNCTION_REFERENCE_SIZE 4
#define BOXED_INT_SIZE (BOXED_TERMS_REQUIRED_FOR_INT + 1)
#define BOXED_INT64_SIZE (BOXED_TERMS_REQUIRED_FOR_INT64 + 1)
#define BOXED_FUN_SIZE 3
#define FLOAT_SIZE (sizeof(float_term_t) / sizeof(term) + 1)
#define REF_SIZE ((int) ((sizeof(uint64_t) / sizeof(term)) + 1))
#define TUPLE_SIZE(elems) ((int) (elems + 1))
#define CONS_SIZE 2
#define REFC_BINARY_CONS_OFFSET 4
#define LIST_SIZE(num_elements, element_size) ((num_elements) * ((element_size) + CONS_SIZE))
#define TERM_MAP_SIZE(num_elements) (3 + 2 * (num_elements))
#define TERM_MAP_SHARED_SIZE(num_elements) (2 + (num_elements))

#define LIST_HEAD_INDEX 1
#define LIST_TAIL_INDEX 0

#define TERM_BINARY_SIZE_IS_HEAP(size) ((size) < REFC_BINARY_MIN)

#if TERM_BYTES == 4
#define TERM_BINARY_DATA_SIZE_IN_TERMS(size) \
    (TERM_BINARY_SIZE_IS_HEAP(size) ? (((size) + 4 - 1) >> 2) + 1 : TERM_BOXED_REFC_BINARY_SIZE)
#elif TERM_BYTES == 8
#define TERM_BINARY_DATA_SIZE_IN_TERMS(size) \
    (TERM_BINARY_SIZE_IS_HEAP(size) ? (((size) + 8 - 1) >> 3) + 1 : TERM_BOXED_REFC_BINARY_SIZE)
#endif

#define TERM_BINARY_HEAP_SIZE(size) \
    (TERM_BINARY_DATA_SIZE_IN_TERMS(size) + BINARY_HEADER_SIZE)

#define TERM_DEBUG_ASSERT(...)

#define TERM_FROM_ATOM_INDEX(atom_index) ((atom_index << 6) | 0xB)

#ifndef TYPEDEF_GLOBALCONTEXT
#define TYPEDEF_GLOBALCONTEXT
typedef struct GlobalContext GlobalContext;
#endif

typedef struct PrinterFun PrinterFun;

typedef int (*printer_function_t)(PrinterFun *fun, const char *fmt, ...) PRINTF_FORMAT_ARGS(2, 3);

struct PrinterFun
{
    printer_function_t print;
};

enum RefcBinaryFlags
{
    RefcNoFlags = 0,
    RefcBinaryIsConst
};

typedef enum
{
    TermCompareNoOpts = 0,
    TermCompareExact = 1
} TermCompareOpts;

typedef enum
{
    TermCompareMemoryAllocFail = 0,
    TermEquals = 1,
    TermLessThan = 2,
    TermGreaterThan = 4
} TermCompareResult;

#define TERM_MAP_NOT_FOUND -1
#define TERM_MAP_MEMORY_ALLOC_FAIL -2

/**
 * @brief All empty tuples will reference this
 */
extern const term empty_tuple;

/**
 * @brief Compares two terms
 *
 * @details Tells if first term is >, < or == to the second term.
 * @param t the first term.
 * @param other the second term.
 * @param opts a value of 1 will compare exact equality, 0 for less strict equality.
 * @param global the global context.
 * @return any of TermEquals, TermLessThan, TermGreaterThan or TermCompareMemoryAllocFail error.
 */
TermCompareResult term_compare(term t, term other, TermCompareOpts opts, GlobalContext *global);

/**
 * @brief Create a reference-counted binary on the heap
 *
 * @details This function will create a reference-counted binary on the heap.  If the data
 * supplied is "const" (e.g., read from a literal in a BEAM file), then the returned term
 * will point directly to the supplied data, and will not technically be reference-counted.
 * Otherwise, a block of memory will be allocated to contain a copy of the data, in addition
 * to a reference counter, so that the block can be free'd when no other terms reference
 * the created object.  (The reference count will be initialized to 1).  If the data is
 * non-NULL, it will be copied into the newly allocated block of memory.
 * @param size the size (in bytes) of the data to allocate
 * @param is_const designates whether the data pointed to is "const", such as a term literal
 * @param heap the heap to allocate the binary in
 * @param glb the global context as refc binaries are global
 * @return a term (reference) pointing to the newly allocated binary in the process heap.
 */
term term_alloc_refc_binary(size_t size, bool is_const, Heap *heap, GlobalContext *glb);

/**
 * @brief Create a sub-binary
 *
 * @details This function will create a sub-binary on the heap, using the supplied binary,
 * offset into the binary, and length of the sub-binary.  This function assumes the length
 * of the referenced binary is greater or equal to offset + len.
 * @param binary the referenced binary
 * @param offset the offset into the referenced binary to start the sub-binary
 * @param len the length (in bytes) of the sub-binary
 * @param heap the heap to allocate the binary in
 * @return a term (reference) pointing to the newly allocated sub-binary in the process heap.
 */
term term_alloc_sub_binary(term binary, size_t offset, size_t len, Heap *heap);

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
 * @return 1 if check succeeds, 0 otherwise.
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
 * @return 1 if check succeeds, 0 otherwise.
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
 * @return 1 if check succeeds, 0 otherwise.
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
 * @return 1 if check succeeds, 0 otherwise.
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
 * @return 1 if check succeeds, 0 otherwise.
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
 * @return 1 if check succeeds, 0 otherwise.
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
 * @return 1 if check succeeds, 0 otherwise.
 */
static inline int term_is_movable_boxed(term t)
{
    /* boxed: 10 */
    if ((t & 0x3) == 0x2) {
        const term *boxed_value = term_to_const_term_ptr(t);
        switch (boxed_value[0] & TERM_BOXED_TAG_MASK) {
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
 * @param header the boxed term header.
 * @return the size of the boxed term that follows the header. 0 is returned if the boxed term is just the header.
 */
static inline size_t term_get_size_from_boxed_header(term header)
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
static inline size_t term_boxed_size(term t)
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
 * @return 1 if check succeeds, 0 otherwise.
 */
static inline int term_is_binary(term t)
{
    /* boxed: 10 */
    if ((t & 0x3) == 0x2) {
        const term *boxed_value = term_to_const_term_ptr(t);
        int masked_value = boxed_value[0] & TERM_BOXED_TAG_MASK;
        switch (masked_value) {
            case TERM_BOXED_REFC_BINARY:
            case TERM_BOXED_HEAP_BINARY:
            case TERM_BOXED_SUB_BINARY:
                return 1;
            default:
                return 0;
        }
    }

    return 0;
}

/**
 * @brief Checks if a term is a binary
 *
 * @details Returns 1 if a term is a binary stored on the heap, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succeeds, 0 otherwise.
 */
static inline bool term_is_refc_binary(term t)
{
    /* boxed: 10 */
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        int masked_value = boxed_value[0] & TERM_BOXED_TAG_MASK;
        return masked_value == TERM_BOXED_REFC_BINARY;
    }

    return false;
}

static inline bool term_refc_binary_is_const(term t)
{
    const term *boxed_value = term_to_const_term_ptr(t);
    return boxed_value[2] & RefcBinaryIsConst;
}

/**
 * @brief Checks if a term is a sub-binary
 *
 * @details Returns true if a term is a sub-binary; false, otherwise.
 * @param t the term that will be checked.
 * @return true if check succeeds; false, otherwise.
 */
static inline bool term_is_sub_binary(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        int masked_value = boxed_value[0] & TERM_BOXED_TAG_MASK;
        return masked_value == TERM_BOXED_SUB_BINARY;
    }

    return false;
}

/**
 * @brief Checks if a term is an integer value
 *
 * @details Returns 1 if a term is an integer value, otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succeeds, 0 otherwise.
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
 * @return 1 if check succeeds, 0 otherwise.
 */
static inline uint8_t term_is_uint8(term t)
{
    return ((t & ~((term) 0xFF0)) == 0xF);
}

static inline int term_is_boxed_integer(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_POSITIVE_INTEGER) {
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
 * @return 1 if check succeeds, 0 otherwise.
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
 * @return 1 if check succeeds, 0 otherwise.
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
 * @return 1 if check succeeds, 0 otherwise.
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
 * @return 1 if check succeeds, 0 otherwise.
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
 * @return 1 if check succeeds, 0 otherwise.
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
static inline term term_invalid_term()
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
        AVM_ABORT();

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
        AVM_ABORT();

    } else {
        return (value << 4) | 0xF;
    }

#elif TERM_BITS == 64
    // 1152921504606846975 = 0x0FFFFFFFFFFFFFFF
    if (UNLIKELY((value > 1152921504606846975) || (value < -1152921504606846975))) {
        //TODO: unimplemented on heap integer value
        fprintf(stderr, "unimplemented: term should be moved to heap.");
        AVM_ABORT();

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
            #error "unsupported endianness."
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

static inline term term_make_boxed_int(avm_int_t value, Heap *heap)
{
    term *boxed_int = memory_heap_alloc(heap, 1 + BOXED_TERMS_REQUIRED_FOR_INT);
    boxed_int[0] = (BOXED_TERMS_REQUIRED_FOR_INT << 6) | TERM_BOXED_POSITIVE_INTEGER; // OR sign bit
    boxed_int[1] = value;
    return ((term) boxed_int) | TERM_BOXED_VALUE_TAG;
}

static inline term term_make_boxed_int64(avm_int64_t large_int64, Heap *heap)
{
    term *boxed_int = memory_heap_alloc(heap, 1 + BOXED_TERMS_REQUIRED_FOR_INT64);
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
            #error "unsupported endianness."
        #endif
    #else
        #error "unsupported configuration."
    #endif
    return ((term) boxed_int) | TERM_BOXED_VALUE_TAG;
}

static inline term term_make_maybe_boxed_int64(avm_int64_t value, Heap *heap)
{
    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        if ((value < AVM_INT_MIN) || (value > AVM_INT_MAX)) {
            return term_make_boxed_int64(value, heap);
        }
    #endif

    if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        return term_make_boxed_int(value, heap);

    } else {
        return term_from_int(value);
    }
}

static inline size_t term_boxed_integer_size(avm_int64_t value)
{
    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        if ((value < AVM_INT_MIN) || (value > AVM_INT_MAX)) {
            return BOXED_INT64_SIZE;
        }
    #endif
    if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        return BOXED_INT_SIZE;
    } else {
        return 0;
    }
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
 * @brief Determine whether a binary should be a heap binary or not
 *
 * @details Returns true if a binary of the specified size should be allocated
 *          in the process heap (as opposed to being a refc binary)
 * @param size the intended binary size
 * @return true if the binary should be allocated in the process heap; false, otherwise.
 */
static inline bool term_binary_size_is_heap_binary(size_t size)
{
    return size < REFC_BINARY_MIN;
}

/**
 * @brief The count of terms needed to store the given amount of bytes
 *
 * @details Returns the count of terms needed to store the given size in bytes.
 * @param size the size in bytes
 * @return the count of terms
 */
static inline size_t term_binary_data_size_in_terms(size_t size)
{
    if (term_binary_size_is_heap_binary(size)) {
#if TERM_BYTES == 4
    return ((size + 4 - 1) >> 2) + 1;
#elif TERM_BYTES == 8
    return ((size + 8 - 1) >> 3) + 1;
#else
    #error
#endif
    } else {
        return TERM_BOXED_REFC_BINARY_SIZE;
    }
}

/**
 * @brief The size (in terms) of a binary of size-many bytes in the heap
 *
 * @details Returns the number of terms needed in the heap to store a binary of a given size (in bytes)
 * @param size the size of the binary (in bytes)
 * @return the size (in terms) of a binary of size-many bytes in the heap
 */
static inline size_t term_binary_heap_size(size_t size)
{
    return term_binary_data_size_in_terms(size) + BINARY_HEADER_SIZE;
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
 * @brief Get the pointer off heap refc binary.
 *
 * @details Returns address
 * @return offset (in words).
 */
static inline void *term_refc_binary_ptr(term refc_binary)
{
    TERM_DEBUG_ASSERT(term_is_refc_binary(refc_binary));

    term *boxed_value = term_to_term_ptr(refc_binary);
    return (void *) boxed_value[3];
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
    if (term_is_refc_binary(t)) {
        if (term_refc_binary_is_const(t)) {
            return (const char *) boxed_value[3];
        } else {
            return (const char *) refc_binary_get_data((struct RefcBinary *) boxed_value[3]);
        }
    }
    if (term_is_sub_binary(t)) {
        return term_binary_data(boxed_value[3]) + boxed_value[2]; // offset
    }
    return (const char *) (boxed_value + 2);
}

/**
* @brief Create an uninitialized binary.
*
* @details Allocates a binary on the heap, and returns a term pointing to it.
* Note that the data in the binary is uninitialized and could contain any garbage.
* Make sure to initialize before use, if needed (e.g., via memset).
* The binary may be allocated as a refc if it is large enough.
* @param size size of binary data buffer.
* @param heap the heap to allocate the binary in
* @param glb the global context as refc binaries are global
* @return a term pointing to the boxed binary pointer.
*/
static inline term term_create_uninitialized_binary(size_t size, Heap *heap, GlobalContext *glb)
{
    if (term_binary_size_is_heap_binary(size)) {
        size_t size_in_terms = term_binary_data_size_in_terms(size);

        term *boxed_value = memory_heap_alloc(heap, size_in_terms + 1);
        boxed_value[0] = (size_in_terms << 6) | TERM_BOXED_HEAP_BINARY;
        boxed_value[1] = size;

        return ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
    } else {
        return term_alloc_refc_binary(size, false, heap, glb);
    }
}

/**
 * @brief Term from binary data
 *
 * @details Allocates a binary on the heap, and returns a term pointing to it.
 * @param data binary data.
 * @param size size of binary data buffer.
 * @param heap the heap to allocate the binary in
 * @param glb the global context as refc binaries are global
 * @return a term pointing to the boxed binary pointer.
 */
static inline term term_from_literal_binary(const void *data, size_t size, Heap *heap, GlobalContext *glb)
{
    term binary = term_create_uninitialized_binary(size, heap, glb);
    memcpy((void *) term_binary_data(binary), data, size);
    return binary;
}

/**
 * @brief Get the number of words in the heap to allocate for a sub-binary.
 *
 * @details This function is used to compute the number of words needed on the heap
 * to allocate for a sub-binary.  This function is typically used in conjunction with
 *term_maybe_create_sub_binary
 * @param binary source binary
 * @param len desired length of the sub-binary
 * @return the number of words needed to allocate on the process heap for the desired sub-binary
 */
static inline size_t term_sub_binary_heap_size(term binary, size_t len)
{
    if (term_is_refc_binary(binary) && len >= SUB_BINARY_MIN) {
        return TERM_BOXED_SUB_BINARY_SIZE;
    } else {
        return term_binary_heap_size(len);
    }
}

/**
 * @brief (Maybe) create a sub-binary -- if not, create a heap binary.
 *
 * @details Allocates a sub-binary if the source binary is reference-counted
 * binary and if the length of the sub-binary is sufficiently large.
 * @param binary source binary
 * @param offset offset into the source binary marking the start of the sub-binary
 * @param len desired length of the sub-binary
 * @param heap the heap to allocate memory in
 * @param glb the global context as refc binaries are global
 * @return a term pointing to the boxed binary pointer.
 */
static inline term term_maybe_create_sub_binary(term binary, size_t offset, size_t len, Heap *heap, GlobalContext *glb)
{
    if (term_is_refc_binary(binary) && len >= SUB_BINARY_MIN) {
        return term_alloc_sub_binary(binary, offset, len, heap);
    } else if (term_is_sub_binary(binary) && len >= SUB_BINARY_MIN) {
        const term *boxed_value = term_to_const_term_ptr(binary);
        return term_alloc_sub_binary(boxed_value[3], boxed_value[2] + offset, len, heap);
    } else {
        const char *data = term_binary_data(binary);
        return term_from_literal_binary(data + offset, len, heap, glb);
    }
}

static inline void term_set_refc_binary_data(term t, const void *data)
{
    TERM_DEBUG_ASSERT(term_is_refc_binary(t));
    term *boxed_value = term_to_term_ptr(t);
    boxed_value[3] = (term) data;
}

static inline term term_from_const_binary(const void *data, size_t size, Heap *heap, GlobalContext *glb)
{
    term binary = term_alloc_refc_binary(size, true, heap, glb);
    term_set_refc_binary_data(binary, data);
    return binary;
}

/**
* @brief Create an empty binary.  All bytes in the binary are initialized to 0x00
*
* @details Allocates a binary on the heap, and returns a term pointing to it.
* @param size size of binary data buffer.
* @param heap the heap to allocate memory in
* @param glb the global context as refc binaries are global
* @return a term pointing to the boxed binary pointer.
*/
static inline term term_create_empty_binary(size_t size, Heap *heap, GlobalContext *glb)
{
    term t = term_create_uninitialized_binary(size, heap, glb);
    memset((char *) term_binary_data(t), 0x00, size);
    return t;
}

/**
* @brief Insert an binary into a binary (using bit syntax).
*
* @details Insert the data from the input binary, starting
* at the bit position starting in offset.
* @param t a term pointing to binary data. Fails if t is not a binary term.
* @param offset the bitwise offset in t at which to start writing the integer value
* @param src binary source to insert binary data into.
* @param n the number of low-order bits from value to write.
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
    unsigned long capacity = term_binary_size(t);
    if (capacity < (unsigned long) (offset / 8 + n)) {
        fprintf(stderr, "Insufficient capacity to write binary\n");
        return -4;
    }
    uint8_t *dst_pos = (uint8_t *) term_binary_data(t) + offset / 8;
    uint8_t *src_pos = (uint8_t *) term_binary_data(src);
    memcpy(dst_pos, src_pos, n);
    return 0;
}

/**
 * @brief Get a ref term from ref ticks
 *
 * @param ref_ticks an unique uint64 value that will be used to create ref term.
 * @param heap the heap to allocate memory in
 * @return a ref term created using given ref ticks.
 */
static inline term term_from_ref_ticks(uint64_t ref_ticks, Heap *heap)
{
    term *boxed_value = memory_heap_alloc(heap, REF_SIZE);
    boxed_value[0] = ((REF_SIZE - 1) << 6) | TERM_BOXED_REF;

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
 * @brief Allocates a tuple on a context heap
 *
 * @details Allocates an uninitialized tuple on the heap with given arity.
 * @param size tuple arity (count of tuple elements).
 * @param heap the heap to allocate memory in
 * @return a term pointing on an empty tuple allocated on the heap.
 */
static inline term term_alloc_tuple(uint32_t size, Heap *heap)
{
    //TODO: write a real implementation
    //align constraints here
    term *boxed_value = memory_heap_alloc(heap, 1 + size);
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
 * @param heap the heap to allocate memory in
 * @return a term pointing to a list.
 */
static inline term term_from_string(const uint8_t *data, uint16_t size, Heap *heap)
{
    //TODO: write a real implementation
    //align constraints here
    term *list_cells = memory_heap_alloc(heap, size * 2);
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
 * @param list_elem a pointer to a list element.
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
    return list_ptr[LIST_HEAD_INDEX];
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
    return list_ptr[LIST_TAIL_INDEX];
}

/**
 * @brief Allocate uninitialized memory for a list item
 *
 * @details Allocates a memory area that will be used to store a list item.
 * @param heap the heap to allocate memory in
 * @return a pointer to a newly allocated memory area.
 */
MALLOC_LIKE static inline term *term_list_alloc(Heap *heap)
{
    return memory_heap_alloc(heap, CONS_SIZE);
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
 * @param heap the heap to allocate memory in
 * @return a term pointing to the newly created list item.
 */
static inline term term_list_prepend(term head, term tail, Heap *heap)
{
    term *l = term_list_alloc(heap);
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

static inline int term_is_float(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        return (boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_FLOAT;

    } else {
        return 0;
    }
}

static inline term term_from_float(avm_float_t f, Heap *heap)
{
    term *boxed_value = memory_heap_alloc(heap, FLOAT_SIZE);
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

static inline int term_is_number(term t)
{
    return term_is_any_integer(t) || term_is_float(t);
}

/**
 * @brief Prints a term to stdout
 *
 * @details Print any term to the given file.
 * @param fd the file where the term will be printed.
 * @param t the term that will be printed.
 * @param ctx the context.
 */
void term_display(FILE *fd, term t, const Context *ctx);

/**
 * @brief Prints a term using given printer fun
 *
 * @details Print any given term using a printer fun
 * @param pf function that will handle printing.
 * @param t the term that will be printed.
 * @param global the \c GlobalContext.
 * @returns the number of printed characters.
 */
int term_funprint(PrinterFun *pf, term t, const GlobalContext *global);

/**
 * @brief Prints a term to the given file
 *
 * @details Print any given term to the given file.
 * @param fd the file where the term will be printed.
 * @param t the term that will be printed.
 * @param global the \c GlobalContext.
 * @returns the number of printed characters.
 */
int term_fprint(FILE *fd, term t, const GlobalContext *global);

/**
 * @brief Write a term to a string as text
 *
 * @details Print any given term to the given buffer.
 * @param buf the buffer where the term will be printed.
 * @param size the buffer size.
 * @param t the term that will be printed.
 * @param global the \c GlobalContext.
 * @returns the number of printed characters.
 */
int term_snprint(char *buf, size_t size, term t, const GlobalContext *global);

/**
 * @brief Checks if a term is a string (i.e., a list of characters)
 *
 * @details Returns 1 if a term is a proper (nil-terminated) list of characters
 * or an empty list (nil term), otherwise 0.
 * @param t the term that will be checked.
 * @return 1 if check succeeds, 0 otherwise.
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

static inline term term_make_function_reference(term m, term f, term a, Heap *heap)
{
    term *boxed_func = memory_heap_alloc(heap, FUNCTION_REFERENCE_SIZE);

    boxed_func[0] = ((FUNCTION_REFERENCE_SIZE - 1) << 6) | TERM_BOXED_FUN;
    boxed_func[1] = m;
    boxed_func[2] = f;
    boxed_func[3] = a;

    return ((term) boxed_func) | TERM_BOXED_VALUE_TAG;
}

static inline int term_is_match_state(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & 0x3F) == TERM_BOXED_BIN_MATCH_STATE) {
            return 1;
        }
    }

    return 0;
}

static inline term term_get_match_state_binary(term match_state)
{
    const term *boxed_value = term_to_const_term_ptr(match_state);
    return boxed_value[1];
}

static inline avm_int_t term_get_match_state_offset(term match_state)
{
    const term *boxed_value = term_to_const_term_ptr(match_state);
    return boxed_value[2];
}

static inline void term_set_match_state_offset(term match_state, avm_int_t offset)
{
    term *boxed_value = term_to_term_ptr(match_state);
    boxed_value[2] = offset;
}

static inline void term_match_state_save_offset(term match_state, int index)
{
    term *boxed_value = term_to_term_ptr(match_state);
    boxed_value[4 + index] = boxed_value[2];
}

static inline void term_match_state_save_start_offset(term match_state)
{
    term *boxed_value = term_to_term_ptr(match_state);
    boxed_value[3] = boxed_value[2];
}

static inline void term_match_state_restore_start_offset(term match_state)
{
    term *boxed_value = term_to_term_ptr(match_state);
    boxed_value[2] = boxed_value[3];
}

static inline void term_match_state_restore_offset(term match_state, int index)
{
    term *boxed_value = term_to_term_ptr(match_state);
    boxed_value[2] = boxed_value[4 + index];
}

static inline term term_alloc_bin_match_state(term binary_or_state, int slots, Heap *heap)
{
    term *boxed_match_state = memory_heap_alloc(heap, TERM_BOXED_BIN_MATCH_STATE_SIZE + slots);

    boxed_match_state[0] = (((TERM_BOXED_BIN_MATCH_STATE_SIZE + slots) - 1) << 6) | TERM_BOXED_BIN_MATCH_STATE;
    if (term_is_match_state(binary_or_state)) {
        boxed_match_state[1] = term_get_match_state_binary(binary_or_state);
        term offset = (term) term_get_match_state_offset(binary_or_state);
        boxed_match_state[2] = offset;
        // TODO: not sure about the following
        boxed_match_state[3] = offset;
        for (int i = 0; i < slots; i++) {
            boxed_match_state[4 + i] = offset;
        }
    } else {
        boxed_match_state[1] = binary_or_state;
        // TODO: initialize them with term_from_int(0)
        boxed_match_state[2] = 0;
        boxed_match_state[3] = 0;
        for (int i = 0; i < slots; i++) {
            boxed_match_state[4 + i] = 0;
        }
    }

    return ((term) boxed_match_state) | TERM_BOXED_VALUE_TAG;
}

static inline int term_is_map(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_MAP) {
            return 1;
        }
    }

    return 0;
}

static inline size_t term_get_map_keys_offset()
{
    return 1;
}

static inline size_t term_get_map_value_offset()
{
    return 2;
}

static inline size_t term_map_size_in_terms_maybe_shared(size_t num_entries, bool is_shared)
{
    return is_shared ? TERM_MAP_SHARED_SIZE(num_entries) : TERM_MAP_SIZE(num_entries);
}

static inline size_t term_map_size_in_terms(size_t num_entries)
{
    return TERM_MAP_SIZE(num_entries);
}

static inline term term_alloc_map_maybe_shared(avm_uint_t size, term keys, Heap *heap)
{
    keys = term_is_invalid_term(keys) ? term_alloc_tuple(size, heap) : keys;
    term *boxed_value = memory_heap_alloc(heap, 2 + size);
    boxed_value[0] = ((1 + size) << 6) | TERM_BOXED_MAP;
    boxed_value[term_get_map_keys_offset()] = keys;

    return ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
}

static inline term term_alloc_map(avm_uint_t size, Heap *heap)
{
    return term_alloc_map_maybe_shared(size, term_invalid_term(), heap);
}

static inline term term_get_map_keys(term t)
{
    TERM_DEBUG_ASSERT(term_is_map(t));
    const term *boxed_value = term_to_const_term_ptr(t);
    return boxed_value[term_get_map_keys_offset()];
}

static inline int term_get_map_size(term t)
{
    TERM_DEBUG_ASSERT(term_is_map(t));

    return term_get_tuple_arity(term_get_map_keys(t));
}

static inline void term_set_map_assoc(term map, avm_uint_t pos, term key, term value)
{
    term_put_tuple_element(term_get_map_keys(map), pos, key);
    term *boxed_value = term_to_term_ptr(map);
    boxed_value[term_get_map_value_offset() + pos] = value;
}

static inline term term_get_map_key(term map, avm_uint_t pos)
{
    return term_get_tuple_element(term_get_map_keys(map), pos);
}

static inline term term_get_map_value(term map, avm_uint_t pos)
{
    term *boxed_value = term_to_term_ptr(map);
    return boxed_value[term_get_map_value_offset() + pos];
}

static inline int term_find_map_pos(term map, term key, GlobalContext *global)
{
    term keys = term_get_map_keys(map);
    int arity = term_get_tuple_arity(keys);
    for (int i = 0; i < arity; ++i) {
        term k = term_get_tuple_element(keys, i);
        // TODO: not sure if exact is the right choice here
        TermCompareResult result = term_compare(key, k, TermCompareExact, global);
        if (result == TermEquals) {
            return i;
        } else if (UNLIKELY(result == TermCompareMemoryAllocFail)) {
            return TERM_MAP_MEMORY_ALLOC_FAIL;
        }
    }

    return TERM_MAP_NOT_FOUND;
}

term term_get_map_assoc(term map, term key, GlobalContext *glb);

static inline term term_get_map_assoc_default(term map, term key, term default_value, GlobalContext *glb)
{
    term ret = term_get_map_assoc(map, key, glb);
    if (term_is_invalid_term(ret)) {
        return default_value;
    }
    return ret;
}

static inline term term_get_sub_binary_ref(term t)
{
    const term *boxed_value = term_to_const_term_ptr(t);
    return boxed_value[3];
}

/**
 * @brief Create a resource on the heap.
 * @details This function creates a resource (obtained from `enif_alloc_resource`)
 * on the heap which must have `TERM_BOXED_RESOURCE_SIZE` free terms.
 *
 * Unlike `enif_make_resource`, this function doesn't increment the reference
 * counter but instead makes the heap own the resource. It will be garbage
 * collected when the heap is destroyed.
 *
 * @param resource resource obtained from `enif_alloc_resource`
 * @param heap the heap to allocate the resource in
 * @return a term pointing to the resource
 */
static inline term term_from_resource(void *resource, Heap *heap)
{
    // Resources are currently refc binaries with a size of 0 but may be
    // references in the future.
    struct RefcBinary *refc = refc_binary_from_data(resource);
    term *boxed_value = memory_heap_alloc(heap, TERM_BOXED_REFC_BINARY_SIZE);
    boxed_value[0] = ((TERM_BOXED_REFC_BINARY_SIZE - 1) << 6) | TERM_BOXED_REFC_BINARY;
    boxed_value[1] = (term) 0; // binary size, this is pre ERTS 9.0 (OTP-20.0) behavior
    boxed_value[2] = (term) RefcNoFlags;
    term ret = ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
    boxed_value[3] = (term) refc;
    // Add the resource to the mso list
    heap->root->mso_list = term_list_init_prepend(boxed_value + 4, ret, heap->root->mso_list);
    return ret;
}

#ifdef __cplusplus
}
#endif

#endif
