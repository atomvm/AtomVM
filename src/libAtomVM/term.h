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

// gcc-arm-none-eabi 13.2.1 with newlib requires this first
#include <sys/types.h>

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "atom.h"
#include "intn.h"
#include "memory.h"
#include "refc_binary.h"
#include "utils.h"

#include "term_typedef.h"

#ifdef __cplusplus
extern "C" {
#endif

// Remember to keep in sync with libs/jit/src/*term.hrl

#define COMPACT_LITERAL 0
#define COMPACT_INTEGER 1
#define COMPACT_ATOM 2
#define COMPACT_XREG 3
#define COMPACT_YREG 4
#define COMPACT_LABEL 5
#define COMPACT_EXTENDED 7
#define COMPACT_LARGE_LITERAL 8
#define COMPACT_LARGE_INTEGER 9
#define COMPACT_LARGE_ATOM 10
#define COMPACT_LARGE_XREG 11
#define COMPACT_LARGE_YREG 12

// OTP-20+ format
#define COMPACT_EXTENDED_LIST 0x17
#define COMPACT_EXTENDED_FP_REGISTER 0x27
#define COMPACT_EXTENDED_ALLOCATION_LIST 0x37
#define COMPACT_EXTENDED_LITERAL 0x47
// https://github.com/erlang/otp/blob/master/lib/compiler/src/beam_asm.erl#L433
#define COMPACT_EXTENDED_TYPED_REGISTER 0x57

#define COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_WORDS 0
#define COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FLOATS 1
#define COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FUNS 2

#define COMPACT_LARGE_IMM_MASK 0x18
#define COMPACT_11BITS_VALUE 0x8
#define COMPACT_NBITS_VALUE 0x18

#define TERM_PRIMARY_MASK 0x3
#define TERM_PRIMARY_CP 0x0
#define TERM_PRIMARY_LIST 0x1
#define TERM_PRIMARY_BOXED 0x2
#define TERM_PRIMARY_IMMED 0x3

#define TERM_BOXED_VALUE_TAG _Pragma ("TERM_BOXED_VALUE_TAG is deprecated, use TERM_PRIMARY_BOXED instead") TERM_PRIMARY_BOXED

#define TERM_IMMED_TAG_MASK 0xF
#define TERM_PID_TAG 0x3
#define TERM_PORT_TAG 0x7
#define TERM_INTEGER_TAG 0xF
#define TERM_IMMED2_TAG 0xB

#define TERM_BOXED_TAG_MASK 0x3F
#define TERM_BOXED_TUPLE 0x0
#define TERM_BOXED_BIN_MATCH_STATE 0x4
#define TERM_BOXED_POSITIVE_INTEGER 0x8 // b1000 (b1s00)
#define TERM_BOXED_NEGATIVE_INTEGER (TERM_BOXED_POSITIVE_INTEGER | TERM_BOXED_INTEGER_SIGN_BIT)
#define TERM_BOXED_REF 0x10
#define TERM_BOXED_FUN 0x14
#define TERM_BOXED_FLOAT 0x18
// Do not assign 0x1C: an optimization in libs/jit/src/term.hrl will misidentify this as boxed
// number: define(TERM_BOXED_TAG_MASK_INTEGER_OR_FLOAT, 16#2B).
#define TERM_BOXED_REFC_BINARY 0x20
#define TERM_BOXED_HEAP_BINARY 0x24
#define TERM_BOXED_SUB_BINARY 0x28
#define TERM_BOXED_MAP 0x2C
#define TERM_BOXED_EXTERNAL_THING 0x30
#define TERM_BOXED_EXTERNAL_PID 0x30
#define TERM_BOXED_EXTERNAL_PORT 0x34
#define TERM_BOXED_EXTERNAL_REF 0x38

#define TERM_BOXED_INTEGER_SIGN_BIT_POS 2 // 3rd bit
#define TERM_BOXED_INTEGER_SIGN_BIT (1 << TERM_BOXED_INTEGER_SIGN_BIT_POS)

#define TERM_IMMED2_TAG_MASK 0x3F
#define TERM_IMMED2_TAG_SIZE 6
#define TERM_IMMED2_ATOM 0xB
#define TERM_IMMED2_CATCH 0x1B
#define TERM_NIL 0x3B

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

#define TERM_MAX_LOCAL_PROCESS_ID ((1 << 28) - 1)

#define BINARY_HEADER_SIZE 2
#define FUNCTION_REFERENCE_SIZE 4
#define BOXED_INT_SIZE (BOXED_TERMS_REQUIRED_FOR_INT + 1)
#define BOXED_INT64_SIZE (BOXED_TERMS_REQUIRED_FOR_INT64 + 1)
#define BOXED_INTN_SIZE(term_size) ((term_size) + 1)
#define BOXED_FUN_SIZE 3
#define FLOAT_SIZE (sizeof(float_term_t) / sizeof(term) + 1)
#define REF_SIZE ((int) ((sizeof(uint64_t) / sizeof(term)) + 1))
#if TERM_BYTES == 8
    #define EXTERNAL_PID_SIZE 3
#elif TERM_BYTES == 4
    #define EXTERNAL_PID_SIZE 5
#else
    #error
#endif
#define EXTERNAL_PORT_SIZE EXTERNAL_PID_SIZE
#if TERM_BYTES == 8
    #define EXTERNAL_REF_SIZE(words) (3 + (words / 2))
#elif TERM_BYTES == 4
    #define EXTERNAL_REF_SIZE(words) (3 + words)
#else
    #error
#endif
#define TUPLE_SIZE(elems) ((int) (elems + 1))
#define CONS_SIZE 2
#define REFC_BINARY_CONS_OFFSET 4
#define LIST_SIZE(num_elements, element_size) ((num_elements) * ((element_size) + CONS_SIZE))
#define TERM_STRING_SIZE(length) (2 * (length))
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

#define TERM_FROM_ATOM_INDEX(atom_index) ((atom_index << TERM_IMMED2_TAG_SIZE) | TERM_IMMED2_ATOM)

// Local ref is at most 30 bytes:
// 2^32-1 = 4294967295 (10 chars)
// "#Ref<0." "." ">\0" (10 chars)
// External ref is at most 70 bytes:
// 2^26-1 = 67108863 (8 chars) (node, atom index)
// 2^32-1 = 4294967295 (10 chars)
// "#Ref<" "." "." "." "." "." ">\0" (12 chars)
#define REF_AS_CSTRING_LEN 70

// Local pid is at most 16 bytes:
// 2^28-1 = 268435455 (9 chars)
// "<0." ".0>\0" (7 chars)
// External pid is at most 32 bytes:
// 2^26-1 = 67108863 (8 chars) (node, atom index)
// 2^28-1 = 268435455 (9 chars) (pid number)
// 2^32-1 = 4294967295 (10 chars) (pid serial)
// "<" "." "." ">\0" (5 chars)
#define PID_AS_CSTRING_LEN 32

// Local port is at most 19 bytes:
// 2^28-1 = 268435455 (9 chars)
// "#Port<0." ">\0" (10 chars)
// External port is at most 37 bytes:
// 2^26-1 = 67108863 (8 chars) (node, atom index)
// 2^64-1 = 18446744073709551615 (20 chars)
// "#Port<" "." ">\0" (9 chars)
#define PORT_AS_CSTRING_LEN 37

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

typedef struct BinaryPosLen
{
    avm_int_t pos;
    avm_int_t len;
} BinaryPosLen;

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

typedef enum
{
    TermPositiveInteger = 0,
    TermNegativeInteger = TERM_BOXED_INTEGER_SIGN_BIT
} term_integer_sign_t;

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
 * @details Returns \c true if a term is an atom, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_atom(term t)
{
    /* atom: | atom index | 00 10 11 */
    return ((t & TERM_IMMED2_TAG_MASK) == TERM_IMMED2_ATOM);
}

/**
 * @brief Check if a term is an invalid term
 *
 * @details Returns \c true if a term is an invalid term, otherwise \c false is returned.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_invalid_term(term t)
{
    return (t == 0);
}

/**
 * @brief Checks if a term is nil
 *
 * @details Returns \c true if a term is nil, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_nil(term t)
{
    /* nil: 11 10 11 */
    return ((t & TERM_IMMED2_TAG_MASK) == TERM_NIL);
}

/**
 * @brief Checks if a term is a non empty list
 *
 * @details Returns \c true if a term is a non empty list (cons), otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_nonempty_list(term t)
{
    /* list: 01 */
    return ((t & TERM_PRIMARY_MASK) == TERM_PRIMARY_LIST);
}

/**
 * @brief Checks if a term is a list
 *
 * @details Returns \c true if a term is a list (cons) or an empty list (nil term), otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_list(term t)
{
    /* list: 01 */
    return term_is_nonempty_list(t) || term_is_nil(t);
}

/**
 * @brief Checks if a term is a boxed value
 *
 * @details Returns \c true if a term is a boxed value stored on the heap, such as a tuple, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_boxed(term t)
{
    /* boxed: 10 */
    return ((t & TERM_PRIMARY_MASK) == TERM_PRIMARY_BOXED);
}

/**
 * @brief Returns size of a boxed term from its header
 *
 * @details Returns the size that is stored in boxed term header most significant bits for variable size boxed terms.
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
    TERM_DEBUG_ASSERT((t & TERM_PRIMARY_MASK) == TERM_PRIMARY_BOXED);

    const term *boxed_value = term_to_const_term_ptr(t);
    return term_get_size_from_boxed_header(*boxed_value);
}

/**
 * @brief Checks if a term is a binary
 *
 * @details Returns \c true if a term is a binary stored on the heap, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_binary(term t)
{
    /* boxed: 10 */
    if ((t & TERM_PRIMARY_MASK) == TERM_PRIMARY_BOXED) {
        const term *boxed_value = term_to_const_term_ptr(t);
        int masked_value = boxed_value[0] & TERM_BOXED_TAG_MASK;
        switch (masked_value) {
            case TERM_BOXED_REFC_BINARY:
            case TERM_BOXED_HEAP_BINARY:
            case TERM_BOXED_SUB_BINARY:
                return true;
            default:
                return false;
        }
    }

    return false;
}

/**
 * @brief Checks if a term is a refc binary
 *
 * @details Returns \c true if a term is a ref-counted binary, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
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

/**
 * @brief Checks if a term is a heap binary
 *
 * @details Returns \c true if a term is a binary stored on the heap, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_heap_binary(term t)
{
    /* boxed: 10 */
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        int masked_value = boxed_value[0] & TERM_BOXED_TAG_MASK;
        return masked_value == TERM_BOXED_HEAP_BINARY;
    }

    return false;
}

static inline bool term_refc_binary_is_const(term t)
{
    const term *boxed_value = term_to_const_term_ptr(t);
    return (boxed_value[2] & RefcBinaryIsConst) != 0;
}

/**
 * @brief Checks if a term is a sub-binary
 *
 * @details Returns \c true if a term is a sub-binary; \c false, otherwise.
 * @param t the term that will be checked.
 * @return \c true if check succeeds; \c false, otherwise.
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
 * @brief Check if term is an integer within platform-specific \c avm_int_t range
 *
 * Tests whether a term represents an integer stored directly in the term
 * word without boxing. Returns true only for integers that fit within the
 * platform's unboxed integer range:
 * - 32-bit builds: [-2^28, 2^28 - 1] (28-bit signed)
 * - 64-bit builds: [-2^60, 2^60 - 1] (60-bit signed)
 *
 * Integers outside these ranges are stored as boxed integers on the heap
 * and will return false from this function.
 *
 * @param t Term to check
 * @return true if term is an unboxed integer, false otherwise
 *
 * @note Returns false for boxed integers and big integers, even if their
 *       values would fit in \c avm_int_t full range
 * @note Values passing this check can be safely converted to \c avm_int_t
 *       or \c size_t using \c term_to_int()
 * @note Terms for which this functions returns true are not moved during
 *       garbage collection
 * @warning Values passing this check may NOT fit in \c int on platforms
 *          where \c int is smaller than \c avm_int_t
 *
 * @see term_is_boxed_integer() for boxed integer checking
 * @see term_is_any_integer() for checking all integer representations
 * @see term_to_int() for extracting the integer value
 */
static inline bool term_is_int(term t)
{
    /* integer: 11 11 */
    return ((t & TERM_IMMED_TAG_MASK) == TERM_INTEGER_TAG);
}

/**
 * @brief Check if term is an integer within platform-specific \c avm_int_t range
 *
 * @deprecated Use \c term_is_int() instead. This function will raise a warning
 *             in the future and will eventually be removed.
 *
 * @param t Term to check
 * @return true if term is an unboxed integer, false otherwise
 *
 * @see term_is_int() for the replacement function
 */
static inline bool term_is_integer(term t)
{
    return term_is_int(t);
}

/**
 * @brief Checks if a term is a uint8_t
 *
 * @details Returns \c true if a term is an integer value in the [0, 255] range, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_uint8(term t)
{
    return ((t & ~((term) 0xFF0)) == TERM_INTEGER_TAG);
}

static inline bool term_is_boxed_integer(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if (((boxed_value[0] & TERM_BOXED_TAG_MASK) | TERM_BOXED_INTEGER_SIGN_BIT)
            == TERM_BOXED_NEGATIVE_INTEGER) {
            return true;
        }
    }

    return false;
}

static inline bool term_is_any_integer(term t)
{
    return term_is_integer(t) || term_is_boxed_integer(t);
}

static inline bool term_is_catch_label(term t)
{
    return (t & TERM_IMMED2_TAG_MASK) == TERM_IMMED2_CATCH;
}

/**
 * @brief Checks if a term is a local pid
 *
 * @details Returns \c true if a term is a process id, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_local_pid(term t)
{
    /* integer: 00 11 */
    return ((t & TERM_IMMED_TAG_MASK) == TERM_PID_TAG);
}

/**
 * @brief Checks if a term is an external pid
 *
 * @details Returns \c true if a term is an external process id, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_external_pid(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_EXTERNAL_PID) {
            return true;
        }
    }

    return false;
}

/**
 * @brief Checks if a term is an external thing
 *
 * @details Returns \c true if a term is an external thing, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_external(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & 0x33) == TERM_BOXED_EXTERNAL_THING) {
            return true;
        }
    }

    return false;
}

/**
 * @brief Checks if a term is a pid
 *
 * @details Returns \c true if a term is a process id, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_pid(term t)
{
    return term_is_local_pid(t) || term_is_external_pid(t);
}

/**
 * @brief Checks if a term is a local port
 *
 * @details Returns \c true if a term is a local port, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_local_port(term t)
{
    /* integer: 01 11 */
    return ((t & TERM_IMMED_TAG_MASK) == TERM_PORT_TAG);
}

/**
 * @brief Checks if a term is an external port
 *
 * @details Returns \c true if a term is an external port, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_external_port(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_EXTERNAL_PORT) {
            return true;
        }
    }

    return false;
}

/**
 * @brief Checks if a term is a port
 *
 * @details Returns \c true if a term is a port, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_port(term t)
{
    return term_is_local_port(t) || term_is_external_port(t);
}

/**
 * @brief Checks if a term is a local port or a local pid
 *
 * @details Returns \c true if a term is a local port or a local process id, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_local_pid_or_port(term t)
{
    return term_is_local_pid(t) || term_is_local_port(t);
}

/**
 * @brief Checks if a term is a tuple
 *
 * @details Returns \c true if a term is a tuple, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_tuple(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_TUPLE) {
            return true;
        }
    }

    return false;
}

/**
 * @brief Checks if a term is a local reference
 *
 * @details Returns \c true if a term is a local reference, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_local_reference(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & 0x3F) == TERM_BOXED_REF) {
            return true;
        }
    }

    return false;
}

/**
 * @brief Checks if a term is an external reference
 *
 * @details Returns \c true if a term is a local reference, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_external_reference(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & 0x3F) == TERM_BOXED_EXTERNAL_REF) {
            return true;
        }
    }

    return false;
}

/**
 * @brief Checks if a term is a reference
 *
 * @details Returns \c true if a term is a reference, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_reference(term t)
{
    return term_is_local_reference(t) || term_is_external_reference(t);
}

/**
 * @brief Checks if a term is a fun
 *
 * @details Returns \c true if a term is a fun, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 * @deprecated renamed to term_is_fun.
 */
static inline bool term_is_function(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_FUN) {
            return true;
        }
    }

    return false;
}

/**
 * @brief Checks if a term is a fun
 *
 * @details Returns true if a term is a fun, otherwise false.
 * @param t the term that will be checked.
 * @return true if check succeeds, \c false otherwise.
 */

static inline bool term_is_fun(term t)
{
    return term_is_function(t) != 0;
}

/**
 * @brief Checks if a term is an external fun
 *
 * @details Returns true if a term is an external fun such as "fun m:f/a", otherwise false.
 * @param t the term that will be checked.
 * @return true if check succeeds, \c false otherwise.
 */
static inline bool term_is_external_fun(term t)
{
    if (term_is_fun(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        term index_or_function = boxed_value[2];
        if (term_is_atom(index_or_function)) {
            return true;
        }
    }

    return false;
}

/**
 * @brief Checks if a term is a saved CP
 *
 * @details Returns \c true if a term is a saved continuation pointer, otherwise \c false.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_cp(term t)
{
    return ((t & TERM_PRIMARY_MASK) == TERM_PRIMARY_CP);
}

/**
 * @brief Gets invalid term
 *
 * @details Returns always an invalid term.
 * @return invalid term.
 */
static inline term term_invalid_term(void)
{
    return 0;
}

/**
 * @brief Gets nil value
 *
 * @details Returns always the nil value.
 * @return nil value term.
 */
static inline term term_nil(void)
{
    return TERM_NIL;
}

/**
 * @brief Gets global atom table index
 *
 * @details Returns atom table index for given atom term.
 * @param t the term that will be converted to atom table index. t must be a valid atom term.
 * @return a global atom table index.
 */
static inline atom_index_t term_to_atom_index(term t)
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
static inline term term_from_atom_index(atom_index_t atom_index)
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

/**
 * @brief Extract \c avm_int_t value from unboxed integer term
 *
 * Extracts the \c avm_int_t value from a term that contains an unboxed
 * integer. An unboxed integer is an integer value stored directly within
 * the term itself, not as a separate allocation on the heap.
 *
 * @param t Term containing unboxed integer
 * @return The extracted \c avm_int_t value
 *
 * @pre \c term_is_int(t) must be true
 * @warning Undefined behavior if called on non-integer or boxed integer terms
 *
 * @note This function performs no type checking - validation must be done
 *       by caller using \c term_is_int()
 * @note Only extracts from unboxed integers (28-bit on 32-bit builds,
 *       60-bit on 64-bit builds)
 * @note Safe conversions: \c size_t s = term_to_int(t) is always valid
 * @warning Unsafe conversions on 64-bit builds: \c int or \c int32_t may overflow
 *          since \c avm_int_t can hold 60-bit values
 *
 * @see term_is_int() to validate term before extraction
 * @see term_unbox_int() for extracting boxed integers
 * @see term_maybe_unbox_int() for extracting from either unboxed or boxed integers
 */
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
 * @brief Gets process table index for a local pid or port
 *
 * @details Returns local process table index for given atom term.
 * @param t the term that will be converted to local process table index, term type is checked.
 * @return a local process table index.
 */
static inline int32_t term_to_local_process_id(term t)
{
    TERM_DEBUG_ASSERT(term_is_local_pid_or_port(t));

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
    return (value << 4) | TERM_INTEGER_TAG;
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
    return (value << 4) | TERM_INTEGER_TAG;
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
        return (value << 4) | TERM_INTEGER_TAG;
    }

#elif TERM_BITS == 64
    return (value << 4) | TERM_INTEGER_TAG;

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
        return (value << 4) | TERM_INTEGER_TAG;
    }

#elif TERM_BITS == 64
    // 1152921504606846975 = 0x0FFFFFFFFFFFFFFF
    if (UNLIKELY((value > 1152921504606846975) || (value < -1152921504606846975))) {
        //TODO: unimplemented on heap integer value
        fprintf(stderr, "unimplemented: term should be moved to heap.");
        AVM_ABORT();

    } else {
        return (value << 4) | TERM_INTEGER_TAG;
    }

#else
    #error "Wrong TERM_BITS define"
#endif
}

static inline term term_from_int(avm_int_t value)
{
    return (value << 4) | TERM_INTEGER_TAG;
}

/**
 * @brief Check if term is a non-negative unboxed integer
 *
 * @param t Term to check
 * @return true if term is an unboxed integer >= 0, false otherwise
 *
 * @see term_is_int() for unboxed integer details
 */
static inline bool term_is_non_neg_int(term t)
{
    if (term_is_int(t)) {
        avm_int_t v = term_to_int(t);
        return v >= 0;
    }
    return false;
}

/**
 * @brief Check if term is a positive (non-zero) unboxed integer
 *
 * @param t Term to check
 * @return true if term is an unboxed integer > 0, false otherwise
 *
 * @see term_is_int() for unboxed integer details
 */
static inline bool term_is_pos_int(term t)
{
    if (term_is_int(t)) {
        avm_int_t v = term_to_int(t);
        return v > 0;
    }

    return false;
}

/**
 * @brief Check if term is a negative unboxed integer
 *
 * @param t Term to check
 * @return true if term is an unboxed integer < 0, false otherwise
 *
 * @see term_is_int() for unboxed integer details
 */
static inline bool term_is_neg_int(term t)
{
    if (term_is_int(t)) {
        avm_int_t v = term_to_int(t);
        return v < 0;
    }

    return false;
}

static inline bool term_is_pos_boxed_integer(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        return ((boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_POSITIVE_INTEGER);
    }

    return false;
}

static inline bool term_is_neg_boxed_integer(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        return ((boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_NEGATIVE_INTEGER);
    }

    return false;
}

static inline term_integer_sign_t term_boxed_integer_sign(term t)
{
    const term *boxed_value = term_to_const_term_ptr(t);
    return (term_integer_sign_t) (boxed_value[0] & TERM_BOXED_INTEGER_SIGN_BIT);
}

static inline bool term_is_any_non_neg_integer(term t)
{
    return term_is_non_neg_int(t) || term_is_pos_boxed_integer(t);
}

static inline bool term_is_any_pos_integer(term t)
{
    return term_is_pos_int(t) || term_is_pos_boxed_integer(t);
}

static inline bool term_is_any_neg_integer(term t)
{
    return term_is_neg_int(t) || term_is_neg_boxed_integer(t);
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

static inline term_integer_sign_t term_integer_sign_from_int(avm_int_t value)
{
    avm_uint_t uvalue = ((avm_uint_t) value);
    return (term_integer_sign_t) ((uvalue >> (TERM_BITS - 1)) << TERM_BOXED_INTEGER_SIGN_BIT_POS);
}

static inline term term_make_boxed_int(avm_int_t value, Heap *heap)
{
    avm_uint_t sign = (avm_uint_t) term_integer_sign_from_int(value);
    term *boxed_int = memory_heap_alloc(heap, 1 + BOXED_TERMS_REQUIRED_FOR_INT);
    boxed_int[0] = (BOXED_TERMS_REQUIRED_FOR_INT << 6) | TERM_BOXED_POSITIVE_INTEGER | sign;
    boxed_int[1] = value;
    return ((term) boxed_int) | TERM_PRIMARY_BOXED;
}

static inline term term_make_boxed_int64(avm_int64_t large_int64, Heap *heap)
{
    avm_uint64_t sign = (((avm_uint64_t) large_int64) >> 63) << TERM_BOXED_INTEGER_SIGN_BIT_POS;
    term *boxed_int = memory_heap_alloc(heap, 1 + BOXED_TERMS_REQUIRED_FOR_INT64);
    boxed_int[0] = (BOXED_TERMS_REQUIRED_FOR_INT64 << 6) | TERM_BOXED_POSITIVE_INTEGER | sign;
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
    return ((term) boxed_int) | TERM_PRIMARY_BOXED;
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

static inline term term_create_uninitialized_intn(size_t n, term_integer_sign_t sign, Heap *heap)
{
    term *boxed_int = memory_heap_alloc(heap, 1 + n);
    boxed_int[0] = (n << 6) | TERM_BOXED_POSITIVE_INTEGER | sign;

    return ((term) boxed_int) | TERM_PRIMARY_BOXED;
}

static inline void *term_intn_data(term t)
{
    const term *boxed_value = term_to_const_term_ptr(t);
    return (void *) (boxed_value + 1);
}

static inline void term_intn_to_term_size(size_t n, size_t *intn_data_size, size_t *rounded_num_len)
{
    size_t bytes = n * sizeof(intn_digit_t);
    size_t rounded = ((bytes + 7) >> 3) << 3;
    *intn_data_size = rounded / sizeof(term);

    if (*intn_data_size == BOXED_TERMS_REQUIRED_FOR_INT64) {
        // we need to distinguish between "small" boxed integers, that are integers
        // up to int64, and bigger integers.
        // The real difference is that "small" boxed integers use 2-complement,
        // real bigints not (and also endianess might differ).
        // So we force real bigints to be > BOXED_TERMS_REQUIRED_FOR_INT64 terms
        *intn_data_size = BOXED_TERMS_REQUIRED_FOR_INT64 + 1;
        rounded = *intn_data_size * sizeof(term);
    }

    *rounded_num_len = rounded / sizeof(intn_digit_t);
}

/**
 * @brief Check if term is a multi-precision integer larger than \c int64_t
 *
 * Tests whether a term represents a boxed integer that requires multi-precision
 * representation (i.e., larger than can fit in \c int64_t). These are integers
 * that need more than \c INTN_INT64_LEN digits for their representation.
 *
 * In the current implementation, a bigint is defined as a boxed integer with
 * size greater than:
 * - \c BOXED_TERMS_REQUIRED_FOR_INT64 on 32-bit systems
 * - \c BOXED_TERMS_REQUIRED_FOR_INT on 64-bit systems
 *
 * This effectively identifies integers that cannot be represented in the
 * platform's native integer types and require multi-precision arithmetic,
 * while avoiding confusion with regular boxed \c int64_t values that still
 * fit within standard integer ranges.
 *
 * @param t Term to check
 * @return true if term is a multi-precision integer, false otherwise
 *
 * @note Returns false for integers that fit in \c int64_t, even if boxed
 * @note This is the correct check before calling \c term_to_bigint()
 *
 * @see term_to_bigint() to extract the multi-precision integer data
 * @see term_is_boxed_integer() for checking any boxed integer
 * @see term_is_any_integer() for checking all integer representations
 */
static inline bool term_is_bigint(term t)
{
    return term_is_boxed_integer(t)
        && (term_boxed_size(t) > (INTN_INT64_LEN * sizeof(intn_digit_t)) / sizeof(term));
}

// intn doesn't depend on term
_Static_assert(
    (int) TermPositiveInteger == (int) IntNPositiveInteger, "term/intn definition mismatch");
_Static_assert(
    (int) TermNegativeInteger == (int) IntNNegativeInteger, "term/intn definition mismatch");

/**
 * @brief Extract multi-precision integer data from boxed term
 *
 * Extracts the raw multi-precision integer representation from a boxed
 * integer term. This function provides direct access to the internal
 * digit array without copying, returning a pointer to the data within
 * the term structure.
 *
 * @param t Boxed integer term to extract from
 * @param[out] bigint Pointer to the digit array within the term (borrowed reference)
 * @param[out] bigint_len Number of digits in the integer
 * @param[out] bigint_sign Sign of the integer
 *
 * @pre \c term_is_bigint(t) must be true
 * @pre bigint != NULL
 * @pre bigint_len != NULL
 * @pre bigint_sign != NULL
 *
 * @warning Returned pointer is a borrowed reference into the term structure
 * @warning Data becomes invalid if term is garbage collected or modified
 * @warning Caller must not free the returned pointer
 *
 * @note The digit array may not be normalized (may have leading zeros)
 * @note Length is calculated as boxed_size * (sizeof(term) / sizeof(intn_digit_t))
 *
 * @see term_is_bigint() to check if term is a multi-precision integer
 * @see term_boxed_integer_sign() to get the sign
 */
static inline void term_to_bigint(
    term t, const intn_digit_t *bigint[], size_t *bigint_len, intn_integer_sign_t *bigint_sign)
{
    *bigint = (const intn_digit_t *) term_intn_data(t);

    const term *boxed_value = term_to_const_term_ptr(t);
    size_t boxed_size = term_get_size_from_boxed_header(boxed_value[0]);
    *bigint_len = boxed_size * (sizeof(term) / sizeof(intn_digit_t));

    *bigint_sign = (intn_integer_sign_t) term_boxed_integer_sign(t);
}

static inline term term_from_catch_label(unsigned int module_index, unsigned int label)
{
    return (term) ((module_index << 24) | (label << 6) | TERM_IMMED2_CATCH);
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
    return (local_process_id << 4) | TERM_PID_TAG;
}

/**
 * @brief Port term from local process id
 *
 * @details Returns a term for a given local process table index.
 * @param local_process_id the local process table index that will be converted to a term.
 * @return a term that encapsulates a PID.
 */
static inline term term_port_from_local_process_id(uint32_t local_process_id)
{
    return (local_process_id << 4) | TERM_PORT_TAG;
}

/**
 * @brief Determine whether a binary should be a heap binary or not
 *
 * @details Returns \c true if a binary of the specified size should be allocated
 *          in the process heap (as opposed to being a refc binary)
 * @param size the intended binary size
 * @return \c true if the binary should be allocated in the process heap; \c false, otherwise.
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
static inline struct RefcBinary *term_refc_binary_ptr(term refc_binary)
{
    TERM_DEBUG_ASSERT(term_is_refc_binary(refc_binary));

    term *boxed_value = term_to_term_ptr(refc_binary);
    return (struct RefcBinary *) boxed_value[3];
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

        return ((term) boxed_value) | TERM_PRIMARY_BOXED;
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
    if ((term_is_refc_binary(binary) || term_is_sub_binary(binary)) && len >= SUB_BINARY_MIN) {
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

static inline bool term_normalize_binary_pos_len(term binary, avm_int_t pos, avm_int_t len, BinaryPosLen *pos_len)
{
    avm_int_t size = (avm_int_t) term_binary_size(binary);
    if (len < 0) {
        pos += len;
        len = -len;
    }

    if (UNLIKELY((pos < 0) || (pos > size) || (pos + len > size))) {
        return false;
    }

    pos_len->pos = pos;
    pos_len->len = len;
    return true;
}

static inline bool term_is_nomatch_binary_pos_len(BinaryPosLen pos_len)
{
    return pos_len.pos == -1 && pos_len.len == -1;
}

static inline BinaryPosLen term_nomatch_binary_pos_len(void)
{
    return (BinaryPosLen) { .pos = -1, .len = -1 };
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

    return ((term) boxed_value) | TERM_PRIMARY_BOXED;
}

static inline uint64_t term_to_ref_ticks(term rt)
{
    TERM_DEBUG_ASSERT(term_is_local_reference(rt));

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
 * @brief Make an external pid term from node, process_id, serial and creation
 *
 * @param node name of the node (atom)
 * @param process_id process id on that node
 * @param serial serial of process id on that node
 * @param creation creation of that node
 * @param heap the heap to allocate memory in
 * @return an external heap term created using given parameters.
 */
static inline term term_make_external_process_id(term node, uint32_t process_id, uint32_t serial, uint32_t creation, Heap *heap)
{
    TERM_DEBUG_ASSERT(term_is_atom(node));

    term *boxed_value = memory_heap_alloc(heap, EXTERNAL_PID_SIZE);
    int atom_index = term_to_atom_index(node);
    boxed_value[0] = ((EXTERNAL_PID_SIZE - 1) << 6) | TERM_BOXED_EXTERNAL_PID;
    uint32_t *external_thing_words = (uint32_t *) &boxed_value[1];

    external_thing_words[0] = atom_index;
    external_thing_words[1] = creation;
    external_thing_words[2] = process_id;
    external_thing_words[3] = serial;

    return ((term) boxed_value) | TERM_PRIMARY_BOXED;
}

/**
 * @brief Get a port term from node, number and creation
 *
 * @param node name of the node (atom)
 * @param number port number on that node
 * @param creation creation of that node
 * @param heap the heap to allocate memory in
 * @return an external heap term created using given parameters.
 */
static inline term term_make_external_port_number(term node, uint64_t number, uint32_t creation, Heap *heap)
{
    term *boxed_value = memory_heap_alloc(heap, EXTERNAL_PORT_SIZE);
    int atom_index = term_to_atom_index(node);
    boxed_value[0] = ((EXTERNAL_PORT_SIZE - 1) << 6) | TERM_BOXED_EXTERNAL_PORT;
    uint32_t *external_thing_words = (uint32_t *) &boxed_value[1];

    external_thing_words[0] = atom_index;
    external_thing_words[1] = creation;
    external_thing_words[2] = number >> 32;
    external_thing_words[3] = (uint32_t) number;

    return ((term) boxed_value) | TERM_PRIMARY_BOXED;
}

/**
 * @brief Get the name of a node for a given external thing
 *
 * @param term external term
 * @return the name of the node
 */
static inline term term_get_external_node(term t)
{
    TERM_DEBUG_ASSERT(term_is_external(t));

    const term *boxed_value = term_to_const_term_ptr(t);
    const uint32_t *external_thing_words = (const uint32_t *) &boxed_value[1];
    return term_from_atom_index(external_thing_words[0]);
}

/**
 * @brief Get the creation for a given external thing
 *
 * @param term external term
 * @return the serial of the external pid
 */
static inline uint32_t term_get_external_node_creation(term t)
{
    TERM_DEBUG_ASSERT(term_is_external(t));

    const term *boxed_value = term_to_const_term_ptr(t);
    const uint32_t *external_thing_words = (const uint32_t *) &boxed_value[1];
    return external_thing_words[1];
}

/**
 * @brief Get the process id of an external pid
 *
 * @param term external pid
 * @return the process id of the external pid
 */
static inline uint32_t term_get_external_pid_process_id(term t)
{
    TERM_DEBUG_ASSERT(term_is_external_pid(t));

    const term *boxed_value = term_to_const_term_ptr(t);
    const uint32_t *external_thing_words = (const uint32_t *) &boxed_value[1];
    return external_thing_words[2];
}

/**
 * @brief Get the serial of an external pid
 *
 * @param term external term
 * @return the serial of the external pid
 */
static inline uint32_t term_get_external_pid_serial(term t)
{
    TERM_DEBUG_ASSERT(term_is_external_pid(t));

    const term *boxed_value = term_to_const_term_ptr(t);
    const uint32_t *external_thing_words = (const uint32_t *) &boxed_value[1];
    return external_thing_words[3];
}

/**
 * @brief Get the port number of an external port
 *
 * @param term external port
 * @return the port number of the external port
 */
static inline uint64_t term_get_external_port_number(term t)
{
    TERM_DEBUG_ASSERT(term_is_external_port(t));

    const term *boxed_value = term_to_const_term_ptr(t);
    const uint32_t *external_thing_words = (const uint32_t *) &boxed_value[1];
    return (((uint64_t) external_thing_words[2]) << 32) | (uint64_t) external_thing_words[3];
}

/**
 * @brief Make an external reference term from node, creation, number of words and words
 *
 * @param node name of the node (atom)
 * @param len number of words (1..5)
 * @param data words
 * @param creation creation of that node
 * @param heap the heap to allocate memory in
 * @return an external heap term created using given parameters.
 */
static inline term term_make_external_reference(term node, uint16_t len, uint32_t *data, uint32_t creation, Heap *heap)
{
    TERM_DEBUG_ASSERT(term_is_atom(node));

    term *boxed_value = memory_heap_alloc(heap, EXTERNAL_REF_SIZE(len));
    int atom_index = term_to_atom_index(node);
    boxed_value[0] = ((EXTERNAL_REF_SIZE(len) - 1) << 6) | TERM_BOXED_EXTERNAL_REF;
    uint32_t *external_thing_words = (uint32_t *) &boxed_value[1];

    #if TERM_BYTES == 8
        external_thing_words[0] = atom_index;
        external_thing_words[1] = creation;
        external_thing_words[2] = len;
        for (int i = 0; i < len; i++) {
            external_thing_words[3 + i] = data[i];
        }

    #elif TERM_BYTES == 4
        external_thing_words[0] = atom_index;
        external_thing_words[1] = creation;
        for (int i = 0; i < len; i++) {
            external_thing_words[2 + i] = data[i];
        }

    #else
        #error "terms must be either 32 or 64 bit wide"
    #endif

    return ((term) boxed_value) | TERM_PRIMARY_BOXED;
}

/**
 * @brief Get the number of words of an external reference
 *
 * @param term external term
 * @return the number of words of the external reference (from 1 to 5)
 */
static inline uint32_t term_get_external_reference_len(term t)
{
    TERM_DEBUG_ASSERT(term_is_external_reference(t));

    const term *boxed_value = term_to_const_term_ptr(t);

    #if TERM_BYTES == 8
        const uint32_t *external_thing_words = (const uint32_t *) &boxed_value[1];
        return (uint32_t) external_thing_words[2];

    #elif TERM_BYTES == 4
        return (uint32_t) (boxed_value[0] >> 6) - 2;

    #else
        #error "terms must be either 32 or 64 bit wide"
    #endif
}

/**
 * @brief Get the words of an external reference
 *
 * @param term external term
 * @return a pointer to (len) words of the external reference
 */
static inline const uint32_t *term_get_external_reference_words(term t)
{
    TERM_DEBUG_ASSERT(term_is_external_reference(t));

    const term *boxed_value = term_to_const_term_ptr(t);
    const uint32_t *external_thing_words = (const uint32_t *) &boxed_value[1];

    #if TERM_BYTES == 8
        return external_thing_words + 3;

    #elif TERM_BYTES == 4
        return external_thing_words + 2;

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

    return ((term) boxed_value) | TERM_PRIMARY_BOXED;
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

    TERM_DEBUG_ASSERT((size_t) elem_index < term_get_size_from_boxed_header(boxed_value[0]));

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

    TERM_DEBUG_ASSERT((size_t) elem_index < term_get_size_from_boxed_header(boxed_value[0]));

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
    list_cells[size * 2 - 2] = TERM_NIL;

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

static inline bool term_is_float(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        return (boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_FLOAT;

    } else {
        return false;
    }
}

static inline term term_from_float(avm_float_t f, Heap *heap)
{
    term *boxed_value = memory_heap_alloc(heap, FLOAT_SIZE);
    boxed_value[0] = ((FLOAT_SIZE - 1) << 6) | TERM_BOXED_FLOAT;

    float_term_t *boxed_float = (float_term_t *) (boxed_value + 1);
    boxed_float->f = f;

    return ((term) boxed_value) | TERM_PRIMARY_BOXED;
}

static inline avm_float_t term_to_float(term t)
{
    const float_term_t *boxed_float = (float_term_t *) (term_to_const_term_ptr(t) + 1);
    return boxed_float->f;
}

static inline bool term_is_number(term t)
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

avm_float_t term_conv_to_float(term t);

/**
 * @brief Checks if a term is a string (i.e., a list of characters)
 *
 * @details Returns \c true if a term is a proper (nil-terminated) list of characters
 * or an empty list (nil term), otherwise 0.
 * @param t the term that will be checked.
 * @return \c true if check succeeds, \c false otherwise.
 */
static inline bool term_is_string(term t)
{
    while (term_is_nonempty_list(t)) {
        term e = term_get_list_head(t);
        if (!term_is_uint8(e)) {
            return false;
        }
        t = term_get_list_tail(t);
    }
    return term_is_nil(t);
}

/**
 * @brief Gets function module name, name and arity.
 *
 * @details Allows to retrieve partial information by passing NULL pointers.
 * @param fun function term.
 * @param m module name as an atom.
 * @param f function name as an atom.
 * @param a function arity as an integer.
 *
 */
void term_get_function_mfa(term fun, term *m, term *f, term *a);

static inline term term_make_function_reference(term m, term f, term a, Heap *heap)
{
    term *boxed_func = memory_heap_alloc(heap, FUNCTION_REFERENCE_SIZE);

    boxed_func[0] = ((FUNCTION_REFERENCE_SIZE - 1) << 6) | TERM_BOXED_FUN;
    boxed_func[1] = m;
    boxed_func[2] = f;
    boxed_func[3] = a;

    return ((term) boxed_func) | TERM_PRIMARY_BOXED;
}

static inline bool term_is_match_state(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_BIN_MATCH_STATE) {
            return true;
        }
    }

    return false;
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

    return ((term) boxed_match_state) | TERM_PRIMARY_BOXED;
}

/**
 * @brief truncates last allocated boxed term to given size
 *
 * @details This function can be used to shrink last allocated boxed term
 * @param boxed the boxed term that will be shrinked (it must be the last allocated)
 * @param new_size in terms
 * @param heap the heap where the term has been allocated
 */
static inline void term_truncate_boxed(term boxed, size_t new_size, Heap *heap)
{
    /* boxed: 10 */
    TERM_DEBUG_ASSERT((t & TERM_PRIMARY_MASK) == TERM_PRIMARY_BOXED);

    term *boxed_value = term_to_term_ptr(boxed);
    int size_diff = (boxed_value[0] >> 6) - new_size;
    boxed_value[0] = (boxed_value[0] & TERM_BOXED_TAG_MASK) | (new_size << 6);
    memory_heap_trim(heap, size_diff);
}

static inline bool term_is_map(term t)
{
    if (term_is_boxed(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        if ((boxed_value[0] & TERM_BOXED_TAG_MASK) == TERM_BOXED_MAP) {
            return true;
        }
    }

    return false;
}

static inline size_t term_get_map_keys_offset(void)
{
    return 1;
}

static inline size_t term_get_map_value_offset(void)
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

    return ((term) boxed_value) | TERM_PRIMARY_BOXED;
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

static inline void term_set_map_value(term map, avm_uint_t pos, term value)
{
    term *boxed_value = term_to_term_ptr(map);
    boxed_value[term_get_map_value_offset() + pos] = value;
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
 * This function does increment the reference counter as the resource is
 * added to the heap's mso list.
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
    term ret = ((term) boxed_value) | TERM_PRIMARY_BOXED;
    boxed_value[3] = (term) refc;
    // Add the resource to the mso list
    refc->ref_count++;
    heap->root->mso_list = term_list_init_prepend(boxed_value + 4, ret, heap->root->mso_list);
    return ret;
}

#ifdef __cplusplus
}
#endif

#endif
