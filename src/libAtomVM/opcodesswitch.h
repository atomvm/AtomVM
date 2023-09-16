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

#include "module.h"

#include <assert.h>
#include <string.h>

//#define ENABLE_STACK_TRACE

#include "bif.h"
#include "bitstring.h"
#include "debug.h"
#include "defaultatoms.h"
#include "exportedfunction.h"
#include "nifs.h"
#include "opcodes.h"
#include "scheduler.h"
#include "utils.h"

#ifdef IMPL_EXECUTE_LOOP
    #include "bitstring.h"
    #include "mailbox.h"
    #include "stacktrace.h"
#endif

#define ENABLE_OTP21
#define ENABLE_OTP22
#define ENABLE_OTP23
#define ENABLE_OTP24
#define ENABLE_OTP25
#define ENABLE_OTP26

//#define ENABLE_TRACE

#include "trace.h"

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

#ifdef IMPL_EXECUTE_LOOP
#define SET_ERROR(error_type_atom)   \
    x_regs[0] = ERROR_ATOM;                                        \
    x_regs[1] = error_type_atom;                                   \
    x_regs[2] = stacktrace_create_raw(ctx, mod, i, ERROR_ATOM);    \

// Override nifs.h RAISE_ERROR macro
#ifdef RAISE_ERROR
#undef RAISE_ERROR
#endif
#define RAISE_ERROR(error_type_atom) \
    SET_ERROR(error_type_atom)       \
    goto handle_error;

#define VM_ABORT() \
    goto do_abort;

#endif

#ifdef IMPL_CODE_LOADER

#ifdef ENABLE_TRACE
typedef struct
{
    int reg_type;
    int index;
} dreg_t;

typedef dreg_t dreg_gc_safe_t;

#define T_DEST_REG(dreg) \
    reg_type_c((dreg).reg_type), (int) ((dreg).index)

#define T_DEST_REG_GC_SAFE(dreg) T_DEST_REG(dreg)
#else

typedef struct {} dreg_t;
typedef dreg_t dreg_gc_safe_t;

#endif

#define DECODE_COMPACT_TERM(dest_term, code_chunk, base_index, off)                     \
{                                                                                       \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                            \
    switch (first_byte & 0xF) {                                                         \
        case COMPACT_LARGE_LITERAL:                                                     \
        case COMPACT_LITERAL:                                                           \
            switch (((first_byte) >> 3) & 0x3) {                                        \
                case 0:                                                                 \
                case 2:                                                                 \
                    dest_term = term_from_int4(first_byte >> 4);                        \
                    off += 1;                                                           \
                    break;                                                              \
                                                                                        \
                case 1:                                                                 \
                    dest_term = term_from_int(((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]); \
                    off += 2;                                                           \
                    break;                                                              \
                                                                                        \
                default:                                                                \
                    fprintf(stderr, "Operand not literal: %x, or unsupported encoding\n", (first_byte)); \
                    AVM_ABORT();                                                        \
                    break;                                                              \
            }                                                                           \
            break;                                                                      \
                                                                                        \
        case COMPACT_INTEGER:                                                           \
            switch (((first_byte) >> 3) & 0x3) {                                        \
                case 0:                                                                 \
                case 2:                                                                 \
                    off += 1;                                                           \
                    break;                                                              \
                                                                                        \
                default:                                                                \
                    fprintf(stderr, "Operand not a small integer: %x, or unsupported encoding\n", (first_byte));        \
                    AVM_ABORT();                                                        \
                    break;                                                              \
            }                                                                           \
            break;                                                                      \
                                                                                        \
        case COMPACT_ATOM:                                                              \
        case COMPACT_XREG:                                                              \
        case COMPACT_YREG:                                                              \
            off += 1;                                                                   \
            break;                                                                      \
                                                                                        \
        case COMPACT_EXTENDED:                                                          \
            switch (first_byte) {                                                       \
                case COMPACT_EXTENDED_LITERAL: {                                        \
                    uint8_t ext = (code_chunk[(base_index) + (off) + 1] & 0xF);         \
                    if (ext == 0) {                                                     \
                        off += 2;                                                       \
                    } else if (ext == 0x8) {                                            \
                        off += 3;                                                       \
                    } else {                                                            \
                        AVM_ABORT();                                                    \
                    }                                                                   \
                    break;                                                              \
                }                                                                       \
                case COMPACT_EXTENDED_ALLOCATION_LIST: {                                \
                    uint8_t len = (code_chunk[(base_index) + (off) + 1] >> 4);          \
                    off += (len * 2);                                                   \
                    break;                                                              \
                }                                                                       \
                case COMPACT_EXTENDED_TYPED_REGISTER: {                                 \
                    uint8_t reg_byte = code_chunk[(base_index) + (off) + 1];            \
                    if (((reg_byte & 0x0F) != COMPACT_XREG)                             \
                        && ((reg_byte & 0x0F) != COMPACT_YREG)) {                       \
                        fprintf(stderr, "Unexpected reg byte %x @ %" PRIuPTR "\n", (int) reg_byte, (base_index) + (off) + 1); \
                        AVM_ABORT();                                                    \
                    }                                                                   \
                    off += 2;                                                           \
                    int type_index;                                                     \
                    DECODE_LITERAL(type_index, code_chunk, base_index, off)             \
                    break;                                                              \
                }                                                                       \
                default:                                                                \
                    fprintf(stderr, "Unexpected extended %x @ %" PRIuPTR "\n", (int) first_byte, (base_index) + (off) + 1); \
                    AVM_ABORT();                                                        \
                    break;                                                              \
            }                                                                           \
            break;                                                                      \
                                                                                        \
        case COMPACT_LARGE_INTEGER:                                                     \
        case COMPACT_LARGE_ATOM:                                                        \
            switch (first_byte & COMPACT_LARGE_IMM_MASK) {                              \
                case COMPACT_11BITS_VALUE:                                              \
                    off += 2;                                                           \
                    break;                                                              \
                                                                                        \
                case COMPACT_NBITS_VALUE:                                               \
                    /* TODO: when first_byte >> 5 is 7, a different encoding is used */ \
                    off += (first_byte >> 5) + 3;                                       \
                    break;                                                              \
                                                                                        \
                default:                                                                \
                    assert((first_byte & 0x30) != COMPACT_LARGE_INTEGER);               \
                    break;                                                              \
            }                                                                           \
            break;                                                                      \
                                                                                        \
        case COMPACT_LARGE_YREG:                                                        \
            off += 2;                                                                   \
            break;                                                                      \
                                                                                        \
        default:                                                                        \
            fprintf(stderr, "unknown compact term type: %i\n", ((first_byte) & 0xF));   \
            AVM_ABORT();                                                                \
            break;                                                                      \
    }                                                                                   \
}

#define DECODE_EXTENDED_LIST_TAG(code_chunk, base_index, off)                           \
{                                                                                       \
    if ((code_chunk[(base_index) + (off)]) != COMPACT_EXTENDED_LIST) {                  \
        fprintf(stderr, "Unexpected operand, expected a list, got %x\n", code_chunk[(base_index) + (off)]); \
        AVM_ABORT();                                                                    \
    }                                                                                   \
    off++;                                                                              \
}

#ifdef ENABLE_TRACE

#define DECODE_DEST_REGISTER(dreg, code_chunk, base_index, off)                                     \
{                                                                                                   \
    uint8_t first_byte = code_chunk[(base_index) + (off)];                                          \
    uint8_t reg_type = first_byte & 0xF;                                                            \
    (dreg).reg_type = reg_type;                                                                     \
    switch (reg_type) {                                                                             \
        case COMPACT_XREG:                                                                          \
        case COMPACT_YREG:                                                                          \
            (dreg).index = code_chunk[(base_index) + (off)] >> 4;                                   \
            off += 1;                                                                               \
            break;                                                                                  \
        case COMPACT_LARGE_YREG:                                                                    \
            (dreg).index = (((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]);     \
            off += 2;                                                                               \
            break;                                                                                  \
        default:                                                                                    \
            AVM_ABORT();                                                                            \
    }                                                                                               \
}

#define DECODE_DEST_REGISTER_GC_SAFE(dreg, code_chunk, base_index, off)                             \
    DECODE_DEST_REGISTER(dreg, code_chunk, base_index, off)

#else

#define DECODE_DEST_REGISTER(dreg, code_chunk, base_index, off)                                     \
{                                                                                                   \
    UNUSED(dreg);                                                                                   \
    uint8_t first_byte = code_chunk[(base_index) + (off)];                                          \
    uint8_t reg_type = first_byte & 0xF;                                                            \
    switch (reg_type) {                                                                             \
        case COMPACT_XREG:                                                                          \
        case COMPACT_YREG:                                                                          \
            off += 1;                                                                               \
            break;                                                                                  \
        case COMPACT_LARGE_YREG:                                                                    \
            off += 2;                                                                               \
            break;                                                                                  \
        default:                                                                                    \
            AVM_ABORT();                                                                            \
    }                                                                                               \
}

#define DECODE_DEST_REGISTER_GC_SAFE(dreg, code_chunk, base_index, off)                             \
    DECODE_DEST_REGISTER(dreg, code_chunk, base_index, off)

#endif

#define DECODE_FP_REGISTER(freg, code_chunk, base_index, off)                                       \
{                                                                                                   \
    if ((code_chunk[(base_index) + (off)]) != COMPACT_EXTENDED_FP_REGISTER) {                       \
        fprintf(stderr, "Unexpected operand, expected an fp register, got %x\n", code_chunk[(base_index) + (off)]); \
        AVM_ABORT();                                                                                \
    }                                                                                               \
    off++;                                                                                          \
    DECODE_LITERAL(freg, code_chunk, base_index, off);                                              \
    if (freg > MAX_REG) {                                                                           \
        fprintf(stderr, "FP register index %d > MAX_REG = %d\n", freg, MAX_REG);                    \
        AVM_ABORT();                                                                                \
    }                                                                                               \
}

#define DECODE_VALUE32(val, code_chunk, base_index, off)                                            \
{                                                                                                   \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                                        \
    switch (((first_byte) >> 3) & 0x3) {                                                            \
        case 0:                                                                                     \
        case 2:                                                                                     \
            val = first_byte >> 4;                                                                  \
            off += 1;                                                                               \
            break;                                                                                  \
                                                                                                    \
        case 1:                                                                                     \
            val = ((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1];                \
            off += 2;                                                                               \
            break;                                                                                  \
                                                                                                    \
        case 3: {                                                                                   \
            uint8_t sz = (first_byte >> 5) + 2;                                                     \
            if (sz > 4) {                                                                           \
                fprintf(stderr, "Unexpected operand, expected a literal of at most 4 bytes\n");     \
                AVM_ABORT();                                                                        \
            }                                                                                       \
            val = 0;                                                                                \
            for (uint8_t vi = 0; vi < sz; vi++) {                                                   \
                val <<= 8;                                                                          \
                val |= code[(base_index) + (off) + 1 + vi];                                         \
            }                                                                                       \
            off += 1 + sz;                                                                          \
            break;                                                                                  \
        }                                                                                           \
        default: UNREACHABLE(); /* help gcc 8.4 */                                                  \
    }                                                                                               \
}

#define DECODE_VALUE64(val, code_chunk, base_index, off)                                            \
{                                                                                                   \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                                        \
    switch (((first_byte) >> 3) & 0x3) {                                                            \
        case 0:                                                                                     \
        case 2:                                                                                     \
            val = first_byte >> 4;                                                                  \
            off += 1;                                                                               \
            break;                                                                                  \
                                                                                                    \
        case 1:                                                                                     \
            val = ((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1];                \
            off += 2;                                                                               \
            break;                                                                                  \
                                                                                                    \
        case 3: {                                                                                   \
            uint8_t sz = (first_byte >> 5) + 2;                                                     \
            if (sz > 8) {                                                                           \
                fprintf(stderr, "Unexpected operand, expected a literal of at most 8 bytes\n");     \
                AVM_ABORT();                                                                        \
            }                                                                                       \
            val = 0;                                                                                \
            for (uint8_t vi = 0; vi < sz; vi++) {                                                   \
                val <<= 8;                                                                          \
                val |= code[(base_index) + (off) + 1 + vi];                                         \
            }                                                                                       \
            off += 1 + sz;                                                                          \
            break;                                                                                  \
        }                                                                                           \
    }                                                                                               \
}

#define DECODE_ATOM(atom, code_chunk, base_index, off)                                                  \
{                                                                                                       \
    if (UNLIKELY(((code_chunk[(base_index) + (off)]) & 0x7) != COMPACT_ATOM)) {                         \
        fprintf(stderr, "Unexpected operand, expected an atom (%x)\n", (code_chunk[(base_index) + (off)])); \
        AVM_ABORT();                                                                                    \
    }                                                                                                   \
    uint32_t atom_ix;                                                                                   \
    DECODE_VALUE32(atom_ix, code_chunk, base_index, off);                                               \
    atom = module_get_atom_term_by_id(mod, atom_ix);                                                    \
}

#define DECODE_LABEL(label, code_chunk, base_index, off)                                                \
{                                                                                                       \
    if (UNLIKELY(((code_chunk[(base_index) + (off)]) & 0x7) != COMPACT_LABEL)) {                        \
        fprintf(stderr, "Unexpected operand, expected a label (%x)\n", (code_chunk[(base_index) + (off)])); \
        AVM_ABORT();                                                                                    \
    }                                                                                                   \
    DECODE_VALUE32(label, code_chunk, base_index, off);                                                 \
}

#define DECODE_LITERAL(literal, code_chunk, base_index, off)                                            \
{                                                                                                       \
    if (UNLIKELY(((code_chunk[(base_index) + (off)]) & 0x7) != COMPACT_LITERAL)) {                      \
        fprintf(stderr, "Unexpected operand, expected a literal (%x)\n", (code_chunk[(base_index) + (off)])); \
        AVM_ABORT();                                                                                    \
    }                                                                                                   \
    DECODE_VALUE32(literal, code_chunk, base_index, off);                                               \
}

#define DECODE_INTEGER(integer, code_chunk, base_index, off)                                            \
{                                                                                                       \
    if (UNLIKELY(((code_chunk[(base_index) + (off)]) & 0x7) != COMPACT_INTEGER)) {                      \
        fprintf(stderr, "Unexpected operand, expected an integer (%x)\n", (code_chunk[(base_index) + (off)])); \
        AVM_ABORT();                                                                                    \
    }                                                                                                   \
    DECODE_VALUE64(integer, code_chunk, base_index, off);                                               \
}

#define DECODE_XREG(reg, code_chunk, base_index, off)                                                   \
{                                                                                                       \
    if (UNLIKELY(((code_chunk[(base_index) + (off)]) & 0x7) != COMPACT_XREG)) {                         \
        fprintf(stderr, "Unexpected operand, expected an xreg (%x)\n", (code_chunk[(base_index) + (off)])); \
        AVM_ABORT();                                                                                    \
    }                                                                                                   \
    DECODE_VALUE32(reg, code_chunk, base_index, off);                                                   \
    if (reg > MAX_REG) {                                                                                \
        fprintf(stderr, "Register index %d > MAX_REG = %d\n", reg, MAX_REG);                            \
        AVM_ABORT();                                                                                    \
    }                                                                                                   \
}

#define DECODE_YREG(reg, code_chunk, base_index, off)                                                   \
{                                                                                                       \
    if (UNLIKELY(((code_chunk[(base_index) + (off)]) & 0x7) != COMPACT_YREG)) {                         \
        fprintf(stderr, "Unexpected operand, expected a yreg (%x)\n", (code_chunk[(base_index) + (off)])); \
        AVM_ABORT();                                                                                    \
    }                                                                                                   \
    DECODE_VALUE32(reg, code_chunk, base_index, off);                                                   \
}

#define DECODE_ALLOCATOR_LIST(need, code_chunk, base_index, off)                        \
    if (IS_EXTENDED_ALLOCATOR(code_chunk, base_index, off)) {                           \
        need = 0;                                                                       \
        off++; /* skip list tag */                                                      \
        uint32_t list_size;                                                             \
        DECODE_LITERAL(list_size, code_chunk, base_index, off);                         \
        uint32_t allocator_tag;                                                         \
        uint32_t allocator_size;                                                        \
        for (uint32_t j = 0; j < list_size; j++) {                                      \
            DECODE_LITERAL(allocator_tag, code_chunk, base_index, off);                 \
            DECODE_LITERAL(allocator_size, code_chunk, base_index, off);                \
            if (allocator_tag == COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FLOATS) {          \
                allocator_size *= FLOAT_SIZE;                                           \
            } else if (allocator_tag == COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FUNS) {     \
                allocator_size *= BOXED_FUN_SIZE;                                       \
            }                                                                           \
            need += allocator_size;                                                     \
        }                                                                               \
    } else {                                                                            \
        DECODE_LITERAL(need, code_chunk, base_index, off);                              \
    }

#endif

#ifdef IMPL_EXECUTE_LOOP

// If GC can be performed between decoding of register and accessing it,
// GC_SAFE variant must be used
typedef term * dreg_t;
typedef struct
{
    term *base;
    int index;
} dreg_gc_safe_t;

#define T_DEST_REG(dreg) \
    ((dreg) >= x_regs && (dreg) < x_regs + MAX_REG) ? 'x' : 'y', \
    (int) (((dreg) >= x_regs && (dreg) < x_regs + MAX_REG) ? ((dreg) - x_regs) : ((dreg) - ctx->e))

#define T_DEST_REG_GC_SAFE(dreg_gc_safe) \
    ((dreg).base == x_regs) ? 'x' : 'y', ((dreg).index)

#define DECODE_COMPACT_TERM(dest_term, code_chunk, base_index, off)                                                     \
{                                                                                                                       \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                                                            \
    switch (first_byte & 0xF) {                                                                                         \
        case COMPACT_LARGE_LITERAL:                                                                                     \
        case COMPACT_LITERAL:                                                                                           \
            switch (((first_byte) >> 3) & 0x3) {                                                                        \
                case 0:                                                                                                 \
                case 2:                                                                                                 \
                    dest_term = term_from_int4(first_byte >> 4);                                                        \
                    off += 1;                                                                                           \
                    break;                                                                                              \
                                                                                                                        \
                case 1:                                                                                                 \
                    dest_term = term_from_int(((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]);      \
                    off += 2;                                                                                           \
                    break;                                                                                              \
                                                                                                                        \
                default:                                                                                                \
                    fprintf(stderr, "Operand not a literal: %x, or unsupported encoding\n", (first_byte));              \
                    AVM_ABORT();                                                                                        \
                    break;                                                                                              \
            }                                                                                                           \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_INTEGER:                                                                                           \
            dest_term = term_from_int4(first_byte >> 4);                                                                \
            off += 1;                                                                                                   \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_ATOM:                                                                                              \
            if (first_byte == COMPACT_ATOM) {                                                                           \
                dest_term = term_nil();                                                                                 \
            } else {                                                                                                    \
                dest_term = module_get_atom_term_by_id(mod, first_byte >> 4);                                           \
            }                                                                                                           \
            off += 1;                                                                                                   \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_XREG:                                                                                              \
            dest_term = x_regs[first_byte >> 4];                                                                        \
            off += 1;                                                                                                   \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_YREG:                                                                                              \
            dest_term = ctx->e[first_byte >> 4];                                                                        \
            off += 1;                                                                                                   \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_EXTENDED:                                                                                          \
            switch (first_byte) {                                                                                       \
                case COMPACT_EXTENDED_LITERAL: {                                                                        \
                    uint8_t first_extended_byte = code_chunk[(base_index) + (off) + 1];                                 \
                    if (!(first_extended_byte & 0xF)) {                                                                 \
                        dest_term = module_load_literal(mod, first_extended_byte >> 4, ctx);                            \
                        off += 2;                                                                                       \
                    } else if ((first_extended_byte & 0xF) == 0x8) {                                                    \
                        uint8_t byte_1 = code_chunk[(base_index) + (off) + 2];                                          \
                        uint16_t index = (((uint16_t) first_extended_byte & 0xE0) << 3) | byte_1;                       \
                        dest_term = module_load_literal(mod, index, ctx);                                               \
                        off += 3;                                                                                       \
                    } else {                                                                                            \
                        VM_ABORT();                                                                                     \
                    }                                                                                                   \
                    if (UNLIKELY(term_is_invalid_term(dest_term))) {                                                    \
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);                                                                \
                    }                                                                                                   \
                                                                                                                        \
                    break;                                                                                              \
                }                                                                                                       \
                case COMPACT_EXTENDED_TYPED_REGISTER: {                                                                 \
                    uint8_t reg_byte = code_chunk[(base_index) + (off) + 1];                                            \
                    if ((reg_byte & 0x0F) == COMPACT_XREG) {                                                            \
                        dest_term = x_regs[reg_byte >> 4];                                                              \
                    } else {                                                                                            \
                        dest_term = ctx->e[reg_byte >> 4];                                                              \
                    }                                                                                                   \
                    off += 2;                                                                                           \
                    int type_index;                                                                                     \
                    DECODE_LITERAL(type_index, code_chunk, base_index, off)                                             \
                    break;                                                                                              \
                }                                                                                                       \
                default:                                                                                                \
                    VM_ABORT();                                                                                         \
                    break;                                                                                              \
            }                                                                                                           \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_LARGE_ATOM:                                                                                        \
            switch (first_byte & COMPACT_LARGE_IMM_MASK) {                                                              \
                case COMPACT_11BITS_VALUE:                                                                              \
                    dest_term = module_get_atom_term_by_id(mod, ((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]); \
                    off += 2;                                                                                           \
                    break;                                                                                              \
                                                                                                                        \
                default:                                                                                                \
                    VM_ABORT();                                                                                         \
                    break;                                                                                              \
            }                                                                                                           \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_LARGE_INTEGER:                                                                                     \
            switch (first_byte & COMPACT_LARGE_IMM_MASK) {                                                              \
                case COMPACT_11BITS_VALUE:                                                                              \
                    dest_term = term_from_int11(((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]);     \
                    off += 2;                                                                                           \
                    break;                                                                                              \
                                                                                                                        \
                case COMPACT_NBITS_VALUE:                                                                               \
                    dest_term = large_integer_to_term(ctx, (code_chunk) + (base_index) + (off), &(off));                \
                    if (UNLIKELY(term_is_invalid_term(dest_term))) {                                                    \
                        HANDLE_ERROR();                                                                                 \
                    }                                                                                                   \
                    break;                                                                                              \
                                                                                                                        \
                default:                                                                                                \
                    VM_ABORT();                                                                                         \
                    break;                                                                                              \
            }                                                                                                           \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_LARGE_YREG:                                                                                        \
            if (LIKELY((first_byte & COMPACT_LARGE_IMM_MASK) == COMPACT_11BITS_VALUE)) {                                \
                dest_term = ctx->e[((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]];                  \
                off += 2;                                                                                               \
            } else {                                                                                                    \
                VM_ABORT();                                                                                             \
            }                                                                                                           \
            break;                                                                                                      \
                                                                                                                        \
        default:                                                                                                        \
            VM_ABORT();                                                                                                 \
    }                                                                                                                   \
}

#define READ_DEST_REGISTER(dreg) *(dreg)

#define READ_DEST_REGISTER_GC_SAFE(dreg_gc_safe) \
    ((dreg_gc_safe).base == x_regs ? x_regs[(dreg_gc_safe).index] : ctx->e[(dreg_gc_safe).index])


#define WRITE_REGISTER(dreg, value)                                                     \
{                                                                                       \
    *dreg = value;                                                                      \
}

#define WRITE_REGISTER_GC_SAFE(dreg_gc_safe, value)                                     \
{                                                                                       \
    if ((dreg_gc_safe).base == x_regs) {                                                \
        x_regs[(dreg_gc_safe).index] = value;                                           \
    } else {                                                                            \
        ctx->e[(dreg_gc_safe).index] = value;                                           \
    }                                                                                   \
}

#define DECODE_EXTENDED_LIST_TAG(code_chunk, base_index, off)                           \
{                                                                                       \
    off++;                                                                              \
}

#define DECODE_DEST_REGISTER(dreg, code_chunk, base_index, off)                                                 \
{                                                                                                               \
    uint8_t first_byte = code_chunk[(base_index) + (off)];                                                      \
    uint8_t reg_type = first_byte & 0xF;                                                                        \
    uint8_t reg_index = (first_byte >> 4);                                                                      \
    switch (reg_type) {                                                                                         \
        case COMPACT_XREG:                                                                                      \
            (dreg) = x_regs + reg_index;                                                                        \
            off++;                                                                                              \
            break;                                                                                              \
        case COMPACT_YREG:                                                                                      \
            (dreg) = ctx->e + reg_index;                                                                        \
            off++;                                                                                              \
            break;                                                                                              \
        case COMPACT_LARGE_YREG:                                                                                \
            if (LIKELY((first_byte & COMPACT_LARGE_IMM_MASK) == COMPACT_11BITS_VALUE)) {                        \
                (dreg) = ctx->e + (((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]);          \
                off += 2;                                                                                       \
            } else {                                                                                            \
                VM_ABORT();                                                                                     \
            }                                                                                                   \
            break;                                                                                              \
        default:                                                                                                \
            VM_ABORT();                                                                                         \
    }                                                                                                           \
}

#define DECODE_DEST_REGISTER_GC_SAFE(dreg_gc_safe, code_chunk, base_index, off)                                 \
{                                                                                                               \
    uint8_t first_byte = code_chunk[(base_index) + (off)];                                                      \
    uint8_t reg_type = first_byte & 0xF;                                                                        \
    uint8_t reg_index = (first_byte >> 4);                                                                      \
    switch (reg_type) {                                                                                         \
        case COMPACT_XREG:                                                                                      \
            (dreg_gc_safe).base = x_regs;                                                                       \
            (dreg_gc_safe).index = reg_index;                                                                   \
            off++;                                                                                              \
            break;                                                                                              \
        case COMPACT_YREG:                                                                                      \
            (dreg_gc_safe).base = ctx->e;                                                                       \
            (dreg_gc_safe).index = reg_index;                                                                   \
            off++;                                                                                              \
            break;                                                                                              \
        case COMPACT_LARGE_YREG:                                                                                \
            if (LIKELY((first_byte & COMPACT_LARGE_IMM_MASK) == COMPACT_11BITS_VALUE)) {                        \
                (dreg_gc_safe).base = ctx->e;                                                                   \
                (dreg_gc_safe).index = (((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]);     \
                off += 2;                                                                                       \
            } else {                                                                                            \
                VM_ABORT();                                                                                     \
            }                                                                                                   \
            break;                                                                                              \
        default:                                                                                                \
            VM_ABORT();                                                                                         \
    }                                                                                                           \
}

// MAX_REG is enforced at decode time
#if MAX_REG <= 16
#define DECODE_FP_REGISTER(freg, code_chunk, base_index, off)                                       \
{                                                                                                   \
    off++;                                                                                          \
    uint8_t first_byte = code_chunk[base_index + off];                                              \
    off++;                                                                                          \
    freg = first_byte >> 4;                                                                         \
}
#else
#define DECODE_FP_REGISTER(freg, code_chunk, base_index, off)                                       \
{                                                                                                   \
    off++;                                                                                          \
    DECODE_LITERAL(freg, code_chunk, base_index, off);                                              \
}
#endif

#define DECODE_VALUE(val, code_chunk, base_index, off)                                              \
{                                                                                                   \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                                        \
    switch (((first_byte) >> 3) & 0x3) {                                                            \
        case 0:                                                                                     \
        case 2:                                                                                     \
            val = first_byte >> 4;                                                                  \
            off += 1;                                                                               \
            break;                                                                                  \
                                                                                                    \
        case 1:                                                                                     \
            val = ((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1];                \
            off += 2;                                                                               \
            break;                                                                                  \
                                                                                                    \
        case 3: {                                                                                   \
            uint8_t sz = (first_byte >> 5) + 2;                                                     \
            val = 0;                                                                                \
            for (uint8_t vi = 0; vi < sz; vi++) {                                                   \
                val <<= 8;                                                                          \
                val |= code_chunk[(base_index) + (off) + 1 + vi];                                   \
            }                                                                                       \
            off += 1 + sz;                                                                          \
            break;                                                                                  \
        }                                                                                           \
        default: UNREACHABLE(); /* help gcc 8.4 */                                                  \
    }                                                                                               \
}

#define DECODE_ATOM(atom, code_chunk, base_index, off)                                              \
{                                                                                                   \
    uint32_t atom_ix;                                                                               \
    DECODE_VALUE(atom_ix, code_chunk, base_index, off);                                             \
    atom = module_get_atom_term_by_id(mod, atom_ix);                                                \
}

#define DECODE_LABEL(label, code_chunk, base_index, off) \
    DECODE_VALUE(label, code_chunk, base_index, off)

#define DECODE_LITERAL(val, code_chunk, base_index, off) \
    DECODE_VALUE(val, code_chunk, base_index, off)

#define DECODE_INTEGER(integer, code_chunk, base_index, off) \
    DECODE_VALUE(integer, code_chunk, base_index, off)

// MAX_REG is enforced at decode time
#if MAX_REG <= 16
#define DECODE_XREG(reg, code_chunk, base_index, off)                                               \
{                                                                                                   \
    uint8_t first_byte = code_chunk[base_index + off];                                              \
    reg = first_byte >> 4;                                                                          \
    off++;                                                                                          \
}
#else
#define DECODE_XREG(reg, code_chunk, base_index, off) \
    DECODE_VALUE(reg, code_chunk, base_index, off)
#endif

#define DECODE_YREG(reg, code_chunk, base_index, off) \
    DECODE_VALUE(reg, code_chunk, base_index, off)

#define DECODE_ALLOCATOR_LIST(need, code_chunk, base_index, off)                        \
    if (IS_EXTENDED_ALLOCATOR(code_chunk, base_index, off)) {                           \
        need = 0;                                                                       \
        off++; /* skip list tag */                                                      \
        uint32_t list_size;                                                             \
        DECODE_LITERAL(list_size, code_chunk, base_index, off);                         \
        uint32_t allocator_tag;                                                         \
        uint32_t allocator_size;                                                        \
        for (uint32_t j = 0; j < list_size; j++) {                                      \
            DECODE_LITERAL(allocator_tag, code_chunk, base_index, off);                 \
            DECODE_LITERAL(allocator_size, code_chunk, base_index, off);                \
            if (allocator_tag == COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FLOATS) {          \
                allocator_size *= FLOAT_SIZE;                                           \
            } else if (allocator_tag == COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FUNS) {     \
                allocator_size *= BOXED_FUN_SIZE;                                       \
            }                                                                           \
            need += allocator_size;                                                     \
        }                                                                               \
    } else {                                                                            \
        DECODE_LITERAL(need, code_chunk, base_index, off);                              \
    }

#endif

#define IS_EXTENDED_ALLOCATOR(code_chunk, base_index, off) \
    (code_chunk[(base_index) + (off)]) == COMPACT_EXTENDED_ALLOCATION_LIST

#define IS_EXTENDED_FP_REGISTER(code_chunk, base_index, off) \
    (code_chunk[(base_index) + (off)]) == COMPACT_EXTENDED_FP_REGISTER

#define NEXT_INSTRUCTION(operands_size) \
    i += operands_size

#define JUMP_TO_LABEL(module, label)    \
    if (module != mod) {                \
        prev_mod = mod;                 \
        mod = module;                   \
        code = mod->code->code;         \
    }                                   \
    JUMP_TO_ADDRESS(mod->labels[label])

#ifndef TRACE_JUMP
    #define JUMP_TO_ADDRESS(address) \
        i = ((uint8_t *) (address)) - code
#else
    #define JUMP_TO_ADDRESS(address)        \
        i = ((uint8_t *) (address)) - code; \
        fprintf(stderr, "going to jump to %i\n", i)
#endif

#define SCHEDULE_NEXT(restore_mod, restore_to) \
    {                                                                                             \
        ctx->saved_ip = restore_to;                                                               \
        ctx->saved_module = restore_mod;                                                          \
        ctx = scheduler_next(ctx->global, ctx);                                                   \
        goto schedule_in;                                                                         \
    }

// We use goto label as values, a GCC extension supported by clang.

#define PROCESS_SIGNAL_MESSAGES() \
    {                                                                                           \
        MailboxMessage *signal_message = mailbox_process_outer_list(&ctx->mailbox);             \
        void *next_label = NULL;                                                                \
        while (signal_message) {                                                                \
            switch (signal_message->type) {                                                     \
                case KillSignal: {                                                              \
                    struct TermSignal *kill_signal                                              \
                        = CONTAINER_OF(signal_message, struct TermSignal, base);                \
                    context_process_kill_signal(ctx, kill_signal);                              \
                    break;                                                                      \
                }                                                                               \
                case GCSignal: {                                                                \
                    if (UNLIKELY(memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) { \
                        SET_ERROR(OUT_OF_MEMORY_ATOM);                                          \
                        next_label = &&handle_error;                                            \
                    }                                                                           \
                    break;                                                                      \
                }                                                                               \
                case ProcessInfoRequestSignal: {                                                \
                    struct BuiltInAtomRequestSignal *request_signal                             \
                        = CONTAINER_OF(signal_message, struct BuiltInAtomRequestSignal, base);  \
                    context_process_process_info_request_signal(ctx, request_signal);           \
                    break;                                                                      \
                }                                                                               \
                case TrapAnswerSignal: {                                                        \
                    struct TermSignal *trap_answer                                              \
                        = CONTAINER_OF(signal_message, struct TermSignal, base);                \
                    if (UNLIKELY(!context_process_signal_trap_answer(ctx, trap_answer))) {      \
                        SET_ERROR(OUT_OF_MEMORY_ATOM);                                          \
                        next_label = &&handle_error;                                            \
                    }                                                                           \
                    break;                                                                      \
                }                                                                               \
                case TrapExceptionSignal: {                                                     \
                    struct BuiltInAtomSignal *trap_exception                                    \
                        = CONTAINER_OF(signal_message, struct BuiltInAtomSignal, base);         \
                    SET_ERROR(trap_exception->atom);                                            \
                    next_label = &&handle_error;                                                \
                    break;                                                                      \
                }                                                                               \
                case FlushMonitorSignal:                                                        \
                case FlushInfoMonitorSignal: {                                                  \
                    struct RefSignal *flush_signal                                              \
                        = CONTAINER_OF(signal_message, struct RefSignal, base);                 \
                    bool info = signal_message->type == FlushInfoMonitorSignal;                 \
                    context_process_flush_monitor_signal(ctx, flush_signal->ref_ticks, info);   \
                    break;                                                                      \
                }                                                                               \
                case NormalMessage: {                                                           \
                    UNREACHABLE();                                                              \
                }                                                                               \
            }                                                                                   \
            MailboxMessage *next = signal_message->next;                                        \
            mailbox_message_dispose(signal_message, &ctx->heap);                                \
            signal_message = next;                                                              \
        }                                                                                       \
        if (context_get_flags(ctx, Killed)) {                                                   \
            goto terminate_context;                                                             \
        }                                                                                       \
        if (next_label) {                                                                       \
            goto *next_label;                                                                   \
        }                                                                                       \
        if (context_get_flags(ctx, Trap)) {                                                     \
            SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());                                          \
        }                                                                                       \
    }

#define PROCESS_MAYBE_TRAP_RETURN_VALUE(return_value)           \
    if (term_is_invalid_term(return_value)) {                   \
        if (UNLIKELY(!context_get_flags(ctx, Trap))) {          \
            HANDLE_ERROR();                                     \
        } else {                                                \
            SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());          \
        }                                                       \
    }

#define PROCESS_MAYBE_TRAP_RETURN_VALUE_RESTORE_I(return_value, rest_i) \
    if (term_is_invalid_term(return_value)) {                           \
        if (UNLIKELY(!context_get_flags(ctx, Trap))) {                  \
            i = rest_i;                                                 \
            HANDLE_ERROR();                                             \
        } else {                                                        \
            SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());                  \
        }                                                               \
    }

#define PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(return_value)      \
    if (term_is_invalid_term(return_value)) {                   \
        if (UNLIKELY(!context_get_flags(ctx, Trap))) {          \
            HANDLE_ERROR();                                     \
        } else {                                                \
            DO_RETURN();                                        \
            SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());          \
        }                                                       \
    }

#define INSTRUCTION_POINTER() \
    ((const void *) &code[i])

#define DO_RETURN()                                                     \
    {                                                                   \
        int module_index = ctx->cp >> 24;                               \
        if (module_index == prev_mod->module_index) {                   \
            Module *t = mod;                                            \
            mod = prev_mod;                                             \
            prev_mod = t;                                               \
            code = mod->code->code;                                     \
        } else if (module_index != mod->module_index) {                 \
            prev_mod = mod;                                             \
            mod = globalcontext_get_module_by_index(glb, module_index); \
            code = mod->code->code;                                     \
        }                                                               \
        i = (ctx->cp & 0xFFFFFF) >> 2;                                  \
    }

#define POINTER_TO_II(instruction_pointer) \
    (((uint8_t *) (instruction_pointer)) - code)

#define HANDLE_ERROR()                                         \
    x_regs[2] = stacktrace_create_raw(ctx, mod, i, x_regs[0]); \
    goto handle_error;

#define VERIFY_IS_INTEGER(t, opcode_name)                  \
    if (UNLIKELY(!term_is_integer(t))) {                   \
        TRACE(opcode_name ": " #t " is not an integer\n"); \
        RAISE_ERROR(BADARG_ATOM);                          \
    }

#define VERIFY_IS_ANY_INTEGER(t, opcode_name)               \
    if (UNLIKELY(!term_is_any_integer(t))) {                \
        TRACE(opcode_name ": " #t " is not any integer\n"); \
        RAISE_ERROR(BADARG_ATOM);                           \
    }

#define VERIFY_IS_BINARY(t, opcode_name)                 \
    if (UNLIKELY(!term_is_binary(t))) {                  \
        TRACE(opcode_name ": " #t " is not a binary\n"); \
        RAISE_ERROR(BADARG_ATOM);                        \
    }

#define VERIFY_IS_MATCH_STATE(t, opcode_name)                    \
    if (UNLIKELY(!term_is_match_state(t))) {                     \
        TRACE(opcode_name ": " #t " is not a match context.\n"); \
        RAISE_ERROR(BADARG_ATOM);                                \
    }

#define VERIFY_IS_MATCH_OR_BINARY(t, opcode_name)                          \
    if (UNLIKELY(!(term_is_binary(t) || term_is_match_state(t)))) {        \
        TRACE(opcode_name ": " #t " is not a binary or match context.\n"); \
        RAISE_ERROR(BADARG_ATOM);                                          \
    }

#define CALL_FUN(fun, args_count, next_off)                             \
    Module *fun_module;                                                 \
    unsigned int fun_arity;                                             \
    uint32_t n_freeze = 0;                                              \
    uint32_t label;                                                     \
    const term *boxed_value = term_to_const_term_ptr(fun);              \
    term index_or_function = boxed_value[2];                            \
    if (term_is_atom(index_or_function)) {                              \
        term module = boxed_value[1];                                   \
        fun_arity = term_to_int(boxed_value[3]);                        \
        AtomString module_name = globalcontext_atomstring_from_term(glb, module); \
        AtomString function_name = globalcontext_atomstring_from_term(glb, index_or_function); \
        struct Nif *nif = (struct Nif *) nifs_get(module_name, function_name, fun_arity); \
        if (!IS_NULL_PTR(nif)) {                                        \
            term return_value = nif->nif_ptr(ctx, fun_arity, x_regs);   \
            NEXT_INSTRUCTION(next_off);                                 \
            PROCESS_MAYBE_TRAP_RETURN_VALUE(return_value);              \
            x_regs[0] = return_value;                                   \
            if (ctx->heap.root->next) {                                 \
                if (UNLIKELY(memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) { \
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);                    \
                }                                                       \
            }                                                           \
            continue;                                                   \
        } else {                                                        \
            fun_module = globalcontext_get_module(ctx->global, module_name); \
            if (IS_NULL_PTR(fun_module)) {                              \
                HANDLE_ERROR();                                         \
            }                                                           \
            label = module_search_exported_function(fun_module, function_name, fun_arity, glb); \
            if (UNLIKELY(label == 0)) {                                 \
                HANDLE_ERROR();                                         \
            }                                                           \
        }                                                               \
    } else {                                                            \
        fun_module = (Module *) boxed_value[1];                         \
        uint32_t fun_index = term_to_int(index_or_function);            \
        uint32_t fun_arity_and_freeze;                                  \
        module_get_fun(fun_module, fun_index, &label, &fun_arity_and_freeze, &n_freeze); \
        fun_arity = fun_arity_and_freeze - n_freeze;                    \
        TRACE_CALL(ctx, mod, "call_fun", label, args_count);            \
    }                                                                   \
    if (UNLIKELY(args_count != fun_arity)) {                            \
        RAISE_ERROR(BADARITY_ATOM);                                     \
    }                                                                   \
    for (uint32_t i = 0; i < n_freeze; i++) {                           \
        x_regs[i + fun_arity] = boxed_value[i + 3];                     \
    }                                                                   \
    NEXT_INSTRUCTION(next_off);                                         \
    ctx->cp = module_address(mod->module_index, i);                     \
    JUMP_TO_LABEL(fun_module, label);

#define DECODE_FLAGS_LIST(flags_value, flags, opcode)                   \
    flags_value = 0;                                                    \
    while (term_is_nonempty_list(flags)) {                              \
        switch (term_get_list_head(flags)) {                            \
            case NATIVE_ATOM:                                           \
                flags_value |= NativeEndianInteger;                     \
                break;                                                  \
            case LITTLE_ATOM:                                           \
                flags_value |= LittleEndianInteger;                     \
                break;                                                  \
            case SIGNED_ATOM:                                           \
                flags_value |= SignedInteger;                           \
                break;                                                  \
            default:                                                    \
                TRACE(#opcode ": Unknown flag atom %lx\n", (long) flags); \
                RAISE_ERROR(BADARG_ATOM);                               \
        }                                                               \
        flags = term_get_list_tail(flags);                              \
    }                                                                   \
    if (UNLIKELY(!term_is_nil(flags))) {                                \
        TRACE(#opcode ": Flags not a proper list %lx\n", (long) flags); \
        RAISE_ERROR(BADARG_ATOM);                                       \
    }


#ifndef MIN
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#endif

#ifdef IMPL_EXECUTE_LOOP
struct Int24
{
    int32_t val24 : 24;
};

struct Int40
{
    int64_t val40 : 40;
};

struct Int48
{
    int64_t val48 : 48;
};

struct Int56
{
    int64_t val56 : 56;
};

#define SWAP_KV_PAIR(I, J)            \
    {                                 \
        struct kv_pair tmp = kv[(I)]; \
        kv[(I)] = kv[(J)];            \
        kv[(J)] = tmp;                \
    }

struct kv_pair
{
    term key;
    term value;
};

static bool sort_kv_pairs(struct kv_pair *kv, int size, GlobalContext *global)
{
    int k = size;
    while (1 < k) {
        int max_pos = 0;
        for (int i = 1; i < k; i++) {
            term t_max = kv[max_pos].key;
            term t = kv[i].key;
            // TODO: not sure if exact is the right choice here
            TermCompareResult result = term_compare(t, t_max, TermCompareExact, global);
            if (result == TermGreaterThan) {
                max_pos = i;
            } else if (UNLIKELY(result == TermCompareMemoryAllocFail)) {
                return false;
            }
        }
        if (max_pos != k - 1) {
            SWAP_KV_PAIR(k - 1, max_pos);
        }
        k--;
        // kv[k..size] sorted
    }

    return true;
}

static int get_catch_label_and_change_module(Context *ctx, Module **mod)
{
    term *ct = ctx->e;
    term *last_frame = ctx->e;

    while (ct != ctx->heap.heap_end) {
        if (term_is_catch_label(*ct)) {
            int target_module;
            int target_label = term_to_catch_label_and_module(*ct, &target_module);
            TRACE("- found catch: label: %i, module: %i\n", target_label, target_module);
            *mod = globalcontext_get_module_by_index(ctx->global, target_module);

            DEBUG_DUMP_STACK(ctx);
            ctx->e = last_frame;
            DEBUG_DUMP_STACK(ctx);

            return target_label;

        } else if (term_is_cp(*ct)) {
            last_frame = ct + 1;
        }

        ct++;
    }

    return 0;
}

COLD_FUNC static void cp_to_mod_lbl_off(term cp, Context *ctx, Module **cp_mod, int *label, int *l_off)
{
    Module *mod = globalcontext_get_module_by_index(ctx->global, cp >> 24);
    long mod_offset = (cp & 0xFFFFFF) >> 2;

    *cp_mod = mod;

    uint8_t *code = &mod->code->code[0];
    int labels_count = ENDIAN_SWAP_32(mod->code->labels);

    int i = 1;
    uint8_t *l = mod->labels[1];
    while (mod_offset > l - code) {
        i++;
        if (i >= labels_count) {
            // last label + 1 is reserved for end of module.
            *label = i;
            *l_off = 0;
            return;
        }
        l = mod->labels[i];
    }

    *label = i - 1;
    *l_off = mod_offset - ((uint8_t *) mod->labels[*label] - code);
}

COLD_FUNC static void dump(Context *ctx)
{
    fprintf(stderr, "CRASH \n======\n");

    fprintf(stderr, "pid: ");
    term_display(stderr, term_from_local_process_id(ctx->process_id), ctx);
    fprintf(stderr, "\n");

    fprintf(stderr, "\nStacktrace:\n");
    term_display(stderr, stacktrace_build(ctx, &ctx->x[2]), ctx);
    fprintf(stderr, "\n\n");

    {
        Module *cp_mod;
        int label;
        int offset;
        cp_to_mod_lbl_off(ctx->cp, ctx, &cp_mod, &label, &offset);
        fprintf(stderr, "cp: #CP<module: %i, label: %i, offset: %i>\n\n",
            cp_mod->module_index, label, offset);
    }

    fprintf(stderr, "x[0]: ");
    term_display(stderr, ctx->x[0], ctx);
    fprintf(stderr, "\nx[1]: ");
    term_display(stderr, ctx->x[1], ctx);
    fprintf(stderr, "\nx[2]: ");
    term_display(stderr, ctx->x[2], ctx);
    fprintf(stderr, "\n\nStack \n------\n\n");

    term *ct = ctx->e;

    while (ct != ctx->heap.heap_end) {
        if (term_is_catch_label(*ct)) {
            int target_module;
            int target_label = term_to_catch_label_and_module(*ct, &target_module);
            fprintf(stderr, "catch: %i:%i\n", target_label, target_module);

        } else if (term_is_cp(*ct)) {
            Module *cp_mod;
            int label;
            int offset;
            cp_to_mod_lbl_off(*ct, ctx, &cp_mod, &label, &offset);
            fprintf(stderr, "#CP<module: %i, label: %i, offset: %i>\n", cp_mod->module_index, label, offset);

        } else {
            term_display(stderr, *ct, ctx);
            fprintf(stderr, "\n");
        }

        ct++;
    }

    fprintf(stderr, "\n\nRegisters\n----------");
    for (int i = 0; i < 16; i++) {
        fprintf(stderr, "\nx[%i]: ", i);
        term_display(stderr, ctx->x[i], ctx);
    }
    fprintf(stderr, "\n");

    fprintf(stderr, "\n\nMailbox\n--------\n");
    mailbox_crashdump(ctx);

    fprintf(stderr, "\n\nMonitors\n--------\n");
    // Lock processes table to make sure any dying process will not modify monitors
    struct ListHead *processes_table = synclist_rdlock(&ctx->global->processes_table);
    UNUSED(processes_table);
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->monitors_head) {
        struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
        if (term_is_pid(monitor->monitor_obj)) {
            term_display(stderr, monitor->monitor_obj, ctx);
        } else {
            fprintf(stderr, "<resource %p>", (void *) term_to_const_term_ptr(monitor->monitor_obj));
        }
        fprintf(stderr, " ");
        if (monitor->ref_ticks == 0) {
            fprintf(stderr, "<");
        }
        fprintf(stderr, "---> ");
        term_display(stderr, term_from_local_process_id(ctx->process_id), ctx);
        fprintf(stderr, "\n");
    }
    synclist_unlock(&ctx->global->processes_table);
    fprintf(stderr, "\n\n**End Of Crash Report**\n");
}

static term maybe_alloc_boxed_integer_fragment(Context *ctx, avm_int64_t value)
{
#if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
    if ((value < AVM_INT_MIN) || (value > AVM_INT_MAX)) {
        if (UNLIKELY(memory_ensure_free_opt(ctx, BOXED_INT64_SIZE, MEMORY_NO_GC) != MEMORY_GC_OK)) {
            ctx->x[0] = ERROR_ATOM;
            ctx->x[1] = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        return term_make_boxed_int64(value, &ctx->heap);
    } else
#endif
    if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        if (UNLIKELY(memory_ensure_free_opt(ctx, BOXED_INT_SIZE, MEMORY_NO_GC) != MEMORY_GC_OK)) {
            ctx->x[0] = ERROR_ATOM;
            ctx->x[1] = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        return term_make_boxed_int(value, &ctx->heap);
    } else {
        return term_from_int(value);
    }
}

static inline term maybe_alloc_boxed_integer_fragment_helper(Context *ctx, avm_int64_t value, unsigned int bytes_count)
{
    if (bytes_count < sizeof(avm_int_t)) {
        return term_from_int(value);
    } else {
        return maybe_alloc_boxed_integer_fragment(ctx, value);
    }
}

static term large_integer_to_term(Context *ctx, uint8_t *compact_term, int *next_operand_offset)
{
    int num_bytes = (*compact_term >> 5) + 2;

    switch (num_bytes) {
        case 2: {
            *next_operand_offset += 3;
            int16_t ret_val16 = ((int16_t) compact_term[1]) << 8 | compact_term[2];
            return maybe_alloc_boxed_integer_fragment_helper(ctx, ret_val16, 2);
        }

        case 3: {
            *next_operand_offset += 4;
            struct Int24 ret_val24;
            ret_val24.val24 = ((int32_t) compact_term[1]) << 16 | ((int32_t) compact_term[2] << 8) | compact_term[3];
            return maybe_alloc_boxed_integer_fragment_helper(ctx, ret_val24.val24, 3);
        }

        case 4: {
            *next_operand_offset += 5;
            int32_t ret_val32;
            ret_val32 = ((int32_t) compact_term[1]) << 24 | ((int32_t) compact_term[2] << 16)
                | ((int32_t) compact_term[3] << 8) | compact_term[4];
            return maybe_alloc_boxed_integer_fragment_helper(ctx, ret_val32, 4);
        }

        case 5: {
            *next_operand_offset += 6;
            struct Int40 ret_val40;
            ret_val40.val40 = ((int64_t) compact_term[1]) << 32 | ((int64_t) compact_term[2] << 24)
                | ((int64_t) compact_term[3] << 16) | ((int64_t) compact_term[4] << 8)
                | (int64_t) compact_term[5];

            return maybe_alloc_boxed_integer_fragment_helper(ctx, ret_val40.val40, 5);
        }

        case 6: {
            *next_operand_offset += 7;
            struct Int48 ret_val48;
            ret_val48.val48 = ((int64_t) compact_term[1]) << 40 | ((int64_t) compact_term[2] << 32)
                | ((int64_t) compact_term[3] << 24) | ((int64_t) compact_term[4] << 16)
                | ((int64_t) compact_term[5] << 8) | (int64_t) compact_term[6];

            return maybe_alloc_boxed_integer_fragment_helper(ctx, ret_val48.val48, 6);
        }

        case 7: {
            *next_operand_offset += 8;
            struct Int56 ret_val56;
            ret_val56.val56 = ((int64_t) compact_term[1]) << 48 | ((int64_t) compact_term[2] << 40)
                | ((int64_t) compact_term[3] << 32) | ((int64_t) compact_term[4] << 24)
                | ((int64_t) compact_term[5] << 16) | ((int64_t) compact_term[6] << 8)
                | (int64_t) compact_term[7];

            return maybe_alloc_boxed_integer_fragment_helper(ctx, ret_val56.val56, 7);
        }

        case 8: {
            *next_operand_offset += 9;
            int64_t ret_val64;
            ret_val64 = ((int64_t) compact_term[1]) << 56 | ((int64_t) compact_term[2] << 48)
                | ((int64_t) compact_term[3] << 40) | ((int64_t) compact_term[4] << 32)
                | ((int64_t) compact_term[5] << 24) | ((int64_t) compact_term[6] << 16)
                | ((int64_t) compact_term[7] << 8) | (int64_t) compact_term[8];

            return maybe_alloc_boxed_integer_fragment_helper(ctx, ret_val64, 8);
        }

        default:
            ctx->x[0] = ERROR_ATOM;
            ctx->x[1] = OVERFLOW_ATOM;
            return term_invalid_term();
    }
}

term make_fun(Context *ctx, const Module *mod, int fun_index)
{
    uint32_t n_freeze = module_get_fun_freeze(mod, fun_index);

    int size = BOXED_FUN_SIZE + n_freeze;
    if (memory_ensure_free_opt(ctx, size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
        return term_invalid_term();
    }
    term *boxed_func = memory_heap_alloc(&ctx->heap, size);

    boxed_func[0] = ((size - 1) << 6) | TERM_BOXED_FUN;
    boxed_func[1] = (term) mod;
    boxed_func[2] = term_from_int(fun_index);

    for (uint32_t i = 3; i < n_freeze + 3; i++) {
        boxed_func[i] = ctx->x[i - 3];
    }

    return ((term) boxed_func) | TERM_BOXED_VALUE_TAG;
}

static bool maybe_call_native(Context *ctx, AtomString module_name, AtomString function_name, int arity,
    term *return_value)
{
    const struct ExportedFunction *exported_bif = bif_registry_get_handler(module_name, function_name, arity);
    if (exported_bif) {
        if (exported_bif->type == GCBIFFunctionType) {
            const struct GCBif *gcbif = EXPORTED_FUNCTION_TO_GCBIF(exported_bif);
            switch (arity) {
                case 1: {
                    *return_value = gcbif->gcbif1_ptr(ctx, 0, ctx->x[0]);
                    return true;
                }
                case 2: {
                    *return_value = gcbif->gcbif2_ptr(ctx, 0, ctx->x[0], ctx->x[1]);
                    return true;
                }
                case 3: {
                    *return_value = gcbif->gcbif3_ptr(ctx, 0, ctx->x[0], ctx->x[1], ctx->x[2]);
                    return true;
                }
            }
        } else {
            const struct Bif *bif = EXPORTED_FUNCTION_TO_BIF(exported_bif);
            switch (arity) {
                case 0: {
                    *return_value = bif->bif0_ptr(ctx);
                    return true;
                }
                case 1: {
                    *return_value = bif->bif1_ptr(ctx, ctx->x[0]);
                    return true;
                }
                case 2: {
                    *return_value = bif->bif2_ptr(ctx, ctx->x[0], ctx->x[1]);
                    return true;
                }
            }
        }
    }

    struct Nif *nif = (struct Nif *) nifs_get(module_name, function_name, arity);
    if (nif) {
        *return_value = nif->nif_ptr(ctx, arity, ctx->x);
        return true;
    }

    return false;
}

#ifdef ENABLE_ADVANCED_TRACE
    static void print_function_args(const Context *ctx, int arity)
    {
        for (int i = 0; i < arity; i++) {
            printf("DBG: <0.%i.0> -- arg%i: ", ctx->process_id, i);
            term_display(stdout, ctx->x[i], ctx);
            printf("\n");
        }
    }

    static void trace_apply(const Context *ctx, const char *call_type, AtomString module_name, AtomString function_name, int arity)
    {
        if (UNLIKELY(ctx->trace_calls)) {
            char module_string[255];
            atom_string_to_c(module_name, module_string, 255);
            char func_string[255];
            atom_string_to_c(function_name, func_string, 255);

            if (ctx->trace_call_args && (arity != 0)) {
                printf("DBG: <0.%i.0> - %s %s:%s/%i:\n", ctx->process_id, call_type, module_string, func_string, arity);
                print_function_args(ctx, arity);
            } else {
                printf("DBG: <0.%i.0> - %s %s:%s/%i.\n", ctx->process_id, call_type, module_string, func_string, arity);
            }
        }
    }

    static void trace_call(const Context *ctx, const Module *mod, const char *call_type, int label, int arity)
    {
        if (UNLIKELY(ctx->trace_calls)) {
            if (ctx->trace_call_args && (arity != 0)) {
                printf("DBG: <0.%i.0> - %s %i:%i/%i:\n", ctx->process_id, call_type, mod->module_index, label, arity);
                print_function_args(ctx, arity);
            } else {
                printf("DBG: <0.%i.0> - %s %i:%i/%i.\n", ctx->process_id, call_type, mod->module_index, label, arity);
            }
        }
    }

    static void trace_call_ext(const Context *ctx, const Module *mod, const char *call_type, int index, int arity)
    {
        if (UNLIKELY(ctx->trace_calls)) {
            AtomString module_name;
            AtomString function_name;
            module_get_imported_function_module_and_name(mod, index, &module_name, &function_name);
            trace_apply(ctx, call_type, module_name, function_name, arity);
        }
    }

    static void trace_return(const Context *ctx)
    {
        if (UNLIKELY(ctx->trace_returns)) {
            printf("DBG: <0.%i.0> - return, value: ", ctx->process_id);
            term_display(stdout, ctx->x[0], ctx);
            printf(".\n");
        }
    }

    static void trace_send(const Context *ctx, term pid, term message)
    {
        if (UNLIKELY(ctx->trace_send)) {
            printf("DBG: <0.%i.0> - send, pid: ", ctx->process_id);
            term_display(stdout, pid, ctx);
            printf(" message: ");
            term_display(stdout, message, ctx);
            printf(".\n");
        }
    }

    static void trace_receive(const Context *ctx, term message)
    {
        if (UNLIKELY(ctx->trace_send)) {
            printf("DBG: <0.%i.0> - receive, message: ", ctx->process_id);
            term_display(stdout, message, ctx);
            printf(".\n");
        }
    }

    #define TRACE_APPLY trace_apply
    #define TRACE_CALL trace_call
    #define TRACE_CALL_EXT trace_call_ext
    #define TRACE_RETURN trace_return
    #define TRACE_SEND trace_send
    #define TRACE_RECEIVE trace_receive
#else
    #define TRACE_APPLY(...)
    #define TRACE_CALL(...)
    #define TRACE_CALL_EXT(...)
    #define TRACE_RETURN(...)
    #define TRACE_SEND(...)
    #define TRACE_RECEIVE(...)
#endif

#endif

#ifndef __clang__
#pragma GCC diagnostic push
#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif
#else
#pragma clang diagnostic push
// Apple Clang 13.0.0 and clang < 13 do not know -Wunused-but-set-variable
#pragma clang diagnostic ignored "-Wunknown-warning-option"
#pragma clang diagnostic ignored "-Wunused-but-set-variable"
#endif

#ifdef IMPL_CODE_LOADER
    int read_core_chunk0(Module *mod);

    int read_core_chunk(Module *mod)
#else
    #ifdef IMPL_EXECUTE_LOOP
    #ifndef DOXYGEN_SKIP_SECTION /* documented in context.h */
        int context_execute_loop(Context *ctx, Module *mod, const char *function_name, int arity)
    #endif /* DOXYGEN_SKIP_SECTION */
    #else
        #error Need implementation type
    #endif
#endif
{
    #ifdef IMPL_CODE_LOADER
        TRACE("-- Loading code\n");
    #endif

    #ifdef IMPL_EXECUTE_LOOP
        TRACE("-- Executing code\n");

        int function_len = strlen(function_name);
        uint8_t *tmp_atom_name = malloc(function_len + 1);
        tmp_atom_name[0] = function_len;
        memcpy(tmp_atom_name + 1, function_name, function_len);

        int label = module_search_exported_function(mod, tmp_atom_name, arity, ctx->global);
        free(tmp_atom_name);

        if (UNLIKELY(!label)) {
            fprintf(stderr, "No %s/%i function found.\n", function_name, arity);
            return 0;
        }

        ctx->saved_module = mod;

        ctx->cp = module_address(mod->module_index, mod->end_instruction_ii);
        ctx->saved_ip = mod->labels[label];
        scheduler_init_ready(ctx);
    #endif

#ifdef IMPL_CODE_LOADER
    return read_core_chunk0(mod);
#endif
#ifdef IMPL_EXECUTE_LOOP
    // This process is the first scheduler process
    #ifndef AVM_NO_SMP
        ctx->global->running_schedulers = 1;
    #endif
    return scheduler_entry_point(ctx->global);
#endif
}

#ifdef IMPL_CODE_LOADER
int read_core_chunk0(Module *mod)
#else
#ifdef IMPL_EXECUTE_LOOP
HOT_FUNC int scheduler_entry_point(GlobalContext *glb)
#else
#error Need implementation type
#endif
#endif
{
#ifdef IMPL_EXECUTE_LOOP
    uint8_t *code;
    Module *mod;
    Module *prev_mod;
    term *x_regs;
    uintptr_t i;
    int remaining_reductions;

    Context *ctx = scheduler_run(glb);

// This is where loop starts after context switching.
schedule_in:
    TRACE("scheduling in, ctx = %p\n", ctx);
    if (ctx == NULL) return 0;
    mod = ctx->saved_module;
    prev_mod = mod;
    code = mod->code->code;
    x_regs = ctx->x;
    JUMP_TO_ADDRESS(ctx->saved_ip);
    remaining_reductions = DEFAULT_REDUCTIONS_AMOUNT;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
    // Handle traps.
    if (ctx->restore_trap_handler)
        goto *ctx->restore_trap_handler;
    // Handle signals
    PROCESS_SIGNAL_MESSAGES();
#pragma GCC diagnostic pop
#endif

#ifdef IMPL_CODE_LOADER
    TRACE("-- Loading code\n");
    SMP_MODULE_LOCK(mod);
    uint8_t *code = mod->code->code;
    uintptr_t i = 0;
#endif

    while (1) {
    TRACE("-- loop -- i = %d\n", (int) i);

        switch (code[i]) {
            case OP_LABEL: {
                uint32_t label;
                int next_off = 1;
                DECODE_LITERAL(label, code, i, next_off)

                TRACE("label/1 label=%i\n", label);
                USED_BY_TRACE(label);

                #ifdef IMPL_CODE_LOADER
                    TRACE("Mark label %i here at %i\n", label, (int) i);
                    module_add_label(mod, label, &code[i]);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_FUNC_INFO: {
                int next_off = 1;
                int module_atom;
                DECODE_ATOM(module_atom, code, i, next_off)
                int function_name_atom;
                DECODE_ATOM(function_name_atom, code, i, next_off)
                uint32_t arity;
                DECODE_LITERAL(arity, code, i, next_off);

                TRACE("func_info/3 module_name_a=%i, function_name_a=%i, arity=%i\n", module_atom, function_name_atom, arity);
                USED_BY_TRACE(function_name_atom);
                USED_BY_TRACE(module_atom);
                USED_BY_TRACE(arity);

                #ifdef IMPL_EXECUTE_LOOP
                    RAISE_ERROR(FUNCTION_CLAUSE_ATOM);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_INT_CALL_END: {
                TRACE("int_call_end!\n");

            #ifdef IMPL_CODE_LOADER
                TRACE("-- Code loading finished --\n");
                SMP_MODULE_UNLOCK(mod);
                return i;
            #endif

            #ifdef IMPL_EXECUTE_LOOP
                goto terminate_context;
            #endif
            }

            case OP_CALL: {
                int next_off = 1;
                uint32_t arity;
                DECODE_LITERAL(arity, code, i, next_off);
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off);

                TRACE("call/2, arity=%i, label=%i\n", arity, label);
                USED_BY_TRACE(arity);

                #ifdef IMPL_EXECUTE_LOOP
                    NEXT_INSTRUCTION(next_off);
                    ctx->cp = module_address(mod->module_index, i);

                    remaining_reductions--;
                    if (LIKELY(remaining_reductions)) {
                        TRACE_CALL(ctx, mod, "call", label, arity);
                        JUMP_TO_ADDRESS(mod->labels[label]);
                    } else {
                        SCHEDULE_NEXT(mod, mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_CALL_LAST: {
                int next_off = 1;
                uint32_t arity;
                DECODE_LITERAL(arity, code, i, next_off);
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off);
                uint32_t n_words;
                DECODE_LITERAL(n_words, code, i, next_off);

                TRACE("call_last/3, arity=%i, label=%i, dellocate=%i\n", arity, label, n_words);
                USED_BY_TRACE(arity);
                USED_BY_TRACE(label);
                USED_BY_TRACE(n_words);

                #ifdef IMPL_EXECUTE_LOOP
                    ctx->cp = ctx->e[n_words];
                    ctx->e += (n_words + 1);

                    DEBUG_DUMP_STACK(ctx);

                    remaining_reductions--;
                    if (LIKELY(remaining_reductions)) {
                        TRACE_CALL(ctx, mod, "call_last", label, arity);
                        JUMP_TO_ADDRESS(mod->labels[label]);
                    } else {
                        SCHEDULE_NEXT(mod, mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_CALL_ONLY: {
                int next_off = 1;
                uint32_t arity;
                DECODE_LITERAL(arity, code, i, next_off);
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)

                TRACE("call_only/2, arity=%i, label=%i\n", arity, label);
                USED_BY_TRACE(arity);
                USED_BY_TRACE(label);

                #ifdef IMPL_EXECUTE_LOOP

                    NEXT_INSTRUCTION(next_off);
                    remaining_reductions--;
                    if (LIKELY(remaining_reductions)) {
                        TRACE_CALL(ctx, mod, "call_only", label, arity);
                        JUMP_TO_ADDRESS(mod->labels[label]);
                    } else {
                        SCHEDULE_NEXT(mod, mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_CALL_EXT: {
                int next_off = 1;
                uint32_t arity;
                DECODE_LITERAL(arity, code, i, next_off);
                uint32_t index;
                DECODE_LITERAL(index, code, i, next_off);

                TRACE("call_ext/2, arity=%i, index=%i\n", arity, index);
                USED_BY_TRACE(arity);
                USED_BY_TRACE(index);


                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    remaining_reductions--;
                    if (UNLIKELY(!remaining_reductions)) {
                        SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                    }

                    // save instruction offset in case of error
                    int orig_i = i;
                    NEXT_INSTRUCTION(next_off);

                    TRACE_CALL_EXT(ctx, mod, "call_ext", index, arity);

                    const struct ExportedFunction *func = module_resolve_function(mod, index, glb);
                    if (IS_NULL_PTR(func)) {
                            RAISE_ERROR(UNDEF_ATOM);
                    }

                    switch (func->type) {
                        case NIFFunctionType: {
                            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
                            term return_value = nif->nif_ptr(ctx, arity, x_regs);
                            PROCESS_MAYBE_TRAP_RETURN_VALUE_RESTORE_I(return_value, orig_i);
                            x_regs[0] = return_value;
                            if (ctx->heap.root->next) {
                                if (UNLIKELY(memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
                                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                                }
                            }
                            break;
                        }
                        case ModuleFunction: {
                            const struct ModuleFunction *jump = EXPORTED_FUNCTION_TO_MODULE_FUNCTION(func);

                            ctx->cp = module_address(mod->module_index, i);
                            JUMP_TO_LABEL(jump->target, jump->label);

                            break;
                        }
                        case BIFFunctionType: {
                            // Support compilers < OTP26 that generate CALL_EXT
                            // for min/2 and max/2
                            const struct Bif *bif = EXPORTED_FUNCTION_TO_BIF(func);
                            switch (arity) {
                                case 0:
                                    x_regs[0] = bif->bif0_ptr(ctx);
                                    break;
                                case 1:
                                    x_regs[0] = bif->bif1_ptr(ctx, x_regs[0]);
                                    break;
                                case 2:
                                    x_regs[0] = bif->bif2_ptr(ctx, x_regs[0], x_regs[1]);
                                    break;
                                default:
                                    fprintf(stderr, "Invalid arity %" PRIu32 " for bif\n", arity);
                            }

                            break;
                        }
                        default: {
                            fprintf(stderr, "Invalid function type %i at index: %" PRIu32 "\n", func->type, index);
                            AVM_ABORT();
                        }
                    }
                #endif

                break;
            }

            case OP_CALL_EXT_LAST: {
                int next_off = 1;
                uint32_t arity;
                DECODE_LITERAL(arity, code, i, next_off);
                uint32_t index;
                DECODE_LITERAL(index, code, i, next_off);
                uint32_t n_words;
                DECODE_LITERAL(n_words, code, i, next_off);

                TRACE("call_ext_last/3, arity=%i, index=%i, n_words=%i\n", arity, index, n_words);
                USED_BY_TRACE(arity);
                USED_BY_TRACE(index);
                USED_BY_TRACE(n_words);

                #ifdef IMPL_EXECUTE_LOOP
                    remaining_reductions--;
                    if (UNLIKELY(!remaining_reductions)) {
                        SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                    }

                    TRACE_CALL_EXT(ctx, mod, "call_ext_last", index, arity);

                    const struct ExportedFunction *func = module_resolve_function(mod, index, glb);
                    if (IS_NULL_PTR(func)) {
                        RAISE_ERROR(UNDEF_ATOM);
                    }

                    switch (func->type) {
                        case NIFFunctionType: {
                            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
                            term return_value = nif->nif_ptr(ctx, arity, x_regs);
                            PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(return_value);
                            x_regs[0] = return_value;

                            // We deallocate after (instead of before) as a
                            // workaround for issue
                            // https://github.com/erlang/otp/issues/7152

                            ctx->cp = ctx->e[n_words];
                            ctx->e += (n_words + 1);

                            if (ctx->heap.root->next) {
                                if (UNLIKELY(memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
                                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                                }
                            }
                            DO_RETURN();

                            break;
                        }
                        case ModuleFunction: {
                            // In the non-nif case, we can deallocate before
                            // (and it doesn't matter as the code below does
                            // not access ctx->e or ctx->cp)

                            ctx->cp = ctx->e[n_words];
                            ctx->e += (n_words + 1);

                            const struct ModuleFunction *jump = EXPORTED_FUNCTION_TO_MODULE_FUNCTION(func);
                            JUMP_TO_LABEL(jump->target, jump->label);

                            break;
                        }
                        case BIFFunctionType: {
                            // Support compilers < OTP26 that generate CALL_EXT_LAST
                            // for min/2 and max/2
                            // These are safe regarding otp issue #7152
                            ctx->cp = ctx->e[n_words];
                            ctx->e += (n_words + 1);

                            const struct Bif *bif = EXPORTED_FUNCTION_TO_BIF(func);
                            switch (arity) {
                                case 0:
                                    x_regs[0] = bif->bif0_ptr(ctx);
                                    break;
                                case 1:
                                    x_regs[0] = bif->bif1_ptr(ctx, x_regs[0]);
                                    break;
                                case 2:
                                    x_regs[0] = bif->bif2_ptr(ctx, x_regs[0], x_regs[1]);
                                    break;
                                default:
                                    fprintf(stderr, "Invalid arity %" PRIu32 " for bif\n", arity);
                            }

                            DO_RETURN();

                            break;
                        }
                        default: {
                            fprintf(stderr, "Invalid function type %i at index: %" PRIu32 "\n", func->type, index);
                            AVM_ABORT();
                        }
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_BIF0: {
                int next_off = 1;
                uint32_t bif;
                DECODE_LITERAL(bif, code, i, next_off);
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                TRACE("bif0/2 bif=%i, dreg=%c%i\n", bif, T_DEST_REG(dreg));
                USED_BY_TRACE(bif);
                USED_BY_TRACE(dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    const struct ExportedFunction *exported_bif = mod->imported_funcs[bif];
                    BifImpl0 func = EXPORTED_FUNCTION_TO_BIF(exported_bif)->bif0_ptr;
                    DEBUG_FAIL_NULL(func);
                    term ret = func(ctx);

                    WRITE_REGISTER(dreg, ret);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            //TODO: implement me
            case OP_BIF1: {
                int next_off = 1;
                uint32_t fail_label;
                DECODE_LABEL(fail_label, code, i, next_off);
                uint32_t bif;
                DECODE_LITERAL(bif, code, i, next_off);
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                TRACE("bif1/2 bif=%i, fail=%i, dreg=%c%i\n", bif, fail_label, T_DEST_REG(dreg));
                USED_BY_TRACE(bif);
                USED_BY_TRACE(dreg);

                #ifdef IMPL_CODE_LOADER
                    UNUSED(arg1);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    const struct ExportedFunction *exported_bif = mod->imported_funcs[bif];
                    BifImpl1 func = EXPORTED_FUNCTION_TO_BIF(exported_bif)->bif1_ptr;
                    DEBUG_FAIL_NULL(func);
                    term ret = func(ctx, arg1);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        HANDLE_ERROR();
                    }

                    WRITE_REGISTER(dreg, ret);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            //TODO: implement me
            case OP_BIF2: {
                int next_off = 1;
                uint32_t fail_label;
                DECODE_LABEL(fail_label, code, i, next_off);
                uint32_t bif;
                DECODE_LITERAL(bif, code, i, next_off);
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off)
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                TRACE("bif2/2 bif=%i, fail=%i, dreg=%c%i\n", bif, fail_label, T_DEST_REG(dreg));
                USED_BY_TRACE(bif);
                USED_BY_TRACE(dreg);

                #ifdef IMPL_CODE_LOADER
                    UNUSED(arg1);
                    UNUSED(arg2);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    const struct ExportedFunction *exported_bif = mod->imported_funcs[bif];
                    BifImpl2 func = EXPORTED_FUNCTION_TO_BIF(exported_bif)->bif2_ptr;
                    DEBUG_FAIL_NULL(func);
                    term ret = func(ctx, arg1, arg2);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        HANDLE_ERROR();
                    }

                    WRITE_REGISTER(dreg, ret);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_ALLOCATE: {
                int next_off = 1;
                uint32_t stack_need;
                DECODE_LITERAL(stack_need, code, i, next_off);
                uint32_t live;
                DECODE_LITERAL(live, code, i, next_off);
                TRACE("allocate/2 stack_need=%i, live=%i\n" , stack_need, live);
                USED_BY_TRACE(stack_need);
                USED_BY_TRACE(live);

                #ifdef IMPL_CODE_LOADER
                    if (live > MAX_REG) {
                        fprintf(stderr, "Cannot use more than %d registers.\n", MAX_REG);
                        AVM_ABORT();
                    }
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    context_clean_registers(ctx, live);

                    if (ctx->heap.root->next || ((ctx->heap.heap_ptr > ctx->e - (stack_need + 1)))) {
                        if (UNLIKELY(memory_ensure_free_opt(ctx, stack_need + 1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    }
                    ctx->e -= stack_need + 1;
                    ctx->e[stack_need] = ctx->cp;
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_ALLOCATE_HEAP: {
                int next_off = 1;
                uint32_t stack_need;
                DECODE_LITERAL(stack_need, code, i, next_off);
                uint32_t heap_need;
                DECODE_ALLOCATOR_LIST(heap_need, code, i, next_off);
                uint32_t live;
                DECODE_LITERAL(live, code, i, next_off);
                TRACE("allocate_heap/2 stack_need=%i, heap_need=%i, live=%i\n", stack_need, heap_need, live);
                USED_BY_TRACE(stack_need);
                USED_BY_TRACE(heap_need);
                USED_BY_TRACE(live);

                #ifdef IMPL_CODE_LOADER
                    if (live > MAX_REG) {
                        fprintf(stderr, "Cannot use more than %d registers.\n", MAX_REG);
                        AVM_ABORT();
                    }
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    context_clean_registers(ctx, live);

                    if (ctx->heap.root->next || ((ctx->heap.heap_ptr + heap_need) > ctx->e - (stack_need + 1))) {
                        if (UNLIKELY(memory_ensure_free_opt(ctx, heap_need + stack_need + 1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    }
                    ctx->e -= stack_need + 1;
                    ctx->e[stack_need] = ctx->cp;
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_ALLOCATE_ZERO: {
                int next_off = 1;
                uint32_t stack_need;
                DECODE_LITERAL(stack_need, code, i, next_off);
                uint32_t live;
                DECODE_LITERAL(live, code, i, next_off);
                TRACE("allocate_zero/2 stack_need=%i, live=%i\n", stack_need, live);
                USED_BY_TRACE(stack_need);
                USED_BY_TRACE(live);

                #ifdef IMPL_CODE_LOADER
                    if (live > MAX_REG) {
                        fprintf(stderr, "Cannot use more than %d registers.\n", MAX_REG);
                        AVM_ABORT();
                    }
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    context_clean_registers(ctx, live);

                    if (ctx->heap.root->next || ((ctx->heap.heap_ptr > ctx->e - (stack_need + 1)))) {
                        if (UNLIKELY(memory_ensure_free_opt(ctx, stack_need + 1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    }

                    ctx->e -= stack_need + 1;
                    for (uint32_t s = 0; s < stack_need; s++) {
                        ctx->e[s] = term_nil();
                    }
                    ctx->e[stack_need] = ctx->cp;
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_ALLOCATE_HEAP_ZERO: {
                int next_off = 1;
                uint32_t stack_need;
                DECODE_LITERAL(stack_need, code, i, next_off);
                uint32_t heap_need;
                DECODE_ALLOCATOR_LIST(heap_need, code, i, next_off);
                uint32_t live;
                DECODE_LITERAL(live, code, i, next_off);
                TRACE("allocate_heap_zero/3 stack_need=%i, heap_need=%i, live=%i\n", stack_need, heap_need, live);
                USED_BY_TRACE(stack_need);
                USED_BY_TRACE(heap_need);
                USED_BY_TRACE(live);

                #ifdef IMPL_CODE_LOADER
                    if (live > MAX_REG) {
                        fprintf(stderr, "Cannot use more than %d registers.\n", MAX_REG);
                        AVM_ABORT();
                    }
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    context_clean_registers(ctx, live);

                    if (ctx->heap.root->next || ((ctx->heap.heap_ptr + heap_need) > ctx->e - (stack_need + 1))) {
                        if (UNLIKELY(memory_ensure_free_opt(ctx, heap_need + stack_need + 1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    }
                    ctx->e -= stack_need + 1;
                    for (uint32_t s = 0; s < stack_need; s++) {
                        ctx->e[s] = term_nil();
                    }
                    ctx->e[stack_need] = ctx->cp;
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TEST_HEAP: {
                int next_off = 1;
                uint32_t heap_need;
                DECODE_ALLOCATOR_LIST(heap_need, code, i, next_off);
                uint32_t live_registers;
                DECODE_LITERAL(live_registers, code, i, next_off);

                TRACE("test_heap/2 heap_need=%i, live_registers=%i\n", heap_need, live_registers);
                USED_BY_TRACE(heap_need);
                USED_BY_TRACE(live_registers);

                #ifdef IMPL_EXECUTE_LOOP
                    size_t heap_free = context_avail_free_memory(ctx);
                    // if we need more heap space than is currently free, then try to GC the needed space
                    if (heap_free < heap_need) {
                        context_clean_registers(ctx, live_registers);
                        if (UNLIKELY(memory_ensure_free_opt(ctx, heap_need, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    // otherwise, there is enough space for the needed heap, but there might
                    // more more than necessary.  In that case, try to shrink the heap.
                    } else if (heap_free > heap_need * HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF) {
                        context_clean_registers(ctx, live_registers);
                        if (UNLIKELY(memory_ensure_free_opt(ctx, heap_need * (HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF / 2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                            TRACE("Unable to ensure free memory.  heap_need=%i\n", heap_need);
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    }
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_KILL: {
                int next_off = 1;
                uint32_t target;
                DECODE_YREG(target, code, i, next_off);

                TRACE("kill/1 target=%i\n", target);

                #ifdef IMPL_EXECUTE_LOOP
                    ctx->e[target] = term_nil();
                #endif

                NEXT_INSTRUCTION(next_off);

                break;
            }

            case OP_DEALLOCATE: {
                int next_off = 1;
                uint32_t n_words;
                DECODE_LITERAL(n_words, code, i, next_off);

                TRACE("deallocate/1 n_words=%i\n", n_words);
                USED_BY_TRACE(n_words);

                #ifdef IMPL_EXECUTE_LOOP
                    DEBUG_DUMP_STACK(ctx);

                    ctx->cp = ctx->e[n_words];
                    ctx->e += n_words + 1;
                    DEBUG_DUMP_STACK(ctx);
                    if (ctx->heap.root->next) {
                        if (UNLIKELY(memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    }
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_RETURN: {
                TRACE("return/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE_RETURN(ctx);

                    if ((long) ctx->cp == -1) {
                        return 0;
                    }

                    DO_RETURN();
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(1);
                #endif
                break;
            }

            //TODO: implement send/0
            case OP_SEND: {
                #ifdef IMPL_CODE_LOADER
                    TRACE("send/0\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    int local_process_id = term_to_local_process_id(x_regs[0]);
                    TRACE("send/0 target_pid=%i\n", local_process_id);
                    TRACE_SEND(ctx, x_regs[0], x_regs[1]);
                    globalcontext_send_message(ctx->global, local_process_id, x_regs[1]);

                    x_regs[0] = x_regs[1];
                #endif

                NEXT_INSTRUCTION(1);
                break;
            }

            case OP_REMOVE_MESSAGE: {
                TRACE("remove_message/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    if (context_get_flags(ctx, WaitingTimeout | WaitingTimeoutExpired)) {
                        scheduler_cancel_timeout(ctx);
                    }
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
                    PROCESS_SIGNAL_MESSAGES();
#pragma GCC diagnostic pop
                    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
                    // Cannot GC now as remove_message is GC neutral
                #endif

                NEXT_INSTRUCTION(1);
                break;
            }

            case OP_TIMEOUT: {
                TRACE("timeout/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    context_update_flags(ctx, ~WaitingTimeoutExpired, NoFlags);

                    mailbox_reset(&ctx->mailbox);
                #endif

                NEXT_INSTRUCTION(1);
                break;
            }

            case OP_LOOP_REC: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                TRACE("loop_rec/2, dreg=%c%i\n", T_DEST_REG(dreg));
                USED_BY_TRACE(dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    term ret;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
                    PROCESS_SIGNAL_MESSAGES();
#pragma GCC diagnostic pop
                    if (mailbox_peek(ctx, &ret)) {
                        TRACE_RECEIVE(ctx, ret);

                        WRITE_REGISTER(dreg, ret);
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        JUMP_TO_ADDRESS(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_LOOP_REC_END: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off);

                TRACE("loop_rec_end/1 label=%i\n", label);
                USED_BY_TRACE(label);

#ifdef IMPL_EXECUTE_LOOP
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
                PROCESS_SIGNAL_MESSAGES();
#pragma GCC diagnostic pop
                mailbox_next(&ctx->mailbox);
                i = POINTER_TO_II(mod->labels[label]);
#else
                NEXT_INSTRUCTION(next_off);
#endif
                break;
            }

            //TODO: implement wait/1
            case OP_WAIT: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)

                TRACE("wait/1\n");

                #ifdef IMPL_EXECUTE_LOOP
                    // When a message is sent, process is moved to ready list
                    // after message is enqueued. So we always schedule out
                    // when executing wait/1 and process will be scheduled in
                    // and the outer list will be processed.
                    ctx->saved_ip = mod->labels[label];
                    ctx->saved_module = mod;
                    ctx = scheduler_wait(ctx);
                    goto schedule_in;
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            //TODO: implement wait_timeout/2
            case OP_WAIT_TIMEOUT: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term timeout;
                DECODE_COMPACT_TERM(timeout, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    avm_int64_t t = 0;
                    if (term_is_any_integer(timeout)) {
                        t = term_maybe_unbox_int64(timeout);
                        if (UNLIKELY(t < 0)) {
                            RAISE_ERROR(TIMEOUT_VALUE_ATOM);
                        }
                    } else if (UNLIKELY(timeout != INFINITY_ATOM)) {
                        RAISE_ERROR(TIMEOUT_VALUE_ATOM);
                    }
                    TRACE("wait_timeout/2, label: %i, timeout: %li\n", label, (long int) t);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
                    PROCESS_SIGNAL_MESSAGES();
#pragma GCC diagnostic pop
                    int needs_to_wait = 0;
                    if (context_get_flags(ctx, WaitingTimeout | WaitingTimeoutExpired) == 0) {
                        if (timeout != INFINITY_ATOM) {
                            scheduler_set_timeout(ctx, t);
                        }
                        needs_to_wait = 1;
                    } else if (context_get_flags(ctx, WaitingTimeout) != 0) {
                        needs_to_wait = 1;
                    } else if (!mailbox_has_next(&ctx->mailbox)) {
                        needs_to_wait = 1;
                    }

                    if (needs_to_wait) {
                        ctx->saved_ip = INSTRUCTION_POINTER();
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
                        ctx->restore_trap_handler = &&wait_timeout_trap_handler;
#pragma GCC diagnostic pop
                        ctx->saved_module = mod;
                        ctx = scheduler_wait(ctx);
                        goto schedule_in;
                    } else {
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("wait_timeout/2, label: %i\n", label);

                    UNUSED(timeout)

                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

#ifdef IMPL_EXECUTE_LOOP
wait_timeout_trap_handler:
            {
                // Determine if a message arrived to either jump to timeout label
                // or to continuation.
                // Redo the offset computation and refetch the label
                int next_off = 1;
                int label;
                DECODE_LABEL(label, code, i, next_off)
                int timeout;
                DECODE_COMPACT_TERM(timeout, code, i, next_off)
                TRACE("wait_timeout_trap_handler, label: %i\n", label);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
                PROCESS_SIGNAL_MESSAGES();
#pragma GCC diagnostic pop
                if (context_get_flags(ctx, WaitingTimeoutExpired)) {
                    ctx->restore_trap_handler = NULL;
                    NEXT_INSTRUCTION(next_off);
                } else {
                    if (UNLIKELY(!mailbox_has_next(&ctx->mailbox))) {
                        // No message is here.
                        // We were signaled for another reason.
                        ctx = scheduler_wait(ctx);
                        goto schedule_in;
                    } else {
                        ctx->restore_trap_handler = NULL;
                        JUMP_TO_ADDRESS(mod->labels[label]);
                    }
                }
                break;
            }
#endif

            case OP_IS_LT: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off);
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_lt/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
                    if (result == TermLessThan) {
                        NEXT_INSTRUCTION(next_off);
                    } else if (result & (TermGreaterThan | TermEquals)) {
                        i = POINTER_TO_II(mod->labels[label]);
                    } else {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_lt/2\n");
                    UNUSED(arg1)
                    UNUSED(arg2)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_GE: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off);
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_ge/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
                    if (result & (TermGreaterThan | TermEquals)) {
                        NEXT_INSTRUCTION(next_off);
                    } else if (result == TermLessThan) {
                        i = POINTER_TO_II(mod->labels[label]);
                    } else {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_ge/2\n");
                    UNUSED(arg1)
                    UNUSED(arg2)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_EQUAL: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_equal/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
                    if (result == TermEquals) {
                        NEXT_INSTRUCTION(next_off);
                    } else if (result & (TermLessThan | TermGreaterThan)) {
                        i = POINTER_TO_II(mod->labels[label]);
                    } else {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_equal/2\n");
                    UNUSED(arg1)
                    UNUSED(arg2)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_NOT_EQUAL: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_not_equal/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
                    if (result & (TermLessThan | TermGreaterThan)) {
                        NEXT_INSTRUCTION(next_off);
                    } else if (result == TermEquals) {
                        i = POINTER_TO_II(mod->labels[label]);
                    } else {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_not_equal/2\n");
                    UNUSED(arg1)
                    UNUSED(arg2)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_EQ_EXACT: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_eq_exact/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    TermCompareResult result = term_compare(arg1, arg2, TermCompareExact, ctx->global);
                    if (result == TermEquals) {
                        NEXT_INSTRUCTION(next_off);
                    } else if (result & (TermLessThan | TermGreaterThan)) {
                        i = POINTER_TO_II(mod->labels[label]);
                    } else {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_eq_exact/2\n");
                    UNUSED(arg1)
                    UNUSED(arg2)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_NOT_EQ_EXACT: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_not_eq_exact/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    TermCompareResult result = term_compare(arg1, arg2, TermCompareExact, ctx->global);
                    if (result & (TermLessThan | TermGreaterThan)) {
                        NEXT_INSTRUCTION(next_off);
                    } else if (result == TermEquals) {
                        i = POINTER_TO_II(mod->labels[label]);
                    } else {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_not_eq_exact/2\n");
                    UNUSED(arg1)
                    UNUSED(arg2)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_INTEGER: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_integer/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_any_integer(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_integer/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_FLOAT: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_float/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_float(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_float/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_NUMBER: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_number/2, label=%i, arg1=%lx\n", label, arg1);

                    //TODO: check for floats too
                    if (term_is_number(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_number/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_BINARY: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_binary/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_binary(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_binary/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_LIST: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_list/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_list(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_list/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_NONEMPTY_LIST: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_nonempty_list/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_nonempty_list(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_nonempty_list/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_NIL: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_nil/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_nil(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_nil/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_ATOM: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_atom/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_atom(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_atom/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_PID: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_pid/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_pid(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_pid/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_REFERENCE: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_reference/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_reference(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_reference/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_PORT: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_port/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_pid(arg1)) {
                        int local_process_id = term_to_local_process_id(arg1);
                        Context *target = globalcontext_get_process_lock(ctx->global, local_process_id);
                        bool is_port_driver = false;
                        if (target) {
                            is_port_driver = context_is_port_driver(target);
                            globalcontext_get_process_unlock(ctx->global, target);
                        }
                        if (is_port_driver) {
                            NEXT_INSTRUCTION(next_off);
                        } else {
                            i = POINTER_TO_II(mod->labels[label]);
                        }
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_port/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_TUPLE: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_tuple/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_tuple(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_tuple/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_TEST_ARITY: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off);
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off);
                uint32_t arity;
                DECODE_LITERAL(arity, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("test_arity/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_tuple(arg1) && (uint32_t) term_get_tuple_arity(arg1) == arity) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = (uint8_t *) mod->labels[label] - code;
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("test_arity/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_SELECT_VAL: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, code, i, next_off)
                uint32_t default_label;
                DECODE_LABEL(default_label, code, i, next_off)
                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                uint32_t size;
                DECODE_LITERAL(size, code, i, next_off)

                TRACE("select_val/3, default_label=%i, vals=%i\n", default_label, size);
                USED_BY_TRACE(default_label);
                USED_BY_TRACE(size);

                #ifdef IMPL_CODE_LOADER
                    UNUSED(src_value);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    void *jump_to_address = NULL;
                #endif

                for (uint32_t j = 0; j < size / 2; j++) {
                    term cmp_value;
                    DECODE_COMPACT_TERM(cmp_value, code, i, next_off)
                    uint32_t jmp_label;
                    DECODE_LABEL(jmp_label, code, i, next_off)

                    #ifdef IMPL_CODE_LOADER
                        UNUSED(cmp_value);
                    #endif

                    #ifdef IMPL_EXECUTE_LOOP
                        if (!jump_to_address && (src_value == cmp_value)) {
                            jump_to_address = mod->labels[jmp_label];
                        }
                    #endif
                }

                #ifdef IMPL_EXECUTE_LOOP
                    if (!jump_to_address) {
                        JUMP_TO_ADDRESS(mod->labels[default_label]);
                    } else {
                        JUMP_TO_ADDRESS(jump_to_address);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_SELECT_TUPLE_ARITY: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, code, i, next_off)
                uint32_t default_label;
                DECODE_LABEL(default_label, code, i, next_off)
                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                uint32_t size;
                DECODE_LITERAL(size, code, i, next_off)

                TRACE("select_tuple_arity/3, default_label=%i, vals=%i\n", default_label, size);
                USED_BY_TRACE(default_label);
                USED_BY_TRACE(size);

                #ifdef IMPL_CODE_LOADER
                    UNUSED(src_value);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    void *jump_to_address = NULL;
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                if (LIKELY(term_is_tuple(src_value))) {
                    int arity = term_get_tuple_arity(src_value);
                #endif

                    for (uint32_t j = 0; j < size / 2; j++) {
                        uint32_t cmp_value;
                        DECODE_LITERAL(cmp_value, code, i, next_off)
                        uint32_t jmp_label;
                        DECODE_LABEL(jmp_label, code, i, next_off)

                        #ifdef IMPL_CODE_LOADER
                            UNUSED(cmp_value);
                        #endif

                        #ifdef IMPL_EXECUTE_LOOP
                            //TODO: check if src_value is a tuple
                            if (!jump_to_address && ((uint32_t) arity == cmp_value)) {
                                jump_to_address = mod->labels[jmp_label];
                            }
                        #endif
                    }
                #ifdef IMPL_EXECUTE_LOOP
                }
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    if (!jump_to_address) {
                        JUMP_TO_ADDRESS(mod->labels[default_label]);
                    } else {
                        JUMP_TO_ADDRESS(jump_to_address);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_JUMP: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)

                TRACE("jump/1 label=%i\n", label);
                USED_BY_TRACE(label);

                #ifdef IMPL_EXECUTE_LOOP
                    remaining_reductions--;
                    if (LIKELY(remaining_reductions)) {
                        JUMP_TO_ADDRESS(mod->labels[label]);
                    } else {
                        ctx->saved_ip = mod->labels[label];
                        ctx->saved_module = mod;
                        ctx = scheduler_next(ctx->global, ctx);
                        goto schedule_in;
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_MOVE: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, code, i, next_off);
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("move/2 %lx, %c%i\n", src_value, T_DEST_REG(dreg));

                    WRITE_REGISTER(dreg, src_value);
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("move/2\n");
                    UNUSED(src_value)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_GET_LIST: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, code, i, next_off)
                dreg_t head_dreg;
                DECODE_DEST_REGISTER(head_dreg, code, i, next_off);
                dreg_t tail_dreg;
                DECODE_DEST_REGISTER(tail_dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("get_list/3 %lx, %c%i, %c%i\n", src_value, T_DEST_REG(head_dreg), T_DEST_REG(tail_dreg));

                    term head = term_get_list_head(src_value);
                    term tail = term_get_list_tail(src_value);

                    WRITE_REGISTER(head_dreg, head);
                    WRITE_REGISTER(tail_dreg, tail);
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("get_list/2\n");
                    UNUSED(src_value)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_GET_TUPLE_ELEMENT: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, code, i, next_off);
                uint32_t element;
                DECODE_LITERAL(element, code, i, next_off);
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                TRACE("get_tuple_element/2, element=%i, dest=%c%i\n", element, T_DEST_REG(dreg));
                USED_BY_TRACE(element);

                #ifdef IMPL_EXECUTE_LOOP
                    if (UNLIKELY(!term_is_tuple(src_value) || (element >= (uint32_t) term_get_tuple_arity(src_value)))) {
                        AVM_ABORT();
                    }

                    term t = term_get_tuple_element(src_value, element);
                    WRITE_REGISTER(dreg, t);
                #endif

                #ifdef IMPL_CODE_LOADER
                    UNUSED(src_value)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_SET_TUPLE_ELEMENT: {
                int next_off = 1;
                term new_element;
                DECODE_COMPACT_TERM(new_element, code, i, next_off);
                term tuple;
                DECODE_COMPACT_TERM(tuple, code, i, next_off);
                uint32_t position;
                DECODE_LITERAL(position, code, i, next_off);

                TRACE("set_tuple_element/2\n");

#ifdef IMPL_EXECUTE_LOOP
                if (UNLIKELY(!term_is_tuple(tuple) || (position >= (uint32_t) term_get_tuple_arity(tuple)))) {
                    AVM_ABORT();
                }

                term_put_tuple_element(tuple, position, new_element);
#endif

#ifdef IMPL_CODE_LOADER
                UNUSED(tuple);
                UNUSED(position);
                UNUSED(new_element);
#endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_PUT_LIST: {

                int next_off = 1;
                term head;
                DECODE_COMPACT_TERM(head, code, i, next_off);
                term tail;
                DECODE_COMPACT_TERM(tail, code, i, next_off);

#ifdef IMPL_EXECUTE_LOOP
                term *list_elem = term_list_alloc(&ctx->heap);
#endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("put_list/3\n");
                    UNUSED(head);
                    UNUSED(tail);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("put_list/3 dreg=%c%i\n", T_DEST_REG(dreg));
                    term t = term_list_init_prepend(list_elem, head, tail);
                    WRITE_REGISTER(dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_PUT_TUPLE: {
                int next_off = 1;
                uint32_t size;
                DECODE_LITERAL(size, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    term t = term_alloc_tuple(size, &ctx->heap);
                #endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                TRACE("put_tuple/2 size=%u, dest=%c%i\n", (unsigned) size, T_DEST_REG(dreg));
                USED_BY_TRACE(dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    WRITE_REGISTER(dreg, t);
                #endif

                for (uint32_t j = 0; j < size; j++) {
                    if (code[i + next_off] != OP_PUT) {
                        fprintf(stderr, "Expected put, got opcode: %i\n", code[i + next_off]);
                        AVM_ABORT();
                    }
                    next_off++;
                    term put_value;
                    DECODE_COMPACT_TERM(put_value, code, i, next_off);
                    #ifdef IMPL_CODE_LOADER
                        TRACE("put/2\n");
                        UNUSED(put_value);
                    #endif

                    #ifdef IMPL_EXECUTE_LOOP
                        TRACE("put/2 elem=%i, value=0x%lx\n", j, put_value);
                        term_put_tuple_element(t, j, put_value);
                    #endif
                }

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BADMATCH: {
                #ifdef IMPL_EXECUTE_LOOP
                    // We can gc as we are raising
                    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                int next_off = 1;
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("badmatch/1\n");
                    USED_BY_TRACE(arg1);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("badmatch/1, v=0x%lx\n", arg1);

                    term new_error_tuple = term_alloc_tuple(2, &ctx->heap);
                    term_put_tuple_element(new_error_tuple, 0, BADMATCH_ATOM);
                    term_put_tuple_element(new_error_tuple, 1, arg1);

                    RAISE_ERROR(new_error_tuple);
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IF_END: {
                TRACE("if_end/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    x_regs[0] = ERROR_ATOM;
                    x_regs[1] = IF_CLAUSE_ATOM;

                    RAISE_ERROR(IF_CLAUSE_ATOM);
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(1);
                #endif

                break;
            }

            case OP_CASE_END: {
                #ifdef IMPL_EXECUTE_LOOP
                    // We can gc as we are raising
                    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                int next_off = 1;
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("case_end/1\n");
                    USED_BY_TRACE(arg1);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("case_end/1, v=0x%lx\n", arg1);

                    term new_error_tuple = term_alloc_tuple(2, &ctx->heap);
                    term_put_tuple_element(new_error_tuple, 0, CASE_CLAUSE_ATOM);
                    term_put_tuple_element(new_error_tuple, 1, arg1);

                    RAISE_ERROR(new_error_tuple);
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_CALL_FUN: {
                int next_off = 1;
                uint32_t args_count;
                DECODE_LITERAL(args_count, code, i, next_off)

                TRACE("call_fun/1, args_count=%i\n", args_count);
                USED_BY_TRACE(args_count);

                #ifdef IMPL_EXECUTE_LOOP
                    remaining_reductions--;
                    if (UNLIKELY(!remaining_reductions)) {
                        SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                    }

                    term fun = x_regs[args_count];
                    if (UNLIKELY(!term_is_function(fun))) {
                        // We can gc as we are raising
                        if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                        term new_error_tuple = term_alloc_tuple(2, &ctx->heap);
                        term_put_tuple_element(new_error_tuple, 0, BADFUN_ATOM);
                        term_put_tuple_element(new_error_tuple, 1, x_regs[args_count]);
                        RAISE_ERROR(new_error_tuple);
                    }

                    CALL_FUN(fun, args_count, next_off)
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

           case OP_IS_FUNCTION: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_function/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_function(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_function/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_CALL_EXT_ONLY: {
                int next_off = 1;
                uint32_t arity;
                DECODE_LITERAL(arity, code, i, next_off);
                uint32_t index;
                DECODE_LITERAL(index, code, i, next_off);

                TRACE("call_ext_only/2, arity=%i, index=%i\n", arity, index);
                USED_BY_TRACE(arity);
                USED_BY_TRACE(index);

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    remaining_reductions--;
                    if (UNLIKELY(!remaining_reductions)) {
                        SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                    }

                    TRACE_CALL_EXT(ctx, mod, "call_ext_only", index, arity);

                    const struct ExportedFunction *func = module_resolve_function(mod, index, glb);
                    if (IS_NULL_PTR(func)) {
                        RAISE_ERROR(UNDEF_ATOM);
                    }

                    switch (func->type) {
                        case NIFFunctionType: {
                            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
                            term return_value = nif->nif_ptr(ctx, arity, x_regs);
                            PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(return_value);
                            x_regs[0] = return_value;

                            if (ctx->heap.root->next) {
                                if (UNLIKELY(memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
                                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                                }
                            }
                            if ((long) ctx->cp == -1) {
                                return 0;
                            }

                            DO_RETURN();

                            break;
                        }
                        case ModuleFunction: {
                            const struct ModuleFunction *jump = EXPORTED_FUNCTION_TO_MODULE_FUNCTION(func);
                            JUMP_TO_LABEL(jump->target, jump->label);

                            break;
                        }
                        case BIFFunctionType: {
                            // Support compilers < OTP26 that generate CALL_EXT_ONLY
                            // for min/2 and max/2
                            const struct Bif *bif = EXPORTED_FUNCTION_TO_BIF(func);
                            switch (arity) {
                                case 0:
                                    x_regs[0] = bif->bif0_ptr(ctx);
                                    break;
                                case 1:
                                    x_regs[0] = bif->bif1_ptr(ctx, x_regs[0]);
                                    break;
                                case 2:
                                    x_regs[0] = bif->bif2_ptr(ctx, x_regs[0], x_regs[1]);
                                    break;
                                default:
                                    fprintf(stderr, "Invalid arity %" PRIu32 " for bif\n", arity);
                            }

                            DO_RETURN();

                            break;
                        }
                        default: {
                            AVM_ABORT();
                        }
                    }
                #endif

                break;
            }

            case OP_MAKE_FUN2: {
                int next_off = 1;
                uint32_t fun_index;
                DECODE_LITERAL(fun_index, code, i, next_off)

                TRACE("make_fun/2, fun_index=%i\n", fun_index);
                #ifdef IMPL_EXECUTE_LOOP
                    term f = make_fun(ctx, mod, fun_index);
                    if (term_is_invalid_term(f)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    } else {
                        x_regs[0] = f;
                    }
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TRY: {
                int next_off = 1;
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)

                TRACE("try/2, label=%i, reg=%c%i\n", label, T_DEST_REG(dreg));

                #ifdef IMPL_EXECUTE_LOOP
                    term catch_term = term_from_catch_label(mod->module_index, label);
                    //TODO: here just write to y registers is enough
                    WRITE_REGISTER(dreg, catch_term);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TRY_END: {
                int next_off = 1;
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                TRACE("try_end/1, reg=%c%i\n", T_DEST_REG(dreg));

                #ifdef IMPL_EXECUTE_LOOP
                    //TODO: here just write to y registers is enough
                    WRITE_REGISTER(dreg, term_nil());
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TRY_CASE: {
                int next_off = 1;
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                TRACE("try_case/1, reg=%c%i\n", T_DEST_REG(dreg));

                #ifdef IMPL_EXECUTE_LOOP
                    // clears the catch value on stack
                    WRITE_REGISTER(dreg, term_nil());
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TRY_CASE_END: {
                #ifdef IMPL_EXECUTE_LOOP
                    // We can gc as we are raising
                    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                int next_off = 1;
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("try_case_end/1\n");
                    UNUSED(arg1);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("try_case_end/1, val=%lx\n", arg1);

                    term new_error_tuple = term_alloc_tuple(2, &ctx->heap);
                    term_put_tuple_element(new_error_tuple, 0, TRY_CLAUSE_ATOM);
                    term_put_tuple_element(new_error_tuple, 1, arg1);

                    RAISE_ERROR(new_error_tuple);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_RAISE: {
                int next_off = 1;
                term stacktrace;
                DECODE_COMPACT_TERM(stacktrace, code, i, next_off);
                term exc_value;
                DECODE_COMPACT_TERM(exc_value, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("raise/2\n");
                    UNUSED(stacktrace);
                    UNUSED(exc_value);
                    NEXT_INSTRUCTION(next_off);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("raise/2 stacktrace=0x%lx exc_value=0x%lx\n", stacktrace, exc_value);
                    x_regs[0] = stacktrace_exception_class(stacktrace);
                    x_regs[1] = exc_value;
                    x_regs[2] = stacktrace_create_raw(ctx, mod, i, x_regs[0]);
                    goto handle_error;
                #endif

                break;
            }

            case OP_CATCH: {
                int next_off = 1;
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)

                TRACE("catch/2, label=%i, reg=%c%i\n", label, T_DEST_REG(dreg));

                #ifdef IMPL_EXECUTE_LOOP
                    term catch_term = term_from_catch_label(mod->module_index, label);
                    // TODO: here just write to y registers is enough
                    WRITE_REGISTER(dreg, catch_term);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_CATCH_END: {
                int next_off = 1;
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                TRACE("catch_end/1, reg=%c%i\n", T_DEST_REG(dreg));

                #ifdef IMPL_EXECUTE_LOOP
                    // TODO: here just write to y registers is enough
                    WRITE_REGISTER(dreg, term_nil());
                    // C.f. https://www.erlang.org/doc/reference_manual/expressions.html#catch-and-throw
                    switch (term_to_atom_index(x_regs[0])) {
                        case THROW_ATOM_INDEX:
                            x_regs[0] = x_regs[1];
                            break;

                        case ERROR_ATOM_INDEX: {
                            x_regs[2] = stacktrace_build(ctx, &x_regs[2]);
                            // MEMORY_CAN_SHRINK because catch_end is classified as gc in beam_ssa_codegen.erl
                            if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) * 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                            }
                            term reason_tuple = term_alloc_tuple(2, &ctx->heap);
                            term_put_tuple_element(reason_tuple, 0, x_regs[1]);
                            term_put_tuple_element(reason_tuple, 1, x_regs[2]);
                            term exit_tuple = term_alloc_tuple(2, &ctx->heap);
                            term_put_tuple_element(exit_tuple, 0, EXIT_ATOM);
                            term_put_tuple_element(exit_tuple, 1, reason_tuple);
                            x_regs[0] = exit_tuple;

                            break;
                        }
                        case LOWERCASE_EXIT_ATOM_INDEX: {
                            // MEMORY_CAN_SHRINK because catch_end is classified as gc in beam_ssa_codegen.erl
                            if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                            }
                            term exit_tuple = term_alloc_tuple(2, &ctx->heap);
                            term_put_tuple_element(exit_tuple, 0, EXIT_ATOM);
                            term_put_tuple_element(exit_tuple, 1, x_regs[1]);
                            x_regs[0] = exit_tuple;

                            break;
                        }
                    }
#endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_ADD: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src1;
                DECODE_COMPACT_TERM(src1, code, i, next_off);
                term src2;
                DECODE_COMPACT_TERM(src2, code, i, next_off);
                uint32_t unit;
                DECODE_LITERAL(unit, code, i, next_off)
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_add/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_INTEGER(src1, "bs_add");
                    VERIFY_IS_INTEGER(src2, "bs_add");
                    avm_int_t src1_val = term_to_int(src1);
                    avm_int_t src2_val = term_to_int(src2);

                    TRACE("bs_add/5, fail=%i src1=%li src2=%li unit=%u dreg=%c%i\n", fail, src1_val, src2_val, (unsigned) unit, T_DEST_REG(dreg));

                    WRITE_REGISTER(dreg, term_from_int((src1_val + src2_val) * unit));
                #endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_INIT2: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off)
                uint32_t words;
                DECODE_LITERAL(words, code, i, next_off)
                uint32_t regs;
                UNUSED(regs);
                DECODE_LITERAL(regs, code, i, next_off)
                term flags;
                UNUSED(flags);
                DECODE_COMPACT_TERM(flags, code, i, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_init2/6\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_INTEGER(size, "bs_init2");
                    avm_int_t size_val = term_to_int(size);

                    if (UNLIKELY(memory_ensure_free_opt(ctx, words + term_binary_heap_size(size_val), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    term t = term_create_empty_binary(size_val, &ctx->heap, ctx->global);

                    ctx->bs = t;
                    ctx->bs_offset = 0;
                #endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_init2/6, fail=%u size=%li words=%u regs=%u dreg=%c%i\n", (unsigned) fail, size_val, (unsigned) words, (unsigned) regs, T_DEST_REG(dreg));
                    WRITE_REGISTER(dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_INIT_BITS: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off)
                uint32_t words;
                DECODE_LITERAL(words, code, i, next_off)
                uint32_t regs;
                DECODE_LITERAL(regs, code, i, next_off)
                uint32_t flags_value;
                DECODE_LITERAL(flags_value, code, i, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_init_bits/6\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_INTEGER(size, "bs_init_bits");
                    avm_int_t size_val = term_to_int(size);
                    if (size_val % 8 != 0) {
                        TRACE("bs_init_bits: size_val (%li) is not evenly divisible by 8\n", size_val);
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    if (flags_value != 0) {
                        TRACE("bs_init_bits: neither signed nor native or little endian encoding supported.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    if (UNLIKELY(memory_ensure_free_opt(ctx, words + term_binary_heap_size(size_val / 8), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    term t = term_create_empty_binary(size_val / 8, &ctx->heap, ctx->global);

                    ctx->bs = t;
                    ctx->bs_offset = 0;
                #endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_init_bits/6, fail=%i size=%li words=%i regs=%i dreg=%c%i\n", fail, size_val, words, regs, T_DEST_REG(dreg));
                    WRITE_REGISTER(dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_UTF8_SIZE: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off)
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);
                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_utf8_size/3");
                #endif
                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_INTEGER(src, "bs_utf8_size/3");
                    avm_int_t src_value = term_to_int(src);
                    TRACE("bs_utf8_size/3 fail=%i src=0x%lx dreg=%c%i\n", fail, (long) src_value, T_DEST_REG(dreg));
                    size_t utf8_size;
                    if (UNLIKELY(!bitstring_utf8_size(src_value, &utf8_size))) {
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    WRITE_REGISTER(dreg, term_from_int(utf8_size));
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_PUT_UTF8: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                uint32_t flags;
                DECODE_LITERAL(flags, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off)
                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_put_utf8/3 flags=%x\n", (int) flags);
                    if (flags != 0) {
                        fprintf(stderr, "bs_put_utf8/3 : unsupported flags %x\n", (int) flags);
                        AVM_ABORT();
                    }
                #endif
                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_INTEGER(src, "bs_put_utf8/3");
                    avm_int_t src_value = term_to_int(src);
                    TRACE("bs_put_utf8/3 flags=%x, src=0x%lx\n", (int) flags, (long) src_value);
                    if (UNLIKELY(!term_is_binary(ctx->bs))) {
                        TRACE("bs_put_utf8/3: Bad state.  ctx->bs is not a binary.\n");
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    if (ctx->bs_offset % 8 != 0) {
                        TRACE("bs_put_utf8/3: Unsupported bit syntax operation.  Writing strings must be byte-aligend.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    size_t byte_size;
                    bool result = bitstring_insert_utf8(ctx->bs, ctx->bs_offset, src_value, &byte_size);
                    if (UNLIKELY(!result)) {
                        TRACE("bs_put_utf8/3: Failed to insert character as utf8 into binary: %i\n", result);
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    ctx->bs_offset += byte_size * 8;
                #endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_GET_UTF8: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);
                term arg3;
                DECODE_COMPACT_TERM(arg3, code, i, next_off);
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_get_utf8/5\n");
                    NEXT_INSTRUCTION(next_off);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_get_utf8/5, fail=%i src=0x%lx arg2=0x%lx arg3=0x%lx dreg=%c%i\n", fail, src, arg2, arg3, T_DEST_REG(dreg));

                    VERIFY_IS_MATCH_STATE(src, "bs_get_utf8");

                    term src_bin = term_get_match_state_binary(src);
                    avm_int_t offset_bits = term_get_match_state_offset(src);

                    uint32_t val = 0;
                    size_t out_size = 0;
                    bool is_valid = bitstring_match_utf8(src_bin, (size_t) offset_bits, &val, &out_size);

                    if (!is_valid) {
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term_set_match_state_offset(src, offset_bits + (out_size * 8));
                        WRITE_REGISTER(dreg, term_from_int(val));
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                break;
            }

            case OP_BS_SKIP_UTF8: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);
                term arg3;
                DECODE_COMPACT_TERM(arg3, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_skip_utf8/4\n");
                    NEXT_INSTRUCTION(next_off);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_skip_utf8/4, fail=%i src=0x%lx arg2=0x%lx arg3=0x%lx\n", fail, src, arg2, arg3);

                    VERIFY_IS_MATCH_STATE(src, "bs_get_utf8");

                    term src_bin = term_get_match_state_binary(src);
                    avm_int_t offset_bits = term_get_match_state_offset(src);

                    uint32_t c = 0;
                    size_t out_size = 0;
                    bool is_valid = bitstring_match_utf8(src_bin, (size_t) offset_bits, &c, &out_size);

                    if (!is_valid) {
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term_set_match_state_offset(src, offset_bits + (out_size * 8));
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                break;
            }

           case OP_BS_UTF16_SIZE: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off)
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);
                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_utf16_size/3");
                #endif
                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_INTEGER(src, "bs_utf16_size/3");
                    avm_int_t src_value = term_to_int(src);
                    TRACE("bs_utf16_size/3 fail=%i src=0x%lx dreg=%c%i\n", fail, (long) src_value, T_DEST_REG(dreg));
                    size_t utf16_size;
                    if (UNLIKELY(!bitstring_utf16_size(src_value, &utf16_size))) {
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    WRITE_REGISTER(dreg, term_from_int(utf16_size));
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_PUT_UTF16: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                uint32_t flags;
                DECODE_LITERAL(flags, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off)
                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_put_utf16/3 flags=%x\n", (int) flags);
                    if (flags != 0 && flags != LittleEndianInteger && flags != NativeEndianInteger) {
                        fprintf(stderr, "bs_put_utf16/3 : unsupported flags %x\n", (int) flags);
                        AVM_ABORT();
                    }
                #endif
                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_INTEGER(src, "bs_put_utf16/3");
                    avm_int_t src_value = term_to_int(src);
                    TRACE("bs_put_utf16/3 flags=%x, src=0x%lx\n", (int) flags, src_value);
                    if (UNLIKELY(!term_is_binary(ctx->bs))) {
                        TRACE("bs_put_utf16: Bad state.  ctx->bs is not a binary.\n");
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    if (ctx->bs_offset % 8 != 0) {
                        TRACE("bs_put_utf16: Unsupported bit syntax operation.  Writing strings must be byte-aligend.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    size_t byte_size;
                    bool result = bitstring_insert_utf16(ctx->bs, ctx->bs_offset, src_value, flags, &byte_size);
                    if (UNLIKELY(!result)) {
                        TRACE("bs_put_utf8/3: Failed to insert character as utf8 into binary: %i\n", result);
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    ctx->bs_offset += byte_size * 8;
                #endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_GET_UTF16: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);
                term flags;
                DECODE_LITERAL(flags, code, i, next_off);
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_get_utf16/5\n");
                    NEXT_INSTRUCTION(next_off);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_get_utf16/5, fail=%i src=0x%lx arg2=0x%lx flags=0x%lx dreg=%c%i\n", fail, src, arg2, flags, T_DEST_REG(dreg));

                    VERIFY_IS_MATCH_STATE(src, "bs_get_utf16");

                    term src_bin = term_get_match_state_binary(src);
                    avm_int_t offset_bits = term_get_match_state_offset(src);

                    int32_t val = 0;
                    size_t out_size = 0;
                    bool is_valid = bitstring_match_utf16(src_bin, (size_t) offset_bits, &val, &out_size, flags);

                    if (!is_valid) {
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term_set_match_state_offset(src, offset_bits + (out_size * 8));
                        WRITE_REGISTER(dreg, term_from_int(val));
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                break;
            }

            case OP_BS_SKIP_UTF16: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);
                term flags;
                DECODE_LITERAL(flags, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_skip_utf16/5\n");
                    NEXT_INSTRUCTION(next_off);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_skip_utf16/5, fail=%i src=0x%lx arg2=0x%lx flags=0x%lx\n", fail, src, arg2, flags);

                    VERIFY_IS_MATCH_STATE(src, "bs_skip_utf16");

                    term src_bin = term_get_match_state_binary(src);
                    avm_int_t offset_bits = term_get_match_state_offset(src);

                    int32_t val = 0;
                    size_t out_size = 0;
                    bool is_valid = bitstring_match_utf16(src_bin, (size_t) offset_bits, &val, &out_size, flags);

                    if (!is_valid) {
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term_set_match_state_offset(src, offset_bits + (out_size * 8));
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                break;
            }

            case OP_BS_PUT_UTF32: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                uint32_t flags;
                DECODE_LITERAL(flags, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off)
                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_put_utf32/3 flags=%x\n", (int) flags);
                    if (flags != 0 && flags != LittleEndianInteger && flags != NativeEndianInteger) {
                        fprintf(stderr, "bs_put_utf32/3 : unsupported flags %x\n", (int) flags);
                        AVM_ABORT();
                    }
                #endif
                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_INTEGER(src, "bs_put_utf32/3");
                    avm_int_t src_value = term_to_int(src);
                    TRACE("bs_put_utf32/3 flags=%x, src=0x%lx\n", (int) flags, (long) src_value);
                    if (UNLIKELY(!term_is_binary(ctx->bs))) {
                        TRACE("bs_put_utf32/3: Bad state.  ctx->bs is not a binary.\n");
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    if (ctx->bs_offset % 8 != 0) {
                        TRACE("bs_put_utf32/3: Unsupported bit syntax operation.  Writing strings must be byte-aligend.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    bool result = bitstring_insert_utf32(ctx->bs, ctx->bs_offset, src_value, flags);
                    if (UNLIKELY(!result)) {
                        TRACE("bs_put_utf32/3: Failed to insert integer into binary: %i\n", result);
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    ctx->bs_offset += 4 * 8;
                #endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_GET_UTF32: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);
                term flags;
                DECODE_LITERAL(flags, code, i, next_off);
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_get_utf32/5\n");
                    NEXT_INSTRUCTION(next_off);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_get_utf32/5, fail=%i src=0x%lx arg2=0x%lx flags=0x%lx dreg=%c%i\n", fail, src, arg2, flags, T_DEST_REG(dreg));

                    VERIFY_IS_MATCH_STATE(src, "bs_get_utf32");

                    term src_bin = term_get_match_state_binary(src);
                    avm_int_t offset_bits = term_get_match_state_offset(src);

                    int32_t val = 0;
                    bool is_valid = bitstring_match_utf32(src_bin, (size_t) offset_bits, &val, flags);

                    if (!is_valid) {
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term_set_match_state_offset(src, offset_bits + 32);
                        WRITE_REGISTER(dreg, term_from_int(val));
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                break;
            }

            case OP_BS_SKIP_UTF32: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);
                term flags;
                DECODE_LITERAL(flags, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_skip_utf32/5\n");
                    NEXT_INSTRUCTION(next_off);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_skip_utf32/5, fail=%i src=0x%lx arg2=0x%lx flags=0x%lx\n", fail, src, arg2, flags);

                    VERIFY_IS_MATCH_STATE(src, "bs_skip_utf32");

                    term src_bin = term_get_match_state_binary(src);
                    avm_int_t offset_bits = term_get_match_state_offset(src);

                    int32_t val = 0;
                    bool is_valid = bitstring_match_utf32(src_bin, (size_t) offset_bits, &val, flags);

                    if (!is_valid) {
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term_set_match_state_offset(src, offset_bits + 32);
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                break;
            }

            case OP_BS_INIT_WRITABLE: {
                int next_off = 1;

                TRACE("bs_init_writable/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    if (UNLIKELY(memory_ensure_free_opt(ctx, term_binary_heap_size(0), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    term t = term_create_empty_binary(0, &ctx->heap, ctx->global);

                    ctx->bs = t;
                    ctx->bs_offset = 0;
                    x_regs[0] = t;
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_APPEND: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off)
                term extra;
                UNUSED(extra);
                DECODE_COMPACT_TERM(extra, code, i, next_off)
                term live;
                UNUSED(live);
                DECODE_COMPACT_TERM(live, code, i, next_off)
                uint32_t unit;
                DECODE_LITERAL(unit, code, i, next_off);
                term src;
                #ifdef IMPL_EXECUTE_LOOP
                    int src_off = next_off;
                #endif
                DECODE_COMPACT_TERM(src, code, i, next_off)
                term flags;
                UNUSED(flags);
                DECODE_COMPACT_TERM(flags, code, i, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_append/8\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_BINARY(src, "bs_append");
                    VERIFY_IS_INTEGER(size, "bs_append");
                    VERIFY_IS_INTEGER(extra, "bs_append");
                    avm_int_t size_val = term_to_int(size);
                    avm_int_t extra_val = term_to_int(extra);

                    if (size_val % 8 != 0) {
                        TRACE("bs_append: size_val (%li) is not evenly divisible by 8\n", size_val);
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    if (unit != 8) {
                        TRACE("bs_append: unit is not equal to 8; unit=%u\n", (unsigned) unit);
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    size_t src_size = term_binary_size(src);
                    // TODO: further investigate extra_val
                    if (UNLIKELY(memory_ensure_free_opt(ctx, src_size + term_binary_heap_size(size_val / 8) + extra_val, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_append/8, fail=%u size=%li unit=%u src=0x%lx dreg=%c%i\n", (unsigned) fail, size_val, (unsigned) unit, src, T_DEST_REG(dreg));
                    DECODE_COMPACT_TERM(src, code, i, src_off)
                    term t = term_create_empty_binary(src_size + size_val / 8, &ctx->heap, ctx->global);
                    memcpy((void *) term_binary_data(t), (void *) term_binary_data(src), src_size);

                    ctx->bs = t;
                    ctx->bs_offset = src_size * 8;

                    WRITE_REGISTER(dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_PRIVATE_APPEND: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off)
                uint32_t unit;
                DECODE_LITERAL(unit, code, i, next_off);
                term src;
                #ifdef IMPL_EXECUTE_LOOP
                    int src_off = next_off;
                #endif
                DECODE_COMPACT_TERM(src, code, i, next_off)
                term flags;
                UNUSED(flags);
                DECODE_COMPACT_TERM(flags, code, i, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_private_append/6\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_BINARY(src, "bs_private_append");
                    VERIFY_IS_INTEGER(size, "bs_private_append");
                    avm_int_t size_val = term_to_int(size);

                    if (size_val % 8 != 0) {
                        TRACE("bs_private_append: size_val (%li) is not evenly divisible by 8\n", (long int) size_val);
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    // TODO: further investigate unit.
                    // We currently do not support unaligned binaries, unit seems to be equal to 1 binary comprehensions
                    size_t src_size = term_binary_size(src);
                    if (UNLIKELY(memory_ensure_free_opt(ctx, src_size + term_binary_heap_size(size_val / 8), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    DECODE_COMPACT_TERM(src, code, i, src_off)
                    term t = term_create_empty_binary(src_size + size_val / 8, &ctx->heap, ctx->global);
                    memcpy((void *) term_binary_data(t), (void *) term_binary_data(src), src_size);

                    ctx->bs = t;
                    ctx->bs_offset = src_size * 8;
                #endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_private_append/6, fail=%u size=%li unit=%u src=0x%lx dreg=%c%i\n", (unsigned) fail, size_val, (unsigned) unit, src, T_DEST_REG(dreg));
                    WRITE_REGISTER(dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_PUT_INTEGER: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off)
                uint32_t unit;
                DECODE_LITERAL(unit, code, i, next_off);
                uint32_t flags_value;
                DECODE_LITERAL(flags_value, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_put_integer/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_ANY_INTEGER(src, "bs_put_integer");
                    VERIFY_IS_INTEGER(size, "bs_put_integer");

                    avm_int64_t src_value = term_maybe_unbox_int64(src);
                    avm_int_t size_value = term_to_int(size);

                    TRACE("bs_put_integer/5, fail=%u size=%li unit=%u flags=%x src=%i\n", (unsigned) fail, size_value, (unsigned) unit, (int) flags_value, (unsigned int) src_value);

                    bool result = bitstring_insert_integer(ctx->bs, ctx->bs_offset, src_value, size_value * unit, flags_value);
                    if (UNLIKELY(!result)) {
                        TRACE("bs_put_integer: Failed to insert integer into binary\n");
                        RAISE_ERROR(BADARG_ATOM);
                    }

                    ctx->bs_offset += size_value * unit;
                #endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_PUT_BINARY: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off)
                uint32_t unit;
                DECODE_LITERAL(unit, code, i, next_off);
                uint32_t flags_value;
                DECODE_LITERAL(flags_value, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_put_binary/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_BINARY(src, "bs_put_binary");
                    unsigned long size_val = 0;
                    if (term_is_integer(size)) {
                        avm_int_t bit_size = term_to_int(size) * unit;
                        if (bit_size % 8 != 0) {
                            TRACE("bs_put_binary: Bit size must be evenly divisible by 8.\n");
                            RAISE_ERROR(UNSUPPORTED_ATOM);
                        }
                        size_val = bit_size / 8;
                    } else if (size == ALL_ATOM) {
                        size_val = term_binary_size(src);
                    } else {
                        TRACE("bs_put_binary: Unsupported size term type in put binary: %p\n", (void *) size);
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    if (size_val > term_binary_size(src)) {
                        TRACE("bs_put_binary: binary data size (%li) larger than source binary size (%li)\n", size_val, term_binary_size(src));
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    if (flags_value != 0) {
                        TRACE("bs_put_binary: neither signed nor native or little endian encoding supported.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    if (ctx->bs_offset % 8 != 0) {
                        TRACE("bs_put_binary: Unsupported bit syntax operation.  Writing binaries must be byte-aligend.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    TRACE("bs_put_binary/5, fail=%u size=%li unit=%u flags=%x src=0x%x\n", (unsigned) fail, size_val, (unsigned) unit, (int) flags_value, (unsigned int) src);

                    int result = term_bs_insert_binary(ctx->bs, ctx->bs_offset, src, size_val);
                    if (UNLIKELY(result)) {
                        TRACE("bs_put_binary: Failed to insert binary into binary: %i\n", result);
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    ctx->bs_offset += 8 * size_val;
                #endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_PUT_STRING: {
                int next_off = 1;
                uint32_t size;
                DECODE_LITERAL(size, code, i, next_off);
                uint32_t offset;
                DECODE_LITERAL(offset, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_put_string/2\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    if (UNLIKELY(!term_is_binary(ctx->bs))) {
                        TRACE("bs_put_string: Bad state.  ctx->bs is not a binary.\n");
                        RAISE_ERROR(BADARG_ATOM);
                    }

                    TRACE("bs_put_string/2, size=%u offset=%u\n", size, offset);

                    size_t remaining = 0;
                    const uint8_t *str = module_get_str(mod, offset, &remaining);
                    if (UNLIKELY(IS_NULL_PTR(str))) {
                        TRACE("bs_put_string: Bad offset in strings table.\n");
                        RAISE_ERROR(BADARG_ATOM);
                    }

                    size_t size_in_bits = size * 8;
                    uint8_t *dst = (uint8_t *) term_binary_data(ctx->bs);
                    bitstring_copy_bits(dst, ctx->bs_offset, str, size_in_bits);
                    ctx->bs_offset += size_in_bits;
                #endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_START_MATCH2: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                #ifdef IMPL_EXECUTE_LOOP
                    int next_off_back = next_off;
                #endif
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);
                term slots_term;
                DECODE_COMPACT_TERM(slots_term, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    int slots = term_to_int(slots_term);

                    // MEMORY_CAN_SHRINK because bs_start_match is classified as gc in beam_ssa_codegen.erl
                    if (memory_ensure_free_opt(ctx, TERM_BOXED_BIN_MATCH_STATE_SIZE + slots, MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }

                    DECODE_COMPACT_TERM(src, code, i, next_off_back);
                #endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_start_match2/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_start_match2/5, fail=%i src=0x%lx arg2=0x%lx arg3=0x%lx dreg=%c%i\n", fail, src, arg2, slots_term, T_DEST_REG(dreg));
                    if (!(term_is_binary(src) || term_is_match_state(src))) {
                        WRITE_REGISTER(dreg, src);
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term match_state = term_alloc_bin_match_state(src, slots, &ctx->heap);

                        WRITE_REGISTER(dreg, match_state);
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif
                break;
            }

            case OP_BS_START_MATCH3: {
                // MEMORY_CAN_SHRINK because bs_start_match is classified as gc in beam_ssa_codegen.erl
                #ifdef IMPL_EXECUTE_LOOP
                    if (memory_ensure_free_opt(ctx, TERM_BOXED_BIN_MATCH_STATE_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term live;
                DECODE_COMPACT_TERM(live, code, i, next_off);
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_start_match3/4\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_start_match3/4, fail=%i src=0x%lx live=0x%lx dreg=%c%i\n", fail, src, live, T_DEST_REG(dreg));
                    if (!(term_is_binary(src) || term_is_match_state(src))) {
                        WRITE_REGISTER(dreg, src);
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term match_state = term_alloc_bin_match_state(src, 0, &ctx->heap);

                        WRITE_REGISTER(dreg, match_state);
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif
                break;
            }

            case OP_BS_GET_POSITION: {
                int next_off = 1;
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);
                term live;
                DECODE_COMPACT_TERM(live, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_get_position/3\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_get_position");

                    TRACE("bs_get_position/3 src=0x%lx dreg=%c%i live=0x%lx \n", src, T_DEST_REG(dreg), live);

                    avm_int_t offset = term_get_match_state_offset(src);
                    term offset_term = term_from_int(offset);

                    WRITE_REGISTER(dreg, offset_term);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_GET_TAIL: {
                int next_off = 1;
                term src;
                #ifdef IMPL_EXECUTE_LOOP
                    int src_off = next_off;
                #endif
                DECODE_COMPACT_TERM(src, code, i, next_off);
                dreg_gc_safe_t dreg;
                DECODE_DEST_REGISTER_GC_SAFE(dreg, code, i, next_off);
                term live;
                DECODE_COMPACT_TERM(live, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_get_tail/3\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_get_tail");

                    avm_int_t bs_offset = term_get_match_state_offset(src);
                    term bs_bin = term_get_match_state_binary(src);

                    TRACE("bs_get_tail/3 src=0x%lx dreg=%c%i live=0x%lx \n", src, T_DEST_REG_GC_SAFE(dreg), live);
                    if (bs_offset == 0) {

                        WRITE_REGISTER_GC_SAFE(dreg, bs_bin);

                    } else {
                        if (bs_offset % 8 != 0) {
                            TRACE("bs_get_tail: Unsupported alignment.\n");
                            RAISE_ERROR(UNSUPPORTED_ATOM);
                        } else {
                            size_t start_pos = bs_offset / 8;
                            size_t src_size = term_binary_size(bs_bin);
                            size_t new_bin_size = src_size - start_pos;
                            size_t heap_size = term_sub_binary_heap_size(bs_bin, src_size - start_pos);


                            if (UNLIKELY(memory_ensure_free_opt(ctx, heap_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                            }
                            DECODE_COMPACT_TERM(src, code, i, src_off);
                            bs_bin = term_get_match_state_binary(src);
                            term t = term_maybe_create_sub_binary(bs_bin, start_pos, new_bin_size, &ctx->heap, ctx->global);
                            WRITE_REGISTER_GC_SAFE(dreg, t);

                        }
                    }
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_SET_POSITION: {
                int next_off = 1;
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term pos;
                DECODE_COMPACT_TERM(pos, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_set_position/2\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_set_position");
                    VERIFY_IS_INTEGER(pos, "bs_set_position");

                    avm_int_t pos_val = term_to_int(pos);
                    TRACE("bs_set_position/2 src=0x%lx pos=%li\n", src, pos_val);
                    term_set_match_state_offset(src,  pos_val);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_MATCH_STRING: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                uint32_t bits;
                DECODE_LITERAL(bits, code, i, next_off);
                uint32_t offset;
                DECODE_LITERAL(offset, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_match_string/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_match_string");

                    if (bits % 8 != 0) {
                        TRACE("bs_match_string: Unsupported bits size (must be evenly divisible by 8). bits=%u\n", (unsigned) bits);
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    avm_int_t bytes = bits / 8;
                    avm_int_t bs_offset = term_get_match_state_offset(src);
                    term bs_bin = term_get_match_state_binary(src);

                    if (bs_offset % 8 != 0) {
                        TRACE("bs_match_string: Unsupported offset (must be evenly divisible by 8). bs_offset=%li\n", bs_offset);
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    avm_int_t byte_offset = bs_offset / 8;

                    TRACE("bs_match_string/4, fail=%u src=%p bits=%u offset=%u\n", (unsigned) fail, (void *) src, (unsigned) bits, (unsigned) offset);

                    size_t remaining = 0;
                    const uint8_t *str = module_get_str(mod, offset, &remaining);
                    if (UNLIKELY(IS_NULL_PTR(str))) {
                        TRACE("bs_match_string: Bad offset in strings table.\n");
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    if (memcmp(term_binary_data(bs_bin) + byte_offset, str, MIN(remaining, (unsigned int) bytes)) != 0) {
                        TRACE("bs_match_string: failed to match\n");
                        JUMP_TO_ADDRESS(mod->labels[fail]);
                    } else {
                        term_set_match_state_offset(src, bs_offset + bits);
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif
                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_BS_SAVE2: {
                int next_off = 1;
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term index = 0;
                DECODE_COMPACT_TERM(index, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_save2/2\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_save2");

                    avm_int_t index_val;
                    if (index == START_ATOM) {
                        // TODO: not sure if 'start' is used anytime in generated code
                        term_match_state_save_start_offset(src);
                    } else if (term_is_integer(index)) {
                        index_val = term_to_int(index);
                        term_match_state_save_offset(src, index_val);
                    } else {
                        AVM_ABORT();
                    }

                    TRACE("bs_save2/2, src=0x%lx pos=%li\n", src, index == START_ATOM ? -1 : index_val);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_RESTORE2: {
                int next_off = 1;
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term index = 0;
                DECODE_COMPACT_TERM(index, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_restore2/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_restore2");

                    avm_int_t index_val;
                    if (index == START_ATOM) {
                        term_match_state_restore_start_offset(src);
                    } else if (term_is_integer(index)) {
                        index_val = term_to_int(index);
                        term_match_state_restore_offset(src, index_val);
                    } else {
                        AVM_ABORT();
                    }

                    TRACE("bs_restore2/2, src=0x%lx pos=%li\n", src, index == START_ATOM ? -1 : index_val);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_SKIP_BITS2: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off);
                uint32_t unit;
                DECODE_LITERAL(unit, code, i, next_off);
                uint32_t flags_value;
                DECODE_LITERAL(flags_value, code, i, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_skip_bits2/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_skip_bits2");
                    VERIFY_IS_INTEGER(size, "bs_skip_bits2");
                    if (flags_value != 0) {
                        TRACE("bs_skip_bits2: neither signed nor native or little endian encoding supported.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    avm_int_t size_val = term_to_int(size);

                    TRACE("bs_skip_bits2/5, fail=%u src=%p size=0x%lx unit=%u flags=%x\n", (unsigned) fail, (void *) src, (unsigned long) size_val, (unsigned) unit, (int) flags_value);

                    size_t increment = size_val * unit;
                    avm_int_t bs_offset = term_get_match_state_offset(src);
                    term bs_bin = term_get_match_state_binary(src);
                    if ((bs_offset + increment) > term_binary_size(bs_bin) * 8) {
                        TRACE("bs_skip_bits2: Insufficient capacity to skip bits: %lu, inc: %zu\n", (unsigned long) bs_offset, increment);
                        JUMP_TO_ADDRESS(mod->labels[fail]);
                    } else {
                        term_set_match_state_offset(src, bs_offset + increment);
                        NEXT_INSTRUCTION(next_off);
                    }

                #endif
                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif
                break;
            }

            case OP_BS_TEST_UNIT: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                uint32_t unit;
                DECODE_LITERAL(unit, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_test_unit/3\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_test_unit");

                    TRACE("bs_test_unit/3, fail=%u src=%p unit=%u\n", (unsigned) fail, (void *) src, (unsigned) unit);

                    avm_int_t bs_offset = term_get_match_state_offset(src);
                    if ((term_binary_size(src) * 8 - bs_offset) % unit != 0) {
                        TRACE("bs_test_unit: Available bits in source not evenly divisible by unit");
                        JUMP_TO_ADDRESS(mod->labels[fail]);
                    } else {
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif
                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif
                break;
            }

            case OP_BS_TEST_TAIL2: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                uint32_t bits;
                DECODE_LITERAL(bits, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_test_tail2/3\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_test_tail2");

                    TRACE("bs_test_tail2/3, fail=%u src=%p bits=%u\n", (unsigned) fail, (void *) src, (unsigned) bits);

                    term bs_bin = term_get_match_state_binary(src);
                    avm_int_t bs_offset = term_get_match_state_offset(src);

                    if ((term_binary_size(bs_bin) * 8 - bs_offset) != (unsigned int) bits) {
                        TRACE("bs_test_tail2: Expected exactly %u bits remaining, but remaining=%u\n", (unsigned) bits, (unsigned) (term_binary_size(bs_bin) * 8 - bs_offset));
                        JUMP_TO_ADDRESS(mod->labels[fail]);
                    } else {
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif
                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif
                break;
            }

            case OP_BS_GET_INTEGER2: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off);
                uint32_t unit;
                DECODE_LITERAL(unit, code, i, next_off);
                uint32_t flags_value;
                DECODE_LITERAL(flags_value, code, i, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_get_integer2/7\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_get_integer");
                    VERIFY_IS_INTEGER(size,     "bs_get_integer");

                    avm_int_t size_val = term_to_int(size);

                    TRACE("bs_get_integer2/7, fail=%u src=%p size=%u unit=%u flags=%x\n", (unsigned) fail, (void *) src, (unsigned) size_val, (unsigned) unit, (int) flags_value);

                    avm_int_t increment = size_val * unit;
                    union maybe_unsigned_int64 value;
                    term bs_bin = term_get_match_state_binary(src);
                    avm_int_t bs_offset = term_get_match_state_offset(src);
                    bool status = bitstring_extract_integer(bs_bin, bs_offset, increment, flags_value, &value);
                    if (UNLIKELY(!status)) {
                        TRACE("bs_get_integer2: error extracting integer.\n");
                        JUMP_TO_ADDRESS(mod->labels[fail]);
                    } else {
                        term_set_match_state_offset(src, bs_offset + increment);

                        term t = maybe_alloc_boxed_integer_fragment(ctx, value.s);
                        if (UNLIKELY(term_is_invalid_term(t))) {
                            HANDLE_ERROR();
                        }
                #endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                        WRITE_REGISTER(dreg, t);
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif
                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif
                break;
            }

            case OP_BS_GET_BINARY2: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off)
                term src;
                #ifdef IMPL_EXECUTE_LOOP
                    int src_offset = next_off;
                #endif
                DECODE_COMPACT_TERM(src, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off);
                uint32_t unit;
                DECODE_LITERAL(unit, code, i, next_off);
                uint32_t flags_value;
                DECODE_LITERAL(flags_value, code, i, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_get_binary2/7\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_get_binary2");

                    term bs_bin = term_get_match_state_binary(src);
                    avm_int_t bs_offset = term_get_match_state_offset(src);

                    if (unit != 8) {
                        TRACE("bs_get_binary2: Unsupported: unit must be 8.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    avm_int_t size_val = 0;
                    if (term_is_integer(size)) {
                        size_val = term_to_int(size);
                    } else if (size == ALL_ATOM) {
                        size_val = term_binary_size(bs_bin) - bs_offset / 8;
                    } else {
                        TRACE("bs_get_binary2: size is neither an integer nor the atom `all`\n");
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    if (bs_offset % unit != 0) {
                        TRACE("bs_get_binary2: Unsupported.  Offset on binary read must be aligned on byte boundaries.\n");
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    if (flags_value != 0) {
                        TRACE("bs_get_binary2: neither signed nor native or little endian encoding supported.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    TRACE("bs_get_binary2/7, fail=%u src=%p unit=%u\n", (unsigned) fail, (void *) bs_bin, (unsigned) unit);

                    if ((unsigned int) (bs_offset / unit + size_val) > term_binary_size(bs_bin)) {
                        TRACE("bs_get_binary2: insufficient capacity -- bs_offset = %d, size_val = %d\n", (int) bs_offset, (int) size_val);
                        JUMP_TO_ADDRESS(mod->labels[fail]);
                    } else {
                        term_set_match_state_offset(src, bs_offset + size_val * unit);

                        size_t heap_size = term_sub_binary_heap_size(bs_bin, size_val);
                        if (UNLIKELY(memory_ensure_free_opt(ctx, heap_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                #endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                        // re-compute src
                        DECODE_COMPACT_TERM(src, code, i, src_offset);
                        bs_bin = term_get_match_state_binary(src);

                        term t = term_maybe_create_sub_binary(bs_bin, bs_offset / unit, size_val, &ctx->heap, ctx->global);
                        WRITE_REGISTER(dreg, t);
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif
                break;
            }

            case OP_BS_CONTEXT_TO_BINARY: {
                // Do not check if dreg is a binary or not
                // In case it is not a binary or a match state, dreg will not be changed.
                int next_off = 1;
                dreg_gc_safe_t dreg;
                DECODE_DEST_REGISTER_GC_SAFE(dreg, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_context_to_binary/1\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_context_to_binary/1, dreg=%c%i\n", T_DEST_REG_GC_SAFE(dreg));
                    term src = READ_DEST_REGISTER_GC_SAFE(dreg);
                    term bin;
                    if (term_is_match_state(src)) {
                        avm_int_t offset = term_get_match_state_offset(src);
                        if (offset == 0) {
                            bin = term_get_match_state_binary(src);
                        } else {
                            term src_bin = term_get_match_state_binary(src);
                            int len = term_binary_size(src_bin) - offset / 8;
                            size_t heap_size = term_sub_binary_heap_size(src_bin, len);
                            if (UNLIKELY(memory_ensure_free_opt(ctx, heap_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                            }
                            // src might be invalid after a GC
                            src = READ_DEST_REGISTER_GC_SAFE(dreg);
                            src_bin = term_get_match_state_binary(src);
                            bin = term_maybe_create_sub_binary(src_bin, offset / 8, len, &ctx->heap, ctx->global);
                        }
                    } else {
                        bin = src;
                    }
                    WRITE_REGISTER_GC_SAFE(dreg, bin);
                #endif


                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_APPLY: {
                int next_off = 1;
                uint32_t arity;
                DECODE_LITERAL(arity, code, i, next_off)
#ifdef IMPL_EXECUTE_LOOP
                term module = x_regs[arity];
                term function = x_regs[arity + 1];
                TRACE("apply/1, module=%lu, function=%lu arity=%i\n", module, function, arity);

                remaining_reductions--;
                if (UNLIKELY(!remaining_reductions)) {
                    SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                }

                // save instruction offset in case of error
                int orig_i = i;
                NEXT_INSTRUCTION(next_off);

                if (UNLIKELY(!term_is_atom(module) || !term_is_atom(function))) {
                    RAISE_ERROR(BADARG_ATOM);
                }

                AtomString module_name = globalcontext_atomstring_from_term(glb, module);
                AtomString function_name = globalcontext_atomstring_from_term(glb, function);

                TRACE_APPLY(ctx, "apply", module_name, function_name, arity);

                term native_return;
                if (maybe_call_native(ctx, module_name, function_name, arity, &native_return)) {
                    PROCESS_MAYBE_TRAP_RETURN_VALUE_RESTORE_I(native_return, orig_i);
                    x_regs[0] = native_return;

                } else {
                    Module *target_module = globalcontext_get_module(ctx->global, module_name);
                    if (IS_NULL_PTR(target_module)) {
                        i = orig_i;
                        HANDLE_ERROR();
                    }
                    int target_label = module_search_exported_function(target_module, function_name, arity, glb);
                    if (target_label == 0) {
                        i = orig_i;
                        HANDLE_ERROR();
                    }
                    ctx->cp = module_address(mod->module_index, i);
                    JUMP_TO_LABEL(target_module, target_label);
                }
#endif
#ifdef IMPL_CODE_LOADER
                TRACE("apply/1 arity=%i\n", arity);
                NEXT_INSTRUCTION(next_off);
#endif
                break;
            }

            case OP_APPLY_LAST: {
                int next_off = 1;
                uint32_t arity;
                DECODE_LITERAL(arity, code, i, next_off)
                uint32_t n_words;
                DECODE_LITERAL(n_words, code, i, next_off);
#ifdef IMPL_EXECUTE_LOOP
                term module = x_regs[arity];
                term function = x_regs[arity + 1];
                TRACE("apply_last/1, module=%lu, function=%lu arity=%i deallocate=%i\n", module, function, arity, n_words);

                remaining_reductions--;
                if (UNLIKELY(!remaining_reductions)) {
                    SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                }

                ctx->cp = ctx->e[n_words];
                ctx->e += (n_words + 1);

                if (UNLIKELY(!term_is_atom(module) || !term_is_atom(function))) {
                    RAISE_ERROR(BADARG_ATOM);
                }

                AtomString module_name = globalcontext_atomstring_from_term(glb, module);
                AtomString function_name = globalcontext_atomstring_from_term(glb, function);

                TRACE_APPLY(ctx, "apply_last", module_name, function_name, arity);

                term native_return;
                if (maybe_call_native(ctx, module_name, function_name, arity, &native_return)) {
                    PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(native_return);
                    x_regs[0] = native_return;
                    DO_RETURN();

                } else {
                    Module *target_module = globalcontext_get_module(ctx->global, module_name);
                    if (IS_NULL_PTR(target_module)) {
                        HANDLE_ERROR();
                    }
                    int target_label = module_search_exported_function(target_module, function_name, arity, glb);
                    if (target_label == 0) {
                        HANDLE_ERROR();
                    }
                    JUMP_TO_LABEL(target_module, target_label);
                }
#endif
#ifdef IMPL_CODE_LOADER
                TRACE("apply_last/1 arity=%i deallocate=%i\n", arity, n_words);
                NEXT_INSTRUCTION(next_off);
#endif
                break;
            }

            case OP_IS_BOOLEAN: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_boolean/2, label=%i, arg1=%lx\n", label, arg1);

                    if ((arg1 == TRUE_ATOM) || (arg1 == FALSE_ATOM)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_boolean/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_FUNCTION2: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)
                unsigned int arity;
                DECODE_INTEGER(arity, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_function2/3, label=%i, arg1=%lx, arity=%i\n", label, arg1, arity);

                    if (term_is_function(arg1)) {
                        const term *boxed_value = term_to_const_term_ptr(arg1);

                        Module *fun_module = (Module *) boxed_value[1];
                        term index_or_module = boxed_value[2];

                        uint32_t fun_arity;

                        if (term_is_atom(index_or_module)) {
                            fun_arity = term_to_int(boxed_value[3]);

                        } else {
                            uint32_t fun_index = term_to_int32(index_or_module);

                            uint32_t fun_label;
                            uint32_t fun_arity_and_freeze;
                            uint32_t fun_n_freeze;

                            module_get_fun(fun_module, fun_index, &fun_label, &fun_arity_and_freeze, &fun_n_freeze);
                            fun_arity = fun_arity_and_freeze - fun_n_freeze;
                        }

                        if (arity == fun_arity) {
                            NEXT_INSTRUCTION(next_off);
                        } else {
                            i = POINTER_TO_II(mod->labels[label]);
                        }
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_function/3\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_GC_BIF1: {
                int next_off = 1;
                uint32_t f_label;
                DECODE_LABEL(f_label, code, i, next_off);
                uint32_t live;
                DECODE_LITERAL(live, code, i, next_off);
                uint32_t bif;
                DECODE_LITERAL(bif, code, i, next_off); //s?
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    const struct ExportedFunction *exported_bif = mod->imported_funcs[bif];
                    GCBifImpl1 func = EXPORTED_FUNCTION_TO_GCBIF(exported_bif)->gcbif1_ptr;
                    term ret = func(ctx, live, arg1);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        HANDLE_ERROR();
                    }
                #endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("gc_bif1/5 fail_lbl=%i, live=%i, bif=%i, arg1=0x%lx, dest=%c%i\n", f_label, live, bif, arg1, T_DEST_REG(dreg));
                    WRITE_REGISTER(dreg, ret);
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("gc_bif1/5\n");

                    UNUSED(f_label)
                    UNUSED(live)
                    UNUSED(bif)
                    UNUSED(arg1)
                    UNUSED(dreg)
                #endif

                UNUSED(f_label)

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_GC_BIF2: {
                int next_off = 1;
                uint32_t f_label;
                DECODE_LABEL(f_label, code, i, next_off);
                uint32_t live;
                DECODE_LITERAL(live, code, i, next_off);
                uint32_t bif;
                DECODE_LITERAL(bif, code, i, next_off); //s?
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    const struct ExportedFunction *exported_bif = mod->imported_funcs[bif];
                    GCBifImpl2 func = EXPORTED_FUNCTION_TO_GCBIF(exported_bif)->gcbif2_ptr;
                    term ret = func(ctx, live, arg1, arg2);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        HANDLE_ERROR();
                    }
                #endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("gc_bif2/6 fail_lbl=%i, live=%i, bif=%i, arg1=0x%lx, arg2=0x%lx, dest=%c%i\n", f_label, live, bif, arg1, arg2, T_DEST_REG(dreg));
                    WRITE_REGISTER(dreg, ret);
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("gc_bif2/6\n");

                    UNUSED(f_label)
                    UNUSED(live)
                    UNUSED(bif)
                    UNUSED(arg1)
                    UNUSED(arg2)
                    UNUSED(dreg)
                #endif

                UNUSED(f_label)

                NEXT_INSTRUCTION(next_off);
                break;
            }

            //TODO: stub, always false
            case OP_IS_BITSTR: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_bitstr/2, label=%i, arg1=%lx\n", label, arg1);

                    if (0) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_bitstr/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_GC_BIF3: {
                int next_off = 1;
                uint32_t f_label;
                DECODE_LABEL(f_label, code, i, next_off);
                uint32_t live;
                DECODE_LITERAL(live, code, i, next_off);
                uint32_t bif;
                DECODE_LITERAL(bif, code, i, next_off); //s?
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off);
                term arg3;
                DECODE_COMPACT_TERM(arg3, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    const struct ExportedFunction *exported_bif = mod->imported_funcs[bif];
                    GCBifImpl3 func = EXPORTED_FUNCTION_TO_GCBIF(exported_bif)->gcbif3_ptr;
                    term ret = func(ctx, live, arg1, arg2, arg3);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        HANDLE_ERROR();
                    }
                #endif

                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("gc_bif3/7 fail_lbl=%i, live=%i, bif=%i, arg1=0x%lx, arg2=0x%lx, arg3=0x%lx, dest=%c%i\n", f_label, live, bif, arg1, arg2, arg3, T_DEST_REG(dreg));
                    WRITE_REGISTER(dreg, ret);
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("gc_bif2/6\n");

                    UNUSED(f_label)
                    UNUSED(live)
                    UNUSED(bif)
                    UNUSED(arg1)
                    UNUSED(arg2)
                    UNUSED(arg3)
                    UNUSED(dreg)
                #endif

                UNUSED(f_label)

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TRIM: {
                int next_off = 1;
                uint32_t n_words;
                DECODE_LITERAL(n_words, code, i, next_off);
                uint32_t n_remaining;
                DECODE_LITERAL(n_remaining, code, i, next_off);

                TRACE("trim/2 words=%i, remaining=%i\n", n_words, n_remaining);
                USED_BY_TRACE(n_words);
                USED_BY_TRACE(n_remaining);

                #ifdef IMPL_EXECUTE_LOOP
                    DEBUG_DUMP_STACK(ctx);
                    ctx->e += n_words;
                    DEBUG_DUMP_STACK(ctx);
                #endif

                UNUSED(n_remaining)

                NEXT_INSTRUCTION(next_off);
                break;
            }

            //TODO: stub, implement recv_mark/1
            //it looks like it can be safely left unimplemented
            case OP_RECV_MARK: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off);

                TRACE("recv_mark/1 label=%i\n", label);
                USED_BY_TRACE(label);

                NEXT_INSTRUCTION(next_off);
                break;
            }

            //TODO: stub, implement recv_set/1
            //it looks like it can be safely left unimplemented
            case OP_RECV_SET: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off);

                TRACE("recv_set/1 label=%i\n", label);
                USED_BY_TRACE(label);

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_LINE: {
                int next_off = 1;
                uint32_t line_number;
                DECODE_LITERAL(line_number, code, i, next_off);

                TRACE("line/1: %i\n", line_number);

                #ifdef IMPL_CODE_LOADER
                    module_insert_line_ref_offset(mod, line_number, i);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_PUT_MAP_ASSOC: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term src;
                #ifdef IMPL_EXECUTE_LOOP
                    int src_offset = next_off;
                #endif
                DECODE_COMPACT_TERM(src, code, i, next_off);
                dreg_gc_safe_t dreg;
                DECODE_DEST_REGISTER_GC_SAFE(dreg, code, i, next_off);
                uint32_t live;
                DECODE_LITERAL(live, code, i, next_off);

                TRACE("put_map_assoc/5: label: %i src: 0x%lx dest=%c%i live: %i\n", label, src, T_DEST_REG_GC_SAFE(dreg), live);

                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                uint32_t list_len;
                DECODE_LITERAL(list_len, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    int list_off = next_off;
                #endif
                uint32_t num_elements = list_len / 2;
                //
                // Count how many of the entries in list(...) are not already in src
                //
                #ifdef IMPL_EXECUTE_LOOP
                    unsigned new_entries = 0;
                #endif
                for (uint32_t j = 0;  j < num_elements;  ++j) {
                    term key, value;
                    DECODE_COMPACT_TERM(key, code, i, next_off);
                    DECODE_COMPACT_TERM(value, code, i, next_off);

                    #ifdef IMPL_EXECUTE_LOOP
                        int map_pos = term_find_map_pos(src, key, ctx->global);
                        if (map_pos == TERM_MAP_NOT_FOUND) {
                            new_entries++;
                        } else if (UNLIKELY(map_pos == TERM_MAP_MEMORY_ALLOC_FAIL)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    #endif
                }

                #ifdef IMPL_EXECUTE_LOOP
                    //
                    // Maybe GC, and reset the src term in case it changed
                    //
                    size_t src_size = term_get_map_size(src);
                    size_t new_map_size = src_size + new_entries;
                    bool is_shared = new_entries == 0;
                    size_t heap_needed = term_map_size_in_terms_maybe_shared(new_map_size, is_shared);
                    // MEMORY_CAN_SHRINK because put_map is classified as gc in beam_ssa_codegen.erl
                    if (memory_ensure_free_opt(ctx, heap_needed, MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    DECODE_COMPACT_TERM(src, code, i, src_offset);
                    //
                    //
                    //
                    struct kv_pair *kv = malloc(num_elements * sizeof(struct kv_pair));
                    if (IS_NULL_PTR(kv)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    for (uint32_t j = 0; j < num_elements; j++) {
                        term key, value;
                        DECODE_COMPACT_TERM(key, code, i, list_off);
                        DECODE_COMPACT_TERM(value, code, i, list_off);
                        kv[j].key = key;
                        kv[j].value = value;
                    }
                    if (UNLIKELY(!sort_kv_pairs(kv, num_elements, ctx->global))) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    //
                    // Create a new map of the requested size and stitch src
                    // and kv together into new map.  Both src and kv are sorted.
                    //
                    term map = term_alloc_map_maybe_shared(new_map_size, is_shared ? term_get_map_keys(src) : term_invalid_term(), &ctx->heap);
                    size_t src_pos = 0;
                    uint32_t kv_pos = 0;
                    for (size_t j = 0; j < new_map_size; j++) {
                        if (src_pos >= src_size) {
                            term new_key = kv[kv_pos].key;
                            term new_value = kv[kv_pos].value;
                            term_set_map_assoc(map, j, new_key, new_value);
                            kv_pos++;
                        } else if (kv_pos >= num_elements) {
                            term src_key = term_get_map_key(src, src_pos);
                            term src_value = term_get_map_value(src, src_pos);
                            term_set_map_assoc(map, j, src_key, src_value);
                            src_pos++;
                        } else {
                            term src_key = term_get_map_key(src, src_pos);
                            term new_key = kv[kv_pos].key;
                            // TODO: not sure if exact is the right choice here
                            switch (term_compare(src_key, new_key, TermCompareExact, ctx->global)) {
                                case TermLessThan: {
                                    term src_value = term_get_map_value(src, src_pos);
                                    term_set_map_assoc(map, j, src_key, src_value);
                                    src_pos++;
                                    break;
                                }

                                case TermGreaterThan: {
                                    term new_value = kv[kv_pos].value;
                                    term_set_map_assoc(map, j, new_key, new_value);
                                    kv_pos++;
                                    break;
                                }

                                case TermEquals: {
                                    term new_value = kv[kv_pos].value;
                                    term_set_map_assoc(map, j, src_key, new_value);
                                    src_pos++;
                                    kv_pos++;
                                    break;
                                }

                                case TermCompareMemoryAllocFail: {
                                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                                }
                            }
                        }
                    }
                    free(kv);

                    WRITE_REGISTER_GC_SAFE(dreg, map);
                #endif

                NEXT_INSTRUCTION(next_off);

                break;
            }

            case OP_PUT_MAP_EXACT: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term src;
                #ifdef IMPL_EXECUTE_LOOP
                    int src_offset = next_off;
                #endif
                DECODE_COMPACT_TERM(src, code, i, next_off);
                dreg_gc_safe_t dreg;
                DECODE_DEST_REGISTER_GC_SAFE(dreg, code, i, next_off);
                uint32_t live;
                DECODE_LITERAL(live, code, i, next_off);

                TRACE("put_map_exact/5: label: %i src: 0x%lx dest=%c%i live: %i\n", label, src, T_DEST_REG_GC_SAFE(dreg), live);
                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                uint32_t list_len;
                DECODE_LITERAL(list_len, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    int list_off = next_off;
                #endif
                uint32_t num_elements = list_len / 2;
                //
                // Make sure every key from list is in src
                //
                for (uint32_t j = 0; j < num_elements; ++j) {
                    term key, value;
                    DECODE_COMPACT_TERM(key, code, i, next_off);
                    DECODE_COMPACT_TERM(value, code, i, next_off);

                    #ifdef IMPL_EXECUTE_LOOP
                        int map_pos = term_find_map_pos(src, key, ctx->global);
                        if (map_pos == TERM_MAP_NOT_FOUND) {
                            RAISE_ERROR(BADARG_ATOM);
                        } else if (map_pos == TERM_MAP_MEMORY_ALLOC_FAIL) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    #endif
                }

                #ifdef IMPL_EXECUTE_LOOP
                    //
                    // Maybe GC, and reset the src term in case it changed
                    //
                    size_t src_size = term_get_map_size(src);
                    // MEMORY_CAN_SHRINK because put_map is classified as gc in beam_ssa_codegen.erl
                    if (memory_ensure_free_opt(ctx, term_map_size_in_terms_maybe_shared(src_size, true), MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    DECODE_COMPACT_TERM(src, code, i, src_offset);
                    //
                    // Create a new map of the same size as src and populate with entries from src
                    //
                    term map = term_alloc_map_maybe_shared(src_size, term_get_map_keys(src), &ctx->heap);
                    for (size_t j = 0;  j < src_size;  ++j) {
                        term_set_map_assoc(map, j, term_get_map_key(src, j), term_get_map_value(src, j));
                    }
                    //
                    // Copy the new terms into the new map, in situ only
                    //
                    for (uint32_t j = 0; j < num_elements; ++j) {
                        term key, value;
                        DECODE_COMPACT_TERM(key, code, i, list_off);
                        DECODE_COMPACT_TERM(value, code, i, list_off);
                        int pos = term_find_map_pos(src, key, ctx->global);
                        if (UNLIKELY(pos == TERM_MAP_MEMORY_ALLOC_FAIL)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                        term_set_map_assoc(map, pos, key, value);
                    }
                    WRITE_REGISTER_GC_SAFE(dreg, map);
                #endif

                NEXT_INSTRUCTION(next_off);

                break;
            }

            case OP_IS_MAP: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_map/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_map(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_map/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_HAS_MAP_FIELDS: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("has_map_fields/3: label: %i src: 0x%lx\n", label, src);
                #else
                    TRACE("has_map_fields/3: label: %i\n", label);
                #endif

                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                uint32_t list_len;
                DECODE_LITERAL(list_len, code, i, next_off);
                int fail = 0;
                for (uint32_t j = 0;  j < list_len && !fail;  ++j) {
                    term key;
                    DECODE_COMPACT_TERM(key, code, i, next_off);

                    #ifdef IMPL_EXECUTE_LOOP
                        int pos = term_find_map_pos(src, key, ctx->global);
                        if (pos == TERM_MAP_NOT_FOUND) {
                            i = POINTER_TO_II(mod->labels[label]);
                            fail = 1;
                        } else if (pos == TERM_MAP_MEMORY_ALLOC_FAIL) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    #endif
                }
                if (!fail) {
                    NEXT_INSTRUCTION(next_off);
                }
                break;
            }

            case OP_GET_MAP_ELEMENTS: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("get_map_elements/3: label: %i src: 0x%lx\n", label, src);
                #else
                    TRACE("get_map_elements/3: label: %i\n", label);
                #endif

                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                uint32_t list_len;
                DECODE_LITERAL(list_len, code, i, next_off);
                uint32_t num_elements = list_len / 2;
                uint32_t fail = 0;
                for (uint32_t j = 0;  j < num_elements && !fail;  ++j) {
                    term key;
                    DECODE_COMPACT_TERM(key, code, i, next_off);
                    dreg_t dreg;
                    DECODE_DEST_REGISTER(dreg, code, i, next_off);

                    #ifdef IMPL_EXECUTE_LOOP
                        int pos = term_find_map_pos(src, key, ctx->global);
                        if (pos == TERM_MAP_NOT_FOUND) {
                            i = POINTER_TO_II(mod->labels[label]);
                            fail = 1;
                        } else if (UNLIKELY(pos == TERM_MAP_MEMORY_ALLOC_FAIL)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        } else {
                            term value = term_get_map_value(src, pos);
                            WRITE_REGISTER(dreg, value);
                        }
                    #endif
                }
                if (!fail) {
                    NEXT_INSTRUCTION(next_off);
                }
                break;
            }

            case OP_IS_TAGGED_TUPLE: {
                int next_off = 1;
                uint32_t label;
                DECODE_LABEL(label, code, i, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off)
                uint32_t arity;
                DECODE_LITERAL(arity, code, i, next_off)
                term tag_atom;
                DECODE_ATOM(tag_atom, code, i, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_tagged_tuple/2, label=%u, arg1=%p, arity=%u, atom_id=%p\n", (unsigned) label, (void *) arg1, (unsigned) arity, (void *) tag_atom);

                    if (term_is_tuple(arg1) && ((uint32_t) term_get_tuple_arity(arg1) == arity) && (term_get_tuple_element(arg1, 0) == tag_atom)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_tagged_tuple/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_FCLEARERROR: {
                // This can be a noop as we raise from bifs
                TRACE("fclearerror/0\n");
                NEXT_INSTRUCTION(1);
                break;
            }

            case OP_FCHECKERROR: {
                int next_off = 1;
                // This can be a noop as we raise from bifs
                int fail_label;
                DECODE_LABEL(fail_label, code, i, next_off);
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_FMOVE: {
                int next_off = 1;
                if (IS_EXTENDED_FP_REGISTER(code, i, next_off)) {
                    int freg;
                    DECODE_FP_REGISTER(freg, code, i, next_off);
                    dreg_t dreg;
                    DECODE_DEST_REGISTER(dreg, code, i, next_off);
                    #ifdef IMPL_EXECUTE_LOOP
                        TRACE("fmove/2 fp%i, %c%i\n", freg, T_DEST_REG(dreg));
                        // Space should be available on heap as compiler added an allocate opcode
                        context_ensure_fpregs(ctx);
                        term float_value = term_from_float(ctx->fr[freg], &ctx->heap);
                        WRITE_REGISTER(dreg, float_value);
                    #endif
                    #ifdef IMPL_CODE_LOADER
                        TRACE("fmove/2\n");
                        UNUSED(freg)
                        UNUSED(dreg)
                    #endif
                } else {
                    term src_value;
                    DECODE_COMPACT_TERM(src_value, code, i, next_off);
                    int freg;
                    DECODE_FP_REGISTER(freg, code, i, next_off);
                    #ifdef IMPL_EXECUTE_LOOP
                        TRACE("fmove/2 %lx, fp%i\n", src_value, freg);
                        context_ensure_fpregs(ctx);
                        ctx->fr[freg] = term_to_float(src_value);
                    #endif
                    #ifdef IMPL_CODE_LOADER
                        TRACE("fmove/2\n");
                        UNUSED(src_value)
                        UNUSED(freg)
                    #endif
                }

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_FCONV: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, code, i, next_off);
                int freg;
                DECODE_FP_REGISTER(freg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("fconv/2 %lx, fp%i\n", src_value, freg);
                    context_ensure_fpregs(ctx);
                    ctx->fr[freg] = term_conv_to_float(src_value);
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("fconv/2\n");
                    UNUSED(freg)
                    UNUSED(src_value)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_FADD: {
                #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                    #pragma STDC FENV_ACCESS ON
                #endif
                int next_off = 1;
                int fail_label;
                DECODE_LABEL(fail_label, code, i, next_off);
                int freg1, freg2, freg3;
                DECODE_FP_REGISTER(freg1, code, i, next_off);
                DECODE_FP_REGISTER(freg2, code, i, next_off);
                DECODE_FP_REGISTER(freg3, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("fadd/3 fp%i, fp%i, fp%i\n", freg1, freg2, freg3);
                    #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                        feclearexcept(FE_OVERFLOW);
                    #endif
                    context_ensure_fpregs(ctx);
                    ctx->fr[freg3] = ctx->fr[freg1] + ctx->fr[freg2];
                    #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                        if (fetestexcept(FE_OVERFLOW)) {
                            RAISE_ERROR(BADARITH_ATOM);
                        }
                    #else
                        if (!isfinite(ctx->fr[freg3])) {
                            RAISE_ERROR(BADARITH_ATOM);
                        }
                    #endif
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("fadd/3\n");
                    UNUSED(freg1)
                    UNUSED(freg2)
                    UNUSED(freg3)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_FSUB: {
                #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                    #pragma STDC FENV_ACCESS ON
                #endif
                int next_off = 1;
                int fail_label;
                DECODE_LABEL(fail_label, code, i, next_off);
                int freg1, freg2, freg3;
                DECODE_FP_REGISTER(freg1, code, i, next_off);
                DECODE_FP_REGISTER(freg2, code, i, next_off);
                DECODE_FP_REGISTER(freg3, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("fsub/3 fp%i, fp%i, fp%i\n", freg1, freg2, freg3);
                    #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                        feclearexcept(FE_OVERFLOW);
                    #endif
                    context_ensure_fpregs(ctx);
                    ctx->fr[freg3] = ctx->fr[freg1] - ctx->fr[freg2];
                    #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                        if (fetestexcept(FE_OVERFLOW)) {
                            RAISE_ERROR(BADARITH_ATOM);
                        }
                    #else
                        if (!isfinite(ctx->fr[freg3])) {
                            RAISE_ERROR(BADARITH_ATOM);
                        }
                    #endif
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("fsub/3\n");
                    UNUSED(freg1)
                    UNUSED(freg2)
                    UNUSED(freg3)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_FMUL: {
                #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                    #pragma STDC FENV_ACCESS ON
                #endif
                int next_off = 1;
                int fail_label;
                DECODE_LABEL(fail_label, code, i, next_off);
                int freg1, freg2, freg3;
                DECODE_FP_REGISTER(freg1, code, i, next_off);
                DECODE_FP_REGISTER(freg2, code, i, next_off);
                DECODE_FP_REGISTER(freg3, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("fmul/3 fp%i, fp%i, fp%i\n", freg1, freg2, freg3);
                    #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                        feclearexcept(FE_OVERFLOW);
                    #endif
                    context_ensure_fpregs(ctx);
                    ctx->fr[freg3] = ctx->fr[freg1] * ctx->fr[freg2];
                    #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                        if (fetestexcept(FE_OVERFLOW)) {
                            RAISE_ERROR(BADARITH_ATOM);
                        }
                    #else
                        if (!isfinite(ctx->fr[freg3])) {
                            RAISE_ERROR(BADARITH_ATOM);
                        }
                    #endif
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("fmul/3\n");
                    UNUSED(freg1)
                    UNUSED(freg2)
                    UNUSED(freg3)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_FDIV: {
                #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                    #pragma STDC FENV_ACCESS ON
                #endif
                int next_off = 1;
                int fail_label;
                DECODE_LABEL(fail_label, code, i, next_off);
                int freg1, freg2, freg3;
                DECODE_FP_REGISTER(freg1, code, i, next_off);
                DECODE_FP_REGISTER(freg2, code, i, next_off);
                DECODE_FP_REGISTER(freg3, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("fdiv/3 fp%i, fp%i, fp%i\n", freg1, freg2, freg3);
                    #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                        feclearexcept(FE_OVERFLOW | FE_DIVBYZERO);
                    #endif
                    context_ensure_fpregs(ctx);
                    ctx->fr[freg3] = ctx->fr[freg1] / ctx->fr[freg2];
                    #ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
                        if (fetestexcept(FE_OVERFLOW | FE_DIVBYZERO)) {
                            RAISE_ERROR(BADARITH_ATOM);
                        }
                    #else
                        if (!isfinite(ctx->fr[freg3])) {
                            RAISE_ERROR(BADARITH_ATOM);
                        }
                    #endif
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("fdiv/3\n");
                    UNUSED(freg1)
                    UNUSED(freg2)
                    UNUSED(freg3)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_FNEGATE: {
                int next_off = 1;
                int fail_label;
                DECODE_LABEL(fail_label, code, i, next_off);
                int freg1, freg2;
                DECODE_FP_REGISTER(freg1, code, i, next_off);
                DECODE_FP_REGISTER(freg2, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("fnegate/2 fp%i, fp%i\n", freg1, freg2);
                    context_ensure_fpregs(ctx);
                    ctx->fr[freg2] = - ctx->fr[freg1];
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("fnegate/2\n");
                    UNUSED(freg1)
                    UNUSED(freg2)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BUILD_STACKTRACE: {
                int next_off = 1;

                TRACE("build_stacktrace/0\n");

                #ifdef IMPL_EXECUTE_LOOP

                    x_regs[0] = stacktrace_build(ctx, &x_regs[0]);

                #endif

                NEXT_INSTRUCTION(next_off);

                break;
            }

#ifdef ENABLE_OTP21
            case OP_GET_HD: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, code, i, next_off)
                dreg_t head_dreg;
                DECODE_DEST_REGISTER(head_dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("get_hd/2 %lx, %c%i\n", src_value, T_DEST_REG(head_dreg));

                    term head = term_get_list_head(src_value);

                    WRITE_REGISTER(head_dreg, head);
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("get_hd/2\n");
                    UNUSED(src_value)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_GET_TL: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, code, i, next_off)
                dreg_t tail_dreg;
                DECODE_DEST_REGISTER(tail_dreg, code, i, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("get_tl/2 %lx, %c%i\n", src_value, T_DEST_REG(tail_dreg));

                    term tail = term_get_list_tail(src_value);

                    WRITE_REGISTER(tail_dreg, tail);
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("get_tl/2\n");
                    UNUSED(src_value)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }
#endif

#ifdef ENABLE_OTP22
            case OP_PUT_TUPLE2: {
                int next_off = 1;
                dreg_gc_safe_t dreg;
                DECODE_DEST_REGISTER_GC_SAFE(dreg, code, i, next_off);
                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                uint32_t size;
                DECODE_LITERAL(size, code, i, next_off)

                TRACE("put_tuple2/2, size=%i\n", size);
                USED_BY_TRACE(size);

                #ifdef IMPL_CODE_LOADER
                    UNUSED(dreg);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    term t = term_alloc_tuple(size, &ctx->heap);
                #endif

                for (uint32_t j = 0; j < size; j++) {
                    term element;
                    DECODE_COMPACT_TERM(element, code, i, next_off)

                    #ifdef IMPL_CODE_LOADER
                        UNUSED(element);
                    #endif

                    #ifdef IMPL_EXECUTE_LOOP
                        term_put_tuple_element(t, j, element);
                    #endif
                }

                #ifdef IMPL_EXECUTE_LOOP
                    WRITE_REGISTER_GC_SAFE(dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }
#endif

#ifdef ENABLE_OTP23
            case OP_SWAP: {
                int next_off = 1;
                dreg_t reg_a;
                DECODE_DEST_REGISTER(reg_a, code, i, next_off);
                dreg_t reg_b;
                DECODE_DEST_REGISTER(reg_b, code, i, next_off);

                TRACE("swap/2 a=%c%i, b=%c%i\n", T_DEST_REG(reg_a), T_DEST_REG(reg_b));

                #ifdef IMPL_EXECUTE_LOOP
                    term a = READ_DEST_REGISTER(reg_a);
                    term b = READ_DEST_REGISTER(reg_b);

                    WRITE_REGISTER(reg_a, b);
                    WRITE_REGISTER(reg_b, a);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_START_MATCH4: {
                #ifdef IMPL_EXECUTE_LOOP
                    // MEMORY_CAN_SHRINK because bs_start_match is classified as gc in beam_ssa_codegen.erl
                    if (memory_ensure_free_opt(ctx, TERM_BOXED_BIN_MATCH_STATE_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                int next_off = 1;
                // fail since OTP 23 might be either 'no_fail', 'resume' or a fail label
                // we are ignoring this right now, but we might use it for future optimizations.
                term fail;
                DECODE_COMPACT_TERM(fail, code, i, next_off);
                uint32_t live;
                DECODE_LITERAL(live, code, i, next_off);
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_start_match4/4\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_start_match4/4, fail=%u live=%u src=%p dreg=%c%i\n", (unsigned) fail, (unsigned) live, (void *) src, T_DEST_REG(dreg));

                    if (!(term_is_binary(src) || term_is_match_state(src))) {
                        WRITE_REGISTER(dreg, src);
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term match_state = term_alloc_bin_match_state(src, 0, &ctx->heap);

                        WRITE_REGISTER(dreg, match_state);
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif
                break;
            }
#endif

#ifdef ENABLE_OTP24
            case OP_MAKE_FUN3: {
                int next_off = 1;
                uint32_t fun_index;
                DECODE_LITERAL(fun_index, code, i, next_off);
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);
                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                uint32_t numfree;
                DECODE_LITERAL(numfree, code, i, next_off)
                TRACE("make_fun3/3, fun_index=%i dreg=%c%i numfree=%i\n", fun_index, T_DEST_REG(dreg), numfree);

                #ifdef IMPL_EXECUTE_LOOP
                    size_t size = numfree + BOXED_FUN_SIZE;
                    term *boxed_func = memory_heap_alloc(&ctx->heap, size);

                    boxed_func[0] = ((size - 1) << 6) | TERM_BOXED_FUN;
                    boxed_func[1] = (term) mod;
                    boxed_func[2] = term_from_int(fun_index);
                #endif

                for (uint32_t j = 0; j < numfree; j++) {
                    term arg;
                    DECODE_COMPACT_TERM(arg, code, i, next_off);
                    #ifdef IMPL_EXECUTE_LOOP
                        boxed_func[3 + j] = arg;
                    #endif
                }

                #ifdef IMPL_EXECUTE_LOOP
                    term fun = ((term) boxed_func) | TERM_BOXED_VALUE_TAG;
                    WRITE_REGISTER(dreg, fun);
                #endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_INIT_YREGS: {
                int next_off = 1;
                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                uint32_t size;
                DECODE_LITERAL(size, code, i, next_off);
                for (uint32_t j = 0; j < size; j++) {
                    uint32_t target;
                    DECODE_YREG(target, code, i, next_off);
                    #ifdef IMPL_EXECUTE_LOOP
                        ctx->e[target] = term_nil();
                    #endif
                }
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_RECV_MARKER_BIND: {
                int next_off = 1;
                dreg_t reg_a;
                DECODE_DEST_REGISTER(reg_a, code, i, next_off);
                dreg_t reg_b;
                DECODE_DEST_REGISTER(reg_b, code, i, next_off);
                TRACE("recv_marker_bind/2: reg1=%c%i reg2=%c%i\n", T_DEST_REG(reg_a), T_DEST_REG(reg_b));
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_RECV_MARKER_CLEAR: {
                int next_off = 1;
                dreg_t reg_a;
                DECODE_DEST_REGISTER(reg_a, code, i, next_off);
                TRACE("recv_marker_clean/1: reg1=%c%i\n", T_DEST_REG(reg_a));
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_RECV_MARKER_RESERVE: {
                int next_off = 1;
                dreg_t reg_a;
                DECODE_DEST_REGISTER(reg_a, code, i, next_off);
                TRACE("recv_marker_reserve/1: reg1=%c%i\n", T_DEST_REG(reg_a));
#ifdef IMPL_EXECUTE_LOOP
                // Clear register to avoid any issue with GC
                WRITE_REGISTER(reg_a, term_nil());
#endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_RECV_MARKER_USE: {
                int next_off = 1;
                dreg_t reg_a;
                DECODE_DEST_REGISTER(reg_a, code, i, next_off);
                TRACE("recv_marker_use/1: reg1=%c%i\n", T_DEST_REG(reg_a));
                NEXT_INSTRUCTION(next_off);
                break;
            }
#endif

#ifdef ENABLE_OTP25
            case OP_BS_CREATE_BIN: {
                int next_off = 1;
                uint32_t fail;
                DECODE_LABEL(fail, code, i, next_off);
                uint32_t alloc;
                DECODE_LITERAL(alloc, code, i, next_off);
                uint32_t live;
                DECODE_LITERAL(live, code, i, next_off);
                uint32_t unit;
                DECODE_LITERAL(unit, code, i, next_off);
                dreg_gc_safe_t dreg;
                DECODE_DEST_REGISTER_GC_SAFE(dreg, code, i, next_off);
                TRACE("bs_create_bin/6 fail=%i, alloc=%i live=%i unit=%i dreg=%c%i\n", fail, alloc, live, unit, T_DEST_REG_GC_SAFE(dreg));
                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                uint32_t list_len;
                DECODE_LITERAL(list_len, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    int list_off = next_off;
                #endif
                size_t nb_segments = list_len / 6;
                #ifdef IMPL_CODE_LOADER
                    if (live > MAX_REG) {
                        fprintf(stderr, "Cannot use more than %d registers.\n", MAX_REG);
                        AVM_ABORT();
                    }
                    if (list_len != nb_segments * 6) {
                        fprintf(stderr, "Unexpected number of operations for bs_create_bin/6, each segment should be 6 elements\n");
                        AVM_ABORT();
                    }
                #endif
                // Compute binary size in first iteration
                #ifdef IMPL_EXECUTE_LOOP
                    size_t binary_size = 0;
                #endif
                for (size_t j = 0; j < nb_segments; j++) {
                    term atom_type;
                    DECODE_ATOM(atom_type, code, i, next_off);
                    int seg;
                    DECODE_LITERAL(seg, code, i, next_off);
                    int segment_unit;
                    DECODE_LITERAL(segment_unit, code, i, next_off);
                    term flags;
                    DECODE_COMPACT_TERM(flags, code, i, next_off);
                    term src;
                    DECODE_COMPACT_TERM(src, code, i, next_off);
                    term size;
                    DECODE_COMPACT_TERM(size, code, i, next_off);
                    #ifdef IMPL_EXECUTE_LOOP
                        avm_int_t src_value = 0;
                        switch (atom_type) {
                            case UTF8_ATOM:
                            case UTF16_ATOM:
                            case UTF32_ATOM:
                                VERIFY_IS_INTEGER(src, "bs_create_bin/6");
                                src_value = term_to_int(src);
                                break;
                        }
                        size_t segment_size = 0;
                        switch (size) {
                            case UNDEFINED_ATOM: {
                                // Silently ignore segment_unit != 0
                                segment_unit = 8;
                                switch (atom_type) {
                                    case UTF8_ATOM: {
                                        if (UNLIKELY(!bitstring_utf8_size(src_value, &segment_size))) {
                                            RAISE_ERROR(BADARG_ATOM);
                                        }
                                        break;
                                    }
                                    case UTF16_ATOM: {
                                        if (UNLIKELY(!bitstring_utf16_size(src_value, &segment_size))) {
                                            RAISE_ERROR(BADARG_ATOM);
                                        }
                                        break;
                                    }
                                    case UTF32_ATOM: {
                                        segment_size = 4;
                                        break;
                                    }
                                    default:
                                        // In Erlang/OTP #5281, this is a compile time check
                                        fprintf(stderr, "Unexpected type %lx for bs_create_bin/6 size undefined\n", (long) atom_type);
                                        AVM_ABORT();
                                }
                                break;
                            }
                            case ALL_ATOM: {
                                if (atom_type != BINARY_ATOM && atom_type != APPEND_ATOM && atom_type != PRIVATE_APPEND_ATOM) {
                                    // In Erlang/OTP #5281, this is a compile time check
                                    fprintf(stderr, "Unexpected type for bs_create_bin/6 size all\n");
                                    AVM_ABORT();
                                }
                                VERIFY_IS_BINARY(src, "bs_create_bin/6");
                                // We only support src as a binary of bytes here.
                                segment_size = term_binary_size(src);
                                segment_unit = 8;
                                break;
                            }
                            default: {
                                if (UNLIKELY(!term_is_integer(size) || term_to_int(size) < 0)) {
                                    RAISE_ERROR(BADARG_ATOM);
                                }
                                segment_size = term_to_int(size);
                            }
                        }
                        binary_size += segment_unit * segment_size;
                    #endif
                }
                // Allocate and build binary in second iteration
                #ifdef IMPL_EXECUTE_LOOP
                    if (binary_size % 8) {
                        TRACE("bs_create_bin/6: total binary size (%li) is not evenly divisible by 8\n", binary_size);
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    context_clean_registers(ctx, live);
                    if (UNLIKELY(memory_ensure_free_opt(ctx, alloc + term_binary_heap_size(binary_size / 8), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    term t = term_create_empty_binary(binary_size / 8, &ctx->heap, ctx->global);
                    size_t offset = 0;

                    for (size_t j = 0; j < nb_segments; j++) {
                        term atom_type;
                        DECODE_ATOM(atom_type, code, i, list_off);
                        int seg;
                        DECODE_LITERAL(seg, code, i, list_off);
                        int segment_unit;
                        DECODE_LITERAL(segment_unit, code, i, list_off);
                        term flags;
                        DECODE_COMPACT_TERM(flags, code, i, list_off);
                        term src;
                        DECODE_COMPACT_TERM(src, code, i, list_off);
                        term size;
                        DECODE_COMPACT_TERM(size, code, i, list_off);
                        size_t segment_size;
                        avm_int_t flags_value = 0;
                        avm_int64_t src_value = 0;
                        size_t size_value = 0;
                        switch (atom_type) {
                            case UTF16_ATOM:
                            case UTF32_ATOM:
                            case INTEGER_ATOM:
                                DECODE_FLAGS_LIST(flags_value, flags, opcode);
                                break;
                            default:
                                break;
                        }
                        switch (atom_type) {
                            case STRING_ATOM:
                            case UTF8_ATOM:
                            case UTF16_ATOM:
                            case UTF32_ATOM:
                                VERIFY_IS_INTEGER(src, "bs_create_bin/6");
                                src_value = term_to_int(src);
                                break;
                            case INTEGER_ATOM:
                                VERIFY_IS_ANY_INTEGER(src, "bs_create_bin/6");
                                src_value = term_maybe_unbox_int64(src);
                                break;
                            default:
                                break;
                        }
                        switch (atom_type) {
                            case INTEGER_ATOM:
                            case STRING_ATOM:
                                VERIFY_IS_INTEGER(size, "bs_create_bin/6");
                                avm_int_t signed_size_value = term_to_int(size);
                                if (UNLIKELY(signed_size_value < 0)) {
                                    TRACE("bs_create_bin/6: size value less than 0: %i\n", (int) signed_size_value);
                                    RAISE_ERROR(BADARG_ATOM);
                                }
                                size_value = (size_t) signed_size_value;
                                break;
                            default:
                                break;
                        }
                        switch (atom_type) {
                            case UTF8_ATOM: {
                                bool result = bitstring_insert_utf8(t, offset, src_value, &segment_size);
                                if (UNLIKELY(!result)) {
                                    TRACE("bs_create_bin/6: Failed to insert character as utf8 into binary: %i\n", result);
                                    RAISE_ERROR(BADARG_ATOM);
                                }
                                segment_size *= 8;
                                break;
                            }
                            case UTF16_ATOM: {
                                bool result = bitstring_insert_utf16(t, offset, src_value, flags_value, &segment_size);
                                if (UNLIKELY(!result)) {
                                    TRACE("bs_create_bin/6: Failed to insert character as utf16 into binary: %i\n", result);
                                    RAISE_ERROR(BADARG_ATOM);
                                }
                                segment_size *= 8;
                                break;
                            }
                            case UTF32_ATOM: {
                                bool result = bitstring_insert_utf32(t, offset, src_value, flags_value);
                                if (UNLIKELY(!result)) {
                                    TRACE("bs_create_bin/6: Failed to insert character as utf16 into binary: %i\n", result);
                                    RAISE_ERROR(BADARG_ATOM);
                                }
                                segment_size = 32;
                                break;
                            }
                            case INTEGER_ATOM: {
                                bool result = bitstring_insert_integer(t, offset, src_value, size_value * segment_unit, flags_value);
                                if (UNLIKELY(!result)) {
                                    TRACE("bs_create_bin/6: Failed to insert integer into binary\n");
                                    RAISE_ERROR(BADARG_ATOM);
                                }
                                segment_size = size_value;
                                break;
                            }
                            case STRING_ATOM: {
                                uint8_t *dst = (uint8_t *) term_binary_data(t);
                                size_t remaining = 0;
                                const uint8_t *str = module_get_str(mod, src_value, &remaining);
                                segment_size = size_value * segment_unit;
                                bitstring_copy_bits(dst, offset, str, segment_size);
                                break;
                            }
                            case APPEND_ATOM:
                            case BINARY_ATOM:
                            case PRIVATE_APPEND_ATOM: {
                                if (offset % 8) {
                                    TRACE("bs_create_bin/6: current offset (%li) is not evenly divisible by 8\n", offset);
                                    RAISE_ERROR(UNSUPPORTED_ATOM);
                                }
                                VERIFY_IS_BINARY(src, "bs_create_bin/6");
                                uint8_t *dst = (uint8_t *) term_binary_data(t) + (offset / 8);
                                const uint8_t *bin = (const uint8_t *) term_binary_data(src);
                                size_t binary_size = term_binary_size(src);
                                if (size != ALL_ATOM) {
                                    VERIFY_IS_INTEGER(size, "bs_create_bin/6");
                                    avm_int_t signed_size_value = term_to_int(size);
                                    if (UNLIKELY(signed_size_value < 0)) {
                                        TRACE("bs_create_bin/6: size value less than 0: %i\n", (int) signed_size_value);
                                        RAISE_ERROR(BADARG_ATOM);
                                    }
                                    size_value = (size_t) signed_size_value;
                                    if (size_value > binary_size) {
                                        RAISE_ERROR(BADARG_ATOM);
                                    }
                                    binary_size = size_value;
                                }
                                memcpy(dst, bin, binary_size);
                                segment_size = binary_size * 8;
                                break;
                            }
                            default: {
                                TRACE("bs_create_bin/6: unsupported type atom_index=%i\n", (int) term_to_atom_index(atom_type));
                                RAISE_ERROR(UNSUPPORTED_ATOM);
                            }
                        }
                        offset += segment_size;
                    }
                    WRITE_REGISTER_GC_SAFE(dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_CALL_FUN2: {
                int next_off = 1;
                term tag;
                DECODE_COMPACT_TERM(tag, code, i, next_off)
                unsigned int args_count;
                DECODE_LITERAL(args_count, code, i, next_off)
                #ifdef IMPL_EXECUTE_LOOP
                    int fun_off = next_off;
                #endif
                term fun;
                DECODE_COMPACT_TERM(fun, code, i, next_off)

                TRACE("call_fun2/3, tag, args_count=%i, fun\n", args_count);
                USED_BY_TRACE(args_count);

                #ifdef IMPL_EXECUTE_LOOP
                    if (UNLIKELY(!term_is_function(fun))) {
                        // We can gc as we are raising
                        if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                        // Decode the function again after GC was possibly run
                        DECODE_COMPACT_TERM(fun, code, i, fun_off)
                        term new_error_tuple = term_alloc_tuple(2, &ctx->heap);
                        term_put_tuple_element(new_error_tuple, 0, BADFUN_ATOM);
                        term_put_tuple_element(new_error_tuple, 1, fun);
                        RAISE_ERROR(new_error_tuple);
                    }
                    CALL_FUN(fun, args_count, next_off)
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_BADRECORD: {
                int next_off = 1;
                TRACE("badrecord/1\n");

                #ifdef IMPL_EXECUTE_LOOP
                    // We can gc as we are raising
                    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    term value;
                    DECODE_COMPACT_TERM(value, code, i, next_off)
                    term new_error_tuple = term_alloc_tuple(2, &ctx->heap);
                    term_put_tuple_element(new_error_tuple, 0, BADRECORD_ATOM);
                    term_put_tuple_element(new_error_tuple, 1, value);
                    RAISE_ERROR(new_error_tuple);
                #endif

                #ifdef IMPL_CODE_LOADER
                    term value;
                    DECODE_COMPACT_TERM(value, code, i, next_off)
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }
#endif

#ifdef ENABLE_OTP26
            case OP_UPDATE_RECORD: {
                int next_off = 1;
                #ifdef IMPL_CODE_LOADER
                    TRACE("update_record/5\n");
                #endif

                term hint;
                DECODE_ATOM(hint, code, i, next_off);
                int size;
                DECODE_LITERAL(size, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    term dst;
                    dst = term_alloc_tuple(size, &ctx->heap);
                #endif
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off);
                dreg_t dreg;
                DECODE_DEST_REGISTER(dreg, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    if (UNLIKELY(!term_is_tuple(src) || (size != term_get_tuple_arity(src)))) {
                        fprintf(stderr, "update_record/5 !term_is_tuple(src) or size doesn't match\n");
                        AVM_ABORT();
                    }
                #endif
                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                int list_len;
                DECODE_LITERAL(list_len, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("update_record/5 hint=%lu, size=%i, src=%p, dst=%p, updates_len=%d\n", hint, size, (void *)src, (void *)dst, list_len);
                    bool reuse = hint == REUSE_ATOM;
                    for (int j = 0;  j < size; j++) {
                        term_put_tuple_element(dst, j, term_get_tuple_element(src, j));
                    }
                #endif
                for (int j = 0;  j < list_len; j+=2) {
                    int update_ix;
                    DECODE_LITERAL(update_ix, code, i, next_off);
                    term update_value;
                    DECODE_COMPACT_TERM(update_value, code, i, next_off);
                    #ifdef IMPL_EXECUTE_LOOP
                        if (reuse) {
                            term old_value = term_get_tuple_element(dst, update_ix - 1);
                            TermCompareResult result = term_compare(update_value, old_value, TermCompareExact, ctx->global);
                            if (result == TermEquals) {
                                continue;
                            } else if (UNLIKELY(result == TermCompareMemoryAllocFail)) {
                                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                            }
                            reuse = false;
                        }
                        term_put_tuple_element(dst, update_ix - 1, update_value);
                    #endif
                }
                #ifdef IMPL_EXECUTE_LOOP
                    if (reuse) {
                        WRITE_REGISTER(dreg, src);
                    } else {
                        WRITE_REGISTER(dreg, dst);
                    }
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_MATCH: {
                int next_off = 1;
                TRACE("bs_match/3\n");

                int fail;
                DECODE_LABEL(fail, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    int match_off = next_off;
                #endif
                term match_state;
                DECODE_COMPACT_TERM(match_state, code, i, next_off);
                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(match_state, "bs_match/3")
                    term bs_bin = term_get_match_state_binary(match_state);
                    size_t bs_offset = term_get_match_state_offset(match_state);
                #endif
                DECODE_EXTENDED_LIST_TAG(code, i, next_off);
                int list_len;
                DECODE_LITERAL(list_len, code, i, next_off);
                int j = 0;
                while (j < list_len) {
                    term command;
                    DECODE_ATOM(command, code, i, next_off);
                    j++;
                    switch (command) {
                        case ENSURE_AT_LEAST_ATOM: {
                            int stride;
                            DECODE_LITERAL(stride, code, i, next_off);
                            j++;
                            int unit; // TODO: check use of unit here
                            DECODE_LITERAL(unit, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                size_t bs_bin_size = term_binary_size(bs_bin);
                                if (UNLIKELY(stride < 0)) {
                                    RAISE_ERROR(BADARG_ATOM);
                                }
                                size_t unsigned_stride = (size_t) stride;
                                if ((bs_bin_size * 8) - bs_offset < unsigned_stride) {
                                    TRACE("bs_match/3: ensure_at_least failed -- bs_bin_size = %d, bs_offset = %d, stride = %d, unit = %d\n", (int) bs_bin_size, (int) bs_offset, (int) stride, (int) unit);
                                    goto bs_match_jump_to_fail;
                                }
                            #endif
                            break;
                        }

                        case ENSURE_EXACTLY_ATOM: {
                            int stride;
                            DECODE_LITERAL(stride, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                if (UNLIKELY(stride < 0)) {
                                    RAISE_ERROR(BADARG_ATOM);
                                }
                                size_t unsigned_stride = (size_t) stride;
                                size_t bs_bin_size = term_binary_size(bs_bin);
                                if ((bs_bin_size * 8) - bs_offset != unsigned_stride) {
                                    TRACE("bs_match/3: ensure_exactly failed -- bs_bin_size = %lu, bs_offset = %lu, stride = %lu\n", (unsigned long) bs_bin_size, (unsigned long) bs_offset, (unsigned long) stride);
                                    goto bs_match_jump_to_fail;
                                }
                            #endif
                            break;
                        }

                        case INTEGER_ATOM: {
                            int live;
                            DECODE_LITERAL(live, code, i, next_off);
                            j++;
                            term flags;
                            DECODE_COMPACT_TERM(flags, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                avm_int_t flags_value;
                                DECODE_FLAGS_LIST(flags_value, flags, opcode)
                            #endif
                            term size;
                            DECODE_COMPACT_TERM(size, code, i, next_off);
                            j++;
                            int unit;
                            DECODE_LITERAL(unit, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                // context_clean_registers(ctx, live); // TODO: check if needed
                                VERIFY_IS_INTEGER(size, "bs_match/3");
                                avm_int_t size_val = term_to_int(size);
                                avm_int_t increment = size_val * unit;
                                union maybe_unsigned_int64 value;
                                bool status = bitstring_extract_integer(bs_bin, bs_offset, increment, flags_value, &value);
                                if (UNLIKELY(!status)) {
                                    TRACE("bs_match/3: error extracting integer.\n");
                                    goto bs_match_jump_to_fail;
                                }
                                term t = maybe_alloc_boxed_integer_fragment(ctx, value.s);
                                if (UNLIKELY(term_is_invalid_term(t))) {
                                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                                }
                            #endif
                            dreg_t dreg;
                            DECODE_DEST_REGISTER(dreg, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                WRITE_REGISTER(dreg, t);
                                bs_offset += increment;
                            #endif
                            break;
                        }

                        case BINARY_ATOM: {
                            int live;
                            DECODE_LITERAL(live, code, i, next_off);
                            j++;
                            term flags;
                            DECODE_COMPACT_TERM(flags, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                avm_int_t flags_value;
                                DECODE_FLAGS_LIST(flags_value, flags, opcode)
                            #endif
                            int size;
                            DECODE_LITERAL(size, code, i, next_off);
                            j++;
                            int unit;
                            DECODE_LITERAL(unit, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                // context_clean_registers(ctx, live); // TODO: check if needed
                                int matched_bits = size * unit;
                                if (bs_offset % 8 != 0 || matched_bits % 8 != 0) {
                                    TRACE("bs_match/3: Unsupported.  Offset on binary read must be aligned on byte boundaries.\n");
                                    RAISE_ERROR(BADARG_ATOM);
                                }
                                if ((bs_offset + matched_bits) > term_binary_size(bs_bin) * 8) {
                                    TRACE("bs_match/3: insufficient capacity\n");
                                    goto bs_match_jump_to_fail;
                                }
                                size_t heap_size = term_sub_binary_heap_size(bs_bin, matched_bits / 8);
                                if (UNLIKELY(memory_ensure_free_opt(ctx, heap_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                                }
                                // re-compute match_state as GC could have moved it
                                int temp = match_off;
                                DECODE_COMPACT_TERM(match_state, code, i, temp);
                                bs_bin = term_get_match_state_binary(match_state);
                                term t = term_maybe_create_sub_binary(bs_bin, bs_offset / 8, matched_bits / 8, &ctx->heap, ctx->global);
                            #endif
                            dreg_t dreg;
                            DECODE_DEST_REGISTER(dreg, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                WRITE_REGISTER(dreg, t);
                                bs_offset += matched_bits;
                            #endif
                            break;
                        }

                        case GET_TAIL_ATOM: {
                            int live;
                            DECODE_LITERAL(live, code, i, next_off);
                            j++;
                            int unit;
                            DECODE_LITERAL(unit, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                // context_clean_registers(ctx, live); // TODO: check if needed
                                size_t total_bits = term_binary_size(bs_bin) * 8;
                                size_t tail_bits = total_bits - bs_offset;
                                if (bs_offset % 8 != 0 || tail_bits % 8 != 0) {
                                    TRACE("bs_match/3: Unsupported.  Offset on binary read must be aligned on byte boundaries.\n");
                                    RAISE_ERROR(BADARG_ATOM);
                                }
                                size_t heap_size = term_sub_binary_heap_size(bs_bin, tail_bits / 8);
                                if (UNLIKELY(memory_ensure_free_opt(ctx, heap_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                                }
                                // re-compute match_state as GC could have moved it
                                int temp = match_off;
                                DECODE_COMPACT_TERM(match_state, code, i, temp);
                                bs_bin = term_get_match_state_binary(match_state);
                                term t = term_maybe_create_sub_binary(bs_bin, bs_offset / 8, tail_bits / 8, &ctx->heap, ctx->global);
                            #endif
                            dreg_t dreg;
                            DECODE_DEST_REGISTER(dreg, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                WRITE_REGISTER(dreg, t);
                                bs_offset = total_bits;
                            #endif
                            break;
                        }

                        case EQUAL_COLON_EQUAL_ATOM: {
                            term live;
                            DECODE_COMPACT_TERM(live, code, i, next_off);
                            j++;
                            int size;
                            DECODE_LITERAL(size, code, i, next_off);
                            j++;
                            avm_int_t pattern_value;
                            DECODE_LITERAL(pattern_value, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                union maybe_unsigned_int64 matched_value;
                                bool status = bitstring_extract_integer(bs_bin, bs_offset, size, 0, &matched_value);
                                if (UNLIKELY(!status)) {
                                    TRACE("bs_match/3: error extracting integer for =:=.\n");
                                    goto bs_match_jump_to_fail;
                                }
                                if (matched_value.s != pattern_value) {
                                    TRACE("bs_match/3: =:= : value doesn't match %lu != %lu\n", (unsigned long) pattern_value, (unsigned long) matched_value.s);
                                    goto bs_match_jump_to_fail;
                                }
                                bs_offset += size;
                            #endif
                            break;
                        }

                        case SKIP_ATOM: {
                            int stride;
                            DECODE_LITERAL(stride, code, i, next_off);
                            j++;
                            #ifdef IMPL_EXECUTE_LOOP
                                bs_offset += stride;
                            #endif
                            break;
                        }

                        default:
                            fprintf(stderr, "bs_match/3: undecoded command: %i, j = %d, list_len = %d\n\n", (int) term_to_atom_index(command), j, list_len);
                            #ifdef IMPL_EXECUTE_LOOP
                                fprintf(stderr, "failed at %" PRIuPTR "\n", i);
                            #endif

                            AVM_ABORT();
                    }
                    #ifdef IMPL_EXECUTE_LOOP
                        term_set_match_state_offset(match_state, bs_offset);
                    #endif
                }
                NEXT_INSTRUCTION(next_off);
                break;

                #ifdef IMPL_EXECUTE_LOOP
bs_match_jump_to_fail:
                    JUMP_TO_ADDRESS(mod->labels[fail]);
                    continue;
                #endif
            }
#endif

            default:
                printf("Undecoded opcode: %i\n", code[i]);
                #ifdef IMPL_EXECUTE_LOOP
                    fprintf(stderr, "failed at %u\n", (unsigned int) i);
                #endif

                AVM_ABORT();
                return 1;
        }

        continue;

#ifdef IMPL_EXECUTE_LOOP
do_abort:
        x_regs[0] = ERROR_ATOM;
        x_regs[1] = VM_ABORT_ATOM;

handle_error:
        {
            int target_label = get_catch_label_and_change_module(ctx, &mod);
            if (target_label) {
                code = mod->code->code;
                JUMP_TO_ADDRESS(mod->labels[target_label]);
                continue;
            }
        }

        // Do not print crash dump if reason is normal.
        if (x_regs[0] != LOWERCASE_EXIT_ATOM || x_regs[1] != NORMAL_ATOM) {
            dump(ctx);
        }

        if (x_regs[0] == LOWERCASE_EXIT_ATOM) {
            ctx->exit_reason = x_regs[1];
        } else {
            bool throw = ctx->x[0] == THROW_ATOM;

            int exit_reason_tuple_size = (throw ? TUPLE_SIZE(2) : 0) + TUPLE_SIZE(2);
            if (memory_ensure_free_opt(ctx, exit_reason_tuple_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
                ctx->exit_reason = OUT_OF_MEMORY_ATOM;
            } else {
                term error_term;
                if (throw) {
                    error_term = term_alloc_tuple(2, &ctx->heap);
                    term_put_tuple_element(error_term, 0, NOCATCH_ATOM);
                    term_put_tuple_element(error_term, 1, x_regs[1]);
                } else {
                    // error
                    error_term = x_regs[1];
                }

                term exit_reason_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(exit_reason_tuple, 0, error_term);
                term_put_tuple_element(exit_reason_tuple, 1, term_nil());
                ctx->exit_reason = exit_reason_tuple;
            }
        }

terminate_context:
        TRACE("-- Code execution finished for %i--\n", ctx->process_id);
        GlobalContext *global = ctx->global;
        if (ctx->leader) {
            scheduler_stop_all(global);
        }
        scheduler_terminate(ctx);
        ctx = scheduler_run(global);
        goto schedule_in;
#endif
    }
}

#ifndef __clang__
#pragma GCC diagnostic pop
#else
#pragma clang diagnostic pop
#endif

#undef DECODE_COMPACT_TERM
