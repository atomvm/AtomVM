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

#include "bif.h"
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
#endif

#define ENABLE_OTP21
#define ENABLE_OTP22
#define ENABLE_OTP23

//#define ENABLE_TRACE

#include "trace.h"

#define COMPACT_LITERAL 0
#define COMPACT_SMALLINT4 1
#define COMPACT_ATOM 2
#define COMPACT_XREG 3
#define COMPACT_YREG 4
#define COMPACT_EXTENDED 7
#define COMPACT_LARGE_LITERAL 8
#define COMPACT_LARGE_INTEGER 9
#define COMPACT_LARGE_ATOM 10
#define COMPACT_LARGE_YREG 12

#define COMPACT_EXTENDED_LITERAL 0x47

#define COMPACT_LARGE_IMM_MASK 0x18
#define COMPACT_11BITS_VALUE 0x8
#define COMPACT_NBITS_VALUE 0x18

typedef int dreg_t;

typedef union
{
    term **ptr;
    int reg_type;
} dreg_type_t;

#ifdef IMPL_EXECUTE_LOOP
#define RAISE_ERROR(error_type_atom) \
    ctx->x[0] = ERROR_ATOM;          \
    ctx->x[1] = error_type_atom;     \
    goto handle_error;

#define VM_ABORT() \
    goto do_abort;

#endif

#ifdef IMPL_CODE_LOADER

#define T_DEST_REG(dreg_type, dreg) \
    reg_type_c((dreg_type).reg_type), ((dreg))

#define DECODE_COMPACT_TERM(dest_term, code_chunk, base_index, off, next_operand_offset)\
{                                                                                       \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                            \
    switch (first_byte & 0xF) {                                                         \
        case COMPACT_LARGE_LITERAL:                                                     \
        case COMPACT_LITERAL:                                                           \
            switch (((first_byte) >> 3) & 0x3) {                                        \
                case 0:                                                                 \
                case 2:                                                                 \
                    dest_term = term_from_int4(first_byte >> 4);                        \
                    next_operand_offset += 1;                                           \
                    break;                                                              \
                                                                                        \
                case 1:                                                                 \
                    dest_term = term_from_int4(((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]); \
                    next_operand_offset += 2;                                           \
                    break;                                                              \
                                                                                        \
                default:                                                                \
                    fprintf(stderr, "Operand not literal: %x, or unsupported encoding\n", (first_byte)); \
                    AVM_ABORT();                                                        \
                    break;                                                              \
            }                                                                           \
            break;                                                                      \
                                                                                        \
        case COMPACT_SMALLINT4:                                                         \
        case COMPACT_ATOM:                                                              \
        case COMPACT_XREG:                                                              \
        case COMPACT_YREG:                                                              \
            next_operand_offset += 1;                                                   \
            break;                                                                      \
                                                                                        \
        case COMPACT_EXTENDED:                                                          \
            switch (first_byte) {                                                       \
                case COMPACT_EXTENDED_LITERAL: {                                        \
                    uint8_t ext = (code_chunk[(base_index) + (off) + 1] & 0xF);         \
                    if (ext == 0) {                                                     \
                        next_operand_offset += 2;                                       \
                    }else if (ext == 0x8) {                                             \
                        next_operand_offset += 3;                                       \
                    } else {                                                            \
                        AVM_ABORT();                                                    \
                    }                                                                   \
                    break;                                                              \
                }                                                                       \
                default:                                                                \
                    printf("Unexpected %i\n", (int) first_byte);                        \
                    AVM_ABORT();                                                        \
                    break;                                                              \
            }                                                                           \
            break;                                                                      \
                                                                                        \
        case COMPACT_LARGE_INTEGER:                                                     \
        case COMPACT_LARGE_ATOM:                                                        \
            switch (first_byte & COMPACT_LARGE_IMM_MASK) {                              \
                case COMPACT_11BITS_VALUE:                                              \
                    next_operand_offset += 2;                                           \
                    break;                                                              \
                                                                                        \
                case COMPACT_NBITS_VALUE:                                               \
                    /* TODO: when first_byte >> 5 is 7, a different encoding is used */ \
                    next_operand_offset += (first_byte >> 5) + 3;                       \
                    break;                                                              \
                                                                                        \
                default:                                                                \
                    assert((first_byte & 0x30) != COMPACT_LARGE_INTEGER);               \
                    break;                                                              \
            }                                                                           \
            break;                                                                      \
                                                                                        \
        case COMPACT_LARGE_YREG:                                                        \
            next_operand_offset += 2;                                                   \
            break;                                                                      \
                                                                                        \
        default:                                                                        \
            fprintf(stderr, "unknown compect term type: %i\n", ((first_byte) & 0xF));   \
            AVM_ABORT();                                                                \
            break;                                                                      \
    }                                                                                   \
}

#define DECODE_DEST_REGISTER(dreg, dreg_type, code_chunk, base_index, off, next_operand_offset)     \
{                                                                                                   \
    uint8_t first_byte = code_chunk[(base_index) + (off)];                                          \
    uint8_t reg_type = first_byte & 0xF;                                                            \
    (dreg_type).reg_type = reg_type;                                                                \
    switch (reg_type) {                                                                             \
        case COMPACT_XREG:                                                                          \
        case COMPACT_YREG:                                                                          \
            (dreg) = code_chunk[(base_index) + (off)] >> 4;                                         \
            next_operand_offset += 1;                                                               \
            break;                                                                                  \
        case COMPACT_LARGE_YREG:                                                                    \
            (dreg) = (((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]);           \
            next_operand_offset += 2;                                                               \
            break;                                                                                  \
        default:                                                                                    \
            AVM_ABORT();                                                                            \
    }                                                                                               \
}
#endif

#ifdef IMPL_EXECUTE_LOOP

#define T_DEST_REG(dreg_type, dreg) \
    (*dreg_type.ptr == ctx->x) ? 'x' : 'y', (dreg)

#define DECODE_COMPACT_TERM(dest_term, code_chunk, base_index, off, next_operand_offset)                                \
{                                                                                                                       \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                                                            \
    switch (first_byte & 0xF) {                                                                                         \
        case COMPACT_LARGE_LITERAL:                                                                                     \
        case COMPACT_LITERAL:                                                                                           \
            switch (((first_byte) >> 3) & 0x3) {                                                                        \
                case 0:                                                                                                 \
                case 2:                                                                                                 \
                    dest_term = term_from_int4(first_byte >> 4);                                                        \
                    next_operand_offset += 1;                                                                           \
                    break;                                                                                              \
                                                                                                                        \
                case 1:                                                                                                 \
                    dest_term = term_from_int4(((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]);      \
                    next_operand_offset += 2;                                                                           \
                    break;                                                                                              \
                                                                                                                        \
                default:                                                                                                \
                    fprintf(stderr, "Operand not a literal: %x, or unsupported encoding\n", (first_byte));              \
                    AVM_ABORT();                                                                                        \
                    break;                                                                                              \
            }                                                                                                           \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_SMALLINT4:                                                                                         \
            dest_term = term_from_int4(first_byte >> 4);                                                                \
            next_operand_offset += 1;                                                                                   \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_ATOM:                                                                                              \
            if (first_byte == COMPACT_ATOM) {                                                                           \
                dest_term = term_nil();                                                                                 \
            } else {                                                                                                    \
                dest_term = module_get_atom_term_by_id(mod, first_byte >> 4);                                           \
            }                                                                                                           \
            next_operand_offset += 1;                                                                                   \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_XREG:                                                                                              \
            dest_term = ctx->x[first_byte >> 4];                                                                        \
            next_operand_offset += 1;                                                                                   \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_YREG:                                                                                              \
            dest_term = ctx->e[first_byte >> 4];                                                                        \
            next_operand_offset += 1;                                                                                   \
            break;                                                                                                      \
                                                                                                                        \
        case COMPACT_EXTENDED:                                                                                          \
            switch (first_byte) {                                                                                       \
                case COMPACT_EXTENDED_LITERAL: {                                                                        \
                    uint8_t first_extended_byte = code_chunk[(base_index) + (off) + 1];                                 \
                    if (!(first_extended_byte & 0xF)) {                                                                 \
                        dest_term = module_load_literal(mod, first_extended_byte >> 4, ctx);                            \
                        next_operand_offset += 2;                                                                       \
                    } else if ((first_extended_byte & 0xF) == 0x8) {                                                    \
                        uint8_t byte_1 = code_chunk[(base_index) + (off) + 2];                                          \
                        uint16_t index = (((uint16_t) first_extended_byte & 0xE0) << 3) | byte_1;                       \
                        dest_term = module_load_literal(mod, index, ctx);                                               \
                        next_operand_offset += 3;                                                                       \
                    } else {                                                                                            \
                        VM_ABORT();                                                                                     \
                    }                                                                                                   \
                    if (UNLIKELY(term_is_invalid_term(dest_term))) {                                                    \
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);                                                                \
                    }                                                                                                   \
                                                                                                                        \
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
                    next_operand_offset += 2;                                                                           \
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
                    next_operand_offset += 2;                                                                           \
                    break;                                                                                              \
                                                                                                                        \
                case COMPACT_NBITS_VALUE:                                                                               \
                    dest_term = large_integer_to_term(ctx, (code_chunk) + (base_index) + (off), &(next_operand_offset));\
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
                next_operand_offset += 2;                                                                               \
            } else {                                                                                                    \
                VM_ABORT();                                                                                             \
            }                                                                                                           \
            break;                                                                                                      \
                                                                                                                        \
        default:                                                                                                        \
            VM_ABORT();                                                                                                 \
    }                                                                                                                   \
}

#define READ_DEST_REGISTER(dreg_type, dreg) \
    *(*((dreg_type).ptr) + (dreg));


#define WRITE_REGISTER(dreg_type, dreg, value)                                                      \
{                                                                                                   \
    *(*((dreg_type).ptr) + (dreg)) = value;                                                         \
}

#define DECODE_DEST_REGISTER(dreg, dreg_type, code_chunk, base_index, off, next_operand_offset)                 \
{                                                                                                               \
    uint8_t first_byte = code_chunk[(base_index) + (off)];                                                      \
    uint8_t reg_type = first_byte & 0xF;                                                                        \
    uint8_t reg_index = (first_byte >> 4);                                                                      \
    switch (reg_type) {                                                                                         \
        case COMPACT_XREG:                                                                                      \
            (dreg_type).ptr = &x_regs;                                                                          \
            (dreg) = reg_index;                                                                                 \
            next_operand_offset++;                                                                              \
            break;                                                                                              \
        case COMPACT_YREG:                                                                                      \
            (dreg_type).ptr = &ctx->e;                                                                          \
            (dreg) = reg_index;                                                                                 \
            next_operand_offset++;                                                                              \
            break;                                                                                              \
        case COMPACT_LARGE_YREG:                                                                                \
            if (LIKELY((first_byte & COMPACT_LARGE_IMM_MASK) == COMPACT_11BITS_VALUE)) {                        \
                (dreg_type).ptr = &ctx->e;                                                                      \
                (dreg) = (((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1]);                   \
                next_operand_offset += 2;                                                                       \
            } else {                                                                                            \
                VM_ABORT();                                                                                     \
            }                                                                                                   \
            break;                                                                                              \
        default:                                                                                                \
            VM_ABORT();                                                                                         \
    }                                                                                                           \
}
#endif

#define DECODE_LABEL(label, code_chunk, base_index, off, next_operand_offset)                       \
{                                                                                                   \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                                        \
    switch (((first_byte) >> 3) & 0x3) {                                                            \
        case 0:                                                                                     \
        case 2:                                                                                     \
            label = first_byte >> 4;                                                                \
            next_operand_offset += 1;                                                               \
            break;                                                                                  \
                                                                                                    \
        case 1:                                                                                     \
            label = ((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1];              \
            next_operand_offset += 2;                                                               \
            break;                                                                                  \
                                                                                                    \
        default:                                                                                    \
            fprintf(stderr, "Operand not a label: %x, or unsupported encoding\n", (first_byte));    \
            AVM_ABORT();                                                                            \
            break;                                                                                  \
    }                                                                                               \
}

#define DECODE_ATOM(atom, code_chunk, base_index, off, next_operand_offset)                         \
{                                                                                                   \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                                        \
    switch (((first_byte) >> 3) & 0x3) {                                                            \
        case 0:                                                                                     \
        case 2:                                                                                     \
            atom = first_byte >> 4;                                                                 \
            next_operand_offset += 1;                                                               \
            break;                                                                                  \
                                                                                                    \
        case 1:                                                                                     \
            atom = ((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1];               \
            next_operand_offset += 2;                                                               \
            break;                                                                                  \
                                                                                                    \
        default:                                                                                    \
            fprintf(stderr, "Operand not a label: %x, or unsupported encoding\n", (first_byte));    \
            AVM_ABORT();                                                                            \
            break;                                                                                  \
    }                                                                                               \
}

#define DECODE_INTEGER(label, code_chunk, base_index, off, next_operand_offset)                     \
{                                                                                                   \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                                        \
    switch (((first_byte) >> 3) & 0x3) {                                                            \
        case 0:                                                                                     \
        case 2:                                                                                     \
            label = first_byte >> 4;                                                                \
            next_operand_offset += 1;                                                               \
            break;                                                                                  \
                                                                                                    \
        case 1:                                                                                     \
            label = ((first_byte & 0xE0) << 3) | code_chunk[(base_index) + (off) + 1];              \
            next_operand_offset += 2;                                                               \
            break;                                                                                  \
                                                                                                    \
        default:                                                                                    \
            fprintf(stderr, "Operand not an integer: %x, or unsupported encoding\n", (first_byte)); \
            AVM_ABORT();                                                                            \
            break;                                                                                  \
    }                                                                                               \
}

#define NEXT_INSTRUCTION(operands_size) \
    i += operands_size

#ifndef TRACE_JUMP
    #define JUMP_TO_ADDRESS(address) \
        i = ((uint8_t *) (address)) - code
#else
    #define JUMP_TO_ADDRESS(address) \
        i = ((uint8_t *) (address)) - code; \
        fprintf(stderr, "going to jump to %i\n", i)
#endif

#define SCHEDULE_NEXT(restore_mod, restore_to) \
    {                                                                                             \
        ctx->saved_ip = restore_to;                                                               \
        ctx->jump_to_on_restore = NULL;                                                           \
        ctx->saved_module = restore_mod;                                                          \
        Context *scheduled_context = scheduler_next(ctx->global, ctx);                            \
        ctx = scheduled_context;                                                                  \
        x_regs = ctx->x;                                                                          \
        mod = ctx->saved_module;                                                                  \
        code = mod->code->code;                                                                   \
        remaining_reductions = DEFAULT_REDUCTIONS_AMOUNT;                                         \
        JUMP_TO_ADDRESS(scheduled_context->saved_ip);                                             \
    }

#define INSTRUCTION_POINTER() \
    ((const void *) &code[i])

#define DO_RETURN()                                     \
    mod = mod->global->modules_by_index[ctx->cp >> 24]; \
    code = mod->code->code;                             \
    i = (ctx->cp & 0xFFFFFF) >> 2;

#define POINTER_TO_II(instruction_pointer) \
    (((uint8_t *) (instruction_pointer)) - code)

#define HANDLE_ERROR() \
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

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))

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

static void sort_kv_pairs(Context *ctx, struct kv_pair *kv, int size)
{
    int k = size;
    while (1 < k) {
        int max_pos = 0;
        for (int i = 1; i < k; i++) {
            term t_max = kv[max_pos].key;
            term t = kv[i].key;
            int c = term_compare(t, t_max, ctx);
            if (0 < c) {
                max_pos = i;
            }
        }
        if (max_pos != k - 1) {
            SWAP_KV_PAIR(k - 1, max_pos);
        }
        k--;
        // kv[k..size] sorted
    }
}

static int get_catch_label_and_change_module(Context *ctx, Module **mod)
{
    term *ct = ctx->e;
    term *last_frame = ctx->e;

    while (ct != ctx->stack_base) {
        if (term_is_catch_label(*ct)) {
            int target_module;
            int target_label = term_to_catch_label_and_module(*ct, &target_module);
            TRACE("- found catch: label: %i, module: %i\n", target_label, target_module);
            *mod = ctx->global->modules_by_index[target_module];

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
    Module *mod = ctx->global->modules_by_index[cp >> 24];
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
    fprintf(stderr, "\n\nStack \n------\n\n");

    term *ct = ctx->e;

    while (ct != ctx->stack_base) {
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
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->mailbox) {
        Message *msg = GET_LIST_ENTRY(item, Message, mailbox_list_head);
        term_display(stderr, msg->message, ctx);
        fprintf(stderr, "\n");
    }

    fprintf(stderr, "\n\n**End Of Crash Report**\n");
}

static term maybe_alloc_boxed_integer_fragment(Context *ctx, avm_int64_t value)
{
#if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
    if ((value < AVM_INT_MIN) || (value > AVM_INT_MAX)) {
        term *fragment = memory_alloc_heap_fragment(ctx, BOXED_INT64_SIZE);
        if (IS_NULL_PTR(fragment)) {
            ctx->x[0] = ERROR_ATOM;
            ctx->x[1] = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        term_put_int64(fragment, value);
        return ((term) fragment) | ((term) TERM_BOXED_VALUE_TAG);
    } else
#endif
    if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        term *fragment = memory_alloc_heap_fragment(ctx, BOXED_INT_SIZE);
        if (IS_NULL_PTR(fragment)) {
            ctx->x[0] = ERROR_ATOM;
            ctx->x[1] = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        term_put_int(fragment, value);
        return ((term) fragment) | TERM_BOXED_VALUE_TAG;
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

    int size = 2 + n_freeze;
    if (memory_ensure_free(ctx, size + 1) != MEMORY_GC_OK) {
        return term_invalid_term();
    }
    term *boxed_func = memory_heap_alloc(ctx, size + 1);

    boxed_func[0] = (size << 6) | TERM_BOXED_FUN;
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
    BifImpl bif = bif_registry_get_handler(module_name, function_name, arity);
    if (bif) {
        if (bif_registry_is_gc_bif(module_name, function_name, arity)) {
            switch (arity) {
                case 1: {
                    GCBifImpl1 gcbif1 = (GCBifImpl1) bif;
                    *return_value = gcbif1(ctx, 0, ctx->x[0]);
                    return true;
                }
                case 2: {
                    GCBifImpl2 gcbif2 = (GCBifImpl2) bif;
                    *return_value = gcbif2(ctx, 0, ctx->x[0], ctx->x[1]);
                    return true;
                }
                case 3: {
                    GCBifImpl3 gcbif3 = (GCBifImpl3) bif;
                    *return_value = gcbif3(ctx, 0, ctx->x[0], ctx->x[1], ctx->x[2]);
                    return true;
                }
            }
        } else {
            switch (arity) {
                case 0: {
                    BifImpl0 bif0 = (BifImpl0) bif;
                    *return_value = bif0(ctx);
                    return true;
                }
                case 1: {
                    BifImpl1 bif1 = (BifImpl1) bif;
                    *return_value = bif1(ctx, ctx->x[0]);
                    return true;
                }
                case 2: {
                    BifImpl2 bif2 = (BifImpl2) bif;
                    *return_value = bif2(ctx, ctx->x[0], ctx->x[1]);
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

#pragma GCC diagnostic push
#ifdef __GNUC__
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif
#endif

#ifdef IMPL_CODE_LOADER
    int read_core_chunk(Module *mod)
#else
    #ifdef IMPL_EXECUTE_LOOP
        HOT_FUNC int context_execute_loop(Context *ctx, Module *mod, const char *function_name, int arity)
    #else
        #error Need implementation type
    #endif
#endif
{
    uint8_t *code = mod->code->code;
    #ifdef IMPL_EXECUTE_LOOP
        term *x_regs = ctx->x;
    #endif

    unsigned int i = 0;

    #ifdef IMPL_CODE_LOADER
        TRACE("-- Loading code\n");
    #endif

    #ifdef IMPL_EXECUTE_LOOP
        TRACE("-- Executing code\n");

        int function_len = strlen(function_name);
        uint8_t *tmp_atom_name = malloc(function_len + 1);
        tmp_atom_name[0] = function_len;
        memcpy(tmp_atom_name + 1, function_name, function_len);

        int label = module_search_exported_function(mod, tmp_atom_name, arity);
        free(tmp_atom_name);

        if (UNLIKELY(!label)) {
            fprintf(stderr, "No %s/%i function found.\n", function_name, arity);
            return 0;
        }

        ctx->cp = module_address(mod->module_index, mod->end_instruction_ii);
        JUMP_TO_ADDRESS(mod->labels[label]);

        int remaining_reductions = DEFAULT_REDUCTIONS_AMOUNT;
    #endif

    while (1) {

        switch (code[i]) {
            case OP_LABEL: {
                int label;
                int next_offset = 1;
                DECODE_LABEL(label, code, i, next_offset, next_offset)

                TRACE("label/1 label=%i\n", label);
                USED_BY_TRACE(label);

                #ifdef IMPL_CODE_LOADER
                    TRACE("Mark label %i here at %i\n", label, i);
                    module_add_label(mod, label, &code[i]);
                #endif

                NEXT_INSTRUCTION(next_offset);
                break;
            }

            case OP_FUNC_INFO: {
                int next_offset = 1;
                int module_atom;
                DECODE_ATOM(module_atom, code, i, next_offset, next_offset)
                int function_name_atom;
                DECODE_ATOM(function_name_atom, code, i, next_offset, next_offset)
                int arity;
                DECODE_INTEGER(arity, code, i, next_offset, next_offset);

                TRACE("func_info/3 module_name_a=%i, function_name_a=%i, arity=%i\n", module_atom, function_name_atom, arity);
                USED_BY_TRACE(function_name_atom);
                USED_BY_TRACE(module_atom);
                USED_BY_TRACE(arity);

                #ifdef IMPL_EXECUTE_LOOP
                    RAISE_ERROR(FUNCTION_CLAUSE_ATOM);
                #endif

                NEXT_INSTRUCTION(next_offset);
                break;
            }

            case OP_INT_CALL_END: {
                TRACE("int_call_end!\n");

            #ifdef IMPL_CODE_LOADER
                TRACE("-- Code loading finished --\n");
                return i;
            #endif

            #ifdef IMPL_EXECUTE_LOOP
                ctx->exit_reason = NORMAL_ATOM;
                goto terminate_context;
            #endif
            }

            case OP_CALL: {
                int next_offset = 1;
                int arity;
                DECODE_INTEGER(arity, code, i, next_offset, next_offset);
                int label;
                DECODE_LABEL(label, code, i, next_offset, next_offset);

                TRACE("call/2, arity=%i, label=%i\n", arity, label);
                USED_BY_TRACE(arity);

                #ifdef IMPL_EXECUTE_LOOP
                    NEXT_INSTRUCTION(next_offset);
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
                    NEXT_INSTRUCTION(next_offset);
                #endif

                break;
            }

            case OP_CALL_LAST: {
                int next_offset = 1;
                int arity;
                DECODE_INTEGER(arity, code, i, next_offset, next_offset);
                int label;
                DECODE_LABEL(label, code, i, next_offset, next_offset);
                int n_words;
                DECODE_INTEGER(n_words, code, i, next_offset, next_offset);

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
                    NEXT_INSTRUCTION(next_offset);
                #endif

                break;
            }

            case OP_CALL_ONLY: {
                int next_off = 1;
                int arity;
                DECODE_INTEGER(arity, code, i, next_off, next_off);
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)

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
                int arity;
                DECODE_INTEGER(arity, code, i, next_off, next_off);
                int index;
                DECODE_INTEGER(index, code, i, next_off, next_off);

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
                        continue;
                    }

                    NEXT_INSTRUCTION(next_off);

                    TRACE_CALL_EXT(ctx, mod, "call_ext", index, arity);

                    const struct ExportedFunction *func = mod->imported_funcs[index].func;

                    if (func->type == UnresolvedFunctionCall) {
                        const struct ExportedFunction *resolved_func = module_resolve_function(mod, index);
                        if (IS_NULL_PTR(resolved_func)) {
                            RAISE_ERROR(UNDEF_ATOM);
                        }
                        func = resolved_func;
                    }

                    switch (func->type) {
                        case NIFFunctionType: {
                            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
                            term return_value = nif->nif_ptr(ctx, arity, ctx->x);
                            if (UNLIKELY(term_is_invalid_term(return_value))) {
                                HANDLE_ERROR();
                            }
                            ctx->x[0] = return_value;
                            break;
                        }
                        case ModuleFunction: {
                            const struct ModuleFunction *jump = EXPORTED_FUNCTION_TO_MODULE_FUNCTION(func);

                            ctx->cp = module_address(mod->module_index, i);
                            mod = jump->target;
                            code = mod->code->code;
                            JUMP_TO_ADDRESS(mod->labels[jump->label]);

                            break;
                        }
                        default: {
                            fprintf(stderr, "Invalid function type %i at index: %i\n", func->type, index);
                            AVM_ABORT();
                        }
                    }
                #endif

                break;
            }

            case OP_CALL_EXT_LAST: {
                int next_off = 1;
                int arity;
                DECODE_INTEGER(arity, code, i, next_off, next_off);
                int index;
                DECODE_INTEGER(index, code, i, next_off, next_off);
                int n_words;
                DECODE_INTEGER(n_words, code, i, next_off, next_off);

                TRACE("call_ext_last/3, arity=%i, index=%i, n_words=%i\n", arity, index, n_words);
                USED_BY_TRACE(arity);
                USED_BY_TRACE(index);
                USED_BY_TRACE(n_words);

                #ifdef IMPL_EXECUTE_LOOP
                    remaining_reductions--;
                    if (UNLIKELY(!remaining_reductions)) {
                        SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                        continue;
                    }

                    TRACE_CALL_EXT(ctx, mod, "call_ext_last", index, arity);

                    ctx->cp = ctx->e[n_words];
                    ctx->e += (n_words + 1);

                    const struct ExportedFunction *func = mod->imported_funcs[index].func;

                    if (func->type == UnresolvedFunctionCall) {
                        const struct ExportedFunction *resolved_func = module_resolve_function(mod, index);
                        if (IS_NULL_PTR(resolved_func)) {
                            RAISE_ERROR(UNDEF_ATOM);
                        }
                        func = resolved_func;
                    }

                    switch (func->type) {
                        case NIFFunctionType: {
                            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
                            term return_value = nif->nif_ptr(ctx, arity, ctx->x);
                            if (UNLIKELY(term_is_invalid_term(return_value))) {
                                HANDLE_ERROR();
                            }
                            ctx->x[0] = return_value;

                            DO_RETURN();

                            break;
                        }
                        case ModuleFunction: {
                            const struct ModuleFunction *jump = EXPORTED_FUNCTION_TO_MODULE_FUNCTION(func);

                            mod = jump->target;
                            code = mod->code->code;
                            JUMP_TO_ADDRESS(mod->labels[jump->label]);

                            break;
                        }
                        default: {
                            fprintf(stderr, "Invalid function type %i at index: %i\n", func->type, index);
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
                int bif;
                DECODE_INTEGER(bif, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("bif0/2 bif=%i, dreg=%c%i\n", bif, T_DEST_REG(dreg_type, dreg));
                USED_BY_TRACE(bif);
                USED_BY_TRACE(dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    BifImpl0 func = (BifImpl0) mod->imported_funcs[bif].bif;
                    DEBUG_FAIL_NULL(func);
                    term ret = func(ctx);

                    WRITE_REGISTER(dreg_type, dreg, ret);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            //TODO: implement me
            case OP_BIF1: {
                int next_off = 1;
                int fail_label;
                DECODE_LABEL(fail_label, code, i, next_off, next_off);
                int bif;
                DECODE_INTEGER(bif, code, i, next_off, next_off);
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("bif1/2 bif=%i, fail=%i, dreg=%c%i\n", bif, fail_label, T_DEST_REG(dreg_type, dreg));
                USED_BY_TRACE(bif);
                USED_BY_TRACE(dreg);

                #ifdef IMPL_CODE_LOADER
                    UNUSED(arg1);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    BifImpl1 func = (BifImpl1) mod->imported_funcs[bif].bif;
                    DEBUG_FAIL_NULL(func);
                    term ret = func(ctx, arg1);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        HANDLE_ERROR();
                    }

                    WRITE_REGISTER(dreg_type, dreg, ret);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            //TODO: implement me
            case OP_BIF2: {
                int next_off = 1;
                int fail_label;
                DECODE_LABEL(fail_label, code, i, next_off, next_off);
                int bif;
                DECODE_INTEGER(bif, code, i, next_off, next_off);
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off)
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("bif2/2 bif=%i, fail=%i, dreg=%c%i\n", bif, fail_label, T_DEST_REG(dreg_type, dreg));
                USED_BY_TRACE(bif);
                USED_BY_TRACE(dreg);

                #ifdef IMPL_CODE_LOADER
                    UNUSED(arg1);
                    UNUSED(arg2);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    BifImpl2 func = (BifImpl2) mod->imported_funcs[bif].bif;
                    DEBUG_FAIL_NULL(func);
                    term ret = func(ctx, arg1, arg2);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        HANDLE_ERROR();
                    }

                    WRITE_REGISTER(dreg_type, dreg, ret);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_ALLOCATE: {
                int next_off = 1;
                int stack_need;
                DECODE_INTEGER(stack_need, code, i, next_off, next_off);
                int live;
                DECODE_INTEGER(live, code, i, next_off, next_off);
                TRACE("allocate/2 stack_need=%i, live=%i\n" , stack_need, live);
                USED_BY_TRACE(stack_need);
                USED_BY_TRACE(live);

                #ifdef IMPL_EXECUTE_LOOP
                    if (live > ctx->avail_registers) {
                        fprintf(stderr, "Cannot use more than 16 registers.");
                        AVM_ABORT();
                    }

                    context_clean_registers(ctx, live);

                    if (ctx->heap_ptr > ctx->e - (stack_need + 1)) {
                        if (UNLIKELY(memory_ensure_free(ctx, stack_need + 1) != MEMORY_GC_OK)) {
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
                int stack_need;
                DECODE_INTEGER(stack_need, code, i, next_off, next_off);
                int heap_need;
                DECODE_INTEGER(heap_need, code, i, next_off, next_off);
                int live;
                DECODE_INTEGER(live, code, i, next_off, next_off);
                TRACE("allocate_heap/2 stack_need=%i, heap_need=%i, live=%i\n", stack_need, heap_need, live);
                USED_BY_TRACE(stack_need);
                USED_BY_TRACE(heap_need);
                USED_BY_TRACE(live);

                #ifdef IMPL_EXECUTE_LOOP
                    if (live > ctx->avail_registers) {
                        fprintf(stderr, "Cannot use more than 16 registers.");
                        AVM_ABORT();
                    }

                    context_clean_registers(ctx, live);

                    if ((ctx->heap_ptr + heap_need) > ctx->e - (stack_need + 1)) {
                        if (UNLIKELY(memory_ensure_free(ctx, heap_need + stack_need + 1) != MEMORY_GC_OK)) {
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
                int stack_need;
                DECODE_INTEGER(stack_need, code, i, next_off, next_off);
                int live;
                DECODE_INTEGER(live, code, i, next_off, next_off);
                TRACE("allocate_zero/2 stack_need=%i, live=%i\n", stack_need, live);
                USED_BY_TRACE(stack_need);
                USED_BY_TRACE(live);

                #ifdef IMPL_EXECUTE_LOOP
                    if (live > ctx->avail_registers) {
                        fprintf(stderr, "Cannot use more than 16 registers.");
                        AVM_ABORT();
                    }

                    context_clean_registers(ctx, live);

                    if (ctx->heap_ptr > ctx->e - (stack_need + 1)) {
                        if (UNLIKELY(memory_ensure_free(ctx, stack_need + 1) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    }

                    ctx->e -= stack_need + 1;
                    for (int s = 0; s < stack_need; s++) {
                        ctx->e[s] = term_nil();
                    }
                    ctx->e[stack_need] = ctx->cp;
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_ALLOCATE_HEAP_ZERO: {
                int next_off = 1;
                int stack_need;
                DECODE_INTEGER(stack_need, code, i, next_off, next_off);
                int heap_need;
                DECODE_INTEGER(heap_need, code, i, next_off, next_off);
                int live;
                DECODE_INTEGER(live, code, i, next_off, next_off);
                TRACE("allocate_heap_zero/3 stack_need=%i, heap_need=%i, live=%i\n", stack_need, heap_need, live);
                USED_BY_TRACE(stack_need);
                USED_BY_TRACE(heap_need);
                USED_BY_TRACE(live);

                #ifdef IMPL_EXECUTE_LOOP
                    if (live > ctx->avail_registers) {
                        fprintf(stderr, "Cannot use more than 16 registers.");
                        AVM_ABORT();
                    }

                    context_clean_registers(ctx, live);

                    if ((ctx->heap_ptr + heap_need) > ctx->e - (stack_need + 1)) {
                        if (UNLIKELY(memory_ensure_free(ctx, heap_need + stack_need + 1) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    }
                    ctx->e -= stack_need + 1;
                    for (int s = 0; s < stack_need; s++) {
                        ctx->e[s] = term_nil();
                    }
                    ctx->e[stack_need] = ctx->cp;
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TEST_HEAP: {
                int next_offset = 1;
                unsigned int heap_need;
                DECODE_INTEGER(heap_need, code, i, next_offset, next_offset);
                int live_registers;
                DECODE_INTEGER(live_registers, code, i, next_offset, next_offset);

                TRACE("test_heap/2 heap_need=%i, live_registers=%i\n", heap_need, live_registers);
                USED_BY_TRACE(heap_need);
                USED_BY_TRACE(live_registers);

                #ifdef IMPL_EXECUTE_LOOP
                    size_t heap_free = context_avail_free_memory(ctx);
                    // if we need more heap space than is currently free, then try to GC the needed space
                    if (heap_free < heap_need) {
                        context_clean_registers(ctx, live_registers);
                        if (UNLIKELY(memory_ensure_free(ctx, heap_need) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    // otherwise, there is enough space for the needed heap, but there might
                    // more more than necessary.  In that case, try to shrink the heap.
                    } else if (heap_free > heap_need * HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF) {
                        context_clean_registers(ctx, live_registers);
                        if (UNLIKELY(memory_ensure_free(ctx, heap_need * (HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF / 2)) != MEMORY_GC_OK)) {
                            TRACE("Unable to ensure free memory.  heap_need=%i\n", heap_need);
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }
                    }
                #endif

                NEXT_INSTRUCTION(next_offset);
                break;
            }

            case OP_KILL: {
                int next_offset = 1;
                int target;
                DECODE_INTEGER(target, code, i, next_offset, next_offset);

                TRACE("kill/1 target=%i\n", target);

                #ifdef IMPL_EXECUTE_LOOP
                    ctx->e[target] = term_nil();
                #endif

                NEXT_INSTRUCTION(next_offset);

                break;
            }

            case OP_DEALLOCATE: {
                int next_off = 1;
                int n_words;
                DECODE_INTEGER(n_words, code, i, next_off, next_off);

                TRACE("deallocate/1 n_words=%i\n", n_words);
                USED_BY_TRACE(n_words);

                #ifdef IMPL_EXECUTE_LOOP
                    DEBUG_DUMP_STACK(ctx);

                    ctx->cp = ctx->e[n_words];
                    ctx->e += n_words + 1;
                    DEBUG_DUMP_STACK(ctx);
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
                    int local_process_id = term_to_local_process_id(ctx->x[0]);
                    TRACE("send/0 target_pid=%i\n", local_process_id);
                    TRACE_SEND(ctx, ctx->x[0], ctx->x[1]);
                    Context *target = globalcontext_get_process(ctx->global, local_process_id);
                    if (!IS_NULL_PTR(target)) {
                        mailbox_send(target, ctx->x[1]);
                    }

                    ctx->x[0] = ctx->x[1];
                #endif

                NEXT_INSTRUCTION(1);
                break;
            }

            case OP_REMOVE_MESSAGE: {
                TRACE("remove_message/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    if (ctx->flags & (WaitingTimeout | WaitingTimeoutExpired)) {
                        scheduler_cancel_timeout(ctx);
                    }
                    mailbox_remove(ctx);

                    struct ListHead *item;
                    struct ListHead *tmp;
                    MUTABLE_LIST_FOR_EACH(item, tmp, &ctx->save_queue) {
                        list_prepend(&ctx->mailbox, item);
                    }
                    list_init(&ctx->save_queue);
                #endif

                NEXT_INSTRUCTION(1);
                break;
            }

            case OP_TIMEOUT: {
                TRACE("timeout/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    ctx->flags &= ~WaitingTimeoutExpired;

                    struct ListHead *item;
                    struct ListHead *tmp;
                    MUTABLE_LIST_FOR_EACH(item, tmp, &ctx->save_queue) {
                        list_prepend(&ctx->mailbox, item);
                    }
                    list_init(&ctx->save_queue);
                #endif

                NEXT_INSTRUCTION(1);
                break;
            }

            case OP_LOOP_REC: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("loop_rec/2, dreg=%c%i\n", T_DEST_REG(dreg_type, dreg));
                USED_BY_TRACE(dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    if (list_is_empty(&ctx->mailbox)) {
                        JUMP_TO_ADDRESS(mod->labels[label]);
                    } else {
                        term ret = mailbox_peek(ctx);
                        TRACE_RECEIVE(ctx, ret);

                        WRITE_REGISTER(dreg_type, dreg, ret);
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_LOOP_REC_END: {
                int next_offset = 1;
                int label;
                DECODE_LABEL(label, code, i, next_offset, next_offset);

                TRACE("loop_rec_end/1 label=%i\n", label);
                USED_BY_TRACE(label);

#ifdef IMPL_EXECUTE_LOOP
                struct ListHead *msg = list_first(&ctx->mailbox);
                list_remove(msg);
                list_prepend(&ctx->save_queue, msg);

                i = POINTER_TO_II(mod->labels[label]);
#else
                NEXT_INSTRUCTION(next_offset);
#endif
                break;
            }

            //TODO: implement wait/1
            case OP_WAIT: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)

                TRACE("wait/1\n");

                #ifdef IMPL_EXECUTE_LOOP
                    ctx->saved_ip = mod->labels[label];
                    ctx->jump_to_on_restore = NULL;
                    ctx->saved_module = mod;
                    Context *scheduled_context = scheduler_wait(ctx->global, ctx);
                    ctx = scheduled_context;
                    x_regs = ctx->x;

                    mod = ctx->saved_module;
                    code = mod->code->code;
                    JUMP_TO_ADDRESS(scheduled_context->saved_ip);
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            //TODO: implement wait_timeout/2
            case OP_WAIT_TIMEOUT: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                int timeout;
                DECODE_COMPACT_TERM(timeout, code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("wait_timeout/2, label: %i, timeout: %li\n", label, (long int) term_to_int32(timeout));

                    NEXT_INSTRUCTION(next_off);
                    //TODO: it looks like x[0] might be used instead of jump_to_on_restore
                    ctx->saved_ip = INSTRUCTION_POINTER();
                    ctx->jump_to_on_restore = mod->labels[label];
                    ctx->saved_module = mod;

                    int needs_to_wait = 0;
                    if ((ctx->flags & (WaitingTimeout | WaitingTimeoutExpired)) == 0) {
                        scheduler_set_timeout(ctx, term_to_int32(timeout));
                        needs_to_wait = 1;
                    } else if ((ctx->flags & WaitingTimeout) == 0) {
                        needs_to_wait = 1;
                    } else if (!list_is_empty(&ctx->save_queue)) {
                        needs_to_wait = 1;
                    }

                    if (needs_to_wait) {
                        Context *scheduled_context = scheduler_wait(ctx->global, ctx);
                        ctx = scheduled_context;
                        x_regs = ctx->x;
                        mod = ctx->saved_module;
                        code = mod->code->code;
                        JUMP_TO_ADDRESS(scheduled_context->saved_ip);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("wait_timeout/2, label: %i\n", label);

                    UNUSED(timeout)

                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IS_LT: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off);
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_lt/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    if (term_compare(arg1, arg2, ctx) < 0) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off);
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_ge/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    if (term_compare(arg1, arg2, ctx) >= 0) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
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
                int label;
                term arg1;
                term arg2;
                int next_off = 1;
                DECODE_LABEL(label, code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_equal/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    //TODO: implement this
                    if (term_equals(arg1, arg2, ctx)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_not_equal/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    if (!term_equals(arg1, arg2, ctx)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
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
                int label;
                term arg1;
                term arg2;
                int next_off = 1;
                DECODE_LABEL(label, code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_eq_exact/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    //TODO: implement this
                    if (term_exactly_equals(arg1, arg2, ctx)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_not_eq_exact/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    //TODO: implement this
                    if (!term_exactly_equals(arg1, arg2, ctx)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_float/2, label=%i, arg1=%lx\n", label, arg1);

#ifndef AVM_NO_FP
                    if (term_is_float(arg1)) {
                        NEXT_INSTRUCTION(next_off);
                    } else {
                        i = POINTER_TO_II(mod->labels[label]);
                    }
#else
                    fprintf(stderr, "Warning: is_float/1 unsupported on this platform\n");
                    i = POINTER_TO_II(mod->labels[label]);
#endif
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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                term arg1;
                int next_off = 1;
                DECODE_LABEL(label, code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                term arg1;
                int next_off = 1;
                DECODE_LABEL(label, code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                term arg1;
                int next_off = 1;
                DECODE_LABEL(label, code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_port/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_pid(arg1)) {
                        int local_process_id = term_to_local_process_id(arg1);
                        Context *target = globalcontext_get_process(ctx->global, local_process_id);

                        if (context_is_port_driver(target)) {
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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off);
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off);
                int arity;
                DECODE_INTEGER(arity, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("test_arity/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_tuple(arg1) && term_get_tuple_arity(arg1) == arity) {
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
                DECODE_COMPACT_TERM(src_value, code, i, next_off, next_off)
                int default_label;
                DECODE_LABEL(default_label, code, i, next_off, next_off)
                next_off++; //skip extended list tag
                int size;
                DECODE_INTEGER(size, code, i, next_off, next_off)

                TRACE("select_val/3, default_label=%i, vals=%i\n", default_label, size);
                USED_BY_TRACE(default_label);
                USED_BY_TRACE(size);

                #ifdef IMPL_CODE_LOADER
                    UNUSED(src_value);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    void *jump_to_address = NULL;
                #endif

                for (int j = 0; j < size / 2; j++) {
                    term cmp_value;
                    DECODE_COMPACT_TERM(cmp_value, code, i, next_off, next_off)
                    int jmp_label;
                    DECODE_LABEL(jmp_label, code, i, next_off, next_off)

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
                DECODE_COMPACT_TERM(src_value, code, i, next_off, next_off)
                int default_label;
                DECODE_LABEL(default_label, code, i, next_off, next_off)
                next_off++; //skip extended list tag
                int size;
                DECODE_INTEGER(size, code, i, next_off, next_off)

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

                    for (int j = 0; j < size / 2; j++) {
                        int cmp_value;
                        DECODE_INTEGER(cmp_value, code, i, next_off, next_off)
                        int jmp_label;
                        DECODE_LABEL(jmp_label, code, i, next_off, next_off)

                        #ifdef IMPL_CODE_LOADER
                            UNUSED(cmp_value);
                        #endif

                        #ifdef IMPL_EXECUTE_LOOP
                            //TODO: check if src_value is a tuple
                            if (!jump_to_address && (arity == cmp_value)) {
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
                int label;
                int next_offset = 1;
                DECODE_LABEL(label, code, i, next_offset, next_offset)

                TRACE("jump/1 label=%i\n", label);
                USED_BY_TRACE(label);

                #ifdef IMPL_EXECUTE_LOOP
                    remaining_reductions--;
                    if (LIKELY(remaining_reductions)) {
                        JUMP_TO_ADDRESS(mod->labels[label]);
                    } else {
                        SCHEDULE_NEXT(mod, mod->labels[label]);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_offset);
                #endif

                break;
            }

            case OP_MOVE: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("move/2 %lx, %c%i\n", src_value, T_DEST_REG(dreg_type, dreg));

                    WRITE_REGISTER(dreg_type, dreg, src_value);
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
                DECODE_COMPACT_TERM(src_value, code, i, next_off, next_off)
                dreg_t head_dreg;
                dreg_type_t head_dreg_type;
                DECODE_DEST_REGISTER(head_dreg, head_dreg_type, code, i, next_off, next_off);
                dreg_t tail_dreg;
                dreg_type_t tail_dreg_type;
                DECODE_DEST_REGISTER(tail_dreg, tail_dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("get_list/3 %lx, %c%i, %c%i\n", src_value, T_DEST_REG(head_dreg_type, head_dreg), T_DEST_REG(tail_dreg_type, tail_dreg));

                    term head = term_get_list_head(src_value);
                    term tail = term_get_list_tail(src_value);

                    WRITE_REGISTER(head_dreg_type, head_dreg, head);
                    WRITE_REGISTER(tail_dreg_type, tail_dreg, tail);
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
                DECODE_COMPACT_TERM(src_value, code, i, next_off, next_off);
                int element;
                DECODE_INTEGER(element, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("get_tuple_element/2, element=%i, dest=%c%i\n", element, T_DEST_REG(dreg_type, dreg));
                USED_BY_TRACE(element);

                #ifdef IMPL_EXECUTE_LOOP
                    if (UNLIKELY(!term_is_tuple(src_value) || (element < 0) || (element >= term_get_tuple_arity(src_value)))) {
                        AVM_ABORT();
                    }

                    term t = term_get_tuple_element(src_value, element);
                    WRITE_REGISTER(dreg_type, dreg, t);
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
                DECODE_COMPACT_TERM(new_element, code, i, next_off, next_off);
                term tuple;
                DECODE_COMPACT_TERM(tuple, code, i, next_off, next_off);
                int position;
                DECODE_INTEGER(position, code, i, next_off, next_off);

                TRACE("set_tuple_element/2\n");

#ifdef IMPL_EXECUTE_LOOP
                if (UNLIKELY(!term_is_tuple(tuple) || (position < 0) || (position >= term_get_tuple_arity(tuple)))) {
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
                DECODE_COMPACT_TERM(head, code, i, next_off, next_off);
                term tail;
                DECODE_COMPACT_TERM(tail, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

#ifdef IMPL_EXECUTE_LOOP
                term *list_elem = term_list_alloc(ctx);
#endif

                TRACE("op_put_list/3\n");

                #ifdef IMPL_CODE_LOADER
                    UNUSED(head);
                    UNUSED(tail);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    term t = term_list_init_prepend(list_elem, head, tail);
                    WRITE_REGISTER(dreg_type, dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_PUT_TUPLE: {
                int next_off = 1;
                int size;
                DECODE_INTEGER(size, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("put_tuple/2 size=%i, dest=%c%i\n", size, T_DEST_REG(dreg_type, dreg));
                USED_BY_TRACE(dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    term t = term_alloc_tuple(size, ctx);
                    WRITE_REGISTER(dreg_type, dreg, t);
                #endif

                for (int j = 0; j < size; j++) {
                    if (code[i + next_off] != OP_PUT) {
                        fprintf(stderr, "Expected put, got opcode: %i\n", code[i + next_off]);
                        AVM_ABORT();
                    }
                    next_off++;
                    term put_value;
                    DECODE_COMPACT_TERM(put_value, code, i, next_off, next_off);
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
                    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                int next_off = 1;
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("badmatch/1\n");
                    USED_BY_TRACE(arg1);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("badmatch/1, v=0x%lx\n", arg1);

                    term new_error_tuple = term_alloc_tuple(2, ctx);
                    //TODO: check alloc
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
                    ctx->x[0] = ERROR_ATOM;
                    ctx->x[1] = IF_CLAUSE_ATOM;

                    RAISE_ERROR(IF_CLAUSE_ATOM);
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(1);
                #endif

                break;
            }

            case OP_CASE_END: {
                #ifdef IMPL_EXECUTE_LOOP
                    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                int next_off = 1;
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("case_end/1\n");
                    USED_BY_TRACE(arg1);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("case_end/1, v=0x%lx\n", arg1);

                    term new_error_tuple = term_alloc_tuple(2, ctx);
                    //TODO: reserve memory before
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
                unsigned int args_count;
                DECODE_INTEGER(args_count, code, i, next_off, next_off)

                TRACE("call_fun/1, args_count=%i\n", args_count);
                USED_BY_TRACE(args_count);

                #ifdef IMPL_EXECUTE_LOOP
                    remaining_reductions--;
                    if (UNLIKELY(!remaining_reductions)) {
                        SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                        continue;
                    }

                    term fun = ctx->x[args_count];

                    if (UNLIKELY(!term_is_function(fun))) {
                        term new_error_tuple = term_alloc_tuple(2, ctx);
                        //TODO: ensure memory before
                        term_put_tuple_element(new_error_tuple, 0, BADFUN_ATOM);
                        term_put_tuple_element(new_error_tuple, 1, ctx->x[args_count]);

                        RAISE_ERROR(new_error_tuple);
                    }

                    Module *fun_module;
                    unsigned int fun_arity;
                    uint32_t n_freeze;
                    uint32_t label;

                    const term *boxed_value = term_to_const_term_ptr(fun);
                    term index_or_function = boxed_value[2];
                    if (term_is_atom(index_or_function)) {
                        term module = boxed_value[1];
                        fun_arity = term_to_int(boxed_value[3]);

                        AtomString module_name = globalcontext_atomstring_from_term(mod->global, module);
                        AtomString function_name = globalcontext_atomstring_from_term(mod->global, index_or_function);

                        struct Nif *nif = (struct Nif *) nifs_get(module_name, function_name, fun_arity);
                        if (!IS_NULL_PTR(nif)) {
                            term return_value = nif->nif_ptr(ctx, arity, ctx->x);
                            if (UNLIKELY(term_is_invalid_term(return_value))) {
                                HANDLE_ERROR();
                            }
                            ctx->x[0] = return_value;
                            NEXT_INSTRUCTION(next_off);
                            continue;

                        } else {
                            fun_module = globalcontext_get_module(ctx->global, module_name);
                            if (IS_NULL_PTR(fun_module)) {
                                HANDLE_ERROR();
                            }
                            label = module_search_exported_function(fun_module, function_name, arity);
                            if (UNLIKELY(label == 0)) {
                                HANDLE_ERROR();
                            }
                        }

                    } else {
                        fun_module = (Module *) boxed_value[1];
                        uint32_t fun_index = term_to_int(index_or_function);

                        uint32_t fun_arity_and_freeze;
                        module_get_fun(fun_module, fun_index, &label, &fun_arity_and_freeze, &n_freeze);

                        fun_arity = fun_arity_and_freeze - n_freeze;

                        TRACE_CALL(ctx, mod, "call_fun", label, args_count);
                    }

                    if (UNLIKELY(args_count != fun_arity)) {
                        RAISE_ERROR(BADARITY_ATOM);
                    }

                    for (uint32_t i = 0; i < n_freeze; i++) {
                        ctx->x[i + fun_arity] = boxed_value[i + 3];
                    }

                    NEXT_INSTRUCTION(next_off);
                    ctx->cp = module_address(mod->module_index, i);

                    mod = fun_module;
                    code = mod->code->code;
                    JUMP_TO_ADDRESS(mod->labels[label]);
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

           case OP_IS_FUNCTION: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int arity;
                DECODE_INTEGER(arity, code, i, next_off, next_off);
                int index;
                DECODE_INTEGER(index, code, i, next_off, next_off);

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
                        continue;
                    }

                    TRACE_CALL_EXT(ctx, mod, "call_ext_only", index, arity);

                    const struct ExportedFunction *func = mod->imported_funcs[index].func;

                    if (func->type == UnresolvedFunctionCall) {
                        const struct ExportedFunction *resolved_func = module_resolve_function(mod, index);
                        if (IS_NULL_PTR(resolved_func)) {
                            RAISE_ERROR(UNDEF_ATOM);
                        }
                        func = resolved_func;
                    }

                    switch (func->type) {
                        case NIFFunctionType: {
                            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
                            term return_value = nif->nif_ptr(ctx, arity, ctx->x);
                            if (UNLIKELY(term_is_invalid_term(return_value))) {
                                HANDLE_ERROR();
                            }
                            ctx->x[0] = return_value;
                            if ((long) ctx->cp == -1) {
                                return 0;
                            }

                            DO_RETURN();

                            break;
                        }
                        case ModuleFunction: {
                            const struct ModuleFunction *jump = EXPORTED_FUNCTION_TO_MODULE_FUNCTION(func);

                            mod = jump->target;
                            code = mod->code->code;

                            JUMP_TO_ADDRESS(mod->labels[jump->label]);

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
                int fun_index;
                DECODE_LABEL(fun_index, code, i, next_off, next_off)

                TRACE("make_fun/2, fun_index=%i\n", fun_index);
                #ifdef IMPL_EXECUTE_LOOP
                    term f = make_fun(ctx, mod, fun_index);
                    if (term_is_invalid_term(f)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    } else {
                        ctx->x[0] = f;
                    }
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TRY: {
                int next_off = 1;
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)

                TRACE("try/2, label=%i, reg=%c%i\n", label, T_DEST_REG(dreg_type, dreg));

                #ifdef IMPL_EXECUTE_LOOP
                    term catch_term = term_from_catch_label(mod->module_index, label);
                    //TODO: here just write to y registers is enough
                    WRITE_REGISTER(dreg_type, dreg, catch_term);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TRY_END: {
                int next_off = 1;
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("try_end/1, reg=%c%i\n", T_DEST_REG(dreg_type, dreg));

                #ifdef IMPL_EXECUTE_LOOP
                    //TODO: here just write to y registers is enough
                    WRITE_REGISTER(dreg_type, dreg, term_nil());
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TRY_CASE: {
                int next_off = 1;
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("try_case/1, reg=%c%i\n", T_DEST_REG(dreg_type, dreg));

                #ifdef IMPL_EXECUTE_LOOP
                    // clears the catch value on stack
                    WRITE_REGISTER(dreg_type, dreg, term_nil());
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TRY_CASE_END: {
                #ifdef IMPL_EXECUTE_LOOP
                    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                int next_off = 1;
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("try_case_end/1\n");
                    UNUSED(arg1);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("try_case_end/1, val=%lx\n", arg1);

                    term new_error_tuple = term_alloc_tuple(2, ctx);
                    //TODO: ensure memory before
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
                DECODE_COMPACT_TERM(stacktrace, code, i, next_off, next_off);
                UNUSED(stacktrace);
                term exc_value;
                DECODE_COMPACT_TERM(exc_value, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("raise/2\n");
                    UNUSED(exc_value);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("raise/2 stacktrace=0x%lx exc_value=0x%lx\n", stacktrace, exc_value);

                    RAISE_ERROR(exc_value);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_CATCH: {
                int next_off = 1;
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)

                TRACE("catch/2, label=%i, reg=%c%i\n", label, T_DEST_REG(dreg_type, dreg));

                #ifdef IMPL_EXECUTE_LOOP
                    term catch_term = term_from_catch_label(mod->module_index, label);
                    // TODO: here just write to y registers is enough
                    WRITE_REGISTER(dreg_type, dreg, catch_term);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_CATCH_END: {
                int next_off = 1;
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("catch_end/1, reg=%c%i\n", T_DEST_REG(dreg_type, dreg));

                #ifdef IMPL_EXECUTE_LOOP
                    // TODO: here just write to y registers is enough
                    WRITE_REGISTER(dreg_type, dreg, term_nil());
                    // C.f. https://www.erlang.org/doc/reference_manual/expressions.html#catch-and-throw
                    switch (term_to_atom_index(ctx->x[0])) {
                        case THROW_ATOM_INDEX:
                            ctx->x[0] = ctx->x[1];
                            break;

                        case ERROR_ATOM_INDEX: {
                            if (UNLIKELY(memory_ensure_free(ctx, 6) != MEMORY_GC_OK)) {
                                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                            }
                            term reason_tuple = term_alloc_tuple(2, ctx);
                            term_put_tuple_element(reason_tuple, 0, ctx->x[1]);
                            // TODO add stacktrace
                            term_put_tuple_element(reason_tuple, 1, UNDEFINED_ATOM);
                            term exit_tuple = term_alloc_tuple(2, ctx);
                            term_put_tuple_element(exit_tuple, 0, EXIT_ATOM);
                            term_put_tuple_element(exit_tuple, 1, reason_tuple);
                            ctx->x[0] = exit_tuple;

                            break;
                        }
                        case LOWERCASE_EXIT_ATOM_INDEX: {
                            if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
                                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                            }
                            term exit_tuple = term_alloc_tuple(2, ctx);
                            term_put_tuple_element(exit_tuple, 0, EXIT_ATOM);
                            term_put_tuple_element(exit_tuple, 1, ctx->x[1]);
                            ctx->x[0] = exit_tuple;

                            break;
                        }
                    }
#endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_ADD: {
                int next_off = 1;
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term src1;
                DECODE_COMPACT_TERM(src1, code, i, next_off, next_off);
                term src2;
                DECODE_COMPACT_TERM(src2, code, i, next_off, next_off);
                avm_int_t unit;
                DECODE_INTEGER(unit, code, i, next_off, next_off)
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_add/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_INTEGER(src1, "bs_add");
                    VERIFY_IS_INTEGER(src2, "bs_add");
                    avm_int_t src1_val = term_to_int(src1);
                    avm_int_t src2_val = term_to_int(src2);

                    TRACE("bs_add/5, fail=%i src1=%li src2=%li unit=%li dreg=%c%i\n", fail, src1_val, src2_val, unit, T_DEST_REG(dreg_type, dreg));

                    WRITE_REGISTER(dreg_type, dreg, term_from_int((src1_val + src2_val) * unit));
                #endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_INIT2: {
                int next_off = 1;
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off, next_off)
                avm_int_t words;
                UNUSED(words);
                DECODE_INTEGER(words, code, i, next_off, next_off)
                avm_int_t regs;
                UNUSED(regs);
                DECODE_INTEGER(regs, code, i, next_off, next_off)
                term flags;
                UNUSED(flags);
                DECODE_COMPACT_TERM(flags, code, i, next_off, next_off)
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_init2/6\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_INTEGER(size, "bs_init2");
                    avm_int_t size_val = term_to_int(size);

                    TRACE("bs_init2/6, fail=%i size=%li words=%li regs=%li dreg=%c%i\n", fail, size_val, words, regs, T_DEST_REG(dreg_type, dreg));

                    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(size_val) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    term t = term_create_empty_binary(size_val, ctx);

                    ctx->bs = t;
                    ctx->bs_offset = 0;

                    WRITE_REGISTER(dreg_type, dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_INIT_BITS: {
                int next_off = 1;
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off, next_off)
                int words;
                DECODE_INTEGER(words, code, i, next_off, next_off)
                int regs;
                DECODE_INTEGER(regs, code, i, next_off, next_off)
                term flags;
                DECODE_COMPACT_TERM(flags, code, i, next_off, next_off)
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_init_bits/6\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_INTEGER(size, "bs_init_bits");
                    VERIFY_IS_INTEGER(flags, "bs_init_bits");
                    avm_int_t size_val = term_to_int(size);
                    if (size_val % 8 != 0) {
                        TRACE("bs_init_bits: size_val (%li) is not evenly divisible by 8\n", size_val);
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    avm_int_t flags_value = term_to_int(flags);
                    if (flags_value != 0) {
                        TRACE("bs_init_bits: neither signed nor native or little endian encoding supported.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    TRACE("bs_init_bits/6, fail=%i size=%li words=%i regs=%i dreg=%c%i\n", fail, size_val, words, regs, T_DEST_REG(dreg_type, dreg));

                    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(size_val / 8) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    term t = term_create_empty_binary(size_val / 8, ctx);

                    ctx->bs = t;
                    ctx->bs_offset = 0;

                    WRITE_REGISTER(dreg_type, dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_APPEND: {
                int next_off = 1;
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off, next_off)
                term extra;
                UNUSED(extra);
                DECODE_COMPACT_TERM(extra, code, i, next_off, next_off)
                term live;
                UNUSED(live);
                DECODE_COMPACT_TERM(live, code, i, next_off, next_off)
                avm_int_t unit;
                DECODE_INTEGER(unit, code, i, next_off, next_off);
                term src;
                int src_off = next_off;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off)
                term flags;
                UNUSED(flags);
                DECODE_COMPACT_TERM(flags, code, i, next_off, next_off)
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_append/6\n");
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
                        TRACE("bs_append: unit is not equal to 8; unit=%li\n", unit);
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    TRACE("bs_append/7, fail=%i size=%li unit=%li src=0x%lx dreg=%c%i\n", fail, size_val, unit, src, T_DEST_REG(dreg_type, dreg));

                    size_t src_size = term_binary_size(src);
                    // TODO: further investigate extra_val
                    if (UNLIKELY(memory_ensure_free(ctx, src_size + term_binary_data_size_in_terms(size_val / 8) + extra_val + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    DECODE_COMPACT_TERM(src, code, i, src_off, src_off)
                    term t = term_create_empty_binary(src_size + size_val / 8, ctx);
                    memcpy((void *) term_binary_data(t), (void *) term_binary_data(src), src_size);

                    ctx->bs = t;
                    ctx->bs_offset = src_size * 8;

                    WRITE_REGISTER(dreg_type, dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_PUT_INTEGER: {
                int next_off = 1;
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off, next_off)
                avm_int_t unit;
                DECODE_INTEGER(unit, code, i, next_off, next_off);
                term flags;
                DECODE_COMPACT_TERM(flags, code, i, next_off, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_put_integer/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_ANY_INTEGER(src, "bs_put_integer");
                    VERIFY_IS_INTEGER(size, "bs_put_integer");
                    VERIFY_IS_INTEGER(flags, "bs_put_integer");

                    avm_int64_t src_value = term_maybe_unbox_int64(src);
                    avm_int_t size_value = term_to_int(size);
                    avm_int_t flags_value = term_to_int(flags);
                    if (unit != 1) {
                        TRACE("bs_put_integer: unit is not 1\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    TRACE("bs_put_integer/5, fail=%i size=%li unit=%li flags=0x%lx src=%i\n", fail, size_value, unit, flags_value, (unsigned int) src_value);

                    bool result = bitstring_insert_integer(ctx->bs, ctx->bs_offset, src_value, size_value, flags_value);
                    if (UNLIKELY(!result)) {
                        TRACE("bs_put_integer: Failed to insert integer into binary: %i\n", result);
                        RAISE_ERROR(BADARG_ATOM);
                    }

                    ctx->bs_offset += size_value * unit;
                #endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_PUT_BINARY: {
                int next_off = 1;
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                int size;
                DECODE_COMPACT_TERM(size, code, i, next_off, next_off)
                avm_int_t unit;
                DECODE_INTEGER(unit, code, i, next_off, next_off);
                term flags;
                DECODE_COMPACT_TERM(flags, code, i, next_off, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_put_binary/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_BINARY(src, "bs_put_binary");
                    VERIFY_IS_INTEGER(flags, "bs_put_binary");
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
                        TRACE("bs_put_binary: Unsupported size term type in put binary: 0x%x\n", size);
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    if (size_val > term_binary_size(src)) {
                        TRACE("bs_put_binary: binary data size (%li) larger than source binary size (%li)\n", size_val, term_binary_size(src));
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    avm_int_t flags_value = term_to_int(flags);
                    if (flags_value != 0) {
                        TRACE("bs_put_binary: neither signed nor native or little endian encoding supported.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    if (ctx->bs_offset % 8 != 0) {
                        TRACE("bs_put_binary: Unsupported bit syntax operation.  Writing binaries must be byte-aligend.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    TRACE("bs_put_binary/5, fail=%i size=%li unit=%li flags=0x%lx src=0x%x\n", fail, size_val, unit, flags, (unsigned int) src);

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
                avm_int_t size;
                DECODE_INTEGER(size, code, i, next_off, next_off);
                avm_int_t offset;
                DECODE_INTEGER(offset, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_put_string/2\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    if (UNLIKELY(!term_is_binary(ctx->bs))) {
                        TRACE("bs_put_binary: Bad state.  ctx->bs is not a binary.\n");
                        RAISE_ERROR(BADARG_ATOM);
                    }
                    if (ctx->bs_offset % 8 != 0) {
                        TRACE("bs_put_string: Unsupported bit syntax operation.  Writing strings must be byte-aligend.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    size_t remaining = 0;
                    const uint8_t *str = module_get_str(mod, offset, &remaining);
                    if (UNLIKELY(IS_NULL_PTR(str))) {
                        TRACE("bs_put_binary: Bad offset in strings table.\n");
                        RAISE_ERROR(BADARG_ATOM);
                    }

                    memcpy((char *) term_binary_data(ctx->bs) + ctx->bs_offset / 8, str, size);
                    ctx->bs_offset += 8 * size;
                #endif
                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_START_MATCH2: {
                int next_off = 1;
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                #ifdef IMPL_EXECUTE_LOOP
                    int next_off_back = next_off;
                #endif
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off);
                term slots_term;
                DECODE_COMPACT_TERM(slots_term, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    int slots = term_to_int(slots_term);

                    if (memory_ensure_free(ctx, TERM_BOXED_BIN_MATCH_STATE_SIZE + slots) != MEMORY_GC_OK) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }

                    DECODE_COMPACT_TERM(src, code, i, next_off_back, next_off_back);
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_start_match2/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_start_match2/5, fail=%i src=0x%lx arg2=0x%lx arg3=0x%lx dreg=%c%i\n", fail, src, arg2, slots_term, T_DEST_REG(dreg_type, dreg));
                    if (!(term_is_binary(src) || term_is_match_state(src))) {
                        WRITE_REGISTER(dreg_type, dreg, src);
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term match_state = term_alloc_bin_match_state(src, slots, ctx);

                        WRITE_REGISTER(dreg_type, dreg, match_state);
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif
                break;
            }

            case OP_BS_START_MATCH3: {
                #ifdef IMPL_EXECUTE_LOOP
                    if (memory_ensure_free(ctx, TERM_BOXED_BIN_MATCH_STATE_SIZE) != MEMORY_GC_OK) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                int next_off = 1;
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                term live;
                DECODE_COMPACT_TERM(live, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_start_match3/4\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_start_match3/4, fail=%i src=0x%lx live=0x%lx dreg=%c%i\n", fail, src, live, T_DEST_REG(dreg_type, dreg));
                    if (!(term_is_binary(src) || term_is_match_state(src))) {
                        WRITE_REGISTER(dreg_type, dreg, src);
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term match_state = term_alloc_bin_match_state(src, 0, ctx);

                        WRITE_REGISTER(dreg_type, dreg, match_state);
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
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);
                term live;
                DECODE_COMPACT_TERM(live, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_get_position/3\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_get_position");

                    TRACE("bs_get_position/3 src=0x%lx dreg=%c%i live=0x%lx \n", src, T_DEST_REG(dreg_type, dreg), live);

                    avm_int_t offset = term_get_match_state_offset(src);
                    term offset_term = term_from_int(offset);

                    WRITE_REGISTER(dreg_type, dreg, offset_term);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_GET_TAIL: {
                int next_off = 1;
                term src;
                int src_off = next_off;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);
                term live;
                DECODE_COMPACT_TERM(live, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_get_tail/3\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_get_tail");

                    avm_int_t bs_offset = term_get_match_state_offset(src);
                    term bs_bin = term_get_match_state_binary(src);

                    TRACE("bs_get_tail/3 src=0x%lx dreg=%c%i live=0x%lx \n", src, T_DEST_REG(dreg_type, dreg), live);
                    if (bs_offset == 0) {

                        WRITE_REGISTER(dreg_type, dreg, bs_bin);

                    } else {
                        if (bs_offset % 8 != 0) {
                            TRACE("bs_get_tail: Unsupported alignment.\n");
                            RAISE_ERROR(UNSUPPORTED_ATOM);
                        } else {
                            size_t start_pos = bs_offset / 8;
                            size_t src_size = term_binary_size(bs_bin);
                            size_t new_bin_size = src_size - start_pos;
                            size_t heap_size = term_sub_binary_heap_size(bs_bin, src_size - start_pos);


                            if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
                                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                            }
                            DECODE_COMPACT_TERM(src, code, i, src_off, src_off);
                            bs_bin = term_get_match_state_binary(src);
                            term t = term_maybe_create_sub_binary(bs_bin, start_pos, new_bin_size, ctx);
                            WRITE_REGISTER(dreg_type, dreg, t);

                        }
                    }
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_SET_POSITION: {
                int next_off = 1;
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                term pos;
                DECODE_COMPACT_TERM(pos, code, i, next_off, next_off);

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
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                avm_int_t bits;
                DECODE_INTEGER(bits, code, i, next_off, next_off);
                avm_int_t offset;
                DECODE_INTEGER(offset, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_match_string/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_match_string");

                    if (bits % 8 != 0) {
                        TRACE("bs_match_string: Unsupported bits size (must be evenly divisible by 8). bits=%li\n", bits);
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

                    TRACE("bs_match_string/4, fail=%i src=0x%lx bits=%li offset=%li src=0x%lx\n", fail, src, bits, offset, src);

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
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                term index;
                DECODE_COMPACT_TERM(index, code, i, next_off, next_off);

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

                    TRACE("bs_save2/2, src=0x%lx pos=%li\n", src, index_val);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_RESTORE2: {
                int next_off = 1;
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                term index;
                DECODE_COMPACT_TERM(index, code, i, next_off, next_off);

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

                    TRACE("bs_restore2/2, src=0x%lx pos=%li\n", src, index_val);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_SKIP_BITS2: {
                int next_off = 1;
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off, next_off);
                avm_int_t unit;
                DECODE_INTEGER(unit, code, i, next_off, next_off);
                term flags;
                DECODE_COMPACT_TERM(flags, code, i, next_off, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_skip_bits2/5\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_skip_bits2");
                    VERIFY_IS_INTEGER(size, "bs_skip_bits2");
                    VERIFY_IS_INTEGER(flags, "bs_skip_bits2");
                    avm_int_t flags_value = term_to_int(flags);
                    if (flags_value != 0) {
                        TRACE("bs_skip_bits2: neither signed nor native or little endian encoding supported.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }
                    avm_int_t size_val = term_to_int(size);

                    TRACE("bs_skip_bits2/5, fail=%i src=0x%lx size=0x%lx unit=0x%lx flags=0x%lx\n", fail, src, size, unit, flags);

                    size_t increment = size_val * unit;
                    avm_int_t bs_offset = term_get_match_state_offset(src);
                    term bs_bin = term_get_match_state_binary(src);
                    if ((bs_offset + increment) > term_binary_size(bs_bin) * 8) {
                        TRACE("bs_skip_bits2: Insufficient capacity to skip bits: %i, inc: %i\n", bs_offset, increment);
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
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                avm_int_t unit;
                DECODE_INTEGER(unit, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_test_unit/3\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_test_unit");

                    TRACE("bs_test_unit/3, fail=%i src=0x%lx unit=0x%lx\n", fail, src, unit);

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
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                avm_int_t bits;
                DECODE_INTEGER(bits, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_test_tail2/3\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_test_tail2");

                    TRACE("bs_test_tail2/3, fail=%i src=0x%lx bits=0x%lx\n", fail, src, bits);

                    term bs_bin = term_get_match_state_binary(src);
                    avm_int_t bs_offset = term_get_match_state_offset(src);

                    if ((term_binary_size(bs_bin) * 8 - bs_offset) != (unsigned int) bits) {
                        TRACE("bs_test_tail2: Expected exactly %li bits remaining, but remaining=%li\n", bits, term_binary_size(bs_bin) * 8 - bs_offset);
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
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off);
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off, next_off);
                avm_int_t unit;
                DECODE_INTEGER(unit, code, i, next_off, next_off);
                term flags;
                DECODE_COMPACT_TERM(flags, code, i, next_off, next_off)
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_get_integer2/7\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_get_integer");
                    VERIFY_IS_INTEGER(size,     "bs_get_integer");
                    VERIFY_IS_INTEGER(flags,    "bs_get_integer");

                    avm_int_t size_val = term_to_int(size);
                    avm_int_t flags_value = term_to_int(flags);

                    TRACE("bs_get_integer2/7, fail=%i src=0x%lx size=%li unit=%li flags=%li\n", fail, src, size_val, unit, flags);

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

                        term t = term_make_maybe_boxed_int64(ctx, value.s);
                        if (UNLIKELY(term_is_invalid_term(t))) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }

                        WRITE_REGISTER(dreg_type, dreg, t);
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
                int fail;
                DECODE_LABEL(fail, code, i, next_off, next_off)
                term src;
                int src_offset = next_off;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off);
                term size;
                DECODE_COMPACT_TERM(size, code, i, next_off, next_off);
                avm_int_t unit;
                DECODE_INTEGER(unit, code, i, next_off, next_off);
                term flags;
                DECODE_COMPACT_TERM(flags, code, i, next_off, next_off)
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_get_binary2/7\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    VERIFY_IS_MATCH_STATE(src, "bs_get_binary2");
                    VERIFY_IS_INTEGER(flags,    "bs_get_binary2");

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
                    avm_int_t flags_value = term_to_int(flags);
                    if (flags_value != 0) {
                        TRACE("bs_get_binary2: neither signed nor native or little endian encoding supported.\n");
                        RAISE_ERROR(UNSUPPORTED_ATOM);
                    }

                    TRACE("bs_get_binary2/7, fail=%i src=0x%lx unit=%li\n", fail, bs_bin, unit);

                if ((unsigned int) (bs_offset / unit + size_val) > term_binary_size(bs_bin)) {
                    TRACE("bs_get_binary2: insufficient capacity\n");
                    JUMP_TO_ADDRESS(mod->labels[fail]);
                } else {
                    term_set_match_state_offset(src, bs_offset + size_val * unit);

                    size_t heap_size = term_sub_binary_heap_size(bs_bin, size_val);
                    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    // re-compute src
                    DECODE_COMPACT_TERM(src, code, i, src_offset, src_offset);
                    bs_bin = term_get_match_state_binary(src);

                    term t = term_maybe_create_sub_binary(bs_bin, bs_offset / unit, size_val, ctx);

                    WRITE_REGISTER(dreg_type, dreg, t);
                    NEXT_INSTRUCTION(next_off);
                }

                #endif
                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif
                break;
            }

            case OP_BS_CONTEXT_TO_BINARY: {
                int next_off = 1;
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                // Do not check if dreg is a binary or not
                // In case it is not a binary or a match state, dreg will not be changed.

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_context_to_binary/1\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_context_to_binary/1, dreg=%c%i\n", T_DEST_REG(dreg_type, dreg));

                    term src = READ_DEST_REGISTER(dreg_type, dreg);
                    term bin;
                    if (term_is_match_state(src)) {
                        avm_int_t offset = term_get_match_state_offset(src);
                        if (offset == 0) {
                            bin = term_get_match_state_binary(src);
                        } else {
                            term src_bin = term_get_match_state_binary(src);
                            int len = term_binary_size(src_bin) - offset / 8;
                            size_t heap_size = term_sub_binary_heap_size(src_bin, len);
                            if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
                                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                            }
                            // src might be invalid after a GC
                            src = READ_DEST_REGISTER(dreg_type, dreg);
                            src_bin = term_get_match_state_binary(src);
                            bin = term_maybe_create_sub_binary(src_bin, offset / 8, len, ctx);
                        }
                    } else {
                        bin = src;
                    }
                    WRITE_REGISTER(dreg_type, dreg, bin);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_APPLY: {
                int next_off = 1;
                int arity;
                DECODE_INTEGER(arity, code, i, next_off, next_off)
#ifdef IMPL_EXECUTE_LOOP
                term module = ctx->x[arity];
                term function = ctx->x[arity + 1];
                TRACE("apply/1, module=%lu, function=%lu arity=%i\n", module, function, arity);

                remaining_reductions--;
                if (UNLIKELY(!remaining_reductions)) {
                    SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                    continue;
                }
                NEXT_INSTRUCTION(next_off);

                if (UNLIKELY(!term_is_atom(module) || !term_is_atom(function))) {
                    RAISE_ERROR(BADARG_ATOM);
                }

                AtomString module_name = globalcontext_atomstring_from_term(mod->global, module);
                AtomString function_name = globalcontext_atomstring_from_term(mod->global, function);

                TRACE_APPLY(ctx, "apply", module_name, function_name, arity);

                term native_return;
                if (maybe_call_native(ctx, module_name, function_name, arity, &native_return)) {
                    if (UNLIKELY(term_is_invalid_term(native_return))) {
                        HANDLE_ERROR();
                    }
                    ctx->x[0] = native_return;

                } else {
                    Module *target_module = globalcontext_get_module(ctx->global, module_name);
                    if (IS_NULL_PTR(target_module)) {
                        HANDLE_ERROR();
                    }
                    int target_label = module_search_exported_function(target_module, function_name, arity);
                    if (target_label == 0) {
                        HANDLE_ERROR();
                    }
                    ctx->cp = module_address(mod->module_index, i);
                    mod = target_module;
                    code = mod->code->code;
                    JUMP_TO_ADDRESS(mod->labels[target_label]);
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
                int arity;
                DECODE_INTEGER(arity, code, i, next_off, next_off)
                int n_words;
                DECODE_INTEGER(n_words, code, i, next_off, next_off);
#ifdef IMPL_EXECUTE_LOOP
                term module = ctx->x[arity];
                term function = ctx->x[arity + 1];
                TRACE("apply_last/1, module=%lu, function=%lu arity=%i deallocate=%i\n", module, function, arity, n_words);

                remaining_reductions--;
                if (UNLIKELY(!remaining_reductions)) {
                    SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                    continue;
                }

                ctx->cp = ctx->e[n_words];
                ctx->e += (n_words + 1);

                if (UNLIKELY(!term_is_atom(module) || !term_is_atom(function))) {
                    RAISE_ERROR(BADARG_ATOM);
                }

                AtomString module_name = globalcontext_atomstring_from_term(mod->global, module);
                AtomString function_name = globalcontext_atomstring_from_term(mod->global, function);

                TRACE_APPLY(ctx, "apply_last", module_name, function_name, arity);

                term native_return;
                if (maybe_call_native(ctx, module_name, function_name, arity, &native_return)) {
                    if (UNLIKELY(term_is_invalid_term(native_return))) {
                        HANDLE_ERROR();
                    }
                    ctx->x[0] = native_return;
                    DO_RETURN();

                } else {
                    Module *target_module = globalcontext_get_module(ctx->global, module_name);
                    if (IS_NULL_PTR(target_module)) {
                        HANDLE_ERROR();
                    }
                    int target_label = module_search_exported_function(target_module, function_name, arity);
                    if (target_label == 0) {
                        HANDLE_ERROR();
                    }
                    mod = target_module;
                    code = mod->code->code;
                    JUMP_TO_ADDRESS(mod->labels[target_label]);
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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)
                unsigned int arity;
                DECODE_INTEGER(arity, code, i, next_off, next_off)

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
                int f_label;
                DECODE_LABEL(f_label, code, i, next_off, next_off);
                int live;
                DECODE_INTEGER(live, code, i, next_off, next_off);
                int bif;
                DECODE_INTEGER(bif, code, i, next_off, next_off); //s?
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("gc_bif1/5 fail_lbl=%i, live=%i, bif=%i, arg1=0x%lx, dest=%c%i\n", f_label, live, bif, arg1, T_DEST_REG(dreg_type, dreg));

                    GCBifImpl1 func = (GCBifImpl1) mod->imported_funcs[bif].bif;
                    term ret = func(ctx, live, arg1);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        HANDLE_ERROR();
                    }

                    WRITE_REGISTER(dreg_type, dreg, ret);
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
                int f_label;
                DECODE_LABEL(f_label, code, i, next_off, next_off);
                int live;
                DECODE_INTEGER(live, code, i, next_off, next_off);
                int bif;
                DECODE_INTEGER(bif, code, i, next_off, next_off); //s?
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("gc_bif2/6 fail_lbl=%i, live=%i, bif=%i, arg1=0x%lx, arg2=0x%lx, dest=%c%i\n", f_label, live, bif, arg1, arg2, T_DEST_REG(dreg_type, dreg));

                    GCBifImpl2 func = (GCBifImpl2) mod->imported_funcs[bif].bif;
                    term ret = func(ctx, live, arg1, arg2);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        HANDLE_ERROR();
                    }

                    WRITE_REGISTER(dreg_type, dreg, ret);
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
                int label;
                term arg1;
                int next_off = 1;
                DECODE_LABEL(label, code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int f_label;
                DECODE_LABEL(f_label, code, i, next_off, next_off);
                int live;
                DECODE_INTEGER(live, code, i, next_off, next_off);
                int bif;
                DECODE_INTEGER(bif, code, i, next_off, next_off); //s?
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off);
                term arg2;
                DECODE_COMPACT_TERM(arg2, code, i, next_off, next_off);
                term arg3;
                DECODE_COMPACT_TERM(arg3, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("gc_bif3/7 fail_lbl=%i, live=%i, bif=%i, arg1=0x%lx, arg2=0x%lx, arg3=0x%lx, dest=%c%i\n", f_label, live, bif, arg1, arg2, arg3, T_DEST_REG(dreg_type, dreg));

                    GCBifImpl3 func = (GCBifImpl3) mod->imported_funcs[bif].bif;
                    term ret = func(ctx, live, arg1, arg2, arg3);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        HANDLE_ERROR();
                    }

                    WRITE_REGISTER(dreg_type, dreg, ret);
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
                int next_offset = 1;
                int n_words;
                DECODE_INTEGER(n_words, code, i, next_offset, next_offset);
                int n_remaining;
                DECODE_INTEGER(n_remaining, code, i, next_offset, next_offset);

                TRACE("trim/2 words=%i, remaining=%i\n", n_words, n_remaining);
                USED_BY_TRACE(n_words);
                USED_BY_TRACE(n_remaining);

                #ifdef IMPL_EXECUTE_LOOP
                    DEBUG_DUMP_STACK(ctx);
                    ctx->e += n_words;
                    DEBUG_DUMP_STACK(ctx);
                #endif

                UNUSED(n_remaining)

                NEXT_INSTRUCTION(next_offset);
                break;
            }

            //TODO: stub, implement recv_mark/1
            //it looks like it can be safely left unimplemented
            case OP_RECV_MARK: {
                int next_offset = 1;
                int label;
                DECODE_LABEL(label, code, i, next_offset, next_offset);

                TRACE("recv_mark/1 label=%i\n", label);
                USED_BY_TRACE(label);

                NEXT_INSTRUCTION(next_offset);
                break;
            }

            //TODO: stub, implement recv_set/1
            //it looks like it can be safely left unimplemented
            case OP_RECV_SET: {
                int next_offset = 1;
                int label;
                DECODE_LABEL(label, code, i, next_offset, next_offset);

                TRACE("recv_set/1 label=%i\n", label);
                USED_BY_TRACE(label);

                NEXT_INSTRUCTION(next_offset);
                break;
            }

            case OP_LINE: {
                int next_offset = 1;
                int line_number;
                DECODE_INTEGER(line_number, code, i, next_offset, next_offset);

                TRACE("line/1: %i\n", line_number);

                NEXT_INSTRUCTION(next_offset);
                break;
            }

            case OP_PUT_MAP_ASSOC: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term src;
                int src_offset = next_off;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);
                int live;
                DECODE_INTEGER(live, code, i, next_off, next_off);

                TRACE("put_map_assoc/5: label: %i src: 0x%lx dest=%c%i live: %i\n", label, src, T_DEST_REG(dreg_type, dreg), live);

                next_off++; //skip extended list tag {z, 1}
                int list_len;
                DECODE_INTEGER(list_len, code, i, next_off, next_off);
                int list_off = next_off;
                int num_elements = list_len / 2;
                //
                // Count how many of the entries in list(...) are not already in src
                //
                unsigned new_entries = 0;
                for (int j = 0;  j < num_elements;  ++j) {
                    term key, value;
                    DECODE_COMPACT_TERM(key, code, i, next_off, next_off);
                    DECODE_COMPACT_TERM(value, code, i, next_off, next_off);

                    #ifdef IMPL_EXECUTE_LOOP
                        if (term_find_map_pos(ctx, src, key) == -1) {
                            new_entries++;
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
                    if (memory_ensure_free(ctx, heap_needed) != MEMORY_GC_OK) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    DECODE_COMPACT_TERM(src, code, i, src_offset, src_offset);
                    //
                    //
                    //
                    struct kv_pair *kv = malloc(num_elements * sizeof(struct kv_pair));
                    if (IS_NULL_PTR(kv)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    for (int j = 0; j < num_elements; j++) {
                        term key, value;
                        DECODE_COMPACT_TERM(key, code, i, list_off, list_off);
                        DECODE_COMPACT_TERM(value, code, i, list_off, list_off);
                        kv[j].key = key;
                        kv[j].value = value;
                    }
                    sort_kv_pairs(ctx, kv, num_elements);
                    //
                    // Create a new map of the requested size and stitch src
                    // and kv together into new map.  Both src and kv are sorted.
                    //
                    term map = term_alloc_map_maybe_shared(ctx, new_map_size, is_shared ? term_get_map_keys(src) : term_invalid_term());
                    int src_pos = 0;
                    int kv_pos = 0;
                    for (int j = 0; j < new_map_size; j++) {
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
                            int c = term_compare(src_key, new_key, ctx);
                            if (c < 0) {
                                term src_value = term_get_map_value(src, src_pos);
                                term_set_map_assoc(map, j, src_key, src_value);
                                src_pos++;
                            } else if (0 < c) {
                                term new_value = kv[kv_pos].value;
                                term_set_map_assoc(map, j, new_key, new_value);
                                kv_pos++;
                            } else { // keys are the same
                                term new_value = kv[kv_pos].value;
                                term_set_map_assoc(map, j, src_key, new_value);
                                src_pos++;
                                kv_pos++;
                            }
                        }
                    }
                    free(kv);

                    WRITE_REGISTER(dreg_type, dreg, map);
                #endif

                NEXT_INSTRUCTION(next_off);

                break;
            }

            case OP_PUT_MAP_EXACT: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term src;
                int src_offset = next_off;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);
                int live;
                DECODE_INTEGER(live, code, i, next_off, next_off);

                TRACE("put_map_exact/5: label: %i src: 0x%lx dest=%c%i live: %i\n", label, src, T_DEST_REG(dreg_type, dreg), live);

                next_off++; //skip extended list tag {z, 1}
                int list_len;
                DECODE_INTEGER(list_len, code, i, next_off, next_off);
                int list_off = next_off;
                int num_elements = list_len / 2;
                //
                // Make sure every key from list is in src
                //
                for (int j = 0;  j < num_elements;  ++j) {
                    term key, value;
                    DECODE_COMPACT_TERM(key, code, i, next_off, next_off);
                    DECODE_COMPACT_TERM(value, code, i, next_off, next_off);

                    #ifdef IMPL_EXECUTE_LOOP
                        if (term_find_map_pos(ctx, src, key) == -1) {
                            RAISE_ERROR(BADARG_ATOM);
                        }
                    #endif
                }
                #ifdef IMPL_EXECUTE_LOOP
                    //
                    // Maybe GC, and reset the src term in case it changed
                    //
                    size_t src_size = term_get_map_size(src);
                    if (memory_ensure_free(ctx, term_map_size_in_terms_maybe_shared(src_size, true)) != MEMORY_GC_OK) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                    DECODE_COMPACT_TERM(src, code, i, src_offset, src_offset);
                    //
                    // Create a new map of the same size as src and populate with entries from src
                    //
                    term map = term_alloc_map_maybe_shared(ctx, src_size, term_get_map_keys(src));
                    for (int j = 0;  j < src_size;  ++j) {
                        term_set_map_assoc(map, j, term_get_map_key(src, j), term_get_map_value(src, j));
                    }
                    //
                    // Copy the new terms into the new map, in situ only
                    //
                    for (int j = 0;  j < num_elements;  ++j) {
                        term key, value;
                        DECODE_COMPACT_TERM(key, code, i, list_off, list_off);
                        DECODE_COMPACT_TERM(value, code, i, list_off, list_off);
                        int pos = term_find_map_pos(ctx, src, key);
                        term_set_map_assoc(map, pos, key, value);
                    }
                    WRITE_REGISTER(dreg_type, dreg, map);
                #endif

                NEXT_INSTRUCTION(next_off);

                break;
            }

            case OP_IS_MAP: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);

                TRACE("has_map_fields/3: label: %i src: 0x%lx\n", label, src);

                next_off++; //skip extended list tag {z, 1}
                int list_len;
                DECODE_INTEGER(list_len, code, i, next_off, next_off);
                int fail = 0;
                for (int j = 0;  j < list_len && !fail;  ++j) {
                    term key, value;
                    DECODE_COMPACT_TERM(key, code, i, next_off, next_off);

                    #ifdef IMPL_EXECUTE_LOOP
                        int pos = term_find_map_pos(ctx, src, key);
                        if (pos == -1) {
                            i = POINTER_TO_II(mod->labels[label]);
                            fail = 1;
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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                TRACE("get_map_elements/3: label: %i src: 0x%lx\n", label, src);

                next_off++; //skip extended list tag {z, 1}
                int list_len;
                DECODE_INTEGER(list_len, code, i, next_off, next_off);
                int num_elements = list_len / 2;
                int fail = 0;
                for (int j = 0;  j < num_elements && !fail;  ++j) {
                    term key, value;
                    DECODE_COMPACT_TERM(key, code, i, next_off, next_off);
                    dreg_t dreg;
                    dreg_type_t dreg_type;
                    DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                    #ifdef IMPL_EXECUTE_LOOP
                        int pos = term_find_map_pos(ctx, src, key);
                        if (pos == -1) {
                            i = POINTER_TO_II(mod->labels[label]);
                            fail = 1;
                        } else {
                            term value = term_get_map_value(src, pos);
                            WRITE_REGISTER(dreg_type, dreg, value);
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
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)
                int arity;
                DECODE_INTEGER(arity, code, i, next_off, next_off)
                int tag_atom_id;
                DECODE_ATOM(tag_atom_id, code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_tagged_tuple/2, label=%i, arg1=%lx, arity=%i, atom_id=%i\n", label, arg1, arity, tag_atom_id);

                    term tag_atom = module_get_atom_term_by_id(mod, tag_atom_id);

                    if (term_is_tuple(arg1) && (term_get_tuple_arity(arg1) == arity) && (term_get_tuple_element(arg1, 0) == tag_atom)) {
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

#ifdef ENABLE_OTP21
            case OP_GET_HD: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, code, i, next_off, next_off)
                dreg_t head_dreg;
                dreg_type_t head_dreg_type;
                DECODE_DEST_REGISTER(head_dreg, head_dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("get_hd/2 %lx, %c%i\n", src_value, T_DEST_REG(head_dreg_type, head_dreg));

                    term head = term_get_list_head(src_value);

                    WRITE_REGISTER(head_dreg_type, head_dreg, head);
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
                DECODE_COMPACT_TERM(src_value, code, i, next_off, next_off)
                dreg_t tail_dreg;
                dreg_type_t tail_dreg_type;
                DECODE_DEST_REGISTER(tail_dreg, tail_dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("get_tl/2 %lx, %c%i\n", src_value, T_DEST_REG(tail_dreg_type, tail_dreg));

                    term tail = term_get_list_tail(src_value);

                    WRITE_REGISTER(tail_dreg_type, tail_dreg, tail);
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
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);
                next_off++; //skip extended list tag
                int size;
                DECODE_INTEGER(size, code, i, next_off, next_off)

                TRACE("put_tuple2/2, size=%i\n", size);
                USED_BY_TRACE(size);

                #ifdef IMPL_CODE_LOADER
                    UNUSED(dreg);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    term t = term_alloc_tuple(size, ctx);
                #endif

                for (int j = 0; j < size; j++) {
                    term element;
                    DECODE_COMPACT_TERM(element, code, i, next_off, next_off)

                    #ifdef IMPL_CODE_LOADER
                        UNUSED(element);
                    #endif

                    #ifdef IMPL_EXECUTE_LOOP
                        term_put_tuple_element(t, j, element);
                    #endif
                }

                #ifdef IMPL_EXECUTE_LOOP
                    WRITE_REGISTER(dreg_type, dreg, t);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }
#endif

#ifdef ENABLE_OTP23
            case OP_SWAP: {
                int next_off = 1;
                dreg_t reg_a;
                dreg_type_t reg_a_type;
                DECODE_DEST_REGISTER(reg_a, reg_a_type, code, i, next_off, next_off);
                dreg_t reg_b;
                dreg_type_t reg_b_type;
                DECODE_DEST_REGISTER(reg_b, reg_b_type, code, i, next_off, next_off);

                TRACE("swap/2 a=%c%i, b=%c%i\n", T_DEST_REG(reg_a_type, reg_a), T_DEST_REG(reg_b_type, reg_b));

                #ifdef IMPL_EXECUTE_LOOP
                    term a = READ_DEST_REGISTER(reg_a_type, reg_a);
                    term b = READ_DEST_REGISTER(reg_b_type, reg_b);

                    WRITE_REGISTER(reg_a_type, reg_a, b);
                    WRITE_REGISTER(reg_b_type, reg_b, a);
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BS_START_MATCH4: {
                #ifdef IMPL_EXECUTE_LOOP
                    if (memory_ensure_free(ctx, TERM_BOXED_BIN_MATCH_STATE_SIZE) != MEMORY_GC_OK) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }
                #endif

                int next_off = 1;
                // fail since OTP 23 might be either 'no_fail', 'resume' or a fail label
                // we are ignoring this right now, but we might use it for future optimizations.
                term fail;
                DECODE_COMPACT_TERM(fail, code, i, next_off, next_off);
                term live;
                DECODE_COMPACT_TERM(live, code, i, next_off, next_off);
                term src;
                DECODE_COMPACT_TERM(src, code, i, next_off, next_off);
                dreg_t dreg;
                dreg_type_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_CODE_LOADER
                    TRACE("bs_start_match4/4\n");
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("bs_start_match4/4, fail=%i live=0x%lx src=0x%lx dreg=%c%i\n", fail, live, src, T_DEST_REG(dreg_type, dreg));

                    if (!(term_is_binary(src) || term_is_match_state(src))) {
                        WRITE_REGISTER(dreg_type, dreg, src);
                        i = POINTER_TO_II(mod->labels[fail]);
                    } else {
                        term match_state = term_alloc_bin_match_state(src, 0, ctx);

                        WRITE_REGISTER(dreg_type, dreg, match_state);
                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif
                break;
            }
#endif

            default:
                printf("Undecoded opcode: %i\n", code[i]);
                #ifdef IMPL_EXECUTE_LOOP
                    fprintf(stderr, "failed at %i\n", i);
                #endif

                AVM_ABORT();
                return 1;
        }

        continue;

#ifdef IMPL_EXECUTE_LOOP
do_abort:
        ctx->x[0] = ERROR_ATOM;
        ctx->x[1] = VM_ABORT_ATOM;

handle_error:
        {
            int target_label = get_catch_label_and_change_module(ctx, &mod);
            if (target_label) {
                code = mod->code->code;
                JUMP_TO_ADDRESS(mod->labels[target_label]);
                continue;
            }
        }

        dump(ctx);

        {
            bool throw = ctx->x[0] == THROW_ATOM;

            int exit_reason_tuple_size = (throw ? TUPLE_SIZE(2) : 0) + TUPLE_SIZE(2);
            if (memory_ensure_free(ctx, exit_reason_tuple_size) != MEMORY_GC_OK) {
                ctx->exit_reason = OUT_OF_MEMORY_ATOM;
            } else {
                term error_term;
                if (throw) {
                    error_term = term_alloc_tuple(2, ctx);
                    term_put_tuple_element(error_term, 0, NOCATCH_ATOM);
                    term_put_tuple_element(error_term, 1, ctx->x[1]);
                } else {
                    error_term = ctx->x[1];
                }

                term exit_reason_tuple = term_alloc_tuple(2, ctx);
                term_put_tuple_element(exit_reason_tuple, 0, error_term);
                term_put_tuple_element(exit_reason_tuple, 1, term_nil());
                ctx->exit_reason = exit_reason_tuple;
            }
        }

terminate_context:
        TRACE("-- Code execution finished for %i--\n", ctx->process_id);
        if (ctx->leader) {
            return 0;
        }
        GlobalContext *global = ctx->global;
        scheduler_terminate(ctx);
        Context *scheduled_context = scheduler_do_wait(global);
        if (UNLIKELY(scheduled_context == ctx)) {
            fprintf(stderr, "bug: scheduled a terminated process!\n");
            return 0;
        }

        ctx = scheduled_context;
        x_regs = ctx->x;
        mod = ctx->saved_module;
        code = mod->code->code;
        remaining_reductions = DEFAULT_REDUCTIONS_AMOUNT;
        JUMP_TO_ADDRESS(scheduled_context->saved_ip);
#endif
    }
}

#pragma GCC diagnostic pop

#undef DECODE_COMPACT_TERM
