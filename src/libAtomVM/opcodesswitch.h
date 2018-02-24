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

#include "module.h"

#include <assert.h>
#include <string.h>

#include "debug.h"
#include "exportedfunction.h"
#include "utils.h"
#include "scheduler.h"

#ifdef IMPL_EXECUTE_LOOP
    #include "mailbox.h"
#endif

//#define ENABLE_TRACE

#ifndef TRACE
    #ifdef ENABLE_TRACE
        #define TRACE printf
    #else
        #define TRACE(...)
    #endif
#endif

#define USED_BY_TRACE(x) \
    (void) (x)

#define COMPACT_SMALLINT4 1
#define COMPACT_ATOM 2
#define COMPACT_XREG 3
#define COMPACT_YREG 4
#define COMPACT_EXTENDED 7
#define COMPACT_LARGE_INTEGER 9
#define COMPACT_LARGE_ATOM 10

#define COMPACT_EXTENDED_LITERAL 0x47

#define COMPACT_LARGE_IMM_MASK 0x18
#define COMPACT_11BITS_VALUE 0x8
#define COMPACT_NBITS_VALUE 0x18

#ifdef IMPL_CODE_LOADER
#define DECODE_COMPACT_TERM(dest_term, code_chunk, base_index, off, next_operand_offset)\
{                                                                                       \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                            \
    switch (first_byte & 0xF) {                                                         \
        case COMPACT_SMALLINT4:                                                         \
        case COMPACT_ATOM:                                                              \
        case COMPACT_XREG:                                                              \
        case COMPACT_YREG:                                                              \
            next_operand_offset += 1;                                                   \
            break;                                                                      \
                                                                                        \
        case COMPACT_EXTENDED:                                                          \
            switch (first_byte) {                                                       \
                case COMPACT_EXTENDED_LITERAL:                                          \
                    next_operand_offset += 2;                                           \
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
                default:                                                                \
                    assert((first_byte & 0x30) != COMPACT_LARGE_INTEGER);               \
                    break;                                                              \
            }                                                                           \
            break;                                                                      \
                                                                                        \
        default:                                                                        \
            fprintf(stderr, "unknown compect term type: %i\n", ((first_byte) & 0xF));   \
            abort();                                                                    \
            break;                                                                      \
    }                                                                                   \
}
#endif

#ifdef IMPL_EXECUTE_LOOP
#define DECODE_COMPACT_TERM(dest_term, code_chunk, base_index, off, next_operand_offset)                                \
{                                                                                                                       \
    uint8_t first_byte = (code_chunk[(base_index) + (off)]);                                                            \
    switch (first_byte & 0xF) {                                                                                         \
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
                case COMPACT_EXTENDED_LITERAL:                                                                          \
                    dest_term = module_load_literal(mod, code_chunk[(base_index) + (off) + 1] >> 4);                    \
                    next_operand_offset += 2;                                                                           \
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
                    abort();                                                                                            \
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
                default:                                                                                                \
                    abort();                                                                                            \
                    break;                                                                                              \
            }                                                                                                           \
            break;                                                                                                      \
                                                                                                                        \
        default:                                                                                                        \
            abort();                                                                                                    \
    }                                                                                                                   \
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
            abort();                                                                                \
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
            abort();                                                                                \
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
            abort();                                                                                \
            break;                                                                                  \
    }                                                                                               \
}


#define NEXT_INSTRUCTION(operands_size) \
    i += 1 + operands_size

#ifndef TRACE_JUMP
    #define JUMP_TO_ADDRESS(address) \
        i = ((uint8_t *) (address)) - chunk->code
#else
    #define JUMP_TO_ADDRESS(address) \
        i = ((uint8_t *) (address)) - chunk->code; \
        fprintf(stderr, "going to jump to %i\n", i)
#endif

#define OP_LABEL 1
#define OP_FUNC_INFO 2
#define OP_INT_CALL_END 3
#define OP_CALL 4
#define OP_CALL_LAST 5
#define OP_CALL_ONLY 6
#define OP_CALL_EXT 7
#define OP_BIF0 9
#define OP_BIF1 10
#define OP_BIF2 11
#define OP_ALLOCATE 12
#define OP_ALLOCATE_HEAP 13
#define OP_ALLOCATE_ZERO 14
#define OP_TEST_HEAP 16
#define OP_KILL 17
#define OP_DEALLOCATE 18
#define OP_RETURN 19
#define OP_SEND 20
#define OP_REMOVE_MESSAGE 21
#define OP_TIMEOUT 22
#define OP_LOOP_REC 23
#define OP_WAIT 25
#define OP_WAIT_TIMEOUT 26
#define OP_IS_EQUAL 41
#define OP_IS_EQ_EXACT 43
#define OP_IS_INTEGER 45
#define OP_IS_ATOM 48
#define OP_IS_NIL 52
#define OP_IS_NONEMPTY_LIST 56
#define OP_IS_TUPLE 57
#define OP_TEST_ARITY 58
#define OP_SELECT_VAL 59
#define OP_SELECT_TUPLE_ARITY 60
#define OP_JUMP 61
#define OP_MOVE 64
#define OP_GET_LIST 65
#define OP_GET_TUPLE_ELEMENT 66
#define OP_PUT_LIST 69
#define OP_PUT_TUPLE 70
#define OP_PUT 71
#define OP_CALL_EXT_ONLY 78
#define OP_GC_BIF1 124
#define OP_GC_BIF2 125
#define OP_TRIM 136
#define OP_LINE 153

#define INSTRUCTION_POINTER() \
    ((unsigned long) &chunk->code[i])

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"

#ifdef IMPL_CODE_LOADER
    int read_core_chunk(Module *mod)
#else
    #ifdef IMPL_EXECUTE_LOOP
        int context_execute_loop(Context *ctx, Module *mod, uint8_t *beam_file, const char *function_name, int arity)
    #else
        #error Need implementation type
    #endif
#endif
{
    CodeChunk *chunk = mod->code;

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

        JUMP_TO_ADDRESS(mod->labels[label]);
    #endif

    while(1) {

        switch (chunk->code[i]) {
            case OP_LABEL: {
                int label;
                int next_offset = 1;
                DECODE_LABEL(label, chunk->code, i, next_offset, next_offset)

                TRACE("label/1 label=%i\n", label);

                #ifdef IMPL_CODE_LOADER
                    TRACE("Mark label %i here at %i\n", label, i);
                    module_add_label(mod, label, &chunk->code[i]);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    UNUSED(label)
                #endif

                NEXT_INSTRUCTION(next_offset - 1);
                break;
            }

            case OP_FUNC_INFO: {
                int next_offset = 1;
                int module_atom;
                DECODE_ATOM(module_atom, chunk->code, i, next_offset, next_offset)
                int function_name_atom;
                DECODE_ATOM(function_name_atom, chunk->code, i, next_offset, next_offset)
                int arity = chunk->code[i + next_offset] & 0xF;

                TRACE("func_info/3 module_name_a=%i, function_name_a=%i, arity=%i\n", module_atom, function_name_atom, arity);
                USED_BY_TRACE(function_name_atom);
                USED_BY_TRACE(module_atom);
                USED_BY_TRACE(arity);

                NEXT_INSTRUCTION(next_offset);
                break;
            }

            case OP_INT_CALL_END: {
                TRACE("int_call_end!\n");
                return 1;
            }

            case OP_CALL: {
                int arity = chunk->code[i + 1] >> 4;
                int next_offset = 2;
                int label = 0;
                DECODE_LABEL(label, chunk->code, i, next_offset, next_offset)

                TRACE("call/2, arity=%i, label=%i\n", arity, label);
                USED_BY_TRACE(arity);

                #ifdef IMPL_EXECUTE_LOOP
                    NEXT_INSTRUCTION(next_offset - 1);
                    ctx->cp = INSTRUCTION_POINTER();

                    JUMP_TO_ADDRESS(mod->labels[label]);
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_offset - 1);
                #endif

                break;
            }

            case OP_CALL_LAST: {
                int arity = chunk->code[i + 1] >> 4;
                int next_offset = 2;
                int label = 0;
                DECODE_LABEL(label, chunk->code, i, next_offset, next_offset)
                int n_words = chunk->code[i + next_offset] >> 4;
                UNUSED(n_words)

                TRACE("call_last/3, arity=%i, label=%i, dellocate=%i\n", arity, label, n_words);
                USED_BY_TRACE(arity);

                #ifdef IMPL_EXECUTE_LOOP
                    ctx->cp = ctx->e[n_words];
                    ctx->e += (n_words + 1);

                    DEBUG_DUMP_STACK(ctx);

                    JUMP_TO_ADDRESS(mod->labels[label]);
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_offset + 1 - 1);
                #endif

                break;
            }

            case OP_CALL_ONLY: {
                int arity = chunk->code[i + 1] >> 4;
                int label = chunk->code[i + 2] >> 4;

                TRACE("call_only/2, arity=%i, label=%i\n", arity, label);
                USED_BY_TRACE(arity);

                #ifdef IMPL_EXECUTE_LOOP
                    NEXT_INSTRUCTION(2);
                    JUMP_TO_ADDRESS(mod->labels[label]);
                #endif

                #ifdef IMPL_CODE_LOADER
                    UNUSED(label);
                    NEXT_INSTRUCTION(2);
                #endif

                break;
            }

            case OP_CALL_EXT: {
                int arity = chunk->code[i + 1] >> 4;
                int index = chunk->code[i + 2] >> 4;

                TRACE("call_ext/2, arity=%i, index=%i\n", arity, index);
                USED_BY_TRACE(arity);
                USED_BY_TRACE(index);

                #ifdef IMPL_EXECUTE_LOOP
                    const struct ExportedFunction *func = mod->imported_funcs[index].func;
                    switch (func->type) {
                        case NIFFunctionType: {
                            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
                            ctx->x[0] = nif->nif_ptr(ctx, arity, ctx->x);
                            break;
                        }
                        default: {
                            fprintf(stderr, "Unsupported function type.\n");
                            abort();
                        }
                    }
                #endif

                NEXT_INSTRUCTION(2);

                break;
            }

            case OP_BIF0: {
                int bif = chunk->code[i + 1] >> 4;
                int dreg = chunk->code[i + 2] >> 4;

                TRACE("bif0/2 bif=%i, dreg=%i\n", bif, dreg);
                USED_BY_TRACE(bif);
                USED_BY_TRACE(dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    BifImpl0 func = (BifImpl0) mod->imported_funcs[bif].bif;
                    term ret = func(ctx);

                    int reg_b_type = reg_type_c(chunk->code[i + 2] & 0xF);
                    if (reg_b_type == 'x') {
                        ctx->x[dreg] = ret;
                    } else if (reg_b_type == 'y') {
                        ctx->e[dreg] = ret;
                    } else {
                        abort();
                    }
                #endif

                NEXT_INSTRUCTION(2);
                break;
            }

            //TODO: implement me
            case OP_BIF1: {
                int next_off = 1;
                int fail_label;
                DECODE_LABEL(fail_label, chunk->code, i, next_off, next_off)
                int bif = chunk->code[i + next_off] >> 4;
                next_off++;
                term arg1;
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off)
                int dreg = chunk->code[i + next_off];

                TRACE("bif1/2 bif=%i, dreg=%i\n", bif, dreg);
                USED_BY_TRACE(bif);
                USED_BY_TRACE(dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    BifImpl1 func = (BifImpl1) mod->imported_funcs[bif].bif;
                    term ret = func(ctx, 0, 0, arg1);

                    int reg_b_type = reg_type_c(dreg & 0xF);
                    if (reg_b_type == 'x') {
                        ctx->x[dreg >> 4] = ret;
                    } else if (reg_b_type == 'y') {
                        ctx->e[dreg >> 4] = ret;
                    } else {
                        abort();
                    }
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            //TODO: implement me
            case OP_BIF2: {
                int next_off = 1;
                int fail_label;
                DECODE_LABEL(fail_label, chunk->code, i, next_off, next_off)
                int bif = chunk->code[i + next_off] >> 4;
                next_off++;
                term arg1;
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off)
                term arg2;
                DECODE_COMPACT_TERM(arg2, chunk->code, i, next_off, next_off)
                int dreg = chunk->code[i + next_off];

                TRACE("bif2/2 bif=%i, dreg=%i\n", bif, dreg);
                USED_BY_TRACE(bif);
                USED_BY_TRACE(dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    BifImpl2 func = (BifImpl2) mod->imported_funcs[bif].bif;
                    term ret = func(ctx, 0, 0, arg1, arg2);

                    int reg_b_type = reg_type_c(dreg & 0xF);
                    if (reg_b_type == 'x') {
                        ctx->x[dreg >> 4] = ret;
                    } else if (reg_b_type == 'y') {
                        ctx->e[dreg >> 4] = ret;
                    } else {
                        abort();
                    }
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_ALLOCATE: {
                int stack_need = chunk->code[i + 1] >> 4;
                int live = chunk->code[i + 2] >> 4;
                TRACE("allocate/2 stack_need=%i, live=%i\n" , stack_need, live);
                USED_BY_TRACE(stack_need);
                USED_BY_TRACE(live);

                #ifdef IMPL_EXECUTE_LOOP
                    if (live > 16) {
                        fprintf(stderr, "Cannot use more than 16 registers.");
                        abort();
                    }

                    if ((ctx->e - (stack_need + 1)) < ctx->stack) {
                        fprintf(stderr, "Need to allocate more stack space.");
                        abort();
                    }
                    ctx->e -= stack_need + 1;
                    ctx->e[stack_need] = ctx->cp;
                #endif

                NEXT_INSTRUCTION(2);
                break;
            }

            case OP_ALLOCATE_HEAP: {
                int stack_need = chunk->code[i + 1] >> 4;
                int heap_need = chunk->code[i + 2] >> 4;
                int live = chunk->code[i + 3] >> 4;
                TRACE("allocate_heap/2 stack_need=%i, heap_need=%i, live=%i\n", stack_need, heap_need, live);
                USED_BY_TRACE(stack_need);
                USED_BY_TRACE(heap_need);
                USED_BY_TRACE(live);

                #ifdef IMPL_EXECUTE_LOOP
                    if (live > 16) {
                        fprintf(stderr, "Cannot use more than 16 registers.");
                        abort();
                    }

                    if ((ctx->e - (stack_need + 1)) < ctx->stack) {
                        fprintf(stderr, "Need to allocate more stack space.");
                        abort();
                    }
                    ctx->e -= stack_need + 1;
                    ctx->e[stack_need] = ctx->cp;
                #endif

                NEXT_INSTRUCTION(3);
                break;
            }

            case OP_ALLOCATE_ZERO: {
                int stack_need = chunk->code[i + 1] >> 4;
                int live = chunk->code[i + 2] >> 4;
                TRACE("allocate_zero/2 stack_need=%i, live=%i\n", stack_need, live);
                USED_BY_TRACE(stack_need);
                USED_BY_TRACE(live);

                #ifdef IMPL_EXECUTE_LOOP
                    if (live > 16) {
                        fprintf(stderr, "Cannot use more than 16 registers.");
                        abort();
                    }

                    if ((ctx->e - (stack_need + 1)) < ctx->stack) {
                        fprintf(stderr, "Need to allocate more stack space.");
                        abort();
                    }

                    ctx->e -= stack_need + 1;
                    ctx->e[stack_need] = ctx->cp;
                #endif

                //TODO: bzero/memset

                NEXT_INSTRUCTION(2);
                break;
            }

            case OP_TEST_HEAP: {
                TRACE("test_heap/2\n");

                //TODO: implement test_heap

                NEXT_INSTRUCTION(2);
                break;
            }

            case OP_KILL: {
                TRACE("kill/1");

                NEXT_INSTRUCTION(1);

                break;
            }

            case OP_DEALLOCATE: {
                int n_words = chunk->code[i + 1] >> 4;

                TRACE("deallocate/1 n_words=%i\n", n_words);
                USED_BY_TRACE(n_words);

                #ifdef IMPL_EXECUTE_LOOP
                    DEBUG_DUMP_STACK(ctx);

                    ctx->cp = ctx->e[n_words];
                    ctx->e += n_words + 1;
                    DEBUG_DUMP_STACK(ctx);
                #endif

                NEXT_INSTRUCTION(1);
                break;
            }

            case OP_RETURN: {
                TRACE("return/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    if ((long) ctx->cp == -1) {
                        return 0;
                    }

                    JUMP_TO_ADDRESS(ctx->cp);
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(0);
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
                    Context *target = globalcontext_get_process(ctx->global, local_process_id);

                    mailbox_send(target, ctx->x[1]);
                #endif

                NEXT_INSTRUCTION(0);
                break;
            }

            //TODO: implement remove_message/0
            case OP_REMOVE_MESSAGE: {
                TRACE("remove_message/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    ctx->x[0] = mailbox_receive(ctx);
                #endif

                NEXT_INSTRUCTION(0);
                break;
            }

            //TODO: implement timeout/0
            case OP_TIMEOUT: {
                TRACE("timeout/0\n");

                NEXT_INSTRUCTION(0);
                break;
            }

            //TODO: implemente loop_rec/2
            case OP_LOOP_REC: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, chunk->code, i, next_off, next_off)
                int dreg = chunk->code[i + next_off] >> 4;

                TRACE("loop_rec/2\n");

                #ifdef IMPL_EXECUTE_LOOP
                    if (ctx->mailbox == NULL) {
                        JUMP_TO_ADDRESS(mod->labels[label]);
                    } else {
                        term ret = mailbox_peek(ctx);

                        int reg_b_type = reg_type_c(chunk->code[i + next_off] & 0xF);
                        if (reg_b_type == 'x') {
                            ctx->x[dreg] = ret;
                        } else if (reg_b_type == 'y') {
                            ctx->e[dreg] = ret;
                        } else {
                            abort();
                        }

                        NEXT_INSTRUCTION(next_off);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            //TODO: implement wait/1
            case OP_WAIT: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, chunk->code, i, next_off, next_off)

                TRACE("wait/1\n");

                #ifdef IMPL_EXECUTE_LOOP
                    ctx->saved_ip = mod->labels[label];
                    ctx->jump_to_on_restore = NULL;
                    Context *scheduled_context = scheduler_wait(ctx->global, ctx, -1);
                    ctx = scheduled_context;

                    if (scheduled_context->jump_to_on_restore && ctx->mailbox) {
                        JUMP_TO_ADDRESS(scheduled_context->jump_to_on_restore);
                    } else {
                        JUMP_TO_ADDRESS(scheduled_context->saved_ip);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

            //TODO: implement wait_timeout/2
            case OP_WAIT_TIMEOUT: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, chunk->code, i, next_off, next_off)
                int timeout;
                DECODE_COMPACT_TERM(timeout, chunk->code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("wait_timeout/2, label: %i, timeout: %x\n", label, term_to_int32(timeout));

                    NEXT_INSTRUCTION(next_off - 1);
                    //TODO: it looks like x[0] might be used instead of jump_to_on_restore
                    ctx->saved_ip = INSTRUCTION_POINTER();
                    ctx->jump_to_on_restore = mod->labels[label];
                    Context *scheduled_context = scheduler_wait(ctx->global, ctx, term_to_int32(timeout));
                    ctx = scheduled_context;

                    if (scheduled_context->jump_to_on_restore && ctx->mailbox) {
                        JUMP_TO_ADDRESS(scheduled_context->jump_to_on_restore);
                    } else {
                        JUMP_TO_ADDRESS(scheduled_context->saved_ip);
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("wait_timeout/2, label: %i\n", label);

                    UNUSED(timeout)

                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

            case OP_IS_EQUAL: {
                int label;
                term arg1;
                term arg2;
                int next_off = 1;
                DECODE_LABEL(label, chunk->code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg2, chunk->code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_equal/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    if (arg1 == arg2) {
                        NEXT_INSTRUCTION(next_off - 1);
                    } else {
                        i = (uint8_t *) mod->labels[label] - chunk->code;
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_equal/2\n");
                    UNUSED(arg1)
                    UNUSED(arg2)
                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

            case OP_IS_EQ_EXACT: {
                int label;
                term arg1;
                term arg2;
                int next_off = 1;
                DECODE_LABEL(label, chunk->code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg2, chunk->code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_eq_exact/2, label=%i, arg1=%lx, arg2=%lx\n", label, arg1, arg2);

                    if (arg1 == arg2) {
                        NEXT_INSTRUCTION(next_off - 1);
                    } else {
                        i = (uint8_t *) mod->labels[label] - chunk->code;
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_eq_exact/2\n");
                    UNUSED(arg1)
                    UNUSED(arg2)
                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

           case OP_IS_INTEGER: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, chunk->code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_integer/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_integer(arg1)) {
                        NEXT_INSTRUCTION(next_off - 1);
                    } else {
                        i = (uint8_t *) mod->labels[label] - chunk->code;
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_integer/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

            case OP_IS_NONEMPTY_LIST: {
                int label;
                term arg1;
                int next_off = 1;
                DECODE_LABEL(label, chunk->code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_nonempty_list/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_list(arg1)) {
                        NEXT_INSTRUCTION(next_off - 1);
                    } else {
                        i = (uint8_t *) mod->labels[label] - chunk->code;
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_nonempty_list/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

            case OP_IS_NIL: {
                int label;
                term arg1;
                int next_off = 1;
                DECODE_LABEL(label, chunk->code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_nil/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_nil(arg1)) {
                        NEXT_INSTRUCTION(next_off - 1);
                    } else {
                        i = (uint8_t *) mod->labels[label] - chunk->code;
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_nil/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

            case OP_IS_ATOM: {
                int label;
                term arg1;
                int next_off = 1;
                DECODE_LABEL(label, chunk->code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_atom/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_atom(arg1)) {
                        NEXT_INSTRUCTION(next_off - 1);
                    } else {
                        i = (uint8_t *) mod->labels[label] - chunk->code;
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_atom/2\n");
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

           case OP_IS_TUPLE: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, chunk->code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_tuple/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_is_tuple(arg1)) {
                        NEXT_INSTRUCTION(next_off - 1);
                    } else {
                        i = (uint8_t *) mod->labels[label] - chunk->code;
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("is_tuple/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

           case OP_TEST_ARITY: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, chunk->code, i, next_off, next_off);
                term arg1;
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off);
                int arity;
                DECODE_INTEGER(arity, chunk->code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("test_arity/2, label=%i, arg1=%lx\n", label, arg1);

                    if (term_get_tuple_arity(arg1) == arity) {
                        NEXT_INSTRUCTION(next_off - 1);
                    } else {
                        i = (uint8_t *) mod->labels[label] - chunk->code;
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("test_arity/2\n");
                    UNUSED(label)
                    UNUSED(arg1)
                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

            case OP_SELECT_VAL: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, chunk->code, i, next_off, next_off)
                int default_label;
                DECODE_LABEL(default_label, chunk->code, i, next_off, next_off)
                next_off++; //skip extended list tag
                int size;
                DECODE_INTEGER(size, chunk->code, i, next_off, next_off)

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
                    DECODE_COMPACT_TERM(cmp_value, chunk->code, i, next_off, next_off)
                    int jmp_label;
                    DECODE_LABEL(jmp_label, chunk->code, i, next_off, next_off)

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
                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

            case OP_SELECT_TUPLE_ARITY: {
                int next_off = 1;
                term src_value;
                DECODE_INTEGER(src_value, chunk->code, i, next_off, next_off)
                int default_label;
                DECODE_LABEL(default_label, chunk->code, i, next_off, next_off)
                next_off++; //skip extended list tag
                int size;
                DECODE_INTEGER(size, chunk->code, i, next_off, next_off)

                TRACE("select_tuple_arity/3, default_label=%i, vals=%i\n", default_label, size);
                USED_BY_TRACE(default_label);
                USED_BY_TRACE(size);

                #ifdef IMPL_CODE_LOADER
                    UNUSED(src_value);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    void *jump_to_address = NULL;
                #endif

                for (int j = 0; j < size / 2; j++) {
                    int cmp_value;
                    DECODE_INTEGER(cmp_value, chunk->code, i, next_off, next_off)
                    int jmp_label;
                    DECODE_LABEL(jmp_label, chunk->code, i, next_off, next_off)

                    #ifdef IMPL_CODE_LOADER
                        UNUSED(cmp_value);
                    #endif

                    #ifdef IMPL_EXECUTE_LOOP
                        if (!jump_to_address && (term_get_tuple_arity(src_value) == cmp_value)) {
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
                    NEXT_INSTRUCTION(next_off - 1);
                #endif

                break;
            }

            case OP_JUMP: {
                int label;
                int next_offset = 1;
                DECODE_LABEL(label, chunk->code, i, next_offset, next_offset)

                TRACE("jump/1 label=%i\n", label);
                USED_BY_TRACE(label);

                #ifdef IMPL_EXECUTE_LOOP
                    JUMP_TO_ADDRESS(mod->labels[label]);
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_offset - 1);
                #endif

                break;
            }

            case OP_MOVE: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, chunk->code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    int reg_b_type = reg_type_c(chunk->code[i + next_off] & 0xF);
                    int dest = chunk->code[i + next_off] >> 4;

                    TRACE("move/2 %lx, %c%i\n", src_value, reg_b_type, dest);

                    if (reg_b_type == 'x') {
                        ctx->x[dest] = src_value;
                    } else if (reg_b_type == 'y') {
                        ctx->e[dest] = src_value;
                    } else {
                        abort();
                    }
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
                DECODE_COMPACT_TERM(src_value, chunk->code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    int head_reg_b_type = reg_type_c(chunk->code[i + next_off] & 0xF);
                    int head_dest = chunk->code[i + next_off] >> 4;
                    int tail_reg_b_type = reg_type_c(chunk->code[i + next_off + 1] & 0xF);
                    int tail_dest = chunk->code[i + next_off + 1] >> 4;

                    TRACE("get_list/3 %lx, %c%i, %c%i\n", src_value, head_reg_b_type, head_dest, tail_reg_b_type, tail_dest);

                    term head = term_get_list_head(src_value);
                    term tail = term_get_list_tail(src_value);
                    if (head_reg_b_type == 'x') {
                        ctx->x[head_dest] = head;
                    } else if (head_reg_b_type == 'y') {
                        ctx->e[head_dest] = head;
                    } else {
                        abort();
                    }

                    if (tail_reg_b_type == 'x') {
                        ctx->x[tail_dest] = tail;
                    } else if (tail_reg_b_type == 'y') {
                        ctx->e[tail_dest] = tail;
                    } else {
                        abort();
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("get_list/2\n");
                    UNUSED(src_value)
                #endif

                NEXT_INSTRUCTION(next_off + 1);
                break;
            }

            case OP_GET_TUPLE_ELEMENT: {
                int next_off = 1;
                term src_value;
                DECODE_COMPACT_TERM(src_value, chunk->code, i, next_off, next_off);
                int element;
                DECODE_INTEGER(element, chunk->code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    int reg_b_type = reg_type_c(chunk->code[i + next_off] & 0xF);
                    int dest = chunk->code[i + next_off] >> 4;

                    TRACE("get_tuple_element/2");

                    term t = term_get_tuple_element(src_value, element);
                    if (reg_b_type == 'x') {
                        ctx->x[dest] = t;
                    } else if (reg_b_type == 'y') {
                        ctx->e[dest] = t;
                    } else {
                        abort();
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    TRACE("get_tuple_element/2\n");
                    UNUSED(src_value)
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_PUT_LIST: {
                int next_off = 1;
                term head;
                DECODE_COMPACT_TERM(head, chunk->code, i, next_off, next_off);
                term tail;
                DECODE_COMPACT_TERM(tail, chunk->code, i, next_off, next_off);

                TRACE("op_put_list/3\n");

                #ifdef IMPL_EXECUTE_LOOP
                    int reg_b_type = reg_type_c(chunk->code[i + next_off] & 0xF);
                    int dest = chunk->code[i + next_off] >> 4;

                    term t = term_list_prepend(head, tail);
                    if (reg_b_type == 'x') {
                        ctx->x[dest] = t;
                    } else if (reg_b_type == 'y') {
                        ctx->e[dest] = t;
                    } else {
                        abort();
                    }
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_PUT_TUPLE: {
                int next_off = 1;
                int size;
                DECODE_INTEGER(size, chunk->code, i, next_off, next_off);
                int reg_b_type = reg_type_c(chunk->code[i + next_off] & 0xF);
                int dest = chunk->code[i + next_off] >> 4;
                next_off++;
                TRACE("put_tuple/2 size=%i, dest=%c%i\n", size, reg_b_type, dest);
                USED_BY_TRACE(reg_b_type);
                USED_BY_TRACE(dest);

                #ifdef IMPL_EXECUTE_LOOP
                    term t = term_alloc_tuple(size);
                    if (reg_b_type == 'x') {
                        ctx->x[dest] = t;
                    } else if (reg_b_type == 'y') {
                        ctx->e[dest] = t;
                    } else {
                        abort();
                    }
                #endif

                for (int j = 0; j < size; j++) {
                    if (chunk->code[i + next_off] != OP_PUT) {
                        fprintf(stderr, "Expected put, got opcode: %i\n", chunk->code[i + next_off]);
                        abort();
                    }
                    next_off++;
                    term put_value;
                    DECODE_COMPACT_TERM(put_value, chunk->code, i, next_off, next_off);
                    #ifdef IMPL_CODE_LOADER
                        TRACE("put/2\n");
                        UNUSED(put_value);
                    #endif

                    #ifdef IMPL_EXECUTE_LOOP
                        TRACE("put/2 elem=%i, value=0x%lx\n", j, put_value);
                        term_put_tuple_element(t, j, put_value);
                    #endif
                }

                NEXT_INSTRUCTION(next_off - 1);
                break;
            }

            case OP_CALL_EXT_ONLY: {
                int arity = chunk->code[i + 1] >> 4;
                int index = chunk->code[i + 2] >> 4;

                TRACE("call_ext_only/2, arity=%i, index=%i\n", arity, index);
                USED_BY_TRACE(arity);
                USED_BY_TRACE(index);

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(2);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    const struct ExportedFunction *func = mod->imported_funcs[index].func;
                    switch (func->type) {
                        case NIFFunctionType: {
                            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
                            ctx->x[0] = nif->nif_ptr(ctx, arity, ctx->x);
                            if ((long) ctx->cp == -1) {
                                return 0;
                            }

                            JUMP_TO_ADDRESS(ctx->cp);

                            break;
                        }
                        default: {
                            abort();
                        }
                    }
                #endif

                break;
            }

            case OP_GC_BIF1: {
                int f_label = chunk->code[i + 1] >> 4; //TODO: use DECODE_LABEL here
                int live = chunk->code[i + 2] >> 4;
                int bif = chunk->code[i + 3] >> 4;

                int next_off = 4;
                term arg1;
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off)
                int dreg = chunk->code[i + next_off] >> 4;

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("gc_bif1/5 fail_lbl=%i, live=%i, bif=%i, arg1=0x%lx, dest=r%i\n", f_label, live, bif, arg1, dreg);

                    BifImpl1 func = (BifImpl1) mod->imported_funcs[bif].bif;
                    term ret = func(ctx, f_label, live, arg1);

                    int reg_b_type = reg_type_c(chunk->code[i + next_off] & 0xF);
                    if (reg_b_type == 'x') {
                        ctx->x[dreg] = ret;
                    } else if (reg_b_type == 'y') {
                        ctx->e[dreg] = ret;
                    } else {
                        abort();
                    }
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
                int f_label = chunk->code[i + 1] >> 4; //TODO: use DECODE_LABEL here
                int live = chunk->code[i + 2] >> 4;
                int bif = chunk->code[i + 3] >> 4;

                int next_off = 4;
                term arg1;
                term arg2;
                DECODE_COMPACT_TERM(arg1, chunk->code, i, next_off, next_off)
                DECODE_COMPACT_TERM(arg2, chunk->code, i, next_off, next_off)
                int dreg = chunk->code[i + next_off] >> 4;

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("gc_bif2/6 fail_lbl=%i, live=%i, bif=%i, arg1=0x%lx, arg2=0x%lx, dest=r%i\n", f_label, live, bif, arg1, arg2, dreg);

                    BifImpl2 func = (BifImpl2) mod->imported_funcs[bif].bif;
                    term ret = func(ctx, f_label, live, arg1, arg2);

                    int reg_b_type = reg_type_c(chunk->code[i + next_off] & 0xF);
                    if (reg_b_type == 'x') {
                        ctx->x[dreg] = ret;
                    } else if (reg_b_type == 'y') {
                        ctx->e[dreg] = ret;
                    } else {
                        printf("reg_b_type: %i\n", reg_b_type);
                        abort();
                    }
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

            case OP_TRIM: {
                int n_words = chunk->code[i + 1] >> 4;
                int n_remaining = chunk->code[i + 2] >> 4;

                TRACE("trim/2 words=%i, remaining=%i\n", n_words, n_remaining);
                USED_BY_TRACE(n_words);
                USED_BY_TRACE(n_remaining);

                #ifdef IMPL_EXECUTE_LOOP
                    DEBUG_DUMP_STACK(ctx);
                    ctx->e += n_words;
                    DEBUG_DUMP_STACK(ctx);
                #endif

                UNUSED(n_remaining)

                NEXT_INSTRUCTION(2);
                break;
            }

            case OP_LINE: {
                int next_offset = 1;

                int line_number = chunk->code[i + 1];
                if (line_number == 8) {
                    //TODO: line/1 doesn't look well documented, try to understand what it really means
                    next_offset++;
                }

                TRACE("line/1: %i\n", line_number);

                NEXT_INSTRUCTION(next_offset);
                break;
            }

            default:
                printf("Undecoded opcode: %i\n", chunk->code[i]);
                #ifdef IMPL_EXECUTE_LOOP
                    fprintf(stderr, "failed: %li\n", &chunk->code[i] - beam_file);
                #endif

                abort();
                return 1;
        }
    }
}

#pragma GCC diagnostic pop

#undef DECODE_COMPACT_TERM
