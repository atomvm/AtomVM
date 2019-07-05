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
#include "defaultatoms.h"
#include "exportedfunction.h"
#include "utils.h"
#include "scheduler.h"
#include "nifs.h"
#include "opcodes.h"

#ifdef IMPL_EXECUTE_LOOP
    #include "mailbox.h"
#endif

#define ENABLE_OTP21

//#define ENABLE_TRACE

#include "trace.h"

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
                case COMPACT_EXTENDED_LITERAL: {                                        \
                    uint8_t ext = (code_chunk[(base_index) + (off) + 1] & 0xF);         \
                    if (ext == 0) {                                                     \
                        next_operand_offset += 2;                                       \
                    }else if (ext == 0x8) {                                             \
                        next_operand_offset += 3;                                       \
                    } else {                                                            \
                        abort();                                                        \
                    }                                                                   \
                    break;                                                              \
                }                                                                       \
                default:                                                                \
                    printf("Unexpected %i\n", (int) first_byte);                        \
                    abort();                                                            \
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
                        abort();                                                                                        \
                    }                                                                                                   \
                                                                                                                        \
                    break;                                                                                              \
                }                                                                                                       \
                default:                                                                                                \
                    abort();                                                                                            \
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
                case COMPACT_NBITS_VALUE:                                                                               \
                    dest_term = term_from_int64(                                                                        \
                            large_integer_to_int64((code_chunk) + (base_index) + (off), &(next_operand_offset))         \
                        );                                                                                              \
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

#define DECODE_DEST_REGISTER(dreg, dreg_type, code_chunk, base_index, off, next_operand_offset)     \
{                                                                                                   \
    dreg_type = code_chunk[(base_index) + (off)] & 0xF;                                             \
    dreg = code_chunk[(base_index) + (off)] >> 4;                                                   \
    next_operand_offset++;                                                                          \
}

#define READ_REGISTER(sreg_type, sreg, value)                                                       \
{                                                                                                   \
    switch (sreg_type) {                                                                            \
        case 3:                                                                                     \
            value = ctx->x[sreg];                                                                   \
            break;                                                                                  \
        case 4:                                                                                     \
            value = ctx->e[sreg];                                                                   \
            break;                                                                                  \
        default:                                                                                    \
            abort();                                                                                \
    }                                                                                               \
}

#define WRITE_REGISTER(dreg_type, dreg, value)                                                      \
{                                                                                                   \
    switch (dreg_type) {                                                                            \
        case 3:                                                                                     \
            ctx->x[dreg] = value;                                                                   \
            break;                                                                                  \
        case 4:                                                                                     \
            ctx->e[dreg] = value;                                                                   \
            break;                                                                                  \
        default:                                                                                    \
            abort();                                                                                \
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
        mod = ctx->saved_module;                                                                  \
        code = mod->code->code;                                                                   \
        remaining_reductions = DEFAULT_REDUCTIONS_AMOUNT;                                         \
        JUMP_TO_ADDRESS(scheduled_context->saved_ip);                                             \
    }

#define INSTRUCTION_POINTER() \
    ((const void *) &code[i])

#define DO_RETURN() \
    mod = mod->global->modules_by_index[ctx->cp >> 24]; \
    code = mod->code->code; \
    i = (ctx->cp & 0xFFFFFF) >> 2;

#define POINTER_TO_II(instruction_pointer) \
    (((uint8_t *) (instruction_pointer)) - code)

#define RAISE_EXCEPTION() \
    int target_label = get_catch_label_and_change_module(ctx, &mod); \
    if (target_label) { \
        JUMP_TO_ADDRESS(mod->labels[target_label]); \
        break; \
    } else { \
        fprintf(stderr, "exception.\n"); \
        abort(); \
    }

#ifdef IMPL_EXECUTE_LOOP
struct Int24
{
    int32_t val24 : 24;
};

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

static int64_t large_integer_to_int64(uint8_t *compact_term, int *next_operand_offset)
{
    int num_bytes = (*compact_term >> 5) + 2;

    switch (num_bytes) {
        case 2: {
            *next_operand_offset += 3;
            int16_t ret_val16 = ((int16_t) compact_term[1]) << 8 | compact_term[2];
            return ret_val16;
        }

        case 3: {
            *next_operand_offset += 4;
            struct Int24 ret_val24;
            ret_val24.val24 = ((int32_t) compact_term[1]) << 16 | ((int32_t) compact_term[2] << 8) | compact_term[3];
            return ret_val24.val24;
        }

        case 4: {
            *next_operand_offset += 5;
            int32_t ret_val32;
            ret_val32 = ((int32_t) compact_term[1]) << 24 | ((int32_t) compact_term[2] << 16)
                | ((int32_t) compact_term[3] << 8) | compact_term[4];
            return ret_val32;
        }

        default:
            abort();
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
    boxed_func[2] = fun_index;

    for (uint32_t i = 3; i < n_freeze + 3; i++) {
        boxed_func[i] = ctx->x[i - 3];
    }

    return ((term) boxed_func) | TERM_BOXED_VALUE_TAG;
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

#ifdef IMPL_EXECUTE_LOOP
static const char *const error_atom = "\x5" "error";
static const char *const try_clause_atom = "\xA" "try_clause";
static const char *const out_of_memory_atom = "\xD" "out_of_memory";

#define RAISE_ERROR(error_type_atom)                                    \
    int target_label = get_catch_label_and_change_module(ctx, &mod);    \
    if (target_label) {                                                 \
        ctx->x[0] = context_make_atom(ctx, error_atom);                 \
        ctx->x[1] = context_make_atom(ctx, (error_type_atom));          \
        JUMP_TO_ADDRESS(mod->labels[target_label]);                     \
        continue;                                                       \
    } else {                                                            \
        abort();                                                        \
    }
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

    while(1) {

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
                    int target_label = get_catch_label_and_change_module(ctx, &mod);

                    if (target_label) {
                        ctx->x[0] = ERROR_ATOM;
                        ctx->x[1] = FUNCTION_CLAUSE_ATOM;
                        JUMP_TO_ADDRESS(mod->labels[target_label]);
                    } else {
                        fprintf(stderr, "FUNC_INFO: No function clause for module %i atom %i arity %i.\n", module_atom, function_name_atom, arity);
                        abort();
                    }

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
                TRACE("-- Code execution finished for %i--\n", ctx->process_id);
                if (schudule_processes_count(ctx->global) == 1) {
                    scheduler_terminate(ctx);
                    return 0;
                }

                TRACE("WARNING: some processes are still running.\n");

                Context *scheduled_context = scheduler_next(ctx->global, ctx);
                if (scheduled_context == ctx) {
                    TRACE("There are no more runnable processes\n");
                    return 0;
                }

                scheduler_terminate(ctx);

                ctx = scheduled_context;
                mod = ctx->saved_module;
                code = mod->code->code;
                remaining_reductions = DEFAULT_REDUCTIONS_AMOUNT;
                JUMP_TO_ADDRESS(scheduled_context->saved_ip);

                break;
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
                        func = module_resolve_function(mod, index);
                    }

                    switch (func->type) {
                        case NIFFunctionType: {
                            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
                            term return_value = nif->nif_ptr(ctx, arity, ctx->x);
                            if (UNLIKELY(term_is_invalid_term(return_value))) {
                                RAISE_EXCEPTION();
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
                            abort();
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
                        func = module_resolve_function(mod, index);
                    }

                    switch (func->type) {
                        case NIFFunctionType: {
                            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
                            term return_value = nif->nif_ptr(ctx, arity, ctx->x);
                            if (UNLIKELY(term_is_invalid_term(return_value))) {
                                RAISE_EXCEPTION();
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
                            abort();
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
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("bif0/2 bif=%i, dreg=%i\n", bif, dreg);
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
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("bif1/2 bif=%i, fail=%i, dreg=%i\n", bif, fail_label, dreg);
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
                        RAISE_EXCEPTION();
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
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("bif2/2 bif=%i, fail=%i, dreg=%i\n", bif, fail_label, dreg);
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
                        RAISE_EXCEPTION();
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
                        abort();
                    }

                    context_clean_registers(ctx, live);

                    if (ctx->heap_ptr > ctx->e - (stack_need + 1)) {
                        if (UNLIKELY(memory_ensure_free(ctx, stack_need + 1) != MEMORY_GC_OK)) {
                            RAISE_ERROR(out_of_memory_atom);
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
                        abort();
                    }

                    context_clean_registers(ctx, live);

                    if ((ctx->heap_ptr + heap_need) > ctx->e - (stack_need + 1)) {
                        if (UNLIKELY(memory_ensure_free(ctx, heap_need + stack_need + 1) != MEMORY_GC_OK)) {
                            RAISE_ERROR(out_of_memory_atom);
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
                        abort();
                    }

                    context_clean_registers(ctx, live);

                    if (ctx->heap_ptr > ctx->e - (stack_need + 1)) {
                        if (UNLIKELY(memory_ensure_free(ctx, stack_need + 1) != MEMORY_GC_OK)) {
                            RAISE_ERROR(out_of_memory_atom);
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
                        abort();
                    }

                    context_clean_registers(ctx, live);

                    if ((ctx->heap_ptr + heap_need) > ctx->e - (stack_need + 1)) {
                        if (UNLIKELY(memory_ensure_free(ctx, heap_need + stack_need + 1) != MEMORY_GC_OK)) {
                            RAISE_ERROR(out_of_memory_atom);
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
                    if (context_avail_free_memory(ctx) < heap_need) {
                        context_clean_registers(ctx, live_registers);
                        if (UNLIKELY(memory_ensure_free(ctx, heap_need) != MEMORY_GC_OK)) {
                            RAISE_ERROR(out_of_memory_atom);
                        }
                    } else if (context_avail_free_memory(ctx) > heap_need * HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF) {
                        context_clean_registers(ctx, live_registers);
                        int used_size = context_memory_size(ctx) - context_avail_free_memory(ctx);
                        if (UNLIKELY(memory_ensure_free(ctx, used_size + heap_need * (HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF / 2)) != MEMORY_GC_OK)) {
                            RAISE_ERROR(out_of_memory_atom);
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

            //TODO: implement remove_message/0
            case OP_REMOVE_MESSAGE: {
                TRACE("remove_message/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    mailbox_remove(ctx);
                #endif

                NEXT_INSTRUCTION(1);
                break;
            }

            //TODO: implement timeout/0
            case OP_TIMEOUT: {
                TRACE("timeout/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    ctx->timeout_at.tv_sec = 0;
                    ctx->timeout_at.tv_nsec = 0;
                #endif

                NEXT_INSTRUCTION(1);
                break;
            }

            //TODO: implemente loop_rec/2
            case OP_LOOP_REC: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("loop_rec/2, dreg=%i\n", dreg);
                USED_BY_TRACE(dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    if (ctx->mailbox == NULL) {
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

            //TODO: stub, implement loop_rec_end/1
            case OP_LOOP_REC_END: {
                int next_offset = 1;
                int label;
                DECODE_LABEL(label, code, i, next_offset, next_offset);

                TRACE("loop_rec_end/1 label=%i\n", label);
                USED_BY_TRACE(label);

                NEXT_INSTRUCTION(next_offset);
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
                    if (!context_is_waiting_timeout(ctx)) {
                        scheduler_set_timeout(ctx, term_to_int32(timeout));
                        needs_to_wait = 1;
                    } else if (!scheduler_is_timeout_expired(ctx)){
                        needs_to_wait = 1;
                    }

                    if (needs_to_wait) {
                        Context *scheduled_context = scheduler_wait(ctx->global, ctx);
                        ctx = scheduled_context;
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

                    if (arg1 < arg2) {
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

                    if (arg1 >= arg2) {
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
                    if (term_equals(arg1, arg2)) {
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

                    if (arg1 != arg2) {
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
                    if (term_exactly_equals(arg1, arg2)) {
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
                    if (arg1 != arg2) {
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

           case OP_IS_NUMBER: {
                int next_off = 1;
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("is_number/2, label=%i, arg1=%lx\n", label, arg1);

                    //TODO: check for floats too
                    if (term_is_any_integer(arg1)) {
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

                    if (term_is_binary(arg1) || term_is_nil(arg1)) {
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
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("move/2 %lx, %c%i\n", src_value, reg_type_c(dreg_type), dreg);

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
                int head_dreg;
                uint8_t head_dreg_type;
                DECODE_DEST_REGISTER(head_dreg, head_dreg_type, code, i, next_off, next_off);
                int tail_dreg;
                uint8_t tail_dreg_type;
                DECODE_DEST_REGISTER(tail_dreg, tail_dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("get_list/3 %lx, %c%i, %c%i\n", src_value, reg_type_c(head_dreg_type), head_dreg, reg_type_c(tail_dreg_type), tail_dreg);

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
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("get_tuple_element/2, element=%i, dest=%c%i\n", element, reg_type_c(dreg_type), dreg);
                USED_BY_TRACE(element);

                #ifdef IMPL_EXECUTE_LOOP
                    if (UNLIKELY(!term_is_tuple(src_value) || (element < 0) || (element >= term_get_tuple_arity(src_value)))) {
                        abort();
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
                    abort();
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
                int dreg;
                uint8_t dreg_type;
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
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("put_tuple/2 size=%i, dest=%c%i\n", size, reg_type_c(dreg_type), dreg);
                USED_BY_TRACE(dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    term t = term_alloc_tuple(size, ctx);
                    WRITE_REGISTER(dreg_type, dreg, t);

                    // DECODE_COMPACT_TERM might trigger GC
                    // make sure GC will not find uninitialized memory
                    for (int j = 0; j < size; j++) {
                        term_put_tuple_element(t, j, term_nil());
                    }
                #endif

                for (int j = 0; j < size; j++) {
                    if (code[i + next_off] != OP_PUT) {
                        fprintf(stderr, "Expected put, got opcode: %i\n", code[i + next_off]);
                        abort();
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

                        // DECODE_COMPACT_TERM might trigger GC, so t might be invalid
                        READ_REGISTER(dreg_type, dreg, t);
                        term_put_tuple_element(t, j, put_value);
                    #endif
                }

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_BADMATCH: {
                int next_off = 1;
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("badmatch/1\n");
                    USED_BY_TRACE(arg1);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("badmatch/1, v=0x%lx\n", arg1);

                    int target_label = get_catch_label_and_change_module(ctx, &mod);

                    if (target_label) {
                        JUMP_TO_ADDRESS(mod->labels[target_label]);
                    } else {
                        fprintf(stderr, "No target label for OP_BADMATCH.  arg1=0x%lx\n", arg1);
                        abort();
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(next_off);
                #endif

                break;
            }

            case OP_IF_END: {
                TRACE("if_end/0\n");

                #ifdef IMPL_EXECUTE_LOOP
                    int target_label = get_catch_label_and_change_module(ctx, &mod);

                    if (target_label) {
                        JUMP_TO_ADDRESS(mod->labels[target_label]);
                    } else {
                        fprintf(stderr, "No target label for OP_IF_END\n");
                        abort();
                    }
                #endif

                #ifdef IMPL_CODE_LOADER
                    NEXT_INSTRUCTION(1);
                #endif

                break;
            }

            case OP_CASE_END: {
                int next_off = 1;
                term arg1;
                DECODE_COMPACT_TERM(arg1, code, i, next_off, next_off)

                #ifdef IMPL_CODE_LOADER
                    TRACE("case_end/1\n");
                    USED_BY_TRACE(arg1);
                #endif

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("case_end/1, v=0x%lx\n", arg1);

                    int target_label = get_catch_label_and_change_module(ctx, &mod);

                    if (target_label) {
                        JUMP_TO_ADDRESS(mod->labels[target_label]);
                    } else {
                        fprintf(stderr, "No target label for OP_CASE_END.  arg1=0x%lx\n", arg1);
                        abort();
                    }
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
                    term fun = ctx->x[args_count];

                    if (UNLIKELY(!term_is_function(fun))) {
                        int target_label = get_catch_label_and_change_module(ctx, &mod);
                        if (target_label) {
                            ctx->x[0] = context_make_atom(ctx, error_atom);
                            term new_error_tuple = term_alloc_tuple(2, ctx);
                            term_put_tuple_element(new_error_tuple, 0, BADFUN_ATOM);
                            term_put_tuple_element(new_error_tuple, 1, ctx->x[args_count]);
                            ctx->x[1] = new_error_tuple;
                            JUMP_TO_ADDRESS(mod->labels[target_label]);
                            continue;

                        } else {
                            abort();
                        }
                    }

                    const term *boxed_value = term_to_const_term_ptr(fun);

                    Module *fun_module = (Module *) boxed_value[1];
                    uint32_t fun_index = boxed_value[2];

                    uint32_t label;
                    uint32_t arity;
                    uint32_t n_freeze;
                    module_get_fun(fun_module, fun_index, &label, &arity, &n_freeze);

                    TRACE_CALL(ctx, mod, "call_fun", label, args_count);

                    if (UNLIKELY(args_count != arity - n_freeze)) {
                        int target_label = get_catch_label_and_change_module(ctx, &mod);
                        if (target_label) {
                            ctx->x[0] = ERROR_ATOM;
                            ctx->x[1] = BADARITY_ATOM;
                            JUMP_TO_ADDRESS(mod->labels[target_label]);
                            continue;

                        } else {
                            abort();
                        }
                    }

                    for (unsigned int j = arity - n_freeze; j < arity + n_freeze; j++) {
                        ctx->x[j] = boxed_value[j - (arity - n_freeze) + 3];
                    }

                    NEXT_INSTRUCTION(next_off);
                    ctx->cp = module_address(mod->module_index, i);

                    mod = fun_module;
                    code = mod->code->code;

                    remaining_reductions--;
                    if (LIKELY(remaining_reductions)) {
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
                        func = module_resolve_function(mod, index);
                    }

                    switch (func->type) {
                        case NIFFunctionType: {
                            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
                            term return_value = nif->nif_ptr(ctx, arity, ctx->x);
                            if (UNLIKELY(term_is_invalid_term(return_value))) {
                                RAISE_EXCEPTION();
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
                            abort();
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
                        RAISE_ERROR(out_of_memory_atom);
                    } else {
                        ctx->x[0] = f;
                    }
                #endif

                NEXT_INSTRUCTION(next_off);
                break;

            }

            case OP_TRY: {
                int next_off = 1;
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);
                int label;
                DECODE_LABEL(label, code, i, next_off, next_off)

                TRACE("try/2, label=%i, reg=%c%i\n", label, reg_type_c(dreg_type), dreg);

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
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("try_end/1, reg=%c%i\n", reg_type_c(dreg_type), dreg);

                #ifdef IMPL_EXECUTE_LOOP
                    //TODO: here just write to y registers is enough
                    WRITE_REGISTER(dreg_type, dreg, term_nil());
                #endif

                NEXT_INSTRUCTION(next_off);
                break;
            }

            //TODO: implement
            case OP_TRY_CASE: {
                int next_off = 1;
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                TRACE("try_case/1, reg=%c%i\n", reg_type_c(dreg_type), dreg);

                NEXT_INSTRUCTION(next_off);
                break;
            }

            case OP_TRY_CASE_END: {
                #ifdef IMPL_EXECUTE_LOOP
                    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
                        RAISE_ERROR(out_of_memory_atom);
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

                    int target_label = get_catch_label_and_change_module(ctx, &mod);

                    if (target_label) {
                        term new_error_tuple = term_alloc_tuple(2, ctx);
                        term_put_tuple_element(new_error_tuple, 0, context_make_atom(ctx, try_clause_atom));
                        term_put_tuple_element(new_error_tuple, 1, arg1);
                        ctx->x[0] = context_make_atom(ctx, error_atom);
                        ctx->x[1] = new_error_tuple;
                        JUMP_TO_ADDRESS(mod->labels[target_label]);
                    } else {
                        abort();
                    }
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
                term function = ctx->x[arity+1];
                TRACE("apply/1, module=%lu, function=%lu arity=%i\n", module, function, arity);

                remaining_reductions--;
                if (UNLIKELY(!remaining_reductions)) {
                    SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                    continue;
                }
                NEXT_INSTRUCTION(next_off);

                AtomString module_name = globalcontext_atomstring_from_term(mod->global, module);
                AtomString function_name = globalcontext_atomstring_from_term(mod->global, function);

                TRACE_APPLY(ctx, "apply", module_name, function_name, arity);

                struct Nif *nif = (struct Nif *) nifs_get(module_name, function_name, arity);
                if (!IS_NULL_PTR(nif)) {
                    term return_value = nif->nif_ptr(ctx, arity, ctx->x);
                    if (UNLIKELY(term_is_invalid_term(return_value))) {
                        RAISE_EXCEPTION();
                    }
                    ctx->x[0] = return_value;
                } else {
                    Module *target_module = globalcontext_get_module(ctx->global, module_name);
                    if (IS_NULL_PTR(target_module)) {
                        RAISE_EXCEPTION();
                    }
                    int target_label = module_search_exported_function(target_module, function_name, arity);
                    if (target_label == 0) {
                        RAISE_EXCEPTION();
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
                term function = ctx->x[arity+1];
                TRACE("apply_last/1, module=%lu, function=%lu arity=%i deallocate=%i\n", module, function, arity, n_words);

                remaining_reductions--;
                if (UNLIKELY(!remaining_reductions)) {
                    SCHEDULE_NEXT(mod, INSTRUCTION_POINTER());
                    continue;
                }

                ctx->cp = ctx->e[n_words];
                ctx->e += (n_words + 1);

                AtomString module_name = globalcontext_atomstring_from_term(mod->global, module);
                AtomString function_name = globalcontext_atomstring_from_term(mod->global, function);

                TRACE_APPLY(ctx, "apply_last", module_name, function_name, arity);

                struct Nif *nif = (struct Nif *) nifs_get(module_name, function_name, arity);
                if (!IS_NULL_PTR(nif)) {
                    term return_value = nif->nif_ptr(ctx, arity, ctx->x);
                    if (UNLIKELY(term_is_invalid_term(return_value))) {
                        RAISE_EXCEPTION();
                    }
                    ctx->x[0] = return_value;
                    DO_RETURN();
                } else {
                    Module *target_module = globalcontext_get_module(ctx->global, module_name);
                    if (IS_NULL_PTR(target_module)) {
                        RAISE_EXCEPTION();
                    }
                    int target_label = module_search_exported_function(target_module, function_name, arity);
                    if (target_label == 0) {
                        RAISE_EXCEPTION();
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

                    static const char *const true_atom = "\x04" "true";
                    static const char *const false_atom = "\x05" "false";

                    term true_term = context_make_atom(ctx, true_atom);
                    term false_term = context_make_atom(ctx, false_atom);

                    if ((arg1 == true_term) || (arg1 == false_term)) {
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
                        uint32_t fun_index = boxed_value[2];

                        uint32_t fun_label;
                        uint32_t fun_arity;
                        uint32_t fun_n_freeze;

                        module_get_fun(fun_module, fun_index, &fun_label, &fun_arity, &fun_n_freeze);

                        if (arity == fun_arity - fun_n_freeze) {
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
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("gc_bif1/5 fail_lbl=%i, live=%i, bif=%i, arg1=0x%lx, dest=r%i\n", f_label, live, bif, arg1, dreg);

                    GCBifImpl1 func = (GCBifImpl1) mod->imported_funcs[bif].bif;
                    term ret = func(ctx, live, arg1);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        RAISE_EXCEPTION();
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
                int dreg;
                uint8_t dreg_type;
                DECODE_DEST_REGISTER(dreg, dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("gc_bif2/6 fail_lbl=%i, live=%i, bif=%i, arg1=0x%lx, arg2=0x%lx, dest=r%i\n", f_label, live, bif, arg1, arg2, dreg);

                    GCBifImpl2 func = (GCBifImpl2) mod->imported_funcs[bif].bif;
                    term ret = func(ctx, live, arg1, arg2);
                    if (UNLIKELY(term_is_invalid_term(ret))) {
                        RAISE_EXCEPTION();
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
                int head_dreg;
                uint8_t head_dreg_type;
                DECODE_DEST_REGISTER(head_dreg, head_dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("get_hd/2 %lx, %c%i\n", src_value, reg_type_c(head_dreg_type), head_dreg);

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
                int tail_dreg;
                uint8_t tail_dreg_type;
                DECODE_DEST_REGISTER(tail_dreg, tail_dreg_type, code, i, next_off, next_off);

                #ifdef IMPL_EXECUTE_LOOP
                    TRACE("get_tl/2 %lx, %c%i\n", src_value, reg_type_c(tail_dreg_type), tail_dreg);

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

            default:
                printf("Undecoded opcode: %i\n", code[i]);
                #ifdef IMPL_EXECUTE_LOOP
                    fprintf(stderr, "failed at %i\n", i);
                #endif

                abort();
                return 1;
        }
    }
}

#pragma GCC diagnostic pop

#undef DECODE_COMPACT_TERM
