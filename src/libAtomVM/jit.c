/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

#include "jit.h"

#ifndef AVM_NO_JIT

#include "bif.h"
#include "bitstring.h"
#include "context.h"
#include "debug.h"
#include "defaultatoms.h"
#include "dist_nifs.h"
#include "module.h"
#include "nifs.h"
#include "scheduler.h"
#include "stacktrace.h"
#include "term.h"
#include "utils.h"

#include <math.h>
#include <stddef.h>

// #define ENABLE_TRACE
#include "trace.h"

// Verify matching atom index in defaultatoms.hrl
_Static_assert(ERROR_ATOM_INDEX == 3, "ERROR_ATOM_INDEX is 3 in jit/src/defaultatoms.hrl");
_Static_assert(BADARG_ATOM_INDEX == 5, "BADARG_ATOM_INDEX is 5 in jit/src/defaultatoms.hrl");
_Static_assert(BADARITH_ATOM_INDEX == 6, "BADARITH_ATOM_INDEX is 6 in jit/src/defaultatoms.hrl");
_Static_assert(BADFUN_ATOM_INDEX == 8, "BADFUN_ATOM_INDEX is 8 in jit/src/defaultatoms.hrl");
_Static_assert(FUNCTION_CLAUSE_ATOM_INDEX == 10, "FUNCTION_CLAUSE_ATOM_INDEX is 10 in jit/src/defaultatoms.hrl");
_Static_assert(TRY_CLAUSE_ATOM_INDEX == 11, "TRY_CLAUSE_ATOM_INDEX is 11 in jit/src/defaultatoms.hrl");
_Static_assert(BADMATCH_ATOM_INDEX == 31, "BADMATCH_ATOM_INDEX is 31 in jit/src/defaultatoms.hrl");
_Static_assert(CASE_CLAUSE_ATOM_INDEX == 32, "CASE_CLAUSE_ATOM_INDEX is 32 in jit/src/defaultatoms.hrl");
_Static_assert(IF_CLAUSE_ATOM_INDEX == 33, "IF_CLAUSE_ATOM_INDEX is 33 in jit/src/defaultatoms.hrl");
_Static_assert(THROW_ATOM_INDEX == 34, "THROW_ATOM_INDEX is 34 in jit/src/defaultatoms.hrl");
_Static_assert(UNSUPPORTED_ATOM_INDEX == 36, "UNSUPPORTED_ATOM_INDEX is 36 in jit/src/defaultatoms.hrl");
_Static_assert(ALL_ATOM_INDEX == 38, "ALL_ATOM_INDEX is 38 in jit/src/defaultatoms.hrl");
_Static_assert(LOWERCASE_EXIT_ATOM_INDEX == 58, "LOWERCASE_EXIT_ATOM_INDEX is 58 in jit/src/defaultatoms.hrl");
_Static_assert(BADRECORD_ATOM_INDEX == 77, "BADRECORD_ATOM_INDEX is 77 in jit/src/defaultatoms.hrl");

// Verify offsets in jit_x86_64.erl
#if JIT_ARCH_TARGET == JIT_ARCH_X86_64
_Static_assert(offsetof(Context, e) == 0x28, "ctx->e is 0x28 in jit/src/jit_x86_64.erl");
_Static_assert(offsetof(Context, x) == 0x30, "ctx->x is 0x30 in jit/src/jit_x86_64.erl");
_Static_assert(offsetof(Context, cp) == 0xB8, "ctx->cp is 0xB8 in jit/src/jit_x86_64.erl");
_Static_assert(offsetof(Context, fr) == 0xC0, "ctx->fr is 0xC0 in jit/src/jit_x86_64.erl");
_Static_assert(offsetof(Context, bs) == 0xC8, "ctx->bs is 0xC8 in jit/src/jit_x86_64.erl");
_Static_assert(offsetof(Context, bs_offset) == 0xD0, "ctx->bs_offset is 0xD0 in jit/src/jit_x86_64.erl");

_Static_assert(offsetof(JITState, module) == 0x0, "jit_state->module is 0x0 in jit/src/jit_x86_64.erl");
_Static_assert(offsetof(JITState, continuation) == 0x8, "jit_state->continuation is 0x8 in jit/src/jit_x86_64.erl");
_Static_assert(offsetof(JITState, remaining_reductions) == 0x10, "jit_state->remaining_reductions is 0x10 in jit/src/jit_x86_64.erl");
#endif

#define PROCESS_MAYBE_TRAP_RETURN_VALUE(return_value, offset)    \
    if (term_is_invalid_term(return_value)) {            \
        if (UNLIKELY(!context_get_flags(ctx, Trap))) {   \
            return jit_handle_error(ctx, jit_state, offset);  \
        } else {                                         \
            return jit_schedule_wait_cp(ctx, jit_state); \
        }                                                \
    }

#define PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(return_value, offset)                      \
    if (term_is_invalid_term(return_value)) {                                   \
        if (UNLIKELY(!context_get_flags(ctx, Trap))) {                          \
            return jit_handle_error(ctx, jit_state, offset);                         \
        } else {                                                                \
            return jit_schedule_wait_cp(jit_return(ctx, jit_state), jit_state); \
        }                                                                       \
    }

#ifndef MIN
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#endif

static void destroy_extended_registers(Context *ctx, unsigned int live)
{
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, &ctx->extended_x_regs) {
        struct ExtendedRegister *ext_reg = GET_LIST_ENTRY(item, struct ExtendedRegister, head);
        if (ext_reg->index >= live) {
            list_remove(item);
            free(ext_reg);
        }
    }
}

static void jit_trim_live_regs(Context *ctx, uint32_t live)
{
    if (UNLIKELY(!list_is_empty(&ctx->extended_x_regs))) {
        destroy_extended_registers(ctx, live);
    }
}

#define TRIM_LIVE_REGS(live_regs_no)        \
    jit_trim_live_regs(ctx, live_regs_no);  \
    if (UNLIKELY(live_regs_no > MAX_REG)) { \
        live_regs_no = MAX_REG;             \
    }

// Update jit_state->module and jit_state->continuation
static Context *jit_return(Context *ctx, JITState *jit_state)
{
    int module_index = ctx->cp >> 24;
    Module *mod = globalcontext_get_module_by_index(ctx->global, module_index);
    if (mod->native_code == NULL) {
        // return to emulated
        const uint8_t *code = mod->code->code;
        const uint8_t *pc = code + ((ctx->cp & 0xFFFFFF) >> 2);
        jit_state->continuation = pc;
    } else {
        // return to native
        const void *native_pc = ((const uint8_t *) mod->native_code) + ((ctx->cp & 0xFFFFFF) >> 2);
        jit_state->continuation = native_pc;
    }
    jit_state->module = mod;
    return ctx;
}

static Context *jit_terminate_context(Context *ctx, JITState *jit_state)
{
    TRACE("-- Code execution finished for %i--\n", ctx->process_id);
    GlobalContext *global = ctx->global;
    if (ctx->leader) {
        scheduler_stop_all(global);
    }
    scheduler_terminate(ctx);
    jit_state->remaining_reductions = 0;
    return scheduler_run(global);
}

static Context *jit_handle_error(Context *ctx, JITState *jit_state, int offset)
{
    if (offset || term_is_invalid_term(ctx->x[2])) {
        ctx->x[2] = stacktrace_create_raw(ctx, jit_state->module, offset, ctx->x[0]);
    }
    int target_label = context_get_catch_label(ctx, &jit_state->module);
    if (target_label) {
        if (jit_state->module->native_code) {
            // catch label is in native code.
            jit_state->continuation = module_get_native_entry_point(jit_state->module, target_label);
        } else {
            jit_state->continuation = jit_state->module->labels[target_label];
        }
        return ctx;
    }

    // Do not print crash dump if reason is normal or shutdown.
    if (ctx->x[0] != LOWERCASE_EXIT_ATOM || (ctx->x[1] != NORMAL_ATOM && ctx->x[1] != SHUTDOWN_ATOM)) {
        context_dump(ctx);
    }

    if (ctx->x[0] == LOWERCASE_EXIT_ATOM) {
        ctx->exit_reason = ctx->x[1];
    } else {
        bool throw = ctx->x[0] == THROW_ATOM;

        int exit_reason_tuple_size = (throw ? TUPLE_SIZE(2) : 0) + TUPLE_SIZE(2);
        if (memory_ensure_free_with_roots(ctx, exit_reason_tuple_size, 1, ctx->x + 1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
            ctx->exit_reason = OUT_OF_MEMORY_ATOM;
        } else {
            term error_term;
            if (throw) {
                error_term = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(error_term, 0, NOCATCH_ATOM);
                term_put_tuple_element(error_term, 1, ctx->x[1]);
            } else {
                // error
                error_term = ctx->x[1];
            }

            term exit_reason_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(exit_reason_tuple, 0, error_term);
            term_put_tuple_element(exit_reason_tuple, 1, term_nil());
            ctx->exit_reason = exit_reason_tuple;
        }
    }

    return jit_terminate_context(ctx, jit_state);
}

static void set_error(Context *ctx, JITState *jit_state, int offset, term error_term)
{
    ctx->x[0] = ERROR_ATOM;
    ctx->x[1] = error_term;
    if (offset) {
        ctx->x[2] = stacktrace_create_raw(ctx, jit_state->module, offset, ERROR_ATOM);
    } else {
        ctx->x[2] = term_invalid_term();
    }
}

static Context *jit_raise_error(Context *ctx, JITState *jit_state, int offset, term error_type_atom)
{
    set_error(ctx, jit_state, offset, error_type_atom);
    return jit_handle_error(ctx, jit_state, 0);
}

static Context *jit_raise_error_tuple(Context *ctx, JITState *jit_state, int offset, term error_atom, term arg1)
{
    // We can gc as we are raising
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &arg1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        set_error(ctx, jit_state, offset, OUT_OF_MEMORY_ATOM);
        return jit_handle_error(ctx, jit_state, 0);
    }

    term new_error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(new_error_tuple, 0, error_atom);
    term_put_tuple_element(new_error_tuple, 1, arg1);

    set_error(ctx, jit_state, offset, new_error_tuple);
    return jit_handle_error(ctx, jit_state, 0);
}

static Context *jit_raise(Context *ctx, JITState *jit_state, int offset, term stacktrace, term exc_value)
{
    ctx->x[0] = stacktrace_exception_class(stacktrace);
    ctx->x[1] = exc_value;
    ctx->x[2] = stacktrace_create_raw(ctx, jit_state->module, offset, stacktrace);
    return jit_handle_error(ctx, jit_state, 0);
}

static Context *jit_schedule_next_cp(Context *ctx, JITState *jit_state)
{
    ctx->saved_ip = jit_state->continuation;
    ctx->saved_module = jit_state->module;
    jit_state->remaining_reductions = 0;
    return scheduler_next(ctx->global, ctx);
}

static Context *jit_schedule_wait_cp(Context *ctx, JITState *jit_state)
{
    ctx->saved_ip = jit_state->continuation;
    ctx->saved_module = jit_state->module;
    jit_state->remaining_reductions = 0;
    return scheduler_wait(ctx);
}

static Context *jit_call_ext(Context *ctx, JITState *jit_state, int offset, int arity, int index, int n_words)
{
    const struct ExportedFunction *func = module_resolve_function(jit_state->module, index, ctx->global);
    if (IS_NULL_PTR(func)) {
        return jit_raise_error(ctx, jit_state, 0, UNDEF_ATOM);
    }

    switch (func->type) {
        case NIFFunctionType: {
            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
            term return_value = nif->nif_ptr(ctx, arity, ctx->x);
            PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(return_value, offset);
            ctx->x[0] = return_value;

            // We deallocate after (instead of before) as a
            // workaround for issue
            // https://github.com/erlang/otp/issues/7152
            if (n_words >= 0) {
                ctx->cp = ctx->e[n_words];
                ctx->e += (n_words + 1);
            }

            if (ctx->heap.root->next) {
                if (UNLIKELY(memory_ensure_free_with_roots(ctx, 0, 1, ctx->x, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
                    return jit_raise_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
                }
            }
            if ((long) ctx->cp == -1) {
                return 0;
            }

            return jit_return(ctx, jit_state);
        }
        case ModuleFunction: {
            // In the non-nif case, we can deallocate before
            // (and it doesn't matter as the code below does
            // not access ctx->e or ctx->cp)

            if (n_words >= 0) {
                ctx->cp = ctx->e[n_words];
                ctx->e += (n_words + 1);
            }

            const struct ModuleFunction *jump = EXPORTED_FUNCTION_TO_MODULE_FUNCTION(func);
            // return to emulated
            jit_state->module = jump->target;
            jit_state->continuation = jit_state->module->labels[jump->label];
            break;
        }
        case ModuleNativeFunction: {
            if (n_words >= 0) {
                ctx->cp = ctx->e[n_words];
                ctx->e += (n_words + 1);
            }

            const struct ModuleNativeFunction *jump = EXPORTED_FUNCTION_TO_MODULE_NATIVE_FUNCTION(func);
            // clang cannot tail-optimize this, so return to loop to avoid any stack overflow
            // __attribute__((musttail)) return jump->entry_point(ctx, jit_state, &module_native_interface);
            jit_state->module = jump->target;
            jit_state->continuation = jump->entry_point;
            break;
        }
        case BIFFunctionType: {
            // Support compilers < OTP26 that generate CALL_EXT_ONLY or CALL_EXT_LAST
            // for min/2 and max/2
            // These are safe regarding otp issue #7152
            if (n_words >= 0) {
                ctx->cp = ctx->e[n_words];
                ctx->e += (n_words + 1);
            }

            const struct Bif *bif = EXPORTED_FUNCTION_TO_BIF(func);
            term return_value;
            switch (arity) {
                case 0:
                    return_value = bif->bif0_ptr(ctx);
                    break;
                case 1:
                    return_value = bif->bif1_ptr(ctx, 0, ctx->x[0]);
                    break;
                case 2:
                    return_value = bif->bif2_ptr(ctx, 0, ctx->x[0], ctx->x[1]);
                    break;
                default:
                    fprintf(stderr, "Invalid arity %" PRIu32 " for bif\n", arity);
                    AVM_ABORT();
            }
            PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(return_value, offset);
            ctx->x[0] = return_value;

            return jit_return(ctx, jit_state);
        }
        case GCBIFFunctionType: {
            // Support compilers < OTP28 that generate CALL_EXT_ONLY or CALL_EXT_LAST
            // for binary_to_(existing_)atom/1,2 and list_to_(existing_)atom/1
            // functions.
            // Regular CALL_EXT_ONLYs to those functions are generated as well
            // even on OTP28, so it is required to allow calling them using
            // CALL_EXT_ONLY even on OTP28: BIFs are used for try ... catch.
            if (n_words >= 0) {
                ctx->cp = ctx->e[n_words];
                ctx->e += (n_words + 1);
            }

            const struct GCBif *gcbif = EXPORTED_FUNCTION_TO_GCBIF(func);
            term return_value;
            switch (arity) {
                case 1:
                    return_value = gcbif->gcbif1_ptr(ctx, 0, 0, ctx->x[0]);
                    break;
                case 2:
                    return_value = gcbif->gcbif2_ptr(ctx, 0, 0, ctx->x[0], ctx->x[1]);
                    break;
                default:
                    fprintf(stderr, "Invalid arity %" PRIu32 " for bif\n", arity);
                    AVM_ABORT();
            }
            PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(return_value, offset);
            ctx->x[0] = return_value;

            return jit_return(ctx, jit_state);
        }
        default: {
            AVM_ABORT();
        }
    }
    return ctx;
}

static term jit_module_get_atom_term_by_id(JITState *jit_state, int atom_index)
{
    return module_get_atom_term_by_id(jit_state->module, atom_index);
}

static bool jit_allocate(Context *ctx, JITState *jit_state, uint32_t stack_need, uint32_t heap_need, uint32_t live)
{
    if (ctx->heap.root->next || ((ctx->heap.heap_ptr + heap_need > ctx->e - (stack_need + 1)))) {
        TRIM_LIVE_REGS(live);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, heap_need + stack_need + 1, live, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
            return false;
        }
    }
    ctx->e -= stack_need + 1;
    ctx->e[stack_need] = ctx->cp;
    return true;
}

static void *jit_get_imported_bif(JITState *jit_state, uint32_t bif)
{
    const struct ExportedFunction *exported_bif = jit_state->module->imported_funcs[bif];
    void *result = EXPORTED_FUNCTION_TO_BIF(exported_bif)->bif0_ptr;
    return result;
}

static bool jit_deallocate(Context *ctx, JITState *jit_state, uint32_t n_words)
{
    ctx->cp = ctx->e[n_words];
    ctx->e += n_words + 1;
    // Hopefully, we only need x[0]
    if (ctx->heap.root->next) {
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, 0, 1, ctx->x, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
            set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
            return false;
        }
    }
    return true;
}

static TermCompareResult jit_term_compare(Context *ctx, JITState *jit_state, term t, term other, TermCompareOpts opts)
{
    TermCompareResult result = term_compare(t, other, opts, ctx->global);
    if (UNLIKELY(result == 0)) {
        set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
    }
    return result;
}

static bool jit_test_heap(Context *ctx, JITState *jit_state, uint32_t heap_need, uint32_t live_registers)
{
    size_t heap_free = context_avail_free_memory(ctx);
    // if we need more heap space than is currently free, then try to GC the needed space
    if (heap_free < heap_need) {
        TRIM_LIVE_REGS(live_registers);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, heap_need, live_registers, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
            return false;
        }
        // otherwise, there is enough space for the needed heap, but there might
        // more more than necessary.  In that case, try to shrink the heap.
    } else if (heap_free > heap_need * HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF) {
        TRIM_LIVE_REGS(live_registers);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, heap_need * (HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF / 2), live_registers, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            TRACE("Unable to ensure free memory.  heap_need=%i\n", heap_need);
            set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
            return false;
        }
    }
    return true;
}

static term jit_put_list(Context *ctx, term head, term tail)
{
    term *list_elem = term_list_alloc(&ctx->heap);
    term t = term_list_init_prepend(list_elem, head, tail);
    return t;
}

static term jit_module_load_literal(Context *ctx, JITState *jit_state, int index)
{
    return module_load_literal(jit_state->module, index, ctx);
}

static term jit_alloc_boxed_integer_fragment(Context *ctx, avm_int64_t value)
{
#if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
    if ((value < AVM_INT_MIN) || (value > AVM_INT_MAX)) {
        Heap heap;
        if (UNLIKELY(memory_init_heap(&heap, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
            ctx->x[0] = ERROR_ATOM;
            ctx->x[1] = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        memory_heap_append_heap(&ctx->heap, &heap);

        return term_make_boxed_int64(value, &heap);
    }
#endif
    Heap heap;
    if (UNLIKELY(memory_init_heap(&heap, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
        ctx->x[0] = ERROR_ATOM;
        ctx->x[1] = OUT_OF_MEMORY_ATOM;
        return term_invalid_term();
    }
    memory_heap_append_heap(&ctx->heap, &heap);

    return term_make_boxed_int(value, &heap);
}

static term maybe_alloc_boxed_integer_fragment(Context *ctx, avm_int64_t value)
{
#if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
    if ((value < AVM_INT_MIN) || (value > AVM_INT_MAX)) {
        Heap heap;
        if (UNLIKELY(memory_init_heap(&heap, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
            ctx->x[0] = ERROR_ATOM;
            ctx->x[1] = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        memory_heap_append_heap(&ctx->heap, &heap);

        return term_make_boxed_int64(value, &heap);
    } else
#endif
        if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        Heap heap;
        if (UNLIKELY(memory_init_heap(&heap, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
            ctx->x[0] = ERROR_ATOM;
            ctx->x[1] = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        memory_heap_append_heap(&ctx->heap, &heap);

        return term_make_boxed_int(value, &heap);
    } else {
        return term_from_int(value);
    }
}

static term jit_term_alloc_tuple(Context *ctx, uint32_t size)
{
    return term_alloc_tuple(size, &ctx->heap);
}

static term jit_term_alloc_fun(Context *ctx, JITState *jit_state, uint32_t fun_index, uint32_t numfree)
{
    size_t size = numfree + BOXED_FUN_SIZE;
    term *boxed_func = memory_heap_alloc(&ctx->heap, size);

    boxed_func[0] = ((size - 1) << 6) | TERM_BOXED_FUN;
    boxed_func[1] = (term) jit_state->module;
    boxed_func[2] = term_from_int(fun_index);
    return ((term) boxed_func) | TERM_PRIMARY_BOXED;
}

static bool jit_send(Context *ctx, JITState *jit_state)
{
    term recipient_term = ctx->x[0];
    if (UNLIKELY(term_is_external_pid(recipient_term) || term_is_tuple(recipient_term))) {
        term return_value = dist_send_message(recipient_term, ctx->x[1], ctx);
        if (UNLIKELY(term_is_invalid_term(return_value))) {
            return false;
        }
        ctx->x[0] = return_value;
    } else {
        if (term_is_atom(recipient_term)) {
            recipient_term = globalcontext_get_registered_process(ctx->global, term_to_atom_index(recipient_term));
            if (UNLIKELY(recipient_term == UNDEFINED_ATOM)) {
                set_error(ctx, jit_state, 0, BADARG_ATOM);
                return false;
            }
        }

        int local_process_id;
        if (term_is_local_pid_or_port(recipient_term)) {
            local_process_id = term_to_local_process_id(recipient_term);
        } else {
            set_error(ctx, jit_state, 0, BADARG_ATOM);
            return false;
        }
        globalcontext_send_message(ctx->global, local_process_id, ctx->x[1]);
        ctx->x[0] = ctx->x[1];
    }
    return true;
}

static term *jit_extended_register_ptr(Context *ctx, unsigned int index)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->extended_x_regs) {
        struct ExtendedRegister *ext_reg = GET_LIST_ENTRY(item, struct ExtendedRegister, head);
        if (ext_reg->index == index) {
            return &ext_reg->value;
        }
    }
    struct ExtendedRegister *new_ext_reg = malloc(sizeof(struct ExtendedRegister));
    new_ext_reg->index = index;
    new_ext_reg->value = term_nil();
    list_append(&ctx->extended_x_regs, &new_ext_reg->head);

    return &new_ext_reg->value;
}

static Context *jit_process_signal_messages(Context *ctx, JITState *jit_state)
{
    MailboxMessage *signal_message = mailbox_process_outer_list(&ctx->mailbox);
    bool handle_error = false;
    bool reprocess_outer = false;
    while (signal_message) {
        switch (signal_message->type) {
            case KillSignal: {
                struct TermSignal *kill_signal
                    = CONTAINER_OF(signal_message, struct TermSignal, base);
                context_process_kill_signal(ctx, kill_signal);
                break;
            }
            case GCSignal: {
                if (UNLIKELY(memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
                    set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
                    handle_error = true;
                }
                break;
            }
            case ProcessInfoRequestSignal: {
                struct BuiltInAtomRequestSignal *request_signal
                    = CONTAINER_OF(signal_message, struct BuiltInAtomRequestSignal, base);
                context_process_process_info_request_signal(ctx, request_signal, false);
                break;
            }
            case TrapAnswerSignal: {
                struct TermSignal *trap_answer
                    = CONTAINER_OF(signal_message, struct TermSignal, base);
                if (UNLIKELY(!context_process_signal_trap_answer(ctx, trap_answer))) {
                    set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
                    handle_error = true;
                }
                break;
            }
            case TrapExceptionSignal: {
                struct ImmediateSignal *trap_exception
                    = CONTAINER_OF(signal_message, struct ImmediateSignal, base);
                set_error(ctx, jit_state, 0, trap_exception->immediate);
                handle_error = true;
                break;
            }
            case FlushMonitorSignal:
            case FlushInfoMonitorSignal: {
                struct RefSignal *flush_signal
                    = CONTAINER_OF(signal_message, struct RefSignal, base);
                bool info = signal_message->type == FlushInfoMonitorSignal;
                context_process_flush_monitor_signal(ctx, flush_signal->ref_ticks, info);
                break;
            }
            case SetGroupLeaderSignal: {
                struct TermSignal *group_leader
                    = CONTAINER_OF(signal_message, struct TermSignal, base);
                if (UNLIKELY(!context_process_signal_set_group_leader(ctx, group_leader))) {
                    set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
                    handle_error = true;
                }
                break;
            }
            case UnlinkIDSignal: {
                struct ImmediateRefSignal *immediate_ref_signal
                    = CONTAINER_OF(signal_message, struct ImmediateRefSignal, base);
                context_ack_unlink(ctx, immediate_ref_signal->immediate, immediate_ref_signal->ref_ticks, false);
                break;
            }
            case UnlinkIDAckSignal: {
                struct ImmediateRefSignal *immediate_ref_signal
                    = CONTAINER_OF(signal_message, struct ImmediateRefSignal, base);
                context_unlink_ack(ctx, immediate_ref_signal->immediate, immediate_ref_signal->ref_ticks);
                break;
            }
            case UnlinkRemoteIDSignal: {
                struct TermSignal *term_signal
                    = CONTAINER_OF(signal_message, struct TermSignal, base);
                uint64_t unlink_id = term_maybe_unbox_int64(term_get_tuple_element(term_signal->signal_term, 0));
                term remote_pid = term_get_tuple_element(term_signal->signal_term, 1);
                context_ack_unlink(ctx, remote_pid, unlink_id, false);
                break;
            }
            case UnlinkRemoteIDAckSignal: {
                struct TermSignal *term_signal
                    = CONTAINER_OF(signal_message, struct TermSignal, base);
                uint64_t unlink_id = term_maybe_unbox_int64(term_get_tuple_element(term_signal->signal_term, 0));
                term remote_pid = term_get_tuple_element(term_signal->signal_term, 1);
                context_unlink_ack(ctx, remote_pid, unlink_id);
                break;
            }
            case LinkExitSignal: {
                struct TermSignal *link_exit_signal
                    = CONTAINER_OF(signal_message, struct TermSignal, base);
                if (context_process_link_exit_signal(ctx, link_exit_signal)) {
                    reprocess_outer = true;
                }
                break;
            }
            case MonitorSignal: {
                struct MonitorPointerSignal *monitor_signal
                    = CONTAINER_OF(signal_message, struct MonitorPointerSignal, base);
                context_add_monitor(ctx, monitor_signal->monitor);
                break;
            }
            case DemonitorSignal: {
                struct RefSignal *ref_signal
                    = CONTAINER_OF(signal_message, struct RefSignal, base);
                context_demonitor(ctx, ref_signal->ref_ticks);
                break;
            }
            case MonitorDownSignal: {
                struct TermSignal *monitor_down_signal
                    = CONTAINER_OF(signal_message, struct TermSignal, base);
                context_process_monitor_down_signal(ctx, monitor_down_signal);
                reprocess_outer = true;
                break;
            }
            case NormalMessage: {
                UNREACHABLE();
            }
        }
        MailboxMessage *next = signal_message->next;
        mailbox_message_dispose(signal_message, &ctx->heap);
        signal_message = next;
        if (UNLIKELY(reprocess_outer && signal_message == NULL)) {
            reprocess_outer = false;
            signal_message = mailbox_process_outer_list(&ctx->mailbox);
        }
    }
    if (context_get_flags(ctx, Killed)) {
        return jit_terminate_context(ctx, jit_state);
    }
    if (handle_error) {
        return jit_handle_error(ctx, jit_state, 0);
    }
    if (context_get_flags(ctx, Trap)) {
        return jit_schedule_wait_cp(ctx, jit_state);
    }
    return ctx;
}

static term jit_mailbox_peek(Context *ctx)
{
    term out = term_invalid_term();
    mailbox_peek(ctx, &out);
    return out;
}

static void jit_mailbox_remove_message(Context *ctx)
{
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
}

static void jit_timeout(Context *ctx)
{
    context_update_flags(ctx, ~WaitingTimeoutExpired, NoFlags);
    mailbox_reset(&ctx->mailbox);
}

static void jit_mailbox_next(Context *ctx)
{
    mailbox_next(&ctx->mailbox);
}

static void jit_cancel_timeout(Context *ctx)
{
    if (context_get_flags(ctx, WaitingTimeout | WaitingTimeoutExpired)) {
        scheduler_cancel_timeout(ctx);
    }
}

static void jit_clear_timeout_flag(Context *ctx)
{
    context_update_flags(ctx, ~WaitingTimeoutExpired, NoFlags);
}

static Context *jit_wait_timeout(Context *ctx, JITState *jit_state, term timeout, int label)
{
    avm_int64_t t = 0;
    if (term_is_any_integer(timeout)) {
        t = term_maybe_unbox_int64(timeout);
        if (UNLIKELY(t < 0)) {
            return jit_raise_error(ctx, jit_state, 0, TIMEOUT_VALUE_ATOM);
        }
    } else if (UNLIKELY(timeout != INFINITY_ATOM)) {
        return jit_raise_error(ctx, jit_state, 0, TIMEOUT_VALUE_ATOM);
    }

    Context *r = jit_process_signal_messages(ctx, jit_state);
    if (r != ctx) {
        return r;
    }

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
        return jit_schedule_wait_cp(ctx, jit_state);
    } else {
        // clang cannot tail-optimize this, so return to loop to avoid any stack overflow
        // __attribute__((musttail)) return jit_state->continuation(ctx, jit_state, &module_native_interface);
        jit_state->continuation = module_get_native_entry_point(jit_state->module, label);
        return ctx;
    }
}

static Context *jit_wait_timeout_trap_handler(Context *ctx, JITState *jit_state, int label)
{
    if (UNLIKELY(!mailbox_has_next(&ctx->mailbox))) {
        // No message is here.
        // We were signaled for another reason.
        jit_state->remaining_reductions = 0; // force schedule_in
        return scheduler_wait(ctx);
    }

    jit_state->continuation = module_get_native_entry_point(jit_state->module, label);
    return ctx;
}

static bool maybe_call_native(Context *ctx, atom_index_t module_name, atom_index_t function_name, int arity,
    term *return_value)
{
    char mfa[MAX_MFA_NAME_LEN];
    atom_table_write_mfa(ctx->global->atom_table, mfa, sizeof(mfa), module_name, function_name, arity);
    const struct ExportedFunction *exported_bif = bif_registry_get_handler(mfa);
    if (exported_bif) {
        if (exported_bif->type == GCBIFFunctionType) {
            const struct GCBif *gcbif = EXPORTED_FUNCTION_TO_GCBIF(exported_bif);
            switch (arity) {
                case 1: {
                    *return_value = gcbif->gcbif1_ptr(ctx, 0, 0, ctx->x[0]);
                    return true;
                }
                case 2: {
                    *return_value = gcbif->gcbif2_ptr(ctx, 0, 0, ctx->x[0], ctx->x[1]);
                    return true;
                }
                case 3: {
                    *return_value = gcbif->gcbif3_ptr(ctx, 0, 0, ctx->x[0], ctx->x[1], ctx->x[2]);
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
                    *return_value = bif->bif1_ptr(ctx, 0, ctx->x[0]);
                    return true;
                }
                case 2: {
                    *return_value = bif->bif2_ptr(ctx, 0, ctx->x[0], ctx->x[1]);
                    return true;
                }
            }
        }
    }

    struct Nif *nif = (struct Nif *) nifs_get(mfa);
    if (nif) {
        *return_value = nif->nif_ptr(ctx, arity, ctx->x);
        return true;
    }

    return false;
}

#define MAXI(A, B) ((A > B) ? (A) : (B))
#define MINI(A, B) ((A > B) ? (B) : (A))

static Context *jit_call_fun(Context *ctx, JITState *jit_state, int offset, term fun, unsigned int args_count)
{
    Module *fun_module;
    unsigned int fun_arity;
    uint32_t n_freeze = 0;
    uint32_t label;
    const term *boxed_value = term_to_const_term_ptr(fun);
    term index_or_function = boxed_value[2];
    if (term_is_atom(index_or_function)) {
        term module = boxed_value[1];
        fun_arity = term_to_int(boxed_value[3]);
        atom_index_t module_name = term_to_atom_index(module);
        atom_index_t function_name = term_to_atom_index(index_or_function);
        term return_value;
        if (maybe_call_native(ctx, module_name, function_name, fun_arity, &return_value)) {
            PROCESS_MAYBE_TRAP_RETURN_VALUE(return_value, offset);
            ctx->x[0] = return_value;
            if (ctx->heap.root->next) {
                if (UNLIKELY(memory_ensure_free_with_roots(ctx, 0, 1, ctx->x, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
                    return jit_raise_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
                }
            }
            return jit_return(ctx, jit_state);
        } else {
            fun_module = globalcontext_get_module(ctx->global, module_name);
            if (IS_NULL_PTR(fun_module)) {
                set_error(ctx, jit_state, 0, UNDEF_ATOM);
                return jit_handle_error(ctx, jit_state, 0);
            }
            label = module_search_exported_function(fun_module, function_name, fun_arity);
            if (UNLIKELY(label == 0)) {
                set_error(ctx, jit_state, 0, UNDEF_ATOM);
                return jit_handle_error(ctx, jit_state, 0);
            }
        }
    } else {
        fun_module = (Module *) boxed_value[1];
        uint32_t fun_index = term_to_int(index_or_function);
        uint32_t fun_arity_and_freeze;
        module_get_fun(fun_module, fun_index, &label, &fun_arity_and_freeze, &n_freeze);
        fun_arity = fun_arity_and_freeze - n_freeze;
    }
    if (UNLIKELY(args_count != fun_arity)) {
        return jit_raise_error(ctx, jit_state, 0, BADARITY_ATOM);
    }
    uint32_t lim_freeze = MINI(fun_arity + n_freeze, MAX_REG);
    for (uint32_t i = fun_arity; i < lim_freeze; i++) {
        ctx->x[i] = boxed_value[i - fun_arity + 3];
    }
    uint32_t ext_fun_arity = MAXI(fun_arity, MAX_REG);
    for (uint32_t i = ext_fun_arity; i < fun_arity + n_freeze; i++) {
        term *ext_reg = jit_extended_register_ptr(ctx, i);
        *ext_reg = boxed_value[i - fun_arity + 3];
    }
    jit_state->module = fun_module;
    if (jit_state->module->native_code) {
        // catch label is in native code.
        jit_state->continuation = module_get_native_entry_point(jit_state->module, label);
    } else {
        jit_state->continuation = jit_state->module->labels[label];
    }
    return ctx;
}

static term jit_term_from_float(Context *ctx, avm_float_t f)
{
    return term_from_float(f, &ctx->heap);
}

static void jit_term_conv_to_float(Context *ctx, term t, int freg)
{
    ctx->fr[freg] = term_conv_to_float(t);
}

static bool jit_fadd(Context *ctx, int freg1, int freg2, int freg3)
{
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
#pragma STDC FENV_ACCESS ON
    feclearexcept(FE_OVERFLOW);
#endif
    ctx->fr[freg3] = ctx->fr[freg1] + ctx->fr[freg2];
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
    if (fetestexcept(FE_OVERFLOW)) {
        return false;
    }
#else
    if (!isfinite(ctx->fr[freg3])) {
        return false;
    }
#endif
    return true;
}

static bool jit_fsub(Context *ctx, int freg1, int freg2, int freg3)
{
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
#pragma STDC FENV_ACCESS ON
    feclearexcept(FE_OVERFLOW);
#endif
    ctx->fr[freg3] = ctx->fr[freg1] - ctx->fr[freg2];
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
    if (fetestexcept(FE_OVERFLOW)) {
        return false;
    }
#else
    if (!isfinite(ctx->fr[freg3])) {
        return false;
    }
#endif
    return true;
}

static bool jit_fmul(Context *ctx, int freg1, int freg2, int freg3)
{
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
#pragma STDC FENV_ACCESS ON
    feclearexcept(FE_OVERFLOW);
#endif
    ctx->fr[freg3] = ctx->fr[freg1] * ctx->fr[freg2];
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
    if (fetestexcept(FE_OVERFLOW)) {
        return false;
    }
#else
    if (!isfinite(ctx->fr[freg3])) {
        return false;
    }
#endif
    return true;
}

static bool jit_fdiv(Context *ctx, int freg1, int freg2, int freg3)
{
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
#pragma STDC FENV_ACCESS ON
    feclearexcept(FE_OVERFLOW | FE_DIVBYZERO);
#endif
    ctx->fr[freg3] = ctx->fr[freg1] / ctx->fr[freg2];
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
    if (fetestexcept(FE_OVERFLOW | FE_DIVBYZERO)) {
        return false;
    }
#else
    if (!isfinite(ctx->fr[freg3])) {
        return false;
    }
#endif
    return true;
}

static void jit_fnegate(Context *ctx, int freg1, int freg2)
{
    ctx->fr[freg2] = -ctx->fr[freg1];
}

static bool jit_catch_end(Context *ctx, JITState *jit_state)
{
    // C.f. https://www.erlang.org/doc/reference_manual/expressions.html#catch-and-throw
    switch (term_to_atom_index(ctx->x[0])) {
        case THROW_ATOM_INDEX:
            ctx->x[0] = ctx->x[1];
            break;

        case ERROR_ATOM_INDEX: {
            ctx->x[2] = stacktrace_build(ctx, &ctx->x[2], 3);
            // MEMORY_CAN_SHRINK because catch_end is classified as gc in beam_ssa_codegen.erl
            if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2) * 2, 2, ctx->x + 1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
                return false;
            }
            term reason_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(reason_tuple, 0, ctx->x[1]);
            term_put_tuple_element(reason_tuple, 1, ctx->x[2]);
            term exit_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(exit_tuple, 0, EXIT_ATOM);
            term_put_tuple_element(exit_tuple, 1, reason_tuple);
            ctx->x[0] = exit_tuple;

            break;
        }
        case LOWERCASE_EXIT_ATOM_INDEX: {
            // MEMORY_CAN_SHRINK because catch_end is classified as gc in beam_ssa_codegen.erl
            if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, ctx->x + 1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
                return false;
            }
            term exit_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(exit_tuple, 0, EXIT_ATOM);
            term_put_tuple_element(exit_tuple, 1, ctx->x[1]);
            ctx->x[0] = exit_tuple;

            break;
        }
    }
    return true;
}

static bool jit_memory_ensure_free_with_roots(Context *ctx, JITState *jit_state, int sz, int live, int flags)
{
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, sz, live, ctx->x, flags) != MEMORY_GC_OK)) {
        set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
        return false;
    }
    return true;
}

static term jit_term_alloc_bin_match_state(Context *ctx, term src, int slots)
{
    return term_alloc_bin_match_state(src, slots, &ctx->heap);
}

static term jit_bitstring_extract_integer(Context *ctx, JITState *jit_state, term *bin_ptr, size_t offset, int n, int bs_flags)
{
    union maybe_unsigned_int64 value;
    bool status = bitstring_extract_integer(((term) bin_ptr) | TERM_PRIMARY_BOXED, offset, n, bs_flags, &value);
    if (UNLIKELY(!status)) {
        return FALSE_ATOM;
    }
    term t = maybe_alloc_boxed_integer_fragment(ctx, value.s);
    if (UNLIKELY(term_is_invalid_term(t))) {
        set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
    }
    return t;
}

static term jit_bitstring_extract_float(Context *ctx, term *bin_ptr, size_t offset, int n, int bs_flags)
{
    avm_float_t value;
    bool status;
    switch (n) {
        case 32:
            status = bitstring_extract_f32(((term) bin_ptr) | TERM_PRIMARY_BOXED, offset, n, bs_flags, &value);
            break;
        case 64:
            status = bitstring_extract_f64(((term) bin_ptr) | TERM_PRIMARY_BOXED, offset, n, bs_flags, &value);
            break;
        default:
            status = false;
    }
    if (UNLIKELY(!status)) {
        return FALSE_ATOM;
    }
    return term_from_float(value, &ctx->heap);
}

static size_t jit_term_sub_binary_heap_size(term *bin_ptr, size_t size)
{
    term binary = ((term) bin_ptr) | TERM_PRIMARY_BOXED;
    return term_sub_binary_heap_size(binary, size);
}

static term jit_term_maybe_create_sub_binary(Context *ctx, term binary, size_t offset, size_t len)
{
    return term_maybe_create_sub_binary(binary, offset, len, &ctx->heap, ctx->global);
}

static int jit_term_find_map_pos(Context *ctx, term map, term key)
{
    return term_find_map_pos(map, key, ctx->global);
}

static int jit_bitstring_utf8_size(int c)
{
    size_t utf8_size;
    if (UNLIKELY(!bitstring_utf8_size(c, &utf8_size))) {
        return 0;
    }
    return utf8_size;
}

static int jit_bitstring_utf16_size(int c)
{
    size_t utf16_size;
    if (UNLIKELY(!bitstring_utf16_size(c, &utf16_size))) {
        return 0;
    }
    return utf16_size;
}

static term jit_term_create_empty_binary(Context *ctx, size_t len)
{
    return term_create_empty_binary(len, &ctx->heap, ctx->global);
}

static int jit_decode_flags_list(Context *ctx, JITState *jit_state, term flags)
{
    int flags_value = 0;
    while (term_is_nonempty_list(flags)) {
        switch (term_get_list_head(flags)) {
            case NATIVE_ATOM:
                flags_value |= NativeEndianInteger;
                break;
            case LITTLE_ATOM:
                flags_value |= LittleEndianInteger;
                break;
            case SIGNED_ATOM:
                flags_value |= SignedInteger;
                break;
            default:
                set_error(ctx, jit_state, 0, BADARG_ATOM);
                return -1;
        }
        flags = term_get_list_tail(flags);
    }
    if (UNLIKELY(!term_is_nil(flags))) {
        set_error(ctx, jit_state, 0, BADARG_ATOM);
        return -1;
    }
    return flags_value;
}

static int jit_bitstring_insert_utf8(term bin, size_t offset, int c)
{
    size_t byte_size;
    bool result = bitstring_insert_utf8(bin, offset, c, &byte_size);
    if (UNLIKELY(!result)) {
        return 0;
    }
    return byte_size;
}

static int jit_bitstring_insert_utf16(term bin, size_t offset, int c, enum BitstringFlags flags)
{
    size_t byte_size;
    bool result = bitstring_insert_utf16(bin, offset, c, flags, &byte_size);
    if (UNLIKELY(!result)) {
        return 0;
    }
    return byte_size;
}

static void jit_bitstring_copy_module_str(Context *ctx, JITState *jit_state, term bin, size_t offset, int str_id, size_t len)
{
    UNUSED(ctx);
    uint8_t *dst = (uint8_t *) term_binary_data(bin);
    size_t remaining = 0;
    const uint8_t *str = module_get_str(jit_state->module, str_id, &remaining);
    bitstring_copy_bits(dst, offset, str, len);
}

static int jit_bitstring_copy_binary(Context *ctx, JITState *jit_state, term t, size_t offset, term src, term size)
{
    if (offset % 8) {
        set_error(ctx, jit_state, 0, UNSUPPORTED_ATOM);
        return -1;
    }
    uint8_t *dst = (uint8_t *) term_binary_data(t) + (offset / 8);
    const uint8_t *bin = (const uint8_t *) term_binary_data(src);
    size_t binary_size = term_binary_size(src);
    if (size != ALL_ATOM) {
        binary_size = (size_t) term_to_int(size);
    }
    memcpy(dst, bin, binary_size);
    return binary_size * 8;
}

static Context *jit_apply(Context *ctx, JITState *jit_state, int offset, term module, term function, unsigned int arity)
{
    atom_index_t module_name = term_to_atom_index(module);
    atom_index_t function_name = term_to_atom_index(function);

    term native_return;
    if (maybe_call_native(ctx, module_name, function_name, arity, &native_return)) {
        PROCESS_MAYBE_TRAP_RETURN_VALUE(native_return, offset);
        ctx->x[0] = native_return;
        if (ctx->heap.root->next) {
            if (UNLIKELY(memory_ensure_free_with_roots(ctx, 0, 1, ctx->x, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
                return jit_raise_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
            }
        }
        return jit_return(ctx, jit_state);
    } else {
        Module *target_module = globalcontext_get_module(ctx->global, module_name);
        if (IS_NULL_PTR(target_module)) {
            set_error(ctx, jit_state, 0, UNDEF_ATOM);
            return jit_handle_error(ctx, jit_state, 0);
        }
        int target_label = module_search_exported_function(target_module, function_name, arity);
        if (target_label == 0) {
            set_error(ctx, jit_state, 0, UNDEF_ATOM);
            return jit_handle_error(ctx, jit_state, 0);
        }
        jit_state->module = target_module;
        if (jit_state->module->native_code) {
            // catch label is in native code.
            jit_state->continuation = module_get_native_entry_point(jit_state->module, target_label);
        } else {
            jit_state->continuation = jit_state->module->labels[target_label];
        }
        return ctx;
    }
}

static void *jit_malloc(Context *ctx, JITState *jit_state, size_t sz)
{
    void *ptr = malloc(sz);
    if (IS_NULL_PTR(ptr)) {
        set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
    }
    return ptr;
}

static bool sort_kv_pairs(term *kv, int size, GlobalContext *global)
{
    int k = size;
    while (1 < k) {
        int max_pos = 0;
        for (int i = 1; i < k; i++) {
            term t_max = kv[max_pos * 2];
            term t = kv[i * 2];
            // TODO: not sure if exact is the right choice here
            TermCompareResult result = term_compare(t, t_max, TermCompareExact, global);
            if (result == TermGreaterThan) {
                max_pos = i;
            } else if (UNLIKELY(result == TermCompareMemoryAllocFail)) {
                return false;
            }
        }
        if (max_pos != k - 1) {
            term i_key = kv[(k - 1) * 2];
            term i_val = kv[((k - 1) * 2) + 1];
            kv[(k - 1) * 2] = kv[max_pos * 2];
            kv[((k - 1) * 2) + 1] = kv[(max_pos * 2) + 1];
            kv[max_pos * 2] = i_key;
            kv[(max_pos * 2) + 1] = i_val;
        }
        k--;
        // kv[k..size] sorted
    }

    return true;
}

static term jit_put_map_assoc(Context *ctx, JITState *jit_state, term src, size_t new_entries, size_t num_elements, term *kv)
{
    size_t src_size = term_get_map_size(src);
    size_t new_map_size = src_size + new_entries;
    bool is_shared = new_entries == 0;
    //
    //
    //
    if (UNLIKELY(!sort_kv_pairs(kv, num_elements, ctx->global))) {
        set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
        return term_invalid_term();
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
            term new_key = kv[2 * kv_pos];
            term new_value = kv[(2 * kv_pos) + 1];
            term_set_map_assoc(map, j, new_key, new_value);
            kv_pos++;
        } else if (kv_pos >= num_elements) {
            term src_key = term_get_map_key(src, src_pos);
            term src_value = term_get_map_value(src, src_pos);
            term_set_map_assoc(map, j, src_key, src_value);
            src_pos++;
        } else {
            term src_key = term_get_map_key(src, src_pos);
            term new_key = kv[2 * kv_pos];
            // TODO: not sure if exact is the right choice here
            switch (term_compare(src_key, new_key, TermCompareExact, ctx->global)) {
                case TermLessThan: {
                    term src_value = term_get_map_value(src, src_pos);
                    term_set_map_assoc(map, j, src_key, src_value);
                    src_pos++;
                    break;
                }

                case TermGreaterThan: {
                    term new_value = kv[(2 * kv_pos) + 1];
                    term_set_map_assoc(map, j, new_key, new_value);
                    kv_pos++;
                    break;
                }

                case TermEquals: {
                    term new_value = kv[(2 * kv_pos) + 1];
                    term_set_map_assoc(map, j, src_key, new_value);
                    src_pos++;
                    kv_pos++;
                    break;
                }

                case TermCompareMemoryAllocFail: {
                    set_error(ctx, jit_state, 0, OUT_OF_MEMORY_ATOM);
                    return term_invalid_term();
                }
            }
        }
    }
    return map;
}

static int jit_module_get_fun_arity(Module *fun_module, uint32_t fun_index)
{
    uint32_t fun_label;
    uint32_t fun_arity_and_freeze;
    uint32_t fun_n_freeze;

    module_get_fun(fun_module, fun_index, &fun_label, &fun_arity_and_freeze, &fun_n_freeze);
    return fun_arity_and_freeze - fun_n_freeze;
}

static bool jit_bitstring_match_module_str(Context *ctx, JITState *jit_state, term bs_bin, size_t bs_offset, int str_id, size_t bits)
{
    UNUSED(ctx);
    size_t remaining = 0;
    const uint8_t *str = module_get_str(jit_state->module, str_id, &remaining);

    if (term_binary_size(bs_bin) * 8 - bs_offset < MIN(remaining * 8, bits)) {
        return false;
    }

    if (bits % 8 == 0 && bs_offset % 8 == 0) {
        avm_int_t bytes = bits / 8;
        avm_int_t byte_offset = bs_offset / 8;

        if (memcmp(term_binary_data(bs_bin) + byte_offset, str, MIN(remaining, (unsigned int) bytes)) != 0) {
            return false;
        }
        return true;
    }

    // Compare unaligned bits
    const uint8_t *bs_str = (const uint8_t *) term_binary_data(bs_bin) + (bs_offset / 8);
    uint8_t bin_bit_offset = 7 - (bs_offset - (8 *(bs_offset / 8)));
    uint8_t str_bit_offset = 7;
    size_t remaining_bits = bits;
    while (remaining_bits > 0) {
        uint8_t str_ch = *str;
        uint8_t bin_ch = *bs_str;
        uint8_t str_ch_bit = (str_ch >> str_bit_offset) & 1;
        uint8_t bin_ch_bit = (bin_ch >> bin_bit_offset) & 1;
        if (str_ch_bit ^ bin_ch_bit) {
            return false;
        }
        if (str_bit_offset) {
            str_bit_offset--;
        } else {
            str_bit_offset = 7;
            str++;
        }
        if (bin_bit_offset) {
            bin_bit_offset--;
        } else {
            bin_bit_offset = 7;
            bs_str++;
        }
        remaining_bits--;
    }
    return true;
}

static term jit_bitstring_get_utf8(term src)
{
    term src_bin = term_get_match_state_binary(src);
    avm_int_t offset_bits = term_get_match_state_offset(src);

    uint32_t val = 0;
    size_t out_size = 0;
    bool is_valid = bitstring_match_utf8(src_bin, (size_t) offset_bits, &val, &out_size);

    if (!is_valid) {
        return term_invalid_term();
    } else {
        term_set_match_state_offset(src, offset_bits + (out_size * 8));
        return term_from_int(val);
    }
}

static term jit_bitstring_get_utf16(term src, int flags_value)
{
    term src_bin = term_get_match_state_binary(src);
    avm_int_t offset_bits = term_get_match_state_offset(src);

    int32_t val = 0;
    size_t out_size = 0;
    bool is_valid = bitstring_match_utf16(src_bin, (size_t) offset_bits, &val, &out_size, flags_value);

    if (!is_valid) {
        return term_invalid_term();
    } else {
        term_set_match_state_offset(src, offset_bits + (out_size * 8));
        return term_from_int(val);
    }
}

static term jit_bitstring_get_utf32(term src, int flags_value)
{
    term src_bin = term_get_match_state_binary(src);
    avm_int_t offset_bits = term_get_match_state_offset(src);

    int32_t val = 0;
    bool is_valid = bitstring_match_utf32(src_bin, (size_t) offset_bits, &val, flags_value);

    if (!is_valid) {
        return term_invalid_term();
    } else {
        term_set_match_state_offset(src, offset_bits + 32);
        return term_from_int(val);
    }
}

static term term_copy_map(Context *ctx, term src)
{
    size_t src_size = term_get_map_size(src);
    term map = term_alloc_map_maybe_shared(src_size, term_get_map_keys(src), &ctx->heap);
    for (size_t j = 0;  j < src_size;  ++j) {
        term_set_map_assoc(map, j, term_get_map_key(src, j), term_get_map_value(src, j));
    }
    return map;
}

static term jit_stacktrace_build(Context *ctx)
{
    return stacktrace_build(ctx, ctx->x, 1);
}

const ModuleNativeInterface module_native_interface = {
    jit_raise_error,
    jit_return,
    jit_schedule_next_cp,
    jit_module_get_atom_term_by_id,
    jit_call_ext,
    jit_allocate,
    jit_handle_error,
    jit_trim_live_regs,
    jit_get_imported_bif,
    jit_deallocate,
    jit_terminate_context,
    jit_term_compare,
    jit_test_heap,
    jit_put_list,
    jit_module_load_literal,
    jit_alloc_boxed_integer_fragment,
    jit_term_alloc_tuple,
    jit_send,
    jit_extended_register_ptr,
    jit_raise_error_tuple,
    jit_term_alloc_fun,
    jit_process_signal_messages,
    jit_mailbox_peek,
    jit_mailbox_remove_message,
    jit_timeout,
    jit_mailbox_next,
    jit_cancel_timeout,
    jit_clear_timeout_flag,
    jit_raise,
    jit_schedule_wait_cp,
    jit_wait_timeout,
    jit_wait_timeout_trap_handler,
    jit_call_fun,
    context_get_flags,
    context_ensure_fpregs,
    jit_term_from_float,
    term_is_number,
    jit_term_conv_to_float,
    jit_fadd,
    jit_fsub,
    jit_fmul,
    jit_fdiv,
    jit_fnegate,
    jit_catch_end,
    jit_memory_ensure_free_with_roots,
    jit_term_alloc_bin_match_state,
    jit_bitstring_extract_integer,
    jit_term_sub_binary_heap_size,
    jit_term_maybe_create_sub_binary,
    jit_term_find_map_pos,
    jit_bitstring_utf8_size,
    jit_bitstring_utf16_size,
    jit_term_create_empty_binary,
    jit_decode_flags_list,
    jit_bitstring_insert_utf8,
    jit_bitstring_insert_utf16,
    bitstring_insert_utf32,
    bitstring_insert_integer,
    term_maybe_unbox_int64,
    jit_bitstring_copy_module_str,
    jit_bitstring_copy_binary,
    jit_apply,
    jit_malloc,
    free,
    jit_put_map_assoc,
    jit_bitstring_extract_float,
    jit_module_get_fun_arity,
    jit_bitstring_match_module_str,
    jit_bitstring_get_utf8,
    jit_bitstring_get_utf16,
    jit_bitstring_get_utf32,
    term_copy_map,
    jit_stacktrace_build
};

#endif
