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

#include "bif.h"
#include "context.h"
#include "debug.h"
#include "defaultatoms.h"
#include "dist_nifs.h"
#include "module.h"
#include "nifs.h"
#include "scheduler.h"
#include "stacktrace.h"
#include "utils.h"

#include <math.h>

// #define ENABLE_TRACE
#include "trace.h"

// Verify matching atom index in defaultatoms.hrl
_Static_assert(BADARITH_ATOM_INDEX == 6, "BADARITH_ATOM_INDEX is 6 in jit/src/defaultatoms.hrl");
_Static_assert(BADFUN_ATOM_INDEX == 8, "BADFUN_ATOM_INDEX is 8 in jit/src/defaultatoms.hrl");
_Static_assert(FUNCTION_CLAUSE_ATOM_INDEX == 10, "FUNCTION_CLAUSE_ATOM_INDEX is 10 in jit/src/defaultatoms.hrl");
_Static_assert(BADMATCH_ATOM_INDEX == 31, "BADMATCH_ATOM_INDEX is 31 in jit/src/defaultatoms.hrl");
_Static_assert(CASE_CLAUSE_ATOM_INDEX == 32, "CASE_CLAUSE_ATOM_INDEX is 32 in jit/src/defaultatoms.hrl");
_Static_assert(IF_CLAUSE_ATOM_INDEX == 33, "IF_CLAUSE_ATOM_INDEX is 33 in jit/src/defaultatoms.hrl");

#define HANDLE_ERROR()                                          \
    ctx->x[2] = stacktrace_create_raw(ctx, jit_state->module, 0, ctx->x[0]);  \
    return jit_handle_error(ctx, jit_state);

#define PROCESS_MAYBE_TRAP_RETURN_VALUE(return_value)           \
    if (term_is_invalid_term(return_value)) {                   \
        if (UNLIKELY(!context_get_flags(ctx, Trap))) {          \
            HANDLE_ERROR();                                     \
        } else {                                                \
            return jit_schedule_wait_cp(ctx, jit_state);        \
        }                                                       \
    }

#define PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(return_value)      \
    if (term_is_invalid_term(return_value)) {                   \
        if (UNLIKELY(!context_get_flags(ctx, Trap))) {          \
            HANDLE_ERROR();                                     \
        } else {                                                \
            return jit_schedule_wait_cp(jit_return(ctx, jit_state), jit_state);        \
        }                                                       \
    }

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

static Context *jit_handle_error(Context *ctx, JITState *jit_state)
{
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

static void set_error(Context *ctx, JITState *jit_state, term error_term)
{
    ctx->x[0] = ERROR_ATOM;
    ctx->x[1] = error_term;
    ctx->x[2] = stacktrace_create_raw(ctx, jit_state->module, 0, ERROR_ATOM);
}

static Context *jit_raise_error(Context *ctx, JITState *jit_state, term error_type_atom)
{
    set_error(ctx, jit_state, error_type_atom);
    return jit_handle_error(ctx, jit_state);
}

static Context *jit_raise_error_tuple(Context *ctx, JITState *jit_state, term error_atom, term arg1)
{
    // We can gc as we are raising
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &arg1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        set_error(ctx, jit_state, OUT_OF_MEMORY_ATOM);
        return jit_handle_error(ctx, jit_state);
    }

    term new_error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(new_error_tuple, 0, error_atom);
    term_put_tuple_element(new_error_tuple, 1, arg1);

    set_error(ctx, jit_state, new_error_tuple);
    return jit_handle_error(ctx, jit_state);
}

static Context *jit_raise(Context *ctx, JITState *jit_state, term stacktrace, term exc_value)
{
    ctx->x[0] = stacktrace_exception_class(stacktrace);
    ctx->x[1] = exc_value;
    ctx->x[2] = stacktrace_create_raw(ctx, jit_state->module, 0, stacktrace);
    return jit_handle_error(ctx, jit_state);
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

static Context *jit_call_ext(Context *ctx, JITState *jit_state, int arity, int index, int n_words)
{
    const struct ExportedFunction *func = module_resolve_function(jit_state->module, index, ctx->global);
    if (IS_NULL_PTR(func)) {
        return jit_raise_error(ctx, jit_state, UNDEF_ATOM);
    }

    switch (func->type) {
        case NIFFunctionType: {
            const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(func);
            term return_value = nif->nif_ptr(ctx, arity, ctx->x);
            PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(return_value);
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
                    return jit_raise_error(ctx, jit_state, OUT_OF_MEMORY_ATOM);
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
            PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(return_value);
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
            PROCESS_MAYBE_TRAP_RETURN_VALUE_LAST(return_value);
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
            set_error(ctx, jit_state, OUT_OF_MEMORY_ATOM);
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
            set_error(ctx, jit_state, OUT_OF_MEMORY_ATOM);
            return false;
        }
    }
    return true;
}

static TermCompareResult jit_term_compare(Context *ctx, JITState *jit_state, term t, term other, TermCompareOpts opts)
{
    TermCompareResult result = term_compare(t, other, opts, ctx->global);
    if (UNLIKELY(result == 0)) {
        set_error(ctx, jit_state, OUT_OF_MEMORY_ATOM);
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
            set_error(ctx, jit_state, OUT_OF_MEMORY_ATOM);
            return false;
        }
    // otherwise, there is enough space for the needed heap, but there might
    // more more than necessary.  In that case, try to shrink the heap.
    } else if (heap_free > heap_need * HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF) {
        TRIM_LIVE_REGS(live_registers);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, heap_need * (HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF / 2), live_registers, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            TRACE("Unable to ensure free memory.  heap_need=%i\n", heap_need);
            set_error(ctx, jit_state, OUT_OF_MEMORY_ATOM);
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
                set_error(ctx, jit_state, BADARG_ATOM);
                return false;
            }
        }

        int local_process_id;
        if (term_is_local_pid_or_port(recipient_term)) {
            local_process_id = term_to_local_process_id(recipient_term);
        } else {
            set_error(ctx, jit_state, BADARG_ATOM);
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
                    set_error(ctx, jit_state, OUT_OF_MEMORY_ATOM);
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
                    set_error(ctx, jit_state, OUT_OF_MEMORY_ATOM);
                    handle_error = true;
                }
                break;
            }
            case TrapExceptionSignal: {
                struct ImmediateSignal *trap_exception
                    = CONTAINER_OF(signal_message, struct ImmediateSignal, base);
                set_error(ctx, jit_state, trap_exception->immediate);
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
                    set_error(ctx, jit_state, OUT_OF_MEMORY_ATOM);
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
        return jit_handle_error(ctx, jit_state);
    }
    if (context_get_flags(ctx, Trap)) {
        return jit_schedule_wait_cp(ctx, jit_state);
    }
    return ctx;
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
            return jit_raise_error(ctx, jit_state, TIMEOUT_VALUE_ATOM);
        }
    } else if (UNLIKELY(timeout != INFINITY_ATOM)) {
        return jit_raise_error(ctx, jit_state, TIMEOUT_VALUE_ATOM);
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

static Context *jit_call_fun(Context *ctx, JITState *jit_state, term fun, unsigned int args_count)
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
            PROCESS_MAYBE_TRAP_RETURN_VALUE(return_value);
            ctx->x[0] = return_value;
            if (ctx->heap.root->next) {
                if (UNLIKELY(memory_ensure_free_with_roots(ctx, 0, 1, ctx->x, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
                    return jit_raise_error(ctx, jit_state, OUT_OF_MEMORY_ATOM);
                }
            }
            return jit_return(ctx, jit_state);
        } else {
            fun_module = globalcontext_get_module(ctx->global, module_name);
            if (IS_NULL_PTR(fun_module)) {
                set_error(ctx, jit_state, UNDEF_ATOM);
                HANDLE_ERROR();
            }
            label = module_search_exported_function(fun_module, function_name, fun_arity);
            if (UNLIKELY(label == 0)) {
                set_error(ctx, jit_state, UNDEF_ATOM);
                HANDLE_ERROR();
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
        return jit_raise_error(ctx, jit_state, BADARITY_ATOM);
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

term jit_term_from_float(Context *ctx, avm_float_t f)
{
printf("jit_term_from_float -- f = %f\n", f);
    return term_from_float(f, &ctx->heap);
}

void jit_term_conv_to_float(Context *ctx, term t, int freg)
{
    ctx->fr[freg] = term_conv_to_float(t);
}

bool jit_fadd(Context *ctx, int freg1, int freg2, int freg3)
{
printf("jit_fadd -- freg1 = %d, freg2 = %d, freg3 = %d\n", freg1, freg2, freg3);
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
    mailbox_peek,
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
};
