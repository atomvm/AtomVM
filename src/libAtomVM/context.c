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

#include "context.h"

#include <fenv.h>
#include <math.h>

#include "defaultatoms.h"
#include "dictionary.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "globalcontext.h"
#include "list.h"
#include "mailbox.h"
#include "memory.h"
#include "smp.h"
#include "synclist.h"
#include "sys.h"
#include "term.h"
#include "utils.h"

#ifdef HAVE_PLATFORM_ATOMIC_H
#include "platform_atomic.h"
#else
#if defined(HAVE_ATOMIC)
#include <stdatomic.h>
#define ATOMIC_COMPARE_EXCHANGE_WEAK_INT atomic_compare_exchange_weak
#endif
#endif

#define IMPL_EXECUTE_LOOP
#include "opcodesswitch.h"
#undef IMPL_EXECUTE_LOOP

#define DEFAULT_STACK_SIZE 8
#define BYTES_PER_TERM (TERM_BITS / 8)

static struct Monitor *context_monitors_handle_terminate(Context *ctx);
static void context_distribution_handle_terminate(Context *ctx);
static void destroy_extended_registers(Context *ctx, unsigned int live);

Context *context_new(GlobalContext *glb)
{
    Context *ctx = malloc(sizeof(Context));
    if (IS_NULL_PTR(ctx)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }
    ctx->cp = 0;

    if (UNLIKELY(memory_init_heap(&ctx->heap, DEFAULT_STACK_SIZE) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        free(ctx);
        return NULL;
    }
    ctx->e = ctx->heap.heap_end;

    context_clean_registers(ctx, 0);
    list_init(&ctx->extended_x_regs);

    ctx->fr = NULL;

    ctx->min_heap_size = 0;
    ctx->max_heap_size = 0;
    ctx->heap_growth_strategy = BoundedFreeHeapGrowth;
    ctx->has_min_heap_size = 0;
    ctx->has_max_heap_size = 0;

    mailbox_init(&ctx->mailbox);

    list_init(&ctx->dictionary);

    ctx->native_handler = NULL;

    ctx->saved_module = NULL;
#ifndef AVM_NO_EMU
    ctx->saved_ip = NULL;
#else
    ctx->saved_function_ptr = NULL;
#endif
#ifndef AVM_NO_EMU
    ctx->waiting_with_timeout = false;
#endif

    ctx->leader = 0;

    timer_list_item_init(&ctx->timer_list_head, 0);

    list_init(&ctx->monitors_head);

    ctx->trap_exit = false;
#ifdef ENABLE_ADVANCED_TRACE
    ctx->trace_calls = 0;
    ctx->trace_call_args = 0;
    ctx->trace_returns = 0;
    ctx->trace_send = 0;
    ctx->trace_receive = 0;
#endif

    ctx->flags = NoFlags;
    ctx->platform_data = NULL;

    ctx->group_leader = term_from_local_process_id(INVALID_PROCESS_ID);

    ctx->bs = term_invalid_term();
    ctx->bs_offset = 0;

    ctx->exit_reason = NORMAL_ATOM;

    globalcontext_init_process(glb, ctx);

    return ctx;
}

void context_destroy(Context *ctx)
{
    // Another process can get an access to our mailbox until this point.
    struct ListHead *processes_table_list = synclist_wrlock(&ctx->global->processes_table);
    UNUSED(processes_table_list);

    list_remove(&ctx->processes_table_head);

    // Ensure process is not registered
    globalcontext_maybe_unregister_process_id(ctx->global, ctx->process_id);

    // Handle distribution termination
    if (UNLIKELY(ctx->flags & Distribution)) {
        context_distribution_handle_terminate(ctx);
    }

    // Process any link/unlink/monitor/demonitor signal that arrived recently
    // Also process ProcessInfoRequestSignal so caller isn't trapped waiting
    MailboxMessage *signal_message = mailbox_process_outer_list(&ctx->mailbox);
    while (signal_message) {
        switch (signal_message->type) {
            case ProcessInfoRequestSignal: {
                struct BuiltInAtomRequestSignal *request_signal
                    = CONTAINER_OF(signal_message, struct BuiltInAtomRequestSignal, base);
                context_process_process_info_request_signal(ctx, request_signal, true);
                break;
            }
            case SetGroupLeaderSignal: {
                struct TermSignal *group_leader
                    = CONTAINER_OF(signal_message, struct TermSignal, base);
                (void) context_process_signal_set_group_leader(ctx, group_leader);
                break;
            }
            case MonitorSignal: {
                struct MonitorPointerSignal *monitor_signal
                    = CONTAINER_OF(signal_message, struct MonitorPointerSignal, base);
                (void) context_add_monitor(ctx, monitor_signal->monitor);
                break;
            }
            case UnlinkIDSignal: {
                struct ImmediateRefSignal *immediate_ref_signal
                    = CONTAINER_OF(signal_message, struct ImmediateRefSignal, base);
                context_ack_unlink(ctx, immediate_ref_signal->immediate, immediate_ref_signal->ref_ticks, true);
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
                context_ack_unlink(ctx, remote_pid, unlink_id, true);
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
            case DemonitorSignal: {
                struct RefSignal *ref_signal
                    = CONTAINER_OF(signal_message, struct RefSignal, base);
                context_demonitor(ctx, ref_signal->ref_ticks);
                break;
            }
            case KillSignal: // we are already terminating
            case GCSignal: // we don't need to GC now
            case TrapAnswerSignal: // we don't need to process any trap answer now
            case TrapExceptionSignal: // likewise
            case FlushMonitorSignal:
            case FlushInfoMonitorSignal:
            case LinkExitSignal: // target will not be found when processing this link
            case MonitorDownSignal: // likewise
            case CodeServerResumeSignal:
                break;
            case NormalMessage: {
                UNREACHABLE();
            }
        }
        MailboxMessage *next = signal_message->next;
        mailbox_message_dispose(signal_message, &ctx->heap);
        signal_message = next;
    }

    // When monitor message is sent, process is no longer in the table
    // and is no longer registered either.
    struct Monitor *remaining_monitors = context_monitors_handle_terminate(ctx);

    synclist_unlock(&ctx->global->processes_table);

    // Eventually call distribution and resource monitors handlers after the processes table was unlocked
    // The monitors were removed from the list of monitors.
    if (remaining_monitors) {
        struct ListHead monitors;
        list_prepend(&remaining_monitors->monitor_list_head, &monitors);

        struct ListHead *item;
        struct ListHead *tmp;
        MUTABLE_LIST_FOR_EACH (item, tmp, &monitors) {
            struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
            switch (monitor->monitor_type) {
                case CONTEXT_MONITOR_RESOURCE: {
                    struct ResourceContextMonitor *resource_monitor = CONTAINER_OF(monitor, struct ResourceContextMonitor, monitor);
                    resource_type_fire_monitor(resource_monitor->resource_type, erl_nif_env_from_context(ctx), ctx->process_id, resource_monitor->ref_ticks);
                    free(monitor);
                    break;
                }
                case CONTEXT_MONITOR_LINK_REMOTE: {
                    struct LinkRemoteMonitor *link_monitor = CONTAINER_OF(monitor, struct LinkRemoteMonitor, monitor);
                    // Handle the case of inactive link.
                    if (link_monitor->unlink_id != UNLINK_ID_LINK_ACTIVE) {
                        free(monitor);
                        continue;
                    }
                    dist_send_payload_exit(link_monitor, ctx->exit_reason, ctx);
                    free(monitor);
                    break;
                }
                case CONTEXT_MONITOR_LINK_LOCAL:
                case CONTEXT_MONITOR_MONITORED_LOCAL:
                case CONTEXT_MONITOR_MONITORING_LOCAL:
                case CONTEXT_MONITOR_MONITORING_LOCAL_REGISTEREDNAME:
                    UNREACHABLE();
            }
        }
    }

    // Any other process released our mailbox, so we can clear it.
    mailbox_destroy(&ctx->mailbox, &ctx->heap);

    destroy_extended_registers(ctx, 0);
    free(ctx->fr);

    memory_destroy_heap(&ctx->heap, ctx->global);

    dictionary_destroy(&ctx->dictionary);

    if (ctx->timer_list_head.head.next != &ctx->timer_list_head.head) {
        scheduler_cancel_timeout(ctx);
    }

    // Platform data is freed here to allow drivers to use the
    // globalcontext_get_process_lock lock to protect this pointer
    // Typically, another thread or an interrupt would call
    // globalcontext_get_process_lock before accessing platform_data.
    // Here, the context can no longer be acquired with
    // globalcontext_get_process_lock, so it's safe to free the pointer.
    free(ctx->platform_data);

    ets_delete_owned_tables(&ctx->global->ets, ctx->process_id, ctx->global);

    free(ctx);
}

static inline term term_pid_or_port_from_context(const Context *ctx)
{
    if (ctx->native_handler != NULL) {
        return term_port_from_local_process_id(ctx->process_id);
    } else {
        return term_from_local_process_id(ctx->process_id);
    }
}

void context_process_kill_signal(Context *ctx, struct TermSignal *signal)
{
    // exit_reason is one of the roots when garbage collecting
    ctx->exit_reason = signal->signal_term;
    context_update_flags(ctx, ~NoFlags, Killed);
}

void context_process_process_info_request_signal(Context *ctx, struct BuiltInAtomRequestSignal *signal, bool process_table_locked)
{
    Context *target;
    if (process_table_locked) {
        target = globalcontext_get_process_nolock(ctx->global, signal->sender_pid);
    } else {
        target = globalcontext_get_process_lock(ctx->global, signal->sender_pid);
    }
    if (LIKELY(target)) {
        size_t term_size;
        if (context_get_process_info(ctx, NULL, &term_size, signal->atom, NULL)) {
            Heap heap;
            if (UNLIKELY(memory_init_heap(&heap, term_size) != MEMORY_GC_OK)) {
                mailbox_send_immediate_signal(target, TrapExceptionSignal, OUT_OF_MEMORY_ATOM);
            } else {
                term ret;
                if (context_get_process_info(ctx, &ret, NULL, signal->atom, &heap)) {
                    mailbox_send_term_signal(target, TrapAnswerSignal, ret);
                } else {
                    mailbox_send_immediate_signal(target, TrapExceptionSignal, ret);
                }
                memory_destroy_heap(&heap, ctx->global);
            }
        } else {
            mailbox_send_immediate_signal(target, TrapExceptionSignal, BADARG_ATOM);
        }
        if (!process_table_locked) {
            globalcontext_get_process_unlock(ctx->global, target);
        }
    } // else: sender died
}

bool context_process_signal_trap_answer(Context *ctx, struct TermSignal *signal)
{
    context_update_flags(ctx, ~Trap, NoFlags);
    ctx->x[0] = signal->signal_term;
    return true;
}

bool context_process_signal_set_group_leader(Context *ctx, const struct TermSignal *signal)
{
    size_t leader_term_size = memory_estimate_usage(signal->signal_term);
    ctx->group_leader = UNDEFINED_ATOM;
    if (UNLIKELY(memory_ensure_free_opt(ctx, leader_term_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        return false;
    }
    ctx->group_leader = memory_copy_term_tree(&ctx->heap, signal->signal_term);
    return true;
}

void context_process_flush_monitor_signal(Context *ctx, uint64_t ref_ticks, bool info)
{
    context_update_flags(ctx, ~Trap, NoFlags);
    bool result = true;
    mailbox_reset(&ctx->mailbox);
    term msg;
    while (mailbox_peek(ctx, &msg)) {
        if (term_is_tuple(msg)
            && term_get_tuple_arity(msg) == 5
            && term_get_tuple_element(msg, 0) == DOWN_ATOM
            && term_is_local_reference(term_get_tuple_element(msg, 1))
            && term_to_ref_ticks(term_get_tuple_element(msg, 1)) == ref_ticks) {
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            // If option info is combined with option flush, false is returned if a flush was needed, otherwise true.
            result = !info;
        } else {
            mailbox_next(&ctx->mailbox);
        }
    }
    mailbox_reset(&ctx->mailbox);
    ctx->x[0] = result ? TRUE_ATOM : FALSE_ATOM;
}

bool context_process_link_exit_signal(Context *ctx, struct TermSignal *signal)
{
    term link_pid = term_get_tuple_element(signal->signal_term, 1);
    // Always remove link.
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->monitors_head) {
        struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
        if (monitor->monitor_type == CONTEXT_MONITOR_LINK_LOCAL) {
            struct LinkLocalMonitor *link = CONTAINER_OF(monitor, struct LinkLocalMonitor, monitor);
            if (link->link_local_process_id == link_pid) {
                // Remove link
                list_remove(&monitor->monitor_list_head);
                free(monitor);
                break;
            }
        }
    }
    // Determine if we are trapping exits
    if (ctx->trap_exit) {
        // Simply enqueue the term as a message.
        // Message orders between link_pid and ctx is preserved even if this
        // message is enqueued later than the signal: other messages from
        // link_pid were before the exit signal
        mailbox_send(ctx, signal->signal_term);
        return true;
    } else {
        term reason = term_get_tuple_element(signal->signal_term, 2);
        if (reason != NORMAL_ATOM) {
            // exit_reason is one of the roots when garbage collecting
            ctx->exit_reason = reason;
            context_update_flags(ctx, ~NoFlags, Killed);
        }
        return false;
    }
}

void context_process_monitor_down_signal(Context *ctx, struct TermSignal *signal)
{
    term monitor_ref = term_get_tuple_element(signal->signal_term, 1);
    uint64_t ref_ticks = term_to_ref_ticks(monitor_ref);
    term monitor_obj = term_get_tuple_element(signal->signal_term, 3);
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->monitors_head) {
        struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
        if (monitor->monitor_type == CONTEXT_MONITOR_MONITORING_LOCAL) {
            struct MonitorLocalMonitor *monitoring_monitor = CONTAINER_OF(monitor, struct MonitorLocalMonitor, monitor);
            if (monitoring_monitor->monitor_obj == monitor_obj && monitoring_monitor->ref_ticks == ref_ticks) {
                // Remove link
                list_remove(&monitor->monitor_list_head);
                free(monitor);
                // Enqueue the term as a message.
                mailbox_send(ctx, signal->signal_term);
                break;
            }
        } else if (monitor->monitor_type == CONTEXT_MONITOR_MONITORING_LOCAL_REGISTEREDNAME) {
            int32_t monitor_process_id = term_to_local_process_id(monitor_obj);
            struct MonitorLocalRegisteredNameMonitor *monitoring_monitor = CONTAINER_OF(monitor, struct MonitorLocalRegisteredNameMonitor, monitor);
            if (monitoring_monitor->monitor_process_id == monitor_process_id && monitoring_monitor->ref_ticks == ref_ticks) {
                // Remove link
                list_remove(&monitor->monitor_list_head);

                // We need to modify the monitor_obj item
                BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2), temp_heap)
                term name_tuple = term_alloc_tuple(2, &temp_heap);
                term_put_tuple_element(name_tuple, 0, monitoring_monitor->monitor_name);
                term_put_tuple_element(name_tuple, 1, ctx->global->node_name);
                term_put_tuple_element(signal->signal_term, 3, name_tuple);
                mailbox_send(ctx, signal->signal_term);
                END_WITH_STACK_HEAP(temp_heap, ctx->global);

                free(monitor);
                break;
            }
        }
    }
    // If monitor was not found, it was removed and message should not be sent.
    // (flush option removes messages that were already sent)
}

void context_process_code_server_resume_signal(Context *ctx)
{
#ifndef AVM_NO_JIT
    // jit_trap_and_load stores the label in saved_function_ptr
    uint32_t label = (uint32_t) (uintptr_t) ctx->saved_function_ptr;
    Module *module = ctx->saved_module;
#ifndef AVM_NO_EMU
    if (module->native_code) {
        ctx->saved_function_ptr = module_get_native_entry_point(module, label);
    } else {
        ctx->saved_ip = module->labels[label];
    }
#else
    ctx->saved_function_ptr = module_get_native_entry_point(module, label);
#endif
    // Fix CP to OP_INT_CALL_END
    if (ctx->cp == module_address(module->module_index, 0)) {
        ctx->cp = module_address(module->module_index, module->end_instruction_ii);
    }
#endif
    context_update_flags(ctx, ~Trap, NoFlags);
}

void context_update_flags(Context *ctx, int mask, int value) CLANG_THREAD_SANITIZE_SAFE
{
#ifndef AVM_NO_SMP
    enum ContextFlags expected = ctx->flags;
    enum ContextFlags desired;
    do {
        desired = (expected & mask) | value;
    } while (!ATOMIC_COMPARE_EXCHANGE_WEAK_INT(&ctx->flags, &expected, desired));
#else
    ctx->flags = (ctx->flags & mask) | value;
#endif
}

size_t context_message_queue_len(Context *ctx)
{
    return mailbox_len(&ctx->mailbox);
}

size_t context_size(Context *ctx)
{
    size_t messages_size = mailbox_size(&ctx->mailbox);

    // TODO include ctx->platform_data
    return sizeof(Context)
        + messages_size
        + memory_heap_memory_size(&ctx->heap) * BYTES_PER_TERM;
}

bool context_get_process_info(Context *ctx, term *out, size_t *term_size, term atom_key, Heap *heap)
{
    size_t ret_size;
    switch (atom_key) {
        case HEAP_SIZE_ATOM:
        case TOTAL_HEAP_SIZE_ATOM:
        case STACK_SIZE_ATOM:
        case MESSAGE_QUEUE_LEN_ATOM:
        case REGISTERED_NAME_ATOM:
        case MEMORY_ATOM:
            ret_size = TUPLE_SIZE(2);
            break;
        case LINKS_ATOM: {
            struct ListHead *item;
            ret_size = TUPLE_SIZE(2);
            LIST_FOR_EACH (item, &ctx->monitors_head) {
                struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
                if (monitor->monitor_type == CONTEXT_MONITOR_LINK_LOCAL) {
                    struct LinkLocalMonitor *link = CONTAINER_OF(monitor, struct LinkLocalMonitor, monitor);
                    if (link->unlink_id == UNLINK_ID_LINK_ACTIVE) {
                        ret_size += CONS_SIZE;
                    }
                } else if (monitor->monitor_type == CONTEXT_MONITOR_LINK_REMOTE) {
                    struct LinkRemoteMonitor *link = CONTAINER_OF(monitor, struct LinkRemoteMonitor, monitor);
                    if (link->unlink_id == UNLINK_ID_LINK_ACTIVE) {
                        ret_size += CONS_SIZE + EXTERNAL_PID_SIZE;
                    }
                }
            }
            break;
        }
        case MONITORED_BY_ATOM: {
            struct ListHead *item;
            ret_size = TUPLE_SIZE(2);
            LIST_FOR_EACH (item, &ctx->monitors_head) {
                struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
                if (monitor->monitor_type == CONTEXT_MONITOR_MONITORED_LOCAL) {
                    ret_size += CONS_SIZE;
                } else if (monitor->monitor_type == CONTEXT_MONITOR_RESOURCE) {
                    ret_size += CONS_SIZE + TERM_BOXED_REFERENCE_RESOURCE_SIZE;
                }
            }
            break;
        }
        case CURRENT_STACKTRACE_ATOM: {
            ret_size = TUPLE_SIZE(2);
            break;
        }
        default:
            if (out != NULL) {
                *out = BADARG_ATOM;
            }
            return false;
    }
    if (term_size != NULL) {
        *term_size = ret_size;
    }
    if (out == NULL) {
        return true;
    }

    term ret = term_alloc_tuple(2, heap);
    switch (atom_key) {
        // heap_size size in words of the heap of the process
        case HEAP_SIZE_ATOM: {
            term_put_tuple_element(ret, 0, HEAP_SIZE_ATOM);
            unsigned long value = memory_heap_youngest_size(&ctx->heap);
            term_put_tuple_element(ret, 1, term_from_int(value));
            break;
        }

        // registered_name for process or port..
        case REGISTERED_NAME_ATOM: {
            term name = globalcontext_get_registered_name_process(ctx->global, ctx->process_id);
            if (term_is_invalid_term((name))) {
                ret = term_nil(); // Set ret to an empty list to match erlang behaviour
            } else {
                term_put_tuple_element(ret, 0, REGISTERED_NAME_ATOM);
                term_put_tuple_element(ret, 1, name);
            }
            break;
        }

        // total_heap_size size in words of the heap of the process, including fragments
        case TOTAL_HEAP_SIZE_ATOM: {
            term_put_tuple_element(ret, 0, TOTAL_HEAP_SIZE_ATOM);
            unsigned long value = memory_heap_memory_size(&ctx->heap);
            term_put_tuple_element(ret, 1, term_from_int(value));
            break;
        }

        // stack_size stack size, in words, of the process
        case STACK_SIZE_ATOM: {
            term_put_tuple_element(ret, 0, STACK_SIZE_ATOM);
            unsigned long value = context_stack_size(ctx);
            term_put_tuple_element(ret, 1, term_from_int(value));
            break;
        }

        // message_queue_len number of messages currently in the message queue of the process
        case MESSAGE_QUEUE_LEN_ATOM: {
            term_put_tuple_element(ret, 0, MESSAGE_QUEUE_LEN_ATOM);
            unsigned long value = context_message_queue_len(ctx);
            term_put_tuple_element(ret, 1, term_from_int(value));
            break;
        }

        // memory size in bytes of the process. This includes call stack, heap, and internal structures.
        case MEMORY_ATOM: {
            term_put_tuple_element(ret, 0, MEMORY_ATOM);
            unsigned long value = context_size(ctx);
            term_put_tuple_element(ret, 1, term_from_int(value));
            break;
        }

        // pids of linked processes
        case LINKS_ATOM: {
            term_put_tuple_element(ret, 0, LINKS_ATOM);
            term list = term_nil();
            struct ListHead *item;
            LIST_FOR_EACH (item, &ctx->monitors_head) {
                struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
                if (monitor->monitor_type == CONTEXT_MONITOR_LINK_LOCAL) {
                    struct LinkLocalMonitor *link = CONTAINER_OF(monitor, struct LinkLocalMonitor, monitor);
                    if (link->unlink_id == UNLINK_ID_LINK_ACTIVE) {
                        list = term_list_prepend(link->link_local_process_id, list, heap);
                    }
                } else if (monitor->monitor_type == CONTEXT_MONITOR_LINK_REMOTE) {
                    struct LinkRemoteMonitor *link = CONTAINER_OF(monitor, struct LinkRemoteMonitor, monitor);
                    if (link->unlink_id == UNLINK_ID_LINK_ACTIVE) {
                        term external_pid = term_make_external_process_id(link->node, link->pid_number, link->pid_serial, link->creation, heap);
                        list = term_list_prepend(external_pid, list, heap);
                    }
                }
            }
            term_put_tuple_element(ret, 1, list);
            break;
        }
        // pids of monitoring processes / resources
        case MONITORED_BY_ATOM: {
            term_put_tuple_element(ret, 0, MONITORED_BY_ATOM);
            term list = term_nil();
            struct ListHead *item;
            LIST_FOR_EACH (item, &ctx->monitors_head) {
                struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
                if (monitor->monitor_type == CONTEXT_MONITOR_MONITORED_LOCAL) {
                    struct MonitorLocalMonitor *monitored_monitor = CONTAINER_OF(monitor, struct MonitorLocalMonitor, monitor);
                    list = term_list_prepend(monitored_monitor->monitor_obj, list, heap);
                } else if (monitor->monitor_type == CONTEXT_MONITOR_RESOURCE) {
                    struct ResourceContextMonitor *resource_monitor = CONTAINER_OF(monitor, struct ResourceContextMonitor, monitor);
                    term resource = resource_monitor_to_resource(resource_monitor->resource_type, resource_monitor->ref_ticks, heap);
                    if (LIKELY(!term_is_invalid_term(resource))) {
                        list = term_list_prepend(resource, list, heap);
                    }
                }
            }
            term_put_tuple_element(ret, 1, list);
            break;
        }

        case CURRENT_STACKTRACE_ATOM: {
            term_put_tuple_element(ret, 0, CURRENT_STACKTRACE_ATOM);
            // FIXME: since it's not possible how to build stacktrace here with the current API,
            // this mock implementation returns an empty list
            term_put_tuple_element(ret, 1, term_nil());
            break;
        }

        default:
            UNREACHABLE();
    }
    *out = ret;
    return true;
}

static struct Monitor *context_monitors_handle_terminate(Context *ctx)
{
    GlobalContext *glb = ctx->global;
    struct ListHead *item;
    struct ListHead *tmp;
    struct Monitor *result = NULL;
    MUTABLE_LIST_FOR_EACH (item, tmp, &ctx->monitors_head) {
        struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
        switch (monitor->monitor_type) {
            case CONTEXT_MONITOR_RESOURCE: {
                // monitor with resource
                // remove it from the list we are iterating on and
                // add it to the list of resource monitors to handle afterwards
                if (result == NULL) {
                    list_init(&monitor->monitor_list_head);
                    result = monitor;
                } else {
                    list_append(&result->monitor_list_head, &monitor->monitor_list_head);
                }
                break;
            }
            case CONTEXT_MONITOR_MONITORING_LOCAL: {
                // We are the monitoring process.
                struct MonitorLocalMonitor *monitoring_monitor = CONTAINER_OF(monitor, struct MonitorLocalMonitor, monitor);
                int32_t local_process_id = term_to_local_process_id(monitoring_monitor->monitor_obj);
                Context *target = globalcontext_get_process_nolock(glb, local_process_id);
                if (LIKELY(target != NULL)) {
                    // target can be null if we didn't process a MonitorDownSignal
                    mailbox_send_ref_signal(target, DemonitorSignal, monitoring_monitor->ref_ticks);
                }
                free(monitor);
                break;
            }
            case CONTEXT_MONITOR_MONITORING_LOCAL_REGISTEREDNAME: {
                // We are the monitoring process.
                struct MonitorLocalRegisteredNameMonitor *monitoring_monitor = CONTAINER_OF(monitor, struct MonitorLocalRegisteredNameMonitor, monitor);
                int32_t local_process_id = monitoring_monitor->monitor_process_id;
                Context *target = globalcontext_get_process_nolock(glb, local_process_id);
                if (LIKELY(target != NULL)) {
                    // target can be null if we didn't process a MonitorDownSignal
                    mailbox_send_ref_signal(target, DemonitorSignal, monitoring_monitor->ref_ticks);
                }
                free(monitor);
                break;
            }
            case CONTEXT_MONITOR_LINK_LOCAL: {
                struct LinkLocalMonitor *link_monitor = CONTAINER_OF(monitor, struct LinkLocalMonitor, monitor);
                // Handle the case of inactive link.
                if (link_monitor->unlink_id != UNLINK_ID_LINK_ACTIVE) {
                    free(monitor);
                    continue;
                }
                int32_t local_process_id = term_to_local_process_id(link_monitor->link_local_process_id);
                Context *target = globalcontext_get_process_nolock(glb, local_process_id);
                if (LIKELY(target != NULL)) {
                    // target can be null if we didn't process a LinkExitSignal
                    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(3)) != MEMORY_GC_OK)) {
                        // TODO: handle out of memory here
                        fprintf(stderr, "Cannot handle out of memory.\n");
                        globalcontext_get_process_unlock(glb, target);
                        AVM_ABORT();
                    }
                    // Prepare the message on ctx's heap which will be freed afterwards.
                    term info_tuple = term_alloc_tuple(3, &ctx->heap);
                    term_put_tuple_element(info_tuple, 0, EXIT_ATOM);
                    term_put_tuple_element(info_tuple, 1, term_pid_or_port_from_context(ctx));
                    term_put_tuple_element(info_tuple, 2, ctx->exit_reason);
                    mailbox_send_term_signal(target, LinkExitSignal, info_tuple);
                }
                free(monitor);
                break;
            }
            case CONTEXT_MONITOR_LINK_REMOTE: {
                // Process it afterwards
                if (result == NULL) {
                    list_init(&monitor->monitor_list_head);
                    result = monitor;
                } else {
                    list_append(&result->monitor_list_head, &monitor->monitor_list_head);
                }
                break;
            }
            case CONTEXT_MONITOR_MONITORED_LOCAL: {
                struct MonitorLocalMonitor *monitored_monitor = CONTAINER_OF(monitor, struct MonitorLocalMonitor, monitor);
                int32_t local_process_id = term_to_local_process_id(monitored_monitor->monitor_obj);
                Context *target = globalcontext_get_process_nolock(glb, local_process_id);
                // Target cannot be NULL as we processed Demonitor signals
                assert(target != NULL);
                int required_terms = REF_SIZE + TUPLE_SIZE(5);
                if (UNLIKELY(memory_ensure_free(ctx, required_terms) != MEMORY_GC_OK)) {
                    // TODO: handle out of memory here
                    fprintf(stderr, "Cannot handle out of memory.\n");
                    globalcontext_get_process_unlock(glb, target);
                    AVM_ABORT();
                }
                // Prepare the message on ctx's heap which will be freed afterwards.
                term ref = term_from_ref_ticks(monitored_monitor->ref_ticks, &ctx->heap);

                term port_or_process = term_pid_or_port_from_context(ctx);
                term port_or_process_atom
                    = term_is_local_port(port_or_process) ? PORT_ATOM : PROCESS_ATOM;

                term info_tuple = term_alloc_tuple(5, &ctx->heap);
                term_put_tuple_element(info_tuple, 0, DOWN_ATOM);
                term_put_tuple_element(info_tuple, 1, ref);
                term_put_tuple_element(info_tuple, 2, port_or_process_atom);
                term_put_tuple_element(info_tuple, 3, port_or_process);
                term_put_tuple_element(info_tuple, 4, ctx->exit_reason);

                mailbox_send_term_signal(target, MonitorDownSignal, info_tuple);
                free(monitor);
                break;
            }
        }
    }
    return result;
}

static void context_distribution_handle_terminate(Context *ctx)
{
    // For now, the only process with Distribution flag set is net_kernel.
    GlobalContext *glb = ctx->global;
    glb->node_name = NONODE_AT_NOHOST_ATOM;
    glb->creation = 0;
}

struct Monitor *monitor_link_new(term link_pid)
{
    if (term_is_local_pid_or_port(link_pid)) {
        struct LinkLocalMonitor *monitor = malloc(sizeof(struct LinkLocalMonitor));
        if (IS_NULL_PTR(monitor)) {
            return NULL;
        }
        monitor->monitor.monitor_type = CONTEXT_MONITOR_LINK_LOCAL;
        monitor->unlink_id = UNLINK_ID_LINK_ACTIVE;
        monitor->link_local_process_id = link_pid;
        return &monitor->monitor;
    } else {
        struct LinkRemoteMonitor *monitor = malloc(sizeof(struct LinkRemoteMonitor));
        if (IS_NULL_PTR(monitor)) {
            return NULL;
        }
        monitor->monitor.monitor_type = CONTEXT_MONITOR_LINK_REMOTE;
        monitor->unlink_id = UNLINK_ID_LINK_ACTIVE;
        monitor->node = term_get_external_node(link_pid);
        monitor->pid_number = term_get_external_pid_process_id(link_pid);
        monitor->pid_serial = term_get_external_pid_serial(link_pid);
        monitor->creation = term_get_external_node_creation(link_pid);
        return &monitor->monitor;
    }
}

struct Monitor *monitor_new(term monitor_pid, uint64_t ref_ticks, bool is_monitoring)
{
    struct MonitorLocalMonitor *monitor = malloc(sizeof(struct MonitorLocalMonitor));
    if (IS_NULL_PTR(monitor)) {
        return NULL;
    }
    if (is_monitoring) {
        monitor->monitor.monitor_type = CONTEXT_MONITOR_MONITORING_LOCAL;
    } else {
        monitor->monitor.monitor_type = CONTEXT_MONITOR_MONITORED_LOCAL;
    }
    monitor->monitor_obj = monitor_pid;
    monitor->ref_ticks = ref_ticks;

    return &monitor->monitor;
}

struct Monitor *monitor_registeredname_monitor_new(int32_t monitor_process_id, term monitor_name, uint64_t ref_ticks)
{
    struct MonitorLocalRegisteredNameMonitor *monitor = malloc(sizeof(struct MonitorLocalRegisteredNameMonitor));
    if (IS_NULL_PTR(monitor)) {
        return NULL;
    }
    monitor->monitor.monitor_type = CONTEXT_MONITOR_MONITORING_LOCAL_REGISTEREDNAME;
    monitor->monitor_process_id = monitor_process_id;
    monitor->monitor_name = monitor_name;
    monitor->ref_ticks = ref_ticks;

    return &monitor->monitor;
}

struct Monitor *monitor_resource_monitor_new(void *resource, uint64_t ref_ticks)
{
    struct ResourceContextMonitor *monitor = malloc(sizeof(struct ResourceContextMonitor));
    if (IS_NULL_PTR(monitor)) {
        return NULL;
    }
    monitor->monitor.monitor_type = CONTEXT_MONITOR_RESOURCE;
    struct RefcBinary *refc = refc_binary_from_data(resource);
    monitor->resource_type = refc->resource_type;
    monitor->ref_ticks = ref_ticks;

    return &monitor->monitor;
}

bool context_add_monitor(Context *ctx, struct Monitor *new_monitor)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->monitors_head) {
        struct Monitor *existing = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
        if (existing->monitor_type == new_monitor->monitor_type) {
            switch (new_monitor->monitor_type) {
                case CONTEXT_MONITOR_LINK_LOCAL: {
                    struct LinkLocalMonitor *new_link_monitor = CONTAINER_OF(new_monitor, struct LinkLocalMonitor, monitor);
                    struct LinkLocalMonitor *existing_link_monitor = CONTAINER_OF(existing, struct LinkLocalMonitor, monitor);
                    if (UNLIKELY(existing_link_monitor->link_local_process_id == new_link_monitor->link_local_process_id)) {
                        free(new_monitor);
                        return false;
                    }
                    break;
                }
                case CONTEXT_MONITOR_MONITORING_LOCAL:
                case CONTEXT_MONITOR_MONITORED_LOCAL: {
                    struct MonitorLocalMonitor *new_local_monitor = CONTAINER_OF(new_monitor, struct MonitorLocalMonitor, monitor);
                    struct MonitorLocalMonitor *existing_local_monitor = CONTAINER_OF(existing, struct MonitorLocalMonitor, monitor);
                    if (UNLIKELY(existing_local_monitor->monitor_obj == new_local_monitor->monitor_obj && existing_local_monitor->ref_ticks == new_local_monitor->ref_ticks)) {
                        free(new_monitor);
                        return false;
                    }
                    break;
                }
                case CONTEXT_MONITOR_MONITORING_LOCAL_REGISTEREDNAME: {
                    struct MonitorLocalRegisteredNameMonitor *new_local_registeredname_monitor = CONTAINER_OF(new_monitor, struct MonitorLocalRegisteredNameMonitor, monitor);
                    struct MonitorLocalRegisteredNameMonitor *existing_local_registeredname_monitor = CONTAINER_OF(existing, struct MonitorLocalRegisteredNameMonitor, monitor);
                    if (UNLIKELY(existing_local_registeredname_monitor->monitor_process_id == new_local_registeredname_monitor->monitor_process_id
                            && existing_local_registeredname_monitor->monitor_name == new_local_registeredname_monitor->monitor_name
                            && existing_local_registeredname_monitor->ref_ticks == new_local_registeredname_monitor->ref_ticks)) {
                        free(new_monitor);
                        return false;
                    }
                    break;
                }
                case CONTEXT_MONITOR_RESOURCE: {
                    struct ResourceContextMonitor *new_resource_monitor = CONTAINER_OF(new_monitor, struct ResourceContextMonitor, monitor);
                    struct ResourceContextMonitor *existing_resource_monitor = CONTAINER_OF(existing, struct ResourceContextMonitor, monitor);
                    if (UNLIKELY(existing_resource_monitor->resource_type == new_resource_monitor->resource_type && existing_resource_monitor->ref_ticks == new_resource_monitor->ref_ticks)) {
                        free(new_monitor);
                        return false;
                    }
                    break;
                }
                case CONTEXT_MONITOR_LINK_REMOTE: {
                    struct LinkRemoteMonitor *new_link_monitor = CONTAINER_OF(new_monitor, struct LinkRemoteMonitor, monitor);
                    struct LinkRemoteMonitor *existing_link_monitor = CONTAINER_OF(existing, struct LinkRemoteMonitor, monitor);
                    if (UNLIKELY(existing_link_monitor->node == new_link_monitor->node
                            && existing_link_monitor->pid_number == new_link_monitor->pid_number
                            && existing_link_monitor->pid_serial == new_link_monitor->pid_serial
                            && existing_link_monitor->creation == new_link_monitor->creation)) {
                        free(new_monitor);
                        return false;
                    }
                    break;
                }
            }
        }
    }
    list_append(&ctx->monitors_head, &new_monitor->monitor_list_head);
    return true;
}

bool context_set_unlink_id(Context *ctx, term link_pid, uint64_t *unlink_id)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->monitors_head) {
        struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
        if (term_is_local_pid_or_port(link_pid) && monitor->monitor_type == CONTEXT_MONITOR_LINK_LOCAL) {
            struct LinkLocalMonitor *link = CONTAINER_OF(monitor, struct LinkLocalMonitor, monitor);
            if (link->link_local_process_id == link_pid) {
                if (link->unlink_id == UNLINK_ID_LINK_ACTIVE) {
                    uint64_t new_id = globalcontext_get_ref_ticks(ctx->global);
                    link->unlink_id = new_id;
                    *unlink_id = new_id;
                    return true;
                } else {
                    return false;
                }
            }
        } else if (term_is_external_pid(link_pid) && monitor->monitor_type == CONTEXT_MONITOR_LINK_REMOTE) {
            struct LinkRemoteMonitor *link = CONTAINER_OF(monitor, struct LinkRemoteMonitor, monitor);
            if (link->node == term_get_external_node(link_pid)
                && link->pid_number == term_get_external_pid_process_id(link_pid)
                && link->pid_serial == term_get_external_pid_serial(link_pid)
                && link->creation == term_get_external_node_creation(link_pid)) {
                if (link->unlink_id == UNLINK_ID_LINK_ACTIVE) {
                    uint64_t new_id = globalcontext_get_ref_ticks(ctx->global);
                    link->unlink_id = new_id;
                    *unlink_id = new_id;
                    return true;
                } else {
                    return false;
                }
            }
        }
    }
    return false;
}

void context_ack_unlink(Context *ctx, term link_pid, uint64_t unlink_id, bool process_table_locked)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->monitors_head) {
        struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
        if (term_is_local_pid_or_port(link_pid) && monitor->monitor_type == CONTEXT_MONITOR_LINK_LOCAL) {
            struct LinkLocalMonitor *link = CONTAINER_OF(monitor, struct LinkLocalMonitor, monitor);
            if (link->link_local_process_id == link_pid) {
                if (link->unlink_id == UNLINK_ID_LINK_ACTIVE) {
                    // Send ack and remove link
                    int local_process_id = term_to_local_process_id(link_pid);
                    Context *target;
                    if (process_table_locked) {
                        target = globalcontext_get_process_nolock(ctx->global, local_process_id);
                    } else {
                        target = globalcontext_get_process_lock(ctx->global, local_process_id);
                    }
                    if (LIKELY(target)) {
                        term self_pid = term_pid_or_port_from_context(ctx);
                        mailbox_send_immediate_ref_signal(target, UnlinkIDAckSignal, self_pid, unlink_id);
                        if (!process_table_locked) {
                            globalcontext_get_process_unlock(ctx->global, target);
                        }
                    }
                    list_remove(&monitor->monitor_list_head);
                    free(monitor);
                }
                return;
            }
        } else if (term_is_external_pid(link_pid) && monitor->monitor_type == CONTEXT_MONITOR_LINK_REMOTE) {
            struct LinkRemoteMonitor *link = CONTAINER_OF(monitor, struct LinkRemoteMonitor, monitor);
            if (link->node == term_get_external_node(link_pid)
                && link->pid_number == term_get_external_pid_process_id(link_pid)
                && link->pid_serial == term_get_external_pid_serial(link_pid)
                && link->creation == term_get_external_node_creation(link_pid)) {
                if (link->unlink_id == UNLINK_ID_LINK_ACTIVE) {
                    // Send ack and remove link
                    dist_send_unlink_id_ack(unlink_id, term_from_local_process_id(ctx->process_id), link_pid, ctx);
                    list_remove(&monitor->monitor_list_head);
                    free(monitor);
                }
                return;
            }
        }
    }
}

void context_unlink_ack(Context *ctx, term link_pid, uint64_t unlink_id)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->monitors_head) {
        struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
        if (monitor->monitor_type == CONTEXT_MONITOR_LINK_LOCAL) {
            struct LinkLocalMonitor *link = CONTAINER_OF(monitor, struct LinkLocalMonitor, monitor);
            if (link->link_local_process_id == link_pid) {
                if (link->unlink_id == unlink_id) {
                    // Remove link
                    list_remove(&monitor->monitor_list_head);
                    free(monitor);
                }
                return;
            }
        }
    }
}

void context_demonitor(Context *ctx, uint64_t ref_ticks)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->monitors_head) {
        struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
        switch (monitor->monitor_type) {
            case CONTEXT_MONITOR_MONITORING_LOCAL:
            case CONTEXT_MONITOR_MONITORED_LOCAL: {
                struct MonitorLocalMonitor *local_monitor = CONTAINER_OF(monitor, struct MonitorLocalMonitor, monitor);
                if (local_monitor->ref_ticks == ref_ticks) {
                    list_remove(&monitor->monitor_list_head);
                    free(monitor);
                    return;
                }
                break;
            }
            case CONTEXT_MONITOR_MONITORING_LOCAL_REGISTEREDNAME: {
                struct MonitorLocalRegisteredNameMonitor *local_registeredname_monitor = CONTAINER_OF(monitor, struct MonitorLocalRegisteredNameMonitor, monitor);
                if (local_registeredname_monitor->ref_ticks == ref_ticks) {
                    list_remove(&monitor->monitor_list_head);
                    free(monitor);
                    return;
                }
                break;
            }
            case CONTEXT_MONITOR_RESOURCE: {
                struct ResourceContextMonitor *resource_monitor = CONTAINER_OF(monitor, struct ResourceContextMonitor, monitor);
                if (resource_monitor->ref_ticks == ref_ticks) {
                    list_remove(&monitor->monitor_list_head);
                    free(monitor);
                    return;
                }
            }
            case CONTEXT_MONITOR_LINK_LOCAL:
            case CONTEXT_MONITOR_LINK_REMOTE:
                break;
        }
    }
}

term context_get_monitor_pid(Context *ctx, uint64_t ref_ticks, bool *is_monitoring)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->monitors_head) {
        struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
        switch (monitor->monitor_type) {
            case CONTEXT_MONITOR_MONITORING_LOCAL:
            case CONTEXT_MONITOR_MONITORED_LOCAL: {
                struct MonitorLocalMonitor *local_monitor = CONTAINER_OF(monitor, struct MonitorLocalMonitor, monitor);
                if (local_monitor->ref_ticks == ref_ticks) {
                    *is_monitoring = monitor->monitor_type == CONTEXT_MONITOR_MONITORING_LOCAL;
                    return local_monitor->monitor_obj;
                }
                break;
            }
            case CONTEXT_MONITOR_MONITORING_LOCAL_REGISTEREDNAME: {
                struct MonitorLocalRegisteredNameMonitor *local_registeredname_monitor = CONTAINER_OF(monitor, struct MonitorLocalRegisteredNameMonitor, monitor);
                if (local_registeredname_monitor->ref_ticks == ref_ticks) {
                    *is_monitoring = true;
                    return term_from_local_process_id(local_registeredname_monitor->monitor_process_id);
                }
                break;
            }
            case CONTEXT_MONITOR_LINK_LOCAL:
            case CONTEXT_MONITOR_LINK_REMOTE:
            case CONTEXT_MONITOR_RESOURCE:
                break;
        }
    }
    return term_invalid_term();
}

int context_get_catch_label(Context *ctx, Module **mod)
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

COLD_FUNC void context_dump(Context *ctx)
{
    GlobalContext *glb = ctx->global;

    fprintf(stderr, "CRASH \n======\n");

    fprintf(stderr, "pid: ");
    term_display(stderr, term_from_local_process_id(ctx->process_id), ctx);
    fprintf(stderr, "\n");

    fprintf(stderr, "\nStacktrace:\n");
    term_display(stderr, stacktrace_build(ctx, &ctx->x[2], 3), ctx);
    fprintf(stderr, "\n\n");

    {
        Module *cp_mod;
        int label;
        int offset;
        module_cp_to_label_offset(ctx->cp, &cp_mod, &label, &offset, NULL, ctx->global);
        fprintf(stderr, "cp: #CP<module: %i, label: %i, offset: %i>\n\n",
            cp_mod->module_index, label, offset);
    }

    fprintf(stderr, "x[0]: ");
    term_display(stderr, ctx->x[0], ctx);
    fprintf(stderr, "\nx[1]: ");
    term_display(stderr, ctx->x[1], ctx);
    fprintf(stderr, "\nx[2]: ");
    term_display(stderr, ctx->x[2], ctx);
    fprintf(stderr, "\n\nStack \n-----\n\n");

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
            module_cp_to_label_offset(*ct, &cp_mod, &label, &offset, NULL, ctx->global);
            fprintf(stderr, "#CP<module: %i, label: %i, offset: %i>\n", cp_mod->module_index, label, offset);

        } else {
            term_display(stderr, *ct, ctx);
            fprintf(stderr, "\n");
        }

        ct++;
    }

    fprintf(stderr, "\n\nMailbox\n-------\n");
    mailbox_crashdump(ctx);

    fprintf(stderr, "\n\nMonitors\n--------\n");
    // Lock processes table to make sure any dying process will not modify monitors
    struct ListHead *processes_table = synclist_rdlock(&glb->processes_table);
    UNUSED(processes_table);
    struct ListHead *item;
    LIST_FOR_EACH (item, &ctx->monitors_head) {
        struct Monitor *monitor = GET_LIST_ENTRY(item, struct Monitor, monitor_list_head);
        switch (monitor->monitor_type) {
            case CONTEXT_MONITOR_LINK_LOCAL: {
                struct LinkLocalMonitor *link_monitor = CONTAINER_OF(monitor, struct LinkLocalMonitor, monitor);
                fprintf(stderr, "link ");
                if (link_monitor->unlink_id) {
                    fprintf(stderr, "(inactive) ");
                }
                fprintf(stderr, "to ");
                term_display(stderr, link_monitor->link_local_process_id, ctx);
                fprintf(stderr, "\n");
                break;
            }
            case CONTEXT_MONITOR_LINK_REMOTE: {
                struct LinkRemoteMonitor *link_monitor = CONTAINER_OF(monitor, struct LinkRemoteMonitor, monitor);
                fprintf(stderr, "remote link ");
                if (link_monitor->unlink_id) {
                    fprintf(stderr, "(inactive) ");
                }
                fprintf(stderr, "to ");
                term_display(stderr, link_monitor->node, ctx);
                fprintf(stderr, "\n");
                break;
            }
            case CONTEXT_MONITOR_MONITORING_LOCAL: {
                struct MonitorLocalMonitor *monitoring_monitor = CONTAINER_OF(monitor, struct MonitorLocalMonitor, monitor);
                fprintf(stderr, "monitor to ");
                term_display(stderr, monitoring_monitor->monitor_obj, ctx);
                fprintf(stderr, " ref=%lu", (long unsigned) monitoring_monitor->ref_ticks);
                fprintf(stderr, "\n");
                break;
            }
            case CONTEXT_MONITOR_MONITORED_LOCAL: {
                struct MonitorLocalMonitor *monitored_monitor = CONTAINER_OF(monitor, struct MonitorLocalMonitor, monitor);
                fprintf(stderr, "monitored by ");
                term_display(stderr, monitored_monitor->monitor_obj, ctx);
                fprintf(stderr, " ref=%lu", (long unsigned) monitored_monitor->ref_ticks);
                fprintf(stderr, "\n");
                break;
            }
            case CONTEXT_MONITOR_MONITORING_LOCAL_REGISTEREDNAME: {
                struct MonitorLocalRegisteredNameMonitor *local_registeredname_monitor = CONTAINER_OF(monitor, struct MonitorLocalRegisteredNameMonitor, monitor);
                fprintf(stderr, "monitor to ");
                term_display(stderr, local_registeredname_monitor->monitor_name, ctx);
                fprintf(stderr, " (");
                term_display(stderr, term_from_local_process_id(local_registeredname_monitor->monitor_process_id), ctx);
                fprintf(stderr, ") ref=%lu", (long unsigned) local_registeredname_monitor->ref_ticks);
                fprintf(stderr, "\n");
                break;
            }
            case CONTEXT_MONITOR_RESOURCE: {
                struct ResourceContextMonitor *resource_monitor = CONTAINER_OF(monitor, struct ResourceContextMonitor, monitor);
                fprintf(stderr, "monitored by resource type %s ref=%lu", resource_monitor->resource_type->name, (long unsigned) resource_monitor->ref_ticks);
                fprintf(stderr, "\n");
                break;
            }
        }
    }
    synclist_unlock(&glb->processes_table);

    // If crash is caused by out_of_memory, print more data about memory usage
    if (ctx->x[0] == ERROR_ATOM && ctx->x[1] == OUT_OF_MEMORY_ATOM) {
        fprintf(stderr, "\n\nContext memory info\n-------------------\n");
        fprintf(stderr, "context_size = %zu\n", context_size(ctx));
        fprintf(stderr, "context_avail_free_memory = %zu\n", context_avail_free_memory(ctx));
        fprintf(stderr, "heap_size = %zu\n", memory_heap_youngest_size(&ctx->heap));
        fprintf(stderr, "total_heap_size = %zu\n", memory_heap_memory_size(&ctx->heap));
        fprintf(stderr, "stack_size = %zu\n", context_stack_size(ctx));
        fprintf(stderr, "message_queue_len = %zu\n", context_message_queue_len(ctx));
        fprintf(stderr, "\n\nGlobal memory info\n------------------\n");

        processes_table = synclist_rdlock(&glb->processes_table);
        size_t process_count = 0;
        size_t ports_count = 0;
        LIST_FOR_EACH (item, processes_table) {
            Context *p = GET_LIST_ENTRY(item, Context, processes_table_head);
            process_count++;
            if (p->native_handler) {
                ports_count++;
            }
        }
        synclist_unlock(&glb->processes_table);

        fprintf(stderr, "process_count = %zu\n", process_count);
        fprintf(stderr, "ports_count = %zu\n", ports_count);
        fprintf(stderr, "atoms_count = %zu\n", atom_table_count(glb->atom_table));
        fprintf(stderr, "refc_binary_total_size = %zu\n", refc_binary_total_size(ctx));
    }
    fprintf(stderr, "\n\n**End Of Crash Report**\n");
}
