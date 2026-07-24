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

#include "mailbox.h"

#include <stddef.h>

#include "context.h"
#include "memory.h"
#include "scheduler.h"
#include "synclist.h"
#include "trace.h"

#ifdef HAVE_PLATFORM_ATOMIC_H
#include "platform_atomic.h"
#else
#if defined(HAVE_ATOMIC)
#include <stdatomic.h>
#define ATOMIC_COMPARE_EXCHANGE_WEAK_PTR atomic_compare_exchange_weak
#endif
#endif

#define ADDITIONAL_PROCESSING_MEMORY_SIZE 4

void mailbox_init(Mailbox *mbx)
{
    mbx->outer_first = NULL;
    mbx->inner_first = NULL;
    mbx->inner_last = NULL;
    mbx->receive_pointer = NULL;
    mbx->receive_pointer_prev = NULL;
    mbx->receive_has_match_clauses = false;
}

// Convert a mailbox message (struct Message or struct TermSignal) to a heap
// fragment (HeapFragment) so it can be owned by the recipient.
// We assert this layout mapping is correct.
_Static_assert(offsetof(struct Message, base) + offsetof(struct MailboxMessage, next) == offsetof(HeapFragment, next) ? 1 : 0,
    "Message.base.next doesn't match HeapFragment.next");
_Static_assert(offsetof(struct Message, base) + offsetof(struct MailboxMessage, type) == offsetof(HeapFragment, heap_end) ? 1 : 0,
    "Message.base.type doesn't match HeapFragment.heap_end");
_Static_assert(offsetof(struct Message, message) == offsetof(HeapFragment, storage) ? 1 : 0,
    "Message.message doesn't match HeapFragment.storage[0]");
_Static_assert(offsetof(struct Message, heap_end) == offsetof(HeapFragment, storage[1]) ? 1 : 0,
    "Message.heap_end doesn't match HeapFragment.storage[1]");
_Static_assert(sizeof(struct Message) == sizeof(HeapFragment) + 2 * sizeof(term) ? 1 : 0,
    "sizeof(Message) doesn't match sizeof(HeapFragment) + 2 terms");
_Static_assert(offsetof(struct TermSignal, base) + offsetof(struct MailboxMessage, next) == offsetof(HeapFragment, next) ? 1 : 0,
    "TermSignal.base.next doesn't match HeapFragment.next");
_Static_assert(offsetof(struct TermSignal, base) + offsetof(struct MailboxMessage, type) == offsetof(HeapFragment, heap_end) ? 1 : 0,
    "TermSignal.base.type doesn't match HeapFragment.heap_end");
_Static_assert(offsetof(struct TermSignal, signal_term) == offsetof(HeapFragment, storage) ? 1 : 0,
    "TermSignal.signal_term doesn't match HeapFragment.storage[0]");
_Static_assert(offsetof(struct TermSignal, heap_end) == offsetof(HeapFragment, storage[1]) ? 1 : 0,
    "TermSignal.heap_end doesn't match HeapFragment.storage[1]");
_Static_assert(sizeof(struct TermSignal) == sizeof(HeapFragment) + 2 * sizeof(term) ? 1 : 0,
    "sizeof(TermSignal) doesn't match sizeof(HeapFragment) + 2 terms");

HeapFragment *mailbox_message_to_heap_fragment(void *m, term *heap_end)
{
    HeapFragment *fragment = (HeapFragment *) m;
    fragment->next = NULL; // MailboxMessage.next
    fragment->heap_end = heap_end; // MailboxMessage.type/heap_fragment_end
    // We don't need to erase Message.message/TermSignal.signal_term as they are valid terms
    // Message.heap_end or TrapSignal.heap_end are not valid terms, put nil
    fragment->storage[1] = term_nil(); // Message/TrapSignal.heap_end

    return fragment;
}

// Dispose message. Normal / signal messages are not destroyed, instead they
// are appended to the current heap.
void mailbox_message_dispose(MailboxMessage *m, Heap *heap)
{
    switch (m->type) {
        case NormalMessage: {
            Message *normal_message = CONTAINER_OF(m, Message, base);
            term mso_list = normal_message->storage[STORAGE_MSO_LIST_INDEX];
            HeapFragment *fragment = mailbox_message_to_heap_fragment(normal_message, normal_message->heap_end);
            memory_heap_append_fragment(heap, fragment, mso_list);
            break;
        }
        case KillSignal:
        case TrapAnswerSignal:
        case SetGroupLeaderSignal:
        case LinkExitSignal:
        case MonitorDownSignal:
        case AliasMessageSignal:
        case UnlinkRemoteIDSignal:
        case UnlinkRemoteIDAckSignal: {
            struct TermSignal *term_signal = CONTAINER_OF(m, struct TermSignal, base);
            term mso_list = term_signal->storage[STORAGE_MSO_LIST_INDEX];
            HeapFragment *fragment = mailbox_message_to_heap_fragment(term_signal, term_signal->heap_end);
            memory_heap_append_fragment(heap, fragment, mso_list);
            break;
        }
        case ProcessInfoRequestSignal: {
            struct ProcessInfoRequestSignal *request_signal
                = CONTAINER_OF(m, struct ProcessInfoRequestSignal, base);
            free(request_signal);
            break;
        }
        case TrapExceptionSignal: {
            struct ImmediateSignal *immediate_signal = CONTAINER_OF(m, struct ImmediateSignal, base);
            free(immediate_signal);
            break;
        }
        case UnlinkIDSignal:
        case UnlinkIDAckSignal: {
            struct ImmediateRefSignal *immediate_ref_signal = CONTAINER_OF(m, struct ImmediateRefSignal, base);
            free(immediate_ref_signal);
            break;
        }
        case FlushMonitorSignal:
        case FlushInfoMonitorSignal:
        case DemonitorSignal: {
            struct RefSignal *ref_signal = CONTAINER_OF(m, struct RefSignal, base);
            free(ref_signal);
            break;
        }
        case MonitorSignal: {
            struct MonitorPointerSignal *monitor_signal = CONTAINER_OF(m, struct MonitorPointerSignal, base);
            free(monitor_signal);
            break;
        }
        case CodeServerResumeSignal:
        case GCSignal:
            free(m);
            break;
    }
}

// Dispose message. Normal / signal messages are not destroyed, instead they
// are appended to the current heap.
void mailbox_message_dispose_unsent(Message *m, GlobalContext *global, bool from_task)
{
    term mso_list = m->storage[STORAGE_MSO_LIST_INDEX];
    HeapFragment *fragment = mailbox_message_to_heap_fragment(m, m->heap_end);
    memory_sweep_mso_list(mso_list, global, from_task);
    memory_destroy_heap_fragment(fragment);
}

void mailbox_destroy(Mailbox *mbox, Heap *heap)
{
    MailboxMessage *msg = mbox->outer_first;
    while (msg) {
        MailboxMessage *next = msg->next;
        mailbox_message_dispose(msg, heap);
        msg = next;
    }
    msg = mbox->inner_first;
    while (msg) {
        MailboxMessage *next = msg->next;
        mailbox_message_dispose(msg, heap);
        msg = next;
    }
}

size_t mailbox_len(Mailbox *mbox)
{
    size_t result = 0;
    MailboxMessage *msg = mbox->outer_first;
    while (msg) {
        result++;
        msg = msg->next;
    }
    msg = mbox->inner_first;
    while (msg) {
        result++;
        msg = msg->next;
    }
    return result;
}

size_t mailbox_normal_message_len(Mailbox *mbox)
{
    size_t result = 0;
    MailboxMessage *msg = mbox->outer_first;
    while (msg) {
        if (msg->type == NormalMessage || msg->type == AliasMessageSignal) {
            result++;
        }
        msg = msg->next;
    }
    msg = mbox->inner_first;
    while (msg) {
        result++;
        msg = msg->next;
    }
    return result;
}

size_t mailbox_size(Mailbox *mbox)
{
    size_t result = 0;
    MailboxMessage *msg = mbox->outer_first;
    while (msg) {
        // We don't count signals.
        if (msg->type == NormalMessage) {
            Message *normal_message = CONTAINER_OF(msg, Message, base);
            result += sizeof(Message) + normal_message->heap_end - normal_message->storage;
        }
        msg = msg->next;
    }
    msg = mbox->inner_first;
    while (msg) {
        Message *normal_message = CONTAINER_OF(msg, Message, base);
        result += sizeof(Message) + normal_message->heap_end - normal_message->storage;
        msg = msg->next;
    }
    return result;
}

// Messages are enqueued using atomics (or emulation) unless this is a no-smp
// build with no support for driver tasks
#if !defined(AVM_NO_SMP) || defined(AVM_TASK_DRIVER_ENABLED)
inline void mailbox_enqueue_message(Context *c, MailboxMessage *m)
{
    // Append message at the beginning of outer_first.
    MailboxMessage *current_first = NULL;
    do {
        m->next = current_first;
    } while (!ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(&c->mailbox.outer_first, &current_first, m));
}

void mailbox_post_message(Context *c, MailboxMessage *m)
{
    mailbox_enqueue_message(c, m);
    scheduler_signal_message(c);
}
#else
void mailbox_post_message(Context *c, MailboxMessage *m)
{
    m->next = c->mailbox.outer_first;
    c->mailbox.outer_first = m;
    scheduler_signal_message(c);
}
#endif

MailboxMessage *mailbox_message_create_from_term(enum MessageType type, term t)
{
    unsigned long estimated_mem_usage = memory_estimate_usage(t) + 1; // mso_list

    size_t base_size = type == NormalMessage ? sizeof(Message) : sizeof(struct TermSignal);
    void *msg_buf = malloc(base_size + estimated_mem_usage * sizeof(term));
    if (IS_NULL_PTR(msg_buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }

    if (type == NormalMessage) {
        Message *msg = msg_buf;
        msg->base.type = NormalMessage;
        msg->message = memory_copy_term_tree_to_storage(msg->storage, &msg->heap_end, t);

        return &msg->base;
    } else {
        struct TermSignal *ts = msg_buf;
        ts->base.type = type;
        ts->signal_term = memory_copy_term_tree_to_storage(ts->storage, &ts->heap_end, t);

        return &ts->base;
    }
}

Message *mailbox_message_create_normal_message_from_term(term t)
{
    MailboxMessage *message = mailbox_message_create_from_term(NormalMessage, t);
    return CONTAINER_OF(message, Message, base);
}

void mailbox_send(Context *c, term t)
{
    MailboxMessage *msg = mailbox_message_create_from_term(NormalMessage, t);
    mailbox_post_message(c, msg);
}

void mailbox_send_term_signal(Context *c, enum MessageType type, term t)
{
    MailboxMessage *signal = mailbox_message_create_from_term(type, t);
    mailbox_post_message(c, signal);
}

void mailbox_send_immediate_signal(Context *c, enum MessageType type, term immediate)
{
    struct ImmediateSignal *immediate_signal = malloc(sizeof(struct ImmediateSignal));
    if (IS_NULL_PTR(immediate_signal)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    immediate_signal->base.type = type;
    immediate_signal->immediate = immediate;

    mailbox_post_message(c, &immediate_signal->base);
}

bool mailbox_send_process_info_request_signal(
    Context *c, int32_t sender_pid, process_info_mode_t mode, const term atoms[], size_t atoms_len)
{
    struct ProcessInfoRequestSignal *signal
        = malloc(sizeof(struct ProcessInfoRequestSignal) + atoms_len * sizeof(term));
    if (IS_NULL_PTR(signal)) {
        return false;
    }
    signal->base.type = ProcessInfoRequestSignal;
    signal->sender_pid = sender_pid;
    signal->mode = mode;
    signal->atoms_len = atoms_len;
    for (size_t i = 0; i < atoms_len; i++) {
        signal->atoms[i] = atoms[i];
    }

    mailbox_post_message(c, &signal->base);
    return true;
}

void mailbox_send_ref_signal(Context *c, enum MessageType type, uint64_t ref_ticks)
{
    struct RefSignal *ref_signal = malloc(sizeof(struct RefSignal));
    if (IS_NULL_PTR(ref_signal)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    ref_signal->base.type = type;
    ref_signal->ref_ticks = ref_ticks;

    mailbox_post_message(c, &ref_signal->base);
}

void mailbox_send_immediate_ref_signal(Context *c, enum MessageType type, term immediate, uint64_t ref_ticks)
{
    struct ImmediateRefSignal *immediate_ref_signal = malloc(sizeof(struct ImmediateRefSignal));
    if (IS_NULL_PTR(immediate_ref_signal)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    immediate_ref_signal->base.type = type;
    immediate_ref_signal->immediate = immediate;
    immediate_ref_signal->ref_ticks = ref_ticks;

    mailbox_post_message(c, &immediate_ref_signal->base);
}

void mailbox_send_monitor_signal(Context *c, enum MessageType type, struct Monitor *monitor)
{
    struct MonitorPointerSignal *monitor_signal = malloc(sizeof(struct MonitorPointerSignal));
    if (IS_NULL_PTR(monitor_signal)) {
        // FIXME this function returns void, so the caller is not told the allocation failed
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    monitor_signal->base.type = type;
    monitor_signal->monitor = monitor;

    mailbox_post_message(c, &monitor_signal->base);
}

void mailbox_send_empty_body_signal(Context *c, enum MessageType type)
{
    MailboxMessage *m = malloc(sizeof(MailboxMessage));
    if (IS_NULL_PTR(m)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    m->type = type;

    mailbox_post_message(c, m);
}

void mailbox_reset(Mailbox *mbox)
{
    mbox->receive_pointer = mbox->inner_first;
    mbox->receive_pointer_prev = NULL;
}

// CAS-empty the outer list and return its raw head. The outer list is LIFO, so the head is the
// newest message and each message is older than its predecessor.
static inline MailboxMessage *detach_outer_list(Mailbox *mbox)
{
    MailboxMessage *current = mbox->outer_first;
#if !defined(AVM_NO_SMP) || defined(AVM_TASK_DRIVER_ENABLED)
    while (!ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(&mbox->outer_first, &current, NULL)) {
    };
#else
    mbox->outer_first = NULL;
#endif
    return current;
}

// Append a received-order run of normal messages, from first (oldest) to last (newest), at the end
// of the inner list, restoring the receive pointer when the inner list had been fully consumed.
// first and last are NULL together when no normal message was collected, making this a no-op.
static inline void append_normal_messages(Mailbox *mbox, MailboxMessage *first, MailboxMessage *last)
{
    if (last == NULL) {
        return;
    }

    // With no receive_pointer, it becomes the new list head.
    if (mbox->receive_pointer == NULL) {
        mbox->receive_pointer = first;
        // If we had a prev, set the prev's next to the new current.
        if (mbox->receive_pointer_prev) {
            mbox->receive_pointer_prev->next = first;
        } else if (mbox->inner_first == NULL) {
            // If we had no first, this is the first message.
            mbox->inner_first = first;
        }
    }

    // Append the new items at the end of the inner list. mbox->inner_last may be
    // mbox->receive_pointer_prev, which is then updated a second time here.
    if (mbox->inner_last) {
        mbox->inner_last->next = first;
    }
    mbox->inner_last = last;
}

MailboxMessage *mailbox_process_outer_list_native(Mailbox *mbox)
{
    MailboxMessage *current = detach_outer_list(mbox);

    // Reverse the list
    MailboxMessage *previous_normal = NULL;
    MailboxMessage *previous_signal = NULL;
    MailboxMessage *last_normal = NULL;
    while (current) {
        MailboxMessage *next = current->next;
        if (current->type == NormalMessage) {
            // Get last normal to update inner_last.
            if (last_normal == NULL) {
                last_normal = current;
            }
            current->next = previous_normal;
            previous_normal = current;
        } else {
            current->next = previous_signal;
            previous_signal = current;
        }
        current = next;
    }

    append_normal_messages(mbox, previous_normal, last_normal);
    return previous_signal;
}

MailboxMessage *mailbox_process_outer_list(Context *ctx)
{
    Mailbox *mbox = &ctx->mailbox;
    MailboxMessage *current = detach_outer_list(mbox);

    MailboxMessage *normal_first = NULL;
    MailboxMessage *normal_last = NULL;
    MailboxMessage *signal_first = NULL;

    if (ctx->active_alias_count == 0) {
        // Fast path (the common case): no active alias, so no alias message can be delivered. Same
        // single-pass split as mailbox_process_outer_list_native, except a stale AliasMessageSignal
        // (its alias is inactive) is freed now so it does not reach the signal loop, which would
        // treat it as unreachable.
        while (current) {
            MailboxMessage *next = current->next;
            if (current->type == AliasMessageSignal) {
                mailbox_message_dispose_unsent(CONTAINER_OF(current, Message, base), ctx->global, false);
            } else if (current->type == NormalMessage) {
                if (normal_last == NULL) {
                    normal_last = current;
                }
                current->next = normal_first;
                normal_first = current;
            } else {
                current->next = signal_first;
                signal_first = current;
            }
            current = next;
        }
    } else {
        // At least one active alias: alias side effects can deactivate the alias (e.g.
        // reply_demonitor), so they must run in received order. Of several same-batch sends to one
        // alias, only the first is delivered, like OTP. Reverse the LIFO list into received order.
        MailboxMessage *received = NULL;
        while (current) {
            MailboxMessage *next = current->next;
            current->next = received;
            received = current;
            current = next;
        }

        // Walk oldest to newest, appending so both sublists keep received order.
        MailboxMessage *signal_last = NULL;
        current = received;
        while (current) {
            MailboxMessage *next = current->next;
            if (current->type == NormalMessage) {
                current->next = NULL;
                if (normal_last == NULL) {
                    normal_first = current;
                } else {
                    normal_last->next = current;
                }
                normal_last = current;
            } else if (current->type == AliasMessageSignal) {
                // Validate the alias (in the owner's own context) and convert to a normal message.
                term message = context_process_alias_message_signal(ctx, CONTAINER_OF(current, struct TermSignal, base));
                if (!term_is_invalid_term(message)) {
                    // Re-type in place: struct TermSignal and struct Message share a layout
                    // (static-asserted above) and the message term already lives in this signal's
                    // storage, so nothing is copied. The conversion cannot fail on OOM, which
                    // matters because reply_demonitor's side effects already ran above.
                    Message *converted = CONTAINER_OF(current, Message, base);
                    converted->base.type = NormalMessage;
                    converted->message = message;
                    converted->base.next = NULL;
                    if (normal_last == NULL) {
                        normal_first = &converted->base;
                    } else {
                        normal_last->next = &converted->base;
                    }
                    normal_last = &converted->base;
                } else {
                    // Inactive alias: never delivered, so nothing references the term. Free it
                    // now (sweeping refc binaries) instead of leaving it on the heap until GC.
                    mailbox_message_dispose_unsent(CONTAINER_OF(current, Message, base), ctx->global, false);
                }
            } else {
                // A 'DOWN' auto-removing a {alias, _} monitor also deactivates its alias. Do that
                // here, in received order, so a later same-batch alias send is dropped like OTP
                // (alias messages are converted during this split, before the signal loop runs the
                // 'DOWN'). The signal loop's own deactivation in context_process_monitor_down_signal
                // is then an idempotent no-op.
                if (current->type == MonitorDownSignal) {
                    struct TermSignal *down_signal = CONTAINER_OF(current, struct TermSignal, base);
                    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(down_signal->signal_term, 1));
                    struct MonitorAlias *alias = context_find_alias(ctx, ref_ticks);
                    if (alias != NULL && alias->alias_type != ContextMonitorAliasExplicitUnalias) {
                        context_unalias(ctx, alias);
                    }
                }
                current->next = NULL;
                if (signal_last == NULL) {
                    signal_first = current;
                } else {
                    signal_last->next = current;
                }
                signal_last = current;
            }
            current = next;
        }
    }

    append_normal_messages(mbox, normal_first, normal_last);
    return signal_first;
}

void mailbox_next(Mailbox *mbox)
{
    // This is called from OP_LOOP_REC_END opcode, so we cannot make any
    // assumption about the state and should perform a nop if moving cursor
    // beyond last position.
    if (UNLIKELY(mbox->receive_pointer == NULL)) {
        fprintf(stderr, "OP_LOOP_REC_END beyond mailbox end\n");
        return;
    }

    mbox->receive_pointer_prev = mbox->receive_pointer;
    mbox->receive_pointer = mbox->receive_pointer->next;
}

bool mailbox_peek(Context *c, term *out)
{
    MailboxMessage *m = c->mailbox.receive_pointer;
    if (m == NULL) {
        return false;
    }

    Message *data_message = CONTAINER_OF(m, Message, base);

    TRACE("Pid %i is peeking 0x%lx.\n", c->process_id, data_message->message);

    *out = data_message->message;

    return true;
}

MailboxMessage *mailbox_take_message(Mailbox *mbox)
{
    // This is called from OP_REMOVE_MESSAGE opcode, so we cannot make any
    // assumption about the state and should perform a nop if the mailbox
    // is empty.
    if (UNLIKELY(mbox->receive_pointer == NULL)) {
        fprintf(stderr, "OP_REMOVE_MESSAGE on empty mailbox\n");
        return NULL;
    }
    MailboxMessage *removed = mbox->receive_pointer;
    if (mbox->receive_pointer_prev) {
        // We did not remove first message.
        mbox->receive_pointer_prev->next = removed->next;
        // If we removed last messages, update inner last.
        if (mbox->inner_last == removed) {
            mbox->inner_last = mbox->receive_pointer_prev;
        }
    } else {
        // We did remove first message.
        mbox->inner_first = removed->next;
        if (mbox->inner_first == NULL) {
            // If this also the last, update inner_last.
            mbox->inner_last = NULL;
        }
    }

    // Reset receive pointers
    mailbox_reset(mbox);

    return removed;
}

Message *mailbox_first(Mailbox *mbox)
{
    mailbox_reset(mbox);
    MailboxMessage *msg = mbox->receive_pointer;
    Message *result = NULL;
    if (msg) {
        result = CONTAINER_OF(msg, Message, base);
    }
    return result;
}

void mailbox_crashdump(Context *ctx)
{
    // Signal messages are now in reverse order but the process crashed anyway
    ctx->mailbox.outer_first = mailbox_process_outer_list_native(&ctx->mailbox);
    MailboxMessage *msg = ctx->mailbox.inner_first;
    while (msg) {
        Message *data_message = CONTAINER_OF(msg, Message, base);
        term_display(stderr, data_message->message, ctx);
        fprintf(stderr, "\n");
        msg = msg->next;
    }
}
