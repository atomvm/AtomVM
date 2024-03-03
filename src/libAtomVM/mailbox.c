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
        case TrapAnswerSignal: {
            struct TermSignal *term_signal = CONTAINER_OF(m, struct TermSignal, base);
            term mso_list = term_signal->storage[STORAGE_MSO_LIST_INDEX];
            HeapFragment *fragment = mailbox_message_to_heap_fragment(term_signal, term_signal->heap_end);
            memory_heap_append_fragment(heap, fragment, mso_list);
            break;
        }
        case ProcessInfoRequestSignal: {
            struct BuiltInAtomRequestSignal *request_signal
                = CONTAINER_OF(m, struct BuiltInAtomRequestSignal, base);
            free(request_signal);
            break;
        }
        case TrapExceptionSignal: {
            struct BuiltInAtomSignal *atom_signal = CONTAINER_OF(m, struct BuiltInAtomSignal, base);
            free(atom_signal);
            break;
        }
        case FlushMonitorSignal:
        case FlushInfoMonitorSignal: {
            struct RefSignal *ref_signal = CONTAINER_OF(m, struct RefSignal, base);
            free(ref_signal);
            break;
        }
        case GCSignal:
            free(m);
            break;
    }
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

static void mailbox_post_message(Context *c, MailboxMessage *m)
{
    mailbox_enqueue_message(c, m);
    scheduler_signal_message(c);
}
#else
static void mailbox_post_message(Context *c, MailboxMessage *m)
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

void mailbox_send_built_in_atom_signal(Context *c, enum MessageType type, term atom)
{
    struct BuiltInAtomSignal *atom_signal = malloc(sizeof(struct BuiltInAtomSignal));
    if (IS_NULL_PTR(atom_signal)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    atom_signal->base.type = type;
    atom_signal->atom = atom;

    mailbox_post_message(c, &atom_signal->base);
}

void mailbox_send_built_in_atom_request_signal(
    Context *c, enum MessageType type, int32_t pid, term atom)
{
    struct BuiltInAtomRequestSignal *atom_request = malloc(sizeof(struct BuiltInAtomRequestSignal));
    if (IS_NULL_PTR(atom_request)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    atom_request->base.type = type;
    atom_request->sender_pid = pid;
    atom_request->atom = atom;

    mailbox_post_message(c, &atom_request->base);
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

MailboxMessage *mailbox_process_outer_list(Mailbox *mbox)
{
    // Empty outer list using CAS
    MailboxMessage *current = mbox->outer_first;
#if !defined(AVM_NO_SMP) || defined(AVM_TASK_DRIVER_ENABLED)
    while (!ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(&mbox->outer_first, &current, NULL)) {
    };
#else
    mbox->outer_first = NULL;
#endif
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
    // If we did enqueue some normal messages, lastNormal is the first
    // one in outer list (last received one)
    if (last_normal) {
        // previousNormal is new list head
        // If we had no receive_pointer, it should be this list head
        if (mbox->receive_pointer == NULL) {
            mbox->receive_pointer = previous_normal;
            // If we had a prev, set the prev's next to the new current.
            if (mbox->receive_pointer_prev) {
                mbox->receive_pointer_prev->next = previous_normal;
            } else if (mbox->inner_first == NULL) {
                // If we had no first, this is the first message.
                mbox->inner_first = previous_normal;
            }
        }

        // Update last and previous last's next.
        // Append these new items at the end of inner list.
        if (mbox->inner_last) {
            // This may be mbox->receive_pointer_prev which we
            // are updating a second time here.
            mbox->inner_last->next = previous_normal;
        }
        mbox->inner_last = last_normal;
    }

    return previous_signal;
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
    ctx->mailbox.outer_first = mailbox_process_outer_list(&ctx->mailbox);
    MailboxMessage *msg = ctx->mailbox.inner_first;
    while (msg) {
        Message *data_message = CONTAINER_OF(msg, Message, base);
        term_display(stderr, data_message->message, ctx);
        fprintf(stderr, "\n");
        msg = msg->next;
    }
}
