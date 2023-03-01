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
#include "memory.h"
#include "scheduler.h"
#include "synclist.h"
#include "trace.h"

#define ADDITIONAL_PROCESSING_MEMORY_SIZE 4

static inline term *mailbox_message_memory(term *msg_term)
{
    return msg_term + 1;
}

void mailbox_init(Mailbox *mbx)
{
    mbx->outer_first = NULL;
    mbx->inner_first = NULL;
    mbx->inner_last = NULL;
    mbx->receive_pointer = NULL;
    mbx->receive_pointer_prev = NULL;
}

// Generic destroy function name refers to signal message to make sure ports do
// not call it. They should call mailbox_remove instead.
void mailbox_destroy_signal_message(MailboxMessage *m)
{
    switch (m->type) {
        case NormalMessage: {
            Message *normal_message = CONTAINER_OF(m, Message, base);
            memory_sweep_mso_list(normal_message->mso_list);
            free(normal_message);
            break;
        }
        case KillSignal:
        case TrapAnswerSignal: {
            struct TermSignal *term_signal = CONTAINER_OF(m, struct TermSignal, base);
            memory_sweep_mso_list(term_signal->mso_list);
            free(term_signal);
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
        case GCSignal:
            free(m);
            break;
    }
}

static inline void mailbox_destroy_message(MailboxMessage *m)
{
    mailbox_destroy_signal_message(m);
}

void mailbox_destroy(Mailbox *mbox)
{
    MailboxMessage *msg = mbox->outer_first;
    while (msg) {
        MailboxMessage *next = msg->next;
        mailbox_destroy_message(msg);
        msg = next;
    }
    msg = mbox->inner_first;
    while (msg) {
        MailboxMessage *next = msg->next;
        mailbox_destroy_message(msg);
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
            result += sizeof(Message) + normal_message->msg_memory_size;
        }
        msg = msg->next;
    }
    msg = mbox->inner_first;
    while (msg) {
        Message *normal_message = CONTAINER_OF(msg, Message, base);
        result += sizeof(Message) + normal_message->msg_memory_size;
        msg = msg->next;
    }
    return result;
}

static void mailbox_post_message(Context *c, MailboxMessage *m)
{
    m->next = NULL;

    // Append message at the beginning of outer_first.
#ifndef AVM_NO_SMP
    MailboxMessage *current_first = NULL;
    do {
        m->next = current_first;
    } while (!atomic_compare_exchange_weak(&c->mailbox.outer_first, &current_first, m));
#else
    m->next = c->mailbox.outer_first;
    c->mailbox.outer_first = m;
#endif

    scheduler_signal_message(c);
}

void mailbox_send(Context *c, term t)
{
    TRACE("Sending 0x%lx to pid %i\n", t, c->process_id);

    unsigned long estimated_mem_usage = memory_estimate_usage(t);

    Message *msg = malloc(sizeof(Message) + estimated_mem_usage * sizeof(term));
    if (IS_NULL_PTR(msg)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    msg->mso_list = term_nil();

    term *heap_pos = mailbox_message_memory(&msg->message);
    msg->base.type = NormalMessage;
    msg->message = memory_copy_term_tree(&heap_pos, t, &msg->mso_list);
    msg->msg_memory_size = estimated_mem_usage;

    mailbox_post_message(c, &msg->base);
}

void mailbox_send_term_signal(Context *c, enum MessageType type, term t)
{
    unsigned long estimated_mem_usage = memory_estimate_usage(t);

    struct TermSignal *ts = malloc(sizeof(struct TermSignal) + estimated_mem_usage * sizeof(term));
    if (IS_NULL_PTR(ts)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    ts->mso_list = term_nil();

    term *heap_pos = mailbox_message_memory(&ts->signal_term);
    ts->base.type = type;
    ts->signal_term = memory_copy_term_tree(&heap_pos, t, &ts->mso_list);
    ts->msg_memory_size = estimated_mem_usage;

    mailbox_post_message(c, &ts->base);
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
#ifndef AVM_NO_SMP
    while (!atomic_compare_exchange_weak(&mbox->outer_first, &current, NULL)) {
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

    if (c->e - c->heap_ptr < data_message->msg_memory_size) {
        // ADDITIONAL_PROCESSING_MEMORY_SIZE: ensure some additional memory for message processing,
        // so there is no need to run GC again.
        if (UNLIKELY(memory_gc(c,
                         context_memory_size(c) + data_message->msg_memory_size
                             + ADDITIONAL_PROCESSING_MEMORY_SIZE)
                != MEMORY_GC_OK)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        }
    }

    *out = memory_copy_term_tree(&c->heap_ptr, data_message->message, &c->mso_list);

    return true;
}

void mailbox_remove(Mailbox *mbox)
{
    // This is called from OP_REMOVE_MESSAGE opcode, so we cannot make any
    // assumption about the state and should perform a nop if the mailbox
    // is empty.
    if (UNLIKELY(mbox->receive_pointer == NULL)) {
        fprintf(stderr, "OP_REMOVE_MESSAGE on empty mailbox\n");
        return;
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

    mailbox_destroy_message(removed);
    // Reset receive pointers
    mailbox_reset(mbox);
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
