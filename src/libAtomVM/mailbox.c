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

#define GET_MAILBOX_MESSAGE(message) \
    ((MailboxMessage *) (((char *) (message)) - ((unsigned long) &((MailboxMessage *) 0)->body.normal)))

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
    if (m->header.type == NormalMessage) {
        memory_sweep_mso_list(m->body.normal.mso_list);
    } else if (m->header.type == KillSignal || m->header.type == TrapAnswerSignal) {
        memory_sweep_mso_list(m->body.term.mso_list);
    }
    free(m);
}

static inline void mailbox_destroy_mailbox_message(MailboxMessage *m)
{
    mailbox_destroy_signal_message(m);
}

void mailbox_destroy_message(Message *m)
{
    MailboxMessage *mboxmsg = GET_MAILBOX_MESSAGE(m);
    mailbox_destroy_mailbox_message(mboxmsg);
}

void mailbox_destroy(Mailbox *mbox)
{
    MailboxMessage *msg = mbox->outer_first;
    while (msg) {
        MailboxMessage *next = msg->header.next;
        mailbox_destroy_mailbox_message(msg);
        msg = next;
    }
    msg = mbox->inner_first;
    while (msg) {
        MailboxMessage *next = msg->header.next;
        mailbox_destroy_mailbox_message(msg);
        msg = next;
    }
}

size_t mailbox_len(Mailbox *mbox)
{
    size_t result = 0;
    MailboxMessage *msg = mbox->outer_first;
    while (msg) {
        result++;
        msg = msg->header.next;
    }
    msg = mbox->inner_first;
    while (msg) {
        result++;
        msg = msg->header.next;
    }
    return result;
}

size_t mailbox_size(Mailbox *mbox)
{
    size_t result = 0;
    MailboxMessage *msg = mbox->outer_first;
    while (msg) {
        // We don't count signals.
        if (msg->header.type == NormalMessage) {
            result += sizeof(MailboxMessage) + msg->body.normal.msg_memory_size;
        }
        msg = msg->header.next;
    }
    msg = mbox->inner_first;
    while (msg) {
        result += sizeof(MailboxMessage) + msg->body.normal.msg_memory_size;
        msg = msg->header.next;
    }
    return result;
}

static void mailbox_post_message(Context *c, MailboxMessage *m)
{
    m->header.next = NULL;

    // Append message at the beginning of outer_first.
#ifndef AVM_NO_SMP
    MailboxMessage *current_first = NULL;
    do {
        m->header.next = current_first;
    } while (!atomic_compare_exchange_weak(&c->mailbox.outer_first, &current_first, m));
#else
    m->header.next = c->mailbox.outer_first;
    c->mailbox.outer_first = m;
#endif

    scheduler_signal_message(c);
}

void mailbox_send(Context *c, term t)
{
    TRACE("Sending 0x%lx to pid %i\n", t, c->process_id);

    unsigned long estimated_mem_usage = memory_estimate_usage(t);

    MailboxMessage *m = malloc(sizeof(struct MessageHeader) + sizeof(struct Message) + estimated_mem_usage * sizeof(term));
    if (IS_NULL_PTR(m)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    m->body.normal.mso_list = term_nil();

    term *heap_pos = mailbox_message_memory(&m->body.normal.message);
    m->header.type = NormalMessage;
    m->body.normal.message = memory_copy_term_tree(&heap_pos, t, &m->body.normal.mso_list);
    m->body.normal.msg_memory_size = estimated_mem_usage;

    mailbox_post_message(c, m);
}

void mailbox_send_term_signal(Context *c, enum MessageType type, term t)
{
    unsigned long estimated_mem_usage = memory_estimate_usage(t);

    MailboxMessage *m = malloc(sizeof(struct MessageHeader) + sizeof(struct TermSignal) + estimated_mem_usage * sizeof(term));
    if (IS_NULL_PTR(m)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    m->body.term.mso_list = term_nil();

    term *heap_pos = mailbox_message_memory(&m->body.term.signal_term);
    m->header.type = type;
    m->body.term.signal_term = memory_copy_term_tree(&heap_pos, t, &m->body.term.mso_list);
    m->body.term.msg_memory_size = estimated_mem_usage;

    mailbox_post_message(c, m);
}

void mailbox_send_built_in_atom_signal(Context *c, enum MessageType type, term atom)
{
    MailboxMessage *m = malloc(sizeof(struct MessageHeader) + sizeof(struct BuiltInAtomSignal));
    if (IS_NULL_PTR(m)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    m->header.type = type;
    m->body.atom.atom = atom;

    mailbox_post_message(c, m);
}

void mailbox_send_built_in_atom_request_signal(Context *c, enum MessageType type, int32_t pid, term atom)
{
    MailboxMessage *m = malloc(sizeof(struct MessageHeader) + sizeof(struct BuiltInAtomRequestSignal));
    if (IS_NULL_PTR(m)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    m->header.type = type;
    m->body.atom_request.sender_pid = pid;
    m->body.atom_request.atom = atom;

    mailbox_post_message(c, m);
}

void mailbox_send_empty_body_signal(Context *c, enum MessageType type)
{
    MailboxMessage *m = malloc(sizeof(struct MessageHeader));
    if (IS_NULL_PTR(m)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    m->header.type = type;

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
        MailboxMessage *next = current->header.next;
        if (current->header.type == NormalMessage) {
            // Get last normal to update inner_last.
            if (last_normal == NULL) {
                last_normal = current;
            }
            current->header.next = previous_normal;
            previous_normal = current;
        } else {
            current->header.next = previous_signal;
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
                mbox->receive_pointer_prev->header.next = previous_normal;
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
            mbox->inner_last->header.next = previous_normal;
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
    mbox->receive_pointer = mbox->receive_pointer->header.next;
}

bool mailbox_peek(Context *c, term *out)
{
    MailboxMessage *m = c->mailbox.receive_pointer;
    if (m == NULL) {
        return false;
    }

    TRACE("Pid %i is peeking 0x%lx.\n", c->process_id, m->message);

    if (c->e - c->heap_ptr < m->body.normal.msg_memory_size) {
        // ADDITIONAL_PROCESSING_MEMORY_SIZE: ensure some additional memory for message processing, so there is
        // no need to run GC again.
        if (UNLIKELY(memory_gc(c, context_memory_size(c) + m->body.normal.msg_memory_size + ADDITIONAL_PROCESSING_MEMORY_SIZE) != MEMORY_GC_OK)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        }
    }

    *out = memory_copy_term_tree(&c->heap_ptr, m->body.normal.message, &c->mso_list);

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
        mbox->receive_pointer_prev->header.next = removed->header.next;
        // If we removed last messages, update inner last.
        if (mbox->inner_last == removed) {
            mbox->inner_last = mbox->receive_pointer_prev;
        }
    } else {
        // We did remove first message.
        mbox->inner_first = removed->header.next;
        if (mbox->inner_first == NULL) {
            // If this also the last, update inner_last.
            mbox->inner_last = NULL;
        }
    }

    mailbox_destroy_mailbox_message(removed);
    // Reset receive pointers
    mailbox_reset(mbox);
}

Message *mailbox_first(Mailbox *mbox)
{
    mailbox_reset(mbox);
    MailboxMessage *msg = mbox->receive_pointer;
    Message *result = NULL;
    if (msg) {
        result = &msg->body.normal;
    }
    return result;
}

Message *mailbox_take_first(Mailbox *mbox)
{
    mailbox_reset(mbox);
    MailboxMessage *msg = mbox->receive_pointer;
    Message *first = NULL;
    if (msg) {
        first = &msg->body.normal;

        mbox->inner_first = msg->header.next;
        mbox->receive_pointer = msg->header.next;
        if (mbox->inner_first == NULL) {
            // If this also the last, update inner_last.
            mbox->inner_last = NULL;
        }
    }

    return first;
}

void mailbox_crashdump(Context *ctx)
{
    // Signal messages are now in reverse order but the process crashed anyway
    ctx->mailbox.outer_first = mailbox_process_outer_list(&ctx->mailbox);
    MailboxMessage *msg = ctx->mailbox.inner_first;
    while (msg) {
        term_display(stderr, msg->body.normal.message, ctx);
        fprintf(stderr, "\n");
        msg = msg->header.next;
    }
}
