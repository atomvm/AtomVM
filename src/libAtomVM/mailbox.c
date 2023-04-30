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
#include "trace.h"

#define ADDITIONAL_PROCESSING_MEMORY_SIZE 4

static inline term *mailbox_message_memory(Message *msg)
{
    return &msg->message + 1;
}

void mailbox_send(Context *c, term t)
{
    TRACE("Sending 0x%lx to pid %i\n", t, c->process_id);

    unsigned long estimated_mem_usage = memory_estimate_usage(t);

    Message *m = malloc(sizeof(Message) + estimated_mem_usage * sizeof(term));
    if (IS_NULL_PTR(m)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return;
    }
    m->mso_list = term_nil();

    term *heap_pos = mailbox_message_memory(m);
    m->message = memory_copy_term_tree(&heap_pos, t, &m->mso_list);
    m->msg_memory_size = estimated_mem_usage;

    list_append(&c->mailbox, &m->mailbox_list_head);

    if (c->jump_to_on_restore) {
        c->saved_ip = c->jump_to_on_restore;
        c->jump_to_on_restore = NULL;
    }
    scheduler_make_ready(c->global, c);
}

Message *mailbox_dequeue(Context *c)
{
    Message *m = GET_LIST_ENTRY(list_first(&c->mailbox), Message, mailbox_list_head);
    list_remove(&m->mailbox_list_head);

    TRACE("Pid %i is dequeueing 0x%lx.\n", c->process_id, m->message);

    return m;
}

term mailbox_peek(Context *c)
{
    Message *m = GET_LIST_ENTRY(list_first(&c->mailbox), Message, mailbox_list_head);

    TRACE("Pid %i is peeking 0x%lx.\n", c->process_id, m->message);

    if (c->e - c->heap_ptr < m->msg_memory_size) {
        // ADDITIONAL_PROCESSING_MEMORY_SIZE: ensure some additional memory for message processing, so there is
        // no need to run GC again.
        if (UNLIKELY(memory_ensure_free(c, m->msg_memory_size + ADDITIONAL_PROCESSING_MEMORY_SIZE) != MEMORY_GC_OK)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        }
    }

    term rt = memory_copy_term_tree(&c->heap_ptr, m->message, &c->mso_list);

    return rt;
}

void mailbox_remove(Context *c)
{
    if (UNLIKELY(list_is_empty(&c->mailbox))) {
        TRACE("Pid %i tried to remove a message from an empty mailbox.\n", c->process_id);
        return;
    }

    Message *m = GET_LIST_ENTRY(list_first(&c->mailbox), Message, mailbox_list_head);
    list_remove(&m->mailbox_list_head);

    mailbox_destroy_message(m);
}

void mailbox_destroy_message(Message *m)
{
    memory_sweep_mso_list(m->mso_list);
    free(m);
}
