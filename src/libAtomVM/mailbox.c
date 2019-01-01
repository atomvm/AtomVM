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

    term *heap_pos = mailbox_message_memory(m);
    m->message = memory_copy_term_tree(&heap_pos, t);
    m->msg_memory_size = estimated_mem_usage;

    linkedlist_append(&c->mailbox, &m->mailbox_list_head);

    if (c->jump_to_on_restore) {
        c->saved_ip = c->jump_to_on_restore;
        c->jump_to_on_restore = NULL;
    }
    scheduler_make_ready(c->global, c);
}

term mailbox_receive(Context *c)
{
    Message *m = GET_LIST_ENTRY(c->mailbox, Message, mailbox_list_head);
    linkedlist_remove(&c->mailbox, &m->mailbox_list_head);

    if (c->e - c->heap_ptr < m->msg_memory_size) {
        //ADDITIONAL_PROCESSING_MEMORY_SIZE: ensure some additional memory for message processing, so there is
        //no need to run GC again.
        if (UNLIKELY(memory_gc(c, context_memory_size(c) + m->msg_memory_size + ADDITIONAL_PROCESSING_MEMORY_SIZE) != MEMORY_GC_OK)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        }
    }

    term rt = memory_copy_term_tree(&c->heap_ptr, m->message);

    free(m);

    TRACE("Pid %i is receiving 0x%lx.\n", c->process_id, rt);

    return rt;
}

Message *mailbox_dequeue(Context *c)
{
    Message *m = GET_LIST_ENTRY(c->mailbox, Message, mailbox_list_head);
    linkedlist_remove(&c->mailbox, &m->mailbox_list_head);

    TRACE("Pid %i is dequeueing 0x%lx.\n", c->process_id, m->message);

    return m;
}

term mailbox_peek(Context *c)
{
    Message *m = GET_LIST_ENTRY(c->mailbox, Message, mailbox_list_head);

    TRACE("Pid %i is peeking 0x%lx.\n", c->process_id, m->message);

    if (c->e - c->heap_ptr < m->msg_memory_size) {
        //ADDITIONAL_PROCESSING_MEMORY_SIZE: ensure some additional memory for message processing, so there is
        //no need to run GC again.
        if (UNLIKELY(memory_gc(c, context_memory_size(c) + m->msg_memory_size + ADDITIONAL_PROCESSING_MEMORY_SIZE) != MEMORY_GC_OK)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        }
    }

    term rt = memory_copy_term_tree(&c->heap_ptr, m->message);

    return rt;
}

void mailbox_remove(Context *c)
{
    if (!c->mailbox) {
        TRACE("Pid %i tried to remove a message from an empty mailbox.\n", c->process_id);
        return;
    }

    Message *m = GET_LIST_ENTRY(c->mailbox, Message, mailbox_list_head);
    linkedlist_remove(&c->mailbox, &m->mailbox_list_head);

    TRACE("Pid %i is removing a message.\n", c->process_id);

    free(m);
}
