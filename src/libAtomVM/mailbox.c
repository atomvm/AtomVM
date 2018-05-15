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

#define ADDITIONAL_PROCESSING_MEMORY_SIZE 4

void mailbox_send(Context *c, term t)
{
    Message *m = malloc(sizeof(Message));
    if (IS_NULL_PTR(m)) {
        fprintf(stderr, "Failed to allocate message memory\n");
        return;
    }

    int stack_slots;
    unsigned long estimated_size = memory_estimate_term_memory_usage(t, &stack_slots) + stack_slots;

    term *msg_heap = calloc(estimated_size, sizeof(term));
    if (IS_NULL_PTR(m)) {
        fprintf(stderr, "Failed to allocate message memory\n");
        return;
    }
    term *heap_pos = msg_heap;
    term *stack_pos = msg_heap + estimated_size;
    m->message = memory_copy_term_tree(&heap_pos, &stack_pos, t, 0);
    m->msg_memory = msg_heap;
    m->msg_memory_size = heap_pos - msg_heap;

    linkedlist_append(&c->mailbox, &m->mailbox_list_head);

    scheduler_make_ready(c->global, c);
}

term mailbox_receive(Context *c)
{
    Message *m = GET_LIST_ENTRY(c->mailbox, Message, mailbox_list_head);
    linkedlist_remove(&c->mailbox, &m->mailbox_list_head);

    if (c->e - c->heap_ptr < m->msg_memory_size) {
        //ADDITIONAL_PROCESSING_MEMORY_SIZE: ensure some additional memory for message processing, so there is
        //no need to run GC again.
        memory_gc(c, context_memory_size(c) + m->msg_memory_size + ADDITIONAL_PROCESSING_MEMORY_SIZE);
    }

    term rt = memory_copy_term_tree(&c->heap_ptr, &c->e, m->message, 0);

    free(m->msg_memory);
    free(m);

    return rt;
}

term mailbox_0copy_receive(Context *c, void **msg_term_mem)
{
    Message *m = GET_LIST_ENTRY(c->mailbox, Message, mailbox_list_head);
    linkedlist_remove(&c->mailbox, &m->mailbox_list_head);

    term message_term = m->message;

    *msg_term_mem = m->msg_memory;
    free(m);

    return message_term;
}

term mailbox_peek(Context *c)
{
    Message *m = GET_LIST_ENTRY(c->mailbox, Message, mailbox_list_head);

    return m->message;
}
