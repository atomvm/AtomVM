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

#include "context.h"

#include "globalcontext.h"
#include "list.h"
#include "mailbox.h"

#define IMPL_EXECUTE_LOOP
#include "opcodesswitch.h"
#undef IMPL_EXECUTE_LOOP

#define DEFAULT_STACK_SIZE 8
#define BYTES_PER_TERM (TERM_BITS/8)

Context *context_new(GlobalContext *glb)
{
    Context *ctx = malloc(sizeof(Context));
    if (IS_NULL_PTR(ctx)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }
    ctx->cp = 0;

    ctx->heap_start = (term *) calloc(DEFAULT_STACK_SIZE, sizeof(term));
    if (IS_NULL_PTR(ctx->heap_start)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        free(ctx);
        return NULL;
    }
    ctx->stack_base = ctx->heap_start + DEFAULT_STACK_SIZE;
    ctx->e = ctx->stack_base;
    ctx->heap_ptr = ctx->heap_start;

    ctx->avail_registers = 16;
    context_clean_registers(ctx, 0);

    ctx->min_heap_size = 0;
    ctx->max_heap_size = 0;
    ctx->has_min_heap_size = 0;
    ctx->has_max_heap_size = 0;

    list_append(&glb->ready_processes, &ctx->processes_list_head);

    ctx->mailbox = NULL;

    ctx->global = glb;

    ctx->process_id = globalcontext_get_new_process_id(glb);
    linkedlist_append(&glb->processes_table, &ctx->processes_table_head);

    ctx->native_handler = NULL;

    ctx->saved_ip = NULL;
    ctx->jump_to_on_restore = NULL;

    ctx->leader = 0;

    timer_wheel_item_init(&ctx->timer_wheel_head, NULL, 0);
    ctx->timeout_at.tv_sec = 0;
    ctx->timeout_at.tv_nsec = 0;

    #ifdef ENABLE_ADVANCED_TRACE
        ctx->trace_calls = 0;
        ctx->trace_call_args = 0;
        ctx->trace_returns = 0;
        ctx->trace_send = 0;
        ctx->trace_receive = 0;
    #endif

    list_init(&ctx->heap_fragments);
    ctx->heap_fragments_size = 0;

    ctx->platform_data = NULL;

    return ctx;
}

void context_destroy(Context *ctx)
{
    linkedlist_remove(&ctx->global->processes_table, &ctx->processes_table_head);

    free(ctx->heap_start);
    free(ctx);
}

typedef void *(*maibox_iterator)(Message *msg, void *accum);

static void *context_num_messages(Message *msg, void *accum)
{
    UNUSED(msg);

    return (void *) ((size_t) accum + 1);
}

static void *context_message_size(Message *msg, void *accum)
{
    return (void *) (sizeof(Message) + msg->msg_memory_size + (size_t) accum);
}

static void *context_mailbox_iterator(Context *ctx, maibox_iterator fun, void *initial)
{
    if (ctx->mailbox == NULL) {
        return initial;
    }
    Message *messages = GET_LIST_ENTRY(ctx->mailbox, Message, mailbox_list_head);
    Message *m = messages;
    void *accum = initial;
    do {
        accum = fun(m, accum);
        m = GET_LIST_ENTRY(m->mailbox_list_head.next, Message, mailbox_list_head);
    } while (messages != m);
    return accum;
}

size_t context_message_queue_len(Context *ctx)
{
    return (size_t) context_mailbox_iterator(ctx, context_num_messages, NULL);
}

size_t context_size(Context *ctx)
{
    // TODO include ctx->platform_data
    return sizeof(Context)
        + (size_t) context_mailbox_iterator(ctx, context_message_size, NULL)
        + context_memory_size(ctx) * BYTES_PER_TERM;
}
