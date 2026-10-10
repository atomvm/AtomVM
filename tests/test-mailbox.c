/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

#include <assert.h>
#include <stdlib.h>

#include "context.h"
#include "globalcontext.h"
#include "mailbox.h"
#include "smp.h"

void test_mailbox_send(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    term t;

    assert(!mailbox_has_next(&ctx->mailbox));

    mailbox_send(ctx, term_from_int(1));

    assert(mailbox_process_outer_list_native(&ctx->mailbox) == NULL);
    assert(mailbox_has_next(&ctx->mailbox));
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    assert(!mailbox_has_next(&ctx->mailbox));

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

void test_mailbox_next(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    term t;

    // Various combination of two sends:
    // 1. send send peek next peek remove peek remove
    mailbox_send(ctx, term_from_int(1));
    mailbox_send(ctx, term_from_int(2));
    assert(mailbox_process_outer_list_native(&ctx->mailbox) == NULL);
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    assert(mailbox_has_next(&ctx->mailbox));
    mailbox_next(&ctx->mailbox);

    assert(mailbox_peek(ctx, &t));
    assert(2 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    assert(!mailbox_has_next(&ctx->mailbox));
    assert(mailbox_len(&ctx->mailbox) == 0);

    // 2. send peek send next peek remove peek remove
    mailbox_send(ctx, term_from_int(1));
    assert(mailbox_process_outer_list_native(&ctx->mailbox) == NULL);
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_send(ctx, term_from_int(2));
    assert(mailbox_process_outer_list_native(&ctx->mailbox) == NULL);
    assert(mailbox_has_next(&ctx->mailbox));
    mailbox_next(&ctx->mailbox);

    assert(mailbox_peek(ctx, &t));
    assert(2 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    assert(!mailbox_has_next(&ctx->mailbox));
    assert(mailbox_len(&ctx->mailbox) == 0);

    // 3. send peek next send peek remove peek remove
    mailbox_send(ctx, term_from_int(1));
    assert(mailbox_process_outer_list_native(&ctx->mailbox) == NULL);
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    assert(mailbox_has_next(&ctx->mailbox));
    mailbox_next(&ctx->mailbox);

    mailbox_send(ctx, term_from_int(2));
    assert(mailbox_process_outer_list_native(&ctx->mailbox) == NULL);

    assert(mailbox_peek(ctx, &t));
    assert(2 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    assert(!mailbox_has_next(&ctx->mailbox));
    assert(mailbox_len(&ctx->mailbox) == 0);

    // 4. mailbox_len counts signals, mailbox_normal_message_len does not
    mailbox_send(ctx, term_from_int(1));
    mailbox_send_empty_body_signal(ctx, GCSignal);
    mailbox_send(ctx, term_from_int(2));
    assert(mailbox_len(&ctx->mailbox) == 3);
    assert(mailbox_normal_message_len(&ctx->mailbox) == 2);

    MailboxMessage *signal = mailbox_process_outer_list_native(&ctx->mailbox);
    assert(signal != NULL);
    assert(signal->type == GCSignal);
    assert(signal->next == NULL);
    mailbox_message_dispose(signal, &ctx->heap);

    assert(mailbox_len(&ctx->mailbox) == 2);
    assert(mailbox_normal_message_len(&ctx->mailbox) == 2);

    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    assert(mailbox_peek(ctx, &t));
    assert(2 == term_to_int(t));
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    assert(mailbox_len(&ctx->mailbox) == 0);
    assert(mailbox_normal_message_len(&ctx->mailbox) == 0);

    // 5. mailbox_normal_message_len counts unconverted alias messages
    mailbox_send(ctx, term_from_int(3));
    mailbox_send_term_signal(ctx, AliasMessageSignal, term_from_int(4));
    assert(mailbox_len(&ctx->mailbox) == 2);
    assert(mailbox_normal_message_len(&ctx->mailbox) == 2);

    signal = mailbox_process_outer_list_native(&ctx->mailbox);
    assert(signal != NULL);
    assert(signal->type == AliasMessageSignal);
    assert(signal->next == NULL);
    mailbox_message_dispose(signal, &ctx->heap);

    assert(mailbox_len(&ctx->mailbox) == 1);
    assert(mailbox_normal_message_len(&ctx->mailbox) == 1);

    assert(mailbox_peek(ctx, &t));
    assert(3 == term_to_int(t));
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    assert(mailbox_len(&ctx->mailbox) == 0);
    assert(mailbox_normal_message_len(&ctx->mailbox) == 0);

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

void test_send_message_from_task_order(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    term t;

#ifndef AVM_NO_SMP
    // Held, the lock keeps tasks from enqueuing directly: the messages go
    // through the global queue, as they always do without SMP.
    smp_spinlock_lock(&glb->processes_spinlock);
#endif
    for (int i = 1; i <= 3; i++) {
        globalcontext_send_message_from_task(glb, ctx->process_id, NormalMessage, term_from_int(i));
    }
#ifndef AVM_NO_SMP
    smp_spinlock_unlock(&glb->processes_spinlock);
#endif
    globalcontext_process_task_driver_queues(glb);

    assert(mailbox_process_outer_list_native(&ctx->mailbox) == NULL);
    for (int i = 1; i <= 3; i++) {
        assert(mailbox_peek(ctx, &t));
        assert(i == term_to_int(t));
        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    }
    assert(mailbox_len(&ctx->mailbox) == 0);

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_mailbox_send();
    test_mailbox_next();
    test_send_message_from_task_order();

    return EXIT_SUCCESS;
}
