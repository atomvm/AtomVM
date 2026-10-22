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
#include "erl_nif_priv.h"
#include "globalcontext.h"
#include "mailbox.h"

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

#ifdef AVM_TASK_DRIVER_ENABLED
static unsigned task_resource_destroyed;

static void task_resource_dtor(ErlNifEnv *env, void *obj)
{
    UNUSED(env);
    UNUSED(obj);
    task_resource_destroyed++;
}

void test_task_driver_dead_recipient(enum MessageType message_type)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, glb);
    const ErlNifResourceTypeInit init = {
        .members = 1,
        .dtor = task_resource_dtor,
    };
    ErlNifResourceType *type = enif_init_resource_type(
        &env, "task_driver_test", &init, ERL_NIF_RT_CREATE, NULL);
    assert(type != NULL);
    void *resource = enif_alloc_resource(type, 1);
    assert(resource != NULL);
    task_resource_destroyed = 0;

    BEGIN_WITH_STACK_HEAP(TERM_BOXED_REFERENCE_RESOURCE_SIZE, heap);
    term value = term_from_resource(resource, &heap);
    enif_release_resource(resource);

    // Force deferred delivery in SMP builds too.
    SMP_SPINLOCK_LOCK(&glb->processes_spinlock);
    globalcontext_send_message_from_task(glb, ctx->process_id, message_type, value);
    globalcontext_send_message_from_task(glb, ctx->process_id, message_type, value);
    SMP_SPINLOCK_UNLOCK(&glb->processes_spinlock);
    END_WITH_STACK_HEAP(heap, glb);

    context_destroy(ctx);
    assert(task_resource_destroyed == 0);
    globalcontext_process_task_driver_queues(glb);
    assert(glb->message_queue == NULL);
    assert(task_resource_destroyed == 1);
    globalcontext_destroy(glb);
}

void test_task_driver_message_order(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    // Force the deferred path even when SMP permits direct delivery.
    SMP_SPINLOCK_LOCK(&glb->processes_spinlock);
    globalcontext_send_message_from_task(glb, ctx->process_id, NormalMessage, term_from_int(1));
    globalcontext_send_message_from_task(glb, ctx->process_id, NormalMessage, term_from_int(2));
    SMP_SPINLOCK_UNLOCK(&glb->processes_spinlock);
    globalcontext_process_task_driver_queues(glb);

    assert(mailbox_process_outer_list_native(&ctx->mailbox) == NULL);
    term value;
    assert(mailbox_peek(ctx, &value) && term_to_int(value) == 1);
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    assert(mailbox_peek(ctx, &value) && term_to_int(value) == 2);
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    assert(!mailbox_has_next(&ctx->mailbox));

    context_destroy(ctx);
    globalcontext_destroy(glb);
}
#endif

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_mailbox_send();
    test_mailbox_next();
#ifdef AVM_TASK_DRIVER_ENABLED
    test_task_driver_message_order();
    test_task_driver_dead_recipient(NormalMessage);
    test_task_driver_dead_recipient(TrapAnswerSignal);
#endif

    return EXIT_SUCCESS;
}
