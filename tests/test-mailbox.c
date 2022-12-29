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

void test_mailbox_send()
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    term t;

    assert(!mailbox_has_next(&ctx->mailbox));

    mailbox_send(ctx, term_from_int(1));

    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    assert(mailbox_has_next(&ctx->mailbox));
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove(&ctx->mailbox);

    assert(!mailbox_has_next(&ctx->mailbox));

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

void test_mailbox_next()
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    term t;

    // Various combination of two sends:
    // 1. send send peek next peek remove peek remove
    mailbox_send(ctx, term_from_int(1));
    mailbox_send(ctx, term_from_int(2));
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    assert(mailbox_has_next(&ctx->mailbox));
    mailbox_next(&ctx->mailbox);

    assert(mailbox_peek(ctx, &t));
    assert(2 == term_to_int(t));

    mailbox_remove(&ctx->mailbox);

    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove(&ctx->mailbox);
    assert(!mailbox_has_next(&ctx->mailbox));
    assert(mailbox_len(&ctx->mailbox) == 0);

    // 2. send peek send next peek remove peek remove
    mailbox_send(ctx, term_from_int(1));
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_send(ctx, term_from_int(2));
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    assert(mailbox_has_next(&ctx->mailbox));
    mailbox_next(&ctx->mailbox);

    assert(mailbox_peek(ctx, &t));
    assert(2 == term_to_int(t));

    mailbox_remove(&ctx->mailbox);

    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove(&ctx->mailbox);
    assert(!mailbox_has_next(&ctx->mailbox));
    assert(mailbox_len(&ctx->mailbox) == 0);

    // 3. send peek next send peek remove peek remove
    mailbox_send(ctx, term_from_int(1));
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    assert(mailbox_has_next(&ctx->mailbox));
    mailbox_next(&ctx->mailbox);

    mailbox_send(ctx, term_from_int(2));
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);

    assert(mailbox_peek(ctx, &t));
    assert(2 == term_to_int(t));

    mailbox_remove(&ctx->mailbox);

    assert(mailbox_peek(ctx, &t));
    assert(1 == term_to_int(t));

    mailbox_remove(&ctx->mailbox);
    assert(!mailbox_has_next(&ctx->mailbox));
    assert(mailbox_len(&ctx->mailbox) == 0);

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

void test_mailbox_api_for_port_drivers()
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    mailbox_send(ctx, term_from_int(1));
    mailbox_send(ctx, term_from_int(2));
    mailbox_send(ctx, term_from_int(3));
    mailbox_send(ctx, term_from_int(4));
    mailbox_send(ctx, term_from_int(5));

    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    Message *msg1;
    assert(mailbox_peek_message(&ctx->mailbox, &msg1));
    assert(1 == term_to_int(msg1->message));

    // mailbox_peek_message doesn't move next
    Message *msg1b;
    assert(mailbox_peek_message(&ctx->mailbox, &msg1b));
    assert(1 == term_to_int(msg1b->message));

    mailbox_remove_message(&ctx->mailbox, msg1);

    Message *msg2;
    assert(mailbox_peek_message(&ctx->mailbox, &msg2));
    assert(2 == term_to_int(msg2->message));

    mailbox_next(&ctx->mailbox);

    Message *msg3;
    assert(mailbox_peek_message(&ctx->mailbox, &msg3));
    assert(3 == term_to_int(msg3->message));

    mailbox_remove_message(&ctx->mailbox, msg3);

    Message *msg4;
    assert(mailbox_peek_message(&ctx->mailbox, &msg4));
    assert(4 == term_to_int(msg4->message));

    Message *msg2b = mailbox_first(&ctx->mailbox);
    assert(msg2 == msg2b);

    mailbox_remove(&ctx->mailbox);

    Message *msg4b = mailbox_first(&ctx->mailbox);
    assert(msg4 == msg4b);

    mailbox_next(&ctx->mailbox);

    Message *msg5;
    assert(mailbox_peek_message(&ctx->mailbox, &msg5));
    assert(5 == term_to_int(msg5->message));

    mailbox_remove_message(&ctx->mailbox, msg5);

    Message *no_next = msg5;
    assert(mailbox_peek_message(&ctx->mailbox, &no_next) == false);
    // if there is no next, the pointer must not be updated
    assert(msg5 == no_next);

    Message *msg2c = mailbox_first(&ctx->mailbox);
    assert(msg2c != NULL);
    assert(term_to_int(msg2c->message) == 4);

    mailbox_remove_message(&ctx->mailbox, msg2c);

    assert(mailbox_peek_message(&ctx->mailbox, &no_next) == false);
    assert(mailbox_first(&ctx->mailbox) == NULL);
    assert(mailbox_len(&ctx->mailbox) == 0);

    mailbox_send(ctx, term_from_int(1));
    mailbox_send(ctx, term_from_int(2));
    mailbox_send(ctx, term_from_int(3));
    mailbox_send(ctx, term_from_int(4));
    mailbox_send(ctx, term_from_int(5));
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);

    assert(mailbox_first(&ctx->mailbox) != NULL);
    mailbox_next(&ctx->mailbox);

    Message *msg2d;
    assert(mailbox_peek_message(&ctx->mailbox, &msg2d));
    assert(term_to_int(msg2d->message) == 2);

    mailbox_next(&ctx->mailbox);

    mailbox_remove_message(&ctx->mailbox, msg2d);

    mailbox_remove(&ctx->mailbox);

    mailbox_next(&ctx->mailbox);

    Message *msg4c;
    assert(mailbox_peek_message(&ctx->mailbox, &msg4c));
    assert(term_to_int(msg4c->message) == 4);

    assert(mailbox_len(&ctx->mailbox) == 3);

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_mailbox_send();
    test_mailbox_next();
    test_mailbox_api_for_port_drivers();

    return EXIT_SUCCESS;
}
