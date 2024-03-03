/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Fred Dushin <fred@dushin.net>
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

#ifndef _PORT_H_
#define _PORT_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "context.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "memory.h"
#include "term.h"

// Sometimes ports need to send messages while not executed in a given ctx
// (typically event handlers), so the helpers are available with a Heap
term port_heap_create_tuple2(Heap *heap, term a, term b);
term port_heap_create_tuple3(Heap *heap, term a, term b, term c);
term port_heap_create_tuple_n(Heap *heap, size_t num_terms, term *terms);
term port_heap_create_error_tuple(Heap *heap, term reason);
term port_heap_create_sys_error_tuple(Heap *heap, term syscall, int err);
term port_heap_create_ok_tuple(Heap *heap, term t);
term port_heap_create_reply(Heap *heap, term ref, term payload);

// All port_* functions with a given ctx may only be called from the
// executed ctx.
static inline term port_create_tuple2(Context *ctx, term a, term b)
{
    return port_heap_create_tuple2(&ctx->heap, a, b);
}
static inline term port_create_tuple3(Context *ctx, term a, term b, term c)
{
    return port_heap_create_tuple3(&ctx->heap, a, b, c);
}
static inline term port_create_tuple_n(Context *ctx, size_t num_terms, term *terms)
{
    return port_heap_create_tuple_n(&ctx->heap, num_terms, terms);
}
static inline term port_create_error_tuple(Context *ctx, term reason)
{
    return port_heap_create_error_tuple(&ctx->heap, reason);
}
static inline term port_create_sys_error_tuple(Context *ctx, term syscall, int err)
{
    return port_heap_create_sys_error_tuple(&ctx->heap, syscall, err);
}
static inline term port_create_ok_tuple(Context *ctx, term t)
{
    return port_heap_create_ok_tuple(&ctx->heap, t);
}
static inline term port_create_reply(Context *ctx, term ref, term payload)
{
    return port_heap_create_reply(&ctx->heap, ref, payload);
}

/**
 * @brief Send a message to a given pid. This function must be called from
 * a driver's native handler.
 *
 * @param glb the global context
 * @param pid the pid of the process to send a message to.
 * @param msg the message to send.
 */
void port_send_message(GlobalContext *glb, term pid, term msg);

/**
 * @brief Send a message to a given pid. This function must be called from
 * a driver's native handler after a target context has been locked.
 *
 * @param glb the global context
 * @param pid the pid of the process to send a message to.
 * @param msg the message to send.
 */
void port_send_message_nolock(GlobalContext *glb, term pid, term msg);

#ifdef AVM_TASK_DRIVER_ENABLED
/**
 * @brief Send a message to a given pid. This function can be called from
 * a driver's task, such as an event callback.
 *
 * @param glb the global context
 * @param pid the pid of the process to send a message to.
 * @param msg the message to send.
 */
void port_send_message_from_task(GlobalContext *glb, term pid, term msg);
#endif

void port_ensure_available(Context *ctx, size_t size);

// Helper to send a message from NIFs or from the native handler.
static inline void port_send_reply(Context *ctx, term pid, term ref, term payload)
{
    term reply = port_create_reply(ctx, ref, payload);
    port_send_message(ctx->global, pid, reply);
}

typedef struct
{
    term req;

    term pid;
    term ref;
} GenMessage;

enum GenMessageParseResult
{
    GenCallMessage,
    GenMessageParseError
};

enum GenMessageParseResult port_parse_gen_message(term msg, GenMessage *gen_message);

#ifdef __cplusplus
}
#endif

#endif
