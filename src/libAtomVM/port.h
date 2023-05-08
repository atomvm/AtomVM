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

#include <stdbool.h>

#include "context.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "term.h"

// Constants to use with port_ensure_available*
#define PORT_ERROR_TUPLE_SIZE TUPLE_SIZE(2)
#define PORT_SYS_ERROR_TUPLE_SIZE TUPLE_SIZE(2) + PORT_ERROR_TUPLE_SIZE
#define PORT_OK_TUPLE_SIZE TUPLE_SIZE(2)
#define PORT_REPLY_SIZE TUPLE_SIZE(2)

// All port_* functions with a given ctx may only be called from the
// executed ctx.
term port_create_tuple2(Context *ctx, term a, term b);
term port_create_tuple3(Context *ctx, term a, term b, term c);
term port_create_tuple_n(Context *ctx, size_t num_terms, term *terms);
term port_create_error_tuple(Context *ctx, term reason);
term port_create_sys_error_tuple(Context *ctx, term syscall, int errno);
term port_create_ok_tuple(Context *ctx, term t);
term port_create_reply(Context *ctx, term ref, term payload);
void port_send_message(GlobalContext *glb, term pid, term msg);
void port_send_message_nolock(GlobalContext *glb, term pid, term msg);
void port_ensure_available_with_roots(Context *ctx, size_t size, size_t num_roots, term *roots, enum MemoryShrinkMode shrink_mode);
static inline void port_ensure_available(Context *ctx, size_t size)
{
    port_ensure_available_with_roots(ctx, size, 0, NULL, MEMORY_NO_SHRINK);
}
bool port_is_standard_port_command(term msg);

// Sometimes ports need to send messages while not executed in a given ctx
// (typically event handlers).
term port_heap_create_tuple2(term **heap_ptr, term a, term b);
term port_heap_create_tuple3(term **heap_ptr, term a, term b, term c);
term port_heap_create_tuple_n(term **heap_ptr, size_t num_terms, term *terms);
term port_heap_create_error_tuple(term **heap_ptr, term reason);
term port_heap_create_sys_error_tuple(term **heap_ptr, term syscall, int errno);
term port_heap_create_ok_tuple(term **heap_ptr, term t);
term port_heap_create_reply(term **heap_ptr, term ref, term payload);

/**
 * @brief Send a reply to a process, typically a port client
 * @details Send a reply tuple `{Ref, Payload}` to process pid. This function
 * ensures memory is available, eventually garbage collecting terms except
 * `ref` and `payload`.
 * @param ctx the current context (native handler for ports)
 * @param pid the pid to send the reply to
 * @param ref the ref that tags the reply
 * @param payload reply payload
 */
static inline void port_send_reply(Context *ctx, term pid, term ref, term payload)
{
    term roots[2];
    roots[0] = ref;
    roots[1] = payload;
    port_ensure_available_with_roots(ctx, PORT_REPLY_SIZE, 2, roots, MEMORY_NO_SHRINK);
    term reply = port_create_reply(ctx, ref, payload);
    port_send_message(ctx->global, pid, reply);
}

/**
 * @brief Send an ok tuple to a process, typically a port client
 * @details Send a reply tuple `{Ref, {ok, T}}` to process pid. This
 * function ensures memory is available, eventually garbage collecting terms
 * except `ref` and `t`.
 * @param ctx the current context (native handler for ports)
 * @param pid the pid to send the reply to
 * @param ref the ref that tags the reply
 * @param t term returned with the ok tuple (can be an atom or any term)
 */
static inline void port_send_ok_tuple(Context *ctx, term pid, term ref, term t)
{
    term roots[2];
    roots[0] = ref;
    roots[1] = t;
    port_ensure_available_with_roots(ctx, PORT_REPLY_SIZE + PORT_OK_TUPLE_SIZE, 2, roots, MEMORY_NO_SHRINK);
    term payload = port_create_ok_tuple(ctx, t);
    term reply = port_create_reply(ctx, ref, payload);
    port_send_message(ctx->global, pid, reply);
}

/**
 * @brief Send an error tuple to a process, typically a port client
 * @details Send a reply tuple `{Ref, {error, Reason}}` to process pid. This
 * function ensures memory is available, eventually garbage collecting terms
 * except `ref` and `reason`.
 * @param ctx the current context (native handler for ports)
 * @param pid the pid to send the reply to
 * @param ref the ref that tags the reply
 * @param reason error reason (can be an atom or any term)
 */
static inline void port_send_error_tuple(Context *ctx, term pid, term ref, term reason)
{
    term roots[2];
    roots[0] = ref;
    roots[1] = reason;
    port_ensure_available_with_roots(ctx, PORT_REPLY_SIZE + PORT_ERROR_TUPLE_SIZE, 2, roots, MEMORY_NO_SHRINK);
    term payload = port_create_error_tuple(ctx, reason);
    term reply = port_create_reply(ctx, ref, payload);
    port_send_message(ctx->global, pid, reply);
}

/**
 * @brief Send a sys error tuple to a process, typically a port client
 * @details Send a reply tuple `{Ref, {error, {Syscall, Errno}}` to process pid.
 * This function ensures memory is available, eventually garbage collecting
 * terms except `ref` and `syscall`.
 * @param ctx the current context (native handler for ports)
 * @param pid the pid to send the reply to
 * @param ref the ref that tags the reply
 * @param syscall error syscall (can be an atom or any term)
 * @param errno error number
 */
static inline void port_send_sys_error_tuple(Context *ctx, term pid, term ref, term syscall, int errno)
{
    term roots[2];
    roots[0] = ref;
    roots[1] = syscall;
    port_ensure_available_with_roots(ctx, PORT_REPLY_SIZE + PORT_SYS_ERROR_TUPLE_SIZE, 2, roots, MEMORY_NO_SHRINK);
    term payload = port_create_sys_error_tuple(ctx, syscall, errno);
    term reply = port_create_reply(ctx, ref, payload);
    port_send_message(ctx->global, pid, reply);
}

#ifdef __cplusplus
}
#endif

#endif
