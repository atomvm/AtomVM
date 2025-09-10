/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 by Paul Guyot <pguyot@kallisys.net>
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

#ifndef _EMSCRIPTEN_SYS_H_
#define _EMSCRIPTEN_SYS_H_

#include <pthread.h>
#include <time.h>

#include <erl_nif.h>
#include <list.h>
#include <sys.h>
#include <term_typedef.h>

#include <emscripten.h>
#include <emscripten/fetch.h>
#include <emscripten/promise.h>

#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>

#if defined(MBEDTLS_VERSION_NUMBER) && (MBEDTLS_VERSION_NUMBER >= 0x03000000)
#include <mbedtls/build_info.h>
#else
#include <mbedtls/config.h>
#endif

#include "sys_mbedtls.h"

struct PromiseResource
{
    em_promise_t promise;
    bool resolved;
};

struct HTMLEventUserDataResource
{
    int32_t target_pid;
    ErlNifMonitor monitor;
    bool prevent_default;
    bool unregistered;
    int event;
    term user_data;
    const char *target_element;
    char *target_element_str; // storage or NULL
    term storage[];
};

enum EmscriptenMessageType
{
    Cast,
    Call,
    HTMLEvent,
    UnregisterHTMLEvent,
    Signal
};

struct EmscriptenMessageBase
{
    struct ListHead message_head;
    enum EmscriptenMessageType message_type;
};

struct EmscriptenMessageCast
{
    struct EmscriptenMessageBase base;
    char *target_name;
    char *message;
};

struct EmscriptenMessageCall
{
    struct EmscriptenMessageBase base;
    char *target_name;
    char *message;
    struct PromiseResource *promise_rsrc;
};

struct EmscriptenMessageHTMLEvent
{
    struct EmscriptenMessageBase base;
    int32_t target_pid;
    term message;
    term user_data;
    HeapFragment *message_heap;
};

struct EmscriptenMessageUnregisterHTMLEvent
{
    struct EmscriptenMessageBase base;
    struct HTMLEventUserDataResource *rsrc;
};

struct EmscriptenPlatformData
{
    pthread_mutex_t poll_mutex;
    pthread_cond_t poll_cond;
    struct ListHead messages;
    ErlNifResourceType *promise_resource_type;
    ErlNifResourceType *htmlevent_user_data_resource_type;
    ErlNifResourceType *websocket_resource_type;

#ifndef AVM_NO_SMP
    Mutex *entropy_mutex;
#endif
    mbedtls_entropy_context entropy_ctx;
    bool entropy_is_initialized;

#ifndef AVM_NO_SMP
    Mutex *random_mutex;
#endif
    mbedtls_ctr_drbg_context random_ctx;
    bool random_is_initialized;
};

void sys_enqueue_emscripten_cast_message(GlobalContext *glb, const char *target, const char *message);
em_promise_t sys_enqueue_emscripten_call_message(GlobalContext *glb, const char *target, const char *message);
void sys_enqueue_emscripten_htmlevent_message(GlobalContext *glb, int32_t target_pid, term message, term user_data, HeapFragment *heap);
void sys_enqueue_emscripten_unregister_htmlevent_message(GlobalContext *glb, struct HTMLEventUserDataResource *rsrc);
void sys_promise_resolve_int_and_destroy(em_promise_t promise, em_promise_result_t result, int value);
void sys_promise_resolve_str_and_destroy(em_promise_t promise, em_promise_result_t result, int value);

#endif
