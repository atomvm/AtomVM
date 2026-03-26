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
#include <stdatomic.h>
#include <stdint.h>
#include <time.h>

#include <erl_nif.h>
#include <list.h>
#include <sys.h>
#include <term_typedef.h>

#include <emscripten.h>
#include <emscripten/fetch.h>
#include <emscripten/promise.h>

#include "sys_mbedtls.h"
#include <mbedtls/version.h>

#if MBEDTLS_VERSION_NUMBER < 0x04000000
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>
#endif

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
    TrackedAnswer,
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

struct EmscriptenMessageTrackedAnswer
{
    struct EmscriptenMessageBase base;
    int32_t target_pid;
    // an invalid term makes the trapped caller raise out_of_memory instead
    term answer;
    // NULL when the answer needed none
    HeapFragment *answer_heap;
};

// Reserved key: sys_get_next_tracked_object_key returns it once every other
// key has been handed out, and it never identifies a tracked object.
#define TRACKED_OBJECT_KEY_EXHAUSTED UINT32_MAX

struct TrackedObjectResource
{
    uint32_t key;
};

struct EmscriptenPlatformData
{
    pthread_mutex_t poll_mutex;
    pthread_cond_t poll_cond;
    struct ListHead messages;
    _Atomic uint32_t next_tracked_object_key;
    ErlNifResourceType *promise_resource_type;
    ErlNifResourceType *htmlevent_user_data_resource_type;
    ErlNifResourceType *websocket_resource_type;
    ErlNifResourceType *tracked_object_resource_type;

#if MBEDTLS_VERSION_NUMBER < 0x04000000
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
#endif
};

void sys_enqueue_emscripten_cast_message(GlobalContext *glb, const char *target, const char *message);
em_promise_t sys_enqueue_emscripten_call_message(GlobalContext *glb, const char *target, const char *message);
void sys_enqueue_emscripten_htmlevent_message(GlobalContext *glb, int32_t target_pid, term message, term user_data, HeapFragment *heap);
void sys_enqueue_emscripten_unregister_htmlevent_message(GlobalContext *glb, struct HTMLEventUserDataResource *rsrc);
void sys_enqueue_emscripten_tracked_answer_message(GlobalContext *glb, int32_t target_pid, term answer, HeapFragment *heap);
uint32_t sys_get_next_tracked_object_key(GlobalContext *glb);
void sys_remove_tracked_object(uint32_t key);
void sys_promise_resolve_int_and_destroy(em_promise_t promise, em_promise_result_t result, int value);
void sys_promise_resolve_str_and_destroy(em_promise_t promise, em_promise_result_t result, int value);

#endif
