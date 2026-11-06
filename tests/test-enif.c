/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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
#include <errno.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <unistd.h>

#include "context.h"
#include "defaultatoms.h"
#include "dictionary.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "external_term.h"
#include "generic_unix_sys.h"
#include "globalcontext.h"
#include "list.h"
#include "mailbox.h"
#include "memory.h"
#include "otp_socket.h"
#include "posix_nifs.h"
#include "resources.h"
#include "scheduler.h"
#include "synclist.h"
#include "sys.h"
#include "utils.h"

static uint32_t cb_read_resource = 0;
static int32_t down_pid = 0;
static ErlNifMonitor down_mon = { NULL, 0 };

static uint32_t cb_read_resource_two = 0;
static int32_t down_pid_two = 0;
static ErlNifMonitor down_mon_two = { NULL, 0 };

static int32_t lockable_pid = 0;

// Helpers for resource ref_count sub-fields (packed layout).
// Only valid for resources, not plain refc binaries.
static size_t resource_ref_count(void *resource)
{
    return refc_binary_get_refcount(refc_binary_from_data(resource));
}

static size_t resource_monitor_refc(void *resource)
{
    struct RefcBinary *refc = refc_binary_from_data(resource);
    return (refc->ref_count & REFC_MONITOR_MASK) >> REFC_COUNT_BITS;
}

static uint32_t dtor_call_count = 0;

static void resource_dtor(ErlNifEnv *env, void *resource)
{
    UNUSED(env);

    cb_read_resource = *((uint32_t *) resource);
    dtor_call_count++;
}

static void resource_down(ErlNifEnv *env, void *resource, ErlNifPid *pid, ErlNifMonitor *mon)
{
    UNUSED(env);

    cb_read_resource = *((uint32_t *) resource);
    down_pid = *pid;
    down_mon = *mon;
}

static void resource_down_two(ErlNifEnv *env, void *resource, ErlNifPid *pid, ErlNifMonitor *mon)
{
    UNUSED(env);

    cb_read_resource_two = *((uint32_t *) resource);
    down_pid_two = *pid;
    down_mon_two = *mon;
}

// Simulates the race: release resource from within the down handler.
static void resource_down_releasing(ErlNifEnv *env, void *resource, ErlNifPid *pid, ErlNifMonitor *mon)
{
    UNUSED(env);

    cb_read_resource = *((uint32_t *) resource);
    down_pid = *pid;
    down_mon = *mon;

    enif_release_resource(resource);
}

// down handlers should be able to acquire the process tables lock, e.g. to send
// a message.
static void resource_down_acquiring_lock(ErlNifEnv *env, void *resource, ErlNifPid *pid, ErlNifMonitor *mon)
{
    UNUSED(env);
    UNUSED(resource);
    UNUSED(pid);
    UNUSED(mon);

    Context *target = globalcontext_get_process_lock(env->global, lockable_pid);
    assert(target != NULL);

    cb_read_resource = *((uint32_t *) resource);
    down_pid = *pid;
    down_mon = *mon;

    globalcontext_get_process_unlock(env->global, target);
}

void test_resource(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ErlNifEnv *env = erl_nif_env_from_context(ctx);

    ErlNifResourceTypeInit init;
    init.members = 1;
    init.dtor = resource_dtor;
    ErlNifResourceFlags flags;
    cb_read_resource = 0;

    ErlNifResourceType *resource_type = enif_init_resource_type(env, "test_resource", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);
    assert(flags == ERL_NIF_RT_CREATE);

    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    uint32_t *resource = (uint32_t *) ptr;
    *resource = 42;

    enum MemoryGCResult gc_result = memory_erl_nif_env_ensure_free(env, TERM_BOXED_REFERENCE_RESOURCE_SIZE);
    assert(gc_result == MEMORY_GC_OK);
    UNUSED(gc_result);
    ERL_NIF_TERM resource_term = term_from_resource(ptr, &env->heap);
    assert(term_is_reference(resource_term));
    assert(term_is_resource_reference(resource_term));

    void *gotten_ptr = NULL;
    int wrong_type = enif_get_resource(env, resource_term, NULL, &gotten_ptr);
    assert(gotten_ptr == NULL);
    assert(!wrong_type);

    int correct_type = enif_get_resource(env, resource_term, resource_type, &gotten_ptr);
    assert(gotten_ptr == ptr);
    assert(correct_type);

    assert(cb_read_resource == 0);

    int release_result = enif_release_resource(ptr);
    assert(release_result);

    assert(cb_read_resource == 0);

    scheduler_terminate(ctx);
    assert(cb_read_resource == 42);
    cb_read_resource = 0;

    globalcontext_destroy(glb);

    assert(cb_read_resource == 0);
}

void test_resource_destroyed_with_global(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ErlNifEnv *env = erl_nif_env_from_context(ctx);

    ErlNifResourceTypeInit init;
    init.members = 1;
    init.dtor = resource_dtor;
    ErlNifResourceFlags flags;
    cb_read_resource = 0;

    ErlNifResourceType *resource_type = enif_init_resource_type(env, "test_resource", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);
    assert(flags == ERL_NIF_RT_CREATE);

    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    uint32_t *resource = (uint32_t *) ptr;
    *resource = 42;

    scheduler_terminate(ctx);
    assert(cb_read_resource == 0);

    // This test currently writes a warning
    globalcontext_destroy(glb);

    assert(cb_read_resource == 42);
}

void test_resource_keep_release(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ErlNifEnv *env = erl_nif_env_from_context(ctx);

    ErlNifResourceTypeInit init;
    init.members = 1;
    init.dtor = resource_dtor;
    ErlNifResourceFlags flags;
    cb_read_resource = 0;

    ErlNifResourceType *resource_type = enif_init_resource_type(env, "test_resource", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);
    assert(flags == ERL_NIF_RT_CREATE);

    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    uint32_t *resource = (uint32_t *) ptr;
    *resource = 42;

    assert(cb_read_resource == 0);

    int keep_result = enif_keep_resource(ptr);
    assert(keep_result);

    assert(cb_read_resource == 0);

    int release_result = enif_release_resource(ptr);
    assert(release_result);

    assert(cb_read_resource == 0);

    release_result = enif_release_resource(ptr);
    assert(release_result);

    assert(cb_read_resource == 42);

    cb_read_resource = 0;

    scheduler_terminate(ctx);
    globalcontext_destroy(glb);

    assert(cb_read_resource == 0);
}

void test_resource_monitor(void)
{
    GlobalContext *glb = globalcontext_new();
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, glb);

    ErlNifResourceTypeInit init;
    init.members = 3;
    init.dtor = resource_dtor;
    init.stop = NULL;
    init.down = resource_down;
    ErlNifResourceFlags flags;

    ErlNifResourceType *resource_type = enif_init_resource_type(&env, "test_resource", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);
    assert(flags == ERL_NIF_RT_CREATE);

    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    uint32_t *resource = (uint32_t *) ptr;
    *resource = 42;

    assert(resource_ref_count(ptr) == 1);

    ErlNifMonitor mon;
    Context *ctx;
    int32_t pid;
    int monitor_result;

    // Monitor called on destroy
    cb_read_resource = 0;
    down_pid = 0;
    down_mon.ref_ticks = 0;
    assert(resource_monitor_refc(ptr) == 0);
    ctx = context_new(glb);
    pid = ctx->process_id;
    monitor_result = enif_monitor_process(&env, ptr, &pid, &mon);
    assert(monitor_result == 0);
    assert(cb_read_resource == 0);
    assert(resource_ref_count(ptr) == 1);
    assert(resource_monitor_refc(ptr) == 1);

    scheduler_terminate(ctx);
    assert(cb_read_resource == 42);
    assert(down_pid == pid);
    assert(enif_compare_monitors(&mon, &down_mon) == 0);
    assert(resource_ref_count(ptr) == 1);
    assert(resource_monitor_refc(ptr) == 0);

    // Monitor not called if demonitored
    cb_read_resource = 0;
    down_pid = 0;
    down_mon.ref_ticks = 0;
    ctx = context_new(glb);
    pid = ctx->process_id;
    monitor_result = enif_monitor_process(&env, ptr, &pid, &mon);
    assert(monitor_result == 0);
    assert(cb_read_resource == 0);
    assert(resource_ref_count(ptr) == 1);
    assert(resource_monitor_refc(ptr) == 1);

    monitor_result = enif_demonitor_process(&env, ptr, &mon);
    assert(monitor_result == 0);
    assert(resource_ref_count(ptr) == 1);
    assert(resource_monitor_refc(ptr) == 0);

    scheduler_terminate(ctx);
    assert(cb_read_resource == 0);
    assert(down_pid == 0);

    // Resource demonitored if deallocated
    assert(resource_ref_count(ptr) == 1);
    cb_read_resource = 0;
    down_pid = 0;
    down_mon.ref_ticks = 0;
    ctx = context_new(glb);
    pid = ctx->process_id;
    monitor_result = enif_monitor_process(&env, ptr, &pid, &mon);
    assert(monitor_result == 0);
    assert(cb_read_resource == 0);
    assert(resource_ref_count(ptr) == 1);

    int release_result = enif_release_resource(ptr);
    assert(release_result);
    assert(cb_read_resource == 42);

    cb_read_resource = 0;
    monitor_result = enif_demonitor_process(&env, ptr, &mon);
    assert(monitor_result == -1);
    assert(cb_read_resource == 0);

    scheduler_terminate(ctx);
    assert(cb_read_resource == 0);
    assert(down_pid == 0);

    globalcontext_destroy(glb);

    assert(cb_read_resource == 0);
}

void test_resource_monitor_handler_can_lock(void)
{
    GlobalContext *glb = globalcontext_new();
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, glb);

    ErlNifResourceTypeInit init;
    init.members = 3;
    init.dtor = resource_dtor;
    init.stop = NULL;
    init.down = resource_down_acquiring_lock;
    ErlNifResourceFlags flags;

    ErlNifResourceType *resource_type = enif_init_resource_type(&env, "test_resource", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);
    assert(flags == ERL_NIF_RT_CREATE);

    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    uint32_t *resource = (uint32_t *) ptr;
    *resource = 42;

    ErlNifMonitor mon;
    Context *ctx;
    Context *another_ctx;
    int32_t pid;
    int monitor_result;

    // Monitor called on destroy
    cb_read_resource = 0;
    down_pid = 0;
    down_mon.ref_ticks = 0;
    ctx = context_new(glb);
    another_ctx = context_new(glb);
    lockable_pid = another_ctx->process_id;
    pid = ctx->process_id;
    monitor_result = enif_monitor_process(&env, ptr, &pid, &mon);
    assert(monitor_result == 0);
    assert(cb_read_resource == 0);

    scheduler_terminate(ctx);
    assert(cb_read_resource == 42);
    assert(down_pid == pid);
    assert(enif_compare_monitors(&mon, &down_mon) == 0);

    scheduler_terminate(another_ctx);

    int release_result = enif_release_resource(ptr);
    assert(release_result);

    globalcontext_destroy(glb);
}

void test_resource_monitor_two_resources_two_processes(void)
{
    GlobalContext *glb = globalcontext_new();
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, glb);

    ErlNifResourceTypeInit init_1;
    init_1.members = 3;
    init_1.dtor = resource_dtor;
    init_1.stop = NULL;
    init_1.down = resource_down;
    ErlNifResourceFlags flags;

    ErlNifResourceType *resource_type_1 = enif_init_resource_type(&env, "test_resource_1", &init_1, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type_1 != NULL);
    assert(flags == ERL_NIF_RT_CREATE);

    void *ptr_1 = enif_alloc_resource(resource_type_1, sizeof(uint32_t));
    uint32_t *resource_1 = (uint32_t *) ptr_1;
    *resource_1 = 42;

    ErlNifResourceTypeInit init_2;
    init_2.members = 3;
    init_2.dtor = resource_dtor;
    init_2.stop = NULL;
    init_2.down = resource_down_two;

    ErlNifResourceType *resource_type_2 = enif_init_resource_type(&env, "test_resource_2", &init_2, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type_1 != NULL);
    assert(flags == ERL_NIF_RT_CREATE);

    void *ptr_2 = enif_alloc_resource(resource_type_2, sizeof(uint32_t));
    uint32_t *resource_2 = (uint32_t *) ptr_2;
    *resource_2 = 43;

    ErlNifMonitor mon_1, mon_2, mon_3;
    Context *ctx_1;
    Context *ctx_2;
    int32_t pid_1;
    int32_t pid_2;
    int monitor_result;

    cb_read_resource = 0;
    down_pid = 0;
    down_pid_two = 0;
    down_mon.ref_ticks = 0;
    down_mon_two.ref_ticks = 0;
    ctx_1 = context_new(glb);
    ctx_2 = context_new(glb);
    pid_1 = ctx_1->process_id;
    pid_2 = ctx_2->process_id;

    // Both resources monitor process 1.
    // Resource 1 also monitors process 2.
    monitor_result = enif_monitor_process(&env, ptr_1, &pid_1, &mon_1);
    assert(monitor_result == 0);
    monitor_result = enif_monitor_process(&env, ptr_2, &pid_1, &mon_2);
    assert(monitor_result == 0);
    monitor_result = enif_monitor_process(&env, ptr_1, &pid_2, &mon_3);
    assert(monitor_result == 0);

    // Process #1 terminates, mon_1 & mon_2 are fired.
    assert(cb_read_resource == 0);
    assert(cb_read_resource_two == 0);
    scheduler_terminate(ctx_1);
    assert(cb_read_resource == 42);
    assert(cb_read_resource_two == 43);
    assert(down_pid == pid_1);
    assert(down_pid_two == pid_1);
    assert(enif_compare_monitors(&mon_1, &down_mon) == 0);
    assert(enif_compare_monitors(&mon_2, &down_mon_two) == 0);

    cb_read_resource = 0;
    cb_read_resource_two = 0;
    down_pid = 0;
    down_mon.ref_ticks = 0;

    // Process #2 terminates, mon_3 is fired.
    scheduler_terminate(ctx_2);
    assert(cb_read_resource == 42);
    assert(cb_read_resource_two == 0);
    assert(down_pid == pid_2);
    assert(enif_compare_monitors(&mon_3, &down_mon) == 0);

    int release_result = enif_release_resource(ptr_1);
    assert(release_result);
    release_result = enif_release_resource(ptr_2);
    assert(release_result);

    globalcontext_destroy(glb);
}

void test_resource_binary(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ErlNifEnv *env = erl_nif_env_from_context(ctx);

    ErlNifResourceTypeInit init;
    init.members = 1;
    init.dtor = resource_dtor;
    ErlNifResourceFlags flags;
    cb_read_resource = 0;

    ErlNifResourceType *resource_type = enif_init_resource_type(env, "test_resource", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);
    assert(flags == ERL_NIF_RT_CREATE);

    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    uint32_t *resource = (uint32_t *) ptr;
    *resource = 42;

    enum MemoryGCResult gc_result = memory_erl_nif_env_ensure_free(env, TERM_BOXED_REFC_BINARY_SIZE);
    assert(gc_result == MEMORY_GC_OK);
    UNUSED(gc_result);
    ERL_NIF_TERM binary = term_from_resource_binary(ptr, "hello", 5, &env->heap, env->global);
    assert(term_is_binary(binary));
    assert(term_is_refc_binary(binary));
    assert(term_binary_size(binary) == 5);
    assert(memcmp(term_binary_data(binary), "hello", 5) == 0);

    // When serialized, a resource-managed binary appears becomes a regular binary
    // There is no external_term_to_binary_with_roots, so we use the process dictionary
    term old;
    DictionaryFunctionResult result = dictionary_put(&ctx->dictionary, BINARY_ATOM, binary, &old, ctx->global);
    assert(result == DictionaryOk);

    term binary_ext = external_term_to_binary(ctx, binary);

    result = dictionary_get(&ctx->dictionary, BINARY_ATOM, &binary, ctx->global);
    assert(result == DictionaryOk);

    // Unserialize and then check the result
    size_t bytes_read;
    term roots[2];
    roots[0] = binary_ext;
    roots[1] = binary;
    term binary_unserialized = external_term_from_binary_with_roots(ctx, 0, 0, &bytes_read, 2, roots);
    binary_ext = roots[0];
    binary = roots[1];

    assert(term_is_binary(binary_unserialized));
    assert(!term_is_refc_binary(binary_unserialized));
    assert(term_binary_size(binary_unserialized) == 5);
    assert(memcmp(term_binary_data(binary_unserialized), "hello", 5) == 0);

    // A resource-managed binary is equal to a binary with the same content
    assert(term_compare(binary, binary_unserialized, TermCompareExact, glb) == TermEquals);

    // We no longer need the resource now that we have a binary
    int release_result = enif_release_resource(ptr);
    assert(release_result);

    assert(cb_read_resource == 0);

    // garbage collect the binary
    scheduler_terminate(ctx);

    assert(cb_read_resource == 42);

    cb_read_resource = 0;
    globalcontext_destroy(glb);

    assert(cb_read_resource == 0);
}

void test_resource_binaries(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx1 = context_new(glb);
    ErlNifEnv *env1 = erl_nif_env_from_context(ctx1);
    Context *ctx2 = context_new(glb);
    ErlNifEnv *env2 = erl_nif_env_from_context(ctx2);

    ErlNifResourceTypeInit init;
    init.members = 1;
    init.dtor = resource_dtor;
    ErlNifResourceFlags flags;
    cb_read_resource = 0;

    ErlNifResourceType *resource_type = enif_init_resource_type(env1, "test_resource", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);
    assert(flags == ERL_NIF_RT_CREATE);

    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    uint32_t *resource = (uint32_t *) ptr;
    *resource = 42;

    enum MemoryGCResult gc_result = memory_erl_nif_env_ensure_free(env1, TERM_BOXED_REFC_BINARY_SIZE);
    assert(gc_result == MEMORY_GC_OK);
    ERL_NIF_TERM binary1 = term_from_resource_binary(ptr, "hello", 5, &env1->heap, env1->global);
    assert(term_is_binary(binary1));
    assert(term_is_refc_binary(binary1));
    assert(term_binary_size(binary1) == 5);
    assert(memcmp(term_binary_data(binary1), "hello", 5) == 0);

    assert(cb_read_resource == 0);

    gc_result = memory_erl_nif_env_ensure_free(env2, TERM_BOXED_REFC_BINARY_SIZE);
    assert(gc_result == MEMORY_GC_OK);
    UNUSED(gc_result);
    ERL_NIF_TERM binary2 = term_from_resource_binary(ptr, "world", 5, &env2->heap, env2->global);
    assert(term_is_binary(binary2));
    assert(term_is_refc_binary(binary2));
    assert(term_binary_size(binary2) == 5);
    assert(memcmp(term_binary_data(binary2), "world", 5) == 0);

    // We no longer need the resource
    int release_result = enif_release_resource(ptr);
    assert(release_result);

    // garbage collect the first binary
    scheduler_terminate(ctx1);

    assert(cb_read_resource == 0);

    // garbage collect the second binary
    scheduler_terminate(ctx2);

    assert(cb_read_resource == 42);

    cb_read_resource = 0;
    globalcontext_destroy(glb);

    assert(cb_read_resource == 0);
}

void test_resource_release_in_down_handler(void)
{
    GlobalContext *glb = globalcontext_new();
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, glb);

    ErlNifResourceTypeInit init;
    init.members = 3;
    init.dtor = resource_dtor;
    init.stop = NULL;
    init.down = resource_down_releasing;
    ErlNifResourceFlags flags;

    ErlNifResourceType *resource_type = enif_init_resource_type(&env, "test_resource", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);

    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    uint32_t *resource = (uint32_t *) ptr;
    *resource = 42;

    cb_read_resource = 0;
    dtor_call_count = 0;
    down_pid = 0;
    down_mon.ref_ticks = 0;

    Context *ctx = context_new(glb);
    int32_t pid = ctx->process_id;
    ErlNifMonitor mon;
    int monitor_result = enif_monitor_process(&env, ptr, &pid, &mon);
    assert(monitor_result == 0);
    assert(resource_ref_count(ptr) == 1);
    assert(resource_monitor_refc(ptr) == 1);

    scheduler_terminate(ctx);

    assert(down_pid == pid);
    assert(enif_compare_monitors(&mon, &down_mon) == 0);
    assert(dtor_call_count == 1);
    assert(cb_read_resource == 42);

    globalcontext_destroy(glb);
}

void test_resource_release_in_down_handler_two_monitors(void)
{
    GlobalContext *glb = globalcontext_new();
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, glb);

    ErlNifResourceTypeInit init;
    init.members = 3;
    init.dtor = resource_dtor;
    init.stop = NULL;
    init.down = resource_down_releasing;
    ErlNifResourceFlags flags;

    ErlNifResourceType *resource_type = enif_init_resource_type(&env, "test_resource", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);

    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    uint32_t *resource = (uint32_t *) ptr;
    *resource = 42;

    cb_read_resource = 0;
    dtor_call_count = 0;
    down_pid = 0;

    Context *ctx1 = context_new(glb);
    Context *ctx2 = context_new(glb);
    int32_t pid1 = ctx1->process_id;
    int32_t pid2 = ctx2->process_id;
    ErlNifMonitor mon1, mon2;

    int r = enif_monitor_process(&env, ptr, &pid1, &mon1);
    assert(r == 0);
    r = enif_monitor_process(&env, ptr, &pid2, &mon2);
    assert(r == 0);
    assert(resource_monitor_refc(ptr) == 2);

    scheduler_terminate(ctx1);
    assert(down_pid == pid1);
    assert(dtor_call_count == 1);
    assert(cb_read_resource == 42);

    down_pid = 0;
    dtor_call_count = 0;
    scheduler_terminate(ctx2);
    assert(down_pid == 0);
    assert(dtor_call_count == 0);

    globalcontext_destroy(glb);
}

#ifndef HAVE_KQUEUE
// Regression test for the generic_unix poll(2) backend: enif_select arms an
// event under the select_events lock but marks select_events_poll_count dirty
// only after unlocking. A listener-only rebuild in that window used to size
// the pollfd array from the stale cached count and overflow it by one entry.
static EventListener *regression_noop_listener_handler(GlobalContext *glb, EventListener *listener)
{
    UNUSED(glb);
    return listener;
}

void test_select_event_armed_during_listener_only_rebuild(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ErlNifEnv *env = erl_nif_env_from_context(ctx);

    // Initial rebuild: both cached counts become clean (0 listeners, 0 select events).
    sys_poll_events(glb, 1);

    // Dirty only the listeners count.
    int listener_pipe[2];
    int r = pipe(listener_pipe);
    assert(r == 0);
    UNUSED(r);
    EventListener listener;
    listener.handler = regression_noop_listener_handler;
    listener.fd = listener_pipe[0];
    sys_register_listener(glb, &listener);

    // Arm a select event exactly as enif_select_common does under the lock, but
    // stop before sys_register_select_event: the cached count stays 0.
    ErlNifResourceTypeInit init;
    init.members = 1;
    init.dtor = resource_dtor;
    ErlNifResourceFlags flags;
    ErlNifResourceType *resource_type = enif_init_resource_type(env, "test_select_resource", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);
    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    assert(ptr != NULL);

    int select_pipe[2];
    r = pipe(select_pipe);
    assert(r == 0);
    ssize_t written = write(select_pipe[1], "x", 1);
    assert(written == 1);
    UNUSED(written);

    struct SelectEvent *select_event = malloc(sizeof(struct SelectEvent));
    assert(select_event != NULL);
    select_event->event = select_pipe[0];
    select_event->resource = refc_binary_from_data(ptr);
    select_event->read = true;
    select_event->write = false;
    select_event->close = false;
    select_event->read_local_pid = ctx->process_id;
    select_event->write_local_pid = INVALID_PROCESS_ID;
    select_event->read_ref_ticks = 0;
    select_event->write_ref_ticks = 0;
    select_event->read_message = NULL;
    select_event->write_message = NULL;
    refc_binary_increment_refcount(select_event->resource);
    list_init(&select_event->head);
    struct ListHead *select_events = synclist_wrlock(&glb->select_events);
    list_append(select_events, &select_event->head);
    synclist_unlock(&glb->select_events);

    // Listener-only rebuild. Must size the array from the list, not the cache.
    sys_poll_events(glb, 0);
#ifdef AVM_TASK_DRIVER_ENABLED
    globalcontext_process_task_driver_queues(glb);
#endif

    // The armed event was part of the poll set: it was reported and notified.
    select_events = synclist_wrlock(&glb->select_events);
    bool still_armed = select_event->read;
    synclist_unlock(&glb->select_events);
    assert(!still_armed);
    mailbox_process_outer_list_native(&ctx->mailbox);
    assert(mailbox_has_next(&ctx->mailbox));

    r = enif_select(env, select_pipe[0], ERL_NIF_SELECT_STOP, ptr, NULL, term_nil());
    assert(r == ERL_NIF_SELECT_STOP_CALLED);
    int release_result = enif_release_resource(ptr);
    assert(release_result);
    UNUSED(release_result);
    sys_unregister_listener(glb, &listener);
    close(listener_pipe[0]);
    close(listener_pipe[1]);
    close(select_pipe[0]);
    close(select_pipe[1]);
    scheduler_terminate(ctx);
    globalcontext_destroy(glb);
}
#endif

void test_select_cancel(void)
{
    dtor_call_count = 0;
    GlobalContext *global = globalcontext_new();
    Context *reader = context_new(global);
    Context *writer = context_new(global);
    ErlNifEnv *env = erl_nif_env_from_context(reader);
    ErlNifResourceTypeInit init = { .members = 1, .dtor = resource_dtor };
    ErlNifResourceType *type = enif_init_resource_type(env, "select_cancel", &init, ERL_NIF_RT_CREATE, NULL);
    assert(type != NULL);
    void *resource = enif_alloc_resource(type, sizeof(uint32_t));
    *((uint32_t *) resource) = 42;
    int sockets[2];
    int r = socketpair(AF_UNIX, SOCK_STREAM, 0, sockets);
    assert(r == 0);
    ErlNifEvent event = sockets[0];

    r = enif_select(env, event, ERL_NIF_SELECT_CANCEL, resource, NULL, UNDEFINED_ATOM);
    assert(r == ERL_NIF_SELECT_BADARG);
    r = enif_select(env, event, ERL_NIF_SELECT_READ | ERL_NIF_SELECT_CANCEL, resource, NULL, UNDEFINED_ATOM);
    assert(r == 0);
    enum MemoryGCResult gc_result = memory_erl_nif_env_ensure_free(env, TERM_BOXED_REFERENCE_RESOURCE_SIZE);
    assert(gc_result == MEMORY_GC_OK);
    term resource_message = term_from_resource(resource, &env->heap);
    r = enif_select_read(env, event, resource, &reader->process_id, resource_message, NULL);
    assert(r == 0);
    r = enif_select_write(env, event, resource, &writer->process_id, term_from_int(29), NULL);
    assert(r == 0);
    r = enif_select(env, event, ERL_NIF_SELECT_READ | ERL_NIF_SELECT_CANCEL, resource, NULL, UNDEFINED_ATOM);
    assert(r == ERL_NIF_SELECT_READ_CANCELLED);
    r = enif_select(env, event, ERL_NIF_SELECT_READ | ERL_NIF_SELECT_CANCEL, resource, NULL, UNDEFINED_ATOM);
    assert(r == 0);
    ssize_t written = write(sockets[1], "x", 1);
    assert(written == 1);
    sys_poll_events(global, 0);

    term notification;
    MailboxMessage *signal = mailbox_process_outer_list_native(&reader->mailbox);
    assert(signal == NULL);
    bool has_message = mailbox_peek(reader, &notification);
    assert(!has_message);
    signal = mailbox_process_outer_list_native(&writer->mailbox);
    assert(signal == NULL);
    has_message = mailbox_peek(writer, &notification);
    assert(has_message);
    assert(notification == term_from_int(29));
    mailbox_remove_message(&writer->mailbox, &writer->heap);

    // Reuse the retained event; cancelling write must leave a reference-based
    // read selection intact, including its owner and reference.
    gc_result = memory_erl_nif_env_ensure_free(env, TERM_BOXED_REFERENCE_SHORT_SIZE + TERM_BOXED_REFERENCE_RESOURCE_SIZE);
    assert(gc_result == MEMORY_GC_OK);
    term ref = term_from_ref_ticks(123, &env->heap);
    resource_message = term_from_resource(resource, &env->heap);
    r = enif_select_write(env, event, resource, &writer->process_id, resource_message, NULL);
    assert(r == 0);
    r = enif_select(env, event, ERL_NIF_SELECT_READ, resource, &reader->process_id, ref);
    assert(r == 0);
    r = enif_select(env, event, ERL_NIF_SELECT_WRITE | ERL_NIF_SELECT_CANCEL, resource, NULL, term_from_int(99));
    assert(r == ERL_NIF_SELECT_WRITE_CANCELLED);
    sys_poll_events(global, 0);
    signal = mailbox_process_outer_list_native(&writer->mailbox);
    assert(signal == NULL);
    has_message = mailbox_peek(writer, &notification);
    assert(!has_message);
    signal = mailbox_process_outer_list_native(&reader->mailbox);
    assert(signal == NULL);
    has_message = mailbox_peek(reader, &notification);
    assert(has_message);
    assert(term_is_tuple(notification) && term_get_tuple_arity(notification) == 4);
    assert(term_get_tuple_element(notification, 0) == SELECT_ATOM);
    assert(term_to_ref_ticks(term_get_tuple_element(notification, 2)) == 123);
    assert(term_get_tuple_element(notification, 3) == READY_INPUT_ATOM);
    mailbox_remove_message(&reader->mailbox, &reader->heap);

    r = enif_select_read(env, event, resource, &reader->process_id, term_from_int(37), NULL);
    assert(r == 0);
    r = enif_select_write(env, event, resource, &writer->process_id, term_from_int(41), NULL);
    assert(r == 0);
    enum ErlNifSelectFlags cancel_both = ERL_NIF_SELECT_READ | ERL_NIF_SELECT_WRITE | ERL_NIF_SELECT_CANCEL;
    r = enif_select(env, event, cancel_both, resource, NULL, UNDEFINED_ATOM);
    assert(r == (ERL_NIF_SELECT_READ_CANCELLED | ERL_NIF_SELECT_WRITE_CANCELLED));
    r = enif_select(env, event, cancel_both, resource, NULL, UNDEFINED_ATOM);
    assert(r == 0);
    sys_poll_events(global, 0);
    signal = mailbox_process_outer_list_native(&reader->mailbox);
    assert(signal == NULL);
    has_message = mailbox_peek(reader, &notification);
    assert(!has_message);
    signal = mailbox_process_outer_list_native(&writer->mailbox);
    assert(signal == NULL);
    has_message = mailbox_peek(writer, &notification);
    assert(!has_message);

    // An inactive registration for another resource must not shadow readiness
    // after the OS reuses its descriptor for a new socket.
    void *replacement = enif_alloc_resource(type, sizeof(uint32_t));
    *((uint32_t *) replacement) = 42;
    r = enif_select_read(env, event, replacement, &reader->process_id, term_from_int(53), NULL);
    assert(r == 0);
    sys_poll_events(global, 0);
    signal = mailbox_process_outer_list_native(&reader->mailbox);
    assert(signal == NULL);
    has_message = mailbox_peek(reader, &notification);
    assert(has_message);
    assert(notification == term_from_int(53));
    mailbox_remove_message(&reader->mailbox, &reader->heap);
    r = enif_select(env, event, ERL_NIF_SELECT_STOP, replacement, NULL, UNDEFINED_ATOM);
    assert(r == ERL_NIF_SELECT_STOP_CALLED);
    enif_release_resource(replacement);

    r = enif_select(env, event, ERL_NIF_SELECT_STOP, resource, NULL, UNDEFINED_ATOM);
    assert(r == ERL_NIF_SELECT_STOP_CALLED);
    enif_release_resource(resource);
    close(sockets[0]);
    close(sockets[1]);
    context_destroy(reader);
    context_destroy(writer);
#ifdef AVM_TASK_DRIVER_ENABLED
    globalcontext_process_task_driver_queues(global);
#endif
    assert(dtor_call_count == 2);
    globalcontext_destroy(global);
}

static term call_socket_nif(Context *ctx, const char *name, int argc, term argv[])
{
    const struct Nif *nif = otp_socket_nif_get_nif(name);
    assert(nif != NULL);
    term args[3];
    for (int i = 0; i < argc; i++) {
        args[i] = argv[i];
    }
    term result = nif->nif_ptr(ctx, argc, args);
    assert(!term_is_invalid_term(result));
    return result;
}

// Exercise socket direction ownership through the real public NIF entrypoints,
// including the case where one process owns both independent directions.
static void test_socket_write_stop(void)
{
    // Kernels differ on the errno for UDP send without a destination.
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    assert(fd >= 0);
    struct sockaddr_in local_address = { .sin_family = AF_INET, .sin_port = 0, .sin_addr = { .s_addr = htonl(INADDR_LOOPBACK) } };
    int r = bind(fd, (struct sockaddr *) &local_address, sizeof(local_address));
    assert(r == 0);
    UNUSED(r);
    ssize_t sent = send(fd, "x", 1, 0);
    int expected_send_error = errno;
    assert(sent == -1);
    struct sockaddr_in invalid_destination = { .sin_family = AF_INET, .sin_port = htons(9), .sin_addr = { .s_addr = htonl(INADDR_BROADCAST) } };
    sent = sendto(fd, "x", 1, 0, (struct sockaddr *) &invalid_destination, sizeof(invalid_destination));
    int expected_sendto_error = errno;
    assert(sent == -1);
    UNUSED(sent);
    close(fd);
    GlobalContext *global = globalcontext_new();
    Context *reader = context_new(global);
    Context *writer = context_new(global);
    // Native entrypoints may collect their caller's heap. Keep shared inputs
    // in an independent rooted heap, just like messages passed between NIFs.
    BEGIN_WITH_STACK_HEAP(512, inputs)
    term args[] = { INET_ATOM, globalcontext_make_atom(global, ATOM_STR("\x5", "dgram")), globalcontext_make_atom(global, ATOM_STR("\x3", "udp")) };
    term result = call_socket_nif(reader, "socket:open/3", 3, args);
    assert(term_get_tuple_element(result, 0) == OK_ATOM);
    term socket = memory_copy_term_tree(&inputs, term_get_tuple_element(result, 1));
    term address = term_alloc_map(3, &inputs);
    term_set_map_assoc(address, 0, globalcontext_make_atom(global, ATOM_STR("\x4", "addr")), globalcontext_make_atom(global, ATOM_STR("\x8", "loopback")));
    term_set_map_assoc(address, 1, FAMILY_ATOM, INET_ATOM);
    term_set_map_assoc(address, 2, PORT_ATOM, term_from_int(0));
    args[0] = socket;
    args[1] = address;
    result = call_socket_nif(reader, "socket:bind/2", 2, args);
    assert(result == OK_ATOM);
    result = call_socket_nif(reader, "socket:sockname/1", 1, args);
    assert(term_get_tuple_element(result, 0) == OK_ATOM);
    term destination = memory_copy_term_tree(&inputs, term_get_tuple_element(result, 1));
    term data = term_from_literal_binary((const uint8_t *) "x", 1, &inputs, global);

    for (int same_owner = 0; same_owner < 2; same_owner++) {
        Context *write_owner = same_owner ? reader : writer;
        args[0] = socket;
        args[1] = term_from_ref_ticks(123 + same_owner, &inputs);
        result = call_socket_nif(reader, "socket:nif_select_read/2", 2, args);
        assert(result == OK_ATOM);
        args[1] = term_from_ref_ticks(456, &inputs);
        result = call_socket_nif(write_owner, "socket:nif_select_write/2", 2, args);
        assert(result == OK_ATOM);
        result = call_socket_nif(write_owner, "socket:nif_select_write_stop/1", 1, args);
        assert(result == OK_ATOM);
        // Cancellation is idempotent and cannot clear the remaining read owner.
        result = call_socket_nif(write_owner, "socket:nif_select_write_stop/1", 1, args);
        assert(result == OK_ATOM);
        args[1] = data;
        args[2] = destination;
        result = call_socket_nif(reader, "socket:nif_sendto/3", 3, args);
        assert(result == OK_ATOM);
        // Leave monitor bookkeeping for context_destroy; move only ordinary
        // notifications to the inner mailbox, as a native process would.
        // A poll can return for its registration wakeup before UDP arrives.
        term notification;
        bool ready = false;
        for (int attempt = 0; attempt < 50 && !ready; attempt++) {
            sys_poll_events(global, 100);
            mailbox_requeue_outer_signals(&reader->mailbox, mailbox_process_outer_list_native(&reader->mailbox));
            ready = mailbox_peek(reader, &notification);
        }
        mailbox_requeue_outer_signals(&writer->mailbox, mailbox_process_outer_list_native(&writer->mailbox));
        assert(ready);
        assert(term_get_tuple_element(notification, 0) == DOLLAR_SOCKET_ATOM);
        assert(term_get_tuple_element(notification, 2) == SELECT_ATOM);
        assert(term_to_ref_ticks(term_get_tuple_element(notification, 3)) == (uint64_t) (123 + same_owner));
        mailbox_remove_message(&reader->mailbox, &reader->heap);
        bool has_message = mailbox_peek(reader, &notification);
        assert(!has_message);
        has_message = mailbox_peek(writer, &notification);
        assert(!has_message);
        UNUSED(has_message);
        args[1] = term_from_int(1);
        result = call_socket_nif(reader, "socket:nif_recv/2", 2, args);
        assert(term_get_tuple_element(result, 0) == OK_ATOM);
        term received = term_get_tuple_element(result, 1);
        assert(term_binary_size(received) == 1 && term_binary_data(received)[0] == 'x');
        UNUSED(received);
    }

    // A real OS send error must not be collapsed to closed, for send or sendto.
    args[1] = data;
    result = call_socket_nif(reader, "socket:nif_send/2", 2, args);
    term expected_error = posix_errno_to_term(expected_send_error, global);
    assert(term_get_tuple_element(result, 0) == ERROR_ATOM);
    assert(term_get_tuple_element(result, 1) == expected_error);
    // Broadcasting without SO_BROADCAST is rejected by the OS.
    term broadcast = term_alloc_tuple(4, &inputs);
    for (int i = 0; i < 4; i++) {
        term_put_tuple_element(broadcast, i, term_from_int(255));
    }
    term broadcast_destination = term_alloc_map(3, &inputs);
    term_set_map_assoc(broadcast_destination, 0, globalcontext_make_atom(global, ATOM_STR("\x4", "addr")), broadcast);
    term_set_map_assoc(broadcast_destination, 1, FAMILY_ATOM, INET_ATOM);
    term_set_map_assoc(broadcast_destination, 2, PORT_ATOM, term_from_int(9));
    args[2] = broadcast_destination;
    result = call_socket_nif(reader, "socket:nif_sendto/3", 3, args);
    expected_error = posix_errno_to_term(expected_sendto_error, global);
    assert(term_get_tuple_element(result, 0) == ERROR_ATOM);
    assert(term_get_tuple_element(result, 1) == expected_error);
    UNUSED(expected_error);
    result = call_socket_nif(reader, "socket:close/1", 1, args);
    assert(result == OK_ATOM);
    for (int empty = 0; empty < 2; empty++) {
        args[1] = term_from_literal_binary((const uint8_t *) "x", empty ? 0 : 1, &inputs, global);
        args[2] = destination;
        result = call_socket_nif(reader, "socket:nif_send/2", 2, args);
        assert(term_get_tuple_element(result, 0) == ERROR_ATOM && term_get_tuple_element(result, 1) == CLOSED_ATOM);
        result = call_socket_nif(reader, "socket:nif_sendto/3", 3, args);
        assert(term_get_tuple_element(result, 0) == ERROR_ATOM && term_get_tuple_element(result, 1) == CLOSED_ATOM);
    }
    result = call_socket_nif(reader, "socket:nif_select_write_stop/1", 1, args);
    assert(result == OK_ATOM);
    result = call_socket_nif(reader, "socket:nif_select_read_stop/1", 1, args);
    assert(result == OK_ATOM);
    result = call_socket_nif(reader, "socket:nif_select_stop/1", 1, args);
    assert(result == OK_ATOM);
    args[1] = term_from_ref_ticks(789, &inputs);
    result = call_socket_nif(reader, "socket:nif_select_read/2", 2, args);
    assert(result == OK_ATOM);
    result = call_socket_nif(reader, "socket:nif_select_write/2", 2, args);
    assert(result == OK_ATOM);
    mailbox_requeue_outer_signals(&reader->mailbox, mailbox_process_outer_list_native(&reader->mailbox));
    for (int direction = 0; direction < 2; direction++) {
        term notification;
        bool has_message = mailbox_peek(reader, &notification);
        assert(has_message);
        UNUSED(has_message);
        assert(term_get_tuple_element(notification, 2) == ABORT_ATOM);
        term reason = term_get_tuple_element(notification, 3);
        assert(term_to_ref_ticks(term_get_tuple_element(reason, 0)) == 789);
        assert(term_get_tuple_element(reason, 1) == CLOSED_ATOM);
        UNUSED(reason);
        mailbox_remove_message(&reader->mailbox, &reader->heap);
    }
    args[1] = globalcontext_make_atom(global, ATOM_STR("\xA", "read_write"));
    result = call_socket_nif(reader, "socket:shutdown/2", 2, args);
    assert(term_get_tuple_element(result, 0) == ERROR_ATOM && term_get_tuple_element(result, 1) == CLOSED_ATOM);
    END_WITH_STACK_HEAP(inputs, global)
    context_destroy(reader);
    context_destroy(writer);
#ifdef AVM_TASK_DRIVER_ENABLED
    globalcontext_process_task_driver_queues(global);
#endif
    globalcontext_destroy(global);
}

static void test_socket_destroy_with_live_owners(void)
{
    GlobalContext *global = globalcontext_new();
    Context *reader = context_new(global);
    Context *writer = context_new(global);
    BEGIN_WITH_STACK_HEAP(32, inputs)
    term args[] = { INET_ATOM, globalcontext_make_atom(global, ATOM_STR("\x5", "dgram")), globalcontext_make_atom(global, ATOM_STR("\x3", "udp")) };
    term result = call_socket_nif(reader, "socket:open/3", 3, args);
    assert(term_get_tuple_element(result, 0) == OK_ATOM);
    args[0] = memory_copy_term_tree(&inputs, term_get_tuple_element(result, 1));
    args[1] = UNDEFINED_ATOM;
    result = call_socket_nif(reader, "socket:nif_select_read/2", 2, args);
    assert(result == OK_ATOM);
    result = call_socket_nif(writer, "socket:nif_select_write/2", 2, args);
    assert(result == OK_ATOM);
    END_WITH_STACK_HEAP(inputs, global)

    // Stack-heap resource releases are normally drained by the scheduler.
#ifdef AVM_TASK_DRIVER_ENABLED
    globalcontext_process_task_driver_queues(global);
#endif

    // Global teardown must cancel live owners' selections before freeing the
    // platform polling/signaling state used by their resource-down callbacks.
    globalcontext_destroy(global);
}

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_resource();
    test_resource_destroyed_with_global();
    test_resource_keep_release();
    test_resource_monitor();
    test_resource_monitor_handler_can_lock();
    test_resource_monitor_two_resources_two_processes();
    test_resource_binary();
    test_resource_binaries();
    test_resource_release_in_down_handler();
    test_resource_release_in_down_handler_two_monitors();
#ifndef HAVE_KQUEUE
    test_select_event_armed_during_listener_only_rebuild();
#endif
    test_select_cancel();
    test_socket_write_stop();
    test_socket_destroy_with_live_owners();

    return EXIT_SUCCESS;
}
