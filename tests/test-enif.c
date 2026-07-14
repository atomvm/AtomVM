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
#include <stdlib.h>
#include <unistd.h>

#include "context.h"
#include "defaultatoms.h"
#include "dictionary.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "external_term.h"
#include "globalcontext.h"
#include "memory.h"
#include "resources.h"
#include "scheduler.h"
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

    assert(memory_erl_nif_env_ensure_free(env, TERM_BOXED_REFERENCE_RESOURCE_SIZE) == MEMORY_GC_OK);
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

    assert(memory_erl_nif_env_ensure_free(env, TERM_BOXED_REFC_BINARY_SIZE) == MEMORY_GC_OK);
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

    assert(memory_erl_nif_env_ensure_free(env1, TERM_BOXED_REFC_BINARY_SIZE) == MEMORY_GC_OK);
    ERL_NIF_TERM binary1 = term_from_resource_binary(ptr, "hello", 5, &env1->heap, env1->global);
    assert(term_is_binary(binary1));
    assert(term_is_refc_binary(binary1));
    assert(term_binary_size(binary1) == 5);
    assert(memcmp(term_binary_data(binary1), "hello", 5) == 0);

    assert(cb_read_resource == 0);

    assert(memory_erl_nif_env_ensure_free(env2, TERM_BOXED_REFC_BINARY_SIZE) == MEMORY_GC_OK);
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

// enif_select_read/enif_select_write should track independent messages, refs
// and target pids for the same event, so a resource can have a pending read
// select and a pending write select at the same time (e.g. a socket with a
// permanent recv select and a transient send-backpressure select).
void test_resource_select_read_write_independent_messages(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *read_ctx = context_new(glb);
    Context *write_ctx = context_new(glb);
    ErlNifEnv *env = erl_nif_env_from_context(read_ctx);

    ErlNifResourceTypeInit init;
    init.members = 1;
    init.dtor = resource_dtor;
    ErlNifResourceFlags flags;
    cb_read_resource = 0;
    dtor_call_count = 0;

    ErlNifResourceType *resource_type = enif_init_resource_type(env, "test_select_resource", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);

    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    uint32_t *resource = (uint32_t *) ptr;
    *resource = 99;

    int pipefds[2];
    assert(pipe(pipefds) == 0);
    ErlNifEvent event = (ErlNifEvent) pipefds[0];

    ErlNifPid read_pid = read_ctx->process_id;
    ErlNifPid write_pid = write_ctx->process_id;

    term read_msg = term_from_int(11);
    term write_msg = term_from_int(22);

    int r = enif_select_read(env, event, ptr, &read_pid, read_msg, NULL);
    assert(r == 0);
    r = enif_select_write(env, event, ptr, &write_pid, write_msg, NULL);
    assert(r == 0);

    // Firing both read and write readiness must deliver the read message to
    // read_ctx and the write message to write_ctx, without either clobbering
    // the other (this is the bug being tested: a shared message/ref/pid field
    // would either lose one notification or send it to the wrong process).
    bool notified = select_event_notify(event, true, true, glb);
    assert(notified);

    assert(mailbox_process_outer_list(&read_ctx->mailbox) == NULL);
    assert(mailbox_has_next(&read_ctx->mailbox));
    term read_received;
    assert(mailbox_peek(read_ctx, &read_received));
    assert(read_received == read_msg);
    mailbox_remove_message(&read_ctx->mailbox, &read_ctx->heap);
    assert(!mailbox_has_next(&read_ctx->mailbox));

    assert(mailbox_process_outer_list(&write_ctx->mailbox) == NULL);
    assert(mailbox_has_next(&write_ctx->mailbox));
    term write_received;
    assert(mailbox_peek(write_ctx, &write_received));
    assert(write_received == write_msg);
    mailbox_remove_message(&write_ctx->mailbox, &write_ctx->heap);
    assert(!mailbox_has_next(&write_ctx->mailbox));

    // Once a direction fires, only that direction is consumed; the other
    // stays armed until it is separately triggered or explicitly stopped.
    // Re-arm both to test the ref-ticks (no custom message) path together.
    r = enif_select_read(env, event, ptr, &read_pid, read_msg, NULL);
    assert(r == 0);
    r = enif_select_write(env, event, ptr, &write_pid, write_msg, NULL);
    assert(r == 0);

    // Re-selecting read must not disturb the pending write selection (and
    // vice-versa): overwrite only the read side with a new pid/message.
    Context *other_read_ctx = context_new(glb);
    ErlNifPid other_read_pid = other_read_ctx->process_id;
    term other_read_msg = term_from_int(33);
    r = enif_select_read(env, event, ptr, &other_read_pid, other_read_msg, NULL);
    assert(r == 0);

    notified = select_event_notify(event, true, true, glb);
    assert(notified);

    // Original read_ctx should get nothing new (its select was overwritten).
    assert(mailbox_process_outer_list(&read_ctx->mailbox) == NULL);
    assert(!mailbox_has_next(&read_ctx->mailbox));

    assert(mailbox_process_outer_list(&other_read_ctx->mailbox) == NULL);
    assert(mailbox_has_next(&other_read_ctx->mailbox));
    term other_read_received;
    assert(mailbox_peek(other_read_ctx, &other_read_received));
    assert(other_read_received == other_read_msg);

    assert(mailbox_process_outer_list(&write_ctx->mailbox) == NULL);
    assert(mailbox_has_next(&write_ctx->mailbox));
    term write_received_again;
    assert(mailbox_peek(write_ctx, &write_received_again));
    assert(write_received_again == write_msg);
    mailbox_remove_message(&write_ctx->mailbox, &write_ctx->heap);

    // Both directions have now fired and are no longer armed, so stopping the
    // select should immediately release the extra refcount it was holding.
    int stop_result = enif_select(env, event, ERL_NIF_SELECT_STOP, ptr, NULL, term_nil());
    assert(stop_result == ERL_NIF_SELECT_STOP_CALLED);

    int release_result = enif_release_resource(ptr);
    assert(release_result);

    scheduler_terminate(read_ctx);
    scheduler_terminate(write_ctx);
    scheduler_terminate(other_read_ctx);

    close(pipefds[0]);
    close(pipefds[1]);

    globalcontext_destroy(glb);
}

// The generic enif_select/2 API (no custom message, just a reference) must
// also track read and write refs independently when both directions are
// selected on the same event, e.g. a single ref selecting both directions,
// followed by a read-only re-select that must not clear the write ref.
void test_resource_select_read_write_independent_refs(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);
    ErlNifEnv *env = erl_nif_env_from_context(ctx);

    ErlNifResourceTypeInit init;
    init.members = 1;
    init.dtor = resource_dtor;
    ErlNifResourceFlags flags;
    cb_read_resource = 0;

    ErlNifResourceType *resource_type = enif_init_resource_type(env, "test_select_resource_refs", &init, ERL_NIF_RT_CREATE, &flags);
    assert(resource_type != NULL);

    void *ptr = enif_alloc_resource(resource_type, sizeof(uint32_t));
    uint32_t *resource = (uint32_t *) ptr;
    *resource = 7;

    int pipefds[2];
    assert(pipe(pipefds) == 0);
    ErlNifEvent event = (ErlNifEvent) pipefds[0];

    ErlNifPid pid = ctx->process_id;

    assert(memory_erl_nif_env_ensure_free(env, REF_SIZE) == MEMORY_GC_OK);
    term write_ref = term_from_ref_ticks(globalcontext_get_ref_ticks(glb), &env->heap);

    // Select for write with an explicit ref and no custom message.
    int r = enif_select(env, event, ERL_NIF_SELECT_WRITE, ptr, &pid, write_ref);
    assert(r == 0);

    // Now select read only, with UNDEFINED_ATOM (no ref requested). This must
    // not disturb the write ref registered above.
    r = enif_select(env, event, ERL_NIF_SELECT_READ, ptr, &pid, UNDEFINED_ATOM);
    assert(r == 0);

    bool notified = select_event_notify(event, true, true, glb);
    assert(notified);

    // Two notifications should have been queued: {select, Resource, undefined, ready_input}
    // and {select, Resource, WriteRef, ready_output}.
    assert(mailbox_process_outer_list(&ctx->mailbox) == NULL);
    assert(mailbox_has_next(&ctx->mailbox));

    bool saw_read = false;
    bool saw_write = false;
    for (int i = 0; i < 2; i++) {
        term msg;
        assert(mailbox_peek(ctx, &msg));
        assert(term_is_tuple(msg));
        assert(term_get_tuple_arity(msg) == 4);
        assert(term_get_tuple_element(msg, 0) == SELECT_ATOM);
        term ref_or_undefined = term_get_tuple_element(msg, 2);
        term kind = term_get_tuple_element(msg, 3);
        if (kind == READY_INPUT_ATOM) {
            assert(ref_or_undefined == UNDEFINED_ATOM);
            saw_read = true;
        } else {
            assert(kind == READY_OUTPUT_ATOM);
            assert(term_is_reference(ref_or_undefined));
            saw_write = true;
        }
        if (i == 0) {
            assert(mailbox_has_next(&ctx->mailbox));
            mailbox_next(&ctx->mailbox);
        }
    }
    assert(saw_read);
    assert(saw_write);
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    assert(!mailbox_has_next(&ctx->mailbox));

    // Both directions have now fired and are no longer armed, so stopping the
    // select should immediately release the extra refcount it was holding.
    int stop_result = enif_select(env, event, ERL_NIF_SELECT_STOP, ptr, NULL, term_nil());
    assert(stop_result == ERL_NIF_SELECT_STOP_CALLED);

    int release_result = enif_release_resource(ptr);
    assert(release_result);

    scheduler_terminate(ctx);

    close(pipefds[0]);
    close(pipefds[1]);

#ifdef AVM_TASK_DRIVER_ENABLED
    // Notifications built without a custom message (the ref-only path
    // exercised above) release their transient resource reference through
    // the task-driver refc queue rather than synchronously; drain it so the
    // resource is properly freed instead of merely being reported as a
    // (harmless) dangling resource on shutdown.
    globalcontext_process_task_driver_queues(glb);
#endif

    globalcontext_destroy(glb);
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
    test_resource_select_read_write_independent_messages();
    test_resource_select_read_write_independent_refs();

    return EXIT_SUCCESS;
}
