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

#include "context.h"
#include "defaultatoms.h"
#include "dictionary.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "externalterm.h"
#include "globalcontext.h"
#include "scheduler.h"
#include "utils.h"

static uint32_t cb_read_resource = 0;
static int32_t down_pid = 0;
static ErlNifMonitor down_mon = { NULL, 0 };

static uint32_t cb_read_resource_two = 0;
static int32_t down_pid_two = 0;
static ErlNifMonitor down_mon_two = { NULL, 0 };

static int32_t lockable_pid = 0;

// Helper to get the reference count.
// Uses an internal API
// Implementation should be updated shall resources be references instead of binaries.
static uint32_t resource_ref_count(void *resource)
{
    struct RefcBinary *refc = refc_binary_from_data(resource);
    return refc->ref_count;
}

static void resource_dtor(ErlNifEnv *env, void *resource)
{
    UNUSED(env);

    cb_read_resource = *((uint32_t *) resource);
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

    ERL_NIF_TERM resource_term = enif_make_resource(env, ptr);
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
    ctx = context_new(glb);
    pid = ctx->process_id;
    monitor_result = enif_monitor_process(&env, ptr, &pid, &mon);
    assert(monitor_result == 0);
    assert(cb_read_resource == 0);
    assert(resource_ref_count(ptr) == 1);

    scheduler_terminate(ctx);
    assert(cb_read_resource == 42);
    assert(down_pid == pid);
    assert(enif_compare_monitors(&mon, &down_mon) == 0);
    assert(resource_ref_count(ptr) == 1);

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

    monitor_result = enif_demonitor_process(&env, ptr, &mon);
    assert(monitor_result == 0);
    assert(resource_ref_count(ptr) == 1);

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

    ERL_NIF_TERM binary = enif_make_resource_binary(env, ptr, "hello", 5);
    assert(term_is_binary(binary));
    assert(term_is_refc_binary(binary));
    assert(term_binary_size(binary) == 5);
    assert(memcmp(term_binary_data(binary), "hello", 5) == 0);

    // When serialized, a resource-managed binary appears becomes a regular binary
    // There is no externalterm_to_binary_with_roots, so we use the process dictionary
    term old;
    DictionaryFunctionResult result = dictionary_put(&ctx->dictionary, BINARY_ATOM, binary, &old, ctx->global);
    assert(result == DictionaryOk);

    term binary_ext = externalterm_to_binary(ctx, binary);

    result = dictionary_get(&ctx->dictionary, BINARY_ATOM, &binary, ctx->global);
    assert(result == DictionaryOk);

    // Unserialize and then check the result
    size_t bytes_read;
    term roots[2];
    roots[0] = binary_ext;
    roots[1] = binary;
    term binary_unserialized = externalterm_from_binary_with_roots(ctx, 0, 0, &bytes_read, 2, roots);
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

    ERL_NIF_TERM binary1 = enif_make_resource_binary(env1, ptr, "hello", 5);
    assert(term_is_binary(binary1));
    assert(term_is_refc_binary(binary1));
    assert(term_binary_size(binary1) == 5);
    assert(memcmp(term_binary_data(binary1), "hello", 5) == 0);

    assert(cb_read_resource == 0);

    ERL_NIF_TERM binary2 = enif_make_resource_binary(env2, ptr, "world", 5);
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

    return EXIT_SUCCESS;
}
