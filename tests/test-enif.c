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
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "globalcontext.h"
#include "scheduler.h"
#include "utils.h"

static uint32_t cb_read_resource = 0;
static int32_t down_pid = 0;
static ErlNifMonitor down_mon = 0;

static uint32_t cb_read_resource_two = 0;
static int32_t down_pid_two = 0;
static ErlNifMonitor down_mon_two = 0;

static int32_t lockable_pid = 0;

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

void test_resource()
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
    assert(term_is_binary(resource_term));
    assert(term_binary_size(resource_term) == 0);

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

void test_resource_destroyed_with_global()
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

    globalcontext_destroy(glb);

    assert(cb_read_resource == 42);
}

void test_resource_keep_release()
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

void test_resource_monitor()
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

    ErlNifMonitor mon;
    Context *ctx;
    int32_t pid;
    int monitor_result;

    // Monitor called on destroy
    cb_read_resource = 0;
    down_pid = 0;
    down_mon = 0;
    ctx = context_new(glb);
    pid = ctx->process_id;
    monitor_result = enif_monitor_process(&env, ptr, &pid, &mon);
    assert(monitor_result == 0);
    assert(cb_read_resource == 0);

    scheduler_terminate(ctx);
    assert(cb_read_resource == 42);
    assert(down_pid == pid);
    assert(enif_compare_monitors(&mon, &down_mon) == 0);

    // Monitor not called if demonitored
    cb_read_resource = 0;
    down_pid = 0;
    down_mon = 0;
    ctx = context_new(glb);
    pid = ctx->process_id;
    monitor_result = enif_monitor_process(&env, ptr, &pid, &mon);
    assert(monitor_result == 0);
    assert(cb_read_resource == 0);

    monitor_result = enif_demonitor_process(&env, ptr, &mon);
    assert(monitor_result == 0);

    scheduler_terminate(ctx);
    assert(cb_read_resource == 0);
    assert(down_pid == 0);

    // Monitor not called if resource is deallocated
    cb_read_resource = 0;
    down_pid = 0;
    down_mon = 0;
    ctx = context_new(glb);
    pid = ctx->process_id;
    monitor_result = enif_monitor_process(&env, ptr, &pid, &mon);
    assert(monitor_result == 0);
    assert(cb_read_resource == 0);

    int release_result = enif_release_resource(ptr);
    assert(release_result);
    assert(cb_read_resource == 42);

    cb_read_resource = 0;

    scheduler_terminate(ctx);
    assert(cb_read_resource == 0);
    assert(down_pid == 0);

    globalcontext_destroy(glb);

    assert(cb_read_resource == 0);
}

void test_resource_monitor_handler_can_lock()
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
    down_mon = 0;
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

    globalcontext_destroy(glb);
}

void test_resource_monitor_two_resources_two_processes()
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
    down_mon = 0;
    down_mon_two = 0;
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
    down_mon = 0;

    // Process #2 terminates, mon_3 is fired.
    scheduler_terminate(ctx_2);
    assert(cb_read_resource == 42);
    assert(cb_read_resource_two == 0);
    assert(down_pid == pid_2);
    assert(enif_compare_monitors(&mon_3, &down_mon) == 0);

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

    return EXIT_SUCCESS;
}
