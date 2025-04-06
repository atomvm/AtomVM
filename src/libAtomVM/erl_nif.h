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

/**
 * @file erl_nif.h
 * @brief Public API for nifs, compatible with Erlang/OTP API
 */

#ifndef _ERL_NIF_H_
#define _ERL_NIF_H_

#include "term_typedef.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Opaque environment, passed to nifs.
 */
typedef struct ErlNifEnv ErlNifEnv;

/**
 * @brief A term.
 */
typedef term ERL_NIF_TERM;

/**
 * @brief a pid
 * @details We currently only handle local pids.
 */
typedef int32_t ErlNifPid;

/**
 * @brief Opaque resource type
 */
typedef struct ResourceType ErlNifResourceType;

/**
 * @brief Opaque monitor type
 */
typedef struct
{
    struct ResourceType *resource_type;
    uint64_t ref_ticks;
} ErlNifMonitor;

/**
 * @brief Selectable event.
 */
typedef int ErlNifEvent;

/**
 * @brief Destructor callback
 */
typedef void ErlNifResourceDtor(ErlNifEnv *caller_env, void *obj);

/**
 * @brief Select stop callback
 */
typedef void ErlNifResourceStop(ErlNifEnv *caller_env, void *obj, ErlNifEvent event, int is_direct_call);

/**
 * @brief Resource monitor callback
 */
typedef void ErlNifResourceDown(ErlNifEnv *caller_env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon);

/**
 * @brief Resource callbacks.
 * @details Members should be set to 0, 1 or 2 depending on provided callbacks.
 * Callbacks can also be NULL if not used.
 */
typedef struct
{
    int members;
    ErlNifResourceDtor *dtor;
    ErlNifResourceStop *stop;
    ErlNifResourceDown *down;
} ErlNifResourceTypeInit;

/**
 * @brief resource flags
 */
typedef enum
{
    ERL_NIF_RT_CREATE = 1,
    // ERL_NIF_RT_TAKEOVER is not supported yet
} ErlNifResourceFlags;

/**
 * @brief enif_select mode flags
 * @details ERL_NIF_SELECT_CANCEL which was introduced with OTP-22, is unimplemented.
 */
enum ErlNifSelectFlags
{
    ERL_NIF_SELECT_READ = 1,
    ERL_NIF_SELECT_WRITE = 2,
    ERL_NIF_SELECT_STOP = 4,
    //  ERL_NIF_SELECT_CANCEL = 8,
};

/**
 * @brief enif_select result flags
 * @details ERL_NIF_SELECT_CANCEL which was introduced with OTP-22, is unimplemented.
 */
enum
{
    ERL_NIF_SELECT_STOP_CALLED = 1,
    ERL_NIF_SELECT_STOP_SCHEDULED = 2,
    //  ERL_NIF_SELECT_READ_CANCELLED = 4,
    //  ERL_NIF_SELECT_WRITE_CANCELLED = 8,

    ERL_NIF_SELECT_INVALID_EVENT = -1,
    ERL_NIF_SELECT_FAILED = -2,

    ERL_NIF_SELECT_BADARG = -3,
};

/**
 * @brief Create or take over (code upgrade) a resource type.
 * @param env the current environment
 * @param init a structure describing the callbacks. Callbacks can be NULL if
 * not used.
 * @param name name of the resource (copied)
 * @param flags `ERL_NIF_RT_CREATE` or `ERL_NIF_RT_TAKEOVER` to create or
 * take over.
 * @param tried on output, updated to `ERL_NIF_RT_CREATE` or
 * `ERL_NIF_RT_TAKEOVER` depending on what has been done. On failure, updated
 * to flags. Can be NULL.
 * @return the resource type or `NULL` on failure.
 */
ErlNifResourceType *enif_init_resource_type(ErlNifEnv *env, const char *name, const ErlNifResourceTypeInit *init, ErlNifResourceFlags flags, ErlNifResourceFlags *tried);

/**
 * @brief Allocate a resource for given type for `size` bytes.
 * @details following BEAM semantics, the resource is created with a refcount
 * of 1. A call to `enif_release_resource` will decrement the refcount and
 * destroy the resource if it is zero.
 *
 * Typical usage (as suggested by BEAM documentation) is to call
 * `enif_make_resource` and then `enif_release_resource` to somewhat transfer
 * ownership to the garbage collector. `enif_make_resource` will increment
 * refcount to 2 and also add the resource to the MSO list of the context, so
 * when the term is no longer referenced in the context heap, the reference
 * counter will be decremented by gc.
 *
 * @param type a trype created by `enif_init_resource_type`.
 * @param size the size in bytes.
 * @return a pointer or `NULL` on failure.
 */
void *enif_alloc_resource(ErlNifResourceType *type, unsigned size);

/**
 * @brief Get a pointer to a resource from a term representing it.
 * @param env the current environment
 * @param t the term
 * @param type the resource type
 * @param objp on output the pointer to the resource
 * @return `true` on success, `false` on failure, if term is not a resource of
 * type `type`
 */
int enif_get_resource(ErlNifEnv *env, ERL_NIF_TERM t, ErlNifResourceType *type, void **objp);

/**
 * @brief Increment reference count of a resource
 * @param resource the resource to keep
 * @return `true`.
 */
int enif_keep_resource(void *resource);

/**
 * @brief Decrement reference count of a resource
 * @param resource the resource to release
 * @return `true`.
 */
int enif_release_resource(void *resource);

/**
 * @brief create a term from a resource
 * @details the term can be later passed to `enif_get_resource`.
 *
 * The resource reference counter is incremented and it is added to the MSO
 * list of the heap of env (which must be a context).
 *
 * If the resource was just allocated with `enif_alloc_resource`, the reference
 * counter should typically be decremented by a call to `enif_release_resource`
 * matching usage documented by BEAM.
 *
 * If the resource was not just allocated with `enif_alloc_resource`, to clear
 * usage confusion, users should rather call `term_from_resource` and should
 * not decrement the reference counter.
 *
 * @param env current environment
 * @param obj resource
 * @return a new term representing the resource
 */
ERL_NIF_TERM enif_make_resource(ErlNifEnv *env, void *obj);

/**
 * @brief Run a POSIX-like select on a given object (event) and send a message
 * when the object is readable or writable.
 *
 * @details Actual implementation is platform dependent and platforms may not
 * implement this feature.
 *
 * On `generic_unix`, this is currently implemented using what
 * `sys_poll_events` uses, namely `kqueue(2)` (if available) or `poll(2)`.
 * Please note that `kqueue(2)` and `poll(2)` behave differently for some
 * objects, for example for vnodes and EOF.
 *
 * On `esp32`, this is currently implemented using `poll(2)`.
 *
 * @param env current environment
 * @param event event object (typically a file descriptor)
 * @param mode select mode (`ERL_NIF_SELECT_READ` and/or `ERL_NIF_SELECT_WRITE`)
 * optionally with `ERL_NIF_SELECT_CANCEL` to cancel, or `ERL_NIF_SELECT_STOP`
 * to stop.
 * @param obj resource object working as a container of the event object.
 * @param pid process id to send a message to or NULL to use the current process (from `env`)
 * @param ref reference object used in sent messages or `undefined` atom.
 * @return a negative value on failure, 0 or flags on success.
 */
int enif_select(ErlNifEnv *env, ErlNifEvent event, enum ErlNifSelectFlags mode, void *obj, const ErlNifPid *pid, ERL_NIF_TERM ref);

/**
 * @brief Monitor a process by using a resource object.
 * @details The monitor is automatically removed after being triggered or if the
 * associated resource is deallocated.
 *
 * @param env current environment
 * @param obj resource to use for monitor
 * @param target_pid process to monitor
 * @param mon on output, monitor object (can be NULL)
 * @return 0 on success, <0 if no down callback is provided with resource (badarg), >0 if the process is no longer alive
 */
int enif_monitor_process(ErlNifEnv *env, void *obj, const ErlNifPid *target_pid, ErlNifMonitor *mon);

/**
 * @brief Unmonitor a process
 *
 * @param caller_env current environment
 * @param obj resource used by monitor
 * @param mon monitor
 * @return 0 on success
 */
int enif_demonitor_process(ErlNifEnv *caller_env, void *obj, const ErlNifMonitor *mon);

/**
 * @brief compare two monitors
 *
 * @param monitor1 first monitor
 * @param monitor2 second monitor
 * @return 0 if equals, < 0 if `monitor1` < `monitor2`, > 0 if `monitor1` > `monitor2`.
 */
int enif_compare_monitors(const ErlNifMonitor *monitor1, const ErlNifMonitor *monitor2);

#ifdef __cplusplus
}
#endif

#endif // _ERL_NIF_H_
