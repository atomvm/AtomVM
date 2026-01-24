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

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "context.h"
#include "defaultatoms.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "globalcontext.h"
#include "list.h"
#include "refc_binary.h"
#include "resources.h"
#include "synclist.h"
#include "sys.h"
#include "utils.h"

void resource_type_destroy(struct ResourceType *resource_type)
{
    free((void *) resource_type->name);
    // Assume we have no monitor left.
    synclist_destroy(&resource_type->monitors);
    // Assume we have no serialized resource left.
    synclist_destroy(&resource_type->serialized);
    free(resource_type);
}

ErlNifResourceType *enif_init_resource_type(ErlNifEnv *env, const char *name, const ErlNifResourceTypeInit *init, ErlNifResourceFlags flags, ErlNifResourceFlags *tried)
{
    if (tried) {
        *tried = flags;
    }
    struct ResourceType *result = malloc(sizeof(struct ResourceType));
    if (UNLIKELY(result == NULL)) {
        return NULL;
    }
    result->name = strdup(name);
    result->global = env->global;
    list_init(&result->head);
    synclist_init(&result->monitors);
    synclist_init(&result->serialized);
    result->dtor = NULL;
    result->stop = NULL;
    result->down = NULL;
    if (init->members >= 1) {
        result->dtor = init->dtor;
        if (init->members >= 2) {
            result->stop = init->stop;
            if (init->members >= 3) {
                result->down = init->down;
            }
        }
    }
    synclist_append(&env->global->resource_types, &result->head);

    return result;
}

void *enif_alloc_resource(ErlNifResourceType *type, unsigned size)
{
    struct RefcBinary *refc = refc_binary_create_resource(size, type);
    if (UNLIKELY(refc == NULL)) {
        return NULL;
    }
    // We add it now to the list of refc binaries, so resource is destroyed at
    // the latest when globalcontext is destroyed
    synclist_append(&type->global->refc_binaries, &refc->head);
    // OTP semantics suppose the ref count is 1 as each alloc_resource should
    // be balanced by a release_resource.
    refc->ref_count = 1;
    return (void *) refc_binary_get_data(refc);
}

int enif_get_resource(ErlNifEnv *env, ERL_NIF_TERM t, ErlNifResourceType *type, void **objp)
{
    UNUSED(env);

    if (UNLIKELY(!term_is_resource_reference(t))) {
        return false;
    }
    const term *boxed_value = term_to_const_term_ptr(t);
    struct RefcBinary *refc = (struct RefcBinary *) boxed_value[1];
    if (UNLIKELY(refc->resource_type != type)) {
        return false;
    }
    *objp = (void *) refc_binary_get_data(refc);
    return true;
}

int enif_keep_resource(void *resource)
{
    struct RefcBinary *refc = refc_binary_from_data(resource);
    refc_binary_increment_refcount(refc);
    return true;
}

int enif_release_resource(void *resource)
{
    struct RefcBinary *refc = refc_binary_from_data(resource);
    refc_binary_decrement_refcount(refc, refc->resource_type->global);
    return true;
}

ERL_NIF_TERM enif_make_resource(ErlNifEnv *env, void *obj)
{
    if (UNLIKELY(memory_erl_nif_env_ensure_free(env, TERM_BOXED_REFERENCE_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    return term_from_resource(obj, &env->heap);
}

static void enif_select_event_message_dispose(Message *message, GlobalContext *global, bool from_task)
{
    if (message) {
        mailbox_message_dispose_unsent(message, global, from_task);
    }
}

static int enif_select_common(ErlNifEnv *env, ErlNifEvent event, enum ErlNifSelectFlags mode, void *obj, const ErlNifPid *pid, ERL_NIF_TERM ref, Message *message)
{
    GlobalContext *global = env->global;
    struct RefcBinary *resource = refc_binary_from_data(obj);
    // Search for event and obj
    struct ListHead *item;
    struct ListHead *select_events = synclist_wrlock(&global->select_events);
    struct SelectEvent *select_event = NULL;
    LIST_FOR_EACH (item, select_events) {
        select_event = GET_LIST_ENTRY(item, struct SelectEvent, head);
        if (select_event->event == event && select_event->resource == resource) {
            break;
        }
        select_event = NULL;
    }
    if (mode & ERL_NIF_SELECT_STOP) {
        if (select_event == NULL) {
            synclist_unlock(&global->select_events);
            return ERL_NIF_SELECT_INVALID_EVENT;
        }
        bool was_read = select_event->read;
        bool was_write = select_event->write;
        if (!was_read && !was_write) {
            list_remove(&select_event->head);
            synclist_unlock(&global->select_events);
            // We can call stop now.
            if (resource->resource_type->stop) {
                resource->resource_type->stop(env, obj, event, true);
            }
            refc_binary_decrement_refcount(resource, global);
            enif_select_event_message_dispose(select_event->message, global, false);
            free((void *) select_event);
            return ERL_NIF_SELECT_STOP_CALLED;
        }
        // We cannot call stop now because scheduler loop unlocks after building
        // the select set but before calling select (or equivalent)
        // So instead we flag the event.
        select_event->close = 1;
        select_event->read = 0;
        select_event->write = 0;
        synclist_unlock(&global->select_events);
        // Platform loop should check close flag after unregister is called
        if (was_read) {
            sys_unregister_select_event(global, event, false);
        }
        if (was_write) {
            sys_unregister_select_event(global, event, true);
        }
        return ERL_NIF_SELECT_STOP_SCHEDULED;
    }
    // Create new event if it doesn't exist.
    if (select_event == NULL) {
        select_event = malloc(sizeof(struct SelectEvent));
        if (IS_NULL_PTR(select_event)) {
            AVM_ABORT();
        }
        select_event->event = event;
        select_event->resource = resource;
        select_event->message = NULL;
        select_event->ref_ticks = 0;
        // Resource is used in select_event, so we increase refcount.
        refc_binary_increment_refcount(resource);
        list_init(&select_event->head);
        list_append(select_events, &select_event->head);
    }
    // Second read or second write overwrite ref/message & pid.
    enif_select_event_message_dispose(select_event->message, global, false);
    select_event->message = message;
    if (message) {
        select_event->ref_ticks = 0;
    } else {
        if (ref == UNDEFINED_ATOM) {
            select_event->ref_ticks = 0;
        } else {
            select_event->ref_ticks = term_to_ref_ticks(ref);
        }
    }
    select_event->local_pid = *pid;
    select_event->read = mode & ERL_NIF_SELECT_READ;
    select_event->write = mode & ERL_NIF_SELECT_WRITE;
    select_event->close = 0;
    synclist_unlock(&global->select_events);
    if (select_event->read) {
        sys_register_select_event(global, event, false);
    }
    if (select_event->write) {
        sys_register_select_event(global, event, true);
    }
    return 0;
}

int enif_select(ErlNifEnv *env, ErlNifEvent event, enum ErlNifSelectFlags mode, void *obj, const ErlNifPid *pid, ERL_NIF_TERM ref)
{
    if (!(mode & (ERL_NIF_SELECT_STOP | ERL_NIF_SELECT_READ | ERL_NIF_SELECT_WRITE))) {
        return ERL_NIF_SELECT_BADARG;
    }
    if (UNLIKELY(mode & (ERL_NIF_SELECT_READ | ERL_NIF_SELECT_WRITE) && !term_is_local_reference(ref) && ref != UNDEFINED_ATOM)) {
        return ERL_NIF_SELECT_BADARG;
    }
    return enif_select_common(env, event, mode, obj, pid, ref, NULL);
}

int enif_select_read(ErlNifEnv *env, ErlNifEvent event, void *obj, const ErlNifPid *pid, ERL_NIF_TERM msg, ErlNifEnv *msg_env)
{
    if (UNLIKELY(msg_env != NULL)) {
        return ERL_NIF_SELECT_BADARG;
    }
    Message *message = mailbox_message_create_normal_message_from_term(msg);
    enum ErlNifSelectFlags mode = ERL_NIF_SELECT_READ;
    return enif_select_common(env, event, mode, obj, pid, term_nil(), message);
}

term select_event_make_notification(void *rsrc_obj, uint64_t ref_ticks, bool is_write, Heap *heap)
{
    term notification = term_alloc_tuple(4, heap);
    term_put_tuple_element(notification, 0, SELECT_ATOM);
    term_put_tuple_element(notification, 1, term_from_resource(rsrc_obj, heap));
    term ref;
    if (ref_ticks == 0) {
        ref = UNDEFINED_ATOM;
    } else {
        ref = term_from_ref_ticks(ref_ticks, heap);
    }
    term_put_tuple_element(notification, 2, ref);
    term_put_tuple_element(notification, 3, is_write ? READY_OUTPUT_ATOM : READY_INPUT_ATOM);
    return notification;
}

static void select_event_send_notification(struct SelectEvent *select_event, bool is_write, GlobalContext *global)
{
    if (select_event->message) {
        enum SendMessageResult result;
#ifdef AVM_SELECT_IN_TASK
        result = globalcontext_post_message_from_task(global, select_event->local_pid, select_event->message);
#else
        result = globalcontext_post_message(global, select_event->local_pid, select_event->message);
#endif
        if (result == SEND_MESSAGE_OK) {
            // Ownership was properly transfered.
            // Otherwise, it will be destroyed when we have a context (when enif_select is called with stop for example)
            select_event->message = NULL;
        }
    } else {
        BEGIN_WITH_STACK_HEAP(SELECT_EVENT_NOTIFICATION_SIZE, heap)
        term notification = select_event_make_notification(select_event->resource->data, select_event->ref_ticks, is_write, &heap);
#ifdef AVM_SELECT_IN_TASK
        globalcontext_send_message_from_task(global, select_event->local_pid, NormalMessage, notification);
#else
        globalcontext_send_message(global, select_event->local_pid, notification);
#endif
        END_WITH_STACK_HEAP(heap, global)
    }
    if (is_write) {
        select_event->write = 0;
    } else {
        select_event->read = 0;
    }
    sys_unregister_select_event(global, select_event->event, is_write);
}

bool select_event_notify(ErlNifEvent event, bool is_read, bool is_write, GlobalContext *global)
{
    bool result = false;
    struct SelectEvent *select_event = NULL;
    struct ListHead *item;
    struct ListHead *select_events = synclist_wrlock(&global->select_events);
    LIST_FOR_EACH (item, select_events) {
        select_event = GET_LIST_ENTRY(item, struct SelectEvent, head);
        if (select_event->event == event) {
            break;
        }
        select_event = NULL;
    }
    if (select_event) {
        if (is_read && select_event->read) {
            select_event_send_notification(select_event, false, global);
            result = true;
        }
        if (is_write && select_event->write) {
            select_event_send_notification(select_event, true, global);
            result = true;
        }
    }
    synclist_unlock(&global->select_events);
    return result;
}

static inline void select_event_destroy(struct SelectEvent *select_event, GlobalContext *global)
{
    if (select_event->resource->resource_type->stop) {
        ErlNifEnv env;
        erl_nif_env_partial_init_from_globalcontext(&env, global);
        select_event->resource->resource_type->stop(&env, select_event->resource->data, select_event->event, false);
    }
#ifdef AVM_SELECT_IN_TASK
    globalcontext_refc_decrement_refcount_from_task(global, select_event->resource);
#else
    refc_binary_decrement_refcount(select_event->resource, global);
#endif
    enif_select_event_message_dispose(select_event->message, global, true);
    free((void *) select_event);
}

void select_event_count_and_destroy_closed(struct ListHead *select_events, size_t *read, size_t *write, size_t *either, GlobalContext *global)
{
    size_t read_count = 0;
    size_t write_count = 0;
    size_t either_count = 0;

    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, select_events) {
        struct SelectEvent *select_event = GET_LIST_ENTRY(item, struct SelectEvent, head);
        if (select_event->close) {
            list_remove(&select_event->head);
            select_event_destroy(select_event, global);
        } else {
            if (select_event->read || select_event->write) {
                if (select_event->read) {
                    read_count++;
                }
                if (select_event->write) {
                    write_count++;
                }
                either_count++;
            }
        }
    }
    if (read) {
        *read = read_count;
    }
    if (write) {
        *write = write_count;
    }
    if (either) {
        *either = either_count;
    }
}

int enif_monitor_process(ErlNifEnv *env, void *obj, const ErlNifPid *target_pid, ErlNifMonitor *mon)
{
    struct RefcBinary *resource = refc_binary_from_data(obj);
    struct ResourceType *resource_type = resource->resource_type;
    if (resource_type == NULL || resource_type->down == NULL) {
        return -1;
    }

    struct ResourceMonitor *resource_monitor = malloc(sizeof(struct ResourceMonitor));
    if (IS_NULL_PTR(resource_monitor)) {
        return 1;
    }
    uint64_t ref_ticks = globalcontext_get_ref_ticks(env->global);
    resource_monitor->resource = resource;
    resource_monitor->ref_ticks = ref_ticks;
    resource_monitor->process_id = *target_pid;

    struct Monitor *monitor = monitor_resource_monitor_new(obj, ref_ticks);
    if (IS_NULL_PTR(monitor)) {
        free(resource_monitor);
        return -1;
    }

    Context *target = globalcontext_get_process_lock(env->global, *target_pid);
    if (IS_NULL_PTR(target)) {
        free(resource_monitor);
        free(monitor);
        return 1;
    }

    synclist_append(&resource_type->monitors, &resource_monitor->resource_list_head);
    mailbox_send_monitor_signal(target, MonitorSignal, monitor);
    globalcontext_get_process_unlock(env->global, target);

    if (mon) {
        mon->ref_ticks = ref_ticks;
        mon->resource_type = resource_type;
    }

    return 0;
}

void resource_type_fire_monitor(struct ResourceType *resource_type, ErlNifEnv *env, int32_t process_id, uint64_t ref_ticks)
{
    struct RefcBinary *refc = NULL;
    struct ListHead *monitors = synclist_wrlock(&resource_type->monitors);
    struct ListHead *item;
    LIST_FOR_EACH (item, monitors) {
        struct ResourceMonitor *monitor = GET_LIST_ENTRY(item, struct ResourceMonitor, resource_list_head);
        if (monitor->ref_ticks == ref_ticks) {
            // Resource still exists.
            refc = monitor->resource;
            refc_binary_increment_refcount(refc);
            list_remove(&monitor->resource_list_head);
            free(monitor);
            break;
        }
    }

    synclist_unlock(&resource_type->monitors);

    if (refc) {
        ErlNifMonitor monitor;
        monitor.ref_ticks = ref_ticks;
        monitor.resource_type = resource_type;
        resource_type->down(env, refc->data, &process_id, &monitor);
        refc_binary_decrement_refcount(refc, env->global);
    }
}

term resource_monitor_to_resource(struct ResourceType *resource_type, uint64_t ref_ticks, Heap *heap)
{
    term result = term_invalid_term();

    struct ListHead *monitors = synclist_wrlock(&resource_type->monitors);
    struct ListHead *item;
    LIST_FOR_EACH (item, monitors) {
        struct ResourceMonitor *monitor = GET_LIST_ENTRY(item, struct ResourceMonitor, resource_list_head);
        if (monitor->ref_ticks == ref_ticks) {
            // Resource still exists.
            struct RefcBinary *refc = monitor->resource;
            result = term_from_resource(refc->data, heap);
            break;
        }
    }

    synclist_unlock(&resource_type->monitors);

    return result;
}

int enif_demonitor_process(ErlNifEnv *env, void *obj, const ErlNifMonitor *mon)
{
    GlobalContext *global = env->global;
    struct ResourceType *resource_type = mon->resource_type;
    if (resource_type->down == NULL) {
        return -1;
    }

    int32_t target_process_id = INVALID_PROCESS_ID;
    uint64_t ref_ticks = 0;

    // Phase 1: Find and remove from monitors list while holding monitors lock
    struct ListHead *monitors = synclist_wrlock(&resource_type->monitors);
    struct ListHead *item;
    LIST_FOR_EACH (item, monitors) {
        struct ResourceMonitor *monitor = GET_LIST_ENTRY(item, struct ResourceMonitor, resource_list_head);
        if (monitor->ref_ticks == mon->ref_ticks) {
            struct RefcBinary *resource = refc_binary_from_data(obj);
            if (resource->resource_type != mon->resource_type) {
                synclist_unlock(&resource_type->monitors);
                return -1;
            }

            target_process_id = monitor->process_id;
            ref_ticks = monitor->ref_ticks;
            list_remove(&monitor->resource_list_head);
            free(monitor);
            break;
        }
    }
    synclist_unlock(&resource_type->monitors);

    if (target_process_id == INVALID_PROCESS_ID) {
        return -1;
    }

    // Phase 2: Send demonitor signal without holding monitors lock
    // This avoids lock order inversion with processes_table
    Context *target = globalcontext_get_process_lock(global, target_process_id);
    if (target) {
        mailbox_send_ref_signal(target, DemonitorSignal, ref_ticks);
        globalcontext_get_process_unlock(global, target);
    }

    return 0;
}

void destroy_resource_monitors(struct RefcBinary *resource, GlobalContext *global)
{
    struct ResourceType *resource_type = resource->resource_type;

    // Phase 1: Collect monitors to destroy while holding monitors lock
    struct ListHead to_signal;
    list_init(&to_signal);

    struct ListHead *monitors = synclist_wrlock(&resource_type->monitors);
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, monitors) {
        struct ResourceMonitor *monitor = GET_LIST_ENTRY(item, struct ResourceMonitor, resource_list_head);
        if (monitor->resource == resource) {
            list_remove(&monitor->resource_list_head);
            list_append(&to_signal, &monitor->resource_list_head);
        }
    }
    synclist_unlock(&resource_type->monitors);

    // Phase 2: Send demonitor signals without holding monitors lock
    // This avoids lock order inversion with processes_table
    MUTABLE_LIST_FOR_EACH (item, tmp, &to_signal) {
        struct ResourceMonitor *monitor = GET_LIST_ENTRY(item, struct ResourceMonitor, resource_list_head);
        Context *target = globalcontext_get_process_lock(global, monitor->process_id);
        if (target) {
            mailbox_send_ref_signal(target, DemonitorSignal, monitor->ref_ticks);
            globalcontext_get_process_unlock(global, target);
        }
        list_remove(&monitor->resource_list_head);
        free(monitor);
    }
}

int enif_compare_monitors(const ErlNifMonitor *monitor1, const ErlNifMonitor *monitor2)
{
    uint64_t ref_ticks1 = monitor1->ref_ticks;
    uint64_t ref_ticks2 = monitor2->ref_ticks;
    if (ref_ticks1 < ref_ticks2) {
        return -1;
    }
    if (ref_ticks1 > ref_ticks2) {
        return 1;
    }
    return 0;
}

uint64_t resource_serialize(void *rsrc_obj, struct ResourceType *resource_type)
{
    struct ListHead *serialized_resources = synclist_wrlock(&resource_type->serialized);
    struct ListHead *item = NULL;
    uint64_t serialize_ref = 0;
    LIST_FOR_EACH (item, serialized_resources) {
        struct ResourceSerializedMark *mark = GET_LIST_ENTRY(item, struct ResourceSerializedMark, resource_list_head);
        if (mark->resource_obj == rsrc_obj) {
            serialize_ref = mark->serialize_ref;
            break;
        }
    }
    if (serialize_ref == 0) {
        struct ResourceSerializedMark *resource_mark = malloc(sizeof(struct ResourceSerializedMark));
        if (IS_NULL_PTR(resource_mark)) {
            // Not much we can do except skipping marking the resource as serialized
            synclist_unlock(&resource_type->serialized);
            return 0;
        }
        list_append(serialized_resources, &resource_mark->resource_list_head);
        resource_mark->resource_obj = rsrc_obj;
        serialize_ref = globalcontext_get_ref_ticks(resource_type->global);
        resource_mark->serialize_ref = serialize_ref;
    }
    synclist_unlock(&resource_type->serialized);
    return serialize_ref;
}

void resource_unmark_serialized(void *rsrc_obj, struct ResourceType *resource_type)
{
    struct ListHead *serialized_resources = synclist_wrlock(&resource_type->serialized);
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, serialized_resources) {
        struct ResourceSerializedMark *mark = GET_LIST_ENTRY(item, struct ResourceSerializedMark, resource_list_head);
        if (mark->resource_obj == rsrc_obj) {
            list_remove(&mark->resource_list_head);
            free(mark);
            break;
        }
    }
    synclist_unlock(&resource_type->serialized);
}

void *resource_unserialize(struct ResourceType *resource_type, uint64_t serialize_ref)
{
    void *result = NULL;
    struct ListHead *serialized_resources = synclist_rdlock(&resource_type->serialized);
    struct ListHead *item;
    LIST_FOR_EACH (item, serialized_resources) {
        struct ResourceSerializedMark *mark = GET_LIST_ENTRY(item, struct ResourceSerializedMark, resource_list_head);
        if (mark->serialize_ref == serialize_ref) {
            result = mark->resource_obj;
            break;
        }
    }
    synclist_unlock(&resource_type->serialized);
    return result;
}

static void resource_binary_dtor(ErlNifEnv *caller_env, void *obj)
{
    struct ResourceBinary *resource_binary = (struct ResourceBinary *) obj;
    refc_binary_decrement_refcount(resource_binary->managing_resource, caller_env->global);
}

const ErlNifResourceTypeInit resource_binary_resource_type_init = {
    .members = 1,
    .dtor = resource_binary_dtor,
};

ERL_NIF_TERM enif_make_resource_binary(ErlNifEnv *env, void *obj, const void *data, size_t size)
{
    if (UNLIKELY(memory_erl_nif_env_ensure_free(env, TERM_BOXED_REFERENCE_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    struct ResourceBinary *resource_binary = enif_alloc_resource(env->global->resource_binary_resource_type, sizeof(struct ResourceBinary));
    resource_binary->managing_resource = refc_binary_from_data(obj);
    resource_binary->data = data;

    term result = term_from_resource_binary_pointer(resource_binary, size, &env->heap);
    refc_binary_decrement_refcount(refc_binary_from_data(resource_binary), env->global);
    return result;
}
