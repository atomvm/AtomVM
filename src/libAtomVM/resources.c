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
#include "refc_binary.h"
#include "resources.h"
#include "sys.h"
#include "utils.h"

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
    list_init(&result->monitors);
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
    return (void *) refc_binary_get_data(refc);
}

int enif_get_resource(ErlNifEnv *env, ERL_NIF_TERM t, ErlNifResourceType *type, void **objp)
{
    UNUSED(env);

    if (UNLIKELY(!term_is_refc_binary(t))) {
        return false;
    }
    if (UNLIKELY(term_refc_binary_is_const(t))) {
        return false;
    }
    const term *boxed_value = term_to_const_term_ptr(t);
    struct RefcBinary *refc = (struct RefcBinary *) boxed_value[3];
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
    if (UNLIKELY(memory_erl_nif_env_ensure_free(env, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    struct RefcBinary *refc = refc_binary_from_data(obj);
    refc_binary_increment_refcount(refc);
    return term_from_resource(obj, &env->heap);
}

int enif_select(ErlNifEnv *env, ErlNifEvent event, enum ErlNifSelectFlags mode, void *obj, const ErlNifPid *pid, ERL_NIF_TERM ref)
{
    if (!(mode & (ERL_NIF_SELECT_STOP | ERL_NIF_SELECT_READ | ERL_NIF_SELECT_WRITE))) {
        return ERL_NIF_SELECT_BADARG;
    }
    if (UNLIKELY(mode & (ERL_NIF_SELECT_READ | ERL_NIF_SELECT_WRITE) && !term_is_reference(ref) && ref != UNDEFINED_ATOM)) {
        return ERL_NIF_SELECT_BADARG;
    }

    struct RefcBinary *resource = refc_binary_from_data(obj);
    // Search for event and obj
    struct ListHead *item;
    struct ListHead *select_events = synclist_wrlock(&env->global->select_events);
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
            synclist_unlock(&env->global->select_events);
            return ERL_NIF_SELECT_INVALID_EVENT;
        }
        bool was_read = select_event->read;
        bool was_write = select_event->write;
        if (!was_read && !was_write) {
            list_remove(&select_event->head);
            synclist_unlock(&env->global->select_events);
            // We can call stop now.
            if (resource->resource_type->stop) {
                resource->resource_type->stop(env, obj, event, true);
            }
            refc_binary_decrement_refcount(resource, env->global);
            free((void *) select_event);
            return ERL_NIF_SELECT_STOP_CALLED;
        }
        // We cannot call stop now because scheduler loop unlocks after building
        // the select set but before calling select (or equivalent)
        // So instead we flag the event.
        select_event->close = 1;
        select_event->read = 0;
        select_event->write = 0;
        synclist_unlock(&env->global->select_events);
        // Platform loop should check close flag after unregister is called
        if (was_read) {
            sys_unregister_select_event(env->global, event, false);
        }
        if (was_write) {
            sys_unregister_select_event(env->global, event, true);
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
        // Resource is used in select_event, so we increase refcount.
        refc_binary_increment_refcount(resource);
        list_init(&select_event->head);
        list_append(select_events, &select_event->head);
    }
    // Second read or second write overwrite ref & pid.
    if (ref == UNDEFINED_ATOM) {
        select_event->ref_ticks = 0;
    } else {
        select_event->ref_ticks = term_to_ref_ticks(ref);
    }
    select_event->local_pid = *pid;
    select_event->read = mode & ERL_NIF_SELECT_READ;
    select_event->write = mode & ERL_NIF_SELECT_WRITE;
    select_event->close = 0;
    synclist_unlock(&env->global->select_events);
    if (select_event->read) {
        sys_register_select_event(env->global, event, false);
    }
    if (select_event->write) {
        sys_register_select_event(env->global, event, true);
    }
    return 0;
}

term select_event_make_notification(void *rsrc_obj, uint64_t ref_ticks, bool is_write, Heap *heap)
{
    term notification = term_alloc_tuple(4, heap);
    term_put_tuple_element(notification, 0, SELECT_ATOM);
    term_put_tuple_element(notification, 1, term_from_resource(rsrc_obj, heap));
    struct RefcBinary *rsrc_refc = refc_binary_from_data(rsrc_obj);
    refc_binary_increment_refcount(rsrc_refc);
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
    BEGIN_WITH_STACK_HEAP(SELECT_EVENT_NOTIFICATION_SIZE, heap)
    term notification = select_event_make_notification(select_event->resource->data, select_event->ref_ticks, is_write, &heap);
    globalcontext_send_message(global, select_event->local_pid, notification);
    if (is_write) {
        select_event->write = 0;
    } else {
        select_event->read = 0;
    }
    END_WITH_STACK_HEAP(heap, global)
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
    synclist_unlock(&global->select_events);
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
    return result;
}

static inline void select_event_destroy(struct SelectEvent *select_event, GlobalContext *global)
{
    if (select_event->resource->resource_type->stop) {
        ErlNifEnv env;
        erl_nif_env_partial_init_from_globalcontext(&env, global);
        select_event->resource->resource_type->stop(&env, select_event->resource->data, select_event->event, false);
    }
    refc_binary_decrement_refcount(select_event->resource, global);
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
    if (resource->resource_type == NULL || resource->resource_type->down == NULL) {
        return -1;
    }

    Context *target = globalcontext_get_process_lock(env->global, *target_pid);
    if (IS_NULL_PTR(target)) {
        return 1;
    }

    struct ResourceMonitor *monitor = context_resource_monitor(target, obj);
    list_append(&resource->resource_type->monitors, &monitor->resource_list_head);
    globalcontext_get_process_unlock(env->global, target);

    if (mon) {
        *mon = monitor->base.ref_ticks;
    }

    return 0;
}

int enif_demonitor_process(ErlNifEnv *env, void *obj, const ErlNifMonitor *mon)
{
    GlobalContext *global = env->global;
    struct RefcBinary *resource = refc_binary_from_data(obj);
    if (resource->resource_type == NULL || resource->resource_type->down == NULL) {
        return -1;
    }

    struct ListHead *processes_table_list = synclist_wrlock(&global->processes_table);
    UNUSED(processes_table_list);

    struct ListHead *item;
    LIST_FOR_EACH (item, &resource->resource_type->monitors) {
        struct ResourceMonitor *monitor = GET_LIST_ENTRY(item, struct ResourceMonitor, resource_list_head);
        if (monitor->base.ref_ticks == *mon) {
            list_remove(&monitor->resource_list_head);
            list_remove(&monitor->base.monitor_list_head);
            free(monitor);
            synclist_unlock(&global->processes_table);
            return 0;
        }
    }

    synclist_unlock(&global->processes_table);

    return -1;
}

void destroy_resource_monitors(struct RefcBinary *resource, GlobalContext *global)
{
    struct ListHead *processes_table_list = synclist_wrlock(&global->processes_table);
    UNUSED(processes_table_list);
    term monitor_obj = ((term) resource->data) | TERM_BOXED_VALUE_TAG;

    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, &resource->resource_type->monitors) {
        struct ResourceMonitor *monitor = GET_LIST_ENTRY(item, struct ResourceMonitor, resource_list_head);
        if (monitor->base.monitor_obj == monitor_obj) {
            list_remove(&monitor->resource_list_head);
            list_remove(&monitor->base.monitor_list_head);
            free(monitor);
        }
    }

    synclist_unlock(&global->processes_table);
}

int enif_compare_monitors(const ErlNifMonitor *monitor1, const ErlNifMonitor *monitor2)
{
    uint64_t ref_ticks1 = *monitor1;
    uint64_t ref_ticks2 = *monitor2;
    if (ref_ticks1 < ref_ticks2) {
        return -1;
    }
    if (ref_ticks1 > ref_ticks2) {
        return 1;
    }
    return 0;
}
