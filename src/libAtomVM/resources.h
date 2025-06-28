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

#ifndef _RESOURCES_H_
#define _RESOURCES_H_

#include <stdlib.h>

#include "erl_nif.h"
#include "list.h"
#include "mailbox.h"
#include "memory.h"
#include "synclist.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef TYPEDEF_GLOBALCONTEXT
#define TYPEDEF_GLOBALCONTEXT
typedef struct GlobalContext GlobalContext;
#endif

/**
 * @file resources.h
 * @brief Private data structures for nif object resources
 */

/**
 * @brief A resource type.
 * @details we need a reference to the global context as `enif_release_resource`
 * needs to access the synchronized list of refc_binaries
 */
struct ResourceType
{
    struct ListHead head;
    const char *name;
    GlobalContext *global;
    struct SyncList monitors;
    ErlNifResourceDtor *dtor;
    ErlNifResourceStop *stop;
    ErlNifResourceDown *down;
};

/**
 * @brief A resource monitor.
 */
struct ResourceMonitor
{
    struct ListHead resource_list_head;
    struct RefcBinary *resource;
    uint64_t ref_ticks;
    int32_t process_id;
};

/**
 * @brief A selectable event.
 */
struct SelectEvent
{
    struct ListHead head;
    ErlNifEvent event;
    struct RefcBinary *resource;
    bool read;
    bool write;
    bool close;
    int32_t local_pid;
    uint64_t ref_ticks;
    Message *message;
};

static inline void resource_type_destroy(struct ResourceType *resource_type)
{
    free((void *) resource_type->name);
    free(resource_type);
}

/**
 * @brief Send a notification that an event was selected
 * @details This function is called from sys_poll_events platform function
 * if a select event was selected and the read or write flag was set.
 * It modifies the select_event object so the notification is only sent once.
 *
 * The function can also be called from a select task loop if
 * `AVM_SELECT_IN_TASK` is defined.
 *
 * It is not an error to call this function with an event that is not in the
 * list.
 *
 * This function calls `sys_unregister_select_event`.
 *
 * @param event the event to notify
 * @param is_read if the event was selected for reading
 * @param is_write if the event was selected for writing
 * @param global the global context
 * @return true if the event was found
 */
bool select_event_notify(ErlNifEvent event, bool is_read, bool is_write, GlobalContext *global);

/**
 * @brief Count events available for reading and/or writing and destroy the
 * events marked for close.
 * @details Convenience function that can be called by `sys_poll_events` and
 * iterates on events to be closed and count them.
 *
 * The function can also be called from a select task loop if
 * `AVM_SELECT_IN_TASK` is defined.
 *
 * @param select_events list of events, with a write lock
 * @param read on output number of events with read = 1, can be NULL
 * @param write on output number of events with write = 1, can be NULL
 * @param either on output number of events with either read = 1 or write = 1, can be NULL
 * @param global the global context
 */
void select_event_count_and_destroy_closed(struct ListHead *select_events, size_t *read, size_t *write, size_t *either, GlobalContext *global);

/**
 * @brief Destroy monitors associated with a resource.
 *
 * @param resource resource to destroy monitors for
 * @param global the global context
 */
void destroy_resource_monitors(struct RefcBinary *resource, GlobalContext *global);

#define SELECT_EVENT_NOTIFICATION_SIZE (TUPLE_SIZE(4) + REF_SIZE + TERM_BOXED_RESOURCE_SIZE)

/**
 * @brief Build a select event notification.
 * @param rsrc_obj the resource to build the notification for
 * @param ref_ticks the reference or 0 if it's undefined
 * @param is_write if the notification is for a write or a read
 * @param heap the heap to create the notification in, should have enough memory
 * available (see SELECT_EVENT_NOTIFICATION_SIZE)
 */
term select_event_make_notification(void *rsrc_obj, uint64_t ref_ticks, bool is_write, Heap *heap);

/**
 * @brief Call down handler for a given resource and remove monitor from list.
 * @details handler is called while holding lock on the list of monitors and
 * if monitor is still in the list of resource monitors, thus ensuring that
 * the resource still exists.
 * @param resource_type type holding the list of monitors
 * @param env environment for calling the down handler
 * @param resource resource that monitored the process
 * @param process_id id of the process monitored
 * @param ref_ticks reference of the monitor
 */
void resource_type_fire_monitor(struct ResourceType *resource_type, ErlNifEnv *env, void *resource, int32_t process_id, uint64_t ref_ticks);

/**
 * @brief Remove monitor from list of monitors.
 * @param resource_type type holding the list of monitors
 * @param ref_ticks reference of the monitor
 */
void resource_type_demonitor(struct ResourceType *resource_type, uint64_t ref_ticks);

#ifdef __cplusplus
}
#endif

#endif // _RESOURCES_H_
