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
 * @file listeners.h
 * @brief Common code for port listeners.
 *
 * @details This header defines convenient common functions to implement
 * listeners, and should be included in platform's `sys.c`.
 *
 * Before including this file, define listener_event_t which represent a
 * selectable event, as well as EventListener, which should have a list head
 * member called `listeners_list_head` and a handler member called `handler`.
 *
 * On a platform using select(3) with file descriptors, this typically is done
 * by creating a `platform_sys.h` header with:
 * ```
 * #include "sys.h"
 *
 * typedef int listener_event_t;
 *
 * struct EventListener
 * {
 *    struct ListHead listeners_list_head;
 *    event_handler_t handler;
 *    listener_event_t fd;
 * };
 * ```
 *
 * and by including `platform_sys.h` header in `sys.c` before `listeners.h`.
 */

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Add an event listener to the set of polled events.
 *
 * @details This function must be implemented and will typically access the
 * platform data from `glb` and add the event to the set. It is called by
 * `process_listener_handler` when a handler returns a new listener. It can be
 * called by `sys_register_listener`. It may just set a dirty flag.
 *
 * @param listener the listener to add to polling set
 * @param glb the global context
 */
static void event_listener_add_to_polling_set(struct EventListener *listener, GlobalContext *glb);

/**
 * @brief Remove an event from the set of polled events.
 *
 * @details This function must be implemented and will typically access the
 * platform data from `glb` and remove the event to the set. It is called by
 * `process_listener_handler` when a handler returns NULL or a new listener. It
 * can be called by `sys_unregister_listener`. It may just set a dirty flag.
 *
 * Compared to `event_listener_add_to_polling_set`, the event listener may no
 * longer exist if it was freed by the handler.
 *
 * @param event the listener event to remove from polling set
 * @param glb the global context
 */
static void listener_event_remove_from_polling_set(listener_event_t event, GlobalContext *glb);

/**
 * @brief Determiner if an event is a listener's event.
 *
 * @param listener the listener to test
 * @param event the event to test
 * @return true if event is the listener's event
 */
static bool event_listener_is_event(EventListener *listener, listener_event_t event);

/**
 * @brief Process listener handlers, optionally in advancing order, especially useful with poll(2) which returns fd in the provided order.
 *
 * @param glb the global context
 * @param current_event the selected event
 * @param listeners the list of listeners (locked for writing)
 * @param item_ptr the current cursor or NULL to search in items
 * @param previous_ptr the previous cursor (ignored and can be NULL if item_ptr is NULL).
 * @return true if the current_event was found
 */
static inline bool process_listener_handler(GlobalContext *glb, listener_event_t current_event, struct ListHead *listeners, struct ListHead **item_ptr, struct ListHead **previous_ptr)
{
    bool result = false;
    struct ListHead *item;
    struct ListHead *previous;
    if (item_ptr) {
        item = *item_ptr;
        previous = *previous_ptr;
    } else {
        item = listeners->next;
        previous = listeners;
    }

    while (item != listeners) {
        struct ListHead *next = item->next;
        EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
        if (event_listener_is_event(listener, current_event)) {
            EventListener *new_listener = listener->handler(glb, listener);
            if (new_listener == NULL) {
                listener_event_remove_from_polling_set(current_event, glb);
                previous->next = next;
                next->prev = previous;
                item = next;
            } else if (new_listener != listener) {
                listener_event_remove_from_polling_set(current_event, glb);
                event_listener_add_to_polling_set(new_listener, glb);
                // Replace listener with new_listener in the list
                // listener was freed by handler.
                previous->next = &new_listener->listeners_list_head;
                next->prev = &new_listener->listeners_list_head;
                new_listener->listeners_list_head.prev = previous;
                new_listener->listeners_list_head.next = next;
                item = &new_listener->listeners_list_head;
            }
            result = true;
            break;
        }
        previous = item;
        item = next;
    }
    if (item_ptr) {
        *previous_ptr = previous;
        *item_ptr = item;
    }
    return result;
}

#ifndef DOXYGEN_SKIP_SECTION /* documented in sys.h */
void sys_listener_destroy(struct ListHead *item)
{
    EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
    free(listener);
}
#endif /* DOXYGEN_SKIP_SECTION */

#ifdef __cplusplus
}
#endif
