/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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
 * @file sys.h
 * @brief Platform specific functions.
 *
 * @details This header defines platform dependent functions, that mostly deals
 * with events. Some functions can be implemented by using functions defined
 * and implemented in `listeners.h` header.
 */

#ifndef _SYS_H_
#define _SYS_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "globalcontext.h"
#include "module.h"

#include <stdint.h>
#include <time.h>

enum
{
    SYS_POLL_EVENTS_DO_NOT_WAIT = 0,
    SYS_POLL_EVENTS_WAIT_FOREVER = -1
};

/**
 * @brief Event listener
 *
 * An event listener structure should be defined by the platform. Event
 * listeners belong to the `GlobalContext.listeners` synchronized list.
 */
typedef struct EventListener EventListener;

/**
 * @brief Event handlers (for ports)
 *
 * @details The event handler is called from the scheduler thread but outside
 * any process. It can send messages to processes using `globalcontext_send_message`
 * function.
 *
 * Result of this callback alters the list of handlers which is locked for
 * writing when it is called. It can:
 * - return `listener`, in which case the list is not modified
 * - return `NULL`, in which case the entry is removed. The callback is
 * responsible for freeing the listener.
 * - return another listener, in which case the current listener is replaced
 * by the other listener. The callback is responsible for freeing the previous
 * listener if it is no longer needed.
 *
 * Appending a listener is also possible by altering the list head.
 *
 * This callback is defined for platforms using `listeners.h` header and can be
 * ignored by others.
 *
 * @param glb global context
 * @param listener the current listener
 * @return NULL if the current listener should be removed, listener if it
 * should be kept or another listener if it should be replaced.
 */
typedef EventListener *(*event_handler_t)(GlobalContext *glb, EventListener *listener);

/**
 * @brief Poll events (from drivers and select events), with a timeout (in ms),
 * or until `sys_signal` is called.
 *
 * @details Depending on platforms, check all open file descriptors/queues and
 * call drivers that should send messages to contexts (which will unblock them).
 * With SMP builds, this function can be called from any scheduler.
 *
 * If selectable events are supported on the platform, this function should also:
 * - call `select_event_destroy` on select events that have close set to 1
 * - include the set of ErlNifEvent that are marked for read or write in the
 * select set, and if they are selected, call `select_event_notify` to send
 * the notification.
 *
 * `select_event_count_and_destroy_closed` defined in resources.h can be used
 * to process closed select events.
 *
 * @param glb the global context.
 * @param timeout_ms the number of ms to wait, `SYS_POLL_EVENTS_WAIT_FOREVER` to wait forever.
 */
void sys_poll_events(GlobalContext *glb, int timeout_ms);

/**
 * @brief Register a listener.
 *
 * @details This function is called by ports to register a listener which is a
 * native alternative to select events. The actual definition of listeners
 * is platform dependent.
 *
 * @param global the global context.
 * @param listener the listener to register
 */
void sys_register_listener(GlobalContext *global, EventListener *listener);

/**
 * @brief Unregister a listener.
 *
 * @details This function is called by ports to unregister a listener which is
 * a native alternative to select events. The actual definition of listeners
 * is platform dependent.
 *
 * @param global the global context.
 * @param listener the listener to unregister.
 */
void sys_unregister_listener(GlobalContext *global, EventListener *listener);

/**
 * @brief Free a listener
 *
 * @details This function is called when the global context is destroyed on
 * every remaining listener. An implementation is available in `listeners.h`.
 *
 * @param item list item
 */
void sys_listener_destroy(struct ListHead *item);

#ifndef AVM_NO_SMP

/**
 * @brief Interrupt the wait in `sys_poll_events`.
 *
 * @details This function should signal the thread that is waiting in
 * `sys_poll_events` so it returns before the timeout. It is only used
 * for SMP builds.
 *
 * Please note that this function may be called while no thread is waiting in
 * sys_poll_events and this should have no effect. This function is called in
 * scheduler loop (internal function `scheduler_run0`).
 *
 * @param glb the global context.
 */
void sys_signal(GlobalContext *glb);

#endif

/**
 * @brief gets wall clock time
 *
 * @details gets system wall clock time.
 * @param t the timespec that will be updated.
 */
void sys_time(struct timespec *t);

/**
 * @brief gets monotonic time
 *
 * @details gets monotonic time.
 * @param t the timespec that will be updated.
 */
void sys_monotonic_time(struct timespec *t);

/**
 * @brief Loads a BEAM module using platform dependent methods.
 *
 * @details Loads a BEAM module into memory using platform dependent methods and returns a pointer to a Module struct.
 * @param global the global context.
 * @param module_name the name of the BEAM file (e.g. "mymodule.beam").
 */
Module *sys_load_module(GlobalContext *global, const char *module_name);

/**
 * @brief Create a port driver
 * @details This function creates a port driver, enscapsulated in a Context object.  This function should
 * create a Context object through the supplied global context, which will assume ownership of the new instance.
 * @param glb the global context
 * @param opts the term options passed into the port open command
 * @return a new Context instance, or NULL, if a driver cannot be created from the inputs.
 */
Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts);

/**
 * @brief Get platform-dependent information for the specified key.
 * @details This function returns platform-depndent information specified by the supplied key.
 * If not information is available for the specified key, this function should return the
 * atom 'undefined'
 * @param ctx the current context
 * @param key an atom used to indicate the type of information requested.
 * @return a term containing the requested information, or the atom undefined, if
 * there is no system information for the specified key.
 */
term sys_get_info(Context *ctx, term key);

/**
 * @brief Initialize the platform, setting global->platform_data.
 */
void sys_init_platform(GlobalContext *global);

/**
 * @brief Free the platform data structure.
 *
 * @details Cleanup the platform data structure. Called by
 * global_context_destroy.
 */
void sys_free_platform(GlobalContext *global);

uint64_t sys_millis(GlobalContext *global);

#ifdef __cplusplus
}
#endif

#endif
