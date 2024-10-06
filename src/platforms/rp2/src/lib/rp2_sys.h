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

#ifndef _RP2_SYS_H_
#define _RP2_SYS_H_

#include <time.h>

// Pico SDK
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <pico/cond.h>
#include <pico/util/queue.h>

#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>

#pragma GCC diagnostic pop

#include "portnifloader.h"
#include "sys.h"

// Because pico sdk uses -gc-section, it is required to add -Wl,-u NAME_nif
// to make sure a given module nif or port are linked in.

#define EVENT_QUEUE_LEN 16

typedef queue_t *listener_event_t;

struct EventListener
{
    struct ListHead listeners_list_head;
    event_handler_t handler;
    listener_event_t queue;
};

/**
 * @brief Post an event from ISR to trigger a listener call from task context.
 * @param global            the global context
 * @param listener_queue    the listener's queue (EventListener->queue)
 * @param event             the event to enqueue (copied)
 * @returns true if successful, false if either queue is full
 * @details This function should be called from ISR callbacks to postpone
 * processing in task context (from sys_poll_events). If the system is overloaded
 * with events, it prints a message to stderr and returns false.
 * @end
 */
bool sys_try_post_listener_event_from_isr(GlobalContext *global, listener_event_t listener_queue, const void *event);

/**
 * @brief Unregister a listener using its queue.
 * @param global            the global context
 * @param listener_queue    the listener's queue (EventListener->queue)
 * @details We can avoid storing the listeners in platform data.
 * @end
 */
void sys_unregister_listener_from_event(GlobalContext *global, listener_event_t listener_queue);

struct RP2PlatformData
{
#ifndef AVM_NO_SMP
    mutex_t event_poll_mutex;
    cond_t event_poll_cond;
#endif
    queue_t event_queue;

#ifndef AVM_NO_SMP
    Mutex *entropy_mutex;
#endif
    mbedtls_entropy_context entropy_ctx;
    bool entropy_is_initialized;

#ifndef AVM_NO_SMP
    Mutex *random_mutex;
#endif
    mbedtls_ctr_drbg_context random_ctx;
    bool random_is_initialized;
};

#endif
