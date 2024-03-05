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
 * @file scheduler.h
 * @brief Scheduling functions.
 *
 * @details Scheduling functions are used to schedule processes.
 */

#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "context.h"
#include "globalcontext.h"

#define DEFAULT_REDUCTIONS_AMOUNT 1024

/**
 * @brief run the scheduler and return a process to be executed.
 *
 * @param global the global context.
 */
Context *scheduler_run(GlobalContext *global);

/**
 * @brief move a process to waiting queue and wait a ready one
 *
 * @details move current process to the waiting queue, and schedule the next one or sleep until an event is received.
 * @param c the process context.
 */
Context *scheduler_wait(Context *c);

/**
 * @brief Init a process in the ready state, moving it to the scheduler queue.
 *
 * @param c the process context.
 */
void scheduler_init_ready(Context *c);

/**
 * @brief Signal a process that a message was inserted in the mailbox.
 * @details Cannot be called from a foreign task or from ISR.
 *
 * @param c the process context.
 */
void scheduler_signal_message(Context *c);

#ifdef AVM_TASK_DRIVER_ENABLED
/**
 * @brief Signal a process that a message was inserted in the mailbox.
 * @details Must only be called while global->processes_spinlock is acquired.
 * @param c the process context.
 */
void scheduler_signal_message_from_task(Context *c);
#endif

/**
 * @brief Signal a process that it was killed.
 *
 * @param ctx the process context.
 */
void scheduler_kill(Context *ctx);

/**
 * @brief removes a process and terminates it from the scheduling queue
 *
 * @details removes a process from the scheduling ready queue and destroys it if its not a leader process.
 * @param c the process that is going to be terminated.
 */
void scheduler_terminate(Context *c);

/**
 * @brief Terminate all schedulers. Every process is terminated gracefully at next scheduling point.
 *
 * @param global the global context.
 */
void scheduler_stop_all(GlobalContext *global);

/**
 * @brief gets next runnable process from the ready queue.
 *
 * @details gets next runnable process from the ready queue, it may return current process if there isn't any other runnable process.
 * @param global the global context.
 * @param c the current process.
 * @returns runnable process.
 */
Context *scheduler_next(GlobalContext *global, Context *c);

/**
 * @brief sets context timeout
 *
 * @details set context timeout timestamp, move context to wait queue and update global next timeout timestamp.
 * @param ctx the context that will be put on sleep
 * @param timeout amount of time to be waited in milliseconds.
 */
void scheduler_set_timeout(Context *ctx, avm_int64_t timeout);

void scheduler_cancel_timeout(Context *ctx);

/**
 * @brief Entry point for schedulers.
 *
 * @param glb the global context.
 */
int scheduler_entry_point(GlobalContext *glb);

#ifdef __cplusplus
}
#endif

#endif
