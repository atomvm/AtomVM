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

#include "context.h"
#include "globalcontext.h"
#include "linkedlist.h"

#define DEFAULT_REDUCTIONS_AMOUNT 1024

/**
 * @brief move a process to waiting queue and wait a ready one
 *
 * @details move current process to the waiting queue, and schedule the next one or sleep until an event is received.
 * @param global the global context.
 * @param c the process context.
 */
Context *scheduler_wait(GlobalContext *global, Context *c);

Context *scheduler_do_wait(GlobalContext *global);

/**
 * @brief make sure a process is on the ready queue
 *
 * @details make a process ready again by moving it to the ready queue.
 * @param global the global context.
 * @param c the process context.
 */
void scheduler_make_ready(GlobalContext *global, Context *c);

/**
 * @brief just move a process to the wait queue
 *
 * @details make a process waiting.
 * @param global the global context.
 * @param c the process context.
 */
void scheduler_make_waiting(GlobalContext *global, Context *c);

/**
 * @brief removes a process and terminates it from the scheduling queue
 *
 * @detail removes a process from the scheduling ready queue and destroys it if its not a leader process.
 * @param global the global context.
 * @param c the process that is going to be terminated.
 */
void scheduler_terminate(Context *c);

/**
 * @brief the number of processes
 *
 * @detail counts the number of processes that are registered on the processes table.
 * @param global the global context.
 * @returns the total number of processes in the processes table.
 */
int schudule_processes_count(GlobalContext *global);

/**
 * @brief gets next runnable process from the ready queue.
 *
 * @detail gets next runnable process from the ready queue, it may return current process if there isn't any other runnable process.
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
void scheduler_set_timeout(Context *ctx, uint32_t timeout);

void scheduler_cancel_timeout(Context *ctx);

#endif
