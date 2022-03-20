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
 * @details This header defines platform dependent functions, that mostly deals with events.
 */

#ifndef _SYS_H_
#define _SYS_H_

#include "globalcontext.h"
#include "linkedlist.h"
#include "module.h"

#include <stdint.h>
#include <time.h>

/**
 * @brief process any pending event without blocking
 *
 * @details check all open file descriptors/queues, dispatch messages for new events and wake up contexts accordingly.
 * @param glb the global context.
 */
void sys_consume_pending_events(GlobalContext *glb);

/**
 * @brief gets wall clock time
 *
 * @details gets system wall clock time.
 * @param t the timespec that will be updated.
 */
void sys_time(struct timespec *t);

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

void sys_init_platform(GlobalContext *global);

void sys_start_millis_timer();

void sys_stop_millis_timer();

uint32_t sys_millis();

void sys_sleep(GlobalContext *glb);

#endif
