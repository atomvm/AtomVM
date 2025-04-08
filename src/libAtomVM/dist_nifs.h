/*
 * This file is part of AtomVM.
 *
 * Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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
 * @file dist_nifs.h
 * @brief Declaration of distribution NIFs and resources
 */

#ifndef _DIST_NIFS_H_
#define _DIST_NIFS_H_

#include "exportedfunction.h"
#include "globalcontext.h"
#include "term.h"

#ifdef __cplusplus
extern "C" {
#endif

#define DIST_OTP_RELEASE "27"

extern const ErlNifResourceTypeInit dist_connection_resource_type_init;

extern const struct Nif setnode_3_nif;
extern const struct Nif dist_ctrl_get_data_notification_nif;
extern const struct Nif dist_ctrl_get_data_nif;
extern const struct Nif dist_ctrl_put_data_nif;

struct DistConnection;

/**
 * @doc Enqueue a message to be sent to a remote process.
 * This function may raise a badarg error following OTP if target is incorrect.
 * @param target external pid or a tuple {atom(), node()} to refer to a remote
 * registered process
 * @param payload message to send
 * @param ctx process that sends the message.
 * @return the payload if the message was sent or term_invalid if there was
 * a badarg error
 */
term dist_send_message(term target, term payload, Context *ctx);

/**
 * @doc Setup a monitor on a local process for a distributed process.
 * @end
 * @param conn_obj object of the connection
 * @param from_pid remote pid setting up the monitor
 * @param target_proc atom (for registered process) or pid of the local
 * process to monitor
 * @param monitor_ref reference used for monitor
 * @param ctx context for memory allocation
 */
term dist_monitor(struct DistConnection *conn_obj, term from_pid, term target_proc, term monitor_ref, Context *ctx);

/**
 * @doc Send a spawn reply signal to a node
 * @end
 * @param conn_obj object of the connection
 * @param req_id reference identifying the request
 * @param to_pid (remote) process id identifying the caller
 * @param link if a link was created
 * @param monitor if a monitor was created
 * @param result pid of the spawned process or atom for an error
 * @param ctx context for memory allocation
 */
void dist_spawn_reply(term req_id, term to_pid, bool link, bool monitor, term result, struct DistConnection *connection, GlobalContext *global);

#ifdef __cplusplus
}
#endif

#endif
