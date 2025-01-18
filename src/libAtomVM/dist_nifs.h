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

extern const ErlNifResourceTypeInit dist_connection_resource_type_init;

extern const struct Nif setnode_3_nif;
extern const struct Nif dist_ctrl_get_data_notification_nif;
extern const struct Nif dist_ctrl_get_data_nif;
extern const struct Nif dist_ctrl_put_data_nif;

void dist_send_message(term external_pid, term payload, Context *ctx);

#ifdef __cplusplus
}
#endif

#endif
