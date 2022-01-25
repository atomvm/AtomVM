/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Fred Dushin <fred@dushin.net>
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

#ifndef _PORT_H_
#define _PORT_H_

#include "context.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "term.h"

term port_create_tuple2(Context *ctx, term a, term b);
term port_create_tuple3(Context *ctx, term a, term b, term c);
term port_create_tuple_n(Context *ctx, size_t num_terms, term *terms);
term port_create_error_tuple(Context *ctx, term reason);
term port_create_sys_error_tuple(Context *ctx, term syscall, int errno);
term port_create_ok_tuple(Context *ctx, term t);
void port_send_reply(Context *ctx, term pid, term ref, term reply);
void port_send_message(Context *ctx, term pid, term msg);
void port_ensure_available(Context *ctx, size_t size);
int port_is_standard_port_command(term msg);

#endif
