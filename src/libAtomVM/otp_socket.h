/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Fred Dushin <fred@dushin.net>
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

#ifndef _OTP_SOCKET_H_
#define _OTP_SOCKET_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <globalcontext.h>
#include <term.h>

struct EventListenerKey
{
    void *sender;
    int fd;
};

Context *otp_socket_create_port(GlobalContext *global, term opts);

// platform-specific function implementations
void otp_socket_platform_notify_add(Context *ctx, int fd);
void otp_socket_platform_notify_remove(Context *ctx, int fd);
void otp_socket_platform_set_listener(void *listener, struct EventListenerKey *key);
bool otp_socket_platform_supports_peek();

#ifdef __cplusplus
}
#endif

#endif
