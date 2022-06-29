/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 by Fred Dushin <fred@dushin.net>
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

#ifndef __OTP_SOCKET_PLATFORM_H__
#define __OTP_SOCKET_PLATFORM_H__

#include "generic_unix_sys.h"
#include <netinet/in.h>
#include <stdio.h>
// #include <netinet/in.h>
// #include <netinet/udp.h>
#include <sys/socket.h>
// #include <sys/types.h>
#include <unistd.h>

#define TAG stderr

#define AVM_LOGI fprintf
#define AVM_LOGW fprintf
#define AVM_LOGE fprintf

// platform-specific function implementations
void otp_socket_platform_notify_add(Context *ctx, int fd);
void otp_socket_platform_notify_remove(Context *ctx, int fd);
struct ListHead *otp_socket_platform_get_listeners(void *platform_data);
void otp_socket_platform_set_listener(EventListener *listener, void *sender, int fd);
bool otp_socket_platform_supports_peek();

#endif
