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

#include "otp_socket_platform.h"

//
// platform-specific function implementations
//

void otp_socket_platform_notify_add(Context *ctx, int fd)
{
    UNUSED(ctx);
    UNUSED(fd);

    // no-op
}

void otp_socket_platform_notify_remove(Context *ctx, int fd)
{
    UNUSED(ctx);
    UNUSED(fd);

    // no-op
}

struct ListHead *otp_socket_platform_get_listeners(void *platform_data)
{
    return &((struct GenericUnixPlatformData *) platform_data)->listeners;
}

void otp_socket_platform_set_listener(EventListener *listener, void *sender, int fd)
{
    UNUSED(sender);

    listener->fd = fd;
}

bool otp_socket_platform_supports_peek()
{
    return false;
}
