/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
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

#include <stdbool.h>
#include "avm_log.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef INADDR_LOOPBACK
#define INADDR_LOOPBACK 0x7f000001UL
#endif

inline bool otp_socket_platform_supports_peek(void)
{
    // Zephyr's MSG_PEEK stream path does not support AtomVM's NULL-buffer
    // availability probe reliably when data spans multiple packet buffers.
    return false;
}

#ifdef __cplusplus
}
#endif

#endif /* __OTP_SOCKET_PLATFORM_H__ */
