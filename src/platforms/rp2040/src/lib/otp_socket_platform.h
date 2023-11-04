/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 by Fred Dushin <fred@dushin.net>
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
#include <stdio.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <pico/cyw43_arch.h>

#pragma GCC diagnostic pop

#define TAG "otp_socket"

#define AVM_LOGD(tag, format, ...) \
    ;

#define AVM_LOGI(tag, format, ...) \
    printf("I %s: " format " (%s:%i)\n", tag, ##__VA_ARGS__, __FILE__, __LINE__);

#define AVM_LOGW(tag, format, ...) \
    printf("W %s: " format " (%s:%i)\n", tag, ##__VA_ARGS__, __FILE__, __LINE__);

#define AVM_LOGE(tag, format, ...) \
    printf("E %s: " format " (%s:%i)\n", tag, ##__VA_ARGS__, __FILE__, __LINE__);

// Define macros to lock/unlock lwIP
#define LWIP_BEGIN() cyw43_arch_lwip_begin()
#define LWIP_END() cyw43_arch_lwip_end()

#endif
