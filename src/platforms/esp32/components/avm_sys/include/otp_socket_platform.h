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

#ifdef __cplusplus
extern "C" {
#endif

#include <esp_log.h>

#define TAG "otp_socket"

#define AVM_LOGD(tag, format, ...) \
    ;

#define AVM_LOGI ESP_LOGI
#define AVM_LOGW ESP_LOGW
#define AVM_LOGE ESP_LOGE

inline bool otp_socket_platform_supports_peek()
{
    return true;
}

#ifdef __cplusplus
}
#endif

#endif
