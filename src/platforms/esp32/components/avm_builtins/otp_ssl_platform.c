/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 by Paul Guyot <pguyot@kallisys.net>
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

#include <esp32_sys.h>
#include <nifs.h>
#include <otp_ssl.h>
#include <sdkconfig.h>

#ifdef CONFIG_AVM_ENABLE_OTP_SSL_NIFS

#if defined(CONFIG_MBEDTLS_SSL_PROTO_TLS1_3) \
    && (!defined(CONFIG_ESP_MAIN_TASK_STACK_SIZE) || CONFIG_ESP_MAIN_TASK_STACK_SIZE < 6144)
#error "TLS 1.3 on ESP32 requires CONFIG_ESP_MAIN_TASK_STACK_SIZE >= 6144"
#endif

REGISTER_NIF_COLLECTION(otp_ssl, otp_ssl_init, NULL, otp_ssl_nif_get_nif)

#endif
