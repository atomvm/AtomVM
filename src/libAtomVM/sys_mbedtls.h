/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Davide Bettio <davide@uninstall.it>
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

#ifndef _SYS_MBEDTLS_H_
#define _SYS_MBEDTLS_H_

#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>

#ifdef __cplusplus
extern "C" {
#endif

// On SMP builds, mbedtls_entropy_context or mbedtls_ctr_drbg_context can be
// accessed from several scheduler threads at once through calls to functions
// such as `mbedtls_ctr_drbg_random` that may call a function on
// mbedtls_entropy_context. So they need to be protected by a mutex.
//
// On non-SMP builds, we don't need any lock because all calls we make to
// mbedtls_ctr_drbg* functions are done from the scheduler thread itself.

/**
 * @brief get and acquire lock on mbedtls_entropy_context.
 * @details this function must be called from a scheduler thread (nif,
 * native handler, listener) to access the entropy context and should be
 * balanced by `sys_mbedtls_entropy_context_unlock`.
 * On non-SMP builds, there is no lock as there is only one scheduler.
 *
 * @param global the global context
 */
mbedtls_entropy_context *sys_mbedtls_get_entropy_context_lock(GlobalContext *global);

/**
 * @brief release lock on mbedtls_entropy_context.
 *
 * @param global the global context
 */
void sys_mbedtls_entropy_context_unlock(GlobalContext *global);

/**
 * @brief invoke `mbedtls_entropy_func`
 * @details unless MBEDTLS_THREADING_C is defined, this function must acquire
 * the entropy mutex to call `mbedtls_entropy_func`.
 */
int sys_mbedtls_entropy_func(void *entropy, unsigned char *buf, size_t size);

/**
 * @brief get and acquire lock on mbedtls_ctr_drbg_context.
 * @details this function must be called from a scheduler thread (nif,
 * native handler, listener) to access the random context and should be
 * balanced by `sys_mbedtls_ctr_drbg_context_unlock`.
 * On non-SMP builds, there is no lock as there is only one scheduler.
 *
 * @warning do not call this function when already owning the entropy context
 */
mbedtls_ctr_drbg_context *sys_mbedtls_get_ctr_drbg_context_lock(GlobalContext *global);

/**
 * @brief release lock on mbedtls_ctr_drbg_context.
 *
 * @param global the global context
 */
void sys_mbedtls_ctr_drbg_context_unlock(GlobalContext *global);

#ifdef __cplusplus
}
#endif

#endif
