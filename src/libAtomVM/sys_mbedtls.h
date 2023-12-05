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

mbedtls_entropy_context *sys_mbedtls_get_entropy_context_lock(GlobalContext *global);
void sys_mbedtls_entropy_context_unlock(GlobalContext *global);
int sys_mbedtls_entropy_func(void *entropy, unsigned char *buf, size_t size);

mbedtls_ctr_drbg_context *sys_mbedtls_get_ctr_drbg_context_lock(GlobalContext *global);

/**
 * @warning do not call this function when already owning the entropy context
 */
void sys_mbedtls_ctr_drbg_context_unlock(GlobalContext *global);

#endif
