/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

#ifndef _RTEMS_SYS_H_
#define _RTEMS_SYS_H_

#include <interop.h>
#include <portnifloader.h>
#include <smp.h>
#include <sys.h>

#include <rtems.h>

#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
#include <poll.h>
#endif

#if defined(HAVE_MBEDTLS)
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>
#endif

#ifndef AVM_NO_SMP
#include <pthread.h>
#endif

#define RTEMS_ATOM globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "rtems"))

struct RTEMSPlatformData
{
#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
    struct pollfd *fds;
    int ATOMIC select_events_poll_count;
#endif
#ifndef AVM_NO_SMP
    pthread_mutex_t poll_mutex;
    pthread_cond_t poll_cv;
#else
    rtems_id poll_task;
#endif
    bool ATOMIC signaled;

#if defined(HAVE_MBEDTLS)
#ifndef AVM_NO_SMP
    Mutex *entropy_mutex;
    Mutex *random_mutex;
#endif
    mbedtls_entropy_context entropy_ctx;
    bool entropy_is_initialized;
    mbedtls_ctr_drbg_context random_ctx;
    bool random_is_initialized;
#endif
};

void gpio_interrupt_drain(GlobalContext *glb);

#endif /* _RTEMS_SYS_H_ */
