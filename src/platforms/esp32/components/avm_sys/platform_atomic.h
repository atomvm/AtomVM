/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

#ifndef _PLATFORM_ATOMIC_H
#define _PLATFORM_ATOMIC_H

#include <stdatomic.h>

#include "freertos/FreeRTOS.h"
#include "freertos/task.h"

#if ATOMIC_POINTER_LOCK_FREE != 2

// If we have SMP, we shouldn't need to define atomic cas operations
#if !defined(AVM_NO_SMP)
#error atomic cas emulation code is only meant for ISR support without SMP
#endif

#define ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(object, expected, desired) \
    smp_atomic_compare_exchange_weak_ptr((void **) object, (void **) expected, (void *) desired)

static portMUX_TYPE smp_atomic_spinlock = portMUX_INITIALIZER_UNLOCKED;

// This primitive may be called from both thread and isr
static inline bool smp_atomic_compare_exchange_weak_ptr(void **object, void **expected, void *desired)
{
    bool in_isr = xPortInIsrContext();
    if (in_isr) {
        // taskENTER_CRITICAL_FROM_ISR is broken in some versions of esp-idf we support
        taskENTER_CRITICAL_ISR(&smp_atomic_spinlock);
    } else {
        taskENTER_CRITICAL(&smp_atomic_spinlock);
    }

    bool result;
    result = *object == *expected;
    if (result) {
        *object = desired;
    } else {
        *expected = *object;
    }

    if (in_isr) {
        taskEXIT_CRITICAL_ISR(&smp_atomic_spinlock);
    } else {
        taskEXIT_CRITICAL(&smp_atomic_spinlock);
    }

    return result;
}

#else

#define ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(object, expected, desired) \
    atomic_compare_exchange_weak(object, expected, desired)

#endif

#endif
