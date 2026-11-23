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

#ifndef _PLATFORM_SMP_H
#define _PLATFORM_SMP_H

#include <stdbool.h>

#include "platform_atomic.h"

#define SMP_PLATFORM_SPINLOCK

#ifndef TYPEDEF_SPINLOCK
#define TYPEDEF_SPINLOCK
typedef struct SpinLock SpinLock;
#endif

struct SpinLock
{
    int lock;
};

static inline void smp_spinlock_init(SpinLock *lock)
{
    lock->lock = 0;
}

static inline void smp_spinlock_lock(SpinLock *lock)
{
    int current;
    while (true) {
        current = 0;
        if (ATOMIC_COMPARE_EXCHANGE_WEAK_INT(&lock->lock, &current, 1)) {
            return;
        }
    }
}

static inline bool smp_spinlock_trylock(SpinLock *lock)
{
    int current = 0;
    return ATOMIC_COMPARE_EXCHANGE_WEAK_INT(&lock->lock, &current, 1);
}

static inline void smp_spinlock_unlock(SpinLock *lock)
{
    lock->lock = 0;
}

#endif
