/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

#include <limits.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <pico/mutex.h>

#pragma GCC diagnostic pop

#define SMP_PLATFORM_SPINLOCK

#ifndef TYPEDEF_SPINLOCK
#define TYPEDEF_SPINLOCK
typedef struct SpinLock SpinLock;
#endif

struct SpinLock
{
    mutex_t mutex;
};

// What AtomVM calls spinlocks are lightweight busy locks based on atomics.
// This really is what Pico SDK provides as mutexes.

/**
 * @brief Initialize a spinlock based on atomics.
 * @param lock the spin lock to initialize
 */
static inline void smp_spinlock_init(SpinLock *lock)
{
    mutex_init(&lock->mutex);
}

/**
 * @brief Lock a spinlock.
 * @param lock the spin lock to lock
 */
static inline void smp_spinlock_lock(SpinLock *lock)
{
    mutex_enter_blocking(&lock->mutex);
}

/**
 * @brief Try to lock a spinlock.
 * @param lock the spin lock to lock
 * @return true if the lock was acquired.
 */
static inline bool smp_spinlock_trylock(SpinLock *lock)
{
    return mutex_try_enter(&lock->mutex, NULL);
}

/**
 * @brief Unlock a spinlock.
 * @param lock the spin lock to unlock
 */
static inline void smp_spinlock_unlock(SpinLock *lock)
{
    mutex_exit(&lock->mutex);
}

#endif
