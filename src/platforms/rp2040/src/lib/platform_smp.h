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

#include <pico/critical_section.h>
#include <pico/mutex.h>

#pragma GCC diagnostic pop

#define SMP_PLATFORM_SPINLOCK
#define ATOMIC

#define ATOMIC_COMPARE_EXCHANGE_WEAK(object, expected, desired) \
    smp_atomic_compare_exchange_weak(object, expected, (uint64_t) desired, sizeof(desired))

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
 * @brief Unlock a spinlock.
 * @param lock the spin lock to unlock
 */
static inline void smp_spinlock_unlock(SpinLock *lock)
{
    mutex_exit(&lock->mutex);
}

static critical_section_t atomic_cas_section;

/**
 * @brief Initialize structures of SMP functions
 */
static inline void smp_init()
{
    critical_section_init(&atomic_cas_section);
}

/**
 * @brief Free structures for SMP functions
 */
static inline void smp_free()
{
    critical_section_deinit(&atomic_cas_section);
}

static inline bool smp_atomic_compare_exchange_weak(void *object, void *expected, uint64_t desired, size_t desired_len)
{
    critical_section_enter_blocking(&atomic_cas_section);

    bool result;
    switch (desired_len) {
        case sizeof(uint64_t): {
            uint64_t *object_ptr = (uint64_t *) object;
            uint64_t *expected_ptr = (uint64_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
        case sizeof(uint32_t): {
            uint32_t *object_ptr = (uint32_t *) object;
            uint32_t *expected_ptr = (uint32_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
        case sizeof(uint16_t): {
            uint16_t *object_ptr = (uint16_t *) object;
            uint16_t *expected_ptr = (uint16_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
        case sizeof(uint8_t): {
            uint8_t *object_ptr = (uint8_t *) object;
            uint8_t *expected_ptr = (uint8_t *) expected;
            result = *object_ptr == *expected_ptr;
            if (result) {
                *object_ptr = desired;
            } else {
                *expected_ptr = *object_ptr;
            }
            break;
        }
    }

    critical_section_exit(&atomic_cas_section);
    return result;
}

#endif
