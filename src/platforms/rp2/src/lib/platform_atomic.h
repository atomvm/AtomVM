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

#include <stdbool.h>
#include <stdint.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <pico/critical_section.h>

#pragma GCC diagnostic pop

#define ATOMIC

#define ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(object, expected, desired) \
    smp_atomic_compare_exchange_weak_ptr((void **) object, (void **) expected, (void *) desired)

#define ATOMIC_COMPARE_EXCHANGE_WEAK_INT(object, expected, desired) \
    smp_atomic_compare_exchange_weak_int((void *) object, (void *) expected, (uint64_t) desired, sizeof(desired))

#ifndef AVM_NO_SMP
static critical_section_t atomic_cas_section;
#endif

/**
 * @brief Initialize structures of atomic functions
 */
static inline void atomic_init()
{
#ifndef AVM_NO_SMP
    critical_section_init(&atomic_cas_section);
#endif
}

/**
 * @brief Free structures for atomic functions
 */
static inline void atomic_free()
{
#ifndef AVM_NO_SMP
    critical_section_deinit(&atomic_cas_section);
#endif
}

static inline bool smp_atomic_compare_exchange_weak_ptr(void **object, void **expected, void *desired)
{
#ifndef AVM_NO_SMP
    critical_section_enter_blocking(&atomic_cas_section);
#else
    uint32_t save = save_and_disable_interrupts();
#endif

    bool result;
    result = *object == *expected;
    if (result) {
        *object = desired;
    } else {
        *expected = *object;
    }
#ifndef AVM_NO_SMP
    critical_section_exit(&atomic_cas_section);
#else
    restore_interrupts(save);
#endif
    return result;
}

static inline bool smp_atomic_compare_exchange_weak_int(void *object, void *expected, uint64_t desired, size_t desired_len)
{
#ifndef AVM_NO_SMP
    critical_section_enter_blocking(&atomic_cas_section);
#else
    uint32_t save = save_and_disable_interrupts();
#endif

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

#ifndef AVM_NO_SMP
    critical_section_exit(&atomic_cas_section);
#else
    restore_interrupts(save);
#endif
    return result;
}

#endif
