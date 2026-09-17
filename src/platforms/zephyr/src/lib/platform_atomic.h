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

#ifndef _PLATFORM_ATOMIC_H
#define _PLATFORM_ATOMIC_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define ATOMIC_COMPARE_EXCHANGE_WEAK_PTR(object, expected, desired) \
    platform_atomic_compare_exchange_weak_ptr((void **) (object), (void **) (expected), (void *) (desired))

#define ATOMIC_COMPARE_EXCHANGE_WEAK_INT(object, expected, desired) \
    smp_atomic_compare_exchange_weak_int((void *) (object), (void *) (expected), (uint64_t) (desired), sizeof(desired))

bool platform_atomic_compare_exchange_weak_ptr(void **object, void **expected, void *desired);
bool smp_atomic_compare_exchange_weak_int(void *object, void *expected, uint64_t desired, size_t desired_len);
size_t smp_atomic_fetch_add_size(size_t *object, size_t delta);
size_t smp_atomic_fetch_sub_size(size_t *object, size_t delta);
size_t smp_atomic_fetch_or_size(size_t *object, size_t mask);

#endif
