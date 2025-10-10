/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
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

#ifndef _VALUESHASHTABLE_H_
#define _VALUESHASHTABLE_H_

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define TO_VALUESHASHTABLE_VALUE(value) ((uintptr_t) (value))

#ifndef AVM_NO_SMP
#ifndef TYPEDEF_RWLOCK
#define TYPEDEF_RWLOCK
typedef struct RWLock RWLock;
#endif
#endif

struct ValuesHashTable
{
    size_t capacity;
    size_t count;
#ifndef AVM_NO_SMP
    RWLock *lock;
#endif
    struct HNode **buckets;
};

struct ValuesHashTable *valueshashtable_new(void);
int valueshashtable_insert(struct ValuesHashTable *hash_table, uintptr_t key, uintptr_t value);
uintptr_t valueshashtable_get_value(const struct ValuesHashTable *hash_table, uintptr_t key, uintptr_t default_value);
int valueshashtable_has_key(const struct ValuesHashTable *hash_table, uintptr_t key);

#ifdef __cplusplus
}
#endif

#endif
