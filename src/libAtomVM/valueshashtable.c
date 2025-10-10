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

#include "valueshashtable.h"

#include "utils.h"

#include <stdlib.h>

#ifndef AVM_NO_SMP
#include "smp.h"
#define SMP_RDLOCK(htable) smp_rwlock_rdlock(htable->lock)
#define SMP_WRLOCK(htable) smp_rwlock_wrlock(htable->lock)
#define SMP_UNLOCK(htable) smp_rwlock_unlock(htable->lock)
#else
#define SMP_RDLOCK(htable) UNUSED(htable)
#define SMP_WRLOCK(htable) UNUSED(htable)
#define SMP_UNLOCK(htable) UNUSED(htable)
#endif

#define DEFAULT_SIZE 8

struct HNode
{
    struct HNode *next;
    uintptr_t key;
    uintptr_t value;
};

struct ValuesHashTable *valueshashtable_new(void)
{
    struct ValuesHashTable *htable = malloc(sizeof(struct ValuesHashTable));
    if (IS_NULL_PTR(htable)) {
        return NULL;
    }
    htable->buckets = calloc(DEFAULT_SIZE, sizeof(struct HNode *));
    if (IS_NULL_PTR(htable->buckets)) {
        free(htable);
        return NULL;
    }

    htable->count = 0;
    htable->capacity = DEFAULT_SIZE;
#ifndef AVM_NO_SMP
    htable->lock = smp_rwlock_create();
#endif

    return htable;
}

int valueshashtable_insert(struct ValuesHashTable *hash_table, uintptr_t key, uintptr_t value)
{
    SMP_WRLOCK(hash_table);
    size_t index = key % hash_table->capacity;

    struct HNode *node = hash_table->buckets[index];
    if (node) {
        while (1) {
            if (node->key == key) {
                node->value = value;
                SMP_UNLOCK(hash_table);
                return 1;
            }

            if (node->next) {
                node = node->next;
            } else {
                break;
            }
        }
    }

    struct HNode *new_node = malloc(sizeof(struct HNode));
    if (IS_NULL_PTR(new_node)) {
        SMP_UNLOCK(hash_table);
        return 0;
    }
    new_node->next = NULL;
    new_node->key = key;
    new_node->value = value;

    if (node) {
        node->next = new_node;
    } else {
        hash_table->buckets[index] = new_node;
    }

    hash_table->count++;
    SMP_UNLOCK(hash_table);
    return 1;
}

uintptr_t valueshashtable_get_value(const struct ValuesHashTable *hash_table, uintptr_t key, uintptr_t default_value)
{
    SMP_RDLOCK(hash_table);
    size_t index = key % hash_table->capacity;

    const struct HNode *node = hash_table->buckets[index];
    while (node) {
        if (node->key == key) {
            uintptr_t result = node->value;
            SMP_UNLOCK(hash_table);
            return result;
        }

        node = node->next;
    }

    SMP_UNLOCK(hash_table);
    return default_value;
}

int valueshashtable_has_key(const struct ValuesHashTable *hash_table, uintptr_t key)
{
    SMP_RDLOCK(hash_table);
    size_t index = key % hash_table->capacity;

    const struct HNode *node = hash_table->buckets[index];
    while (node) {
        if (node->key == key) {
            SMP_UNLOCK(hash_table);
            return 1;
        }

        node = node->next;
    }

    SMP_UNLOCK(hash_table);
    return 0;
}
