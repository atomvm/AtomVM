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

#include "atom_table.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "atom.h"
#include "smp.h"
#include "utils.h"

#ifndef AVM_NO_SMP
#define SMP_RDLOCK(htable) smp_rwlock_rdlock(htable->lock)
#define SMP_WRLOCK(htable) smp_rwlock_wrlock(htable->lock)
#define SMP_UNLOCK(htable) smp_rwlock_unlock(htable->lock)
#else
#define SMP_RDLOCK(htable)
#define SMP_WRLOCK(htable)
#define SMP_UNLOCK(htable)
#endif

#define DEFAULT_SIZE 8

struct HNode
{
    struct HNode *next;
    AtomString key;
    long index;
};

struct HNodeGroup;

struct AtomTable
{
    int capacity;
    int count;
#ifndef AVM_NO_SMP
    RWLock *lock;
#endif
    struct HNode **buckets;

    struct HNodeGroup *first_node_group;
    struct HNodeGroup *last_node_group;
};

struct HNodeGroup
{
    struct HNodeGroup *next;
    long first_index;
    uint16_t len;
    uint16_t avail;

    struct HNode nodes[];
};

static struct HNodeGroup *new_node_group(struct AtomTable *table, int len);

struct AtomTable *atom_table_new()
{
    struct AtomTable *htable = malloc(sizeof(struct AtomTable));
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

    htable->last_node_group = NULL;
    htable->first_node_group = new_node_group(htable, DEFAULT_SIZE);

#ifndef AVM_NO_SMP
    htable->lock = smp_rwlock_create();
#endif

    return htable;
}

void atom_table_destroy(struct AtomTable *table)
{
    struct HNodeGroup *node_group = table->first_node_group;
    while (node_group) {
        struct HNodeGroup *next_group = node_group->next;
        free(node_group);
        node_group = next_group;
    }
#ifndef AVM_NO_SMP
    smp_rwlock_destroy(table->lock);
#endif
    free(table->buckets);
    free(table);
}

int atom_table_count(struct AtomTable *table)
{
    SMP_RDLOCK(table);
    int count = table->count;
    SMP_UNLOCK(table);

    return count;
}

static struct HNodeGroup *new_node_group(struct AtomTable *table, int len)
{
    struct HNodeGroup *new_group = malloc(sizeof(struct HNodeGroup) + sizeof(struct HNode) * len);
    new_group->next = NULL;
    new_group->first_index = table->count;
    new_group->len = len;
    new_group->avail = len;

    if (LIKELY(table->last_node_group != NULL)) {
        table->last_node_group->next = new_group;
    }
    table->last_node_group = new_group;

    return new_group;
}

static unsigned long sdbm_hash(const unsigned char *str, int len)
{
    unsigned long hash = 0;
    int c;

    for (int i = 0; i < len; i++) {
        c = *str++;
        hash = c + (hash << 6) + (hash << 16) - hash;
    }

    return hash;
}

static inline struct HNode *get_node_from_bucket(
    const struct AtomTable *hash_table, unsigned long bucket_index, AtomString string)
{
    struct HNode *node = hash_table->buckets[bucket_index];
    while (node) {
        if (atom_are_equals(string, node->key)) {
            return node;
        }

        node = node->next;
    }

    return NULL;
}

static inline struct HNode *get_node(const struct AtomTable *hash_table, AtomString string)
{
    unsigned long hash = sdbm_hash(string, atom_string_len(string));

    unsigned long bucket_index = hash % hash_table->capacity;
    return get_node_from_bucket(hash_table, bucket_index, string);
}

static inline struct HNode *lock_and_get_node(struct AtomTable *hash_table, AtomString string)
{
    unsigned long hash = sdbm_hash(string, atom_string_len(string));

    SMP_RDLOCK(hash_table);
    unsigned long bucket_index = hash % hash_table->capacity;
    return get_node_from_bucket(hash_table, bucket_index, string);
}

long atom_table_get_index(struct AtomTable *table, AtomString string)
{
    struct HNode *node = lock_and_get_node(table, string);
    long result = (node != NULL) ? node->index : ATOM_TABLE_NOT_FOUND;

    SMP_UNLOCK(table);
    return result;
}

// TODO: this function needs to be optimized
AtomString atom_table_get_atom_string(struct AtomTable *table, long index)
{
    SMP_RDLOCK(table);

    struct HNodeGroup *node_group = table->first_node_group;
    while (node_group) {
        long first_index = node_group->first_index;
        if (first_index + node_group->len > index) {
            if (index - first_index > (node_group->len - node_group->avail)) {
                SMP_UNLOCK(table);
                return NULL;
            }

            SMP_UNLOCK(table);
            return node_group->nodes[index - first_index].key;
        }

        node_group = node_group->next;
    }

    SMP_UNLOCK(table);
    return NULL;
}

static inline void init_node(struct HNode *node, AtomString atom, long index)
{
    node->key = atom;
    node->index = index;
}

static inline void insert_node_into_bucket(
    struct AtomTable *table, int bucket_index, struct HNode *node)
{
    struct HNode *maybe_existing_node = table->buckets[bucket_index];
    table->buckets[bucket_index] = node;
    node->next = maybe_existing_node;
}

static inline long insert_node(struct AtomTable *table, struct HNodeGroup *node_group,
    unsigned long bucket_index, AtomString string)
{
    long new_index = table->count;
    table->count++;

    struct HNode *node = &node_group->nodes[new_index - node_group->first_index];
    node_group->avail--;
    init_node(node, string, new_index);
    insert_node_into_bucket(table, bucket_index, node);

    return new_index;
}

long atom_table_ensure_atom(struct AtomTable *table, AtomString string)
{
    unsigned long hash = sdbm_hash(string, atom_string_len(string));
    SMP_WRLOCK(table);
    unsigned long bucket_index = hash % table->capacity;

    struct HNode *node = get_node_from_bucket(table, bucket_index, string);
    if (node) {
        SMP_UNLOCK(table);
        return node->index;
    }

    struct HNodeGroup *node_group = table->last_node_group;
    if (!node_group->avail) {
        node_group = new_node_group(table, DEFAULT_SIZE);
    }

    long new_index = insert_node(table, node_group, bucket_index, string);

    SMP_UNLOCK(table);
    return new_index;
}

void atom_table_ensure_atoms(
    struct AtomTable *table, const void *atoms, int count, int *translate_table)
{
    SMP_WRLOCK(table);

    int new_atoms_count = 0;

    const uint8_t *current_atom = atoms;

    for (int i = 0; i < count; i++) {
        struct HNode *node = get_node(table, current_atom);
        if (node) {
            translate_table[i] = node->index;
        } else {
            new_atoms_count++;
            translate_table[i] = ATOM_TABLE_NOT_FOUND;
        }

        uint8_t atom_len = current_atom[0];
        current_atom += 1 + atom_len;
    }

    current_atom = atoms;
    int remaining_atoms = new_atoms_count;
    struct HNodeGroup *node_group = table->last_node_group;
    for (int i = 0; i < count; i++) {
        if (translate_table[i] == ATOM_TABLE_NOT_FOUND) {
            if (!node_group->avail) {
                node_group = new_node_group(table, remaining_atoms);
            }

            unsigned long hash = sdbm_hash(current_atom, atom_string_len(current_atom));
            unsigned long bucket_index = hash % table->capacity;

            translate_table[i] = insert_node(table, node_group, bucket_index, current_atom);
            remaining_atoms--;
            if (remaining_atoms == 0) {
                break;
            }
        }
        uint8_t atom_len = current_atom[0];
        current_atom += 1 + atom_len;
    }

    SMP_UNLOCK(table);
}
