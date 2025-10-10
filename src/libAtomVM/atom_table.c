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

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "atom.h"
#include "smp.h"
#include "unicode.h"
#include "utils.h"

#ifndef AVM_NO_SMP
#define SMP_RDLOCK(htable) smp_rwlock_rdlock(htable->lock)
#define SMP_WRLOCK(htable) smp_rwlock_wrlock(htable->lock)
#define SMP_UNLOCK(htable) smp_rwlock_unlock(htable->lock)
#else
#define SMP_RDLOCK(htable) UNUSED(htable)
#define SMP_WRLOCK(htable) UNUSED(htable)
#define SMP_UNLOCK(htable) UNUSED(htable)
#endif

#define DEFAULT_SIZE 8
#define CAPACITY_INCREASE 8
#define MAX_ATOM_LEN ((1 << 12) - 1)

#define ATOM_TABLE_THRESHOLD(capacity) (capacity + (capacity >> 2))
#define ATOM_TABLE_NEW_CAPACITY(new_count) (new_count + CAPACITY_INCREASE)

struct HNode
{
    struct HNode *next;
    const uint8_t *key;
    uint32_t index : 20;
    uint32_t bytes_len : 10;
};

struct HNodeGroup
{
    struct HNodeGroup *next;
    atom_index_t first_index;
    uint16_t len;

    struct HNode nodes[];
};

struct AtomTable
{
    size_t capacity;
    size_t count;
    int last_node_group_avail;
#ifndef AVM_NO_SMP
    RWLock *lock;
#endif
    struct HNode **buckets;

    struct HNodeGroup *first_node_group;
    struct HNodeGroup *last_node_group;
};

static struct HNodeGroup *new_node_group(struct AtomTable *table, int len);

struct AtomTable *atom_table_new(void)
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

size_t atom_table_count(struct AtomTable *table)
{
    SMP_RDLOCK(table);
    size_t count = table->count;
    SMP_UNLOCK(table);

    return count;
}

static struct HNodeGroup *new_node_group(struct AtomTable *table, int len)
{
    struct HNodeGroup *new_group = malloc(sizeof(struct HNodeGroup) + sizeof(struct HNode) * len);
    if (IS_NULL_PTR(new_group)) {
        return NULL;
    }
    new_group->next = NULL;
    new_group->first_index = table->count;
    new_group->len = len;

    if (LIKELY(table->last_node_group != NULL)) {
        table->last_node_group->next = new_group;
    }
    table->last_node_group = new_group;
    table->last_node_group_avail = len;

    return new_group;
}

static unsigned long sdbm_hash(const unsigned char *str, int len)
{
    unsigned long hash = len;
    int c;

    for (int i = 0; i < len; i++) {
        c = *str++;
        hash = c + (hash << 6) + (hash << 16) - hash;
    }

    return hash;
}

static inline struct HNode *get_node_from_bucket(
    const struct AtomTable *hash_table, unsigned long bucket_index, const uint8_t *string, size_t string_len)
{
    struct HNode *node = hash_table->buckets[bucket_index];
    while (node) {
        if (node->bytes_len == string_len && memcmp(node->key, string, string_len) == 0) {
            return node;
        }

        node = node->next;
    }

    return NULL;
}

static inline struct HNode *get_node_with_hash(
    const struct AtomTable *hash_table, const uint8_t *string, size_t string_len, unsigned long hash)
{
    unsigned long bucket_index = hash % hash_table->capacity;
    return get_node_from_bucket(hash_table, bucket_index, string, string_len);
}

static inline struct HNode *get_node(const struct AtomTable *hash_table, const uint8_t *string, size_t string_len)
{
    unsigned long hash = sdbm_hash(string, string_len);

    return get_node_with_hash(hash_table, string, string_len, hash);
}

// TODO: this function needs use an efficient structure such as a skip list
static struct HNode *get_node_using_index(struct AtomTable *table, atom_index_t index)
{
    if (UNLIKELY(((size_t) index) >= table->count)) {
        return NULL;
    }

    struct HNodeGroup *node_group = table->first_node_group;
    while (node_group) {
        atom_index_t first_index = node_group->first_index;
        if (first_index + node_group->len > index) {
            return &node_group->nodes[index - first_index];
        }

        node_group = node_group->next;
    }

    return NULL;
}

const uint8_t *atom_table_get_atom_string(struct AtomTable *table, atom_index_t index, size_t *out_size)
{
    const uint8_t *result;
    SMP_RDLOCK(table);

    struct HNode *node = get_node_using_index(table, index);
    if (IS_NULL_PTR(node)) {
        SMP_UNLOCK(table);
        return NULL;
    }
    result = node->key;
    *out_size = node->bytes_len;

    SMP_UNLOCK(table);
    return result;
}

bool atom_table_is_equal_to_atom_string(struct AtomTable *table, atom_index_t t_atom_index, AtomString string)
{
    size_t t_atom_len;
    const uint8_t *t_atom_data = atom_table_get_atom_string(table, t_atom_index, &t_atom_len);
    if (IS_NULL_PTR(t_atom_data)) {
        return false;
    }

    return (t_atom_len == atom_string_len(string)) && (memcmp(t_atom_data, atom_string_data(string), t_atom_len) == 0);
}

int atom_table_cmp_using_atom_index(struct AtomTable *table, atom_index_t t_atom_index, atom_index_t other_atom_index)
{
    size_t t_atom_len;
    const uint8_t *t_atom_data = atom_table_get_atom_string(table, t_atom_index, &t_atom_len);
    if (IS_NULL_PTR(t_atom_data)) {
        return -1;
    }

    size_t other_atom_len;
    const uint8_t *other_atom_data = atom_table_get_atom_string(table, other_atom_index, &other_atom_len);
    if (IS_NULL_PTR(other_atom_data)) {
        return 1;
    }

    int cmp_size = (t_atom_len > other_atom_len) ? other_atom_len : t_atom_len;

    int memcmp_result = memcmp(t_atom_data, other_atom_data, cmp_size);

    if (memcmp_result == 0) {
        if (t_atom_len == other_atom_len) {
            return 0;
        } else {
            return (t_atom_len > other_atom_len) ? 1 : -1;
        }
    }

    return memcmp_result;
}

atom_ref_t atom_table_get_atom_ptr_and_len(struct AtomTable *table, atom_index_t index, size_t *out_len)
{
    SMP_RDLOCK(table);

    struct HNode *node = get_node_using_index(table, index);
    if (IS_NULL_PTR(node)) {
        SMP_RDLOCK(table);
        return NULL;
    }

    *out_len = atom_string_len(node->key);

    SMP_UNLOCK(table);
    return node;
}

static inline void init_node(struct HNode *node, const uint8_t *atom_data, size_t atom_len, long index)
{
    node->key = atom_data;
    node->bytes_len = atom_len;
    node->index = index;
}

static inline void insert_node_into_bucket(
    struct AtomTable *table, int bucket_index, struct HNode *node)
{
    struct HNode *maybe_existing_node = table->buckets[bucket_index];
    table->buckets[bucket_index] = node;
    node->next = maybe_existing_node;
}

static inline atom_index_t insert_node(struct AtomTable *table, struct HNodeGroup *node_group,
    unsigned long bucket_index, const uint8_t *atom_data, size_t atom_len)
{
    atom_index_t new_index = table->count;
    table->count++;

    struct HNode *node = &node_group->nodes[new_index - node_group->first_index];
    table->last_node_group_avail--;
    init_node(node, atom_data, atom_len, new_index);
    insert_node_into_bucket(table, bucket_index, node);

    return new_index;
}

static bool do_rehash(struct AtomTable *table, int new_capacity)
{
    int new_size_bytes = sizeof(struct HNode *) * new_capacity;
    struct HNode **new_buckets = realloc(table->buckets, new_size_bytes);
    if (IS_NULL_PTR(new_buckets)) {
        // Allocation failure can be ignored, the hash table will continue with the previous bucket
        return false;
    }
    memset(new_buckets, 0, new_size_bytes);
    table->buckets = new_buckets;
    table->capacity = new_capacity;

    struct HNodeGroup *group = table->first_node_group;

    while (group) {
        int group_count;
        if (group == table->last_node_group) {
            group_count = group->len - table->last_node_group_avail;
        } else {
            group_count = group->len;
        }

        for (int i = 0; i < group_count; i++) {
            struct HNode *node = &group->nodes[i];
            unsigned long hash = sdbm_hash(node->key, node->bytes_len);
            unsigned long bucket_index = hash % table->capacity;

            insert_node_into_bucket(table, bucket_index, node);
        }

        group = group->next;
    }

    return true;
}

static inline bool maybe_rehash(struct AtomTable *table, int new_entries)
{
    int new_count = table->count + new_entries;
    int threshold = ATOM_TABLE_THRESHOLD(table->capacity);
    if (new_count > threshold) {
        return false;
    }

    int new_capacity = ATOM_TABLE_NEW_CAPACITY(new_count);
    return do_rehash(table, new_capacity);
}

enum AtomTableEnsureAtomResult atom_table_ensure_atom(struct AtomTable *table, const uint8_t *atom_data, size_t atom_len, enum AtomTableCopyOpt opts, atom_index_t *result)
{
    unsigned long hash = sdbm_hash(atom_data, atom_len);
    SMP_WRLOCK(table);
    unsigned long bucket_index = hash % table->capacity;

    struct HNode *node = get_node_from_bucket(table, bucket_index, atom_data, atom_len);
    if (node) {
        SMP_UNLOCK(table);
        *result = node->index;
        return AtomTableEnsureAtomOk;
    }
    if (opts & AtomTableAlreadyExisting) {
        SMP_UNLOCK(table);
        return AtomTableEnsureAtomNotFound;
    }

    struct HNodeGroup *node_group = table->last_node_group;
    if (!table->last_node_group_avail) {
        node_group = new_node_group(table, DEFAULT_SIZE);
        if (IS_NULL_PTR(node_group)) {
            SMP_UNLOCK(table);
            return AtomTableEnsureAtomAllocFail;
        }
    }

    if (opts & AtomTableCopyAtom) {
        uint8_t *buf = malloc(atom_len);
        if (atom_len > 0) {
            if (IS_NULL_PTR(buf)) {
                SMP_UNLOCK(table);
                return AtomTableEnsureAtomAllocFail;
            }
            memcpy(buf, atom_data, atom_len);
        }
        atom_data = buf;
    }

    if (maybe_rehash(table, 1)) {
        bucket_index = hash % table->capacity;
    }

    *result = insert_node(table, node_group, bucket_index, atom_data, atom_len);

    SMP_UNLOCK(table);
    return AtomTableEnsureAtomOk;
}

static inline int read_encoded_len(const uint8_t **len_bytes)
{
    uint8_t byte0 = (*len_bytes)[0];

    if ((byte0 & 0x8) == 0) {
        (*len_bytes)++;
        return byte0 >> 4;

    } else if ((byte0 & 0x10) == 0) {
        uint8_t byte1 = (*len_bytes)[1];
        (*len_bytes) += 2;
        return ((byte0 >> 5) << 8) | byte1;

    } else {
        return -1;
    }
}

// -1 is not a valid atom index as we're limited to 2^20
#define ATOM_TABLE_NOT_FOUND_MARKER ((atom_index_t) -1)

enum AtomTableEnsureAtomResult atom_table_ensure_atoms(struct AtomTable *table, const void *atoms, size_t count,
    atom_index_t *translate_table, enum EnsureAtomsOpt opt)
{
    bool is_long_format = (opt & EnsureLongEncoding) != 0;

    SMP_WRLOCK(table);

    int new_atoms_count = 0;

    const uint8_t *current_atom = atoms;

    for (size_t i = 0; i < count; i++) {
        struct HNode *node;
        int atom_len;
        if (is_long_format) {
            atom_len = read_encoded_len(&current_atom);
            if (UNLIKELY(atom_len < 0 || atom_len > MAX_ATOM_LEN)) {
                fprintf(stderr, "Found invalid atom len.");
                SMP_UNLOCK(table);
                return AtomTableEnsureAtomInvalidLen;
            }
        } else {
            atom_len = current_atom[0];
            current_atom++;
        }
        node = get_node(table, current_atom, atom_len);
        current_atom += atom_len;

        if (node) {
            translate_table[i] = node->index;
        } else {
            new_atoms_count++;
            translate_table[i] = ATOM_TABLE_NOT_FOUND_MARKER;
        }
    }

    maybe_rehash(table, new_atoms_count);

    current_atom = atoms;
    int remaining_atoms = new_atoms_count;
    struct HNodeGroup *node_group = table->last_node_group;
    for (size_t i = 0; i < count; i++) {
        size_t atom_len;
        if (is_long_format) {
            // Size was checked above
            atom_len = (size_t) read_encoded_len(&current_atom);
        } else {
            atom_len = current_atom[0];
            current_atom++;
        }

        if (translate_table[i] == ATOM_TABLE_NOT_FOUND_MARKER) {
            if (!table->last_node_group_avail) {
                node_group = new_node_group(table, remaining_atoms);
                if (IS_NULL_PTR(node_group)) {
                    SMP_UNLOCK(table);
                    return AtomTableEnsureAtomAllocFail;
                }
            }

            unsigned long hash = sdbm_hash(current_atom, atom_len);
            unsigned long bucket_index = hash % table->capacity;

            translate_table[i] = insert_node(table, node_group, bucket_index, current_atom, atom_len);
            remaining_atoms--;
            if (remaining_atoms == 0) {
                break;
            }
        }
        current_atom += atom_len;
    }

    SMP_UNLOCK(table);

    return AtomTableEnsureAtomOk;
}
