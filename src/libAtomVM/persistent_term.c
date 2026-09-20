/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

#include "persistent_term.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include "context.h"
#include "globalcontext.h"
#include "memory.h"
#include "smp.h"
#include "term.h"
#include "term_hash.h"
#include "utils.h"

struct PersistentTermEntry
{
    struct PersistentTermEntry *next;
    term key;
    term value;
    Heap *heap;
    size_t memory;
};

static persistent_term_result_t find_entry(
    PersistentTerm *persistent_term,
    uint32_t bucket_index,
    term key,
    struct PersistentTermEntry **out_entry,
    GlobalContext *global);
static struct PersistentTermEntry *entry_new(term key, term value);
static void entry_destroy(struct PersistentTermEntry *entry, GlobalContext *global);
static bool term_is_equal(term a, term b, GlobalContext *global, persistent_term_result_t *result);

void persistent_term_init(PersistentTerm *persistent_term)
{
    persistent_term->count = 0;
    persistent_term->memory = 0;
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        persistent_term->buckets[i] = NULL;
    }

#ifndef AVM_NO_SMP
    persistent_term->lock = smp_rwlock_create();
#endif
}

void persistent_term_destroy(PersistentTerm *persistent_term, GlobalContext *global)
{
    SMP_RWLOCK_WRLOCK(persistent_term->lock);
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        struct PersistentTermEntry *entry = persistent_term->buckets[i];
        while (entry != NULL) {
            struct PersistentTermEntry *next = entry->next;
            entry_destroy(entry, global);
            entry = next;
        }
        persistent_term->buckets[i] = NULL;
    }

    persistent_term->count = 0;
    persistent_term->memory = 0;
    SMP_RWLOCK_UNLOCK(persistent_term->lock);
#ifndef AVM_NO_SMP
    smp_rwlock_destroy(persistent_term->lock);
    persistent_term->lock = NULL;
#endif
}

persistent_term_result_t persistent_term_put(
    PersistentTerm *persistent_term,
    term key,
    term value,
    GlobalContext *global)
{
    uint32_t bucket_index = term_hash(key, global) % PERSISTENT_TERM_NUM_BUCKETS;

    SMP_RWLOCK_WRLOCK(persistent_term->lock);

    struct PersistentTermEntry *entry;
    persistent_term_result_t result = find_entry(persistent_term, bucket_index, key, &entry, global);
    if (UNLIKELY(result != PersistentTermOk)) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return result;
    }

    if (entry != NULL) {
        bool equal = term_is_equal(entry->value, value, global, &result);
        if (UNLIKELY(result != PersistentTermOk)) {
            SMP_RWLOCK_UNLOCK(persistent_term->lock);
            return result;
        }
        if (equal) {
            SMP_RWLOCK_UNLOCK(persistent_term->lock);
            return PersistentTermOk;
        }
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return PersistentTermExists;
    }

    SMP_RWLOCK_UNLOCK(persistent_term->lock);

    struct PersistentTermEntry *new_entry = entry_new(key, value);
    if (IS_NULL_PTR(new_entry)) {
        return PersistentTermAllocationError;
    }

    SMP_RWLOCK_WRLOCK(persistent_term->lock);

    result = find_entry(persistent_term, bucket_index, key, &entry, global);
    if (UNLIKELY(result != PersistentTermOk)) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        entry_destroy(new_entry, global);
        return result;
    }

    if (entry != NULL) {
        bool equal = term_is_equal(entry->value, value, global, &result);
        if (UNLIKELY(result != PersistentTermOk)) {
            SMP_RWLOCK_UNLOCK(persistent_term->lock);
            entry_destroy(new_entry, global);
            return result;
        }

        if (equal) {
            SMP_RWLOCK_UNLOCK(persistent_term->lock);
            entry_destroy(new_entry, global);
            return PersistentTermOk;
        }

        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        entry_destroy(new_entry, global);
        return PersistentTermExists;
    }

    if (UNLIKELY(new_entry->memory > SIZE_MAX - persistent_term->memory)) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        entry_destroy(new_entry, global);
        return PersistentTermAllocationError;
    }

    new_entry->next = persistent_term->buckets[bucket_index];
    persistent_term->buckets[bucket_index] = new_entry;
    persistent_term->count++;
    persistent_term->memory += new_entry->memory;

    SMP_RWLOCK_UNLOCK(persistent_term->lock);
    return PersistentTermOk;
}

persistent_term_result_t persistent_term_get(
    PersistentTerm *persistent_term,
    term key,
    term *value,
    GlobalContext *global)
{
    assert(value != NULL);

    uint32_t bucket_index = term_hash(key, global) % PERSISTENT_TERM_NUM_BUCKETS;

    SMP_RWLOCK_RDLOCK(persistent_term->lock);

    struct PersistentTermEntry *entry;
    persistent_term_result_t result = find_entry(persistent_term, bucket_index, key, &entry, global);
    if (UNLIKELY(result != PersistentTermOk)) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return result;
    }

    if (entry == NULL) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return PersistentTermNotFound;
    }

    *value = entry->value;
    SMP_RWLOCK_UNLOCK(persistent_term->lock);
    return PersistentTermOk;
}

persistent_term_result_t persistent_term_get_all_maybe_gc(
    PersistentTerm *persistent_term,
    term *ret,
    Context *ctx)
{
    assert(ret != NULL);

    SMP_RWLOCK_RDLOCK(persistent_term->lock);

    size_t needed = 0;
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        for (struct PersistentTermEntry *entry = persistent_term->buckets[i]; entry != NULL; entry = entry->next) {
            needed += CONS_SIZE + TUPLE_SIZE(2);
        }
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, needed, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        SMP_RWLOCK_UNLOCK(persistent_term->lock);
        return PersistentTermAllocationError;
    }

    term list = term_nil();
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        for (struct PersistentTermEntry *entry = persistent_term->buckets[i]; entry != NULL; entry = entry->next) {
            term tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(tuple, 0, entry->key);
            term_put_tuple_element(tuple, 1, entry->value);
            list = term_list_prepend(tuple, list, &ctx->heap);
        }
    }

    *ret = list;
    SMP_RWLOCK_UNLOCK(persistent_term->lock);
    return PersistentTermOk;
}

void persistent_term_info(PersistentTerm *persistent_term, size_t *count, size_t *memory)
{
    assert(count != NULL);
    assert(memory != NULL);

    SMP_RWLOCK_RDLOCK(persistent_term->lock);
    *count = persistent_term->count;
    *memory = persistent_term->memory;
    SMP_RWLOCK_UNLOCK(persistent_term->lock);
}

static persistent_term_result_t find_entry(
    PersistentTerm *persistent_term,
    uint32_t bucket_index,
    term key,
    struct PersistentTermEntry **out_entry,
    GlobalContext *global)
{
    assert(out_entry != NULL);

    *out_entry = NULL;

    for (struct PersistentTermEntry *entry = persistent_term->buckets[bucket_index]; entry != NULL; entry = entry->next) {
        persistent_term_result_t result = PersistentTermOk;
        bool equal = term_is_equal(entry->key, key, global, &result);
        if (UNLIKELY(result != PersistentTermOk)) {
            return result;
        }

        if (equal) {
            *out_entry = entry;
            return PersistentTermOk;
        }
    }

    return PersistentTermOk;
}

static struct PersistentTermEntry *entry_new(term key, term value)
{
    struct PersistentTermEntry *entry = malloc(sizeof(struct PersistentTermEntry));
    if (IS_NULL_PTR(entry)) {
        return NULL;
    }

    Heap *heap = malloc(sizeof(Heap));
    if (IS_NULL_PTR(heap)) {
        free(entry);
        return NULL;
    }

    const size_t accounting_overhead = sizeof(struct PersistentTermEntry) + sizeof(Heap) + sizeof(HeapFragment);
    const size_t max_heap_terms = (SIZE_MAX - accounting_overhead) / sizeof(term);
    size_t key_size;
    if (UNLIKELY(!memory_estimate_usage_with_limit(key, max_heap_terms, &key_size))) {
        free(heap);
        free(entry);
        return NULL;
    }
    size_t value_size;
    if (UNLIKELY(!memory_estimate_usage_with_limit(value, max_heap_terms - key_size, &value_size))) {
        free(heap);
        free(entry);
        return NULL;
    }
    size_t size = key_size + value_size;
    if (UNLIKELY(memory_init_heap(heap, size) != MEMORY_GC_OK)) {
        free(heap);
        free(entry);
        return NULL;
    }

    entry->key = memory_copy_term_tree(heap, key);
    entry->value = memory_copy_term_tree(heap, value);
    entry->heap = heap;
    entry->memory = accounting_overhead + ((size_t) (heap->heap_end - heap->heap_start) * sizeof(term));
    entry->next = NULL;

    return entry;
}

static void entry_destroy(struct PersistentTermEntry *entry, GlobalContext *global)
{
    memory_destroy_heap(entry->heap, global);
    free(entry->heap);
    free(entry);
}

static bool term_is_equal(term a, term b, GlobalContext *global, persistent_term_result_t *result)
{
    TermCompareResult compare_result = term_compare(a, b, TermCompareExact, global);
    if (UNLIKELY(compare_result == TermCompareMemoryAllocFail)) {
        *result = PersistentTermAllocationError;
        return false;
    }

    *result = PersistentTermOk;
    return compare_result == TermEquals;
}
