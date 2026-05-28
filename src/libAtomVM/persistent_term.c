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
#include <stdlib.h>

#include "context.h"
#include "globalcontext.h"
#include "memory.h"
#include "smp.h"
#include "term.h"
#include "term_hash.h"
#include "utils.h"

#ifndef AVM_NO_SMP
#define SMP_RDLOCK(persistent_term) smp_spinlock_lock(&(persistent_term)->lock)
#define SMP_WRLOCK(persistent_term) smp_spinlock_lock(&(persistent_term)->lock)
#define SMP_UNLOCK(persistent_term) smp_spinlock_unlock(&(persistent_term)->lock)
#else
#define SMP_RDLOCK(persistent_term) UNUSED(persistent_term)
#define SMP_WRLOCK(persistent_term) UNUSED(persistent_term)
#define SMP_UNLOCK(persistent_term) UNUSED(persistent_term)
#endif

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
    term key,
    struct PersistentTermEntry ***out_link,
    struct PersistentTermEntry **out_entry,
    GlobalContext *global);
static struct PersistentTermEntry *entry_new(term key, term value);
static void entry_destroy(struct PersistentTermEntry *entry, GlobalContext *global);
static void retire_entry(PersistentTerm *persistent_term, struct PersistentTermEntry *entry);
static bool term_is_equal(term a, term b, GlobalContext *global, persistent_term_result_t *result);

void persistent_term_init(PersistentTerm *persistent_term)
{
    persistent_term->count = 0;
    persistent_term->memory = 0;
    persistent_term->retired_entries = NULL;
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        persistent_term->buckets[i] = NULL;
    }

#ifndef AVM_NO_SMP
    smp_spinlock_init(&persistent_term->lock);
#endif
}

void persistent_term_destroy(PersistentTerm *persistent_term, GlobalContext *global)
{
    SMP_WRLOCK(persistent_term);
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        struct PersistentTermEntry *entry = persistent_term->buckets[i];
        while (entry != NULL) {
            struct PersistentTermEntry *next = entry->next;
            entry_destroy(entry, global);
            entry = next;
        }
        persistent_term->buckets[i] = NULL;
    }

    struct PersistentTermEntry *entry = persistent_term->retired_entries;
    while (entry != NULL) {
        struct PersistentTermEntry *next = entry->next;
        entry_destroy(entry, global);
        entry = next;
    }
    persistent_term->retired_entries = NULL;
    persistent_term->count = 0;
    persistent_term->memory = 0;
    SMP_UNLOCK(persistent_term);
}

persistent_term_result_t persistent_term_put(
    PersistentTerm *persistent_term,
    term key,
    term value,
    bool put_new,
    GlobalContext *global)
{
    SMP_WRLOCK(persistent_term);

    struct PersistentTermEntry **link;
    struct PersistentTermEntry *entry;
    persistent_term_result_t result = find_entry(persistent_term, key, &link, &entry, global);
    if (UNLIKELY(result != PersistentTermOk)) {
        SMP_UNLOCK(persistent_term);
        return result;
    }

    if (entry != NULL) {
        bool equal = term_is_equal(entry->value, value, global, &result);
        if (UNLIKELY(result != PersistentTermOk)) {
            SMP_UNLOCK(persistent_term);
            return result;
        }

        if (equal) {
            SMP_UNLOCK(persistent_term);
            return PersistentTermOk;
        }

        if (put_new) {
            SMP_UNLOCK(persistent_term);
            return PersistentTermExists;
        }
    }

    struct PersistentTermEntry *new_entry = entry_new(key, value);
    if (IS_NULL_PTR(new_entry)) {
        SMP_UNLOCK(persistent_term);
        return PersistentTermAllocationError;
    }

    if (entry == NULL) {
        uint32_t idx = term_hash(key, global) % PERSISTENT_TERM_NUM_BUCKETS;
        new_entry->next = persistent_term->buckets[idx];
        persistent_term->buckets[idx] = new_entry;
        persistent_term->count++;
        persistent_term->memory += new_entry->memory;
    } else {
        new_entry->next = entry->next;
        *link = new_entry;
        persistent_term->memory += new_entry->memory;
        retire_entry(persistent_term, entry);
    }

    SMP_UNLOCK(persistent_term);
    return PersistentTermOk;
}

persistent_term_result_t persistent_term_get(
    PersistentTerm *persistent_term,
    term key,
    term *value,
    GlobalContext *global)
{
    assert(value != NULL);

    SMP_RDLOCK(persistent_term);

    struct PersistentTermEntry *entry;
    persistent_term_result_t result = find_entry(persistent_term, key, NULL, &entry, global);
    if (UNLIKELY(result != PersistentTermOk)) {
        SMP_UNLOCK(persistent_term);
        return result;
    }

    if (entry == NULL) {
        SMP_UNLOCK(persistent_term);
        return PersistentTermNotFound;
    }

    *value = entry->value;
    SMP_UNLOCK(persistent_term);
    return PersistentTermOk;
}

persistent_term_result_t persistent_term_erase(
    PersistentTerm *persistent_term,
    term key,
    bool *removed,
    GlobalContext *global)
{
    assert(removed != NULL);

    *removed = false;

    SMP_WRLOCK(persistent_term);

    struct PersistentTermEntry **link;
    struct PersistentTermEntry *entry;
    persistent_term_result_t result = find_entry(persistent_term, key, &link, &entry, global);
    if (UNLIKELY(result != PersistentTermOk)) {
        SMP_UNLOCK(persistent_term);
        return result;
    }

    if (entry == NULL) {
        SMP_UNLOCK(persistent_term);
        return PersistentTermOk;
    }

    *link = entry->next;
    persistent_term->count--;
    retire_entry(persistent_term, entry);

    *removed = true;
    SMP_UNLOCK(persistent_term);
    return PersistentTermOk;
}

persistent_term_result_t persistent_term_get_all_maybe_gc(
    PersistentTerm *persistent_term,
    term *ret,
    Context *ctx)
{
    assert(ret != NULL);

    SMP_RDLOCK(persistent_term);

    size_t needed = 0;
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        for (struct PersistentTermEntry *entry = persistent_term->buckets[i]; entry != NULL; entry = entry->next) {
            needed += CONS_SIZE + TUPLE_SIZE(2) + memory_estimate_usage(entry->key);
        }
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, needed, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        SMP_UNLOCK(persistent_term);
        return PersistentTermAllocationError;
    }

    term list = term_nil();
    for (size_t i = 0; i < PERSISTENT_TERM_NUM_BUCKETS; i++) {
        for (struct PersistentTermEntry *entry = persistent_term->buckets[i]; entry != NULL; entry = entry->next) {
            term tuple = term_alloc_tuple(2, &ctx->heap);
            term key = memory_copy_term_tree(&ctx->heap, entry->key);
            term_put_tuple_element(tuple, 0, key);
            term_put_tuple_element(tuple, 1, entry->value);
            list = term_list_prepend(tuple, list, &ctx->heap);
        }
    }

    *ret = list;
    SMP_UNLOCK(persistent_term);
    return PersistentTermOk;
}

void persistent_term_info(PersistentTerm *persistent_term, size_t *count, size_t *memory)
{
    assert(count != NULL);
    assert(memory != NULL);

    SMP_RDLOCK(persistent_term);
    *count = persistent_term->count;
    *memory = persistent_term->memory;
    SMP_UNLOCK(persistent_term);
}

static persistent_term_result_t find_entry(
    PersistentTerm *persistent_term,
    term key,
    struct PersistentTermEntry ***out_link,
    struct PersistentTermEntry **out_entry,
    GlobalContext *global)
{
    assert(out_entry != NULL);

    *out_entry = NULL;

    uint32_t idx = term_hash(key, global) % PERSISTENT_TERM_NUM_BUCKETS;
    struct PersistentTermEntry **link = &persistent_term->buckets[idx];
    while (*link != NULL) {
        persistent_term_result_t result = PersistentTermOk;
        bool equal = term_is_equal((*link)->key, key, global, &result);
        if (UNLIKELY(result != PersistentTermOk)) {
            return result;
        }

        if (equal) {
            if (out_link != NULL) {
                *out_link = link;
            }
            *out_entry = *link;
            return PersistentTermOk;
        }

        link = &(*link)->next;
    }

    if (out_link != NULL) {
        *out_link = link;
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

    size_t size = memory_estimate_usage(key) + memory_estimate_usage(value);
    if (UNLIKELY(memory_init_heap(heap, size) != MEMORY_GC_OK)) {
        free(heap);
        free(entry);
        return NULL;
    }

    entry->key = memory_copy_term_tree(heap, key);
    entry->value = memory_copy_term_tree(heap, value);
    entry->heap = heap;
    entry->memory = sizeof(struct PersistentTermEntry) + sizeof(Heap) + sizeof(HeapFragment)
        + ((size_t) (heap->heap_ptr - heap->heap_start) * sizeof(term));
    entry->next = NULL;

    return entry;
}

static void entry_destroy(struct PersistentTermEntry *entry, GlobalContext *global)
{
    memory_destroy_heap(entry->heap, global);
    free(entry->heap);
    free(entry);
}

static void retire_entry(PersistentTerm *persistent_term, struct PersistentTermEntry *entry)
{
    entry->next = persistent_term->retired_entries;
    persistent_term->retired_entries = entry;
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
