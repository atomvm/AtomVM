/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 Mateusz Furga <mateusz.furga@swmansion.com>
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

#include <assert.h>
#include <stdint.h>

#include "globalcontext.h"
#include "term.h"

#include "ets_multimap.h"

#define DYNARRAY_INITIAL_CAPACITY 8
#define DYNARRAY_GROWTH_FACTOR 2

static uint32_t hash_term(term t, GlobalContext *global);

static EtsMultimapEntry *entry_new(term tuple);
static void entry_delete(EtsMultimapEntry *entry, GlobalContext *global);
static EtsMultimapNode *node_new(EtsMultimapNode *next, EtsMultimapEntry *entries);
static void node_delete(EtsMultimapNode *node, GlobalContext *global);
static ets_result_t node_find(
    EtsMultimap *multimap,
    term key,
    EtsMultimapNode **out_node,
    GlobalContext *global);
static term node_key(EtsMultimap *multimap, EtsMultimapNode *node);
static void multimap_to_single(EtsMultimap *multimap, GlobalContext *global);
static void insert_revert(
    EtsMultimap *multimap,
    EtsMultimapEntry **entries,
    size_t count,
    GlobalContext *global);
static ets_result_t tuple_exists(
    EtsMultimapNode *node,
    term tuple,
    bool *exists,
    GlobalContext *global);

EtsMultimap *ets_multimap_new(ets_multimap_type_t type, size_t key_index)
{
    EtsMultimap *multimap = malloc(sizeof(struct EtsMultimap));
    if (IS_NULL_PTR(multimap)) {
        return NULL;
    }

    multimap->type = type;
    multimap->key_index = key_index;

    for (size_t i = 0; i < ETS_MULTIMAP_NUM_BUCKETS; i++) {
        multimap->buckets[i] = NULL;
    }

    return multimap;
}

void ets_multimap_delete(EtsMultimap *multimap, GlobalContext *global)
{
    for (size_t i = 0; i < ETS_MULTIMAP_NUM_BUCKETS; i++) {
        EtsMultimapNode *node = multimap->buckets[i];
        while (node != NULL) {
            EtsMultimapNode *next = node->next;
            node_delete(node, global);
            node = next;
        }
    }
    free(multimap);
}

ets_result_t ets_multimap_lookup(
    EtsMultimap *multimap,
    term key,
    term **tuples,
    size_t *count,
    GlobalContext *global)
{
    assert(count != NULL);

    *count = 0;

    EtsMultimapNode *node;
    ets_result_t result = node_find(multimap, key, &node, global);
    if (UNLIKELY(result == EtsAllocationError)) {
        return result;
    }

    if (node == NULL) {
        return EtsOk;
    }

    assert(node->entries != NULL);

    for (EtsMultimapEntry *iter = node->entries; iter != NULL; iter = iter->next) {
        (*count)++;
    }

    if (tuples == NULL) {
        // only return number of tuples found
        return EtsOk;
    }

    *tuples = malloc(sizeof(term) * (*count));
    if (IS_NULL_PTR(*tuples)) {
        return EtsAllocationError;
    }

    size_t i = 0;
    for (EtsMultimapEntry *iter = node->entries; iter != NULL; iter = iter->next, i++) {
        assert(i < *count);
        (*tuples)[i] = iter->tuple;
    }

    return EtsOk;
}

ets_result_t ets_multimap_insert(
    EtsMultimap *multimap,
    term *tuples,
    size_t count,
    GlobalContext *global)
{
    if (tuples == NULL || count == 0) {
        return EtsOk;
    }

    EtsMultimapEntry **entries = malloc(sizeof(EtsMultimapEntry *) * count);
    if (IS_NULL_PTR(entries)) {
        return EtsAllocationError;
    }

    for (size_t i = 0; i < count; i++) {
        entries[i] = entry_new(tuples[i]);
        if (IS_NULL_PTR(entries[i])) {
            for (size_t j = 0; j < i; j++) {
                entry_delete(entries[j], global);
            }
            free(entries);
            return EtsAllocationError;
        }
    }

    ets_result_t status = EtsOk;

    for (size_t i = 0; i < count; i++) {
        EtsMultimapEntry *entry = entries[i];
        term key = term_get_tuple_element(entry->tuple, multimap->key_index);

        EtsMultimapNode *node;
        if (UNLIKELY(node_find(multimap, key, &node, global) == EtsAllocationError)) {
            status = EtsAllocationError;
            break;
        }

        if (node == NULL) {
            EtsMultimapNode *new_node = node_new(NULL, entry);
            if (IS_NULL_PTR(new_node)) {
                status = EtsAllocationError;
                break;
            }

            assert(new_node->entries != NULL);

            uint32_t idx = hash_term(key, global) % ETS_MULTIMAP_NUM_BUCKETS;
            new_node->next = multimap->buckets[idx];
            multimap->buckets[idx] = new_node;
            continue;
        }

        assert(node->entries != NULL);

        if (multimap->type == EtsMultimapTypeSet) {
            bool exists;

            if (UNLIKELY(tuple_exists(node, entry->tuple, &exists, global) == EtsAllocationError)) {
                status = EtsAllocationError;
                break;
            }

            if (exists) {
                entry_delete(entry, global);
                entries[i] = NULL;
                continue;
            }
        }

        entry->next = node->entries;
        node->entries = entry;
    }

    if (status != EtsOk) {
        insert_revert(multimap, entries, count, global);
    } else if (multimap->type == EtsMultimapTypeSingle) {
        multimap_to_single(multimap, global);
    }

    free(entries);

    return status;
}

ets_result_t ets_multimap_remove(
    EtsMultimap *multimap,
    term key,
    GlobalContext *global)
{
    EtsMultimapNode *node;
    if (UNLIKELY(node_find(multimap, key, &node, global) == EtsAllocationError)) {
        return EtsAllocationError;
    }

    if (node == NULL) {
        return EtsOk;
    }

    assert(node->entries != NULL);
    assert(term_compare(key, node_key(multimap, node), TermCompareExact, global) == TermEquals);

    uint32_t idx = hash_term(key, global) % ETS_MULTIMAP_NUM_BUCKETS;
    EtsMultimapNode *iter = multimap->buckets[idx];
    EtsMultimapNode *prev = NULL;

    while (iter) {
        if (iter == node) {
            if (prev == NULL) {
                multimap->buckets[idx] = iter->next;
            } else {
                prev->next = iter->next;
            }
            break;
        }
        prev = iter;
        iter = iter->next;
    }

    node_delete(node, global);

    return EtsOk;
}

ets_result_t ets_multimap_remove_tuple(
    EtsMultimap *multimap,
    term tuple,
    GlobalContext *global)
{
    assert(term_is_tuple(tuple));

    if (multimap->key_index >= (size_t) term_get_tuple_arity(tuple)) {
        return EtsBadEntry;
    }

    term key = term_get_tuple_element(tuple, multimap->key_index);

    EtsMultimapNode *node;
    if (UNLIKELY(node_find(multimap, key, &node, global) == EtsAllocationError)) {
        return EtsAllocationError;
    }

    if (node == NULL) {
        return EtsOk;
    }

    assert(node->entries != NULL);

    size_t capacity = DYNARRAY_INITIAL_CAPACITY;
    size_t count = 0;

    EtsMultimapEntry **to_remove = malloc(sizeof(EtsMultimapEntry *) * capacity);
    if (IS_NULL_PTR(to_remove)) {
        return EtsAllocationError;
    }

    for (EtsMultimapEntry *iter = node->entries; iter != NULL; iter = iter->next) {
        TermCompareResult result = term_compare(tuple, iter->tuple, TermCompareExact, global);

        if (UNLIKELY(result == TermCompareMemoryAllocFail)) {
            free(to_remove);
            return EtsAllocationError;
        }

        if (result == TermEquals) {
            if (count >= capacity) {
                capacity *= DYNARRAY_GROWTH_FACTOR;
                EtsMultimapEntry **new_to_remove = realloc(to_remove, sizeof(EtsMultimapEntry *) * capacity);
                if (IS_NULL_PTR(new_to_remove)) {
// GCC 12 is raising here a false positive warning, according to man realloc:
// "If realloc() fails, the original block is left untouched; it is not freed or moved."
#pragma GCC diagnostic push
#if defined(__GNUC__) && !defined(__clang__) && __GNUC__ == 12
#pragma GCC diagnostic ignored "-Wuse-after-free"
#endif
                    free(to_remove);
#pragma GCC diagnostic pop
                    return EtsAllocationError;
                }
                to_remove = new_to_remove;
            }
            to_remove[count++] = iter;
        }
    }

    EtsMultimapEntry *prev = NULL;
    for (EtsMultimapEntry *iter = node->entries; iter != NULL; iter = iter->next) {
        bool removed = false;
        for (size_t i = 0; i < count; i++) {
            if (iter == to_remove[i]) {
                if (prev == NULL) {
                    node->entries = iter->next;
                } else {
                    prev->next = iter->next;
                }
                removed = true;
                break;
            }
        }
        if (!removed) {
            prev = iter;
        }
    }

    for (size_t i = 0; i < count; i++) {
        entry_delete(to_remove[i], global);
    }

    if (node->entries == NULL) {
        uint32_t idx = hash_term(key, global) % ETS_MULTIMAP_NUM_BUCKETS;

        EtsMultimapNode *prev_node = NULL;
        for (EtsMultimapNode *iter = multimap->buckets[idx]; iter != NULL; prev_node = iter, iter = iter->next) {
            if (iter == node) {
                if (prev_node == NULL) {
                    multimap->buckets[idx] = iter->next;
                } else {
                    prev_node->next = iter->next;
                }
                break;
            }
        }

        node_delete(node, global);
    }

    free(to_remove);

    return EtsOk;
}

static ets_result_t node_find(
    EtsMultimap *multimap,
    term key,
    EtsMultimapNode **out_node,
    GlobalContext *global)
{
    assert(out_node != NULL);

    *out_node = NULL;

    uint32_t idx = hash_term(key, global) % ETS_MULTIMAP_NUM_BUCKETS;
    EtsMultimapNode *node = multimap->buckets[idx];

    while (node) {
        TermCompareResult result = term_compare(key, node_key(multimap, node), TermCompareExact, global);

        if (UNLIKELY(result == TermCompareMemoryAllocFail)) {
            return EtsAllocationError;
        }

        if (result == TermEquals) {
            *out_node = node;
            return EtsOk;
        }

        node = node->next;
    }

    return EtsOk;
}

// Revert a partial batch insert. Assumes that newly inserted items are always at the head.
static void insert_revert(
    EtsMultimap *multimap,
    EtsMultimapEntry **entries,
    size_t count,
    GlobalContext *global)
{
    for (size_t i = 0; i < ETS_MULTIMAP_NUM_BUCKETS; i++) {
        EtsMultimapNode *node = multimap->buckets[i];

        while (node != NULL) {
            EtsMultimapNode *next_node = node->next;
            EtsMultimapEntry *entry = node->entries;

            assert(entry != NULL);

            while (entry != NULL) {
                EtsMultimapEntry *next_entry = entry->next;

                for (size_t j = 0; j < count; j++) {
                    if (entry == entries[j]) {
                        node->entries = next_entry;
                    }
                }

                entry = next_entry;
            }

            if (node->entries == NULL) {
                multimap->buckets[i] = next_node;
                node_delete(node, global);
            }

            node = next_node;
        }
    }

    for (size_t i = 0; i < count; i++) {
        if (entries[i] != NULL) {
            entry_delete(entries[i], global);
        }
    }
}

static void multimap_to_single(EtsMultimap *multimap, GlobalContext *global)
{
    for (size_t i = 0; i < ETS_MULTIMAP_NUM_BUCKETS; i++) {
        for (EtsMultimapNode *node = multimap->buckets[i]; node != NULL; node = node->next) {
            assert(node->entries != NULL);
            EtsMultimapEntry *entry = node->entries->next;

            while (entry != NULL) {
                EtsMultimapEntry *next = entry->next;
                entry_delete(entry, global);
                entry = next;
            }

            node->entries->next = NULL;
        }
    }
}

static ets_result_t tuple_exists(
    EtsMultimapNode *node,
    term tuple,
    bool *exists,
    GlobalContext *global)
{
    for (EtsMultimapEntry *iter = node->entries; iter != NULL; iter = iter->next) {
        TermCompareResult result = term_compare(tuple, iter->tuple, TermCompareExact, global);

        if (UNLIKELY(result == TermCompareMemoryAllocFail)) {
            return EtsAllocationError;
        }

        if (result == TermEquals) {
            *exists = true;
            return EtsOk;
        }
    }

    *exists = false;
    return EtsOk;
}

static term node_key(EtsMultimap *multimap, EtsMultimapNode *node)
{
    EtsMultimapEntry *entry = node->entries;
    assert(entry != NULL);
    return term_get_tuple_element(entry->tuple, multimap->key_index);
}

static EtsMultimapNode *node_new(EtsMultimapNode *next, EtsMultimapEntry *entries)
{
    EtsMultimapNode *node = malloc(sizeof(EtsMultimapNode));
    if (IS_NULL_PTR(node)) {
        return NULL;
    }
    node->next = next;
    node->entries = entries;
    return node;
}

static EtsMultimapEntry *entry_new(term tuple)
{
    EtsMultimapEntry *entry = malloc(sizeof(EtsMultimapEntry));
    if (IS_NULL_PTR(entry)) {
        return NULL;
    }

    Heap *heap = malloc(sizeof(Heap));
    if (IS_NULL_PTR(heap)) {
        free(entry);
        return NULL;
    }

    size_t size = memory_estimate_usage(tuple);
    if (UNLIKELY(memory_init_heap(heap, size) != MEMORY_GC_OK)) {
        free(entry);
        free(heap);
        return NULL;
    }

    tuple = memory_copy_term_tree(heap, tuple);

    entry->tuple = tuple;
    entry->heap = heap;
    entry->next = NULL;

    return entry;
}

static void node_delete(EtsMultimapNode *node, GlobalContext *global)
{
    EtsMultimapEntry *entry = node->entries;

    while (entry != NULL) {
        EtsMultimapEntry *next = entry->next;
        entry_delete(entry, global);
        entry = next;
    }

    free(node);
}

static void entry_delete(EtsMultimapEntry *entry, GlobalContext *global)
{
    memory_destroy_heap(entry->heap, global);
    free(entry->heap);
    free(entry);
}

//
// hash function
//
// Conceptually similar to (but not identical to) the `make_hash` algorithm described in
// https://github.com/erlang/otp/blob/cbd1378ee1fde835e55614bac9290b281bafe49a/erts/emulator/beam/utils.c#L644
//
// Also described in character folding algorithm (PJW Hash)
// https://en.wikipedia.org/wiki/Hash_function#Character_folding
//
// TODO: implement erlang:phash2 using the OTP algorithm
//

// some large (close to 2^24) primes taken from
// http://compoasso.free.fr/primelistweb/page/prime/liste_online_en.php

#define LARGE_PRIME_INITIAL 16777259
#define LARGE_PRIME_ATOM 16777643
#define LARGE_PRIME_INTEGER 16777781
#define LARGE_PRIME_FLOAT 16777973
#define LARGE_PRIME_PID 16778147
#define LARGE_PRIME_REF 16778441
#define LARGE_PRIME_BINARY 16780483
#define LARGE_PRIME_TUPLE 16778821
#define LARGE_PRIME_LIST 16779179
#define LARGE_PRIME_MAP 16779449
#define LARGE_PRIME_PORT 16778077

static uint32_t hash_atom(term t, uint32_t h, GlobalContext *global)
{
    size_t len;
    const uint8_t *data = atom_table_get_atom_string(global->atom_table, term_to_atom_index(t), &len);
    for (size_t i = 0; i < len; ++i) {
        h = h * LARGE_PRIME_ATOM + data[i];
    }
    return h * LARGE_PRIME_ATOM;
}

static uint32_t hash_integer(term t, uint32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint64_t n = (uint64_t) term_maybe_unbox_int64(t);
    while (n) {
        h = h * LARGE_PRIME_INTEGER + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_INTEGER;
}

static uint32_t hash_float(term t, uint32_t h, GlobalContext *global)
{
    UNUSED(global);
    avm_float_t f = term_to_float(t);
    // Normalize -0.0 to +0.0 so that hash is consistent with term_compare (-0.0 == +0.0).
    if (f == 0.0) {
        f = 0.0;
    }
    uint8_t *data = (uint8_t *) &f;
    size_t len = sizeof(avm_float_t);
    for (size_t i = 0; i < len; ++i) {
        h = h * LARGE_PRIME_FLOAT + data[i];
    }
    return h * LARGE_PRIME_FLOAT;
}

static uint32_t hash_local_pid(term t, uint32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint32_t n = (uint32_t) term_to_local_process_id(t);
    while (n) {
        h = h * LARGE_PRIME_PID + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_PID;
}

static uint32_t hash_local_port(term t, uint32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint32_t n = (uint32_t) term_to_local_process_id(t);
    while (n) {
        h = h * LARGE_PRIME_PORT + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_PORT;
}

static uint32_t hash_external_pid(term t, uint32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint32_t n = (uint32_t) term_get_external_pid_process_id(t);
    while (n) {
        h = h * LARGE_PRIME_PID + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_PID;
}

static uint32_t hash_external_port(term t, uint32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint32_t n = (uint32_t) term_get_external_port_number(t);
    while (n) {
        h = h * LARGE_PRIME_PORT + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_PORT;
}

static uint32_t hash_local_reference(term t, uint32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint64_t n = term_to_ref_ticks(t);
    while (n) {
        h = h * LARGE_PRIME_REF + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_REF;
}

static uint32_t hash_external_reference(term t, uint32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint32_t l = term_get_external_reference_len(t);
    const uint32_t *words = term_get_external_reference_words(t);
    for (uint32_t i = 0; i < l; i++) {
        uint32_t n = words[i];
        while (n) {
            h = h * LARGE_PRIME_REF + (n & 0xFF);
            n >>= 8;
        }
    }
    return h * LARGE_PRIME_REF;
}

static uint32_t hash_binary(term t, uint32_t h, GlobalContext *global)
{
    UNUSED(global);
    size_t len = (size_t) term_binary_size(t);
    uint8_t *data = (uint8_t *) term_binary_data(t);
    for (size_t i = 0; i < len; ++i) {
        h = h * LARGE_PRIME_BINARY + data[i];
    }
    return h * LARGE_PRIME_BINARY;
}

static uint32_t hash_term_incr(term t, uint32_t h, GlobalContext *global)
{
    if (term_is_atom(t)) {
        return hash_atom(t, h, global);
    } else if (term_is_any_integer(t)) {
        return hash_integer(t, h, global);
    } else if (term_is_float(t)) {
        return hash_float(t, h, global);
    } else if (term_is_local_pid(t)) {
        return hash_local_pid(t, h, global);
    } else if (term_is_external_pid(t)) {
        return hash_external_pid(t, h, global);
    } else if (term_is_local_port(t)) {
        return hash_local_port(t, h, global);
    } else if (term_is_external_port(t)) {
        return hash_external_port(t, h, global);
    } else if (term_is_local_reference(t)) {
        return hash_local_reference(t, h, global);
    } else if (term_is_external_reference(t)) {
        return hash_external_reference(t, h, global);
    } else if (term_is_binary(t)) {
        return hash_binary(t, h, global);
    } else if (term_is_tuple(t)) {
        size_t arity = term_get_tuple_arity(t);
        for (size_t i = 0; i < arity; ++i) {
            term elt = term_get_tuple_element(t, (int) i);
            h = h * LARGE_PRIME_TUPLE + hash_term_incr(elt, h, global);
        }
        return h * LARGE_PRIME_TUPLE;
    } else if (term_is_list(t)) {
        while (term_is_nonempty_list(t)) {
            term elt = term_get_list_head(t);
            h = h * LARGE_PRIME_LIST + hash_term_incr(elt, h, global);
            t = term_get_list_tail(t);
            if (term_is_nil(t)) {
                h = h * LARGE_PRIME_LIST;
                break;
            } else if (!term_is_list(t)) {
                h = h * LARGE_PRIME_LIST + hash_term_incr(t, h, global);
                break;
            }
        }
        return h * LARGE_PRIME_LIST;
    } else if (term_is_map(t)) {
        size_t size = term_get_map_size(t);
        for (size_t i = 0; i < size; ++i) {
            term key = term_get_map_key(t, (avm_uint_t) i);
            h = h * LARGE_PRIME_MAP + hash_term_incr(key, h, global);
            term value = term_get_map_value(t, (avm_uint_t) i);
            h = h * LARGE_PRIME_MAP + hash_term_incr(value, h, global);
        }
        return h * LARGE_PRIME_MAP;
    } else {
        fprintf(stderr, "hash_term: unsupported term type\n");
        return h;
    }
}

static uint32_t hash_term(term t, GlobalContext *global)
{
    return hash_term_incr(t, LARGE_PRIME_INITIAL, global);
}
