/*
 * This file is part of AtomVM.
 *
 * Copyright 2024 Fred Dushin <fred@dushin.net>
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

#include "ets_hashtable.h"

#include "smp.h"
#include "term.h"
#include "utils.h"

#include <stdlib.h>
#include <string.h>

// #define TRACE_ENABLED
#include "trace.h"

struct HNode
{
    struct HNode *next;
    term key;
    term entry;
    Heap heap;
};

static uint32_t hash_term(term t, GlobalContext *global);

struct EtsHashTable *ets_hashtable_new(void)
{
    struct EtsHashTable *htable = malloc(sizeof(struct EtsHashTable));
    if (IS_NULL_PTR(htable)) {
        return NULL;
    }

    memset(htable->buckets, 0, NUM_BUCKETS * sizeof(struct HNode *));
    htable->capacity = NUM_BUCKETS;

    return htable;
}

void ets_hashtable_free_node(struct HNode *node, GlobalContext *global)
{
    memory_destroy_heap(&node->heap, global);
    free(node);
}

void ets_hashtable_destroy(struct EtsHashTable *hash_table, GlobalContext *global)
{
    for (size_t i = 0; i < hash_table->capacity; ++i) {
        struct HNode *node = hash_table->buckets[i];
        while (node != NULL) {
            struct HNode *next_node = node->next;
            ets_hashtable_free_node(node, global);
            node = next_node;
        }
    }
}

#ifdef TRACE_ENABLED
static void print_info(struct EtsHashTable *hash_table)
{
    fprintf(stderr, "============\n");
    for (size_t i = 0; i < hash_table->capacity; ++i) {
        size_t len = 0;
        struct HNode *node = hash_table->buckets[i];
        while (node) {
            node = node->next;
            ++len;
        }
        fprintf(stderr, "len bucket[%zu]: %zu\n", i, len);
    }
}
#endif

struct HNode *ets_hashtable_new_node(term entry, int keypos)
{
    struct HNode *new_node = malloc(sizeof(struct HNode));
    if (IS_NULL_PTR(new_node)) {
        goto cleanup;
    }

    size_t size = memory_estimate_usage(entry);
    if (UNLIKELY(memory_init_heap(&new_node->heap, size) != MEMORY_GC_OK)) {
        goto cleanup;
    }

    term new_entry = memory_copy_term_tree(&new_node->heap, entry);
    assert(term_is_tuple(new_entry));
    assert(term_get_tuple_arity(new_entry) >= keypos);
    term key = term_get_tuple_element(new_entry, keypos);

    new_node->next = NULL;
    new_node->key = key;
    new_node->entry = new_entry;

    return new_node;

cleanup:
    free(new_node);
    return NULL;
}

EtsHashtableStatus ets_hashtable_insert(struct EtsHashTable *hash_table, struct HNode *new_node, EtsHashtableOptions opts, GlobalContext *global)
{
    term key = new_node->key;
    uint32_t hash = hash_term(key, global);
    uint32_t index = hash % hash_table->capacity;

#ifdef TRACE_ENABLED
    fprintf(stderr, "hash=%u index=%i key=", hash, index);
    term_fprint(stderr, key, global);
    fprintf(stderr, "\n");
#endif

    struct HNode *node = hash_table->buckets[index];
    struct HNode *last_node = NULL;
    while (node) {
        TermCompareResult cmp = term_compare(key, node->key, TermCompareExact, global);
        if (UNLIKELY(cmp == TermCompareMemoryAllocFail)) {
            return EtsHashtableOutOfMemory;
        }

        if (cmp == TermEquals) {
            if (opts & EtsHashtableAllowOverwrite) {
                if (IS_NULL_PTR(last_node)) {
                    new_node->next = node->next;
                    hash_table->buckets[index] = new_node;
                } else {
                    last_node->next = new_node;
                    new_node->next = node->next;
                }
                ets_hashtable_free_node(node, global);
                return EtsHashtableOk;
            } else {
                return EtsHashtableKeyAlreadyExists;
            }
        }
        last_node = node;
        node = node->next;
    }

    if (last_node) {
        last_node->next = new_node;
    } else {
        hash_table->buckets[index] = new_node;
    }

#ifdef TRACE_ENABLED
    print_info(hash_table);
#endif

    return EtsHashtableOk;
}

term ets_hashtable_lookup(struct EtsHashTable *hash_table, term key, size_t keypos, GlobalContext *global)
{
    uint32_t hash = hash_term(key, global);
    uint32_t index = hash % hash_table->capacity;

    const struct HNode *node = hash_table->buckets[index];
    while (node) {
        term key_to_compare = term_get_tuple_element(node->entry, keypos);
        if (term_compare(key, key_to_compare, TermCompareExact, global) == TermEquals) {
            return node->entry;
        }
        node = node->next;
    }

    return term_nil();
}

bool ets_hashtable_remove(struct EtsHashTable *hash_table, term key, size_t keypos, GlobalContext *global)
{
    uint32_t hash = hash_term(key, global);
    uint32_t index = hash % hash_table->capacity;

    struct HNode *node = hash_table->buckets[index];
    struct HNode *prev_node = NULL;
    while (node) {
        term key_to_compare = term_get_tuple_element(node->entry, keypos);
        if (term_compare(key, key_to_compare, TermCompareExact, global) == TermEquals) {
            struct HNode *next_node = node->next;
            ets_hashtable_free_node(node, global);
            if (prev_node != NULL) {
                prev_node->next = next_node;
            } else {
                hash_table->buckets[index] = next_node;
            }
            return true;
        } else {
            prev_node = node;
            node = node->next;
        }
    }

    return false;
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

static uint32_t hash_atom(term t, int32_t h, GlobalContext *global)
{
    size_t len;
    const uint8_t *data = atom_table_get_atom_string(global->atom_table, term_to_atom_index(t), &len);
    for (size_t i = 0; i < len; ++i) {
        h = h * LARGE_PRIME_ATOM + data[i];
    }
    return h * LARGE_PRIME_ATOM;
}

static uint32_t hash_integer(term t, int32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint64_t n = (uint64_t) term_maybe_unbox_int64(t);
    while (n) {
        h = h * LARGE_PRIME_INTEGER + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_INTEGER;
}

static uint32_t hash_float(term t, int32_t h, GlobalContext *global)
{
    UNUSED(global);
    avm_float_t f = term_to_float(t);
    uint8_t *data = (uint8_t *) &f;
    size_t len = sizeof(float);
    for (size_t i = 0; i < len; ++i) {
        h = h * LARGE_PRIME_FLOAT + data[i];
    }
    return h * LARGE_PRIME_FLOAT;
}

static uint32_t hash_local_pid(term t, int32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint32_t n = (uint32_t) term_to_local_process_id(t);
    while (n) {
        h = h * LARGE_PRIME_PID + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_PID;
}

static uint32_t hash_local_port(term t, int32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint32_t n = (uint32_t) term_to_local_process_id(t);
    while (n) {
        h = h * LARGE_PRIME_PORT + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_PORT;
}

static uint32_t hash_external_pid(term t, int32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint32_t n = (uint32_t) term_get_external_pid_process_id(t);
    while (n) {
        h = h * LARGE_PRIME_PID + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_PID;
}

static uint32_t hash_external_port(term t, int32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint32_t n = (uint32_t) term_get_external_port_number(t);
    while (n) {
        h = h * LARGE_PRIME_PORT + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_PORT;
}

static uint32_t hash_local_reference(term t, int32_t h, GlobalContext *global)
{
    UNUSED(global);
    uint64_t n = term_to_ref_ticks(t);
    while (n) {
        h = h * LARGE_PRIME_REF + (n & 0xFF);
        n >>= 8;
    }
    return h * LARGE_PRIME_REF;
}

static uint32_t hash_external_reference(term t, int32_t h, GlobalContext *global)
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

static uint32_t hash_binary(term t, int32_t h, GlobalContext *global)
{
    UNUSED(global);
    size_t len = (size_t) term_binary_size(t);
    uint8_t *data = (uint8_t *) term_binary_data(t);
    for (size_t i = 0; i < len; ++i) {
        h = h * LARGE_PRIME_BINARY + data[i];
    }
    return h * LARGE_PRIME_BINARY;
}

static uint32_t hash_term_incr(term t, int32_t h, GlobalContext *global)
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
        while (!term_is_nonempty_list(t)) {
            term elt = term_get_list_head(t);
            h = h * LARGE_PRIME_LIST + hash_term_incr(elt, h, global);
            t = term_get_list_tail(t);
            if (term_is_nil(t)) {
                h = h * LARGE_PRIME_LIST;
                break;
            } else if (!term_is_list(t)) {
                h = h * LARGE_PRIME_LIST + hash_term_incr(elt, h, global);
                break;
            }
        }
        return h * LARGE_PRIME_TUPLE;
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
