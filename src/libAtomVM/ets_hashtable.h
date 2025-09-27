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

#ifndef _ETS_HASHTABLE_H_
#define _ETS_HASHTABLE_H_

#include "globalcontext.h"
#include "term.h"
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

#define NUM_BUCKETS 16

struct EtsHashTable
{
    size_t capacity;
    struct HNode *buckets[NUM_BUCKETS];
};

typedef enum EtsHashtableOptions
{
    EtsHashtableAllowOverwrite = 1
} EtsHashtableOptions;

typedef enum EtsHashtableStatus
{
    EtsHashtableOk = 0,
    EtsHashtableKeyAlreadyExists,
    EtsHashtableOutOfMemory
} EtsHashtableStatus;

struct EtsHashTable *ets_hashtable_new(void);
void ets_hashtable_destroy(struct EtsHashTable *hash_table, GlobalContext *global);

EtsHashtableStatus ets_hashtable_insert(struct EtsHashTable *hash_table, struct HNode *new_node, EtsHashtableOptions opts, GlobalContext *global);
term ets_hashtable_lookup(struct EtsHashTable *hash_table, term key, size_t keypos, GlobalContext *global);
bool ets_hashtable_remove(struct EtsHashTable *hash_table, term key, size_t keypos, GlobalContext *global);
struct HNode *ets_hashtable_new_node(term entry, int keypos);
void ets_hashtable_free_node(struct HNode *node, GlobalContext *global);

#ifdef __cplusplus
}
#endif

#endif
