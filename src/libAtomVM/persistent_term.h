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

#ifndef _PERSISTENT_TERM_H_
#define _PERSISTENT_TERM_H_

#include <stdbool.h>
#include <stddef.h>

#include "smp.h"
#include "term_typedef.h"

#ifdef __cplusplus
extern "C" {
#endif

#define PERSISTENT_TERM_NUM_BUCKETS 32

struct Context;
struct GlobalContext;
struct PersistentTermEntry;

typedef enum
{
    PersistentTermOk,
    PersistentTermNotFound,
    PersistentTermExists,
    PersistentTermAllocationError
} persistent_term_result_t;

typedef struct PersistentTerm
{
    size_t count;
    size_t memory;
    struct PersistentTermEntry *buckets[PERSISTENT_TERM_NUM_BUCKETS];
    struct PersistentTermEntry *retired_entries;
#ifndef AVM_NO_SMP
    RWLock *lock;
#endif
} PersistentTerm;

void persistent_term_init(PersistentTerm *persistent_term);
void persistent_term_destroy(PersistentTerm *persistent_term, struct GlobalContext *global);

persistent_term_result_t persistent_term_put(
    PersistentTerm *persistent_term,
    term key,
    term value,
    bool put_new,
    struct GlobalContext *global);

persistent_term_result_t persistent_term_get(
    PersistentTerm *persistent_term,
    term key,
    term *value,
    struct GlobalContext *global);

persistent_term_result_t persistent_term_erase(
    PersistentTerm *persistent_term,
    term key,
    bool *removed,
    struct GlobalContext *global);

persistent_term_result_t persistent_term_get_all_maybe_gc(
    PersistentTerm *persistent_term,
    term *ret,
    struct Context *ctx);

void persistent_term_info(PersistentTerm *persistent_term, size_t *count, size_t *memory);

#ifdef __cplusplus
}
#endif

#endif
