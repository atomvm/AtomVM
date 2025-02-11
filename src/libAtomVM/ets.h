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

#ifndef _ETS_H_
#define _ETS_H_

struct Context;
struct GlobalContext;

#include "list.h"
#include "synclist.h"
#include "term.h"

#ifdef __cplusplus
extern "C" {
#endif

// N.B. Only EtsTableSet currently supported
typedef enum EtsTableType
{
    EtsTableSet,
    EtsTableOrderedSet,
    EtsTableBag,
    EtsTableDuplicateBag
} EtsTableType;

typedef enum EtsAccessType
{
    EtsAccessPrivate,
    EtsAccessProtected,
    EtsAccessPublic
} EtsAccessType;

typedef enum EtsErrorCode
{
    EtsOk,
    EtsTableNotFound,
    EtsTableNameInUse,
    EtsPermissionDenied,
    EtsBadEntry,
    EtsAllocationFailure,
    EtsEntryNotFound,
    EtsBadPosition,
    EtsOverlfow
} EtsErrorCode;
struct Ets
{
    // TODO Using a list imposes O(len(ets_tables)) cost
    // on lookup, so in the future we may want to consider
    // a table or map instead of a list.
    struct SyncList ets_tables;
};

void ets_init(struct Ets *ets);
void ets_destroy(struct Ets *ets, GlobalContext *global);

EtsErrorCode ets_create_table_maybe_gc(term name, bool is_named, EtsTableType table_type, EtsAccessType access_type, size_t keypos, term *ret, Context *ctx);
void ets_delete_owned_tables(struct Ets *ets, int32_t process_id, GlobalContext *global);

EtsErrorCode ets_insert(term ref, term entry, Context *ctx);
EtsErrorCode ets_lookup_maybe_gc(term ref, term key, term *ret, Context *ctx);
EtsErrorCode ets_lookup_element_maybe_gc(term ref, term key, size_t pos, term *ret, Context *ctx);
EtsErrorCode ets_delete(term ref, term key, term *ret, Context *ctx);
EtsErrorCode ets_update_counter_maybe_gc(term ref, term key, term value, term pos, term *ret, Context *ctx);
EtsErrorCode ets_drop_table(term ref, term *ret, Context *ctx);
#ifdef __cplusplus
}
#endif

#endif
