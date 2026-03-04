/*
 * This file is part of AtomVM.
 *
 * Copyright 2024 Fred Dushin <fred@dushin.net>
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

#ifndef _ETS_H_
#define _ETS_H_

#include <stdbool.h>

struct GlobalContext;
struct Context;

#include "synclist.h"
#include "term.h"

#ifdef __cplusplus
extern "C" {
#endif

// NOTE: Ordered set is not currently supported
typedef enum
{
    EtsTableSet,
    EtsTableOrderedSet,
    EtsTableBag,
    EtsTableDuplicateBag
} ets_table_type_t;

typedef enum
{
    EtsTableAccessPrivate,
    EtsTableAccessProtected,
    EtsTableAccessPublic
} ets_table_access_t;

typedef enum
{
    EtsOk,
    EtsKeyExists,
    EtsTableNameExists,
    EtsTupleNotExists,
    EtsBadEntry,
    EtsBadAccess,
    EtsBadIndex,
    EtsAllocationError,
    EtsOverflow
} ets_status_t;

typedef struct Ets
{
    struct SyncList ets_tables;
} Ets;

void ets_init(Ets *ets);
void ets_destroy(Ets *ets, GlobalContext *global);

ets_status_t ets_create_table_maybe_gc(
    term name,
    bool named,
    ets_table_type_t type,
    ets_table_access_t access,
    size_t index,
    term *ret,
    Context *ctx);
void ets_delete_owned_tables(Ets *ets, int32_t process_id, GlobalContext *global);

ets_status_t ets_lookup_maybe_gc(term name_or_ref, term key, term *ret, Context *ctx);
ets_status_t ets_lookup_element_maybe_gc(term name_or_ref, term key, size_t index, term *ret, Context *ctx);
ets_status_t ets_member(term name_or_ref, term key, Context *ctx);
ets_status_t ets_insert(term name_or_ref, term entry, bool as_new, Context *ctx);
ets_status_t ets_update_element(term name_or_ref, term key, term element_spec, term default_tuple, Context *ctx);
ets_status_t ets_update_counter_maybe_gc(term name_or_ref, term key, term op, term default_tuple, term *ret, Context *ctx);
ets_status_t ets_take_maybe_gc(term name_or_ref, term key, term *ret, Context *ctx);
ets_status_t ets_delete(term name_or_ref, term key, Context *ctx);
ets_status_t ets_delete_table(term name_or_ref, Context *ctx);
ets_status_t ets_delete_object(term name_or_ref, term tuple, Context *ctx);

#ifdef __cplusplus
}
#endif

#endif // _ETS_H_
