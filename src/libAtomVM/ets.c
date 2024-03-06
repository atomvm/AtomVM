/*
 * This file is part of AtomVM.
 *
 * Copyright 2024 Fred Dushin <fred@dushin.nt>
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

#include "ets.h"

#include "context.h"
#include "defaultatoms.h"
#include "ets_hashtable.h"
#include "list.h"
#include "memory.h"

#ifndef AVM_NO_SMP
#define SMP_RDLOCK(table) smp_rwlock_rdlock(table->lock)
#define SMP_WRLOCK(table) smp_rwlock_wrlock(table->lock)
#define SMP_UNLOCK(table) smp_rwlock_unlock(table->lock)
#else
#define SMP_RDLOCK(table)
#define SMP_WRLOCK(table)
#define SMP_UNLOCK(table)
#endif

#ifndef AVM_NO_SMP
#ifndef TYPEDEF_RWLOCK
#define TYPEDEF_RWLOCK
typedef struct RWLock RWLock;
#endif
#endif

struct EtsTable
{
    struct ListHead head;
    uint64_t ref_ticks;
    term name;
    bool is_named;
    int32_t owner_process_id;
    size_t keypos;
    EtsTableType table_type;
    // In the future, we might support rb-trees for sorted sets
    // For this MVP, we only support unsorted sets
    struct EtsHashTable *hashtable;
    EtsAccessType access_type;

#ifndef AVM_NO_SMP
    RWLock *lock;
#endif
};
typedef enum TableAccessType
{
    TableAccessNone,
    TableAccessRead,
    TableAccessWrite
} TableAccessType;

static void ets_delete_all_tables(struct Ets *ets, GlobalContext *global);

static void ets_add_table(struct Ets *ets, struct EtsTable *ets_table)
{
    struct ListHead *ets_tables_list = synclist_wrlock(&ets->ets_tables);

    list_append(ets_tables_list, &ets_table->head);

    synclist_unlock(&ets->ets_tables);
}

static struct EtsTable *ets_get_table_by_ref(struct Ets *ets, uint64_t ref, TableAccessType access_type)
{
    struct ListHead *ets_tables_list = synclist_rdlock(&ets->ets_tables);
    struct ListHead *item;
    struct EtsTable *ret = NULL;
    LIST_FOR_EACH (item, ets_tables_list) {
        struct EtsTable *table = GET_LIST_ENTRY(item, struct EtsTable, head);
        if (table->ref_ticks == ref) {
            switch (access_type) {
                case TableAccessRead:
                    SMP_RDLOCK(table);
                    break;
                case TableAccessWrite:
                    SMP_WRLOCK(table);
                    break;
                default:
                    break;
            }
            ret = table;
            break;
        }
    }
    synclist_unlock(&ets->ets_tables);
    return ret;
}

static struct EtsTable *ets_get_table_by_name(struct Ets *ets, term name, TableAccessType access_type)
{
    struct ListHead *ets_tables_list = synclist_rdlock(&ets->ets_tables);
    struct ListHead *item;
    struct EtsTable *ret = NULL;
    LIST_FOR_EACH (item, ets_tables_list) {
        struct EtsTable *table = GET_LIST_ENTRY(item, struct EtsTable, head);
        if (table->is_named && table->name == name) {
            switch (access_type) {
                case TableAccessRead:
                    SMP_RDLOCK(table);
                    break;
                case TableAccessWrite:
                    SMP_WRLOCK(table);
                    break;
                default:
                    break;
            }
            ret = table;
            break;
        }
    }
    synclist_unlock(&ets->ets_tables);
    return ret;
}

void ets_init(struct Ets *ets)
{
    synclist_init(&ets->ets_tables);
}

void ets_destroy(struct Ets *ets, GlobalContext *global)
{
    ets_delete_all_tables(ets, global);
    synclist_destroy(&ets->ets_tables);
}

EtsErrorCode ets_create_table(term name, bool is_named, EtsTableType table_type, EtsAccessType access_type, size_t keypos, term *ret, Context *ctx)
{
    if (is_named) {
        struct EtsTable *ets_table = ets_get_table_by_name(&ctx->global->ets, name, TableAccessNone);
        if (ets_table != NULL) {
            return EtsTableNameInUse;
        }
    }

    struct EtsTable *ets_table = malloc(sizeof(struct EtsTable));
    if (IS_NULL_PTR(ets_table)) {
        return EtsAllocationFailure;
    }

    list_init(&ets_table->head);

    ets_table->name = name;
    ets_table->is_named = is_named;
    ets_table->access_type = access_type;

    ets_table->table_type = table_type;
    struct EtsHashTable *hashtable = ets_hashtable_new();
    if (IS_NULL_PTR(hashtable)) {
        free(ets_table);
        return EtsAllocationFailure;
    }
    ets_table->hashtable = hashtable;

    ets_table->owner_process_id = ctx->process_id;

    uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);
    ets_table->ref_ticks = ref_ticks;

    ets_table->keypos = keypos;

#ifndef AVM_NO_SMP
    ets_table->lock = smp_rwlock_create();
#endif

    if (is_named) {
        *ret = name;
    } else {
        if (UNLIKELY(memory_ensure_free_opt(ctx, REF_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            ets_hashtable_destroy(hashtable, ctx->global);
            free(ets_table);
            return EtsAllocationFailure;
        }
        *ret = term_from_ref_ticks(ref_ticks, &ctx->heap);
    }

    ets_add_table(&ctx->global->ets, ets_table);

    return EtsOk;
}

static void ets_table_destroy(struct EtsTable *table, GlobalContext *global)
{
    SMP_WRLOCK(table);
    ets_hashtable_destroy(table->hashtable, global);
    SMP_UNLOCK(table);

#ifndef AVM_NO_SMP
    smp_rwlock_destroy(table->lock);
#endif

    free(table);
}

typedef bool (*ets_table_filter_pred)(struct EtsTable *table, void *data);

static void ets_delete_tables_internal(struct Ets *ets, ets_table_filter_pred pred, void *data, GlobalContext *global)
{
    struct ListHead *ets_tables_list = synclist_wrlock(&ets->ets_tables);
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, ets_tables_list) {
        struct EtsTable *table = GET_LIST_ENTRY(item, struct EtsTable, head);
        if (pred(table, data)) {
            list_remove(&table->head);
            ets_table_destroy(table, global);
        }
    }
    synclist_unlock(&ets->ets_tables);
}

static bool equal_process_id_pred(struct EtsTable *table, void *data)
{
    int32_t *process_id = (int32_t *) data;
    return table->owner_process_id == *process_id;
}

void ets_delete_owned_tables(struct Ets *ets, int32_t process_id, GlobalContext *global)
{
    ets_delete_tables_internal(ets, equal_process_id_pred, &process_id, global);
}

static bool true_pred(struct EtsTable *table, void *data)
{
    UNUSED(table);
    UNUSED(data);

    return true;
}

static void ets_delete_all_tables(struct Ets *ets, GlobalContext *global)
{
    ets_delete_tables_internal(ets, true_pred, NULL, global);
}

EtsErrorCode ets_insert(term ref, term entry, Context *ctx)
{
    struct EtsTable *ets_table = term_is_atom(ref) ? ets_get_table_by_name(&ctx->global->ets, ref, TableAccessWrite) : ets_get_table_by_ref(&ctx->global->ets, term_to_ref_ticks(ref), TableAccessWrite);
    if (ets_table == NULL) {
        return EtsTableNotFound;
    }

    if (ets_table->access_type != EtsAccessPublic && ets_table->owner_process_id != ctx->process_id) {
        SMP_UNLOCK(ets_table);
        return EtsPermissionDenied;
    }

    if ((size_t) term_get_tuple_arity(entry) < (ets_table->keypos + 1)) {
        SMP_UNLOCK(ets_table);
        return EtsBadEntry;
    }

    Heap *heap = malloc(sizeof(Heap));
    if (IS_NULL_PTR(heap)) {
        SMP_UNLOCK(ets_table);
        return EtsAllocationFailure;
    }
    size_t size = (size_t) memory_estimate_usage(entry);
    if (memory_init_heap(heap, size) != MEMORY_GC_OK) {
        free(heap);
        SMP_UNLOCK(ets_table);
        return EtsAllocationFailure;
    }

    term new_entry = memory_copy_term_tree(heap, entry);
    term key = term_get_tuple_element(new_entry, (int) ets_table->keypos);

    EtsErrorCode ret = EtsOk;
    EtsHashtableErrorCode res = ets_hashtable_insert(ets_table->hashtable, key, new_entry, EtsHashtableAllowOverwrite, heap, ctx->global);
    if (UNLIKELY(res != EtsHashtableOk)) {
        ret = EtsAllocationFailure;
    }

    SMP_UNLOCK(ets_table);

    return ret;
}

EtsErrorCode ets_lookup(term ref, term key, term *ret, Context *ctx)
{
    struct EtsTable *ets_table = term_is_atom(ref) ? ets_get_table_by_name(&ctx->global->ets, ref, TableAccessRead) : ets_get_table_by_ref(&ctx->global->ets, term_to_ref_ticks(ref), TableAccessRead);
    if (ets_table == NULL) {
        return EtsTableNotFound;
    }

    if (ets_table->access_type == EtsAccessPrivate && ets_table->owner_process_id != ctx->process_id) {
        SMP_UNLOCK(ets_table);
        return EtsPermissionDenied;
    }

    term res = ets_hashtable_lookup(ets_table->hashtable, key, ets_table->keypos, ctx->global);

    if (term_is_nil(res)) {
        *ret = term_nil();
    } else {

        size_t size = (size_t) memory_estimate_usage(res);
        // alocate [object]
        if (UNLIKELY(memory_ensure_free_opt(ctx, size + CONS_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            SMP_UNLOCK(ets_table);
            return EtsAllocationFailure;
        }
        term new_res = memory_copy_term_tree(&ctx->heap, res);
        *ret = term_list_prepend(new_res, term_nil(), &ctx->heap);
    }
    SMP_UNLOCK(ets_table);

    return EtsOk;
}

EtsErrorCode ets_delete(term ref, term key, term *ret, Context *ctx)
{
    struct EtsTable *ets_table = term_is_atom(ref) ? ets_get_table_by_name(&ctx->global->ets, ref, TableAccessRead) : ets_get_table_by_ref(&ctx->global->ets, term_to_ref_ticks(ref), TableAccessRead);
    if (ets_table == NULL) {
        return EtsTableNotFound;
    }

    if (ets_table->access_type != EtsAccessPublic && ets_table->owner_process_id != ctx->process_id) {
        SMP_UNLOCK(ets_table);
        return EtsPermissionDenied;
    }

    bool _res = ets_hashtable_remove(ets_table->hashtable, key, ets_table->keypos, ctx->global);
    UNUSED(_res);

    SMP_UNLOCK(ets_table);
    *ret = TRUE_ATOM;

    return EtsOk;
}
