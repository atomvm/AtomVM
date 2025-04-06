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
#include "overflow_helpers.h"
#include "term.h"

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

EtsErrorCode ets_create_table_maybe_gc(term name, bool is_named, EtsTableType table_type, EtsAccessType access_type, size_t keypos, term *ret, Context *ctx)
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

static EtsErrorCode ets_table_insert(struct EtsTable *ets_table, term entry, Context *ctx)
{
    if (ets_table->access_type != EtsAccessPublic && ets_table->owner_process_id != ctx->process_id) {
        return EtsPermissionDenied;
    }

    size_t keypos = ets_table->keypos;

    if ((size_t) term_get_tuple_arity(entry) < keypos + 1) {
        return EtsBadEntry;
    }

    struct HNode *new_node = ets_hashtable_new_node(entry, keypos);
    if (IS_NULL_PTR(new_node)) {
        return EtsAllocationFailure;
    }

    EtsHashtableErrorCode res = ets_hashtable_insert(ets_table->hashtable, new_node, EtsHashtableAllowOverwrite, ctx->global);
    if (UNLIKELY(res != EtsHashtableOk)) {
        return EtsAllocationFailure;
    }

    return EtsOk;
}

static EtsErrorCode ets_table_insert_list(struct EtsTable *ets_table, term list, Context *ctx)
{
    if (ets_table->access_type != EtsAccessPublic && ets_table->owner_process_id != ctx->process_id) {
        return EtsPermissionDenied;
    }

    term iter = list;
    size_t size = 0;

    while (term_is_nonempty_list(iter)) {
        term tuple = term_get_list_head(iter);
        iter = term_get_list_tail(iter);
        if (!term_is_tuple(tuple) || (size_t) term_get_tuple_arity(tuple) < (ets_table->keypos + 1)) {
            return EtsBadEntry;
        }
        ++size;
    }
    if (!term_is_nil(iter)) {
        return EtsBadEntry;
    }

    struct HNode **nodes = malloc(size * sizeof(struct HNode *));
    if (IS_NULL_PTR(nodes)) {
        return EtsAllocationFailure;
    }

    size_t i = 0;
    while (term_is_nonempty_list(list)) {
        term tuple = term_get_list_head(list);
        nodes[i] = ets_hashtable_new_node(tuple, ets_table->keypos);
        if (IS_NULL_PTR(nodes[i])) {
            ets_hashtable_free_node_array(nodes, i, ctx->global);
            free(nodes);
            return EtsAllocationFailure;
        }
        ++i;
        list = term_get_list_tail(list);
    }

    for (size_t i = 0; i < size; ++i) {

        EtsHashtableErrorCode res = ets_hashtable_insert(ets_table->hashtable, nodes[i], EtsHashtableAllowOverwrite, ctx->global);
        assert(res == EtsHashtableOk);
    }

    free(nodes);
    return EtsOk;
}

EtsErrorCode ets_insert(term name_or_ref, term entry, Context *ctx)
{
    struct EtsTable *ets_table = term_is_atom(name_or_ref) ? ets_get_table_by_name(&ctx->global->ets, name_or_ref, TableAccessWrite) : ets_get_table_by_ref(&ctx->global->ets, term_to_ref_ticks(name_or_ref), TableAccessWrite);
    if (ets_table == NULL) {
        return EtsTableNotFound;
    }

    EtsErrorCode result;
    if (term_is_tuple(entry)) {
        result = ets_table_insert(ets_table, entry, ctx);
    } else if (term_is_list(entry)) {
        result = ets_table_insert_list(ets_table, entry, ctx);
    } else {
        result = EtsBadEntry;
    }

    SMP_UNLOCK(ets_table);

    return result;
}

static EtsErrorCode ets_table_lookup_maybe_gc(struct EtsTable *ets_table, term key, term *ret, Context *ctx, int num_roots, term *roots)
{
    if (ets_table->access_type == EtsAccessPrivate && ets_table->owner_process_id != ctx->process_id) {
        return EtsPermissionDenied;
    }

    term res = ets_hashtable_lookup(ets_table->hashtable, key, ets_table->keypos, ctx->global);

    if (term_is_nil(res)) {
        *ret = term_nil();
    } else {

        size_t size = (size_t) memory_estimate_usage(res);
        // allocate [object]
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, size + CONS_SIZE, num_roots, roots, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            return EtsAllocationFailure;
        }
        term new_res = memory_copy_term_tree(&ctx->heap, res);
        *ret = term_list_prepend(new_res, term_nil(), &ctx->heap);
    }

    return EtsOk;
}

EtsErrorCode ets_lookup_maybe_gc(term name_or_ref, term key, term *ret, Context *ctx)
{
    struct EtsTable *ets_table = term_is_atom(name_or_ref) ? ets_get_table_by_name(&ctx->global->ets, name_or_ref, TableAccessRead) : ets_get_table_by_ref(&ctx->global->ets, term_to_ref_ticks(name_or_ref), TableAccessRead);
    if (ets_table == NULL) {
        return EtsTableNotFound;
    }

    EtsErrorCode result = ets_table_lookup_maybe_gc(ets_table, key, ret, ctx, 0, NULL);
    SMP_UNLOCK(ets_table);

    return result;
}

EtsErrorCode ets_lookup_element_maybe_gc(term name_or_ref, term key, size_t pos, term *ret, Context *ctx)
{
    if (UNLIKELY(pos == 0)) {
        return EtsBadPosition;
    }

    struct EtsTable *ets_table = term_is_atom(name_or_ref) ? ets_get_table_by_name(&ctx->global->ets, name_or_ref, TableAccessRead) : ets_get_table_by_ref(&ctx->global->ets, term_to_ref_ticks(name_or_ref), TableAccessRead);
    if (ets_table == NULL) {
        return EtsTableNotFound;
    }

    if (ets_table->access_type == EtsAccessPrivate && ets_table->owner_process_id != ctx->process_id) {
        SMP_UNLOCK(ets_table);
        return EtsPermissionDenied;
    }

    term entry = ets_hashtable_lookup(ets_table->hashtable, key, ets_table->keypos, ctx->global);

    if (term_is_nil(entry)) {
        SMP_UNLOCK(ets_table);
        return EtsEntryNotFound;
    }

    if ((size_t) term_get_tuple_arity(entry) < pos) {
        SMP_UNLOCK(ets_table);
        return EtsBadPosition;
    }

    term res = term_get_tuple_element(entry, pos - 1);
    size_t size = (size_t) memory_estimate_usage(res);
    // allocate [object]
    if (UNLIKELY(memory_ensure_free_opt(ctx, size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        SMP_UNLOCK(ets_table);
        return EtsAllocationFailure;
    }
    *ret = memory_copy_term_tree(&ctx->heap, res);
    SMP_UNLOCK(ets_table);

    return EtsOk;
}

static EtsErrorCode ets_table_delete(struct EtsTable *ets_table, term key, term *ret, Context *ctx)
{
    if (ets_table->access_type != EtsAccessPublic && ets_table->owner_process_id != ctx->process_id) {
        return EtsPermissionDenied;
    }

    bool _res = ets_hashtable_remove(ets_table->hashtable, key, ets_table->keypos, ctx->global);
    UNUSED(_res);

    *ret = TRUE_ATOM;
    return EtsOk;
}

EtsErrorCode ets_drop_table(term name_or_ref, term *ret, Context *ctx)
{
    struct EtsTable *ets_table = term_is_atom(name_or_ref)
        ? ets_get_table_by_name(&ctx->global->ets, name_or_ref, TableAccessWrite)
        : ets_get_table_by_ref(&ctx->global->ets, term_to_ref_ticks(name_or_ref), TableAccessWrite);
    if (IS_NULL_PTR(ets_table)) {
        return EtsTableNotFound;
    }
    if (ets_table->access_type != EtsAccessPublic && ets_table->owner_process_id != ctx->process_id) {
        return EtsPermissionDenied;
    }

    struct ListHead *ets_tables_list = synclist_wrlock(&ctx->global->ets.ets_tables);
    UNUSED(ets_tables_list);
    list_remove(&ets_table->head);
    SMP_UNLOCK(ets_table);
    ets_table_destroy(ets_table, ctx->global);
    synclist_unlock(&ctx->global->ets.ets_tables);

    *ret = TRUE_ATOM;
    return EtsOk;
}

EtsErrorCode ets_delete(term name_or_ref, term key, term *ret, Context *ctx)
{
    struct EtsTable *ets_table = term_is_atom(name_or_ref)
        ? ets_get_table_by_name(&ctx->global->ets, name_or_ref, TableAccessRead)
        : ets_get_table_by_ref(&ctx->global->ets, term_to_ref_ticks(name_or_ref), TableAccessRead);
    if (IS_NULL_PTR(ets_table)) {
        return EtsTableNotFound;
    }

    EtsErrorCode res = ets_table_delete(ets_table, key, ret, ctx);

    SMP_UNLOCK(ets_table);
    return res;
}

static bool operation_to_tuple4(term operation, size_t default_pos, term *position, term *increment, term *threshold, term *set_value)
{
    if (term_is_integer(operation)) {
        *increment = operation;
        *position = term_from_int(default_pos);
        *threshold = term_invalid_term();
        *set_value = term_invalid_term();
        return true;
    }

    if (UNLIKELY(!term_is_tuple(operation))) {
        return false;
    }
    int n = term_get_tuple_arity(operation);
    if (UNLIKELY(n != 2 && n != 4)) {
        return false;
    }

    term pos = term_get_tuple_element(operation, 0);
    term incr = term_get_tuple_element(operation, 1);
    if (UNLIKELY(!term_is_integer(pos) || !term_is_integer(incr))) {
        return false;
    }

    if (n == 2) {
        *position = pos;
        *increment = incr;
        *threshold = term_invalid_term();
        *set_value = term_invalid_term();
        return true;
    }

    term tresh = term_get_tuple_element(operation, 2);
    term set_val = term_get_tuple_element(operation, 3);
    if (UNLIKELY(!term_is_integer(tresh) || !term_is_integer(set_val))) {
        return false;
    }

    *position = pos;
    *increment = incr;
    *threshold = tresh;
    *set_value = set_val;
    return true;
}

EtsErrorCode ets_update_counter_maybe_gc(term ref, term key, term operation, term default_value, term *ret, Context *ctx)
{
    struct EtsTable *ets_table = term_is_atom(ref)
        ? ets_get_table_by_name(&ctx->global->ets, ref, TableAccessWrite)
        : ets_get_table_by_ref(&ctx->global->ets, term_to_ref_ticks(ref), TableAccessWrite);
    if (IS_NULL_PTR(ets_table)) {
        return EtsTableNotFound;
    }

    // do not use an invalid term as a root
    term safe_default_value = term_is_invalid_term(default_value) ? term_nil() : default_value;
    term roots[] = { key, operation, safe_default_value };

    term list;
    EtsErrorCode result = ets_table_lookup_maybe_gc(ets_table, key, &list, ctx, 3, roots);
    if (UNLIKELY(result != EtsOk)) {
        SMP_UNLOCK(ets_table);
        return result;
    }

    key = roots[0];
    operation = roots[1];
    default_value = term_is_invalid_term(default_value) ? term_invalid_term() : roots[2];

    term to_insert;
    if (term_is_nil(list)) {
        if (term_is_invalid_term(default_value)) {
            SMP_UNLOCK(ets_table);
            return EtsBadEntry;
        }
        to_insert = default_value;
    } else {
        to_insert = term_get_list_head(list);
    }

    if (UNLIKELY(!term_is_tuple(to_insert))) {
        SMP_UNLOCK(ets_table);
        return EtsBadEntry;
    }
    term position_term;
    term increment_term;
    term threshold_term;
    term set_value_term;
    // +1 to position, +1 to elem after key
    size_t default_pos = (ets_table->keypos + 1) + 1;

    if (UNLIKELY(!operation_to_tuple4(operation, default_pos, &position_term, &increment_term, &threshold_term, &set_value_term))) {
        SMP_UNLOCK(ets_table);
        return EtsBadEntry;
    }
    int arity = term_get_tuple_arity(to_insert);
    avm_int_t position = term_to_int(position_term) - 1;
    if (UNLIKELY(arity <= position || position < 1)) {
        SMP_UNLOCK(ets_table);
        return EtsBadEntry;
    }

    term elem = term_get_tuple_element(to_insert, position);
    if (UNLIKELY(!term_is_integer(elem))) {
        SMP_UNLOCK(ets_table);
        return EtsBadEntry;
    }
    avm_int_t increment = term_to_int(increment_term);
    avm_int_t elem_value;
    if (BUILTIN_ADD_OVERFLOW_INT(increment, term_to_int(elem), &elem_value)) {
        SMP_UNLOCK(ets_table);
        return EtsOverlfow;
    }
    if (!term_is_invalid_term(threshold_term) && !term_is_invalid_term(set_value_term)) {
        avm_int_t threshold = term_to_int(threshold_term);
        avm_int_t set_value = term_to_int(set_value_term);

        if (increment >= 0 && elem_value > threshold) {
            elem_value = set_value;
        } else if (increment < 0 && elem_value < threshold) {
            elem_value = set_value;
        }
    }

    term final_value = term_from_int(elem_value);
    term_put_tuple_element(to_insert, position, final_value);
    EtsErrorCode insert_result = ets_table_insert(ets_table, to_insert, ctx);
    if (insert_result == EtsOk) {
        *ret = final_value;
    }
    SMP_UNLOCK(ets_table);
    return insert_result;
}
