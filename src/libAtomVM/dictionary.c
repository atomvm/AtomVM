/*
 * This file is part of AtomVM.
 *
 * Copyright 2020 Davide Bettio <davide@uninstall.it>
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

#include "dictionary.h"

#include "defaultatoms.h"
#include "list.h"
#include "term.h"

#include <stdlib.h>

static DictionaryFunctionResult dictionary_find(
    struct ListHead *dictionary, term key, struct DictEntry **found, GlobalContext *global)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, dictionary) {
        struct DictEntry *entry = GET_LIST_ENTRY(item, struct DictEntry, head);
        TermCompareResult result = term_compare(entry->key, key, TermCompareExact, global);
        if (result == TermEquals) {
            *found = entry;
            return DictionaryOk;
        } else if (UNLIKELY(result == TermCompareMemoryAllocFail)) {
            return DictionaryMemoryAllocFail;
        }
    }

    *found = NULL;
    return DictionaryOk;
}

DictionaryFunctionResult dictionary_put(
    struct ListHead *dict, term key, term value, term *old, GlobalContext *global)
{
    struct DictEntry *entry;
    DictionaryFunctionResult result = dictionary_find(dict, key, &entry, global);
    if (UNLIKELY(result != DictionaryOk)) {
        return result;
    }

    if (entry) {
        *old = entry->value;
        entry->value = value;

    } else {
        entry = malloc(sizeof(struct DictEntry));
        if (IS_NULL_PTR(entry)) {
            return DictionaryMemoryAllocFail;
        }
        entry->key = key;
        entry->value = value;
        list_prepend(dict, &entry->head);

        *old = UNDEFINED_ATOM;
    }

    return DictionaryOk;
}

DictionaryFunctionResult dictionary_get(
    struct ListHead *dict, term key, term *old, GlobalContext *global)
{
    struct DictEntry *entry;
    DictionaryFunctionResult result = dictionary_find(dict, key, &entry, global);
    if (UNLIKELY(result != DictionaryOk)) {
        return result;
    }

    *old = entry ? entry->value : UNDEFINED_ATOM;
    return DictionaryOk;
}

DictionaryFunctionResult dictionary_erase(
    struct ListHead *dict, term key, term *old, GlobalContext *global)
{
    struct DictEntry *entry;
    DictionaryFunctionResult result = dictionary_find(dict, key, &entry, global);
    if (UNLIKELY(result != DictionaryOk)) {
        return result;
    }

    if (!entry) {
        *old = UNDEFINED_ATOM;
        return DictionaryOk;
    }
    *old = entry->value;

    list_remove(&entry->head);
    free(entry);

    return DictionaryOk;
}

void dictionary_destroy(struct ListHead *dict)
{
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, dict) {
        struct DictEntry *entry = GET_LIST_ENTRY(item, struct DictEntry, head);
        free(entry);
    }
}
