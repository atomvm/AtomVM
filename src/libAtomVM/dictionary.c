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

static struct DictEntry *dictionary_find(struct ListHead *dictionary, Context *ctx, term key)
{
    struct ListHead *item;
    LIST_FOR_EACH (item, dictionary) {
        struct DictEntry *entry = GET_LIST_ENTRY(item, struct DictEntry, head);
        if (term_compare(entry->key, key, ctx) == 0) {
            return entry;
        }
    }

    return NULL;
}

term dictionary_put(struct ListHead *dict, Context *ctx, term key, term value)
{
    struct DictEntry *entry = dictionary_find(dict, ctx, key);
    if (entry) {
        term old = entry->value;
        entry->value = value;

        return old;
    } else {
        entry = malloc(sizeof(struct DictEntry));
        entry->key = key;
        entry->value = value;
        list_prepend(dict, &entry->head);

        return UNDEFINED_ATOM;
    }
}

term dictionary_get(struct ListHead *dict, Context *ctx, term key)
{
    struct DictEntry *entry = dictionary_find(dict, ctx, key);
    return entry ? entry->value : UNDEFINED_ATOM;
}

term dictionary_erase(struct ListHead *dict, Context *ctx, term key)
{
    struct DictEntry *entry = dictionary_find(dict, ctx, key);
    if (!entry) {
        return UNDEFINED_ATOM;
    }
    term old = entry->value;

    list_remove(&entry->head);
    free(entry);

    return old;
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
