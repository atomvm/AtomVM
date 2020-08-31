/***************************************************************************
 *   Copyright 2020 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include "dictionary.h"

#include "defaultatoms.h"
#include "list.h"
#include "term.h"

#include <stdlib.h>

static struct DictEntry *dictionary_find(struct ListHead *dictionary, Context *ctx, term key)
{
    struct ListHead *item;
    LIST_FOR_EACH(item, dictionary) {
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
    struct ListHead *entry;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH(entry, tmp, dict) {
        free(entry);
    }
}
