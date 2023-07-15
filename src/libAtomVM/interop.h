/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
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

#ifndef _INTEROP_H_
#define _INTEROP_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "context.h"
#include "term.h"

typedef enum
{
    InteropOk,
    InteropMemoryAllocFail,
    InteropBadArg
} InteropFunctionResult;

/**
 * An idiomatic macro for marking an AtomStringIntPair table entry as a
 * interop_atom_term_select_int default.
 */
#define SELECT_INT_DEFAULT(i_val) \
    {                             \
        .as_val = NULL, i_val     \
    }

/**
 * A structure to represent atom strings and int pairs. Such as {"\x8" "universe", 42}.
 */
typedef struct
{
    AtomString as_val;
    int i_val;
} AtomStringIntPair;

typedef InteropFunctionResult (*interop_iolist_fold_fun)(term t, void *accum);

char *interop_term_to_string(term t, int *ok);
char *interop_binary_to_string(term binary);
char *interop_list_to_string(term list, int *ok);
char *interop_iolist_to_string(term list, int *ok);
char *interop_atom_to_string(Context *ctx, term atom);
term interop_proplist_get_value(term list, term key);
term interop_proplist_get_value_default(term list, term key, term default_value);
term interop_map_get_value(GlobalContext *glb, term map, term key);
term interop_map_get_value_default(GlobalContext *glb, term map, term key, term default_value);

NO_DISCARD InteropFunctionResult interop_iolist_size(term t, size_t *size);
NO_DISCARD InteropFunctionResult interop_write_iolist(term t, char *p);
NO_DISCARD InteropFunctionResult interop_iolist_fold(term t, interop_iolist_fold_fun fold_fun, void *accum);

/**
 * @brief Finds on a table the first matching atom string.
 *
 * @details Allows to quickly translate atoms to any integer constant. This function is useful for
 * creating switch statements for atom values.
 * A linear search is performed, so table entries should be sorted by frequency.
 * @param table an array AtomStringIntPair structs, teminated with a default entry marked with
 * SELECT_INT_DEFAULT macro.
 * @param atom the atom used for comparison.
 * @param global the global context.
 * @returns the found int value which corresponds to the given atom.
 */
int interop_atom_term_select_int(const AtomStringIntPair *table, term atom, GlobalContext *global);

/**
 * @brief Get a value given a key (as AtomString) from any proplist or map
 *
 * @details This function allows to easily get values from proplists or maps, without poluting the
 * atom table.
 * @param kv any proplist or map.
 * @param key an AtomString, such as ATOM_STR("\x3", "key").
 * @param default_value that is returned in case of missing item.
 * @param glb the global context.
 * @returns the value term in case given key exists, otherwise the default_value.
 */
term interop_kv_get_value_default(term kv, AtomString key, term default_value, GlobalContext *glb);

/**
 * @brief Get a value given a key (as AtomString) from any proplist or map
 *
 * @details This function allows to easily get values from proplists or maps, without poluting the
 * atom table.  This function returns the invalid term if there is no such entry in kv.
 * @param kv any proplist or map.
 * @param key an AtomString, such as ATOM_STR("\x3", "key").
 * @param glb the global context.
 * @returns the value term in case given key exists, otherwise the invalid term.
 */
static inline term interop_kv_get_value(term kv, AtomString key, GlobalContext *glb)
{
    return interop_kv_get_value_default(kv, key, term_invalid_term(), glb);
}

#ifdef __cplusplus
}
#endif

#endif
