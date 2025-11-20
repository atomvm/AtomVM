/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Davide Bettio <davide@uninstall.it>
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

#ifndef _ATOM_TABLE_
#define _ATOM_TABLE_

#include <stdbool.h>

#include "atom.h"
#include "utils.h"

#ifdef __cplusplus
extern "C" {
#endif

struct AtomTable;

enum EnsureAtomsOpt
{
    EnsureAtomsNoOpts = 0,
    EnsureLongEncoding = 1
};

enum AtomTableCopyOpt
{
    AtomTableNoOpts = 0,
    AtomTableCopyAtom = 1,
    AtomTableAlreadyExisting = 2
};

enum AtomTableEnsureAtomResult
{
    AtomTableEnsureAtomOk = 0,
    AtomTableEnsureAtomNotFound = -1,
    AtomTableEnsureAtomAllocFail = -2,
    AtomTableEnsureAtomInvalidLen = -3,
};

typedef const void *atom_ref_t;

struct AtomTable *atom_table_new(void);
void atom_table_destroy(struct AtomTable *table);

size_t atom_table_count(struct AtomTable *table);

atom_index_t atom_table_get_index(struct AtomTable *table, AtomString string);
atom_index_t atom_table_get_index_from_cstring(struct AtomTable *table, const char *name);

enum AtomTableEnsureAtomResult atom_table_ensure_atom(struct AtomTable *table, const uint8_t *atom_data, size_t atom_len, enum AtomTableCopyOpt opts, atom_index_t *result) MUST_CHECK;

enum AtomTableEnsureAtomResult atom_table_ensure_atoms(struct AtomTable *table, const void *atoms, size_t count,
    atom_index_t *translate_table, enum EnsureAtomsOpt opts) MUST_CHECK;

bool atom_table_is_equal_to_atom_string(
    struct AtomTable *table, atom_index_t t_atom_index, AtomString atom_string);
int atom_table_cmp_using_atom_index(
    struct AtomTable *table, atom_index_t t_atom_index, atom_index_t other_atom_index);

atom_ref_t atom_table_get_atom_ptr_and_len(struct AtomTable *table, atom_index_t index, size_t *out_len);
bool atom_table_is_atom_ref_ascii(struct AtomTable *table, atom_ref_t atom);

/**
 * @brief Get a pointer to the character data of a given atom.
 *
 * @details returned pointer is not null terminated
 *
 * @param   table atom table
 * @param   index index of the atom to get the representation of
 * @param   out_len on output, size of the character data
 */
const uint8_t *atom_table_get_atom_string(struct AtomTable *table, atom_index_t index, size_t *out_len);

/**
 * @brief Write module:function/arity to the supplied buffer.
 *
 * @details Write module:function/arity to the supplied buffer.  This function will abort
 *          if the written module, function, and arity are longer than the supplied
 *          buffer size.
 * @param   table atom table
 * @param   buf the buffer to write into
 * @param   buf_size the amount of room in the buffer
 * @param   module the module name
 * @param   function the function name
 * @param   arity the function arity
 */
static inline void atom_table_write_mfa(struct AtomTable *table, char *buf, size_t buf_size, atom_index_t module, atom_index_t function, unsigned int arity)
{
    size_t module_name_len;
    const uint8_t *module_name = atom_table_get_atom_string(table, module, &module_name_len);
    size_t function_name_len;
    const uint8_t *function_name = atom_table_get_atom_string(table, function, &function_name_len);
    atom_write_mfa(buf, buf_size, module_name_len, module_name, function_name_len, function_name, arity);
}

#ifdef __cplusplus
}
#endif

#endif
