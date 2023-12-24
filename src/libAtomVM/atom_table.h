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

#include "atom.h"

#define ATOM_TABLE_NOT_FOUND -1
#define ATOM_TABLE_ALLOC_FAIL -2

struct AtomTable;

enum AtomTableCopyOpt
{
    AtomTableNoOpts = 0,
    AtomTableCopyAtom = 1,
    AtomTableAlreadyExisting = 2
};

typedef const void *atom_ref_t;

struct AtomTable *atom_table_new();
void atom_table_destroy(struct AtomTable *table);

int atom_table_count(struct AtomTable *table);

long atom_table_ensure_atom(struct AtomTable *table, AtomString string, enum AtomTableCopyOpt opts);

// This function is deprecated and it will be removed.
// atom strings should be copied to caller owned buffers.
AtomString atom_table_get_atom_string(struct AtomTable *table, long index);

long atom_table_get_index(struct AtomTable *table, AtomString string);

int atom_table_ensure_atoms(
    struct AtomTable *table, const void *atoms, int count, int *translate_table);

int atom_table_cmp_using_atom_index(
    struct AtomTable *table, int t_atom_index, int other_atom_index);
atom_ref_t atom_table_get_atom_ptr_and_len(struct AtomTable *table, long index, size_t *out_len);
void atom_table_write_bytes(struct AtomTable *table, atom_ref_t atom, size_t buf_len, void *outbuf);
void atom_table_write_cstring(
    struct AtomTable *table, atom_ref_t atom, size_t buf_len, char *outbuf);

#endif
