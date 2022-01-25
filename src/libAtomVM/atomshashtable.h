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

#ifndef _ATOMSHASHTABLE_H_
#define _ATOMSHASHTABLE_H_

#include "atom.h"

struct AtomsHashTable
{
    int capacity;
    int count;
    struct HNode **buckets;
};

struct AtomsHashTable *atomshashtable_new();
int atomshashtable_insert(struct AtomsHashTable *hash_table, AtomString string, unsigned long value);
unsigned long atomshashtable_get_value(const struct AtomsHashTable *hash_table, AtomString string, unsigned long default_value);
int atomshashtable_has_key(const struct AtomsHashTable *hash_table, AtomString string);

#define TO_ATOMSHASHTABLE_VALUE(value) ((unsigned long) (value))

#endif
