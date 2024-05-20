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

#ifndef _DICTIONARY_H_
#define _DICTIONARY_H_

#include "list.h"
#include "term.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum
{
    DictionaryOk,
    DictionaryMemoryAllocFail
} DictionaryFunctionResult;

struct DictEntry
{
    struct ListHead head;
    term key;
    term value;
};

DictionaryFunctionResult dictionary_put(
    struct ListHead *dict, term key, term value, term *old, GlobalContext *ctx);
DictionaryFunctionResult dictionary_get(
    struct ListHead *dict, term key, term *old, GlobalContext *ctx);
DictionaryFunctionResult dictionary_erase(
    struct ListHead *dict, term key, term *old, GlobalContext *ctx);
void dictionary_destroy(struct ListHead *dict);

#ifdef __cplusplus
}
#endif

#endif
