/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

#ifndef _RP2040_SYS_H_
#define _RP2040_SYS_H_

#include <time.h>

#include <pico/cond.h>

// Because pico sdk uses -gc-section, it is required to add -Wl,-u NAME_nif
// to make sure a given module nif is linked in.

#define REGISTER_NIF_COLLECTION(NAME, INIT_CB, DESTROY_CB, RESOLVE_NIF_CB)  \
    struct NifCollectionDef NAME##_nif_collection_def = {                   \
        .nif_collection_init_cb = INIT_CB,                                  \
        .nif_collection_destroy_cb = DESTROY_CB,                            \
        .nif_collection_resove_nif_cb = RESOLVE_NIF_CB                      \
    };                                                                      \
                                                                            \
    struct NifCollectionDefListItem NAME##_nif_collection_def_list_item = { \
        .def = &NAME##_nif_collection_def                                   \
    };                                                                      \
                                                                            \
    __attribute__((constructor)) void NAME##_nif()                          \
    {                                                                       \
        NAME##_nif_collection_def_list_item.next = nif_collection_list;     \
        nif_collection_list = &NAME##_nif_collection_def_list_item;         \
    }

struct RP2040PlatformData
{
    mutex_t event_poll_mutex;
    cond_t event_poll_cond;
};

typedef void (*nif_collection_init_t)(GlobalContext *global);
typedef void (*nif_collection_destroy_t)(GlobalContext *global);
typedef const struct Nif *(*nif_collection_resolve_nif_t)(const char *name);

struct NifCollectionDef
{
    const nif_collection_init_t nif_collection_init_cb;
    const nif_collection_destroy_t nif_collection_destroy_cb;
    const nif_collection_resolve_nif_t nif_collection_resove_nif_cb;
};

struct NifCollectionDefListItem
{
    struct NifCollectionDefListItem *next;
    const struct NifCollectionDef *const def;
};

extern struct NifCollectionDefListItem *nif_collection_list;

void nif_collection_init_all(GlobalContext *global);
void nif_collection_destroy_all(GlobalContext *global);
const struct Nif *nif_collection_resolve_nif(const char *name);

#endif
