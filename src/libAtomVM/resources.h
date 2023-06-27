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

#ifndef _RESOURCES_H_
#define _RESOURCES_H_

#include <stdlib.h>

#include "erl_nif.h"
#include "list.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef TYPEDEF_GLOBALCONTEXT
#define TYPEDEF_GLOBALCONTEXT
typedef struct GlobalContext GlobalContext;
#endif

/**
 * @file resources.h
 * @brief Private data structures for nif object resources
 */

/**
 * @brief A resource type.
 * @details we need a reference to the global context as `enif_release_resource`
 * needs to access the synchronized list of refc_binaries
 */
struct ResourceType
{
    struct ListHead head;
    const char *name;
    GlobalContext *global;
    ErlNifResourceDtor *dtor;
};

static inline void resource_type_destroy(struct ResourceType *resource_type)
{
    free((void *) resource_type->name);
    free(resource_type);
}

#ifdef __cplusplus
}
#endif

#endif // _RESOURCES_H_
