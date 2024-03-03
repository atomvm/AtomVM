/*
 * This file is part of AtomVM.
 *
 * Copyright 2021 Fred Dushin <fred@dushin.net>
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

#ifndef _REFC_BINARY_H_
#define _REFC_BINARY_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stdlib.h>

#include "list.h"
#include "resources.h"

#ifdef HAVE_PLATFORM_ATOMIC_H
#include "platform_atomic.h"
#endif

#if defined(HAVE_ATOMIC) && !defined(__cplusplus)
#include <stdatomic.h>
#define ATOMIC _Atomic
#else
#define ATOMIC
#endif

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

#ifndef TYPEDEF_GLOBALCONTEXT
#define TYPEDEF_GLOBALCONTEXT
typedef struct GlobalContext GlobalContext;
#endif

struct RefcBinary
{
    struct ListHead head;
    size_t ATOMIC ref_count;
    size_t size;
    struct ResourceType *resource_type; // Resource type or NULL for regular refc binaries.
    uint8_t data[];
};

/**
 * @brief Create a reference-counted resource object outside of the process heap
 *
 * @details This function will create a reference-counted resource object outside of the context heap.
 * A blob will be allocated in the VM memory (e.g., via malloc).  The allocated data will include
 * an internal data structure that includes the data size and reference count.
 * @param size the size of the data to create
 * @param resource_type the resource type, `NULL` for regular refc binaries.
 * @returns a pointer to the out-of-context data.
 */
struct RefcBinary *refc_binary_create_resource(size_t size, struct ResourceType *resource_type);

/**
 * @brief Create a reference-counted binary outside of the process heap
 *
 * @details This function will create a reference-counted binary outside of the context heap.
 * A blob will be allocated in the VM memory (e.g., via malloc).   The allocated data will include
 * an internal data structure that includes the data size and reference count.
 * @param size the size of the data to create
 * @returns a pointer to the out-of-context data.
 */
static inline struct RefcBinary *refc_binary_create_refc(size_t size)
{
    return refc_binary_create_resource(size, NULL);
}

/**
 * @brief get the data of the off-context binary
 *
 * @details Return the data of the off-context binary
 * @param ptr Refc binary returned from memory_create_refc_binary
 */
const char *refc_binary_get_data(const struct RefcBinary *ptr);

/**
 * @brief get the refc binary from its pointer
 * @details This must only be passed the result of `refc_binary_get_data`.
 *
 * @param ptr pointer obtained from `refc_binary_get_data`
 */
struct RefcBinary *refc_binary_from_data(void *ptr);

/**
 * @brief Increment the reference count on the refc binary
 * @param ptr the refc binary
 */
void refc_binary_increment_refcount(struct RefcBinary *ptr);

/**
 * @brief Decrement the reference count on the refc binary
 *
 * @details This function will call `refc_binary_destroy` if the
 * reference count reaches 0.
 * @param ptr the refc binary
 * @param global the global context
 * @return true if the refc binary was free'd; false, otherwise
 */
bool refc_binary_decrement_refcount(struct RefcBinary *ptr, GlobalContext *global);

/**
 * @brief Destroy a refc binary after its reference count reached 0.
 *
 * @details This function will call the destructor if the refc binary is a
 * resource and will free the refc binary.
 * @param refc the binary to destroy
 * @param global the global context
 */
void refc_binary_destroy(struct RefcBinary *refc, struct GlobalContext *global);

/**
 * TODO consider implementing erlang:memory/0,1 instead
 */
term refc_binary_create_binary_info(Context *ctx);

/**
 * @brief Return the total size (in bytes) of all reference counted binaries
 *
 * @return the total size (in bytes) of all reference counted binaries
 */
size_t refc_binary_total_size(Context *ctx);

#ifdef __cplusplus
}
#endif

#endif // _REFC_BINARY_H_
