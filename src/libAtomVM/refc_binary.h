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

#ifdef __cplusplus
extern "C" {
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
    // For resources (resource_type != NULL), this word is packed:
    //   [dying:1 | monitor_refc:7/15 | ref_count:24/48]
    // (7+24 on 32-bit, 15+48 on 64-bit).
    size_t ATOMIC ref_count;
    size_t size;
    struct ResourceType *resource_type; // Resource type or NULL for regular refc binaries.
    uint8_t data[];
};

#define REFC_COUNT_BITS (sizeof(size_t) * 6)
#define REFC_COUNT_MASK (((size_t) 1 << REFC_COUNT_BITS) - 1)
#define REFC_MONITOR_INC ((size_t) 1 << REFC_COUNT_BITS)
#define REFC_DYING_FLAG ((size_t) 1 << (sizeof(size_t) * 8 - 1))
#define REFC_MONITOR_MASK (~REFC_COUNT_MASK & ~REFC_DYING_FLAG)
#define REFC_MONITOR_MAX ((REFC_DYING_FLAG >> REFC_COUNT_BITS) - 1)

#ifndef __cplusplus
#if !defined(AVM_NO_SMP) && !defined(HAVE_ATOMIC) && !defined(HAVE_PLATFORM_ATOMIC_H)
#error "SMP build requires either C11 atomics (HAVE_ATOMIC) or a platform atomic header (HAVE_PLATFORM_ATOMIC_H)"
#endif

static inline size_t refc_binary_sub_refcount(struct RefcBinary *refc, size_t delta)
{
#if defined(AVM_NO_SMP)
    return (refc->ref_count -= delta);
#elif defined(HAVE_ATOMIC)
    return atomic_fetch_sub(&refc->ref_count, delta) - delta;
#else // HAVE_PLATFORM_ATOMIC_H guaranteed by #error above
    return smp_atomic_fetch_sub_size(&refc->ref_count, delta) - delta;
#endif
}

static inline void refc_binary_add_refcount(struct RefcBinary *refc, size_t delta)
{
#if defined(AVM_NO_SMP)
    refc->ref_count += delta;
#elif defined(HAVE_ATOMIC)
    atomic_fetch_add(&refc->ref_count, delta);
#else // HAVE_PLATFORM_ATOMIC_H guaranteed by #error above
    smp_atomic_fetch_add_size(&refc->ref_count, delta);
#endif
}

static inline void refc_binary_or_refcount(struct RefcBinary *refc, size_t mask)
{
#if defined(AVM_NO_SMP)
    refc->ref_count |= mask;
#elif defined(HAVE_ATOMIC)
    atomic_fetch_or(&refc->ref_count, mask);
#else // HAVE_PLATFORM_ATOMIC_H guaranteed by #error above
    smp_atomic_fetch_or_size(&refc->ref_count, mask);
#endif
}

static inline bool refc_binary_cas_refcount(struct RefcBinary *refc, size_t *expected, size_t desired)
{
#if defined(AVM_NO_SMP)
    if (refc->ref_count == *expected) {
        refc->ref_count = desired;
        return true;
    }
    *expected = refc->ref_count;
    return false;
#elif defined(HAVE_ATOMIC)
    return atomic_compare_exchange_strong(&refc->ref_count, expected, desired);
#else // HAVE_PLATFORM_ATOMIC_H guaranteed by #error above
    return ATOMIC_COMPARE_EXCHANGE_WEAK_INT(&refc->ref_count, expected, desired);
#endif
}
#endif // __cplusplus

/**
 * @brief Remove the refcount
 *
 * @param refc the resource binary to get the ref count of
 * @return the ref count
 */
static inline size_t refc_binary_get_refcount(const struct RefcBinary *refc)
{
    return refc->resource_type ? (refc->ref_count & REFC_COUNT_MASK) : refc->ref_count;
}

/**
 * @brief Unmark, remove from global list, and free a resource RefcBinary.
 *
 * @details Equivalent to resource_unmark_serialized + synclist_remove +
 * refc_binary_destroy. Only valid when resource_type != NULL.
 * @param refc the resource binary to free
 * @param global the global context
 */
void refc_binary_free_resource(struct RefcBinary *refc, GlobalContext *global);

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

/**
 * @brief Dump detailed information about reference counted binaries
 *
 * @details This function prints diagnostic information including the count,
 * total size, and details about the top 5 largest binaries including
 * their first bytes. Used for debugging memory issues.
 * @param ctx the context
 */
COLD_FUNC void refc_binary_dump_info(Context *ctx);

#ifdef __cplusplus
}
#endif

#endif // _REFC_BINARY_H_
