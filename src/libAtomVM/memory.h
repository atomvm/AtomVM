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

#ifndef _MEMORY_H_
#define _MEMORY_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "term_typedef.h"
#include "utils.h"

#include <stdint.h>

#define HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF 64
#define MIN_FREE_SPACE_SIZE 16

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

enum MemoryGCResult
{
    MEMORY_GC_OK = 0,
    MEMORY_GC_ERROR_FAILED_ALLOCATION = 1,
    MEMORY_GC_DENIED_ALLOCATION = 2
};

enum MemoryShrinkMode
{
    MEMORY_NO_SHRINK = 0,
    MEMORY_CAN_SHRINK = 1,
    MEMORY_FORCE_SHRINK = 2
};

/**
 * @brief allocates space for a certain amount of terms on the heap
 *
 * @details allocates space for a certain amount of terms on the heap, GC will be performed when needed, any existing term might be invalid after this call.
 * @param ctx the context that owns the heap.
 * @param size the amount of terms that will be allocated.
 * @returns a pointer to the newly allocated memory block.
 */
MALLOC_LIKE term *memory_heap_alloc(Context *ctx, size_t size);

MALLOC_LIKE term *memory_alloc_heap_fragment(Context *ctx, size_t size);

/**
 * @brief copies a term to a destination heap
 *
 * @details deep copies a term to a destination heap, once finished old memory can be freed.
 * @param new_heap the destination heap where terms will be copied.
 * @returns a new term that is stored on the new heap.
 */
term memory_copy_term_tree(term **new_heap, term t, term *mso_list);

/**
 * @brief makes sure that the given context has given free memory.
 *
 * @details this function makes sure at least size terms are available. Optionally,
 * it can shrink the heap to the specified size, depending on allocation strategy.
 * The function can also be passed roots to update during any garbage collection.
 * @param ctx the target context.
 * @param size needed available memory.
 * @param num_roots number of roots
 * @param roots roots to preserve
 * @param shrink_mode whether the function can or should shrink
 */
enum MemoryGCResult memory_ensure_free_with_roots(Context *ctx, size_t size, size_t num_roots, term *roots, enum MemoryShrinkMode shrink_mode) MUST_CHECK;

/**
 * @brief makes sure that the given context has given free memory.
 *
 * @details this function makes sure at least size terms are available. Optionally,
 * it can shrink the heap to the specified size, depending on allocation strategy.
 * @param ctx the target context.
 * @param size needed available memory.
 * @param shrink_mode whether the function can or should shrink
 */
MUST_CHECK static inline enum MemoryGCResult memory_ensure_free_opt(Context *ctx, size_t size, enum MemoryShrinkMode shrink_mode)
{
    return memory_ensure_free_with_roots(ctx, size, 0, NULL, shrink_mode);
}

/**
 * @brief makes sure that the given context has given free memory
 *
 * @details this function makes sure that at least size terms are available, when not available gc will be performed, any existing term might be invalid after this call.
 * It does not shrink the heap, so if this function is called with a given value N and it is later called with a smaller value n, the actual amount of available free
 * memory is N.
 * @param ctx the target context.
 * @param size needed available memory.
 */
MUST_CHECK static inline enum MemoryGCResult memory_ensure_free(Context *ctx, size_t size)
{
    return memory_ensure_free_opt(ctx, size, MEMORY_NO_SHRINK);
}

/**
 * @brief calculates term memory usage
 *
 * @details perform an used memory calculation using given term as root, shared memory (that is not part of the memory block) is not accounted.
 * @param t root term on which used memory calculation will be performed.
 * @returns used memory terms count in term units output parameter.
 */
unsigned long memory_estimate_usage(term t);

/**
 * @brief Sweep any unreferenced binaries in the "Mark Sweep Object" (MSO) list
 *
 * @details The MSO list is a list of term (references) in a heap.  Currently,
 * the elements of this list are referenced to reference-counted binaries or
 * match binaries that themselves reference reference counted binaries.
 * This function will iterate over the binaries in this list, and decrement
 * the reference count of any elements that have not been marked for move
 * (e.g., into a new heap).  If the reference count reaches 0, then memory associated
 * with the referenced binary will be freed.In a typical GC event, the terms in this list are
 * within in the old heap or potentially in a heap fragment.  However, this
 * function may be called in a copy even, such as in a process spawn, or in
 * the copy of a term to or from a process mailbox.
 * @param mso_list the list of mark-sweep object in a heap "space"
 */
void memory_sweep_mso_list(term mso_list);

#ifdef __cplusplus
}
#endif

#endif
