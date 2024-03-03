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

#include "erl_nif.h"
#include "term_typedef.h"
#include "utils.h"

// #define DEBUG_HEAP_ALLOC

#include <stdint.h>
#include <stdlib.h>
#ifdef DEBUG_HEAP_ALLOC
#include <stdio.h>
#endif

#define HEAP_NEED_GC_SHRINK_THRESHOLD_COEFF 64
#define MIN_FREE_SPACE_SIZE 16

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

#ifndef TYPEDEF_GLOBALCONTEXT
#define TYPEDEF_GLOBALCONTEXT
typedef struct GlobalContext GlobalContext;
#endif

enum MemoryGCResult
{
    MEMORY_GC_OK = 0,
    MEMORY_GC_ERROR_FAILED_ALLOCATION = 1,
    MEMORY_GC_DENIED_ALLOCATION = 2
};

enum MemoryAllocMode
{
    MEMORY_NO_SHRINK = 0,
    MEMORY_CAN_SHRINK = 1,
    MEMORY_FORCE_SHRINK = 2,
    MEMORY_NO_GC = 3
};

struct HeapFragment;
typedef struct HeapFragment HeapFragment;

struct HeapFragment
{
    HeapFragment *next;
    union
    {
        term mso_list; // root fragment holds mso_list, with heap_end being in Heap
        term *heap_end; // other fragments hold their own heap_end
    };
    term storage[];
};

struct Heap
{
    HeapFragment *root;
    term *heap_start;
    term *heap_ptr;
    term *heap_end;
};

#ifndef TYPEDEF_HEAP
#define TYPEDEF_HEAP
typedef struct Heap Heap;
#endif

// Stack heap macros can be used from driver tasks.

#define BEGIN_WITH_STACK_HEAP(size, name) \
    struct                                \
    {                                     \
        HeapFragment *next;               \
        union                             \
        {                                 \
            term mso_list;                \
            term *heap_end;               \
        };                                \
        term storage[size];               \
    } name##__root__;                     \
    Heap name;                            \
    memory_init_heap_root_fragment(&name, (HeapFragment *) &(name##__root__), size);

#define END_WITH_STACK_HEAP(name, global)                     \
    memory_sweep_mso_list(name.root->mso_list, global, true); \
    if (name.root->next) {                                    \
        memory_destroy_heap_fragment(name.root->next);        \
    }

// mso_list is the first term for message storage
#define STORAGE_MSO_LIST_INDEX 0
#define STORAGE_HEAP_START_INDEX 1

/**
 * @brief Setup heap from its root.
 * Set the `mso_list` to NIL and initialize `heap_ptr`.
 *
 * @param heap heap to initialize.
 * @param root fragment root.
 * @param size capacity of the heap to create, not including the mso_list.
 */
void memory_init_heap_root_fragment(Heap *heap, HeapFragment *root, size_t size);

/**
 * @brief Initialize a root heap.
 *
 * @details This function should be balanced with `memory_destroy_heap` or
 * `memory_destroy_heap_from_task` if the heap is created by a driver task.
 *
 * @param heap heap to initialize.
 * @param size capacity of the heap to create, not including the mso_list.
 * @returns MEMORY_GC_OK or MEMORY_GC_ERROR_FAILED_ALLOCATION depending on the outcome.
 */
enum MemoryGCResult memory_init_heap(Heap *heap, size_t size) MUST_CHECK;

/**
 * @brief return the total memory size of a heap fragment and its children.
 *
 * @param fragment the root fragment to get the size of
 * @returns the size in terms
 */
static inline size_t memory_heap_fragment_memory_size(const HeapFragment *fragment)
{
    size_t result = 0;
    do {
        result += fragment->heap_end - fragment->storage;
        fragment = fragment->next;
    } while (fragment);
    return result;
}

/**
 * @brief return the size of the youngest generation of the heap.
 * @details in some condition, this function returns the size of a fragment
 * where the stack is not.
 * @param heap the heap to get the youngest size of
 * @returns the size in terms
 */
static inline size_t memory_heap_youngest_size(const Heap *heap)
{
    return heap->heap_end - heap->heap_start;
}

/**
 * @brief return the total memory size of a heap, including fragments.
 *
 * @param heap the heap to get the size of
 * @returns the size in terms
 */
static inline size_t memory_heap_memory_size(const Heap *heap)
{
    size_t result = memory_heap_youngest_size(heap);
    if (heap->root->next) {
        result += memory_heap_fragment_memory_size(heap->root->next);
    }
    return result;
}

/**
 * @brief allocates space for a certain amount of terms on the heap
 *
 * @details allocates space for a certain amount of terms on the heap, GC will be performed when needed, any existing term might be invalid after this call.
 * @param heap heap to initialize.
 * @param size the amount of terms that will be allocated.
 * @returns a pointer to the newly allocated memory block.
 */
static inline MALLOC_LIKE term *memory_heap_alloc(Heap *heap, size_t size)
{
    term *allocated = heap->heap_ptr;
    heap->heap_ptr += size;
#ifdef DEBUG_HEAP_ALLOC
    if (UNLIKELY(heap->heap_ptr > heap->heap_end)) {
        fprintf(stderr, "Tried to allocate heap terms beyond end of heap, size=%u, overflow=%u, heap_size=%u\n", (unsigned) size, (unsigned) (heap->heap_ptr - heap->heap_end), (unsigned) (heap->heap_end - heap->heap_start));
        AVM_ABORT();
    }
#endif

    return allocated;
}

/**
 * @brief copies a term to a destination heap
 *
 * @details deep copies a term to a destination heap, once finished old memory can be freed.
 * @param new_heap the destination heap where terms will be copied.
 * @param t term to copy
 * @returns a new term that is stored on the new heap.
 */
term memory_copy_term_tree(Heap *new_heap, term t);

/**
 * @brief makes sure that the given context has given free memory.
 *
 * @details this function makes sure at least size terms are available. Optionally,
 * it can allocate a fragment or shrink the heap to the specified size, depending
 * on allocation strategy.
 * The function can also be passed roots to update during any garbage collection.
 * @param ctx the target context.
 * @param size needed available memory.
 * @param num_roots number of roots
 * @param roots roots to preserve
 * @param alloc_mode constraint on allocation of additional memory
 */
enum MemoryGCResult memory_ensure_free_with_roots(Context *ctx, size_t size, size_t num_roots, term *roots, enum MemoryAllocMode alloc_mode) MUST_CHECK;

/**
 * @brief makes sure that the given context has given free memory.
 *
 * @details this function makes sure at least size terms are available. Optionally,
 * it can shrink the heap to the specified size, depending on allocation strategy.
 * @param ctx the target context.
 * @param size needed available memory.
 * @param alloc_mode constraint on allocation of additional memory
 */
MUST_CHECK static inline enum MemoryGCResult memory_ensure_free_opt(Context *ctx, size_t size, enum MemoryAllocMode alloc_mode)
{
    return memory_ensure_free_with_roots(ctx, size, 0, NULL, alloc_mode);
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
 * @brief makes sure that the given nif environment has given free memory
 *
 * @details this function makes sure that at least size terms are available. When not available, a new fragment will be allocated.
 * @param env the target environment.
 * @param size needed available memory.
 */
MUST_CHECK enum MemoryGCResult memory_erl_nif_env_ensure_free(ErlNifEnv *env, size_t size);

/**
 * @brief copies a term to a storage, typically for mailbox messages
 *
 * @details deep copies a term to a destination heap, once finished old memory can be freed.
 * @param storage storage for the copied data, should be large enough
 * @param heap_end on output, pointer to the end of the term.
 * @param t term to copy
 * @returns a boxed term pointer to the new term content that is stored in the storage.
 */
term memory_copy_term_tree_to_storage(term *storage, term **heap_end, term t);

/**
 * @brief calculates term memory usage
 *
 * @details perform an used memory calculation using given term as root, shared memory (that is not part of the memory block) is not accounted.
 * @param t root term on which used memory calculation will be performed.
 * @returns used memory terms count in term units output parameter.
 */
unsigned long memory_estimate_usage(term t);

/**
 * @brief append a fragment to a heap. The MSO list is merged. The fragment will then be owned by the heap.
 *
 * @param heap the heap to append the fragment to
 * @param fragment the fragment to add
 * @param mso_list associated mso list or nil
 */
void memory_heap_append_fragment(Heap *heap, HeapFragment *fragment, term mso_list);

/**
 * @brief append a heap to another heap. The MSO list is merged. The fragments
 * of the source heap will be owned by the target heap.
 *
 * @param target the heap to append the heap's fragments to
 * @param source the heap to add
 */
static inline void memory_heap_append_heap(Heap *target, Heap *source)
{
    // Convert root fragment to non-root
    HeapFragment *root = source->root;
    term mso_list = root->mso_list;
    root->heap_end = source->heap_end;
    memory_heap_append_fragment(target, root, mso_list);
}

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
 * @param global the global context
 */
void memory_sweep_mso_list(term mso_list, GlobalContext *global, bool from_task);

/**
 * @brief Destroy a chain of heap fragments.
 *
 * @param fragment fragment to destroy.
 */
static inline void memory_destroy_heap_fragment(HeapFragment *fragment)
{
    while (fragment->next) {
        HeapFragment *next = fragment->next;
        free(fragment);
        fragment = next;
    }
    free(fragment);
}

/**
 * @brief Destroy a root heap. First sweep its mso list.
 *
 * @details This function shall only be called from a scheduler thread
 * (native handler or listener) because it decrements the reference count of
 * refc binaries and may call resource destructors.
 *
 * @param heap the heap to destroy
 * @param global the global context
 */
static inline void memory_destroy_heap(Heap *heap, GlobalContext *global)
{
    memory_sweep_mso_list(heap->root->mso_list, global, false);
    memory_destroy_heap_fragment(heap->root);
}

#ifdef AVM_TASK_DRIVER_ENABLED
/**
 * @brief Destroy a root heap. First sweep its mso list.
 *
 * @details This variant is safer and is meant for driver tasks.
 *
 * @param heap the heap to destroy
 * @param global the global context
 */
static inline void memory_destroy_heap_from_task(Heap *heap, GlobalContext *global)
{
    memory_sweep_mso_list(heap->root->mso_list, global, true);
    memory_destroy_heap_fragment(heap->root);
}
#endif

#ifdef __cplusplus
}
#endif

#endif
