/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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

/**
 * @file externalterm.h
 * @brief External term deserialization functions
 *
 * @details This header provides external term deserialization functions.
 */

#ifndef _EXTERNALTERM_H_
#define _EXTERNALTERM_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "term.h"

enum ExternalTermResult
{
    EXTERNAL_TERM_OK = 0,
    EXTERNAL_TERM_BAD_ARG = 1,
    EXTERNAL_TERM_MALLOC = 2,
    EXTERNAL_TERM_HEAP_ALLOC = 3
};

/**
 * @brief Gets a term from external term data.
 *
 * @details Deserialize an external term from external format and returns a term.
 * @param external_term the external term that will be deserialized.
 * @param ctx the context that owns the memory that will be allocated.
 * @param use_heap_fragment if non-zero, use a heap fragment to store the generated
 * terms.  Otherwise, use the heap in the provided context.  Note that when using the
 * context heap, this function may call the GC, if there is insufficient space to
 * store the generated terms.
 * @returns a term.
 */
term externalterm_to_term(const void *external_term, size_t size, Context *ctx, int use_heap_fragment);

/**
 * @brief Create a term from a binary.
 *
 * @details Deserialize a binary term that stores term data in Erlang external term format,
 * and instantiate the serialized terms.  The heap from the context will be used to
 * allocate the instantiated terms.  This function is the complement of externalterm_to_binary.
 * WARNING: This function may call the GC, which may render the input binary invalid.
 * @param ctx the context that owns the memory that will be allocated.
 * @param binary the binary
 * @param bytes_read the number of bytes read from the input binary
 * @param num_extra_terms the number of words (terms) to (possibly) allocate space for
 * in the heap, prior to instantiating the destination term (may trigger GC).
 * @returns the term deserialized from the input term, or an invalid term, if
 * deserialization fails.
 */
enum ExternalTermResult externalterm_from_binary(Context *ctx, term *dst, term binary, size_t *bytes_read);

/**
 * @brief Create a binary from a term.
 *
 * @details Serialize a term in Erlang external term format, and store the result in
 * a binary term.  The heap from the context will be used to allocate the hydrated
 * terms.  This function is the complement of externalterm_to_binary.
 * WARNING: This function may call the GC, which may render the input binary invalid.
 * @param ctx the context that owns the memory that will be allocated.
 * @param binary the binary
 * @returns the term deserialized from the input term, or an invalid term, if
 * deserialization fails.
 */
term externalterm_to_binary(Context *ctx, term t);

#ifdef __cplusplus
}
#endif

#endif
