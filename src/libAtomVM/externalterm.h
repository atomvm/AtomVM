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

#include "term.h"

#define EXTERNAL_TERM_TAG 131

#ifdef __cplusplus
extern "C" {
#endif

enum ExternalTermResult
{
    EXTERNAL_TERM_OK = 0,
    EXTERNAL_TERM_BAD_ARG = 1,
    EXTERNAL_TERM_MALLOC = 2,
    EXTERNAL_TERM_HEAP_ALLOC = 3
};

typedef enum
{
    ExternalTermNoOpts = 0,
    ExternalTermToHeapFragment = 1
} ExternalTermOpts;

/**
 * @brief Gets a term from external term data.
 *
 * @details Deserialize an external term from external format and returns a term.
 * @param external_term the external term that will be deserialized.
 * @param size to allocate for term.
 * @param ctx the context that owns the memory that will be allocated.
 * @param opts if non-zero, use a heap fragment to store the generated
 * terms.  Otherwise, use the heap in the provided context.  Note that when using the
 * context heap, this function may call the GC, if there is insufficient space to
 * store the generated terms.
 * @returns a term.
 */
term externalterm_to_term(
    const void *external_term, size_t size, Context *ctx, ExternalTermOpts opts);

/**
 * @brief Gets a term from external term data, and makes a copy of all data.
 *
 * @details Deserialize an external term from external format and returns a term.
 * @param external_term the external term that will be deserialized.
 * @param size to allocate for term.
 * @param ctx the context that owns the memory that will be allocated.
 * @param opts if non-zero, use a heap fragment to store the generated
 * terms.  Otherwise, use the heap in the provided context.  Note that when using the
 * context heap, this function may call the GC, if there is insufficient space to
 * store the generated terms.
 * @returns a term.
 */
term externalterm_to_term_copy(
    const void *external_term, size_t size, Context *ctx, ExternalTermOpts opts);

/**
 * @brief Create a term from a binary.
 *
 * @details Deserialize a binary term that stores term data in Erlang external term format,
 * and instantiate the serialized terms.  The heap from the context will be used to
 * allocate the instantiated terms.  This function is the complement of externalterm_to_binary.
 * WARNING: This function may call the GC, which may render the input binary invalid.
 * @param ctx the context that owns the memory that will be allocated.
 * @param dst a pointer to a term that will contain the binary encoded term.
 * @param binary the binary.
 * @param bytes_read the number of bytes read from the input binary.
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
 * @param t the term to return as binary.
 * @returns the term deserialized from the input term, or an invalid term, if
 * deserialization fails.
 */
term externalterm_to_binary(Context *ctx, term t);

/**
 * @brief Computes the size required for a external term (tag excluded)
 *
 * @details This function should be called in order to calculate the required buffer size to store
 * a serialized term in external term format. This function doesn't prepend the external term 1 byte
 * tag.
 *
 * @param t the term for which size is calculated
 * @param size the required buffer size (tag excluded)
 * @param glb the global context
 * @returns EXTERNAL_TERM_OK in case of success
 */
enum ExternalTermResult externalterm_compute_external_size_raw(
    term t, size_t *size, GlobalContext *glb);

/**
 * @brief Serialize a term (tag excluded)
 *
 * @details This function serializes in external term format given term, and writes it to the given
 * buffer. This function doesn't prepend the external term 1 byte tag.
 *
 * @param buf the buffer where the external term is written
 * @param t the term that will be serialized
 * @param glb the global context
 * @returns EXTERNAL_TERM_OK in case of success
 */
enum ExternalTermResult externalterm_serialize_term_raw(void *buf, term t, GlobalContext *glb);

/**
 * @brief Computes the size required for a external term
 *
 * @details This function should be called in order to calculate the required buffer size to store
 * a serialized term in external term format.
 *
 * @param t the term for which size is calculated
 * @param size the required buffer size (tag excluded)
 * @param glb the global context
 * @returns EXTERNAL_TERM_OK in case of success
 */
static inline enum ExternalTermResult externalterm_compute_external_size(
    term t, size_t *size, GlobalContext *glb)
{
    size_t raw_size;
    enum ExternalTermResult result = externalterm_compute_external_size_raw(t, &raw_size, glb);
    if (LIKELY(result == EXTERNAL_TERM_OK)) {
        *size = raw_size + 1;
    }
    return result;
}

/**
 * @brief Serialize a term
 *
 * @details This function serializes in external term format given term, and writes it to the given
 * buffer.
 *
 * @param buf the buffer where the external term is written
 * @param t the term that will be serialized
 * @param glb the global context
 * @returns EXTERNAL_TERM_OK in case of success
 */
static inline enum ExternalTermResult externalterm_serialize_term(
    void *buf, term t, GlobalContext *glb)
{
    enum ExternalTermResult result = externalterm_serialize_term_raw((uint8_t *) buf + 1, t, glb);
    if (LIKELY(result == EXTERNAL_TERM_OK)) {
        ((uint8_t *) buf)[0] = EXTERNAL_TERM_TAG;
    }
    return result;
}

#ifdef __cplusplus
}
#endif

#endif
