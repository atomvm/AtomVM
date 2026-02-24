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
#include "utils.h"

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

/**
 * @brief Result of an external term read operation.
 *
 * Returned by validate and deserialize functions to indicate whether
 * the operation succeeded or the input was malformed.
 */
typedef enum
{
    ExternalTermReadOk,
    ExternalTermReadInvalid
} external_term_read_result_t;

/**
 * @brief Options for external term read operations.
 *
 * Passed to validate and deserialize functions to control their behavior.
 * Reserved for future use; pass \c ExternalTermReadNoOpts for now.
 */
typedef enum
{
    ExternalTermReadNoOpts
} external_term_read_opts_t;

/**
 * @brief Validate a raw external term buffer (tag byte excluded).
 *
 * Like \c externalterm_validate_buf() but expects a buffer without the
 * leading \c EXTERNAL_TERM_TAG byte. \p bytes_read does not account for
 * the tag byte.
 *
 * @see externalterm_validate_buf() for the standard tag-included variant.
 */
external_term_read_result_t externalterm_validate_buf_raw(const void *buf, size_t buf_size,
    external_term_read_opts_t opts, size_t *required_heap, size_t *bytes_read, GlobalContext *glb);

/**
 * @brief Deserialize a raw external term buffer (tag byte excluded) into a term.
 *
 * Like \c externalterm_deserialize_buf() but expects a buffer without the
 * leading \c EXTERNAL_TERM_TAG byte.
 *
 * @warning \c externalterm_validate_buf_raw() MUST be called successfully on
 * the same buffer before calling this function, and the heap must have been
 * grown to accommodate at least the number of words reported by that call.
 * Skipping validation or providing insufficient heap space results in undefined
 * behavior.
 *
 * @see externalterm_deserialize_buf() for the standard tag-included variant.
 */
external_term_read_result_t externalterm_deserialize_buf_raw(const void *buf, size_t buf_size,
    external_term_read_opts_t opts, Heap *heap, term *out_term, GlobalContext *glb);

/**
 * @brief Validate an external term buffer and compute required heap size.
 *
 * Verifies that \p buf contains a well-formed Erlang external term, starting
 * with \c EXTERNAL_TERM_TAG. On success, \p required_heap holds the number of
 * heap words needed by \c externalterm_deserialize_buf() and \p bytes_read
 * holds the total number of bytes consumed (including the tag byte).
 *
 * @warning This function MUST be called and must return \c ExternalTermReadOk
 * before calling \c externalterm_deserialize_buf() on the same buffer.
 * Calling the deserialize function on an unvalidated or invalid buffer results
 * in undefined behavior.
 *
 * @param buf buffer holding the external term, including the leading \c EXTERNAL_TERM_TAG byte
 * @param buf_size size of \p buf in bytes
 * @param opts options for the read operation; pass \c ExternalTermReadNoOpts
 * @param[out] required_heap number of heap words needed to deserialize the term
 * @param[out] bytes_read total number of bytes consumed from \p buf
 * @param glb the global context
 * @return \c ExternalTermReadOk on success, \c ExternalTermReadInvalid if the
 *         buffer does not contain a valid external term
 *
 * @see externalterm_deserialize_buf() to deserialize after a successful validation
 * @see externalterm_validate_buf_raw() for the tag-excluded variant
 */
static inline external_term_read_result_t externalterm_validate_buf(const void *buf,
    size_t buf_size, external_term_read_opts_t opts, size_t *required_heap, size_t *bytes_read,
    GlobalContext *glb)
{
    if (UNLIKELY(buf_size < 1)) {
        return ExternalTermReadInvalid;
    }

    const uint8_t *external_term_buf = (const uint8_t *) buf;
    if (UNLIKELY(external_term_buf[0] != EXTERNAL_TERM_TAG)) {
        return ExternalTermReadInvalid;
    }

    size_t raw_bytes_read;
    external_term_read_result_t res = externalterm_validate_buf_raw(
        external_term_buf + 1, buf_size - 1, opts, required_heap, &raw_bytes_read, glb);
    if (LIKELY(res == ExternalTermReadOk)) {
        *bytes_read = raw_bytes_read + 1;
    }

    return res;
}

/**
 * @brief Deserialize an external term buffer into a term.
 *
 * Instantiates the Erlang external term stored in \p buf, allocating storage
 * from \p heap. \p buf must start with the \c EXTERNAL_TERM_TAG byte.
 *
 * @warning \c externalterm_validate_buf() MUST be called successfully on the
 * same buffer before calling this function, and the heap must have been grown
 * to accommodate at least the number of words reported by that call. Skipping
 * validation or providing insufficient heap space results in undefined behavior.
 *
 * @param buf buffer holding the external term, including the leading \c EXTERNAL_TERM_TAG byte
 * @param buf_size size of \p buf in bytes
 * @param opts options for the read operation; pass \c ExternalTermReadNoOpts
 * @param[in,out] heap heap from which term storage is allocated
 * @param[out] out_term the deserialized term (undefined on error)
 * @param glb the global context
 * @return \c ExternalTermReadOk on success, \c ExternalTermReadInvalid on failure
 *
 * @see externalterm_validate_buf() which MUST be called before this function
 * @see externalterm_deserialize_buf_raw() for the tag-excluded variant
 */
static inline external_term_read_result_t externalterm_deserialize_buf(const void *buf,
    size_t buf_size, external_term_read_opts_t opts, Heap *heap, term *out_term, GlobalContext *glb)
{
    const uint8_t *raw_buf = ((const uint8_t *) buf) + 1;
    return externalterm_deserialize_buf_raw(raw_buf, buf_size - 1, opts, heap, out_term, glb);
}

/**
 * @brief Create a term from a binary.
 *
 * @details Deserialize a binary term that stores term data in Erlang external
 * term format, and instantiate the serialized terms.  The heap from the
 * context will be used to allocate the instantiated terms.  This function is
 * the complement of externalterm_to_binary.
 * WARNING: This function may call the GC, which may render the input binary
 * invalid. See `externalterm_from_binary_with_roots'
 * @param ctx the context that owns the memory that will be allocated.
 * @param binary the binary.
 * @param bytes_read the number of bytes read from the input binary.
 * @returns the term deserialized from the input term, or an invalid term, if
 * deserialization fails.
 */
term externalterm_from_binary(Context *ctx, term binary, size_t *bytes_read);

/**
 * @brief Create a term from a binary.
 *
 * @details Deserialize a binary term that stores term data in Erlang external term format,
 * and instantiate the serialized terms.  The heap from the context will be used to
 * allocate the instantiated terms.  This function is the complement of externalterm_to_binary.
 * WARNING: This function may call the GC, which may render the input binary invalid.
 * @param ctx the context that owns the memory that will be allocated.
 * @param binary_ix offset of the binary in roots
 * @param offset offset in the binary
 * @param bytes_read the number of bytes read from the input binary.
 * @param num_roots number of roots to preserve in case of GC
 * @param roots roots to preserve in case of GC
 * @returns the term deserialized from the input term, or an invalid term, if
 * deserialization fails.
 */
term externalterm_from_binary_with_roots(Context *ctx, size_t binary_ix, size_t offset, size_t *bytes_read, size_t num_roots, term *roots);

/**
 * @brief Gets a term from a const literal (module in flash).
 *
 * @details Deserialize an external term from external format and returns a
 * term. Use a heap fragment to store the generated terms. The heap fragment
 * is appended to the context heap. Atoms and binaries are not copied.
 * @param external_term the const literal buffer that will be deserialized
 * @param size to allocate for term.
 * @param ctx the context that owns the memory that will be allocated.
 * @returns a term.
 */
term externalterm_from_const_literal(const void *external_term, size_t size, Context *ctx);

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
