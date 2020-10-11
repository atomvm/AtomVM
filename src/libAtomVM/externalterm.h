/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

/**
 * @file externalterm.h
 * @brief External term deserialization functions
 *
 * @details This header provides external term deserialization functions.
 */

#ifndef _EXTERNALTERM_H_
#define _EXTERNALTERM_H_

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
term externalterm_to_term(const void *external_term, Context *ctx, int use_heap_fragment);

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
enum ExternalTermResult externalterm_from_binary(Context *ctx, term *dst, term binary, size_t *bytes_read, size_t num_extra_terms);

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

#endif
