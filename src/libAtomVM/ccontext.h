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

/**
 * @file ccontext.h
 * @brief C context functions
 *
 * @details This header implements functions for term references handling, term references remain valid after a garbage collection, so they are useful in native C code.
 */

#ifndef _CCONTEXT_H_
#define _CCONTEXT_H_

#include <stdint.h>

#include "context.h"
#include "memory.h"
#include "term.h"

/**
 * A reference to a term that stays valid after a garbage collection.
 */
typedef uintptr_t term_ref;

/**
 * Holds information required to handle term references.
 */
typedef struct CContext
{
    Context *ctx;
    unsigned int terms_count;
} CContext;

/**
 * @brief Initializes a CContext
 *
 * @details Initializes an uninizialed CContext, this function must not be used on an already initialized CContext.
 * @param ccontext the CContext that will be initialized.
 * @param parent_context the Context that owns the memory.
 */
static inline void ccontext_init(struct CContext *ccontext, struct Context *parent_context)
{
    ccontext->ctx = parent_context;
    ccontext->terms_count = 0;
}

/**
 * @brief Releases all term references
 *
 * @details This function must be called before returning from C code or when all the term references are not needed anymore.
 * @param ccontext the current CContext.
 */
static inline void ccontext_release_all_refs(struct CContext *ccontext)
{
    ccontext->ctx->e += ccontext->terms_count;
    ccontext->terms_count = 0;
}

/**
 * @brief Returns a new term reference
 *
 * @details After a garbage collection all terms that are on the native stack will be invalid, this function returns a safe term reference that will remain valid.
 * @warning This function might cause a garbage collection and make existing terms invalid.
 * @param ccontext the current CContext.
 * @param t the term that will be referenced.
 * @return a new term reference.
 */
static inline term_ref ccontext_make_term_ref(struct CContext *ccontext, term t)
{
    Context *ctx = ccontext->ctx;

    if (ctx->heap_ptr > ctx->e - 1) {
        switch (memory_ensure_free(ctx, 1)) {
            case MEMORY_GC_OK:
                break;
            case MEMORY_GC_ERROR_FAILED_ALLOCATION:
                // TODO Improve error handling
                fprintf(stderr, "Failed to allocate additional heap storage: [%s:%i]\n", __FILE__, __LINE__);
                abort();
            case MEMORY_GC_DENIED_ALLOCATION:
                // TODO Improve error handling
                fprintf(stderr, "Not permitted to allocate additional heap storage: [%s:%i]\n", __FILE__, __LINE__);
                abort();
        }
    }

    ctx->e--;
    ccontext->terms_count++;

    *ctx->e = t;
    return ccontext->terms_count;
}

/**
 * @brief Mark a term reference as unused
 *
 * @details This function should be used to tell the GC that a term is not referenced anymore from C code. This function does not replaces ccontext_release_all_refs.
 * @param ccontext the current CContext.
 * @param tref the term reference that will be released.
 */
static inline void ccontext_kill_term_ref(struct CContext *ccontext, term_ref tref)
{
    *(ccontext->ctx->e + ccontext->terms_count - tref) = term_nil();
}

/*
 * @brief Gets the term referenced by a certain term reference
 *
 * @details Returns a term that will be valid until a garabe collection occours, after a garbage collection this function should be used again.
 * @param ccontext the current CContext.
 * @param tref a reference to a term.
 */
static inline term ccontext_get_term(const struct CContext *ccontext, term_ref tref)
{
    return *(ccontext->ctx->e + ccontext->terms_count - tref);
}

#endif
