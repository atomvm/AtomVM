/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
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
 * @file ccontext.h
 * @brief C context functions
 *
 * @details This header implements functions for term references handling, term references remain valid after a garbage collection, so they are useful in native C code.
 */

#ifndef _CCONTEXT_H_
#define _CCONTEXT_H_

#include "memory.h"
#include "term.h"

/**
 * A reference to a term that stays valid after a garbage collection.
 */
typedef unsigned long term_ref;

/**
 * Holds information required to handle term references.
 */
struct CContext
{
    Context *ctx;
    unsigned int terms_count;
};

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
term_ref ccontext_make_term_ref(struct CContext *ccontext, term t)
{
    Context *ctx = ccontext->ctx;

    if (ctx->heap_ptr > ctx->e - 1) {
        memory_ensure_free(ctx, 1);
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
term ccontext_get_term(const struct CContext *ccontext, term_ref tref)
{
    return *(ccontext->ctx->e + ccontext->terms_count - tref);
}

#endif
