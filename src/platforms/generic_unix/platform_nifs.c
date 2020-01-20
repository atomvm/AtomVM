/***************************************************************************
 *   Copyright 2019 by Fred Dushin <fred@dushin.net>                       *
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

#include "platform_nifs.h"

#include "atom.h"
#include "defaultatoms.h"
#include "platform_defaultatoms.h"
#include "nifs.h"
#include "memory.h"
#include "term.h"
#include <stdlib.h>

#if defined ATOMVM_HAS_OPENSSL
#include <openssl/md5.h>
#include <openssl/rand.h>
#endif

//#define ENABLE_TRACE
#include "trace.h"

#define VALIDATE_VALUE(value, verify_function) \
if (UNLIKELY(!verify_function((value)))) { \
    argv[0] = ERROR_ATOM; \
    argv[1] = BADARG_ATOM; \
    return term_invalid_term(); \
}

#define RAISE_ERROR(error_type_atom) \
    ctx->x[0] = ERROR_ATOM; \
    ctx->x[1] = (error_type_atom); \
    return term_invalid_term();

#if defined ATOMVM_HAS_OPENSSL
static term nif_openssl_md5(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term data = argv[0];
    VALIDATE_VALUE(data, term_is_binary);

    unsigned char digest[MD5_DIGEST_LENGTH];
    MD5((const unsigned char *) term_binary_data(data), term_binary_size(data), digest);
    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(MD5_DIGEST_LENGTH) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_from_literal_binary(digest, MD5_DIGEST_LENGTH, ctx);
}

static term nif_openssl_rand_bytes(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term t = argv[0];
    VALIDATE_VALUE(t, term_is_any_integer);
    avm_int_t n = term_maybe_unbox_int(t);

    char *buf = malloc(n);
    if (UNLIKELY(IS_NULL_PTR(buf))) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    int status = RAND_bytes((unsigned char *) buf, n);
    if (status != 1) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
        if (RAND_pseudo_bytes((unsigned char *) buf, n) != 1) {
            free(buf);
            RAISE_ERROR(LOW_ENTROPY_ATOM);
        } else {
            fprintf(stderr, "WARNING: Unable to generate cryptographically strong random bytes.  Generated pseudo-random bytes.\n");
        }
    }
#pragma GCC diagnostic pop

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(n) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term ret = term_from_literal_binary(buf, n, ctx);
    free(buf);
    return ret;
}

static term nif_openssl_random(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    term ra[1] = {term_from_int(4)};
    term t = nif_openssl_rand_bytes(ctx, 1, ra);
    if (term_is_invalid_term(t)) {
        return t;
    }
    uint32_t *r = (uint32_t *) term_binary_data(t);
    return term_make_boxed_int(*r, ctx);
}

static const struct Nif openssl_md5_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_openssl_md5
};
static const struct Nif openssl_rand_bytes_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_openssl_rand_bytes
};
static const struct Nif openssl_random_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_openssl_random
};
#endif

static term nif_atomvm_platform(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    return GENERIC_UNIX_ATOM;
}

static const struct Nif atomvm_platform_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_platform
};

const struct Nif *platform_nifs_get_nif(const char *nifname)
{
#if defined ATOMVM_HAS_OPENSSL
    if (strcmp("erlang:md5/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &openssl_md5_nif;
    }
    if (strcmp("atomvm:rand_bytes/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &openssl_rand_bytes_nif;
    }
    if (strcmp("atomvm:random/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &openssl_random_nif;
    }
#endif
    if (strcmp("atomvm:platform/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &atomvm_platform_nif;
    }
    return NULL;
}
