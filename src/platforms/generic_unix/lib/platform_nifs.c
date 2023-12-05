/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Fred Dushin <fred@dushin.net>
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

#include "platform_nifs.h"

#include "atom.h"
#include "defaultatoms.h"
#include "interop.h"
#include "memory.h"
#include "nifs.h"
#include "otp_crypto.h"
#include "otp_net.h"
#include "otp_socket.h"
#include "otp_ssl.h"
#include "platform_defaultatoms.h"
#include "term.h"
#include <stdlib.h>

//#define ENABLE_TRACE
#include "trace.h"

#define VALIDATE_VALUE(value, verify_function) \
    if (UNLIKELY(!verify_function((value)))) { \
        argv[0] = ERROR_ATOM;                  \
        argv[1] = BADARG_ATOM;                 \
        return term_invalid_term();            \
    }

#define RAISE_ERROR(error_type_atom) \
    ctx->x[0] = ERROR_ATOM;          \
    ctx->x[1] = (error_type_atom);   \
    return term_invalid_term();

#if ATOMVM_HAS_MBEDTLS

// declared in otp_crypt
term nif_crypto_strong_rand_bytes(Context *ctx, int argc, term argv[]);

static term nif_atomvm_rand_bytes(Context *ctx, int argc, term argv[])
{
    return nif_crypto_strong_rand_bytes(ctx, argc, argv);
}

static term nif_atomvm_random(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    term ra[1] = { term_from_int(4) };
    term t = nif_atomvm_rand_bytes(ctx, 1, ra);
    if (term_is_invalid_term(t)) {
        return t;
    }
    uint32_t *r = (uint32_t *) term_binary_data(t);
    avm_int_t value = *r;
    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_make_boxed_int(value, &ctx->heap);
}

static const struct Nif atomvm_rand_bytes_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_rand_bytes
};
static const struct Nif atomvm_random_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_random
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
#if ATOMVM_HAS_MBEDTLS
    if (strcmp("atomvm:rand_bytes/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &atomvm_rand_bytes_nif;
    }
    if (strcmp("atomvm:random/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &atomvm_random_nif;
    }
#endif
    if (strcmp("atomvm:platform/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &atomvm_platform_nif;
    }
    const struct Nif *nif = otp_net_nif_get_nif(nifname);
    if (nif) {
        return nif;
    }
    nif = otp_socket_nif_get_nif(nifname);
#if defined ATOMVM_HAS_MBEDTLS
    if (nif) {
        return nif;
    }
    nif = otp_crypto_nif_get_nif(nifname);
    if (nif) {
        return nif;
    }
    nif = otp_ssl_nif_get_nif(nifname);
#endif
    return nif;
}
