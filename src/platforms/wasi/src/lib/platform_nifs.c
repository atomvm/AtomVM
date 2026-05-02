/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include <assert.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <defaultatoms.h>
#include <erl_nif_priv.h>
#include <exportedfunction.h>
#include <globalcontext.h>
#include <nifs.h>
#include <term.h>
#include <utils.h>

#include "platform_defaultatoms.h"
#include "wasi_sys.h"

#ifdef __wasip2__
#include <otp_net.h>
#include <otp_socket.h>
#if ATOMVM_HAS_MBEDTLS
#include <otp_ssl.h>
#endif
#endif

static const struct Nif *get_crypto_nif(const char *nifname, GlobalContext *global);

static term nif_atomvm_platform(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    return WASI_ATOM;
}

static term nif_atomvm_random_impl(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    uint32_t r;
    if (UNLIKELY(wasi_get_random((uint8_t *) &r, sizeof(r)) != 0)) {
        RAISE_ERROR(LOW_ENTROPY_ATOM);
    }

    return term_from_int32((int32_t) r);
}

static term nif_atomvm_random_bytes(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    avm_int_t len = term_to_int(argv[0]);
    if (len < 0 || len > INT_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term result = term_create_empty_binary(len, &ctx->heap, ctx->global);
    uint8_t *data = (uint8_t *) term_binary_data(result);

    if (UNLIKELY(wasi_get_random(data, (size_t) len) != 0)) {
        RAISE_ERROR(LOW_ENTROPY_ATOM);
    }

    return result;
}

static const struct Nif atomvm_platform_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_platform
};

static const struct Nif atomvm_random_bytes_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_random_bytes
};

static const struct Nif atomvm_random_impl_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_random_impl
};

const struct Nif *platform_nifs_get_nif(const char *nifname)
{
    const struct Nif *nif = get_crypto_nif(nifname, NULL);
    if (nif != NULL) {
        return nif;
    }

    if (strcmp("atomvm:platform/0", nifname) == 0) {
        return &atomvm_platform_nif;
    }
    if (strcmp("atomvm:random/0", nifname) == 0) {
        return &atomvm_random_impl_nif;
    }
    if (strcmp("atomvm:random_bytes/1", nifname) == 0) {
        return &atomvm_random_bytes_nif;
    }

#ifdef __wasip2__
    nif = otp_net_nif_get_nif(nifname);
    if (nif != NULL) {
        return nif;
    }
    nif = otp_socket_nif_get_nif(nifname);
    if (nif != NULL) {
        return nif;
    }
#if ATOMVM_HAS_MBEDTLS
    nif = otp_ssl_nif_get_nif(nifname);
    if (nif != NULL) {
        return nif;
    }
#endif
#endif

    return NULL;
}

#if ATOMVM_HAS_MBEDTLS
#include <otp_crypto.h>

static const struct Nif *get_crypto_nif(const char *nifname, GlobalContext *global)
{
    UNUSED(global);
    return otp_crypto_nif_get_nif(nifname);
}
#else
static const struct Nif *get_crypto_nif(const char *nifname, GlobalContext *global)
{
    UNUSED(nifname);
    UNUSED(global);
    return NULL;
}
#endif
