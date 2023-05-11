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
#include "otp_socket.h"
#include "platform_defaultatoms.h"
#include "term.h"
#include <stdlib.h>

#if defined ATOMVM_HAS_OPENSSL
#include <openssl/evp.h>
#include <openssl/rand.h>
#endif

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

#if defined ATOMVM_HAS_OPENSSL

enum crypto_algorithm
{
    CryptoInvalidAlgorithm = 0,
    CryptoMd5,
    CryptoSha1,
    CryptoSha224,
    CryptoSha256,
    CryptoSha384,
    CryptoSha512
};

static const AtomStringIntPair crypto_algorithm_table[] = {
    { ATOM_STR("\x3", "md5"), CryptoMd5 },
    { ATOM_STR("\x3", "sha"), CryptoSha1 },
    { ATOM_STR("\x6", "sha224"), CryptoSha224 },
    { ATOM_STR("\x6", "sha256"), CryptoSha256 },
    { ATOM_STR("\x6", "sha384"), CryptoSha384 },
    { ATOM_STR("\x6", "sha512"), CryptoSha512 },
    SELECT_INT_DEFAULT(CryptoInvalidAlgorithm)
};

const char *get_crypto_algorithm(GlobalContext *global, term type)
{
    enum crypto_algorithm algo = interop_atom_term_select_int(crypto_algorithm_table, type, global);
    switch (algo) {
        case CryptoMd5:
            return "MD5";
        case CryptoSha1:
            return "SHA1";
        case CryptoSha224:
            return "SHA224";
        case CryptoSha256:
            return "SHA256";
        case CryptoSha384:
            return "SHA384";
        case CryptoSha512:
            return "SHA512";
        default:
            return NULL;
    }
}

static InteropFunctionResult hash_fold_fun(term t, void *accum)
{
    EVP_MD_CTX *md_ctx = (EVP_MD_CTX *) accum;
    if (term_is_any_integer(t)) {
        avm_int64_t tmp = term_maybe_unbox_int64(t);
        if (tmp < 0 || tmp > 255) {
            return InteropBadArg;
        }
        uint8_t val = (avm_int64_t) tmp;
        if (!EVP_DigestUpdate(md_ctx, &val, 1)) {
            return InteropBadArg;
        }
    } else if (term_is_binary(t)) {
        if (!EVP_DigestUpdate(md_ctx, (uint8_t *) term_binary_data(t), term_binary_size(t))) {
            return InteropBadArg;
        }
    }
    return InteropOk;
}

static term nif_crypto_hash(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term type = argv[0];
    term data = argv[1];
    VALIDATE_VALUE(type, term_is_atom);

    const char *algorithm = get_crypto_algorithm(ctx->global, type);
    if (IS_NULL_PTR(algorithm)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    const EVP_MD *md = EVP_get_digestbyname(algorithm);
    if (IS_NULL_PTR(md)) {
        fprintf(stderr, "Could not find digest by name: %s\n", algorithm);
        RAISE_ERROR(BADARG_ATOM);
    }
    EVP_MD_CTX *md_ctx = EVP_MD_CTX_new();
    if (IS_NULL_PTR(md_ctx)) {
        fprintf(stderr, "Could not allocate MD context\n");
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    if (UNLIKELY(!EVP_DigestInit_ex(md_ctx, md, NULL))) {
        fprintf(stderr, "Digest init failed for algorithm: %s\n", algorithm);
        EVP_MD_CTX_free(md_ctx);
        RAISE_ERROR(BADARG_ATOM);
    }

    InteropFunctionResult result = interop_chardata_fold(data, hash_fold_fun, NULL, (void *) md_ctx);
    if (result != InteropOk) {
        fprintf(stderr, "Failed hashing input for algorithm: %s\n", algorithm);
        RAISE_ERROR(BADARG_ATOM);
    }

    unsigned char digest[EVP_MAX_MD_SIZE];
    unsigned int md_len;
    if (!EVP_DigestFinal_ex(md_ctx, digest, &md_len)) {
        fprintf(stderr, "Failed finishing hash for algorithm: %s\n", algorithm);
        EVP_MD_CTX_free(md_ctx);
        RAISE_ERROR(BADARG_ATOM);
    }

    EVP_MD_CTX_free(md_ctx);
    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(md_len)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_from_literal_binary(digest, md_len, &ctx->heap, ctx->global);
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
    if (UNLIKELY(status != 1)) {
        free(buf);
        RAISE_ERROR(LOW_ENTROPY_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(n)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term ret = term_from_literal_binary(buf, n, &ctx->heap, ctx->global);
    free(buf);
    return ret;
}

static term nif_openssl_random(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    term ra[1] = { term_from_int(4) };
    term t = nif_openssl_rand_bytes(ctx, 1, ra);
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

static const struct Nif crypto_hash_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_hash
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
    if (strcmp("crypto:hash/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &crypto_hash_nif;
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
    return otp_socket_nif_get_nif(nifname);
}
