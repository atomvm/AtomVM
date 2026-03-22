/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Fred Dushin <fred@dushin.net>
 * Copyright 2023-2026 Davide Bettio <davide@uninstall.it>
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

#include <otp_crypto.h>

#include <context.h>
#include <defaultatoms.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <interop.h>
#include <nifs.h>
#include <smp.h>
#include <sys_mbedtls.h>
#include <term.h>
#include <term_typedef.h>

#include <mbedtls/version.h>
#if MBEDTLS_VERSION_NUMBER < 0x04000000
#include <mbedtls/cipher.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>
#include <mbedtls/md5.h>
#include <mbedtls/platform_util.h>
#include <mbedtls/sha1.h>
#include <mbedtls/sha256.h>
#include <mbedtls/sha512.h>
#endif
#if defined(MBEDTLS_VERSION_NUMBER) && (MBEDTLS_VERSION_NUMBER >= 0x03000000)
#include <mbedtls/build_info.h>
#else
#include <mbedtls/config.h>
#endif
#ifdef MBEDTLS_VERSION_C
#include <mbedtls/version.h>
#endif

#ifdef HAVE_PSA_CRYPTO
#include <mbedtls/psa_util.h>
#endif
#if defined(HAVE_PSA_CRYPTO) || defined(MBEDTLS_PSA_CRYPTO_C) || MBEDTLS_VERSION_NUMBER >= 0x04000000
#include <psa/crypto.h>
#endif

#if MBEDTLS_VERSION_NUMBER < 0x04000000 && defined(MBEDTLS_PKCS5_C)
#include <mbedtls/md.h>
#include <mbedtls/pkcs5.h>
#endif

#ifdef HAVE_LIBSODIUM
#include <sodium.h>
#endif

#if MBEDTLS_VERSION_NUMBER >= 0x04000000 || defined(MBEDTLS_PKCS5_C)
#define AVM_HAVE_PBKDF2_HMAC 1
#endif

// mbedtls_ct_memcmp is available in 2.28.x+ and 3.1.x+ (absent in 3.0.x)
#if (MBEDTLS_VERSION_NUMBER >= 0x021C0000 && MBEDTLS_VERSION_NUMBER < 0x03000000) \
    || MBEDTLS_VERSION_NUMBER >= 0x03010000
#include <mbedtls/constant_time.h>
#define AVM_HAVE_MBEDTLS_CT_MEMCMP 1
#endif
// #define ENABLE_TRACE
#include "trace.h"

#if MBEDTLS_VERSION_NUMBER > 0x03060100
#define HAVE_MBEDTLS_ECDSA_RAW_TO_DER 1
#define HAVE_MBEDTLS_ECDSA_DER_TO_RAW 1
#endif

#if defined(HAVE_MBEDTLS_ECDSA_RAW_TO_DER) \
    || (defined(HAVE_LIBSODIUM) && defined(MBEDTLS_PSA_CRYPTO_C))
#define CRYPTO_SIGN_AVAILABLE 1
#endif

#if defined(HAVE_MBEDTLS_ECDSA_DER_TO_RAW) \
    || (defined(HAVE_LIBSODIUM) && defined(MBEDTLS_PSA_CRYPTO_C))
#define CRYPTO_VERIFY_AVAILABLE 1
#endif

#define MAX_MD_SIZE 64

#if defined(HAVE_PSA_CRYPTO) || defined(MBEDTLS_PSA_CRYPTO_C) || MBEDTLS_VERSION_NUMBER >= 0x04000000
static void do_psa_init(void)
{
    if (UNLIKELY(psa_crypto_init() != PSA_SUCCESS)) {
        abort();
    }
}
#endif

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

#if MBEDTLS_VERSION_NUMBER < 0x04000000
static const AtomStringIntPair crypto_algorithm_table[] = {
    { ATOM_STR("\x3", "md5"), CryptoMd5 },
    { ATOM_STR("\x3", "sha"), CryptoSha1 },
    { ATOM_STR("\x6", "sha224"), CryptoSha224 },
    { ATOM_STR("\x6", "sha256"), CryptoSha256 },
    { ATOM_STR("\x6", "sha384"), CryptoSha384 },
    { ATOM_STR("\x6", "sha512"), CryptoSha512 },
    SELECT_INT_DEFAULT(CryptoInvalidAlgorithm)
};
#endif

#define DEFINE_HASH_FOLD(ALGORITHM, SUFFIX)                                                                                          \
    static InteropFunctionResult ALGORITHM##_hash_fold_fun(term t, void *accum)                                                      \
    {                                                                                                                                \
        mbedtls_##ALGORITHM##_context *md_ctx = (mbedtls_##ALGORITHM##_context *) accum;                                             \
        if (term_is_integer(t)) {                                                                                                    \
            avm_int64_t tmp = term_maybe_unbox_int64(t);                                                                             \
            if (tmp < 0 || tmp > 255) {                                                                                              \
                return InteropBadArg;                                                                                                \
            }                                                                                                                        \
            uint8_t val = (uint8_t) tmp;                                                                                             \
            if (UNLIKELY(mbedtls_##ALGORITHM##_update##SUFFIX(md_ctx, &val, 1) != 0)) {                                              \
                return InteropBadArg;                                                                                                \
            }                                                                                                                        \
        } else /* term_is_binary(t) */ {                                                                                             \
            if (UNLIKELY(mbedtls_##ALGORITHM##_update##SUFFIX(md_ctx, (uint8_t *) term_binary_data(t), term_binary_size(t)) != 0)) { \
                return InteropBadArg;                                                                                                \
            }                                                                                                                        \
        }                                                                                                                            \
        return InteropOk;                                                                                                            \
    }

#define DEFINE_HASH_FOLD_NORET(ALGORITHM, SUFFIX)                                                               \
    static InteropFunctionResult ALGORITHM##_hash_fold_fun(term t, void *accum)                                 \
    {                                                                                                           \
        mbedtls_##ALGORITHM##_context *md_ctx = (mbedtls_##ALGORITHM##_context *) accum;                        \
        if (term_is_integer(t)) {                                                                               \
            avm_int64_t tmp = term_maybe_unbox_int64(t);                                                        \
            if (tmp < 0 || tmp > 255) {                                                                         \
                return InteropBadArg;                                                                           \
            }                                                                                                   \
            uint8_t val = (uint8_t) tmp;                                                                        \
            mbedtls_##ALGORITHM##_update##SUFFIX(md_ctx, &val, 1);                                              \
        } else /* term_is_binary(t) */ {                                                                        \
            mbedtls_##ALGORITHM##_update##SUFFIX(md_ctx, (uint8_t *) term_binary_data(t), term_binary_size(t)); \
        }                                                                                                       \
        return InteropOk;                                                                                       \
    }

#define DEFINE_DO_HASH(ALGORITHM, SUFFIX)                                                                              \
    static bool do_##ALGORITHM##_hash(term data, unsigned char *dst)                                                   \
    {                                                                                                                  \
        mbedtls_##ALGORITHM##_context md_ctx;                                                                          \
                                                                                                                       \
        mbedtls_##ALGORITHM##_init(&md_ctx);                                                                           \
        mbedtls_##ALGORITHM##_starts##SUFFIX(&md_ctx);                                                                 \
                                                                                                                       \
        InteropFunctionResult result = interop_chardata_fold(data, ALGORITHM##_hash_fold_fun, NULL, (void *) &md_ctx); \
        if (UNLIKELY(result != InteropOk)) {                                                                           \
            return false;                                                                                              \
        }                                                                                                              \
                                                                                                                       \
        if (UNLIKELY(mbedtls_##ALGORITHM##_finish##SUFFIX(&md_ctx, dst) != 0)) {                                       \
            return false;                                                                                              \
        }                                                                                                              \
                                                                                                                       \
        return true;                                                                                                   \
    }

#define DEFINE_DO_HASH_IS_OTHER(ALGORITHM, SUFFIX, IS_OTHER)                                                           \
    static bool do_##ALGORITHM##_hash_##IS_OTHER(term data, unsigned char *dst)                                        \
    {                                                                                                                  \
        mbedtls_##ALGORITHM##_context md_ctx;                                                                          \
                                                                                                                       \
        mbedtls_##ALGORITHM##_init(&md_ctx);                                                                           \
        mbedtls_##ALGORITHM##_starts##SUFFIX(&md_ctx, IS_OTHER);                                                       \
                                                                                                                       \
        InteropFunctionResult result = interop_chardata_fold(data, ALGORITHM##_hash_fold_fun, NULL, (void *) &md_ctx); \
        if (UNLIKELY(result != InteropOk)) {                                                                           \
            return false;                                                                                              \
        }                                                                                                              \
                                                                                                                       \
        if (UNLIKELY(mbedtls_##ALGORITHM##_finish##SUFFIX(&md_ctx, dst) != 0)) {                                       \
            return false;                                                                                              \
        }                                                                                                              \
                                                                                                                       \
        return true;                                                                                                   \
    }

#define DEFINE_DO_HASH_NORET(ALGORITHM, SUFFIX)                                                                        \
    static bool do_##ALGORITHM##_hash(term data, unsigned char *dst)                                                   \
    {                                                                                                                  \
        mbedtls_##ALGORITHM##_context md_ctx;                                                                          \
                                                                                                                       \
        mbedtls_##ALGORITHM##_init(&md_ctx);                                                                           \
        mbedtls_##ALGORITHM##_starts##SUFFIX(&md_ctx);                                                                 \
                                                                                                                       \
        InteropFunctionResult result = interop_chardata_fold(data, ALGORITHM##_hash_fold_fun, NULL, (void *) &md_ctx); \
        if (UNLIKELY(result != InteropOk)) {                                                                           \
            return false;                                                                                              \
        }                                                                                                              \
                                                                                                                       \
        mbedtls_##ALGORITHM##_finish##SUFFIX(&md_ctx, dst);                                                            \
                                                                                                                       \
        return true;                                                                                                   \
    }

#define DEFINE_DO_HASH_NORET_IS_OTHER(ALGORITHM, SUFFIX, IS_OTHER)                                                     \
    static bool do_##ALGORITHM##_hash_##IS_OTHER(term data, unsigned char *dst)                                        \
    {                                                                                                                  \
        mbedtls_##ALGORITHM##_context md_ctx;                                                                          \
                                                                                                                       \
        mbedtls_##ALGORITHM##_init(&md_ctx);                                                                           \
        mbedtls_##ALGORITHM##_starts##SUFFIX(&md_ctx, IS_OTHER);                                                       \
                                                                                                                       \
        InteropFunctionResult result = interop_chardata_fold(data, ALGORITHM##_hash_fold_fun, NULL, (void *) &md_ctx); \
        if (UNLIKELY(result != InteropOk)) {                                                                           \
            return false;                                                                                              \
        }                                                                                                              \
                                                                                                                       \
        mbedtls_##ALGORITHM##_finish##SUFFIX(&md_ctx, dst);                                                            \
                                                                                                                       \
        return true;                                                                                                   \
    }

#if MBEDTLS_VERSION_NUMBER < 0x04000000
#if MBEDTLS_VERSION_NUMBER >= 0x03000000

// 3.x API: functions return an int that represents errors

DEFINE_HASH_FOLD(md5, )
DEFINE_DO_HASH(md5, )
DEFINE_HASH_FOLD(sha1, )
DEFINE_DO_HASH(sha1, )
DEFINE_HASH_FOLD(sha256, )
DEFINE_DO_HASH_IS_OTHER(sha256, , true)
DEFINE_DO_HASH_IS_OTHER(sha256, , false)
DEFINE_HASH_FOLD(sha512, )
DEFINE_DO_HASH_IS_OTHER(sha512, , true)
DEFINE_DO_HASH_IS_OTHER(sha512, , false)

#elif MBEDTLS_VERSION_NUMBER >= 0x02070000

// 2.x API: functions are suffixed with _ret and return an int that represents errors

DEFINE_HASH_FOLD(md5, _ret)
DEFINE_DO_HASH(md5, _ret)
DEFINE_HASH_FOLD(sha1, _ret)
DEFINE_DO_HASH(sha1, _ret)
DEFINE_HASH_FOLD(sha256, _ret)
DEFINE_DO_HASH_IS_OTHER(sha256, _ret, true)
DEFINE_DO_HASH_IS_OTHER(sha256, _ret, false)
DEFINE_HASH_FOLD(sha512, _ret)
DEFINE_DO_HASH_IS_OTHER(sha512, _ret, true)
DEFINE_DO_HASH_IS_OTHER(sha512, _ret, false)

#else

// 1.x API: functions do not return anything

DEFINE_HASH_FOLD_NORET(md5, )
DEFINE_DO_HASH_NORET(md5, )
DEFINE_HASH_FOLD_NORET(sha1, )
DEFINE_DO_HASH_NORET(sha1, )
DEFINE_HASH_FOLD_NORET(sha256, )
DEFINE_DO_HASH_NORET_IS_OTHER(sha256, , true)
DEFINE_DO_HASH_NORET_IS_OTHER(sha256, , false)
DEFINE_HASH_FOLD_NORET(sha512, )
DEFINE_DO_HASH_NORET_IS_OTHER(sha512, , true)
DEFINE_DO_HASH_NORET_IS_OTHER(sha512, , false)

#endif
#endif

#if MBEDTLS_VERSION_NUMBER >= 0x04000000
static psa_algorithm_t atom_to_psa_hash_alg(term type, GlobalContext *global)
{
    if (type == globalcontext_make_atom(global, ATOM_STR("\x3", "md5"))) {
        return PSA_ALG_MD5;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\x3", "sha"))) {
        return PSA_ALG_SHA_1;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\x6", "sha224"))) {
        return PSA_ALG_SHA_224;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\x6", "sha256"))) {
        return PSA_ALG_SHA_256;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\x6", "sha384"))) {
        return PSA_ALG_SHA_384;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\x6", "sha512"))) {
        return PSA_ALG_SHA_512;
    }
#ifdef PSA_ALG_RIPEMD160
    if (type == globalcontext_make_atom(global, ATOM_STR("\x9", "ripemd160"))) {
        return PSA_ALG_RIPEMD160;
    }
#endif
    return PSA_ALG_NONE;
}

static InteropFunctionResult psa_hash_fold_fun(term t, void *accum)
{
    psa_hash_operation_t *operation = (psa_hash_operation_t *) accum;
    if (term_is_integer(t)) {
        avm_int64_t tmp = term_maybe_unbox_int64(t);
        if (tmp < 0 || tmp > 255) {
            return InteropBadArg;
        }
        uint8_t val = (uint8_t) tmp;
        if (UNLIKELY(psa_hash_update(operation, &val, 1) != PSA_SUCCESS)) {
            return InteropBadArg;
        }
    } else /* term_is_binary(t) */ {
        if (UNLIKELY(psa_hash_update(operation, (uint8_t *) term_binary_data(t), term_binary_size(t)) != PSA_SUCCESS)) {
            return InteropBadArg;
        }
    }
    return InteropOk;
}
#endif

static term nif_crypto_hash(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term type = argv[0];
    VALIDATE_VALUE(type, term_is_atom);
    term data = argv[1];

    unsigned char digest[MAX_MD_SIZE];
    size_t digest_len = 0;

#if MBEDTLS_VERSION_NUMBER >= 0x04000000
    do_psa_init();
    psa_algorithm_t alg = atom_to_psa_hash_alg(type, ctx->global);
    if (alg == PSA_ALG_NONE) {
        TRACE("crypto:hash unknown algorithm\n");
        RAISE_ERROR(BADARG_ATOM);
    }
    digest_len = PSA_HASH_LENGTH(alg);

    psa_hash_operation_t operation = PSA_HASH_OPERATION_INIT;
    psa_status_t status = psa_hash_setup(&operation, alg);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        TRACE("crypto:hash psa_hash_setup failed with status %d for alg 0x%08lx\n", (int) status, (unsigned long) alg);
        RAISE_ERROR(BADARG_ATOM);
    }

    InteropFunctionResult result = interop_chardata_fold(data, psa_hash_fold_fun, NULL, (void *) &operation);
    if (UNLIKELY(result != InteropOk)) {
        psa_hash_abort(&operation);
        RAISE_ERROR(BADARG_ATOM);
    }

    status = psa_hash_finish(&operation, digest, sizeof(digest), &digest_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        psa_hash_abort(&operation);
        RAISE_ERROR(BADARG_ATOM);
    }
#else
    enum crypto_algorithm algo = interop_atom_term_select_int(crypto_algorithm_table, type, ctx->global);
    switch (algo) {
        case CryptoMd5: {
            if (UNLIKELY(!do_md5_hash(data, digest))) {
                RAISE_ERROR(BADARG_ATOM);
            }
            digest_len = 16;
            break;
        }
        case CryptoSha1: {
            if (UNLIKELY(!do_sha1_hash(data, digest))) {
                RAISE_ERROR(BADARG_ATOM);
            }
            digest_len = 20;
            break;
        }
        case CryptoSha224: {
            if (UNLIKELY(!do_sha256_hash_true(data, digest))) {
                RAISE_ERROR(BADARG_ATOM);
            }
            digest_len = 28;
            break;
        }
        case CryptoSha256: {
            if (UNLIKELY(!do_sha256_hash_false(data, digest))) {
                RAISE_ERROR(BADARG_ATOM);
            }
            digest_len = 32;
            break;
        }
        case CryptoSha384: {
            if (UNLIKELY(!do_sha512_hash_true(data, digest))) {
                RAISE_ERROR(BADARG_ATOM);
            }
            digest_len = 48;
            break;
        }
        case CryptoSha512: {
            if (UNLIKELY(!do_sha512_hash_false(data, digest))) {
                RAISE_ERROR(BADARG_ATOM);
            }
            digest_len = 64;
            break;
        }
        default:
            RAISE_ERROR(BADARG_ATOM);
    }
#endif

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(digest_len)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_from_literal_binary(digest, digest_len, &ctx->heap, ctx->global);
}

#if MBEDTLS_VERSION_NUMBER < 0x04000000
static const AtomStringIntPair cipher_table[] = {
    { ATOM_STR("\xB", "aes_128_ecb"), MBEDTLS_CIPHER_AES_128_ECB },
    { ATOM_STR("\xB", "aes_192_ecb"), MBEDTLS_CIPHER_AES_192_ECB },
    { ATOM_STR("\xB", "aes_256_ecb"), MBEDTLS_CIPHER_AES_256_ECB },
    { ATOM_STR("\xB", "aes_128_cbc"), MBEDTLS_CIPHER_AES_128_CBC },
    { ATOM_STR("\xB", "aes_192_cbc"), MBEDTLS_CIPHER_AES_192_CBC },
    { ATOM_STR("\xB", "aes_256_cbc"), MBEDTLS_CIPHER_AES_256_CBC },
    { ATOM_STR("\xE", "aes_128_cfb128"), MBEDTLS_CIPHER_AES_128_CFB128 },
    { ATOM_STR("\xE", "aes_192_cfb128"), MBEDTLS_CIPHER_AES_192_CFB128 },
    { ATOM_STR("\xE", "aes_256_cfb128"), MBEDTLS_CIPHER_AES_256_CFB128 },
    { ATOM_STR("\xB", "aes_128_ctr"), MBEDTLS_CIPHER_AES_128_CTR },
    { ATOM_STR("\xB", "aes_192_ctr"), MBEDTLS_CIPHER_AES_192_CTR },
    { ATOM_STR("\xB", "aes_256_ctr"), MBEDTLS_CIPHER_AES_256_CTR },
    SELECT_INT_DEFAULT(MBEDTLS_CIPHER_NONE)
};

static const AtomStringIntPair padding_table[] = {
    { ATOM_STR("\x4", "none"), MBEDTLS_PADDING_NONE },
    { ATOM_STR("\xC", "pkcs_padding"), MBEDTLS_PADDING_PKCS7 },
    SELECT_INT_DEFAULT(-1)
};
#endif

static void secure_free(void *buf, size_t len)
{
    if (buf) {
        mbedtls_platform_zeroize(buf, len);
        free(buf);
    }
}

static term handle_iodata(term iodata, const void **data, size_t *len, void **allocated_ptr)
{
    *allocated_ptr = NULL;

    if (term_is_binary(iodata)) {
        *data = term_binary_data(iodata);
        *len = term_binary_size(iodata);
        return OK_ATOM;
    } else if (term_is_list(iodata)) {
        InteropFunctionResult result = interop_iolist_size(iodata, len);
        switch (result) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                return OUT_OF_MEMORY_ATOM;
            case InteropBadArg:
                return BADARG_ATOM;
        }
        void *allocated_buf = malloc(*len);
        if (IS_NULL_PTR(allocated_buf)) {
            return OUT_OF_MEMORY_ATOM;
        }
        result = interop_write_iolist(iodata, allocated_buf);
        switch (result) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                free(allocated_buf);
                return OUT_OF_MEMORY_ATOM;
            case InteropBadArg:
                free(allocated_buf);
                return BADARG_ATOM;
        }
        *data = allocated_buf;
        *allocated_ptr = allocated_buf;
        return OK_ATOM;
    } else {
        return BADARG_ATOM;
    }
}

#if MBEDTLS_VERSION_NUMBER >= 0x04000000
static psa_algorithm_t atom_to_psa_cipher_alg(term type, GlobalContext *global, psa_key_type_t *key_type, size_t *key_bits)
{
    if (type == globalcontext_make_atom(global, ATOM_STR("\xB", "aes_128_ecb"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 128;
        return PSA_ALG_ECB_NO_PADDING;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\xB", "aes_192_ecb"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 192;
        return PSA_ALG_ECB_NO_PADDING;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\xB", "aes_256_ecb"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 256;
        return PSA_ALG_ECB_NO_PADDING;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\xB", "aes_128_cbc"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 128;
        return PSA_ALG_CBC_NO_PADDING;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\xB", "aes_192_cbc"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 192;
        return PSA_ALG_CBC_NO_PADDING;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\xB", "aes_256_cbc"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 256;
        return PSA_ALG_CBC_NO_PADDING;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\xE", "aes_128_cfb128"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 128;
        return PSA_ALG_CFB;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\xE", "aes_192_cfb128"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 192;
        return PSA_ALG_CFB;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\xE", "aes_256_cfb128"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 256;
        return PSA_ALG_CFB;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\xB", "aes_128_ctr"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 128;
        return PSA_ALG_CTR;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\xB", "aes_192_ctr"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 192;
        return PSA_ALG_CTR;
    }
    if (type == globalcontext_make_atom(global, ATOM_STR("\xB", "aes_256_ctr"))) {
        *key_type = PSA_KEY_TYPE_AES;
        *key_bits = 256;
        return PSA_ALG_CTR;
    }
    return PSA_ALG_NONE;
}
#else
static bool bool_to_mbedtls_operation(term encrypt_flag, mbedtls_operation_t *operation)
{
    switch (encrypt_flag) {
        case TRUE_ATOM:
            *operation = MBEDTLS_ENCRYPT;
            return true;
        case FALSE_ATOM:
            *operation = MBEDTLS_DECRYPT;
            return true;
        default:
            return false;
    }
}
#endif

static term make_crypto_error_tag(
    const char *file, int line, const char *message, term tag, Context *ctx)
{
    int err_needed_mem = (strlen(file) * CONS_SIZE) + TUPLE_SIZE(2) + (strlen(message) * CONS_SIZE)
        + TUPLE_SIZE(3);

    if (UNLIKELY(memory_ensure_free(ctx, err_needed_mem) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term file_t = interop_bytes_to_list(file, strlen(file), &ctx->heap);
    term file_line_t = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(file_line_t, 0, file_t);
    term_put_tuple_element(file_line_t, 1, term_from_int(line));

    term message_t = interop_bytes_to_list(message, strlen(message), &ctx->heap);

    term err_t = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(err_t, 0, tag);
    term_put_tuple_element(err_t, 1, file_line_t);
    term_put_tuple_element(err_t, 2, message_t);

    return err_t;
}

static term make_crypto_error(const char *file, int line, const char *message, Context *ctx)
{
    return make_crypto_error_tag(file, line, message, BADARG_ATOM, ctx);
}

static term nif_crypto_crypto_one_time(Context *ctx, int argc, term argv[])
{
    bool has_iv = argc == 5;
    term key;
    term iv;
    term data;
    term flag_or_options;
    if (has_iv) {
        key = argv[1];
        iv = argv[2];
        data = argv[3];
        flag_or_options = argv[4];
    } else {
        key = argv[1];
        data = argv[2];
        flag_or_options = argv[3];
    }

    term cipher_term = argv[0];

#if MBEDTLS_VERSION_NUMBER >= 0x04000000
    do_psa_init();
    psa_key_type_t key_type;
    size_t key_bits;
    psa_algorithm_t alg = atom_to_psa_cipher_alg(cipher_term, ctx->global, &key_type, &key_bits);
    if (UNLIKELY(alg == PSA_ALG_NONE)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unknown cipher", ctx));
    }
#else
    mbedtls_cipher_type_t cipher
        = interop_atom_term_select_int(cipher_table, cipher_term, ctx->global);
    if (UNLIKELY(cipher == MBEDTLS_CIPHER_NONE)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unknown cipher", ctx));
    }
#endif

    // from this point onward use `goto raise_error` in order to raise and free all buffers
    term error_atom = UNDEFINED_ATOM;

    void *allocated_key_data = NULL;
    void *allocated_iv_data = NULL;
    void *allocated_data_data = NULL;
    size_t data_size = 0;
    const void *iv_data = NULL;
    size_t iv_len = 0;

    const void *key_data;
    size_t key_len = 0;
    term result_t = handle_iodata(key, &key_data, &key_len, &allocated_key_data);
    if (UNLIKELY(result_t != OK_ATOM)) {
        error_atom = result_t;
        goto raise_error;
    }
    if (has_iv) {
        result_t = handle_iodata(iv, &iv_data, &iv_len, &allocated_iv_data);
        if (UNLIKELY(result_t != OK_ATOM)) {
            error_atom = result_t;
            goto raise_error;
        }
    }

    const void *data_data;
    result_t = handle_iodata(data, &data_data, &data_size, &allocated_data_data);
    if (UNLIKELY(result_t != OK_ATOM)) {
        error_atom = result_t;
        goto raise_error;
    }

#if MBEDTLS_VERSION_NUMBER >= 0x04000000
    bool encrypt = true;
    bool padding_pkcs7 = false;
    psa_key_id_t key_id = 0;
    size_t output_size = 0;
    void *temp_buf = NULL;
    psa_cipher_operation_t operation = PSA_CIPHER_OPERATION_INIT;

    if (term_is_list(flag_or_options)) {
        term encrypt_flag = interop_kv_get_value_default(
            flag_or_options, ATOM_STR("\x7", "encrypt"), UNDEFINED_ATOM, ctx->global);
        if (encrypt_flag == FALSE_ATOM) {
            encrypt = false;
        } else if (encrypt_flag != TRUE_ATOM && encrypt_flag != UNDEFINED_ATOM) {
            error_atom = BADARG_ATOM;
            goto raise_error;
        }

        term padding_term = interop_kv_get_value_default(
            flag_or_options, ATOM_STR("\x7", "padding"), UNDEFINED_ATOM, ctx->global);

        if (padding_term != UNDEFINED_ATOM) {
            if (padding_term == globalcontext_make_atom(ctx->global, ATOM_STR("\xC", "pkcs_padding"))) {
                padding_pkcs7 = true;
            } else if (padding_term != globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "none"))) {
                error_atom = BADARG_ATOM;
                goto raise_error;
            }
        }

    } else {
        if (flag_or_options == FALSE_ATOM) {
            encrypt = false;
        } else if (flag_or_options != TRUE_ATOM) {
            error_atom = make_crypto_error(
                __FILE__, __LINE__, "Options are not a boolean or a proper list", ctx);
            goto raise_error;
        }
    }

    if (padding_pkcs7) {
        if (alg == PSA_ALG_CBC_NO_PADDING) {
            alg = PSA_ALG_CBC_PKCS7;
        } else if (alg == PSA_ALG_ECB_NO_PADDING) {
            // PSA does not support PKCS7 padding with ECB mode
            error_atom = BADARG_ATOM;
            goto raise_error;
        }
    }

    psa_key_attributes_t attributes = PSA_KEY_ATTRIBUTES_INIT;
    psa_set_key_usage_flags(&attributes, encrypt ? PSA_KEY_USAGE_ENCRYPT : PSA_KEY_USAGE_DECRYPT);
    psa_set_key_algorithm(&attributes, alg);
    psa_set_key_type(&attributes, key_type);
    psa_set_key_bits(&attributes, key_bits);

    psa_status_t status = psa_import_key(&attributes, key_data, key_len, &key_id);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        char err_msg[48];
        snprintf(err_msg, sizeof(err_msg), "key import err %d", (int) status);
        error_atom = make_crypto_error(__FILE__, __LINE__, err_msg, ctx);
        goto psa_error;
    }

    output_size = PSA_CIPHER_ENCRYPT_OUTPUT_SIZE(key_type, alg, data_size);
    if (!encrypt) {
        output_size = PSA_CIPHER_DECRYPT_OUTPUT_SIZE(key_type, alg, data_size);
    }
    temp_buf = malloc(output_size);
    if (IS_NULL_PTR(temp_buf)) {
        error_atom = OUT_OF_MEMORY_ATOM;
        goto psa_error;
    }

    size_t output_len;
    if (encrypt) {
        status = psa_cipher_encrypt_setup(&operation, key_id, alg);
    } else {
        status = psa_cipher_decrypt_setup(&operation, key_id, alg);
    }
    if (UNLIKELY(status != PSA_SUCCESS)) {
        char err_msg[48];
        snprintf(err_msg, sizeof(err_msg), "cipher setup err %d", (int) status);
        error_atom = make_crypto_error(__FILE__, __LINE__, err_msg, ctx);
        goto psa_error;
    }

    // PSA rejects IVs for ECB; ignore IV to preserve legacy behavior.
    if (iv_len > 0 && alg != PSA_ALG_ECB_NO_PADDING) {
        status = psa_cipher_set_iv(&operation, iv_data, iv_len);
        if (UNLIKELY(status != PSA_SUCCESS)) {
            char err_msg[24];
            snprintf(err_msg, sizeof(err_msg), "IV err %d", (int) status);
            error_atom = make_crypto_error(__FILE__, __LINE__, err_msg, ctx);
            goto psa_error;
        }
    }

    // For CBC/ECB with no padding, PSA requires block-aligned input.
    // The legacy mbedtls behavior was to process only complete blocks,
    // so we truncate the input to the nearest block boundary for these modes.
    size_t block_size = PSA_BLOCK_CIPHER_BLOCK_LENGTH(key_type);
    size_t process_size = data_size;
    if (alg == PSA_ALG_CBC_NO_PADDING || alg == PSA_ALG_ECB_NO_PADDING) {
        process_size = (data_size / block_size) * block_size;
        if (process_size == 0) {
            // No complete blocks to process
            psa_cipher_abort(&operation);
            psa_destroy_key(key_id);
            secure_free(temp_buf, output_size);
            secure_free(allocated_key_data, key_len);
            secure_free(allocated_iv_data, iv_len);
            secure_free(allocated_data_data, data_size);
            // Return empty binary
            if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(0)) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            return term_from_literal_binary("", 0, &ctx->heap, ctx->global);
        }
    }

    size_t update_len = 0;
    status = psa_cipher_update(&operation, data_data, process_size, temp_buf, output_size, &update_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        char err_msg[24];
        snprintf(err_msg, sizeof(err_msg), "update err %d", (int) status);
        error_atom = make_crypto_error(__FILE__, __LINE__, err_msg, ctx);
        goto psa_error;
    }

    size_t finish_len = 0;
    status = psa_cipher_finish(&operation, (uint8_t *) temp_buf + update_len, output_size - update_len, &finish_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        char err_msg[24];
        snprintf(err_msg, sizeof(err_msg), "finish err %d", (int) status);
        error_atom = make_crypto_error(__FILE__, __LINE__, err_msg, ctx);
        goto psa_error;
    }
    output_len = update_len + finish_len;

    psa_destroy_key(key_id);

    secure_free(allocated_key_data, key_len);
    secure_free(allocated_iv_data, iv_len);
    secure_free(allocated_data_data, data_size);

    int ensure_size = term_binary_heap_size(output_len);
    if (UNLIKELY(memory_ensure_free(ctx, ensure_size) != MEMORY_GC_OK)) {
        secure_free(temp_buf, output_size);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term out = term_from_literal_binary(temp_buf, output_len, &ctx->heap, ctx->global);
    secure_free(temp_buf, output_size);
    return out;

psa_error:
    psa_cipher_abort(&operation);
    if (key_id != 0) {
        psa_destroy_key(key_id);
    }
    secure_free(temp_buf, output_size);
    goto raise_error;
#else
    mbedtls_operation_t operation;
    mbedtls_cipher_padding_t padding = MBEDTLS_PADDING_NONE;
    bool padding_has_been_set = false;

    if (term_is_list(flag_or_options)) {
        term encrypt_flag = interop_kv_get_value_default(
            flag_or_options, ATOM_STR("\x7", "encrypt"), UNDEFINED_ATOM, ctx->global);
        if (UNLIKELY(!bool_to_mbedtls_operation(encrypt_flag, &operation))) {
            error_atom = BADARG_ATOM;
            goto raise_error;
        }

        term padding_term = interop_kv_get_value_default(
            flag_or_options, ATOM_STR("\x7", "padding"), UNDEFINED_ATOM, ctx->global);

        if (padding_term != UNDEFINED_ATOM) {
            padding_has_been_set = true;

            int padding_int = interop_atom_term_select_int(padding_table, padding_term, ctx->global);
            if (UNLIKELY(padding_int < 0)) {
                error_atom = BADARG_ATOM;
                goto raise_error;
            }
            padding = (mbedtls_cipher_padding_t) padding_int;
        }

    } else {
        if (UNLIKELY(!bool_to_mbedtls_operation(flag_or_options, &operation))) {
            error_atom = make_crypto_error(__FILE__, __LINE__, "Options are not a boolean or a proper list", ctx);
            goto raise_error;
        }
    }

    const mbedtls_cipher_info_t *cipher_info = mbedtls_cipher_info_from_type(cipher);

    mbedtls_cipher_context_t cipher_ctx;
    mbedtls_cipher_init(&cipher_ctx);

    void *temp_buf = NULL;
    size_t temp_buf_capacity = 0;

    int source_line;
    int result = mbedtls_cipher_setup(&cipher_ctx, cipher_info);
    if (UNLIKELY(result != 0)) {
        source_line = __LINE__;
        goto mbed_error;
    }

    result = mbedtls_cipher_setkey(&cipher_ctx, key_data, key_len * 8, operation);
    if (UNLIKELY(result != 0)) {
        source_line = __LINE__;
        goto mbed_error;
    }

    // we know that mbedtls supports padding just for CBC, so it makes sense to change to OTP
    // default (none) just for it. However in case a padding is set for other modes let mbedtls
    // decide which error should be raised.
    if (mbedtls_cipher_get_cipher_mode(&cipher_ctx) == MBEDTLS_MODE_CBC || padding_has_been_set) {
        result = mbedtls_cipher_set_padding_mode(&cipher_ctx, padding);
        if (UNLIKELY(result != 0)) {
            source_line = __LINE__;
            goto mbed_error;
        }
    }

    unsigned int block_size = mbedtls_cipher_get_block_size(&cipher_ctx);

    temp_buf_capacity = data_size + block_size;
    temp_buf = malloc(temp_buf_capacity);
    if (IS_NULL_PTR(temp_buf)) {
        error_atom = OUT_OF_MEMORY_ATOM;
        goto raise_error;
    }

    // from this point onward use `mbed_error` in order to raise and free all buffers

    size_t temp_buf_size = temp_buf_capacity;
    result = mbedtls_cipher_crypt(
        &cipher_ctx, iv_data, iv_len, data_data, data_size, temp_buf, &temp_buf_size);
    if (result != 0 && result != MBEDTLS_ERR_CIPHER_FULL_BLOCK_EXPECTED) {
        source_line = __LINE__;
        goto mbed_error;
    }
    mbedtls_cipher_free(&cipher_ctx);

    secure_free(allocated_key_data, key_len);
    secure_free(allocated_iv_data, iv_len);
    free(allocated_data_data);

    int ensure_size = term_binary_heap_size(temp_buf_size);
    if (UNLIKELY(memory_ensure_free(ctx, ensure_size) != MEMORY_GC_OK)) {
        free(temp_buf);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term out = term_from_literal_binary(temp_buf, temp_buf_size, &ctx->heap, ctx->global);

    free(temp_buf);

    return out;

raise_error:
    secure_free(allocated_key_data, key_len);
    secure_free(allocated_iv_data, iv_len);
    free(allocated_data_data);
    RAISE_ERROR(error_atom);

mbed_error:
    mbedtls_cipher_free(&cipher_ctx);
    secure_free(temp_buf, temp_buf_capacity);
    secure_free(allocated_key_data, key_len);
    secure_free(allocated_iv_data, iv_len);
    secure_free(allocated_data_data, data_size);

    char err_msg[24];
    snprintf(err_msg, sizeof(err_msg), "Error %x", -result);
    RAISE_ERROR(make_crypto_error(__FILE__, source_line, err_msg, ctx));
#endif

#if MBEDTLS_VERSION_NUMBER >= 0x04000000
raise_error:
    secure_free(allocated_key_data, key_len);
    secure_free(allocated_iv_data, iv_len);
    secure_free(allocated_data_data, data_size);
    RAISE_ERROR(error_atom);
#endif
}

#ifdef HAVE_PSA_CRYPTO

enum pk_type_t
{
    InvalidPkType = 0,
    Eddh,
    Eddsa,
    Ecdh
};

// not working with latest mbedtls (yet): PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_TWISTED_EDWARDS))
// Also tried Eddsa and failed with bits=255
static const AtomStringIntPair pk_type_table[] = {
    { ATOM_STR("\x4", "eddh"), Eddh },
    { ATOM_STR("\x5", "eddsa"), Eddsa },
    { ATOM_STR("\x4", "ecdh"), Ecdh },
    SELECT_INT_DEFAULT(InvalidPkType)
};

enum pk_param_t
{
    InvalidPkParam = 0,
    X25519,
    X448,
    Ed25519,
    Ed448,
    Secp256r1,
    Secp384r1,
    Secp521r1,
    Secp256k1,
    BrainpoolP256r1,
    BrainpoolP384r1,
    BrainpoolP512r1
};

static const AtomStringIntPair pk_param_table[] = {
    { ATOM_STR("\x6", "x25519"), X25519 },
    { ATOM_STR("\x4", "x448"), X448 },
    { ATOM_STR("\x7", "ed25519"), Ed25519 },
    { ATOM_STR("\x5", "ed448"), Ed448 },

    { ATOM_STR("\x9", "secp256r1"), Secp256r1 },
    { ATOM_STR("\x9", "secp384r1"), Secp384r1 },
    { ATOM_STR("\x9", "secp521r1"), Secp521r1 },
    { ATOM_STR("\x9", "secp256k1"), Secp256k1 },
    { ATOM_STR("\xF", "brainpoolP256r1"), BrainpoolP256r1 },
    { ATOM_STR("\xF", "brainpoolP384r1"), BrainpoolP384r1 },
    { ATOM_STR("\xF", "brainpoolP512r1"), BrainpoolP512r1 },

    SELECT_INT_DEFAULT(InvalidPkParam)
};

struct PsaEccCurveParams
{
    enum pk_param_t pk_param;
    psa_ecc_family_t family;
    size_t key_bits;
};

static const struct PsaEccCurveParams psa_ecc_curve_table[] = {
    { X25519, PSA_ECC_FAMILY_MONTGOMERY, 255 },
    { X448, PSA_ECC_FAMILY_MONTGOMERY, 448 },
    { Ed25519, PSA_ECC_FAMILY_TWISTED_EDWARDS, 255 },
    { Ed448, PSA_ECC_FAMILY_TWISTED_EDWARDS, 448 },
    { Secp256k1, PSA_ECC_FAMILY_SECP_K1, 256 },
    { Secp256r1, PSA_ECC_FAMILY_SECP_R1, 256 },
    { Secp384r1, PSA_ECC_FAMILY_SECP_R1, 384 },
    { Secp521r1, PSA_ECC_FAMILY_SECP_R1, 521 },
    { BrainpoolP256r1, PSA_ECC_FAMILY_BRAINPOOL_P_R1, 256 },
    { BrainpoolP384r1, PSA_ECC_FAMILY_BRAINPOOL_P_R1, 384 },
    { BrainpoolP512r1, PSA_ECC_FAMILY_BRAINPOOL_P_R1, 512 },
};

#define PSA_ECC_CURVE_TABLE_LEN (sizeof(psa_ecc_curve_table) / sizeof(psa_ecc_curve_table[0]))

static const struct PsaEccCurveParams *psa_ecc_curve_table_lookup(enum pk_param_t pk_param)
{
    for (size_t i = 0; i < PSA_ECC_CURVE_TABLE_LEN; i++) {
        if (psa_ecc_curve_table[i].pk_param == pk_param) {
            return &psa_ecc_curve_table[i];
        }
    }
    return NULL;
}

// TODO: MbedTLS PSA Crypto API is expected to add Ed25519/X25519 support in a future version.
// Once that version is widely adopted, we may be able to replace the libsodium backend with
// pure PSA API calls.
#ifdef HAVE_LIBSODIUM
static void do_sodium_init(void)
{
    if (UNLIKELY(sodium_init() < 0)) {
        abort();
    }
}

static term sodium_try_generate_key(
    Context *ctx, enum pk_type_t key_type, enum pk_param_t pk_param, bool *is_handled)
{
    GlobalContext *glb = ctx->global;

    if (key_type == Eddsa && pk_param == Ed25519) {
        *is_handled = true;

        unsigned char seed[crypto_sign_SEEDBYTES];
        unsigned char pk[crypto_sign_PUBLICKEYBYTES];
        unsigned char sk[crypto_sign_SECRETKEYBYTES];

        do_sodium_init();
        randombytes_buf(seed, sizeof seed);

        if (UNLIKELY(crypto_sign_seed_keypair(pk, sk, seed) != 0)) {
            sodium_memzero(seed, sizeof seed);
            sodium_memzero(sk, sizeof sk);
            RAISE_ERROR(
                make_crypto_error(__FILE__, __LINE__, "libsodium Ed25519 keygen failed", ctx));
        }

        if (UNLIKELY(memory_ensure_free(ctx,
                         TERM_BINARY_HEAP_SIZE(sizeof pk) + TERM_BINARY_HEAP_SIZE(sizeof seed)
                             + TUPLE_SIZE(2))
                != MEMORY_GC_OK)) {
            sodium_memzero(seed, sizeof seed);
            sodium_memzero(sk, sizeof sk);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term pub_term = term_from_literal_binary(pk, sizeof pk, &ctx->heap, glb);
        term priv_term = term_from_literal_binary(seed, sizeof seed, &ctx->heap, glb);

        term result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, pub_term);
        term_put_tuple_element(result, 1, priv_term);

        sodium_memzero(seed, sizeof seed);
        sodium_memzero(sk, sizeof sk);
        return result;
    }

    if ((key_type == Eddh || key_type == Ecdh) && pk_param == X25519) {
        *is_handled = true;

        unsigned char sk[crypto_scalarmult_SCALARBYTES];
        unsigned char pk[crypto_scalarmult_BYTES];

        do_sodium_init();
        randombytes_buf(sk, sizeof sk);

        if (UNLIKELY(crypto_scalarmult_base(pk, sk) != 0)) {
            sodium_memzero(sk, sizeof sk);
            RAISE_ERROR(
                make_crypto_error(__FILE__, __LINE__, "libsodium X25519 keygen failed", ctx));
        }

        if (UNLIKELY(memory_ensure_free(ctx,
                         TERM_BINARY_HEAP_SIZE(sizeof pk) + TERM_BINARY_HEAP_SIZE(sizeof sk)
                             + TUPLE_SIZE(2))
                != MEMORY_GC_OK)) {
            sodium_memzero(sk, sizeof sk);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term pub_term = term_from_literal_binary(pk, sizeof pk, &ctx->heap, glb);
        term priv_term = term_from_literal_binary(sk, sizeof sk, &ctx->heap, glb);

        term result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, pub_term);
        term_put_tuple_element(result, 1, priv_term);

        sodium_memzero(sk, sizeof sk);
        return result;
    }

    *is_handled = false;
    return term_invalid_term();
}

static term sodium_try_compute_key(Context *ctx, enum pk_type_t key_type, enum pk_param_t pk_param,
    term pub_key_term, term priv_key_term, bool *is_handled)
{
    GlobalContext *glb = ctx->global;

    if (!((key_type == Eddh || key_type == Ecdh) && pk_param == X25519)) {
        *is_handled = false;
        return term_invalid_term();
    }

    *is_handled = true;

    if (UNLIKELY(!term_is_binary(pub_key_term) || !term_is_binary(priv_key_term))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    const void *pub = term_binary_data(pub_key_term);
    size_t pub_len = term_binary_size(pub_key_term);
    const void *priv = term_binary_data(priv_key_term);
    size_t priv_len = term_binary_size(priv_key_term);

    if (UNLIKELY(pub_len != crypto_scalarmult_BYTES || priv_len != crypto_scalarmult_SCALARBYTES)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    unsigned char shared[crypto_scalarmult_BYTES];

    do_sodium_init();
    if (UNLIKELY(crypto_scalarmult(shared, priv, pub) != 0)) {
        sodium_memzero(shared, sizeof shared);
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Invalid X25519 public key", ctx));
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BINARY_HEAP_SIZE(sizeof shared)) != MEMORY_GC_OK)) {
        sodium_memzero(shared, sizeof shared);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term result = term_from_literal_binary(shared, sizeof shared, &ctx->heap, glb);
    sodium_memzero(shared, sizeof shared);
    return result;
}

static term sodium_try_sign(
    Context *ctx, term alg_term, term digest_term, term data_term, term key_term, bool *is_handled)
{
    GlobalContext *glb = ctx->global;

    *is_handled = false;

    if (!globalcontext_is_term_equal_to_atom_string(glb, alg_term, ATOM_STR("\x5", "eddsa"))) {
        return term_invalid_term();
    }

    // Ed25519 is a PureEdDSA scheme with its own internal hashing,
    // so the digest parameter is ignored (matching OTP behavior).
    UNUSED(digest_term);

    // Extract curve to decide whether sodium handles this (ed25519) or should
    // fall through to the PSA path (e.g. ed448). If the key is too malformed
    // to extract a curve, claim *is_handled since the PSA path cannot produce
    // a meaningful eddsa error.
    if (UNLIKELY(!term_is_nonempty_list(key_term))) {
        *is_handled = true;
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get EDDSA private key", ctx));
    }

    term priv_term = term_get_list_head(key_term);
    term tail = term_get_list_tail(key_term);
    if (UNLIKELY(!term_is_nonempty_list(tail))) {
        *is_handled = true;
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get EDDSA private key", ctx));
    }

    term curve_term = term_get_list_head(tail);
    if (!globalcontext_is_term_equal_to_atom_string(glb, curve_term, ATOM_STR("\x7", "ed25519"))) {
        return term_invalid_term();
    }

    *is_handled = true;

    if (UNLIKELY(!term_is_binary(priv_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get EDDSA private key", ctx));
    }

    const void *seed = term_binary_data(priv_term);
    size_t seed_len = term_binary_size(priv_term);
    if (UNLIKELY(seed_len != crypto_sign_SEEDBYTES)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get EDDSA private key", ctx));
    }

    void *maybe_allocated_data = NULL;
    const void *data = NULL;
    size_t data_len = 0;
    term iodata_result = handle_iodata(data_term, &data, &data_len, &maybe_allocated_data);
    if (UNLIKELY(iodata_result != OK_ATOM)) {
        free(maybe_allocated_data);
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Expected a binary or a list", ctx));
    }

    unsigned char pk[crypto_sign_PUBLICKEYBYTES];
    unsigned char sk[crypto_sign_SECRETKEYBYTES];
    unsigned char sig[crypto_sign_BYTES];
    unsigned long long sig_len = 0;

    do_sodium_init();

    if (UNLIKELY(crypto_sign_seed_keypair(pk, sk, seed) != 0
            || crypto_sign_detached(sig, &sig_len, data, data_len, sk) != 0)) {
        free(maybe_allocated_data);
        sodium_memzero(sk, sizeof sk);
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "libsodium Ed25519 sign failed", ctx));
    }

    free(maybe_allocated_data);
    sodium_memzero(sk, sizeof sk);

    if (UNLIKELY(
            memory_ensure_free(ctx, TERM_BINARY_HEAP_SIZE(crypto_sign_BYTES)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_from_literal_binary(sig, crypto_sign_BYTES, &ctx->heap, glb);
}

static term sodium_try_verify(Context *ctx, term alg_term, term digest_term, term data_term,
    term sig_term, term key_term, bool *is_handled)
{
    GlobalContext *glb = ctx->global;

    *is_handled = false;

    if (!globalcontext_is_term_equal_to_atom_string(glb, alg_term, ATOM_STR("\x5", "eddsa"))) {
        return term_invalid_term();
    }

    // Ed25519 is a PureEdDSA scheme with its own internal hashing,
    // so the digest parameter is ignored (matching OTP behavior).
    UNUSED(digest_term);

    // Extract curve to decide whether sodium handles this (ed25519) or should
    // fall through to the PSA path (e.g. ed448). If the key is too malformed
    // to extract a curve, claim *is_handled since the PSA path cannot produce
    // a meaningful eddsa error.
    if (UNLIKELY(!term_is_nonempty_list(key_term))) {
        *is_handled = true;
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get EDDSA public key", ctx));
    }

    term pub_term = term_get_list_head(key_term);
    term tail = term_get_list_tail(key_term);
    if (UNLIKELY(!term_is_nonempty_list(tail))) {
        *is_handled = true;
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get EDDSA public key", ctx));
    }

    term curve_term = term_get_list_head(tail);
    if (!globalcontext_is_term_equal_to_atom_string(glb, curve_term, ATOM_STR("\x7", "ed25519"))) {
        return term_invalid_term();
    }

    *is_handled = true;

    if (UNLIKELY(!term_is_binary(sig_term) || term_binary_size(sig_term) != crypto_sign_BYTES)) {
        return FALSE_ATOM;
    }

    if (UNLIKELY(!term_is_binary(pub_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get EDDSA public key", ctx));
    }

    const void *pub = term_binary_data(pub_term);
    size_t pub_len = term_binary_size(pub_term);
    if (UNLIKELY(pub_len != crypto_sign_PUBLICKEYBYTES)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get EDDSA public key", ctx));
    }

    void *maybe_allocated_data = NULL;
    const void *data = NULL;
    size_t data_len = 0;
    term iodata_result = handle_iodata(data_term, &data, &data_len, &maybe_allocated_data);
    if (UNLIKELY(iodata_result != OK_ATOM)) {
        free(maybe_allocated_data);
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Expected a binary or a list", ctx));
    }

    const void *sig = term_binary_data(sig_term);

    do_sodium_init();
    int rc = crypto_sign_verify_detached(sig, data, data_len, pub);

    free(maybe_allocated_data);
    return (rc == 0) ? TRUE_ATOM : FALSE_ATOM;
}
#endif /* HAVE_LIBSODIUM */

static term nif_crypto_generate_key(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    GlobalContext *glb = ctx->global;

    enum pk_type_t key_type = interop_atom_term_select_int(pk_type_table, argv[0], glb);
    enum pk_param_t pk_param = interop_atom_term_select_int(pk_param_table, argv[1], glb);

#ifdef HAVE_LIBSODIUM
    bool sodium_handled;
    term sodium_result = sodium_try_generate_key(ctx, key_type, pk_param, &sodium_handled);
    if (sodium_handled) {
        return sodium_result;
    }
#endif

    do_psa_init();

    const struct PsaEccCurveParams *curve = psa_ecc_curve_table_lookup(pk_param);
    if (IS_NULL_PTR(curve)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // Validate curve is acceptable for this key type
    // eddh: Montgomery only (x25519, x448)
    // eddsa: Twisted Edwards only (ed25519, ed448)
    // ecdh: Montgomery + Weierstrass (not Twisted Edwards)
    switch (key_type) {
        case Eddh:
            if (curve->family != PSA_ECC_FAMILY_MONTGOMERY) {
                RAISE_ERROR(BADARG_ATOM);
            }
            break;
        case Eddsa:
            if (curve->family != PSA_ECC_FAMILY_TWISTED_EDWARDS) {
                RAISE_ERROR(BADARG_ATOM);
            }
            break;
        case Ecdh:
            if (curve->family == PSA_ECC_FAMILY_TWISTED_EDWARDS) {
                RAISE_ERROR(BADARG_ATOM);
            }
            break;
        default:
            RAISE_ERROR(BADARG_ATOM);
    }

    psa_key_type_t psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(curve->family);
    size_t psa_key_bits = curve->key_bits;

    psa_key_attributes_t attributes = PSA_KEY_ATTRIBUTES_INIT;
    psa_set_key_usage_flags(&attributes, PSA_KEY_USAGE_EXPORT);
    psa_set_key_type(&attributes, psa_key_type);
    psa_set_key_bits(&attributes, psa_key_bits);

    psa_key_id_t key_id = 0;
    psa_status_t status = psa_generate_key(&attributes, &key_id);
    psa_reset_key_attributes(&attributes);
    switch (status) {
        case PSA_SUCCESS:
            break;
        case PSA_ERROR_NOT_SUPPORTED:
            RAISE_ERROR(
                make_crypto_error(__FILE__, __LINE__, "Unsupported key type or parameter", ctx));
        default:
            RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx));
    }

    bool successful = false;
    term result = ERROR_ATOM;

    size_t exported_priv_size = PSA_KEY_EXPORT_ECC_KEY_PAIR_MAX_SIZE(psa_key_bits);
    uint8_t *exported_priv = NULL;
    size_t exported_pub_size = PSA_KEY_EXPORT_ECC_PUBLIC_KEY_MAX_SIZE(psa_key_bits);
    uint8_t *exported_pub = NULL;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    exported_priv = malloc(exported_priv_size);
    exported_pub = malloc(exported_pub_size);
    if (UNLIKELY(exported_priv == NULL || exported_pub == NULL)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    size_t exported_priv_length = 0;
    status = psa_export_key(key_id, exported_priv, exported_priv_size, &exported_priv_length);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        result = make_crypto_error(__FILE__, __LINE__, "Failed private key export", ctx);
        goto cleanup;
    }

    size_t exported_pub_length = 0;
    status = psa_export_public_key(key_id, exported_pub, exported_pub_size, &exported_pub_length);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        result = make_crypto_error(__FILE__, __LINE__, "Failed public key export", ctx);
        goto cleanup;
    }

    if (UNLIKELY(memory_ensure_free(ctx,
                     TERM_BINARY_HEAP_SIZE(exported_priv_length)
                         + TERM_BINARY_HEAP_SIZE(exported_pub_length) + TUPLE_SIZE(2))
            != MEMORY_GC_OK)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    term priv_term = term_from_literal_binary(exported_priv, exported_priv_length, &ctx->heap, glb);
    term pub_term = term_from_literal_binary(exported_pub, exported_pub_length, &ctx->heap, glb);
    successful = true;
    result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, pub_term);
    term_put_tuple_element(result, 1, priv_term);

cleanup:
    psa_destroy_key(key_id);
    secure_free(exported_priv, exported_priv_size);
    secure_free(exported_pub, exported_pub_size);

    if (UNLIKELY(!successful)) {
        RAISE_ERROR(result);
    }

    return result;
}

static term nif_crypto_compute_key(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    GlobalContext *glb = ctx->global;

    enum pk_type_t key_type = interop_atom_term_select_int(pk_type_table, argv[0], glb);
    enum pk_param_t pk_param = interop_atom_term_select_int(pk_param_table, argv[3], glb);

#ifdef HAVE_LIBSODIUM
    bool sodium_handled;
    term sodium_result
        = sodium_try_compute_key(ctx, key_type, pk_param, argv[1], argv[2], &sodium_handled);
    if (sodium_handled) {
        return sodium_result;
    }
#endif

    do_psa_init();

    const struct PsaEccCurveParams *curve = psa_ecc_curve_table_lookup(pk_param);
    if (IS_NULL_PTR(curve)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // compute_key only supports key agreement: eddh (Montgomery only) and ecdh (not Twisted Edwards)
    switch (key_type) {
        case Eddh:
            if (curve->family != PSA_ECC_FAMILY_MONTGOMERY) {
                RAISE_ERROR(BADARG_ATOM);
            }
            break;
        case Ecdh:
            if (curve->family == PSA_ECC_FAMILY_TWISTED_EDWARDS) {
                RAISE_ERROR(BADARG_ATOM);
            }
            break;
        default:
            RAISE_ERROR(BADARG_ATOM);
    }

    psa_algorithm_t psa_algo = PSA_ALG_ECDH;
    psa_key_type_t psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(curve->family);
    size_t psa_key_bits = curve->key_bits;

    term pub_key_term = argv[1];
    VALIDATE_VALUE(pub_key_term, term_is_binary);
    const void *pub_key = term_binary_data(pub_key_term);
    size_t pub_key_size = term_binary_size(pub_key_term);

    term priv_key_term = argv[2];
    VALIDATE_VALUE(priv_key_term, term_is_binary);
    const void *priv_key = term_binary_data(priv_key_term);
    size_t priv_key_size = term_binary_size(priv_key_term);

    psa_key_attributes_t attr = PSA_KEY_ATTRIBUTES_INIT;
    psa_set_key_type(&attr, psa_key_type);
    psa_set_key_bits(&attr, psa_key_bits);
    psa_set_key_usage_flags(&attr, PSA_KEY_USAGE_DERIVE);
    psa_set_key_algorithm(&attr, psa_algo);

    psa_key_id_t key_id = 0;
    psa_status_t status = psa_import_key(&attr, priv_key, priv_key_size, &key_id);
    psa_reset_key_attributes(&attr);
    switch (status) {
        case PSA_SUCCESS:
            break;
        case PSA_ERROR_NOT_SUPPORTED:
            RAISE_ERROR(
                make_crypto_error(__FILE__, __LINE__, "Unsupported key type or parameter", ctx));
        default:
            RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx));
    }

    bool success = false;
    term result = ERROR_ATOM;

    size_t shared_out_size = PSA_RAW_KEY_AGREEMENT_OUTPUT_SIZE(psa_key_type, psa_key_bits);
    uint8_t *shared_out = NULL;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    shared_out = malloc(shared_out_size);
    if (IS_NULL_PTR(shared_out)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }
    size_t shared_len = 0;
    status = psa_raw_key_agreement(
        psa_algo, key_id, pub_key, pub_key_size, shared_out, shared_out_size, &shared_len);
    switch (status) {
        case PSA_SUCCESS:
            break;
        case PSA_ERROR_NOT_SUPPORTED:
            result
                = make_crypto_error(__FILE__, __LINE__, "Unsupported key type or parameter", ctx);
            goto cleanup;
        default:
            result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
            goto cleanup;
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BINARY_HEAP_SIZE(shared_len)) != MEMORY_GC_OK)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    success = true;
    result = term_from_literal_binary(shared_out, shared_len, &ctx->heap, glb);

cleanup:
    psa_destroy_key(key_id);
    secure_free(shared_out, shared_out_size);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

static const AtomStringIntPair psa_hash_algorithm_table[] = {
    { ATOM_STR("\x3", "sha"), PSA_ALG_SHA_1 },
    { ATOM_STR("\x6", "sha224"), PSA_ALG_SHA_224 },
    { ATOM_STR("\x6", "sha256"), PSA_ALG_SHA_256 },
    { ATOM_STR("\x6", "sha384"), PSA_ALG_SHA_384 },
    { ATOM_STR("\x6", "sha512"), PSA_ALG_SHA_512 },
    { ATOM_STR("\x8", "sha3_224"), PSA_ALG_SHA3_224 },
    { ATOM_STR("\x8", "sha3_256"), PSA_ALG_SHA3_256 },
    { ATOM_STR("\x8", "sha3_384"), PSA_ALG_SHA3_384 },
    { ATOM_STR("\x8", "sha3_512"), PSA_ALG_SHA3_512 },
    { ATOM_STR("\x3", "md5"), PSA_ALG_MD5 },
    { ATOM_STR("\x9", "ripemd160"), PSA_ALG_RIPEMD160 },

    SELECT_INT_DEFAULT(PSA_ALG_NONE)
};

#ifdef CRYPTO_SIGN_AVAILABLE

static term nif_crypto_sign(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#ifdef HAVE_LIBSODIUM
    bool sodium_handled;
    term sodium_result = sodium_try_sign(ctx, argv[0], argv[1], argv[2], argv[3], &sodium_handled);
    if (sodium_handled) {
        return sodium_result;
    }
#endif

#ifdef HAVE_MBEDTLS_ECDSA_RAW_TO_DER
    do_psa_init();

    GlobalContext *glb = ctx->global;

    term alg_term = argv[0];
    if (UNLIKELY(
            !globalcontext_is_term_equal_to_atom_string(glb, alg_term, ATOM_STR("\x5", "ecdsa")))) {
        RAISE_ERROR(
            make_crypto_error_tag(__FILE__, __LINE__, "Invalid public key", ERROR_ATOM, ctx));
    }

    term hash_algo_term = argv[1];
    psa_algorithm_t hash_algo
        = interop_atom_term_select_int(psa_hash_algorithm_table, hash_algo_term, glb);
    if (UNLIKELY(hash_algo == PSA_ALG_NONE)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad digest type", ctx));
    }
    psa_algorithm_t psa_key_alg = PSA_ALG_ECDSA(hash_algo);

    // argv[2] is data, will handle later

    term key_list_term = argv[3];
    if (UNLIKELY(!term_is_nonempty_list(key_list_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA private key", ctx));
    }

    term priv_term = term_get_list_head(key_list_term);
    if (UNLIKELY(!term_is_binary(priv_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA private key", ctx));
    }
    const void *priv = term_binary_data(priv_term);
    size_t priv_len = term_binary_size(priv_term);

    term key_list_term_tail = term_get_list_tail(key_list_term);
    if (UNLIKELY(!term_is_nonempty_list(key_list_term_tail))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA private key", ctx));
    }
    term priv_param_term = term_get_list_head(key_list_term_tail);

    enum pk_param_t pk_param = interop_atom_term_select_int(pk_param_table, priv_param_term, glb);

    // ECDSA sign: only Weierstrass curves
    const struct PsaEccCurveParams *curve = psa_ecc_curve_table_lookup(pk_param);
    if (IS_NULL_PTR(curve)
        || curve->family == PSA_ECC_FAMILY_MONTGOMERY
        || curve->family == PSA_ECC_FAMILY_TWISTED_EDWARDS) {
        RAISE_ERROR(
            make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA private key", ctx));
    }

    psa_key_type_t psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(curve->family);
    size_t psa_key_bits = curve->key_bits;

    if (UNLIKELY(priv_len != PSA_BITS_TO_BYTES(psa_key_bits))) {
        // OTP even accepts empty binaries as keys, PSA API doesn't like it
        // so we rather fail before with an understandable error message
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA private key", ctx));
    }

    psa_key_attributes_t attr = PSA_KEY_ATTRIBUTES_INIT;
    psa_set_key_type(&attr, psa_key_type);
    psa_set_key_bits(&attr, psa_key_bits);
    psa_set_key_usage_flags(&attr, PSA_KEY_USAGE_SIGN_MESSAGE);
    psa_set_key_algorithm(&attr, psa_key_alg);

    psa_key_id_t key_id = 0;
    psa_status_t status = psa_import_key(&attr, priv, priv_len, &key_id);
    psa_reset_key_attributes(&attr);
    switch (status) {
        case PSA_SUCCESS:
            break;
        case PSA_ERROR_NOT_SUPPORTED:
            RAISE_ERROR(
                make_crypto_error(__FILE__, __LINE__, "Unsupported key type or parameter", ctx));
        default:
            RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx));
    }

    term result = ERROR_ATOM;
    bool success = false;

    size_t sig_raw_size = PSA_ECDSA_SIGNATURE_SIZE(psa_key_bits);
    uint8_t *sig_raw = NULL;
#if MBEDTLS_VERSION_NUMBER >= 0x04000000
    size_t sig_der_size = MBEDTLS_ECDSA_DER_MAX_SIG_LEN(psa_key_bits);
#else
    size_t sig_der_size = MBEDTLS_ECDSA_MAX_SIG_LEN(psa_key_bits);
#endif
    void *sig_der = NULL;
    void *maybe_allocated_data = NULL;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    term data_term = argv[2];
    const void *data;
    size_t data_len;
    term iodata_handle_result = handle_iodata(data_term, &data, &data_len, &maybe_allocated_data);
    if (UNLIKELY(iodata_handle_result != OK_ATOM)) {
        result = make_crypto_error(__FILE__, __LINE__, "Expected a binary or a list", ctx);
        goto cleanup;
    }

    sig_raw = malloc(sig_raw_size);
    if (IS_NULL_PTR(sig_raw)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    size_t sig_raw_len = 0;
    status = psa_sign_message(
        key_id, psa_key_alg, data, data_len, sig_raw, sig_raw_size, &sig_raw_len);
    switch (status) {
        case PSA_SUCCESS:
            break;
        case PSA_ERROR_NOT_SUPPORTED:
            result
                = make_crypto_error(__FILE__, __LINE__, "Unsupported key type or parameter", ctx);
            goto cleanup;
        default:
            result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
            goto cleanup;
    }

    assert(sig_raw_len == sig_raw_size);

    sig_der = malloc(sig_der_size);
    if (IS_NULL_PTR(sig_der)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    size_t sig_der_len = 0;
    int ret = mbedtls_ecdsa_raw_to_der(
        psa_key_bits, sig_raw, sig_raw_len, sig_der, sig_der_size, &sig_der_len);
    if (ret != 0) {
        result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
        goto cleanup;
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BINARY_HEAP_SIZE(sig_der_len)) != MEMORY_GC_OK)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    success = true;
    result = term_from_literal_binary(sig_der, sig_der_len, &ctx->heap, glb);

cleanup:
    psa_destroy_key(key_id);

    free(maybe_allocated_data);
    free(sig_raw);
    free(sig_der);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
#else
    RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unsupported key type or parameter", ctx));
#endif /* HAVE_MBEDTLS_ECDSA_RAW_TO_DER */
}

#endif /* CRYPTO_SIGN_AVAILABLE */

#ifdef CRYPTO_VERIFY_AVAILABLE

static term nif_crypto_verify(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#ifdef HAVE_LIBSODIUM
    bool sodium_handled;
    term sodium_result
        = sodium_try_verify(ctx, argv[0], argv[1], argv[2], argv[3], argv[4], &sodium_handled);
    if (sodium_handled) {
        return sodium_result;
    }
#endif

#ifdef HAVE_MBEDTLS_ECDSA_DER_TO_RAW
    do_psa_init();

    GlobalContext *glb = ctx->global;

    term alg_term = argv[0];
    if (UNLIKELY(
            !globalcontext_is_term_equal_to_atom_string(glb, alg_term, ATOM_STR("\x5", "ecdsa")))) {
        RAISE_ERROR(
            make_crypto_error_tag(__FILE__, __LINE__, "Invalid public key", ERROR_ATOM, ctx));
    }

    term hash_algo_term = argv[1];
    psa_algorithm_t hash_algo
        = interop_atom_term_select_int(psa_hash_algorithm_table, hash_algo_term, glb);
    if (UNLIKELY(hash_algo == PSA_ALG_NONE)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad digest type", ctx));
    }
    psa_algorithm_t psa_key_alg = PSA_ALG_ECDSA(hash_algo);

    // argv[2] is data, will handle it later

    term sig_der_term = argv[3];
    VALIDATE_VALUE(sig_der_term, term_is_binary);
    const void *sig_der = term_binary_data(sig_der_term);
    size_t sig_der_len = term_binary_size(sig_der_term);

    term key_list_term = argv[4];
    if (UNLIKELY(!term_is_nonempty_list(key_list_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA public key", ctx));
    }

    term pub_term = term_get_list_head(key_list_term);
    if (UNLIKELY(!term_is_binary(pub_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA public key", ctx));
    }
    const void *pub = term_binary_data(pub_term);
    size_t pub_len = term_binary_size(pub_term);

    term key_list_term_tail = term_get_list_tail(key_list_term);
    if (UNLIKELY(!term_is_nonempty_list(key_list_term_tail))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA public key", ctx));
    }
    term priv_param_term = term_get_list_head(key_list_term_tail);

    enum pk_param_t pk_param = interop_atom_term_select_int(pk_param_table, priv_param_term, glb);

    // ECDSA verify: only Weierstrass curves, uses public key type
    const struct PsaEccCurveParams *curve = psa_ecc_curve_table_lookup(pk_param);
    if (IS_NULL_PTR(curve)
        || curve->family == PSA_ECC_FAMILY_MONTGOMERY
        || curve->family == PSA_ECC_FAMILY_TWISTED_EDWARDS) {
        RAISE_ERROR(
            make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA public key", ctx));
    }

    psa_key_type_t psa_key_type = PSA_KEY_TYPE_ECC_PUBLIC_KEY(curve->family);
    size_t psa_key_bits = curve->key_bits;

    psa_key_attributes_t attr = PSA_KEY_ATTRIBUTES_INIT;
    psa_set_key_type(&attr, psa_key_type);
    psa_set_key_bits(&attr, psa_key_bits);
    psa_set_key_usage_flags(&attr, PSA_KEY_USAGE_VERIFY_MESSAGE);
    psa_set_key_algorithm(&attr, psa_key_alg);

    psa_key_id_t key_id = 0;
    psa_status_t status = psa_import_key(&attr, pub, pub_len, &key_id);
    psa_reset_key_attributes(&attr);
    switch (status) {
        case PSA_SUCCESS:
            break;
        case PSA_ERROR_NOT_SUPPORTED:
            RAISE_ERROR(
                make_crypto_error(__FILE__, __LINE__, "Unsupported key type or parameter", ctx));
        case PSA_ERROR_INVALID_ARGUMENT:
            RAISE_ERROR(
                make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA public key", ctx));
        default:
            RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx));
    }

    term result = ERROR_ATOM;
    bool success = false;
    size_t sig_raw_size = PSA_ECDSA_SIGNATURE_SIZE(psa_key_bits);
    void *sig_raw = NULL;
    void *maybe_allocated_data = NULL;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    term data_term = argv[2];
    const void *data;
    size_t data_len;
    term iodata_handle_result = handle_iodata(data_term, &data, &data_len, &maybe_allocated_data);
    if (UNLIKELY(iodata_handle_result != OK_ATOM)) {
        result = make_crypto_error(__FILE__, __LINE__, "Expected a binary or a list", ctx);
        goto cleanup;
    }

    sig_raw = malloc(sig_raw_size);
    if (IS_NULL_PTR(sig_raw)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }
    size_t sig_raw_len = 0;

    int ret = mbedtls_ecdsa_der_to_raw(
        psa_key_bits, sig_der, sig_der_len, sig_raw, sig_raw_size, &sig_raw_len);
    if (UNLIKELY(ret != 0 || sig_raw_len != sig_raw_size)) {
        // an invalid signature doesn't raise error on OTP, but it just fails verify
        result = FALSE_ATOM;
        success = true;
        goto cleanup;
    }

    status = psa_verify_message(key_id, psa_key_alg, data, data_len, sig_raw, sig_raw_len);
    switch (status) {
        case PSA_SUCCESS:
            result = TRUE_ATOM;
            success = true;
            break;

        case PSA_ERROR_INVALID_SIGNATURE:
            result = FALSE_ATOM;
            success = true;
            break;

        case PSA_ERROR_NOT_SUPPORTED:
            result
                = make_crypto_error(__FILE__, __LINE__, "Unsupported key type or parameter", ctx);
            break;

        default:
            result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
    }

cleanup:
    psa_destroy_key(key_id);

    free(maybe_allocated_data);
    free(sig_raw);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
#else
    RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unsupported key type or parameter", ctx));
#endif /* HAVE_MBEDTLS_ECDSA_DER_TO_RAW */
}

#endif /* CRYPTO_VERIFY_AVAILABLE */

static const AtomStringIntPair cmac_algorithm_bits_table[] = {
    { ATOM_STR("\xB", "aes_128_cbc"), 128 },
    { ATOM_STR("\xB", "aes_128_ecb"), 128 },
    { ATOM_STR("\xB", "aes_192_cbc"), 192 },
    { ATOM_STR("\xB", "aes_192_ecb"), 192 },
    { ATOM_STR("\xB", "aes_256_cbc"), 256 },
    { ATOM_STR("\xB", "aes_256_ecb"), 256 },

    SELECT_INT_DEFAULT(0)
};

static const AtomStringIntPair mac_key_table[] = {
    { ATOM_STR("\x4", "cmac"), PSA_KEY_TYPE_AES },
    { ATOM_STR("\x4", "hmac"), PSA_KEY_TYPE_HMAC },

    SELECT_INT_DEFAULT(PSA_KEY_TYPE_NONE)
};

static term nif_crypto_mac(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    term mac_type_term = argv[0];
    term sub_type_term = argv[1];

    // argv[2] is key, will handle here below
    // argv[3] is data, will handle it later

    bool success = false;
    term result = ERROR_ATOM;

    psa_key_id_t key_id = 0;
    void *maybe_allocated_key = NULL;
    size_t key_len = 0;
    void *maybe_allocated_data = NULL;
    size_t mac_out_size = 0;
    void *mac_out = NULL;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    term key_term = argv[2];
    const void *key;
    term iodata_handle_result = handle_iodata(key_term, &key, &key_len, &maybe_allocated_key);
    if (UNLIKELY(iodata_handle_result != OK_ATOM)) {
        result = make_crypto_error(__FILE__, __LINE__, "Expected a binary or a list", ctx);
        goto cleanup;
    }

    psa_key_type_t psa_key_type = interop_atom_term_select_int(mac_key_table, mac_type_term, glb);
    psa_algorithm_t psa_key_algo;
    psa_key_bits_t key_bit_size;
    switch (psa_key_type) {
        case PSA_KEY_TYPE_AES:
            psa_key_algo = PSA_ALG_CMAC;
            key_bit_size
                = interop_atom_term_select_int(cmac_algorithm_bits_table, sub_type_term, glb);
            if (UNLIKELY(key_bit_size == 0)) {
                result = make_crypto_error(__FILE__, __LINE__, "Unknown cipher", ctx);
                goto cleanup;
            }

            if (UNLIKELY(key_bit_size != key_len * 8)) {
                result = make_crypto_error(__FILE__, __LINE__, "Bad key size", ctx);
                goto cleanup;
            }
            break;
        case PSA_KEY_TYPE_HMAC: {
            psa_algorithm_t sub_type_algo
                = interop_atom_term_select_int(psa_hash_algorithm_table, sub_type_term, glb);
            if (UNLIKELY(sub_type_algo == PSA_ALG_NONE)) {
                result
                    = make_crypto_error(__FILE__, __LINE__, "Bad digest algorithm for HMAC", ctx);
                goto cleanup;
            }
            psa_key_algo = PSA_ALG_HMAC(sub_type_algo);
            key_bit_size = key_len * 8;
        } break;
        default:
            result = make_crypto_error(__FILE__, __LINE__, "Unknown mac algorithm", ctx);
            goto cleanup;
    }

    psa_key_attributes_t attr = PSA_KEY_ATTRIBUTES_INIT;
    psa_set_key_type(&attr, psa_key_type);
    psa_set_key_bits(&attr, key_bit_size);
    psa_set_key_usage_flags(&attr, PSA_KEY_USAGE_SIGN_MESSAGE);
    psa_set_key_algorithm(&attr, psa_key_algo);

    psa_status_t status = psa_import_key(&attr, key, key_len, &key_id);
    psa_reset_key_attributes(&attr);
    switch (status) {
        case PSA_SUCCESS:
            break;
        case PSA_ERROR_NOT_SUPPORTED:
            result = make_crypto_error(__FILE__, __LINE__, "Unsupported algorithm", ctx);
            goto cleanup;
        default:
            result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
            goto cleanup;
    }

    term data_term = argv[3];
    const void *data;
    size_t data_len;
    iodata_handle_result = handle_iodata(data_term, &data, &data_len, &maybe_allocated_data);
    if (UNLIKELY(iodata_handle_result != OK_ATOM)) {
        result = make_crypto_error(__FILE__, __LINE__, "Expected a binary or a list", ctx);
        goto cleanup;
    }

    mac_out_size = PSA_MAC_LENGTH(psa_key_type, key_bit_size, psa_key_algo);
    mac_out = malloc(mac_out_size);
    if (IS_NULL_PTR(mac_out)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    size_t mac_len = 0;
    status = psa_mac_compute(key_id, psa_key_algo, data, data_len, mac_out, mac_out_size, &mac_len);
    switch (status) {
        case PSA_SUCCESS:
            break;
        case PSA_ERROR_NOT_SUPPORTED:
            result = make_crypto_error(__FILE__, __LINE__, "Unsupported algorithm", ctx);
            goto cleanup;
        default:
            result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
            goto cleanup;
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BINARY_HEAP_SIZE(mac_len)) != MEMORY_GC_OK)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    success = true;
    result = term_from_literal_binary(mac_out, mac_len, &ctx->heap, glb);

cleanup:
    psa_destroy_key(key_id);
    secure_free(mac_out, mac_out_size);
    secure_free(maybe_allocated_key, key_len);
    free(maybe_allocated_data);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

struct HashState
{
    psa_algorithm_t psa_algo;
    psa_hash_operation_t psa_op;
};

static void psa_hash_op_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct HashState *hash_state = (struct HashState *) obj;
    psa_hash_abort(&hash_state->psa_op);
}

const ErlNifResourceTypeInit psa_hash_op_resource_type_init = {
    .members = 1,
    .dtor = psa_hash_op_dtor
};

struct MacState
{
    psa_mac_operation_t psa_op;
    psa_key_id_t key_id;
    psa_algorithm_t psa_algo;
    psa_key_type_t psa_key_type;
    size_t key_bit_size;
#ifndef AVM_NO_SMP
    Mutex *mutex;
#endif
};

static void psa_mac_op_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct MacState *mac_state = (struct MacState *) obj;
    psa_mac_abort(&mac_state->psa_op);
    if (mac_state->key_id != 0) {
        psa_destroy_key(mac_state->key_id);
        mac_state->key_id = 0;
    }
#ifndef AVM_NO_SMP
    if (mac_state->mutex) {
        smp_mutex_destroy(mac_state->mutex);
    }
#endif
}

const ErlNifResourceTypeInit psa_mac_op_resource_type_init = {
    .members = 1,
    .dtor = psa_mac_op_dtor
};

static term nif_crypto_mac_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    term mac_type_term = argv[0];
    term sub_type_term = argv[1];
    // argv[2] is key

    bool success = false;
    term result = ERROR_ATOM;

    void *maybe_allocated_key = NULL;
    size_t key_len = 0;
    struct MacState *mac_obj = NULL;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    term key_term = argv[2];
    const void *key;
    term iodata_handle_result = handle_iodata(key_term, &key, &key_len, &maybe_allocated_key);
    if (UNLIKELY(iodata_handle_result != OK_ATOM)) {
        result = make_crypto_error(__FILE__, __LINE__, "Expected a binary or a list", ctx);
        goto cleanup;
    }

    psa_key_type_t psa_key_type = interop_atom_term_select_int(mac_key_table, mac_type_term, glb);
    psa_algorithm_t psa_key_algo;
    psa_key_bits_t key_bit_size;
    switch (psa_key_type) {
        case PSA_KEY_TYPE_AES:
            psa_key_algo = PSA_ALG_CMAC;
            key_bit_size
                = interop_atom_term_select_int(cmac_algorithm_bits_table, sub_type_term, glb);
            if (UNLIKELY(key_bit_size == 0)) {
                result = make_crypto_error(__FILE__, __LINE__, "Unknown cipher", ctx);
                goto cleanup;
            }
            if (UNLIKELY(key_bit_size != key_len * 8)) {
                result = make_crypto_error(__FILE__, __LINE__, "Bad key size", ctx);
                goto cleanup;
            }
            break;
        case PSA_KEY_TYPE_HMAC: {
            psa_algorithm_t sub_type_algo
                = interop_atom_term_select_int(psa_hash_algorithm_table, sub_type_term, glb);
            if (UNLIKELY(sub_type_algo == PSA_ALG_NONE)) {
                result
                    = make_crypto_error(__FILE__, __LINE__, "Bad digest algorithm for HMAC", ctx);
                goto cleanup;
            }
            psa_key_algo = PSA_ALG_HMAC(sub_type_algo);
            key_bit_size = key_len * 8;
        } break;
        default:
            result = make_crypto_error(__FILE__, __LINE__, "Unknown mac algorithm", ctx);
            goto cleanup;
    }

    psa_key_id_t key_id = 0;
    psa_key_attributes_t attr = PSA_KEY_ATTRIBUTES_INIT;
    psa_set_key_type(&attr, psa_key_type);
    psa_set_key_bits(&attr, key_bit_size);
    psa_set_key_usage_flags(&attr, PSA_KEY_USAGE_SIGN_MESSAGE);
    psa_set_key_algorithm(&attr, psa_key_algo);

    psa_status_t status = psa_import_key(&attr, key, key_len, &key_id);
    psa_reset_key_attributes(&attr);
    switch (status) {
        case PSA_SUCCESS:
            break;
        case PSA_ERROR_NOT_SUPPORTED:
            result = make_crypto_error(__FILE__, __LINE__, "Unsupported algorithm", ctx);
            goto cleanup;
        default:
            result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
            goto cleanup;
    }

    mac_obj = enif_alloc_resource(glb->psa_mac_op_resource_type, sizeof(struct MacState));
    if (IS_NULL_PTR(mac_obj)) {
        psa_destroy_key(key_id);
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }
    memset(mac_obj, 0, sizeof(struct MacState));
    mac_obj->key_id = key_id;
    mac_obj->psa_algo = psa_key_algo;
    mac_obj->psa_key_type = psa_key_type;
    mac_obj->key_bit_size = key_bit_size;
#ifndef AVM_NO_SMP
    mac_obj->mutex = smp_mutex_create();
    if (IS_NULL_PTR(mac_obj->mutex)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }
#endif

    status = psa_mac_sign_setup(&mac_obj->psa_op, key_id, psa_key_algo);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
        goto cleanup;
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_REFERENCE_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    success = true;
    result = term_from_resource(mac_obj, &ctx->heap);

cleanup:
    if (mac_obj) {
        enif_release_resource(mac_obj);
    }
    secure_free(maybe_allocated_key, key_len);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

static term nif_crypto_mac_update(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    void *psa_mac_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            glb->psa_mac_op_resource_type, &psa_mac_obj_ptr))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad ref", ctx));
    }
    struct MacState *mac_state = (struct MacState *) psa_mac_obj_ptr;

    void *maybe_allocated_data = NULL;
    size_t data_len;
    term data_term = argv[1];
    const void *data;
    term iodata_handle_result = handle_iodata(data_term, &data, &data_len, &maybe_allocated_data);
    if (UNLIKELY(iodata_handle_result != OK_ATOM)) {
        free(maybe_allocated_data);
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad text", ctx));
    }

    SMP_MUTEX_LOCK(mac_state->mutex);
    psa_status_t status = psa_mac_update(&mac_state->psa_op, data, data_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        psa_mac_abort(&mac_state->psa_op);
        psa_destroy_key(mac_state->key_id);
        mac_state->key_id = 0;
        SMP_MUTEX_UNLOCK(mac_state->mutex);
        secure_free(maybe_allocated_data, data_len);
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx));
    }
    SMP_MUTEX_UNLOCK(mac_state->mutex);
    secure_free(maybe_allocated_data, data_len);

    return argv[0];
}

static term nif_crypto_mac_final(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    void *psa_mac_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            glb->psa_mac_op_resource_type, &psa_mac_obj_ptr))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad ref", ctx));
    }
    struct MacState *mac_state = (struct MacState *) psa_mac_obj_ptr;

    bool success = false;
    term result = ERROR_ATOM;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    size_t mac_size
        = PSA_MAC_LENGTH(mac_state->psa_key_type, mac_state->key_bit_size, mac_state->psa_algo);
    if (UNLIKELY(memory_ensure_free(ctx, TERM_BINARY_HEAP_SIZE(mac_size)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term mac_bin = term_create_uninitialized_binary(mac_size, &ctx->heap, glb);
    void *mac_buf = (void *) term_binary_data(mac_bin);

    size_t mac_len = 0;
    SMP_MUTEX_LOCK(mac_state->mutex);
    psa_status_t status = psa_mac_sign_finish(&mac_state->psa_op, mac_buf, mac_size, &mac_len);
    psa_destroy_key(mac_state->key_id);
    mac_state->key_id = 0;
    SMP_MUTEX_UNLOCK(mac_state->mutex);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
        goto cleanup;
    }

    success = true;
    result = mac_bin;

cleanup:
    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

static term nif_crypto_mac_finalN(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    void *psa_mac_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            glb->psa_mac_op_resource_type, &psa_mac_obj_ptr))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad ref", ctx));
    }
    struct MacState *mac_state = (struct MacState *) psa_mac_obj_ptr;

    avm_int_t requested_len;
    if (UNLIKELY(!term_is_integer(argv[1]))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad length", ctx));
    }
    requested_len = term_to_int(argv[1]);
    if (UNLIKELY(requested_len <= 0)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad length", ctx));
    }

    bool success = false;
    term result = ERROR_ATOM;

    size_t mac_size
        = PSA_MAC_LENGTH(mac_state->psa_key_type, mac_state->key_bit_size, mac_state->psa_algo);
    uint8_t *mac_buf = NULL;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    mac_buf = malloc(mac_size);
    if (IS_NULL_PTR(mac_buf)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    size_t mac_len = 0;
    SMP_MUTEX_LOCK(mac_state->mutex);
    psa_status_t status = psa_mac_sign_finish(&mac_state->psa_op, mac_buf, mac_size, &mac_len);
    psa_destroy_key(mac_state->key_id);
    mac_state->key_id = 0;
    SMP_MUTEX_UNLOCK(mac_state->mutex);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
        goto cleanup;
    }

    size_t out_len = (size_t) requested_len < mac_len ? (size_t) requested_len : mac_len;
    if (UNLIKELY(memory_ensure_free(ctx, TERM_BINARY_HEAP_SIZE(out_len)) != MEMORY_GC_OK)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    success = true;
    result = term_from_literal_binary(mac_buf, out_len, &ctx->heap, glb);

cleanup:
    secure_free(mac_buf, mac_size);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

static term nif_crypto_hash_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    term hash_algo_term = argv[0];
    psa_algorithm_t hash_algo
        = interop_atom_term_select_int(psa_hash_algorithm_table, hash_algo_term, glb);
    if (UNLIKELY(hash_algo == PSA_ALG_NONE)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad digest type", ctx));
    }

    struct HashState *hash_obj
        = enif_alloc_resource(glb->psa_hash_op_resource_type, sizeof(struct HashState));
    if (IS_NULL_PTR(hash_obj)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    memset(hash_obj, 0, sizeof(struct HashState));
    hash_obj->psa_algo = hash_algo;
    psa_status_t status = psa_hash_setup(&hash_obj->psa_op, hash_algo);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        enif_release_resource(hash_obj);
        switch (status) {
            case PSA_ERROR_NOT_SUPPORTED:
                RAISE_ERROR(make_crypto_error(
                    __FILE__, __LINE__, "Unsupported key type or parameter", ctx));
            default:
                RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx));
        }
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_REFERENCE_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        enif_release_resource(hash_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = term_from_resource(hash_obj, &ctx->heap);
    enif_release_resource(hash_obj);

    return obj;
}

static term nif_crypto_hash_update(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    void *psa_hash_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            glb->psa_hash_op_resource_type, &psa_hash_obj_ptr))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad state", ctx));
    }
    struct HashState *hash_state = (struct HashState *) psa_hash_obj_ptr;

    bool success = false;
    term result = ERROR_ATOM;

    void *maybe_allocated_data = NULL;
    struct HashState *new_hash_obj = NULL;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    size_t data_len;
    term data_term = argv[1];
    const void *data;
    term iodata_handle_result = handle_iodata(data_term, &data, &data_len, &maybe_allocated_data);
    if (UNLIKELY(iodata_handle_result != OK_ATOM)) {
        result = BADARG_ATOM;
        goto cleanup;
    }

    new_hash_obj = enif_alloc_resource(glb->psa_hash_op_resource_type, sizeof(struct HashState));
    if (IS_NULL_PTR(new_hash_obj)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }
    memset(new_hash_obj, 0, sizeof(struct HashState));
    new_hash_obj->psa_algo = hash_state->psa_algo;
    psa_status_t status = psa_hash_clone(&hash_state->psa_op, &new_hash_obj->psa_op);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
        goto cleanup;
    }

    status = psa_hash_update(&new_hash_obj->psa_op, data, data_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
        goto cleanup;
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_REFERENCE_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    success = true;
    result = term_from_resource(new_hash_obj, &ctx->heap);

cleanup:
    free(maybe_allocated_data);
    if (new_hash_obj) {
        enif_release_resource(new_hash_obj);
    }

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

static term nif_crypto_hash_final(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    void *psa_hash_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            glb->psa_hash_op_resource_type, &psa_hash_obj_ptr))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad state", ctx));
    }
    struct HashState *hash_state = (struct HashState *) psa_hash_obj_ptr;

    bool success = false;
    term result = ERROR_ATOM;

    psa_algorithm_t psa_algo = hash_state->psa_algo;
    psa_hash_operation_t psa_op = PSA_HASH_OPERATION_INIT;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    psa_status_t status = psa_hash_clone(&hash_state->psa_op, &psa_op);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
        goto cleanup;
    }

    size_t hash_size = PSA_HASH_LENGTH(psa_algo);
    if (UNLIKELY(memory_ensure_free(ctx, TERM_BINARY_HEAP_SIZE(hash_size)) != MEMORY_GC_OK)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    term hash_bin = term_create_uninitialized_binary(hash_size, &ctx->heap, glb);
    void *hash_buf = (void *) term_binary_data(hash_bin);

    size_t hash_len = 0;
    status = psa_hash_finish(&psa_op, hash_buf, hash_size, &hash_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
        goto cleanup;
    }
    assert(hash_size == hash_len);

    success = true;
    result = hash_bin;

cleanup:
    psa_hash_abort(&psa_op);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

enum cipher_padding
{
    CipherPaddingDefault = 0, /* discard partial block on final, return <<>> */
    CipherPaddingNone = 1, /* error if partial block remains on final */
    CipherPaddingPkcs = 2 /* PKCS7-pad last block on final (CBC only, via PSA) */
};

struct CipherState
{
#ifndef AVM_NO_SMP
    Mutex *mutex;
#endif
    psa_cipher_operation_t psa_op;
    psa_key_id_t key_id;
    psa_algorithm_t psa_algo;
    psa_key_type_t key_type;
    bool encrypting;
    bool finalized; /* true after crypto_final; further calls raise badarg */
    uint8_t block_size;
    enum cipher_padding padding;
};

static void psa_cipher_op_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct CipherState *cipher_state = (struct CipherState *) obj;
    psa_cipher_abort(&cipher_state->psa_op);
    if (cipher_state->key_id != 0) {
        psa_destroy_key(cipher_state->key_id);
        cipher_state->key_id = 0;
    }
#ifndef AVM_NO_SMP
    if (cipher_state->mutex) {
        smp_mutex_destroy(cipher_state->mutex);
    }
#endif
}

const ErlNifResourceTypeInit psa_cipher_op_resource_type_init = {
    .members = 1,
    .dtor = psa_cipher_op_dtor
};

struct PsaCipherParams
{
    const char *atom_str;
    psa_key_type_t key_type;
    psa_algorithm_t algorithm;
    uint16_t key_bits;
    uint8_t block_size;
    uint8_t iv_len;
};

static const struct PsaCipherParams psa_cipher_table[] = {
    // ECB modes - block cipher, no IV
    { ATOM_STR("\xB", "aes_128_ecb"), PSA_KEY_TYPE_AES, PSA_ALG_ECB_NO_PADDING, 128, 16, 0 },
    { ATOM_STR("\xB", "aes_192_ecb"), PSA_KEY_TYPE_AES, PSA_ALG_ECB_NO_PADDING, 192, 16, 0 },
    { ATOM_STR("\xB", "aes_256_ecb"), PSA_KEY_TYPE_AES, PSA_ALG_ECB_NO_PADDING, 256, 16, 0 },
    // CBC modes - block cipher, with IV
    { ATOM_STR("\xB", "aes_128_cbc"), PSA_KEY_TYPE_AES, PSA_ALG_CBC_NO_PADDING, 128, 16, 16 },
    { ATOM_STR("\xB", "aes_192_cbc"), PSA_KEY_TYPE_AES, PSA_ALG_CBC_NO_PADDING, 192, 16, 16 },
    { ATOM_STR("\xB", "aes_256_cbc"), PSA_KEY_TYPE_AES, PSA_ALG_CBC_NO_PADDING, 256, 16, 16 },
    // CFB128 modes - stream-like, with IV
    { ATOM_STR("\xE", "aes_128_cfb128"), PSA_KEY_TYPE_AES, PSA_ALG_CFB, 128, 0, 16 },
    { ATOM_STR("\xE", "aes_192_cfb128"), PSA_KEY_TYPE_AES, PSA_ALG_CFB, 192, 0, 16 },
    { ATOM_STR("\xE", "aes_256_cfb128"), PSA_KEY_TYPE_AES, PSA_ALG_CFB, 256, 0, 16 },
    // CTR modes - stream cipher, with IV
    { ATOM_STR("\xB", "aes_128_ctr"), PSA_KEY_TYPE_AES, PSA_ALG_CTR, 128, 0, 16 },
    { ATOM_STR("\xB", "aes_192_ctr"), PSA_KEY_TYPE_AES, PSA_ALG_CTR, 192, 0, 16 },
    { ATOM_STR("\xB", "aes_256_ctr"), PSA_KEY_TYPE_AES, PSA_ALG_CTR, 256, 0, 16 },
    // OFB modes - stream-like, with IV
    { ATOM_STR("\xB", "aes_128_ofb"), PSA_KEY_TYPE_AES, PSA_ALG_OFB, 128, 0, 16 },
    { ATOM_STR("\xB", "aes_192_ofb"), PSA_KEY_TYPE_AES, PSA_ALG_OFB, 192, 0, 16 },
    { ATOM_STR("\xB", "aes_256_ofb"), PSA_KEY_TYPE_AES, PSA_ALG_OFB, 256, 0, 16 },
};

#define PSA_CIPHER_TABLE_LEN (sizeof(psa_cipher_table) / sizeof(psa_cipher_table[0]))

static const struct PsaCipherParams *psa_cipher_table_lookup(GlobalContext *glb, term cipher_atom)
{
    for (size_t i = 0; i < PSA_CIPHER_TABLE_LEN; i++) {
        AtomString atom_str = (AtomString) psa_cipher_table[i].atom_str;
        if (globalcontext_is_term_equal_to_atom_string(glb, cipher_atom, atom_str)) {
            return &psa_cipher_table[i];
        }
    }
    return NULL;
}

/*
 * Accepts:
 *   - true / false  (boolean shorthand for {encrypt, true/false})
 *   - proplist with {encrypt, boolean()} and/or {padding, none|pkcs_padding}
 */
static term parse_cipher_flag_or_options(term flag_or_opts, bool *encrypting,
    enum cipher_padding *padding, GlobalContext *glb, Context *ctx)
{
    if (flag_or_opts == TRUE_ATOM) {
        *encrypting = true;
        return OK_ATOM;
    }

    if (flag_or_opts == FALSE_ATOM) {
        *encrypting = false;
        return OK_ATOM;
    }

    if (!term_is_list(flag_or_opts)) {
        return make_crypto_error(
            __FILE__, __LINE__, "Options are not a boolean or a proper list", ctx);
    }

    term t = flag_or_opts;
    while (term_is_nonempty_list(t)) {
        term head = term_get_list_head(t);

        if (UNLIKELY(!term_is_tuple(head) || term_get_tuple_arity(head) != 2)) {
            return make_crypto_error(__FILE__, __LINE__, "Bad option format", ctx);
        }

        term opt_key = term_get_tuple_element(head, 0);
        term opt_val = term_get_tuple_element(head, 1);

        if (globalcontext_is_term_equal_to_atom_string(glb, opt_key, ATOM_STR("\x7", "encrypt"))) {
            if (opt_val == TRUE_ATOM) {
                *encrypting = true;
            } else if (opt_val == FALSE_ATOM) {
                *encrypting = false;
            } else {
                return make_crypto_error(__FILE__, __LINE__, "Bad encrypt option value", ctx);
            }
        } else if (globalcontext_is_term_equal_to_atom_string(
                       glb, opt_key, ATOM_STR("\x7", "padding"))) {
            if (globalcontext_is_term_equal_to_atom_string(glb, opt_val, ATOM_STR("\x4", "none"))) {
                *padding = CipherPaddingNone;
            } else if (globalcontext_is_term_equal_to_atom_string(
                           glb, opt_val, ATOM_STR("\xC", "pkcs_padding"))) {
                *padding = CipherPaddingPkcs;
            } else {
                return make_crypto_error(__FILE__, __LINE__, "Bad padding option value", ctx);
            }
        } else {
            return make_crypto_error(__FILE__, __LINE__, "Unknown option", ctx);
        }

        t = term_get_list_tail(t);
    }

    if (UNLIKELY(!term_is_nil(t))) {
        return make_crypto_error(__FILE__, __LINE__, "Options is not a proper list", ctx);
    }

    return OK_ATOM;
}

static term nif_crypto_crypto_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    /* 1. Look up cipher parameters */
    term cipher_atom = argv[0];
    const struct PsaCipherParams *cipher_params = psa_cipher_table_lookup(glb, cipher_atom);
    if (IS_NULL_PTR(cipher_params)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unknown cipher", ctx));
    }

    /* 2. Validate key - must be a flat binary of the correct size */
    term key_term = argv[1];
    if (UNLIKELY(!term_is_binary(key_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad key", ctx));
    }
    size_t key_len = term_binary_size(key_term);
    if (UNLIKELY(key_len != cipher_params->key_bits / 8)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad key size", ctx));
    }
    const uint8_t *key_data = (const uint8_t *) term_binary_data(key_term);

    /* 3. Validate IV - must be a flat binary of the correct size.
     *    For ciphers that do not use an IV (iv_len == 0, e.g. ECB) any binary
     *    is accepted and silently ignored, matching OTP behaviour. */
    term iv_term = argv[2];
    if (UNLIKELY(!term_is_binary(iv_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad iv type", ctx));
    }
    size_t iv_len = term_binary_size(iv_term);
    if (cipher_params->iv_len > 0 && UNLIKELY(iv_len != cipher_params->iv_len)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad iv size", ctx));
    }

    /* 4. Parse FlagOrOptions */
    bool encrypting = true;
    enum cipher_padding padding = CipherPaddingDefault;
    term parse_result = parse_cipher_flag_or_options(argv[3], &encrypting, &padding, glb, ctx);
    if (UNLIKELY(parse_result != OK_ATOM)) {
        RAISE_ERROR(parse_result);
    }

    /* 4b. Resolve effective PSA algorithm based on padding option.
     *     PKCS7 padding is natively supported by PSA only for CBC; for any
     *     other cipher mode it is not available and we reject it early. */
    psa_algorithm_t effective_algo = cipher_params->algorithm;
    if (padding == CipherPaddingPkcs) {
        if (cipher_params->algorithm == PSA_ALG_CBC_NO_PADDING) {
            effective_algo = PSA_ALG_CBC_PKCS7;
        } else {
            RAISE_ERROR(make_crypto_error(
                __FILE__, __LINE__, "PKCS padding is supported only with CBC ciphers", ctx));
        }
    }

    /* 5. Import key via PSA */
    bool success = false;
    term result = ERROR_ATOM;

    psa_key_id_t key_id = 0;
    struct CipherState *cipher_obj = NULL;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    psa_key_attributes_t attr = PSA_KEY_ATTRIBUTES_INIT;
    psa_set_key_type(&attr, cipher_params->key_type);
    psa_set_key_bits(&attr, cipher_params->key_bits);
    psa_set_key_usage_flags(&attr, encrypting ? PSA_KEY_USAGE_ENCRYPT : PSA_KEY_USAGE_DECRYPT);
    psa_set_key_algorithm(&attr, effective_algo);

    psa_status_t status = psa_import_key(&attr, key_data, key_len, &key_id);
    psa_reset_key_attributes(&attr);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        switch (status) {
            case PSA_ERROR_NOT_SUPPORTED:
                result = make_crypto_error(__FILE__, __LINE__, "Unsupported algorithm", ctx);
                break;
            default:
                result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
                break;
        }
        goto cleanup;
    }

    /* 6. Allocate and initialise the resource */
    cipher_obj = enif_alloc_resource(glb->psa_cipher_op_resource_type, sizeof(struct CipherState));
    if (IS_NULL_PTR(cipher_obj)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }
    memset(cipher_obj, 0, sizeof(struct CipherState));
#ifndef AVM_NO_SMP
    cipher_obj->mutex = smp_mutex_create();
    if (IS_NULL_PTR(cipher_obj->mutex)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }
#endif
    cipher_obj->key_id = key_id;
    key_id = 0; // ownership transferred to resource immediately
    cipher_obj->psa_algo = effective_algo;
    cipher_obj->key_type = cipher_params->key_type;
    cipher_obj->encrypting = encrypting;
    cipher_obj->block_size = cipher_params->block_size;
    cipher_obj->padding = padding;

    /* 7. Set up the cipher operation */
    if (encrypting) {
        status = psa_cipher_encrypt_setup(&cipher_obj->psa_op, cipher_obj->key_id, effective_algo);
    } else {
        status = psa_cipher_decrypt_setup(&cipher_obj->psa_op, cipher_obj->key_id, effective_algo);
    }
    if (UNLIKELY(status != PSA_SUCCESS)) {
        result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
        goto cleanup;
    }

    /* 8. Set IV if this cipher requires one */
    if (cipher_params->iv_len > 0) {
        const uint8_t *iv_data = (const uint8_t *) term_binary_data(iv_term);
        status = psa_cipher_set_iv(&cipher_obj->psa_op, iv_data, iv_len);
        if (UNLIKELY(status != PSA_SUCCESS)) {
            result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
            goto cleanup;
        }
    }

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_REFERENCE_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    success = true;
    result = term_from_resource(cipher_obj, &ctx->heap);

cleanup:
    if (key_id != 0) {
        psa_destroy_key(key_id);
    }
    if (!IS_NULL_PTR(cipher_obj)) {
        enif_release_resource(cipher_obj);
    }

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

static term nif_crypto_crypto_update(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    /* 1. Retrieve the cipher state from the resource reference */
    void *cipher_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            glb->psa_cipher_op_resource_type, &cipher_obj_ptr))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad State", ctx));
    }
    struct CipherState *cipher_state = (struct CipherState *) cipher_obj_ptr;
    SMP_MUTEX_LOCK(cipher_state->mutex);

    if (UNLIKELY(cipher_state->finalized)) {
        SMP_MUTEX_UNLOCK(cipher_state->mutex);
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__,
            "Bad state: AtomVM does not allow operations after crypto_final", ctx));
    }

    bool success = false;
    term result = ERROR_ATOM;

    void *maybe_allocated_data = NULL;
    void *out_buf = NULL;
    size_t data_len = 0;
    size_t out_size = 0;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    /* 2. Handle iodata input */
    const void *data;
    term iodata_result = handle_iodata(argv[1], &data, &data_len, &maybe_allocated_data);
    if (UNLIKELY(iodata_result == BADARG_ATOM)) {
        SMP_MUTEX_UNLOCK(cipher_state->mutex);
        result = make_crypto_error(__FILE__, __LINE__, "expected binary", ctx);
        goto cleanup;
    }
    if (UNLIKELY(iodata_result != OK_ATOM)) {
        SMP_MUTEX_UNLOCK(cipher_state->mutex);
        result = iodata_result;
        goto cleanup;
    }

    /* 3. Encrypt/decrypt via PSA - PSA handles internal block buffering */
    out_size = PSA_CIPHER_UPDATE_OUTPUT_MAX_SIZE(data_len);
    if (out_size == 0) {
        out_size = 1; /* ensure valid malloc even for zero-length input */
    }
    out_buf = malloc(out_size);
    if (IS_NULL_PTR(out_buf)) {
        SMP_MUTEX_UNLOCK(cipher_state->mutex);
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    size_t out_len = 0;
    psa_status_t status
        = psa_cipher_update(&cipher_state->psa_op, data, data_len, out_buf, out_size, &out_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        psa_cipher_abort(&cipher_state->psa_op);
        psa_destroy_key(cipher_state->key_id);
        cipher_state->key_id = 0;
        cipher_state->finalized = true;
        SMP_MUTEX_UNLOCK(cipher_state->mutex);
        result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
        goto cleanup;
    }
    SMP_MUTEX_UNLOCK(cipher_state->mutex);

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BINARY_HEAP_SIZE(out_len)) != MEMORY_GC_OK)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    success = true;
    result = term_from_literal_binary(out_buf, out_len, &ctx->heap, glb);

cleanup:
    secure_free(maybe_allocated_data, data_len);
    secure_free(out_buf, out_size);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

static term nif_crypto_crypto_final(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    /* 1. Retrieve the cipher state */
    void *cipher_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            glb->psa_cipher_op_resource_type, &cipher_obj_ptr))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad State", ctx));
    }
    struct CipherState *cipher_state = (struct CipherState *) cipher_obj_ptr;
    SMP_MUTEX_LOCK(cipher_state->mutex);

    if (UNLIKELY(cipher_state->finalized)) {
        SMP_MUTEX_UNLOCK(cipher_state->mutex);
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__,
            "Bad state: AtomVM does not allow calling crypto_final more than once", ctx));
    }

    bool success = false;
    term result = ERROR_ATOM;

    void *out_buf = NULL;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    /* 2. Finalise via PSA - this terminates the operation */
    size_t out_size = PSA_CIPHER_FINISH_OUTPUT_MAX_SIZE;
    if (out_size == 0) {
        out_size = 1;
    }
    out_buf = malloc(out_size);
    if (IS_NULL_PTR(out_buf)) {
        SMP_MUTEX_UNLOCK(cipher_state->mutex);
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    size_t out_len = 0;
    psa_status_t status = psa_cipher_finish(&cipher_state->psa_op, out_buf, out_size, &out_len);
    cipher_state->finalized = true;
    psa_destroy_key(cipher_state->key_id);
    cipher_state->key_id = 0;
    SMP_MUTEX_UNLOCK(cipher_state->mutex);

    if (status == PSA_SUCCESS) {
        if (UNLIKELY(memory_ensure_free_with_roots(
                         ctx, TERM_BINARY_HEAP_SIZE(out_len), 1, &argv[0], MEMORY_NO_SHRINK)
                != MEMORY_GC_OK)) {
            result = OUT_OF_MEMORY_ATOM;
            goto cleanup;
        }
        success = true;
        result = term_from_literal_binary(out_buf, out_len, &ctx->heap, glb);
    } else if (status == PSA_ERROR_INVALID_ARGUMENT) {
        /* Block cipher with a partial last block: behaviour depends on padding.
         * Note: CipherPaddingPkcs uses PSA_ALG_CBC_PKCS7 which PSA handles
         * natively, so this branch should never be reached for PKCS padding. */
        psa_cipher_abort(&cipher_state->psa_op);
        switch (cipher_state->padding) {
            case CipherPaddingNone:
                result = make_crypto_error_tag(
                    __FILE__, __LINE__, "Padding 'none' but unfilled last block", ERROR_ATOM, ctx);
                goto cleanup;
            case CipherPaddingDefault:
                /* Silently discard the partial block */
                if (UNLIKELY(memory_ensure_free_with_roots(
                                 ctx, TERM_BINARY_HEAP_SIZE(0), 1, &argv[0], MEMORY_NO_SHRINK)
                        != MEMORY_GC_OK)) {
                    result = OUT_OF_MEMORY_ATOM;
                    goto cleanup;
                }
                success = true;
                result = term_from_literal_binary("", 0, &ctx->heap, glb);
                break;
            case CipherPaddingPkcs:
                /* Defensive: should be unreachable since PSA_ALG_CBC_PKCS7
                 * handles padding natively and never returns INVALID_ARGUMENT
                 * for a partial block. */
                result
                    = make_crypto_error(__FILE__, __LINE__, "Unexpected PKCS padding error", ctx);
                goto cleanup;
        }
    } else {
        result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
        goto cleanup;
    }

cleanup:
    secure_free(out_buf, out_size);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

struct PsaAeadParams
{
    AtomString atom_str;
    psa_key_type_t key_type;
    psa_algorithm_t algorithm;
    uint16_t key_bits;
    uint8_t default_tag_len;
};

static const struct PsaAeadParams psa_aead_table[] = {
    { ATOM_STR("\xB", "aes_128_gcm"), PSA_KEY_TYPE_AES, PSA_ALG_GCM, 128, 16 },
    { ATOM_STR("\xB", "aes_192_gcm"), PSA_KEY_TYPE_AES, PSA_ALG_GCM, 192, 16 },
    { ATOM_STR("\xB", "aes_256_gcm"), PSA_KEY_TYPE_AES, PSA_ALG_GCM, 256, 16 },
    { ATOM_STR("\xB", "aes_128_ccm"), PSA_KEY_TYPE_AES, PSA_ALG_CCM, 128, 16 },
    { ATOM_STR("\xB", "aes_192_ccm"), PSA_KEY_TYPE_AES, PSA_ALG_CCM, 192, 16 },
    { ATOM_STR("\xB", "aes_256_ccm"), PSA_KEY_TYPE_AES, PSA_ALG_CCM, 256, 16 },
    { ATOM_STR("\x11", "chacha20_poly1305"), PSA_KEY_TYPE_CHACHA20, PSA_ALG_CHACHA20_POLY1305, 256,
        16 },
};

#define PSA_AEAD_TABLE_LEN (sizeof(psa_aead_table) / sizeof(psa_aead_table[0]))

static const struct PsaAeadParams *psa_aead_table_lookup(GlobalContext *glb, term cipher_atom)
{
    for (size_t i = 0; i < PSA_AEAD_TABLE_LEN; i++) {
        AtomString atom_str = psa_aead_table[i].atom_str;
        if (globalcontext_is_term_equal_to_atom_string(glb, cipher_atom, atom_str)) {
            return &psa_aead_table[i];
        }
    }
    return NULL;
}

static term nif_crypto_crypto_one_time_aead(Context *ctx, int argc, term argv[])
{
    do_psa_init();

    GlobalContext *glb = ctx->global;

    term cipher_atom = argv[0];
    const struct PsaAeadParams *aead_params = psa_aead_table_lookup(glb, cipher_atom);
    if (IS_NULL_PTR(aead_params)) {
        size_t needed = TUPLE_SIZE(2) + (strlen("Unknown cipher") * CONS_SIZE);
        if (UNLIKELY(memory_ensure_free(ctx, needed) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term desc = interop_bytes_to_list("Unknown cipher", strlen("Unknown cipher"), &ctx->heap);
        term err_t = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err_t, 0, BADARG_ATOM);
        term_put_tuple_element(err_t, 1, desc);
        RAISE_ERROR(err_t);
    }

    bool encrypting;
    term enc_flag_term;
    term tag_or_len_term = UNDEFINED_ATOM;

    if (argc == 6) {
        enc_flag_term = argv[5];
    } else {
        tag_or_len_term = argv[5];
        enc_flag_term = argv[6];
    }

    if (enc_flag_term == TRUE_ATOM) {
        encrypting = true;
    } else if (enc_flag_term == FALSE_ATOM) {
        encrypting = false;
    } else {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "EncFlag must be a boolean", ctx));
    }

    size_t tag_len = aead_params->default_tag_len;
    const void *tag_data = NULL;
    size_t tag_data_len = 0;

    if (argc == 7) {
        if (encrypting) {
            if (UNLIKELY(!term_is_int(tag_or_len_term))) {
                RAISE_ERROR(
                    make_crypto_error(__FILE__, __LINE__, "TagLength must be an integer", ctx));
            }
            avm_int_t requested = term_to_int(tag_or_len_term);
            if (UNLIKELY(requested < 0)) {
                RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad tag length", ctx));
            }
            tag_len = requested;
        } else {
            if (UNLIKELY(!term_is_binary(tag_or_len_term))) {
                RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Tag must be a binary", ctx));
            }
            tag_data = term_binary_data(tag_or_len_term);
            tag_data_len = term_binary_size(tag_or_len_term);
            tag_len = tag_data_len;
        }
    }

    psa_algorithm_t psa_algo;
    if (tag_len != aead_params->default_tag_len) {
        psa_algo = PSA_ALG_AEAD_WITH_SHORTENED_TAG(aead_params->algorithm, tag_len);
    } else {
        psa_algo = aead_params->algorithm;
    }

    term key_term = argv[1];
    if (UNLIKELY(!term_is_binary(key_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad key", ctx));
    }
    size_t key_len = term_binary_size(key_term);
    if (UNLIKELY(key_len != aead_params->key_bits / 8)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad key size", ctx));
    }
    const void *key_data = term_binary_data(key_term);

    term iv_term = argv[2];
    if (UNLIKELY(!term_is_binary(iv_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad iv", ctx));
    }
    const void *iv_data = term_binary_data(iv_term);
    size_t iv_len = term_binary_size(iv_term);

    psa_key_attributes_t attr = PSA_KEY_ATTRIBUTES_INIT;
    psa_set_key_type(&attr, aead_params->key_type);
    psa_set_key_bits(&attr, aead_params->key_bits);
    psa_set_key_usage_flags(&attr, encrypting ? PSA_KEY_USAGE_ENCRYPT : PSA_KEY_USAGE_DECRYPT);
    psa_set_key_algorithm(&attr, psa_algo);

    psa_key_id_t key_id = 0;
    psa_status_t status = psa_import_key(&attr, key_data, key_len, &key_id);
    psa_reset_key_attributes(&attr);
    switch (status) {
        case PSA_SUCCESS:
            break;
        case PSA_ERROR_NOT_SUPPORTED:
            RAISE_ERROR(
                make_crypto_error(__FILE__, __LINE__, "Unsupported key type or parameter", ctx));
        default:
            RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx));
    }

    bool success = false;
    term result = ERROR_ATOM;

    void *maybe_allocated_intext = NULL;
    void *maybe_allocated_aad = NULL;
    void *out_buf = NULL;
    size_t out_buf_size = 0;

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    const void *intext_data;
    size_t intext_len;
    term iodata_result = handle_iodata(argv[3], &intext_data, &intext_len, &maybe_allocated_intext);
    if (UNLIKELY(iodata_result != OK_ATOM)) {
        result = make_crypto_error(__FILE__, __LINE__, "Expected a binary or a list", ctx);
        goto cleanup;
    }

    const void *aad_data;
    size_t aad_len;
    iodata_result = handle_iodata(argv[4], &aad_data, &aad_len, &maybe_allocated_aad);
    if (UNLIKELY(iodata_result != OK_ATOM)) {
        result = make_crypto_error(__FILE__, __LINE__, "Expected a binary or a list", ctx);
        goto cleanup;
    }

    if (encrypting) {
        size_t out_size = PSA_AEAD_ENCRYPT_OUTPUT_SIZE(aead_params->key_type, psa_algo, intext_len);
        out_buf_size = out_size;
        out_buf = malloc(out_size);
        if (IS_NULL_PTR(out_buf)) {
            result = OUT_OF_MEMORY_ATOM;
            goto cleanup;
        }

        size_t out_len = 0;
        status = psa_aead_encrypt(key_id, psa_algo, iv_data, iv_len, aad_data, aad_len, intext_data,
            intext_len, out_buf, out_size, &out_len);
        switch (status) {
            case PSA_SUCCESS:
                break;
            case PSA_ERROR_NOT_SUPPORTED:
                result = make_crypto_error(
                    __FILE__, __LINE__, "Unsupported algorithm or parameters", ctx);
                goto cleanup;
            case PSA_ERROR_INVALID_ARGUMENT:
                result = make_crypto_error(__FILE__, __LINE__, "Invalid argument", ctx);
                goto cleanup;
            default:
                result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
                goto cleanup;
        }

        size_t ct_len = out_len - tag_len;

        if (UNLIKELY(
                memory_ensure_free(ctx,
                    TERM_BINARY_HEAP_SIZE(ct_len) + TERM_BINARY_HEAP_SIZE(tag_len) + TUPLE_SIZE(2))
                != MEMORY_GC_OK)) {
            result = OUT_OF_MEMORY_ATOM;
            goto cleanup;
        }

        term ct_bin = term_from_literal_binary(out_buf, ct_len, &ctx->heap, glb);
        term tag_bin
            = term_from_literal_binary((uint8_t *) out_buf + ct_len, tag_len, &ctx->heap, glb);
        success = true;
        result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, ct_bin);
        term_put_tuple_element(result, 1, tag_bin);

    } else {
        size_t ct_len = intext_len;
        size_t combined_len = ct_len + tag_data_len;
        void *combined_buf = malloc(combined_len);
        if (IS_NULL_PTR(combined_buf)) {
            result = OUT_OF_MEMORY_ATOM;
            goto cleanup;
        }
        memcpy(combined_buf, intext_data, ct_len);
        memcpy((uint8_t *) combined_buf + ct_len, tag_data, tag_data_len);

        size_t pt_size
            = PSA_AEAD_DECRYPT_OUTPUT_SIZE(aead_params->key_type, psa_algo, combined_len);
        out_buf_size = pt_size + 1;
        out_buf = malloc(out_buf_size); // +1 to ensure valid malloc even for 0
        if (IS_NULL_PTR(out_buf)) {
            free(combined_buf);
            result = OUT_OF_MEMORY_ATOM;
            goto cleanup;
        }

        size_t pt_len = 0;
        status = psa_aead_decrypt(key_id, psa_algo, iv_data, iv_len, aad_data, aad_len,
            combined_buf, combined_len, out_buf, pt_size, &pt_len);
        free(combined_buf);

        switch (status) {
            case PSA_SUCCESS:
                break;
            case PSA_ERROR_INVALID_SIGNATURE:
                // Authentication failed, return atom `error`, no exception
                success = true;
                result = ERROR_ATOM;
                goto cleanup;
            case PSA_ERROR_NOT_SUPPORTED:
                result = make_crypto_error(
                    __FILE__, __LINE__, "Unsupported algorithm or parameters", ctx);
                goto cleanup;
            case PSA_ERROR_INVALID_ARGUMENT:
                result = make_crypto_error(__FILE__, __LINE__, "Invalid argument", ctx);
                goto cleanup;
            default:
                result = make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx);
                goto cleanup;
        }

        if (UNLIKELY(memory_ensure_free(ctx, TERM_BINARY_HEAP_SIZE(pt_len)) != MEMORY_GC_OK)) {
            result = OUT_OF_MEMORY_ATOM;
            goto cleanup;
        }

        success = true;
        result = term_from_literal_binary(out_buf, pt_len, &ctx->heap, glb);
    }

cleanup:
    psa_destroy_key(key_id);
    secure_free(maybe_allocated_intext, intext_len);
    free(maybe_allocated_aad);
    secure_free(out_buf, out_buf_size);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

#endif

static term nif_crypto_hash_equals(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term mac1_term = argv[0];
    term mac2_term = argv[1];

    if (UNLIKELY(!term_is_binary(mac1_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Expected a binary", ctx));
    }
    if (UNLIKELY(!term_is_binary(mac2_term))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Expected a binary", ctx));
    }

    size_t mac1_len = term_binary_size(mac1_term);
    size_t mac2_len = term_binary_size(mac2_term);

    if (UNLIKELY(mac1_len != mac2_len)) {
        RAISE_ERROR(
            make_crypto_error(__FILE__, __LINE__, "Binaries must have the same length", ctx));
    }

    const void *mac1 = term_binary_data(mac1_term);
    const void *mac2 = term_binary_data(mac2_term);

#ifdef AVM_HAVE_MBEDTLS_CT_MEMCMP
    int cmp = mbedtls_ct_memcmp(mac1, mac2, mac1_len);
#else
    // Constant-time fallback for older mbedTLS (< 2.28.0 or 3.0.x)
    const unsigned char *pa = mac1;
    const unsigned char *pb = mac2;
    unsigned char diff = 0;
    for (size_t i = 0; i < mac1_len; i++) {
        diff |= (unsigned char) (pa[i] ^ pb[i]);
    }
    int cmp = diff;
#endif

    return cmp == 0 ? TRUE_ATOM : FALSE_ATOM;
}

#ifdef AVM_HAVE_PBKDF2_HMAC
#if MBEDTLS_VERSION_NUMBER < 0x04000000
static const AtomStringIntPair md_hash_algorithm_table[] = {
    { ATOM_STR("\x3", "sha"), MBEDTLS_MD_SHA1 },
    { ATOM_STR("\x6", "sha224"), MBEDTLS_MD_SHA224 },
    { ATOM_STR("\x6", "sha256"), MBEDTLS_MD_SHA256 },
    { ATOM_STR("\x6", "sha384"), MBEDTLS_MD_SHA384 },
    { ATOM_STR("\x6", "sha512"), MBEDTLS_MD_SHA512 },
    { ATOM_STR("\x3", "md5"), MBEDTLS_MD_MD5 },
    { ATOM_STR("\x9", "ripemd160"), MBEDTLS_MD_RIPEMD160 },

    SELECT_INT_DEFAULT(MBEDTLS_MD_NONE)
};
#endif

static term nif_crypto_pbkdf2_hmac(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    GlobalContext *glb = ctx->global;

    term digest_type_term = argv[0];
    // argv[1] is password, argv[2] is salt, argv[3] is iterations, argv[4] is key_len

    bool success = false;
    term result = ERROR_ATOM;

    void *maybe_allocated_password = NULL;
    size_t password_len = 0;
    void *maybe_allocated_salt = NULL;
    size_t salt_len = 0;
    void *derived_key = NULL;
    avm_int_t derived_key_len = 0;

#if MBEDTLS_VERSION_NUMBER >= 0x04000000
    psa_algorithm_t hash_alg = atom_to_psa_hash_alg(digest_type_term, glb);
    if (UNLIKELY(hash_alg == PSA_ALG_NONE)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unknown digest type", ctx));
    }
#else
    mbedtls_md_type_t md_type
        = interop_atom_term_select_int(md_hash_algorithm_table, digest_type_term, glb);
    if (UNLIKELY(md_type == MBEDTLS_MD_NONE)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unknown digest type", ctx));
    }
#endif

    term password_term = argv[1];
    const void *password;
    term iodata_handle_result
        = handle_iodata(password_term, &password, &password_len, &maybe_allocated_password);
    if (UNLIKELY(iodata_handle_result != OK_ATOM)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Expected a binary or a list", ctx));
    }

    // from this point onward use `goto cleanup` in order to raise and free all buffers

    term salt_term = argv[2];
    const void *salt;
    iodata_handle_result = handle_iodata(salt_term, &salt, &salt_len, &maybe_allocated_salt);
    if (UNLIKELY(iodata_handle_result != OK_ATOM)) {
        result = make_crypto_error(__FILE__, __LINE__, "Expected a binary or a list", ctx);
        goto cleanup;
    }

    term iterations_term = argv[3];
    if (UNLIKELY(!term_is_uint32(iterations_term))) {
        result
            = make_crypto_error(__FILE__, __LINE__, "Iterations must be a positive integer", ctx);
        goto cleanup;
    }
    uint32_t iterations = term_to_uint32(iterations_term);
    if (UNLIKELY(iterations == 0)) {
        result
            = make_crypto_error(__FILE__, __LINE__, "Iterations must be a positive integer", ctx);
        goto cleanup;
    }

    term key_len_term = argv[4];
    if (UNLIKELY(!term_is_pos_int(key_len_term))) {
        result = make_crypto_error(__FILE__, __LINE__, "KeyLen must be a positive integer", ctx);
        goto cleanup;
    }
    derived_key_len = term_to_int(key_len_term);

    derived_key = malloc(derived_key_len);
    if (IS_NULL_PTR(derived_key)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

#if MBEDTLS_VERSION_NUMBER >= 0x04000000
    do_psa_init();
    psa_key_derivation_operation_t operation = PSA_KEY_DERIVATION_OPERATION_INIT;

    psa_status_t status = psa_key_derivation_setup(&operation, PSA_ALG_PBKDF2_HMAC(hash_alg));
    if (UNLIKELY(status != PSA_SUCCESS)) {
        psa_key_derivation_abort(&operation);
        result = make_crypto_error(__FILE__, __LINE__, "Key derivation failed", ctx);
        goto cleanup;
    }

    status = psa_key_derivation_input_integer(
        &operation, PSA_KEY_DERIVATION_INPUT_COST, iterations);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        psa_key_derivation_abort(&operation);
        result = make_crypto_error(__FILE__, __LINE__, "Key derivation failed", ctx);
        goto cleanup;
    }

    status = psa_key_derivation_input_bytes(
        &operation, PSA_KEY_DERIVATION_INPUT_SALT, salt, salt_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        psa_key_derivation_abort(&operation);
        result = make_crypto_error(__FILE__, __LINE__, "Key derivation failed", ctx);
        goto cleanup;
    }

    status = psa_key_derivation_input_bytes(
        &operation, PSA_KEY_DERIVATION_INPUT_PASSWORD, password, password_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        psa_key_derivation_abort(&operation);
        result = make_crypto_error(__FILE__, __LINE__, "Key derivation failed", ctx);
        goto cleanup;
    }

    status = psa_key_derivation_output_bytes(&operation, derived_key, derived_key_len);
    psa_key_derivation_abort(&operation);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        result = make_crypto_error(__FILE__, __LINE__, "Key derivation failed", ctx);
        goto cleanup;
    }
#else
#if MBEDTLS_VERSION_NUMBER >= 0x03030000
    // mbedtls_pkcs5_pbkdf2_hmac_ext is available since 3.3.0
    int ret = mbedtls_pkcs5_pbkdf2_hmac_ext(
        md_type, password, password_len, salt, salt_len, iterations, derived_key_len, derived_key);
#else
    const mbedtls_md_info_t *md_info = mbedtls_md_info_from_type(md_type);
    int ret;
    if (UNLIKELY(md_info == NULL)) {
        ret = MBEDTLS_ERR_PKCS5_BAD_INPUT_DATA;
    } else {
        mbedtls_md_context_t md_ctx;
        mbedtls_md_init(&md_ctx);
        ret = mbedtls_md_setup(&md_ctx, md_info, 1);
        if (ret == 0) {
            ret = mbedtls_pkcs5_pbkdf2_hmac(&md_ctx, password, password_len, salt, salt_len,
                iterations, derived_key_len, derived_key);
        }
        mbedtls_md_free(&md_ctx);
    }
#endif
    if (UNLIKELY(ret != 0)) {
        result = make_crypto_error(__FILE__, __LINE__, "Key derivation failed", ctx);
        goto cleanup;
    }
#endif

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BINARY_HEAP_SIZE(derived_key_len)) != MEMORY_GC_OK)) {
        result = OUT_OF_MEMORY_ATOM;
        goto cleanup;
    }

    success = true;
    result = term_from_literal_binary(derived_key, derived_key_len, &ctx->heap, glb);

cleanup:
    secure_free(derived_key, derived_key_len);
    secure_free(maybe_allocated_password, password_len);
    secure_free(maybe_allocated_salt, salt_len);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}
#endif

// not static since we are using it elsewhere to provide backward compatibility
term nif_crypto_strong_rand_bytes(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term count_term = argv[0];
    VALIDATE_VALUE(count_term, term_is_integer);
    avm_int_t out_len = term_to_int(count_term);
    if (out_len < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int ensure_size = term_binary_heap_size(out_len);
    if (UNLIKELY(memory_ensure_free(ctx, ensure_size) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

#if MBEDTLS_VERSION_NUMBER >= 0x04000000
    do_psa_init();
    term out_bin = term_create_uninitialized_binary(out_len, &ctx->heap, ctx->global);
    unsigned char *out = (unsigned char *) term_binary_data(out_bin);

    psa_status_t status = psa_generate_random(out, out_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Failed random", ctx));
    }

    return out_bin;
#else
    mbedtls_ctr_drbg_context *rnd_ctx = sys_mbedtls_get_ctr_drbg_context_lock(ctx->global);
    if (IS_NULL_PTR(rnd_ctx)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Failed CTR_DRBG init", ctx));
    }

    term out_bin = term_create_uninitialized_binary(out_len, &ctx->heap, ctx->global);
    unsigned char *out = (unsigned char *) term_binary_data(out_bin);

    int err = mbedtls_ctr_drbg_random(rnd_ctx, out, out_len);
    sys_mbedtls_ctr_drbg_context_unlock(ctx->global);
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Failed random", ctx));
    }

    return out_bin;
#endif
}

static const char *get_mbedtls_version_string_full(char *buf, size_t buf_size)
{
#if defined(MBEDTLS_VERSION_C)
    /*
     * Mbed TLS 2.x / 3.x: void mbedtls_version_get_string_full(char *string)
     * Mbed TLS 4.x:       const char *mbedtls_version_get_string_full(void)
     */
#if defined(MBEDTLS_VERSION_MAJOR) && (MBEDTLS_VERSION_MAJOR >= 4)
    UNUSED(buf);
    UNUSED(buf_size);
    return mbedtls_version_get_string_full();
#else
    if (buf_size < 18) {
        return MBEDTLS_VERSION_STRING_FULL;
    }
    mbedtls_version_get_string_full(buf);
    return buf;
#endif
#else
    // No runtime getter for mbedTLS version string, use build time macro
    UNUSED(buf);
    UNUSED(buf_size);
    return MBEDTLS_VERSION_STRING_FULL;
#endif
}

term nif_crypto_info_lib(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    const char *mbedtls_str = "mbedtls";
    size_t mbedtls_len = strlen("mbedtls");

    char version_string_buf[18];
    const char *version_string
        = get_mbedtls_version_string_full(version_string_buf, sizeof(version_string_buf));
    size_t version_string_len = strlen(version_string);

#ifdef HAVE_LIBSODIUM
    const char *libsodium_str = "libsodium";
    size_t libsodium_len = strlen("libsodium");
    const char *libsodium_version_str = sodium_version_string();
    size_t libsodium_version_len = strlen(libsodium_version_str);
    int libsodium_major = sodium_library_version_major();
    int libsodium_minor = sodium_library_version_minor();
    int64_t libsodium_version_number = (int64_t) libsodium_major * 10000 + libsodium_minor;

    if (UNLIKELY(memory_ensure_free(ctx,
                     LIST_SIZE(2, TUPLE_SIZE(3)) + TERM_BINARY_HEAP_SIZE(mbedtls_len)
                         + TERM_BINARY_HEAP_SIZE(version_string_len) + BOXED_INT64_SIZE
                         + TERM_BINARY_HEAP_SIZE(libsodium_len)
                         + TERM_BINARY_HEAP_SIZE(libsodium_version_len) + BOXED_INT64_SIZE)
            != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
#else
    if (UNLIKELY(memory_ensure_free(ctx,
                     LIST_SIZE(1, TUPLE_SIZE(3)) + TERM_BINARY_HEAP_SIZE(mbedtls_len)
                         + TERM_BINARY_HEAP_SIZE(version_string_len) + BOXED_INT64_SIZE)
            != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
#endif

    term mbedtls_term = term_from_literal_binary(mbedtls_str, mbedtls_len, &ctx->heap, ctx->global);
    term version_term = term_make_maybe_boxed_int64(MBEDTLS_VERSION_NUMBER, &ctx->heap);
    term version_string_term
        = term_from_literal_binary(version_string, version_string_len, &ctx->heap, ctx->global);

    term mbedtls_tuple = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(mbedtls_tuple, 0, mbedtls_term);
    term_put_tuple_element(mbedtls_tuple, 1, version_term);
    term_put_tuple_element(mbedtls_tuple, 2, version_string_term);

#ifdef HAVE_LIBSODIUM
    term libsodium_term
        = term_from_literal_binary(libsodium_str, libsodium_len, &ctx->heap, ctx->global);
    term libsodium_version_term = term_make_maybe_boxed_int64(libsodium_version_number, &ctx->heap);
    term libsodium_version_string_term = term_from_literal_binary(
        libsodium_version_str, libsodium_version_len, &ctx->heap, ctx->global);

    term libsodium_tuple = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(libsodium_tuple, 0, libsodium_term);
    term_put_tuple_element(libsodium_tuple, 1, libsodium_version_term);
    term_put_tuple_element(libsodium_tuple, 2, libsodium_version_string_term);

    term list = term_list_prepend(mbedtls_tuple, term_nil(), &ctx->heap);
    return term_list_prepend(libsodium_tuple, list, &ctx->heap);
#else
    return term_list_prepend(mbedtls_tuple, term_nil(), &ctx->heap);
#endif
}

static const struct Nif crypto_hash_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_hash
};
static const struct Nif crypto_crypto_one_time_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_crypto_one_time
};
#ifdef HAVE_PSA_CRYPTO
static const struct Nif crypto_generate_key_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_generate_key
};
static const struct Nif crypto_compute_key_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_compute_key
};
#ifdef CRYPTO_SIGN_AVAILABLE
static const struct Nif crypto_sign_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_sign
};
#endif
#ifdef CRYPTO_VERIFY_AVAILABLE
static const struct Nif crypto_verify_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_verify
};
#endif
static const struct Nif crypto_mac_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_mac
};
static const struct Nif crypto_mac_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_mac_init
};
static const struct Nif crypto_mac_update_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_mac_update
};
static const struct Nif crypto_mac_final_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_mac_final
};
static const struct Nif crypto_mac_finalN_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_mac_finalN
};
static const struct Nif crypto_hash_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_hash_init
};
static const struct Nif crypto_hash_update_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_hash_update
};
static const struct Nif crypto_hash_final_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_hash_final
};
static const struct Nif crypto_crypto_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_crypto_init
};
static const struct Nif crypto_crypto_update_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_crypto_update
};
static const struct Nif crypto_crypto_final_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_crypto_final
};
static const struct Nif crypto_crypto_one_time_aead_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_crypto_one_time_aead
};
#endif
#ifdef AVM_HAVE_PBKDF2_HMAC
static const struct Nif crypto_pbkdf2_hmac_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_pbkdf2_hmac
};
#endif
static const struct Nif crypto_hash_equals_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_hash_equals
};
static const struct Nif crypto_strong_rand_bytes_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_strong_rand_bytes
};
static const struct Nif crypto_info_lib = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_info_lib
};

//
// Entrypoints
//

const struct Nif *otp_crypto_nif_get_nif(const char *nifname)
{
    if (strncmp("crypto:", nifname, 7) == 0) {
        const char *rest = nifname + 7;
        if (strcmp("hash/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_hash_nif;
        }
        if (strcmp("crypto_one_time/4", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_crypto_one_time_nif;
        }
        if (strcmp("crypto_one_time/5", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_crypto_one_time_nif;
        }
#ifdef HAVE_PSA_CRYPTO
        if (strcmp("generate_key/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_generate_key_nif;
        }
        if (strcmp("compute_key/4", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_compute_key_nif;
        }
#ifdef CRYPTO_SIGN_AVAILABLE
        if (strcmp("sign/4", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_sign_nif;
        }
#endif
#ifdef CRYPTO_VERIFY_AVAILABLE
        if (strcmp("verify/5", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_verify_nif;
        }
#endif
        if (strcmp("mac/4", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_mac_nif;
        }
        if (strcmp("mac_init/3", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_mac_init_nif;
        }
        if (strcmp("mac_update/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_mac_update_nif;
        }
        if (strcmp("mac_final/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_mac_final_nif;
        }
        if (strcmp("mac_finalN/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_mac_finalN_nif;
        }
        if (strcmp("hash_init/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_hash_init_nif;
        }
        if (strcmp("hash_update/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_hash_update_nif;
        }
        if (strcmp("hash_final/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_hash_final_nif;
        }
        if (strcmp("crypto_init/4", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_crypto_init_nif;
        }
        if (strcmp("crypto_update/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_crypto_update_nif;
        }
        if (strcmp("crypto_final/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_crypto_final_nif;
        }
        if (strcmp("crypto_one_time_aead/6", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_crypto_one_time_aead_nif;
        }
        if (strcmp("crypto_one_time_aead/7", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_crypto_one_time_aead_nif;
        }
#endif
#ifdef AVM_HAVE_PBKDF2_HMAC
        if (strcmp("pbkdf2_hmac/5", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_pbkdf2_hmac_nif;
        }
#endif
        if (strcmp("hash_equals/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_hash_equals_nif;
        }
        if (strcmp("strong_rand_bytes/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_strong_rand_bytes_nif;
        }
        if (strcmp("info_lib/0", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_info_lib;
        }
    }
    return NULL;
}
