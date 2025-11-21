/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Fred Dushin <fred@dushin.net>
 * Copyright 2023 Davide Bettio <davide@uninstall.it>
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
#include <globalcontext.h>
#include <interop.h>
#include <nifs.h>
#include <sys_mbedtls.h>
#include <term.h>
#include <term_typedef.h>

#if MBEDTLS_VERSION_NUMBER < 0x04000000
#include <mbedtls/cipher.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>
#include <mbedtls/md5.h>
#include <mbedtls/sha1.h>
#include <mbedtls/sha256.h>
#include <mbedtls/sha512.h>
#endif
#include <mbedtls/version.h>
#if MBEDTLS_VERSION_NUMBER >= 0x04000000
#include <psa/crypto.h>
#endif

// #define ENABLE_TRACE
#include "trace.h"

#define MAX_MD_SIZE 64

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
    psa_algorithm_t alg = atom_to_psa_hash_alg(type, ctx->global);
    if (alg == PSA_ALG_NONE) {
        RAISE_ERROR(BADARG_ATOM);
    }
    digest_len = PSA_HASH_LENGTH(alg);

    psa_hash_operation_t operation = PSA_HASH_OPERATION_INIT;
    psa_status_t status = psa_hash_setup(&operation, alg);
    if (UNLIKELY(status != PSA_SUCCESS)) {
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
                RAISE_ERROR(BADARG_ATOM)
            }
            digest_len = 16;
            break;
        }
        case CryptoSha1: {
            if (UNLIKELY(!do_sha1_hash(data, digest))) {
                RAISE_ERROR(BADARG_ATOM)
            }
            digest_len = 20;
            break;
        }
        case CryptoSha224: {
            if (UNLIKELY(!do_sha256_hash_true(data, digest))) {
                RAISE_ERROR(BADARG_ATOM)
            }
            digest_len = 28;
            break;
        }
        case CryptoSha256: {
            if (UNLIKELY(!do_sha256_hash_false(data, digest))) {
                RAISE_ERROR(BADARG_ATOM)
            }
            digest_len = 32;
            break;
        }
        case CryptoSha384: {
            if (UNLIKELY(!do_sha512_hash_true(data, digest))) {
                RAISE_ERROR(BADARG_ATOM)
            }
            digest_len = 48;
            break;
        }
        case CryptoSha512: {
            if (UNLIKELY(!do_sha512_hash_false(data, digest))) {
                RAISE_ERROR(BADARG_ATOM)
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

static term make_crypto_error(const char *file, int line, const char *message, Context *ctx)
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
    term_put_tuple_element(err_t, 0, BADARG_ATOM);
    term_put_tuple_element(err_t, 1, file_line_t);
    term_put_tuple_element(err_t, 2, message_t);

    return err_t;
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

    const void *key_data;
    size_t key_len;
    term result_t = handle_iodata(key, &key_data, &key_len, &allocated_key_data);
    if (UNLIKELY(result_t != OK_ATOM)) {
        error_atom = result_t;
        goto raise_error;
    }

    const void *iv_data = NULL;
    size_t iv_len = 0;
    if (has_iv) {
        result_t = handle_iodata(iv, &iv_data, &iv_len, &allocated_iv_data);
        if (UNLIKELY(result_t != OK_ATOM)) {
            error_atom = result_t;
            goto raise_error;
        }
    }

    const void *data_data;
    size_t data_size;
    result_t = handle_iodata(data, &data_data, &data_size, &allocated_data_data);
    if (UNLIKELY(result_t != OK_ATOM)) {
        error_atom = result_t;
        goto raise_error;
    }

#if MBEDTLS_VERSION_NUMBER >= 0x04000000
    bool encrypt = true;
    bool padding_pkcs7 = false;

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
            error_atom = BADARG_ATOM;
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

    psa_key_id_t key_id;
    psa_status_t status = psa_import_key(&attributes, key_data, key_len, &key_id);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        error_atom = BADARG_ATOM;
        goto raise_error;
    }

    size_t output_size = PSA_CIPHER_ENCRYPT_OUTPUT_SIZE(key_type, alg, data_size);
    if (!encrypt) {
        output_size = PSA_CIPHER_DECRYPT_OUTPUT_SIZE(key_type, alg, data_size);
    }
    void *temp_buf = malloc(output_size);
    if (IS_NULL_PTR(temp_buf)) {
        psa_destroy_key(key_id);
        error_atom = OUT_OF_MEMORY_ATOM;
        goto raise_error;
    }

    size_t output_len;
    psa_cipher_operation_t operation = PSA_CIPHER_OPERATION_INIT;
    if (encrypt) {
        status = psa_cipher_encrypt_setup(&operation, key_id, alg);
    } else {
        status = psa_cipher_decrypt_setup(&operation, key_id, alg);
    }
    if (UNLIKELY(status != PSA_SUCCESS)) {
        psa_destroy_key(key_id);
        free(temp_buf);
        char err_msg[24];
        snprintf(err_msg, sizeof(err_msg), "Error %d", (int) status);
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, err_msg, ctx));
    }

    if (iv_len > 0) {
        status = psa_cipher_set_iv(&operation, iv_data, iv_len);
        if (UNLIKELY(status != PSA_SUCCESS)) {
            psa_cipher_abort(&operation);
            psa_destroy_key(key_id);
            free(temp_buf);
            char err_msg[24];
            snprintf(err_msg, sizeof(err_msg), "Error %d", (int) status);
            RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, err_msg, ctx));
        }
    }

    size_t update_len = 0;
    status = psa_cipher_update(&operation, data_data, data_size, temp_buf, output_size, &update_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        psa_cipher_abort(&operation);
        psa_destroy_key(key_id);
        free(temp_buf);
        char err_msg[24];
        snprintf(err_msg, sizeof(err_msg), "Error %d", (int) status);
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, err_msg, ctx));
    }

    size_t finish_len = 0;
    status = psa_cipher_finish(&operation, (uint8_t *) temp_buf + update_len, output_size - update_len, &finish_len);
    if (UNLIKELY(status != PSA_SUCCESS)) {
        psa_cipher_abort(&operation);
        psa_destroy_key(key_id);
        free(temp_buf);
        char err_msg[24];
        snprintf(err_msg, sizeof(err_msg), "Error %d", (int) status);
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, err_msg, ctx));
    }
    output_len = update_len + finish_len;

    psa_destroy_key(key_id);

    free(allocated_key_data);
    free(allocated_iv_data);
    free(allocated_data_data);

    int ensure_size = term_binary_heap_size(output_len);
    if (UNLIKELY(memory_ensure_free(ctx, ensure_size) != MEMORY_GC_OK)) {
        free(temp_buf);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term out = term_from_literal_binary(temp_buf, output_len, &ctx->heap, ctx->global);
    free(temp_buf);
    return out;
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
            error_atom = BADARG_ATOM;
            goto raise_error;
        }
    }

    const mbedtls_cipher_info_t *cipher_info = mbedtls_cipher_info_from_type(cipher);

    mbedtls_cipher_context_t cipher_ctx;

    void *temp_buf = NULL;

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

    size_t temp_buf_size = data_size + block_size;
    temp_buf = malloc(temp_buf_size);
    if (IS_NULL_PTR(temp_buf)) {
        error_atom = OUT_OF_MEMORY_ATOM;
        goto raise_error;
    }

    // from this point onward use `mbed_error` in order to raise and free all buffers

    result = mbedtls_cipher_crypt(
        &cipher_ctx, iv_data, iv_len, data_data, data_size, temp_buf, &temp_buf_size);
    if (result != 0 && result != MBEDTLS_ERR_CIPHER_FULL_BLOCK_EXPECTED) {
        source_line = __LINE__;
        goto mbed_error;
    }
    mbedtls_cipher_free(&cipher_ctx);

    free(allocated_key_data);
    free(allocated_iv_data);
    free(allocated_data_data);

    int ensure_size = term_binary_heap_size(temp_buf_size);
    if (UNLIKELY(memory_ensure_free(ctx, ensure_size) != MEMORY_GC_OK)) {
        free(temp_buf);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term out = term_from_literal_binary(temp_buf, temp_buf_size, &ctx->heap, ctx->global);

    free(temp_buf);

    return out;

mbed_error:
    free(temp_buf);
    free(allocated_key_data);
    free(allocated_iv_data);
    free(allocated_data_data);

    char err_msg[24];
    snprintf(err_msg, sizeof(err_msg), "Error %x", -result);
    RAISE_ERROR(make_crypto_error(__FILE__, source_line, err_msg, ctx));
#endif

raise_error:
    free(allocated_key_data);
    free(allocated_iv_data);
    free(allocated_data_data);
    RAISE_ERROR(error_atom);
}

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
    psa_status_t status = psa_crypto_init();
    if (UNLIKELY(status != PSA_SUCCESS)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Failed PSA init", ctx));
    }

    term out_bin = term_create_uninitialized_binary(out_len, &ctx->heap, ctx->global);
    unsigned char *out = (unsigned char *) term_binary_data(out_bin);

    status = psa_generate_random(out, out_len);
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

static const struct Nif crypto_hash_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_hash
};
static const struct Nif crypto_crypto_one_time_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_crypto_one_time
};
static const struct Nif crypto_strong_rand_bytes_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_strong_rand_bytes
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
        if (strcmp("strong_rand_bytes/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &crypto_strong_rand_bytes_nif;
        }
    }
    return NULL;
}
