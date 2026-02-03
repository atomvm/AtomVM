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

#include <mbedtls/cipher.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>
#include <mbedtls/md5.h>
#include <mbedtls/sha1.h>
#include <mbedtls/sha256.h>
#include <mbedtls/sha512.h>
#include <mbedtls/version.h>

#ifdef MBEDTLS_PSA_CRYPTO_C
#include <mbedtls/psa_util.h>
#include <psa/crypto.h>
#endif

// #define ENABLE_TRACE
#include "term.h"
#include "trace.h"

#if MBEDTLS_VERSION_NUMBER > 0x03060100
#define HAVE_MBEDTLS_ECDSA_RAW_TO_DER 1
#define HAVE_MBEDTLS_ECDSA_DER_TO_RAW 1
#endif

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

static const AtomStringIntPair crypto_algorithm_table[] = {
    { ATOM_STR("\x3", "md5"), CryptoMd5 },
    { ATOM_STR("\x3", "sha"), CryptoSha1 },
    { ATOM_STR("\x6", "sha224"), CryptoSha224 },
    { ATOM_STR("\x6", "sha256"), CryptoSha256 },
    { ATOM_STR("\x6", "sha384"), CryptoSha384 },
    { ATOM_STR("\x6", "sha512"), CryptoSha512 },
    SELECT_INT_DEFAULT(CryptoInvalidAlgorithm)
};

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

static term nif_crypto_hash(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term type = argv[0];
    VALIDATE_VALUE(type, term_is_atom);
    term data = argv[1];

    unsigned char digest[MAX_MD_SIZE];
    size_t digest_len = 0;

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

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(digest_len)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_from_literal_binary(digest, digest_len, &ctx->heap, ctx->global);
}

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
    mbedtls_cipher_type_t cipher
        = interop_atom_term_select_int(cipher_table, cipher_term, ctx->global);
    if (UNLIKELY(cipher == MBEDTLS_CIPHER_NONE)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unknown cipher", ctx));
    }

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

raise_error:
    free(allocated_key_data);
    free(allocated_iv_data);
    free(allocated_data_data);
    RAISE_ERROR(error_atom);

mbed_error:
    free(temp_buf);
    free(allocated_key_data);
    free(allocated_iv_data);
    free(allocated_data_data);

    char err_msg[24];
    snprintf(err_msg, sizeof(err_msg), "Error %x", -result);
    RAISE_ERROR(make_crypto_error(__FILE__, source_line, err_msg, ctx));
}

#ifdef MBEDTLS_PSA_CRYPTO_C

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
    // prime256v1
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
    // prime256v1
    { ATOM_STR("\x9", "secp384r1"), Secp384r1 },
    { ATOM_STR("\x9", "secp521r1"), Secp521r1 },
    { ATOM_STR("\x9", "secp256k1"), Secp256k1 },
    { ATOM_STR("\xF", "brainpoolP256r1"), BrainpoolP256r1 },
    { ATOM_STR("\xF", "brainpoolP384r1"), BrainpoolP384r1 },
    { ATOM_STR("\xF", "brainpoolP512r1"), BrainpoolP512r1 },

    SELECT_INT_DEFAULT(InvalidPkType)
};

static void do_psa_init(void)
{
    if (UNLIKELY(psa_crypto_init() != PSA_SUCCESS)) {
        abort();
    }
}

static term nif_crypto_generate_key(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    enum pk_type_t key_type = interop_atom_term_select_int(pk_type_table, argv[0], glb);
    enum pk_param_t pk_param = interop_atom_term_select_int(pk_param_table, argv[1], glb);

    psa_key_type_t psa_key_type;
    size_t psa_key_bits;
    switch (key_type) {
        case Eddh:
            // In OTP cotext: Eddh is Ecdh only on Montgomery curves
            psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_MONTGOMERY);
            switch (pk_param) {
                case X25519:
                    psa_key_bits = 255;
                    break;
                case X448:
                    psa_key_bits = 448;
                    break;
                default:
                    RAISE_ERROR(BADARG_ATOM);
            }
            break;
        case Eddsa:
            psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_TWISTED_EDWARDS);
            switch (pk_param) {
                case Ed25519:
                    psa_key_bits = 255;
                    break;
                case Ed448:
                    psa_key_bits = 448;
                    break;
                default:
                    RAISE_ERROR(BADARG_ATOM);
            }
            break;
        case Ecdh:
            switch (pk_param) {
                case X25519:
                    psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_MONTGOMERY);
                    psa_key_bits = 255;
                    break;
                case X448:
                    psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_MONTGOMERY);
                    psa_key_bits = 448;
                    break;
                case Secp256k1:
                    psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_SECP_K1);
                    psa_key_bits = 256;
                    break;
                case Secp256r1:
                    psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_SECP_R1);
                    psa_key_bits = 256;
                    break;
                case Secp384r1:
                    psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_SECP_R1);
                    psa_key_bits = 384;
                    break;
                case Secp521r1:
                    psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_SECP_R1);
                    psa_key_bits = 521;
                    break;
                default:
                    RAISE_ERROR(BADARG_ATOM);
            }
            break;

        default:
            RAISE_ERROR(BADARG_ATOM);
    }

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
    if (exported_priv) {
        memset(exported_priv, 0, exported_priv_size);
    }
    free(exported_priv);
    if (exported_pub) {
        memset(exported_pub, 0, exported_pub_size);
    }
    free(exported_pub);

    if (UNLIKELY(!successful)) {
        RAISE_ERROR(result);
    }

    return result;
}

static term nif_crypto_compute_key(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    enum pk_type_t key_type = interop_atom_term_select_int(pk_type_table, argv[0], glb);
    enum pk_param_t pk_param = interop_atom_term_select_int(pk_param_table, argv[3], glb);

    psa_algorithm_t psa_algo;
    psa_key_type_t psa_key_type;
    size_t psa_key_bits;

    switch (key_type) {
        case Eddh:
            // In OTP cotext: Eddh is Ecdh only on Montgomery curves
            psa_algo = PSA_ALG_ECDH;
            psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_MONTGOMERY);
            switch (pk_param) {
                case X25519:
                    psa_key_bits = 255;
                    break;
                case X448:
                    psa_key_bits = 448;
                    break;
                default:
                    RAISE_ERROR(BADARG_ATOM);
            }
            break;
        case Ecdh:
            psa_algo = PSA_ALG_ECDH;
            psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_MONTGOMERY);
            switch (pk_param) {
                case X25519:
                    psa_key_bits = 255;
                    break;
                case X448:
                    psa_key_bits = 448;
                    break;
                default:
                    RAISE_ERROR(BADARG_ATOM);
            }
            break;

        default:
            RAISE_ERROR(BADARG_ATOM);
    }

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
    uint8_t *shared_out = malloc(shared_out_size);
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
    if (shared_out) {
        memset(shared_out, 0, shared_out_size);
    }
    free(shared_out);

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
    { ATOM_STR("\x6", "sha3_224"), PSA_ALG_SHA_224 },
    { ATOM_STR("\x6", "sha3_256"), PSA_ALG_SHA_256 },
    { ATOM_STR("\x6", "sha3_384"), PSA_ALG_SHA_384 },
    { ATOM_STR("\x6", "sha3_512"), PSA_ALG_SHA_512 },
    { ATOM_STR("\x3", "md5"), PSA_ALG_MD5 },
    { ATOM_STR("\x6", "ripemd160"), PSA_ALG_RIPEMD160 },

    SELECT_INT_DEFAULT(PSA_ALG_NONE)
};

#ifdef HAVE_MBEDTLS_ECDSA_RAW_TO_DER

#define CRYPTO_SIGN_AVAILABLE 1

static term nif_crypto_sign(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    term alg_term = argv[0];
    if (UNLIKELY(
            !globalcontext_is_term_equal_to_atom_string(glb, alg_term, ATOM_STR("\x5", "ecdsa")))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Invalid public key", ctx));
    }

    term hash_algo_term = argv[1];
    psa_algorithm_t hash_algo
        = interop_atom_term_select_int(psa_hash_algorithm_table, hash_algo_term, glb);
    if (UNLIKELY(hash_algo == PSA_ALG_NONE)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad digest type", ctx));
    }
    psa_algorithm_t psa_key_alg = PSA_ALG_ECDSA(hash_algo);

    term data_term = argv[2];
    VALIDATE_VALUE(data_term, term_is_binary);
    const void *data = term_binary_data(data_term);
    size_t data_len = term_binary_size(data_term);

    term key_list_term = argv[3];
    VALIDATE_VALUE(key_list_term, term_is_nonempty_list);

    term priv_term = term_get_list_head(key_list_term);
    VALIDATE_VALUE(priv_term, term_is_binary);
    const void *priv = term_binary_data(priv_term);
    size_t priv_len = term_binary_size(priv_term);

    term key_list_term_tail = term_get_list_tail(key_list_term);
    VALIDATE_VALUE(key_list_term_tail, term_is_nonempty_list);
    term priv_param_term = term_get_list_head(key_list_term_tail);

    enum pk_param_t pk_param = interop_atom_term_select_int(pk_param_table, priv_param_term, glb);
    psa_key_type_t psa_key_type;
    size_t psa_key_bits;

    switch (pk_param) {
        case Secp256k1:
            psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_SECP_K1);
            psa_key_bits = 256;
            break;
        case Secp256r1:
            psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_SECP_R1);
            psa_key_bits = 256;
            break;
        case Secp384r1:
            psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_SECP_R1);
            psa_key_bits = 384;
            break;
        case Secp521r1:
            psa_key_type = PSA_KEY_TYPE_ECC_KEY_PAIR(PSA_ECC_FAMILY_SECP_R1);
            psa_key_bits = 521;
            break;
        default:
            RAISE_ERROR(
                make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA private key", ctx));
    }

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
    size_t sig_der_size = MBEDTLS_ECDSA_MAX_SIG_LEN(psa_key_bits);
    void *sig_der = NULL;

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

    if (sig_raw) {
        memset(sig_raw, 0, sig_raw_len);
    }
    free(sig_raw);

    if (sig_der) {
        memset(sig_der, 0, sig_der_size);
    }
    free(sig_der);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

#endif

#ifdef HAVE_MBEDTLS_ECDSA_DER_TO_RAW

#define CRYPTO_VERIFY_AVAILABLE 1

static term nif_crypto_verify(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    do_psa_init();

    GlobalContext *glb = ctx->global;

    term alg_term = argv[0];
    if (UNLIKELY(
            !globalcontext_is_term_equal_to_atom_string(glb, alg_term, ATOM_STR("\x5", "ecdsa")))) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Invalid public key", ctx));
    }

    term hash_algo_term = argv[1];
    psa_algorithm_t hash_algo
        = interop_atom_term_select_int(psa_hash_algorithm_table, hash_algo_term, glb);
    if (UNLIKELY(hash_algo == PSA_ALG_NONE)) {
        RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad digest type", ctx));
    }
    psa_algorithm_t psa_key_alg = PSA_ALG_ECDSA(hash_algo);

    term data_term = argv[2];
    VALIDATE_VALUE(data_term, term_is_binary);
    const void *data = term_binary_data(data_term);
    size_t data_len = term_binary_size(data_term);

    term sig_der_term = argv[3];
    VALIDATE_VALUE(sig_der_term, term_is_binary);
    const void *sig_der = term_binary_data(sig_der_term);
    size_t sig_der_len = term_binary_size(sig_der_term);

    term key_list_term = argv[4];
    VALIDATE_VALUE(key_list_term, term_is_nonempty_list);

    term pub_term = term_get_list_head(key_list_term);
    VALIDATE_VALUE(pub_term, term_is_binary);
    const void *pub = term_binary_data(pub_term);
    size_t pub_len = term_binary_size(pub_term);

    term key_list_term_tail = term_get_list_tail(key_list_term);
    VALIDATE_VALUE(key_list_term_tail, term_is_nonempty_list);
    term priv_param_term = term_get_list_head(key_list_term_tail);

    enum pk_param_t pk_param = interop_atom_term_select_int(pk_param_table, priv_param_term, glb);
    psa_key_type_t psa_key_type;
    size_t psa_key_bits;

    switch (pk_param) {
        case Secp256k1:
            psa_key_type = PSA_KEY_TYPE_ECC_PUBLIC_KEY(PSA_ECC_FAMILY_SECP_K1);
            psa_key_bits = 256;
            break;
        case Secp256r1:
            psa_key_type = PSA_KEY_TYPE_ECC_PUBLIC_KEY(PSA_ECC_FAMILY_SECP_R1);
            psa_key_bits = 256;
            break;
        case Secp384r1:
            psa_key_type = PSA_KEY_TYPE_ECC_PUBLIC_KEY(PSA_ECC_FAMILY_SECP_R1);
            psa_key_bits = 384;
            break;
        case Secp521r1:
            psa_key_type = PSA_KEY_TYPE_ECC_PUBLIC_KEY(PSA_ECC_FAMILY_SECP_R1);
            psa_key_bits = 521;
            break;
        default:
            RAISE_ERROR(
                make_crypto_error(__FILE__, __LINE__, "Couldn't get ECDSA private key", ctx));
    }

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
    void *sig_raw = malloc(sig_raw_size);
    if (IS_NULL_PTR(sig_raw)) {
        result = OUT_OF_MEMORY_ATOM;
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

    if (sig_raw) {
        memset(sig_raw, 0, sig_raw_len);
    }
    free(sig_raw);

    if (UNLIKELY(!success)) {
        RAISE_ERROR(result);
    }

    return result;
}

#endif

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

    term key_term = argv[2];
    VALIDATE_VALUE(key_term, term_is_binary);
    const void *key = term_binary_data(key_term);
    size_t key_len = term_binary_size(key_term);

    psa_key_type_t psa_key_type = interop_atom_term_select_int(mac_key_table, mac_type_term, glb);
    psa_algorithm_t psa_key_algo;
    psa_key_bits_t key_bit_size;
    switch (psa_key_type) {
        case PSA_KEY_TYPE_AES:
            psa_key_algo = PSA_ALG_CMAC;
            key_bit_size
                = interop_atom_term_select_int(cmac_algorithm_bits_table, sub_type_term, glb);
            if (UNLIKELY(key_bit_size == 0)) {
                RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unknown cipher", ctx));
            }

            if (UNLIKELY(key_bit_size != key_len * 8)) {
                RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Bad key size", ctx));
            }
            break;
        case PSA_KEY_TYPE_HMAC: {
            psa_algorithm_t sub_type_algo
                = interop_atom_term_select_int(psa_hash_algorithm_table, sub_type_term, glb);
            if (UNLIKELY(sub_type_algo == PSA_ALG_NONE)) {
                RAISE_ERROR(
                    make_crypto_error(__FILE__, __LINE__, "Bad digest algorithm for HMAC", ctx));
            }
            psa_key_algo = PSA_ALG_HMAC(sub_type_algo);
            key_bit_size = key_len * 8;
        } break;
        default:
            RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unknown mac algorithm", ctx));
    }

    term data_term = argv[3];
    VALIDATE_VALUE(data_term, term_is_binary);
    const void *data = term_binary_data(data_term);
    size_t data_len = term_binary_size(data_term);

    psa_key_attributes_t attr = PSA_KEY_ATTRIBUTES_INIT;
    psa_set_key_type(&attr, psa_key_type);
    psa_set_key_bits(&attr, key_bit_size);
    psa_set_key_usage_flags(&attr, PSA_KEY_USAGE_SIGN_MESSAGE);
    psa_set_key_algorithm(&attr, psa_key_algo);

    psa_key_id_t key_id = 0;
    psa_status_t status = psa_import_key(&attr, key, key_len, &key_id);
    psa_reset_key_attributes(&attr);
    switch (status) {
        case PSA_SUCCESS:
            break;
        case PSA_ERROR_NOT_SUPPORTED:
            RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unsupported algorithm", ctx));
        default:
            RAISE_ERROR(make_crypto_error(__FILE__, __LINE__, "Unexpected error", ctx));
    }

    bool success = false;
    term result = ERROR_ATOM;

    size_t mac_out_size = PSA_MAC_LENGTH(psa_key_type, key_bit_size, psa_key_algo);
    void *mac_out = malloc(mac_out_size);
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
    if (mac_out) {
        memset(mac_out, 0, mac_out_size);
    }
    free(mac_out);

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
}

term nif_crypto_info_lib(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    const char *mbedtls_str = "mbedtls";
    size_t mbedtls_len = strlen("mbedtls");

    // 18 bytes including null byte according to mbedtls doc
    char version_string[18];
    mbedtls_version_get_string_full(version_string);
    size_t version_string_len = strlen(version_string);

    if (UNLIKELY(memory_ensure_free(ctx,
                     LIST_SIZE(1, TUPLE_SIZE(3)) + TERM_BINARY_HEAP_SIZE(mbedtls_len)
                         + TERM_BINARY_HEAP_SIZE(version_string_len) + BOXED_INT64_SIZE)
            != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term mbedtls_term = term_from_literal_binary(mbedtls_str, mbedtls_len, &ctx->heap, ctx->global);
    term version_term = term_make_maybe_boxed_int64(MBEDTLS_VERSION_NUMBER, &ctx->heap);
    term version_string_term
        = term_from_literal_binary(version_string, version_string_len, &ctx->heap, ctx->global);

    term mbedtls_tuple = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(mbedtls_tuple, 0, mbedtls_term);
    term_put_tuple_element(mbedtls_tuple, 1, version_term);
    term_put_tuple_element(mbedtls_tuple, 2, version_string_term);

    return term_list_prepend(mbedtls_tuple, term_nil(), &ctx->heap);
}

static const struct Nif crypto_hash_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_hash
};
static const struct Nif crypto_crypto_one_time_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_crypto_crypto_one_time
};
#ifdef MBEDTLS_PSA_CRYPTO_C
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
#endif
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
#ifdef MBEDTLS_PSA_CRYPTO_C
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
#endif
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
