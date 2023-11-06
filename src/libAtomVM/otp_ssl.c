/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 by Paul Guyot <pguyot@kallisys.net>
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

#include <context.h>
#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <inet.h>
#include <interop.h>
#include <nifs.h>
#include <otp_socket.h>
#include <otp_ssl.h>
#include <port.h>
#include <refc_binary.h>
#include <term.h>
#include <term_typedef.h>

#include <string.h>

#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>
#include <mbedtls/ssl.h>

// #define ENABLE_TRACE
#include <trace.h>

#ifndef MBEDTLS_PRIVATE
#define MBEDTLS_PRIVATE(member) member
#endif

// Default read buffer if mbedtls_ssl_get_max_in_record_payload fails
#define DEFAULT_READ_BUFFER_FALLBACK 512

#if defined(MBEDTLS_DEBUG_C) && defined(ENABLE_TRACE)

#include <mbedtls/debug.h>

static void mbedtls_debug_cb(void *ctx, int level, const char *filename, int line, const char *msg)
{
    UNUSED(ctx);
    UNUSED(level);

    TRACE("%s:%d: %s", filename, line, msg);
}

#endif

//
// Resources
//

struct EntropyContextResource
{
    mbedtls_entropy_context context;
};

struct CtrDrbgResource
{
    mbedtls_ctr_drbg_context context;
};

struct SSLContextResource
{
    mbedtls_ssl_context context;
};

struct SSLConfigResource
{
    mbedtls_ssl_config config;
};

static void entropycontext_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct EntropyContextResource *rsrc_obj = (struct EntropyContextResource *) obj;
    mbedtls_entropy_free(&rsrc_obj->context);
}

static void ctrdrbg_dtor(ErlNifEnv *caller_env, void *obj)
{
    TRACE("%s\n", __func__);
    UNUSED(caller_env);

    struct CtrDrbgResource *rsrc_obj = (struct CtrDrbgResource *) obj;
    // Release the entropy
    mbedtls_entropy_context *entropy_context = rsrc_obj->context.MBEDTLS_PRIVATE(p_entropy);
    if (entropy_context) {
        struct EntropyContextResource *entropy_obj = CONTAINER_OF(entropy_context, struct EntropyContextResource, context);
        struct RefcBinary *entropy_refc = refc_binary_from_data(entropy_obj);
        refc_binary_decrement_refcount(entropy_refc, caller_env->global);
    }
    mbedtls_ctr_drbg_free(&rsrc_obj->context);
}

static void sslcontext_dtor(ErlNifEnv *caller_env, void *obj)
{
    TRACE("%s\n", __func__);
    UNUSED(caller_env);

    struct SSLContextResource *rsrc_obj = (struct SSLContextResource *) obj;
    // Release the config
    const mbedtls_ssl_config *config = rsrc_obj->context.MBEDTLS_PRIVATE(conf);
    if (config) {
        struct SSLConfigResource *config_obj = CONTAINER_OF(config, struct SSLConfigResource, config);
        struct RefcBinary *config_refc = refc_binary_from_data(config_obj);
        refc_binary_decrement_refcount(config_refc, caller_env->global);
    }
    mbedtls_ssl_free(&rsrc_obj->context);
}

static void sslconfig_dtor(ErlNifEnv *caller_env, void *obj)
{
    TRACE("%s\n", __func__);
    UNUSED(caller_env);

    struct SSLConfigResource *rsrc_obj = (struct SSLConfigResource *) obj;
    mbedtls_ssl_config_free(&rsrc_obj->config);
}

static const ErlNifResourceTypeInit EntropyContextResourceTypeInit = {
    .members = 1,
    .dtor = entropycontext_dtor,
};
static const ErlNifResourceTypeInit CtrDrbgResourceTypeInit = {
    .members = 1,
    .dtor = ctrdrbg_dtor,
};
static const ErlNifResourceTypeInit SSLContextResourceTypeInit = {
    .members = 1,
    .dtor = sslcontext_dtor,
};
static const ErlNifResourceTypeInit SSLConfigResourceTypeInit = {
    .members = 1,
    .dtor = sslconfig_dtor,
};

static ErlNifResourceType *entropycontext_resource_type;
static ErlNifResourceType *ctrdrbg_resource_type;
static ErlNifResourceType *sslcontext_resource_type;
static ErlNifResourceType *sslconfig_resource_type;

//
// Interface with sockets
//
int mbedtls_ssl_send_cb(void *ctx, const unsigned char *buf, size_t len)
{
    TRACE("%s\n", __func__);
    ssize_t res = socket_send((struct SocketResource *) ctx, buf, len, term_invalid_term());
    if (res == SocketWouldBlock) {
        return MBEDTLS_ERR_SSL_WANT_WRITE;
    }
    return res;
}

int mbedtls_ssl_recv_cb(void *ctx, unsigned char *buf, size_t len)
{
    TRACE("%s\n", __func__);
    ssize_t res = socket_recv((struct SocketResource *) ctx, buf, len, 0, NULL, NULL);
    if (res == SocketWouldBlock) {
        return MBEDTLS_ERR_SSL_WANT_READ;
    }
    return res;
}

//
// Interop
//

#define UNKNOWN_TABLE_VALUE -1

static const AtomStringIntPair endpoint_table[] = {
    { ATOM_STR("\x6", "client"), MBEDTLS_SSL_IS_CLIENT },
    { ATOM_STR("\x6", "server"), MBEDTLS_SSL_IS_SERVER },
    SELECT_INT_DEFAULT(UNKNOWN_TABLE_VALUE)
};

static const AtomStringIntPair authmode_table[] = {
    { ATOM_STR("\x4", "none"), MBEDTLS_SSL_VERIFY_NONE },
    { ATOM_STR("\x8", "optional"), MBEDTLS_SSL_VERIFY_OPTIONAL },
    { ATOM_STR("\x8", "required"), MBEDTLS_SSL_VERIFY_REQUIRED },
    SELECT_INT_DEFAULT(UNKNOWN_TABLE_VALUE)
};

//
// Nifs
//

static term nif_ssl_entropy_init(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);
    UNUSED(argv);

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    struct EntropyContextResource *rsrc_obj = enif_alloc_resource(entropycontext_resource_type, sizeof(struct EntropyContextResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = enif_make_resource(erl_nif_env_from_context(ctx), rsrc_obj);
    enif_release_resource(rsrc_obj);

    mbedtls_entropy_init(&rsrc_obj->context);

    return obj;
}

static term nif_ssl_ctr_drbg_init(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);
    UNUSED(argv);

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    struct CtrDrbgResource *rsrc_obj = enif_alloc_resource(ctrdrbg_resource_type, sizeof(struct CtrDrbgResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = enif_make_resource(erl_nif_env_from_context(ctx), rsrc_obj);
    enif_release_resource(rsrc_obj);

    mbedtls_ctr_drbg_init(&rsrc_obj->context);

    return obj;
}

static term nif_ssl_ctr_drbg_seed(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    VALIDATE_VALUE(argv[2], term_is_binary);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], ctrdrbg_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct CtrDrbgResource *ctrdrbg_obj = (struct CtrDrbgResource *) rsrc_obj_ptr;

    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[1], entropycontext_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct EntropyContextResource *entropy_obj = (struct EntropyContextResource *) rsrc_obj_ptr;

    int err = mbedtls_ctr_drbg_seed(&ctrdrbg_obj->context, mbedtls_entropy_func, &entropy_obj->context, (const unsigned char *) term_binary_data(argv[2]), term_binary_size(argv[2]));
    if (UNLIKELY(err)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct RefcBinary *entropy_refc = refc_binary_from_data(entropy_obj);
    refc_binary_increment_refcount(entropy_refc);

    return OK_ATOM;
}

static term nif_ssl_init(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);
    UNUSED(argv);

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    struct SSLContextResource *rsrc_obj = enif_alloc_resource(sslcontext_resource_type, sizeof(struct SSLContextResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = enif_make_resource(erl_nif_env_from_context(ctx), rsrc_obj);
    enif_release_resource(rsrc_obj);

    mbedtls_ssl_init(&rsrc_obj->context);

    return obj;
}

static term nif_ssl_set_bio(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);
    VALIDATE_VALUE(argv[1], term_is_otp_socket);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], sslcontext_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SSLContextResource *rsrc_obj = (struct SSLContextResource *) rsrc_obj_ptr;

    struct SocketResource *socket_resource;
    if (UNLIKELY(!term_to_otp_socket(argv[1], &socket_resource, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    mbedtls_ssl_set_bio(&rsrc_obj->context, socket_resource, mbedtls_ssl_send_cb, mbedtls_ssl_recv_cb, NULL);

    return OK_ATOM;
}

static term nif_ssl_config_init(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);
    UNUSED(argv);

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    struct SSLConfigResource *rsrc_obj = enif_alloc_resource(sslconfig_resource_type, sizeof(struct SSLConfigResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = enif_make_resource(erl_nif_env_from_context(ctx), rsrc_obj);
    enif_release_resource(rsrc_obj);

    mbedtls_ssl_config_init(&rsrc_obj->config);

#if defined(MBEDTLS_DEBUG_C) && defined(ENABLE_TRACE)
    mbedtls_ssl_conf_dbg(&rsrc_obj->config, mbedtls_debug_cb, NULL);
#endif

    return obj;
}

static term nif_ssl_config_defaults(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);
    VALIDATE_VALUE(argv[1], term_is_atom);
    VALIDATE_VALUE(argv[2], term_is_atom);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], sslconfig_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SSLConfigResource *rsrc_obj = (struct SSLConfigResource *) rsrc_obj_ptr;

    int endpoint = interop_atom_term_select_int(endpoint_table, argv[1], ctx->global);
    if (UNLIKELY(endpoint == UNKNOWN_TABLE_VALUE)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    enum inet_type transport_type = inet_atom_to_type(argv[2], ctx->global);
    if (UNLIKELY(transport_type != InetStreamType && transport_type != InetDgramType)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int transport = transport_type == InetStreamType ? MBEDTLS_SSL_TRANSPORT_STREAM : MBEDTLS_SSL_TRANSPORT_DATAGRAM;

    int err = mbedtls_ssl_config_defaults(&rsrc_obj->config, endpoint, transport, MBEDTLS_SSL_PRESET_DEFAULT);
    if (UNLIKELY(err != 0)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return OK_ATOM;
}

static term nif_ssl_set_hostname(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], sslcontext_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SSLContextResource *rsrc_obj = (struct SSLContextResource *) rsrc_obj_ptr;

    int ok;
    char *host_str = interop_term_to_string(argv[1], &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int err = mbedtls_ssl_set_hostname(&rsrc_obj->context, host_str);
    free(host_str);

    if (UNLIKELY(err == MBEDTLS_ERR_SSL_ALLOC_FAILED)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    if (UNLIKELY(err)) { // MBEDTLS_ERR_SSL_BAD_INPUT_DATA or any undocumented error
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

static term nif_ssl_conf_authmode(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], sslconfig_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SSLConfigResource *rsrc_obj = (struct SSLConfigResource *) rsrc_obj_ptr;

    int authmode = interop_atom_term_select_int(authmode_table, argv[1], ctx->global);
    if (UNLIKELY(authmode == UNKNOWN_TABLE_VALUE)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    mbedtls_ssl_conf_authmode(&rsrc_obj->config, authmode);

    return OK_ATOM;
}

static term nif_ssl_conf_rng(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], sslconfig_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SSLConfigResource *conf_obj = (struct SSLConfigResource *) rsrc_obj_ptr;

    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[1], ctrdrbg_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct CtrDrbgResource *ctr_drbg_obj = (struct CtrDrbgResource *) rsrc_obj_ptr;

    mbedtls_ssl_conf_rng(&conf_obj->config, mbedtls_ctr_drbg_random, &ctr_drbg_obj->context);

    return OK_ATOM;
}

static term nif_ssl_setup(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], sslcontext_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SSLContextResource *context_rsrc = (struct SSLContextResource *) rsrc_obj_ptr;

    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[1], sslconfig_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SSLConfigResource *config_rsrc = (struct SSLConfigResource *) rsrc_obj_ptr;

    int err = mbedtls_ssl_setup(&context_rsrc->context, &config_rsrc->config);
    if (UNLIKELY(err == MBEDTLS_ERR_SSL_ALLOC_FAILED)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    if (UNLIKELY(err)) { // Any undocumented error
        RAISE_ERROR(BADARG_ATOM);
    }

    struct RefcBinary *config_refc = refc_binary_from_data(config_rsrc);
    refc_binary_increment_refcount(config_refc);

    return OK_ATOM;
}

static term make_err_result(int err, Context *ctx)
{
    switch (err) {
        case 0:
            return OK_ATOM;
        case MBEDTLS_ERR_SSL_WANT_READ:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "want_read"));
        case MBEDTLS_ERR_SSL_WANT_WRITE:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\xA", "want_write"));
#if MBEDTLS_VERSION_NUMBER >= 0x020B0000
        case MBEDTLS_ERR_SSL_ASYNC_IN_PROGRESS:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\xA", "async_in_progress"));
#if MBEDTLS_VERSION_NUMBER >= 0x020E0000
        case MBEDTLS_ERR_SSL_CRYPTO_IN_PROGRESS:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\xA", "crypto_in_progress"));
#endif
#endif
        default: {
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, term_from_int(err));
            return error_tuple;
        }
    }
}

static term nif_ssl_handshake_step(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], sslcontext_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SSLContextResource *context_rsrc = (struct SSLContextResource *) rsrc_obj_ptr;

    int err = mbedtls_ssl_handshake_step(&context_rsrc->context);

#if MBEDTLS_VERSION_NUMBER >= 0x03020000
    if (err == 0 && mbedtls_ssl_is_handshake_over(&context_rsrc->context)) {
        return globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "done"));
    }
#else
    if (err == 0 && context_rsrc->context.state >= MBEDTLS_SSL_HANDSHAKE_OVER) {
        return globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "done"));
    }
#endif
    return make_err_result(err, ctx);
}

static term nif_ssl_close_notify(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], sslcontext_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SSLContextResource *context_rsrc = (struct SSLContextResource *) rsrc_obj_ptr;

    int err = mbedtls_ssl_close_notify(&context_rsrc->context);
    return make_err_result(err, ctx);
}

static term nif_ssl_write(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);
    VALIDATE_VALUE(argv[1], term_is_binary);

    term data = argv[1];

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], sslcontext_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SSLContextResource *context_rsrc = (struct SSLContextResource *) rsrc_obj_ptr;

    const uint8_t *buffer = (const uint8_t *) term_binary_data(data);
    size_t len = term_binary_size(data);

    int res = mbedtls_ssl_write(&context_rsrc->context, buffer, len);

    if (res == (int) len) {
        return OK_ATOM;
    }
    if (LIKELY(res >= 0)) { // ensure we don't return OK if res is 0
        size_t rest_len = len - res;
        size_t requested_size = term_sub_binary_heap_size(data, rest_len);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2) + requested_size, 1, &data, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term rest = term_maybe_create_sub_binary(data, res, rest_len, &ctx->heap, ctx->global);
        return port_create_tuple2(ctx, OK_ATOM, rest);
    }

    return make_err_result(res, ctx);
}

static term nif_ssl_read(Context *ctx, int argc, term argv[])
{
    TRACE("%s\n", __func__);
    UNUSED(argc);
    VALIDATE_VALUE(argv[1], term_is_integer);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], sslcontext_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SSLContextResource *context_rsrc = (struct SSLContextResource *) rsrc_obj_ptr;

    avm_int_t len = term_to_int(argv[1]);
    if (len < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
#if MBEDTLS_VERSION_NUMBER >= 0x03000000
    if (len == 0) {
        len = mbedtls_ssl_get_max_in_record_payload(&context_rsrc->context);
    }
#endif
    if (len <= 0) {
        len = DEFAULT_READ_BUFFER_FALLBACK;
    }
    size_t ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
    size_t requested_size = TUPLE_SIZE(2) + ensure_packet_avail;

    if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term data = term_create_uninitialized_binary(len, &ctx->heap, ctx->global);
    uint8_t *buffer = (uint8_t *) term_binary_data(data);

    int res = mbedtls_ssl_read(&context_rsrc->context, buffer, len);

    if (res == len) {
        return port_create_tuple2(ctx, OK_ATOM, data);
    }

    if (res >= 0 && res < len) {
        size_t requested_size = term_sub_binary_heap_size(data, res);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2) + requested_size, 1, &data, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term rest = term_maybe_create_sub_binary(data, 0, res, &ctx->heap, ctx->global);
        return port_create_tuple2(ctx, OK_ATOM, rest);
    }

    return make_err_result(res, ctx);
}

static const struct Nif ssl_entropy_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_entropy_init
};
static const struct Nif ssl_ctr_drbg_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_ctr_drbg_init
};
static const struct Nif ssl_ctr_drbg_seed_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_ctr_drbg_seed
};
static const struct Nif ssl_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_init
};
static const struct Nif ssl_set_bio_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_set_bio
};
static const struct Nif ssl_config_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_config_init
};
static const struct Nif ssl_config_defaults_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_config_defaults
};
static const struct Nif ssl_conf_authmode_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_conf_authmode
};
static const struct Nif ssl_conf_rng_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_conf_rng
};
static const struct Nif ssl_set_hostname_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_set_hostname
};
static const struct Nif ssl_setup_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_setup
};
static const struct Nif ssl_handshake_step_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_handshake_step
};
static const struct Nif ssl_close_notify_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_close_notify
};
static const struct Nif ssl_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_write
};
static const struct Nif ssl_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ssl_read
};

//
// Entrypoints
//

const struct Nif *otp_ssl_nif_get_nif(const char *nifname)
{
    if (strncmp("ssl:", nifname, 4) == 0) {
        const char *rest = nifname + 4;
        if (strcmp("nif_entropy_init/0", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_entropy_init_nif;
        }
        if (strcmp("nif_ctr_drbg_init/0", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_ctr_drbg_init_nif;
        }
        if (strcmp("nif_ctr_drbg_seed/3", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_ctr_drbg_seed_nif;
        }
        if (strcmp("nif_init/0", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_init_nif;
        }
        if (strcmp("nif_set_bio/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_set_bio_nif;
        }
        if (strcmp("nif_config_init/0", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_config_init_nif;
        }
        if (strcmp("nif_config_defaults/3", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_config_defaults_nif;
        }
        if (strcmp("nif_conf_authmode/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_conf_authmode_nif;
        }
        if (strcmp("nif_conf_rng/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_conf_rng_nif;
        }
        if (strcmp("nif_set_hostname/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_set_hostname_nif;
        }
        if (strcmp("nif_setup/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_setup_nif;
        }
        if (strcmp("nif_handshake_step/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_handshake_step_nif;
        }
        if (strcmp("nif_close_notify/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_close_notify_nif;
        }
        if (strcmp("nif_write/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_write_nif;
        }
        if (strcmp("nif_read/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &ssl_read_nif;
        }
    }
    return NULL;
}

void otp_ssl_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    entropycontext_resource_type = enif_init_resource_type(&env, "entropycontext", &EntropyContextResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
    ctrdrbg_resource_type = enif_init_resource_type(&env, "ctr_drbg", &CtrDrbgResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
    sslcontext_resource_type = enif_init_resource_type(&env, "sslcontext", &SSLContextResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
    sslconfig_resource_type = enif_init_resource_type(&env, "sslconfig", &SSLConfigResourceTypeInit, ERL_NIF_RT_CREATE, NULL);

#if defined(MBEDTLS_DEBUG_C) && defined(ENABLE_TRACE)
    mbedtls_debug_set_threshold(5);
#endif
}
