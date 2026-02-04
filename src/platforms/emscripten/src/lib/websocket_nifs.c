/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <term.h>
#include <term_typedef.h>

#include <emscripten.h>
#include <emscripten/websocket.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "emscripten_sys.h"
#include "memory.h"
#include "platform_defaultatoms.h"
#include "websocket_nifs.h"

struct WebsocketResource
{
    uint64_t ref;
    int32_t controlling_process_pid;
    ErlNifMonitor controlling_process_monitor;
    EMSCRIPTEN_WEBSOCKET_T websocket;
    bool closed;
};

enum
{
    CLOSE_STATUS_GARBAGE_COLLECTED_CLOSURE = 4000,
};

enum
{
    READY_STATE_CONNECTING = 0,
    READY_STATE_OPEN = 1,
    READY_STATE_CLOSING = 2,
    READY_STATE_CLOSED = 3
};

#define UNKNOWN_TABLE_VALUE -1

static const AtomStringIntPair ready_state_table[] = {
    { ATOM_STR("\xA", "connecting"), READY_STATE_CONNECTING },
    { ATOM_STR("\x4", "open"), READY_STATE_OPEN },
    { ATOM_STR("\x7", "closing"), READY_STATE_CLOSING },
    { ATOM_STR("\x6", "closed"), READY_STATE_CLOSED },
    SELECT_INT_DEFAULT(UNKNOWN_TABLE_VALUE)
};

static void websocket_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct WebsocketResource *websocket_rsrc = (struct WebsocketResource *) obj;
    if (!websocket_rsrc->closed) {
        emscripten_websocket_close(websocket_rsrc->websocket, CLOSE_STATUS_GARBAGE_COLLECTED_CLOSURE, "");
    }
    emscripten_websocket_delete(websocket_rsrc->websocket);
}

static void websocket_down(ErlNifEnv *caller_env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    UNUSED(pid);
    UNUSED(mon);
    UNUSED(caller_env);

    struct WebsocketResource *websocket_rsrc = (struct WebsocketResource *) obj;
    if (!websocket_rsrc->closed) {
        emscripten_websocket_close(websocket_rsrc->websocket, CLOSE_STATUS_GARBAGE_COLLECTED_CLOSURE, "");
        websocket_rsrc->closed = true;
    }
}

#define TERM_WEBSOCKET_RESOURCE_SIZE (TERM_BOXED_RESOURCE_SIZE + REF_SIZE + TUPLE_SIZE(3))

static term term_make_websocket_resource(struct WebsocketResource *rsrc, Heap *heap)
{
    term socket_tuple = term_alloc_tuple(3, heap);
    term_put_tuple_element(socket_tuple, 0, DOLLAR_WEBSOCKET_ATOM);
    term_put_tuple_element(socket_tuple, 1, term_from_ref_ticks(rsrc->ref, heap));
    term_put_tuple_element(socket_tuple, 2, term_from_resource(rsrc, heap));
    return socket_tuple;
}

static struct WebsocketResource *get_websocket_resource(Context *ctx, term rsrc)
{
    struct EmscriptenPlatformData *platform = ctx->global->platform_data;
    void *rsrc_obj_ptr;
    if (UNLIKELY(!term_is_tuple(rsrc) || term_get_tuple_arity(rsrc) != 3 || term_get_tuple_element(rsrc, 0) != DOLLAR_WEBSOCKET_ATOM || !term_is_reference(term_get_tuple_element(rsrc, 1)))) {
        return NULL;
    }
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), term_get_tuple_element(rsrc, 2), platform->websocket_resource_type, &rsrc_obj_ptr))) {
        return NULL;
    }
    term ref_term = term_get_tuple_element(rsrc, 1);
    if (UNLIKELY(term_to_ref_ticks(ref_term) != ((struct WebsocketResource *) rsrc_obj_ptr)->ref)) {
        return NULL;
    }
    return (struct WebsocketResource *) rsrc_obj_ptr;
}

static term nif_websocket_is_supported(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    return emscripten_websocket_is_supported() ? TRUE_ATOM : FALSE_ATOM;
}

static bool websocket_open_callback_func(int eventType, const EmscriptenWebSocketOpenEvent *websocketEvent __attribute__((nonnull)), void *userData)
{
    UNUSED(eventType);
    UNUSED(websocketEvent);

    struct WebsocketResource *websocket_rsrc = (struct WebsocketResource *) userData;
    struct RefcBinary *refc = refc_binary_from_data(userData);
    GlobalContext *global = refc->resource_type->global;

    // {websocket_open, Socket}
    BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + TERM_WEBSOCKET_RESOURCE_SIZE, heap)

    term event_term = term_alloc_tuple(2, &heap);
    term_put_tuple_element(event_term, 0, WEBSOCKET_OPEN_ATOM);
    term_put_tuple_element(event_term, 1, term_make_websocket_resource(websocket_rsrc, &heap));

    globalcontext_send_message_from_task(global, websocket_rsrc->controlling_process_pid, NormalMessage, event_term);

    END_WITH_STACK_HEAP(heap, global)

    return true;
}

static bool websocket_message_callback_func(int eventType, const EmscriptenWebSocketMessageEvent *websocketEvent __attribute__((nonnull)), void *userData)
{
    UNUSED(eventType);

    struct WebsocketResource *websocket_rsrc = (struct WebsocketResource *) userData;
    struct RefcBinary *refc = refc_binary_from_data(userData);
    GlobalContext *global = refc->resource_type->global;
    size_t numBytes = websocketEvent->isText ? websocketEvent->numBytes - 1 : websocketEvent->numBytes;

    // {websocket, Socket, Data}
    Heap heap;
    if (UNLIKELY(memory_init_heap(&heap, TUPLE_SIZE(3) + TERM_WEBSOCKET_RESOURCE_SIZE + TERM_BINARY_HEAP_SIZE(numBytes)) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }

    term event_term = term_alloc_tuple(3, &heap);
    term_put_tuple_element(event_term, 0, WEBSOCKET_ATOM);
    term_put_tuple_element(event_term, 1, term_make_websocket_resource(websocket_rsrc, &heap));
    term_put_tuple_element(event_term, 2, term_from_literal_binary(websocketEvent->data, numBytes, &heap, global));

    globalcontext_send_message_from_task(global, websocket_rsrc->controlling_process_pid, NormalMessage, event_term);

    memory_destroy_heap(&heap, global);

    return true;
}

static bool websocket_error_callback_func(int eventType, const EmscriptenWebSocketErrorEvent *websocketEvent __attribute__((nonnull)), void *userData)
{
    UNUSED(eventType);
    UNUSED(websocketEvent);

    struct WebsocketResource *websocket_rsrc = (struct WebsocketResource *) userData;
    struct RefcBinary *refc = refc_binary_from_data(userData);
    GlobalContext *global = refc->resource_type->global;

    // {websocket_open, Socket}
    BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + TERM_WEBSOCKET_RESOURCE_SIZE, heap)

    term event_term = term_alloc_tuple(3, &heap);
    term_put_tuple_element(event_term, 0, WEBSOCKET_ERROR_ATOM);
    term_put_tuple_element(event_term, 1, term_make_websocket_resource(websocket_rsrc, &heap));

    globalcontext_send_message_from_task(global, websocket_rsrc->controlling_process_pid, NormalMessage, event_term);

    END_WITH_STACK_HEAP(heap, global)

    return true;
}

static bool websocket_close_callback_func(int eventType, const EmscriptenWebSocketCloseEvent *websocketEvent __attribute__((nonnull)), void *userData)
{
    UNUSED(eventType);

    struct WebsocketResource *websocket_rsrc = (struct WebsocketResource *) userData;
    struct RefcBinary *refc = refc_binary_from_data(userData);
    GlobalContext *global = refc->resource_type->global;

    size_t reason_len = strlen(websocketEvent->reason);

    // {websocket_close, Socket, {Clean, Code, Reason}}
    Heap heap;
    if (UNLIKELY(memory_init_heap(&heap, TUPLE_SIZE(3) + TERM_WEBSOCKET_RESOURCE_SIZE + TUPLE_SIZE(3) + TERM_BINARY_HEAP_SIZE(reason_len)) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }

    term event_term = term_alloc_tuple(3, &heap);
    term_put_tuple_element(event_term, 0, WEBSOCKET_CLOSE_ATOM);
    term_put_tuple_element(event_term, 1, term_make_websocket_resource(websocket_rsrc, &heap));
    term close_reason_term = term_alloc_tuple(3, &heap);
    term_put_tuple_element(close_reason_term, 0, websocketEvent->wasClean ? TRUE_ATOM : FALSE_ATOM);
    term_put_tuple_element(close_reason_term, 1, term_from_int(websocketEvent->code));
    term_put_tuple_element(close_reason_term, 2, term_from_literal_binary(websocketEvent->reason, reason_len, &heap, global));
    term_put_tuple_element(event_term, 2, close_reason_term);

    globalcontext_send_message_from_task(global, websocket_rsrc->controlling_process_pid, NormalMessage, event_term);

    memory_destroy_heap(&heap, global);

    return true;
}

static term nif_websocket_new(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    VALIDATE_VALUE(argv[2], term_is_local_pid);
    int ok;
    char *url = interop_term_to_string(argv[0], &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct EmscriptenPlatformData *platform = ctx->global->platform_data;
    struct WebsocketResource *rsrc_obj = enif_alloc_resource(platform->websocket_resource_type, sizeof(struct WebsocketResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        free(url);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    rsrc_obj->ref = globalcontext_get_ref_ticks(ctx->global);
    rsrc_obj->controlling_process_pid = term_to_local_process_id(argv[2]);
    rsrc_obj->closed = true;
    rsrc_obj->websocket = 0;
    ErlNifEnv *env = erl_nif_env_from_context(ctx);
    if (UNLIKELY(enif_monitor_process(env, rsrc_obj, &ctx->process_id, &rsrc_obj->controlling_process_monitor) != 0)) {
        free(url);
        RAISE_ERROR(NOPROC_ATOM);
    }

    EmscriptenWebSocketCreateAttributes create_attributes;
    emscripten_websocket_init_create_attributes(&create_attributes);
    create_attributes.url = url;
    create_attributes.protocols = NULL;
    create_attributes.createOnMainThread = true;
    rsrc_obj->websocket = emscripten_websocket_new(&create_attributes);
    free(url);
    rsrc_obj->closed = false;

    emscripten_websocket_set_onopen_callback(rsrc_obj->websocket, rsrc_obj, websocket_open_callback_func);
    emscripten_websocket_set_onerror_callback(rsrc_obj->websocket, rsrc_obj, websocket_error_callback_func);
    emscripten_websocket_set_onclose_callback(rsrc_obj->websocket, rsrc_obj, websocket_close_callback_func);
    emscripten_websocket_set_onmessage_callback(rsrc_obj->websocket, rsrc_obj, websocket_message_callback_func);

    if (UNLIKELY(memory_ensure_free(ctx, TERM_WEBSOCKET_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_make_websocket_resource(rsrc_obj, &ctx->heap);
    enif_release_resource(rsrc_obj);
    return result;
}

static term nif_websocket_controlling_process(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct WebsocketResource *rsrc_obj = get_websocket_resource(ctx, argv[0]);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (ctx->process_id != rsrc_obj->controlling_process_pid) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term error_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "not_owner")));
        return error_tuple;
    }

    rsrc_obj->controlling_process_pid = term_to_local_process_id(argv[1]);

    return OK_ATOM;
}

static term nif_websocket_ready_state(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct WebsocketResource *rsrc_obj = get_websocket_resource(ctx, argv[0]);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    unsigned short ready_state;
    EMSCRIPTEN_RESULT err = emscripten_websocket_get_ready_state(rsrc_obj->websocket, &ready_state);
    if (UNLIKELY(err != EMSCRIPTEN_RESULT_SUCCESS)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return interop_atom_term_select_atom(ready_state_table, ready_state, ctx->global);
}

static term nif_websocket_buffered_amount(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct WebsocketResource *rsrc_obj = get_websocket_resource(ctx, argv[0]);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t buffered_amount;
    EMSCRIPTEN_RESULT err = emscripten_websocket_get_buffered_amount(rsrc_obj->websocket, &buffered_amount);
    if (UNLIKELY(err != EMSCRIPTEN_RESULT_SUCCESS)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return term_from_int(buffered_amount);
}

static term nif_websocket_url(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct WebsocketResource *rsrc_obj = get_websocket_resource(ctx, argv[0]);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int len;
    EMSCRIPTEN_RESULT err = emscripten_websocket_get_url_length(rsrc_obj->websocket, &len);
    if (UNLIKELY(err != EMSCRIPTEN_RESULT_SUCCESS)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t requested_size = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE + TERM_BOXED_SUB_BINARY_SIZE;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 1, &argv[0], MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term data = term_create_uninitialized_binary(len, &ctx->heap, ctx->global);
    char *buffer = (char *) term_binary_data(data);

    err = emscripten_websocket_get_url(rsrc_obj->websocket, buffer, len);
    if (UNLIKELY(err != EMSCRIPTEN_RESULT_SUCCESS)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term result = term_alloc_sub_binary(data, 0, strlen(buffer), &ctx->heap);

    return result;
}

static term nif_websocket_extensions(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct WebsocketResource *rsrc_obj = get_websocket_resource(ctx, argv[0]);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int len;
    EMSCRIPTEN_RESULT err = emscripten_websocket_get_extensions_length(rsrc_obj->websocket, &len);
    if (UNLIKELY(err != EMSCRIPTEN_RESULT_SUCCESS)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t requested_size = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 1, &argv[0], MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term data = term_create_uninitialized_binary(len, &ctx->heap, ctx->global);
    char *buffer = (char *) term_binary_data(data);

    err = emscripten_websocket_get_extensions(rsrc_obj->websocket, buffer, len);
    if (UNLIKELY(err != EMSCRIPTEN_RESULT_SUCCESS)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term result = term_alloc_sub_binary(data, 0, strlen(buffer), &ctx->heap);

    return result;
}

static term nif_websocket_protocol(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct WebsocketResource *rsrc_obj = get_websocket_resource(ctx, argv[0]);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int len;
    EMSCRIPTEN_RESULT err = emscripten_websocket_get_protocol_length(rsrc_obj->websocket, &len);
    if (UNLIKELY(err != EMSCRIPTEN_RESULT_SUCCESS)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t requested_size = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 1, &argv[0], MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term data = term_create_uninitialized_binary(len, &ctx->heap, ctx->global);
    char *buffer = (char *) term_binary_data(data);

    err = emscripten_websocket_get_protocol(rsrc_obj->websocket, buffer, len);
    if (UNLIKELY(err != EMSCRIPTEN_RESULT_SUCCESS)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term result = term_alloc_sub_binary(data, 0, strlen(buffer), &ctx->heap);

    return result;
}

static term nif_websocket_send_utf8(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct WebsocketResource *rsrc_obj = get_websocket_resource(ctx, argv[0]);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (rsrc_obj->closed) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term error_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, CLOSED_ATOM);
        return error_tuple;
    }

    int ok;
    char *text = interop_term_to_string(argv[1], &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }
    EMSCRIPTEN_RESULT err = emscripten_websocket_send_utf8_text(rsrc_obj->websocket, text);
    free(text);
    if (err != EMSCRIPTEN_RESULT_SUCCESS) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

static term nif_websocket_send_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_binary);

    struct WebsocketResource *rsrc_obj = get_websocket_resource(ctx, argv[0]);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (rsrc_obj->closed) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term error_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, CLOSED_ATOM);
        return error_tuple;
    }
    if (term_is_binary(argv[1])) {
        EMSCRIPTEN_RESULT err = emscripten_websocket_send_binary(rsrc_obj->websocket, (void *) term_binary_data(argv[1]), term_binary_size(argv[1]));
        if (err != EMSCRIPTEN_RESULT_SUCCESS) {
            RAISE_ERROR(BADARG_ATOM);
        }
    } else {
        size_t iolist_size;
        InteropFunctionResult interop_err = interop_iolist_size(argv[1], &iolist_size);
        if (UNLIKELY(interop_err == InteropMemoryAllocFail)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else if (UNLIKELY(interop_err == InteropBadArg)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        void *buffer = malloc(iolist_size);
        if (IS_NULL_PTR(buffer)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        interop_err = interop_write_iolist(argv[1], (char *) buffer);
        if (UNLIKELY(interop_err == InteropMemoryAllocFail)) {
            free(buffer);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else if (UNLIKELY(interop_err == InteropBadArg)) {
            free(buffer);
            RAISE_ERROR(BADARG_ATOM);
        }
        EMSCRIPTEN_RESULT err = emscripten_websocket_send_binary(rsrc_obj->websocket, buffer, iolist_size);
        free(buffer);
        if (err != EMSCRIPTEN_RESULT_SUCCESS) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    return OK_ATOM;
}

static term nif_websocket_close(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_integer);
    avm_int_t status_code = term_to_int(argv[1]);
    int ok;
    char *reason = interop_term_to_string(argv[2], &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct WebsocketResource *rsrc_obj = get_websocket_resource(ctx, argv[0]);
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (!rsrc_obj->closed) {
        EMSCRIPTEN_RESULT err = emscripten_websocket_close(rsrc_obj->websocket, status_code, reason);
        rsrc_obj->closed = true;
        if (err != EMSCRIPTEN_RESULT_SUCCESS) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    return OK_ATOM;
}

static const struct Nif websocket_is_supported_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_websocket_is_supported
};
static const struct Nif websocket_new_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_websocket_new
};
static const struct Nif websocket_controlling_process_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_websocket_controlling_process
};
static const struct Nif websocket_ready_state_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_websocket_ready_state
};
static const struct Nif websocket_buffered_amount_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_websocket_buffered_amount
};
static const struct Nif websocket_url_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_websocket_url
};
static const struct Nif websocket_extensions_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_websocket_extensions
};
static const struct Nif websocket_protocol_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_websocket_protocol
};
static const struct Nif websocket_send_utf8_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_websocket_send_utf8
};
static const struct Nif websocket_send_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_websocket_send_binary
};
static const struct Nif websocket_close_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_websocket_close
};

const struct Nif *websocket_get_nif(const char *nifname)
{
    if (memcmp("websocket:", nifname, strlen("websocket:"))) {
        return NULL;
    }
    nifname += strlen("websocket:");
    if (strcmp("is_supported/0", nifname) == 0) {
        return &websocket_is_supported_nif;
    }
    if (strcmp("new/3", nifname) == 0) {
        return &websocket_new_nif;
    }
    if (strcmp("controlling_process/2", nifname) == 0) {
        return &websocket_controlling_process_nif;
    }
    if (strcmp("ready_state/1", nifname) == 0) {
        return &websocket_ready_state_nif;
    }
    if (strcmp("buffered_amount/1", nifname) == 0) {
        return &websocket_buffered_amount_nif;
    }
    if (strcmp("url/1", nifname) == 0) {
        return &websocket_url_nif;
    }
    if (strcmp("extensions/1", nifname) == 0) {
        return &websocket_extensions_nif;
    }
    if (strcmp("protocol/1", nifname) == 0) {
        return &websocket_protocol_nif;
    }
    if (strcmp("send_utf8/2", nifname) == 0) {
        return &websocket_send_utf8_nif;
    }
    if (strcmp("send_binary/2", nifname) == 0) {
        return &websocket_send_binary_nif;
    }
    if (strcmp("close/3", nifname) == 0) {
        return &websocket_close_nif;
    }

    return NULL;
}

const ErlNifResourceTypeInit websocket_resource_type_init = {
    .members = 3,
    .dtor = websocket_dtor,
    .stop = NULL,
    .down = websocket_down,
};
