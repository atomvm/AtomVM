/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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
#include <emscripten/html5.h>
#include <emscripten/promise.h>
#include <emscripten/proxying.h>
#include <emscripten/threading.h>

#include <math.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "emscripten_sys.h"
#include "memory.h"
#include "platform_defaultatoms.h"
#include "platform_nifs.h"

static term nif_atomvm_platform(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    return EMSCRIPTEN_ATOM;
}

static term nif_atomvm_random(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    float val1 = emscripten_random();
    float val2 = emscripten_random();
    uint32_t result = ((uint32_t) floor(val1 * (1 << 16))) << 16 | ((uint32_t) floor(val2 * (1 << 16)));
    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_make_maybe_boxed_int64(result, &ctx->heap);
}

static void do_run_script(GlobalContext *global, char *script, int sync, int sync_caller_pid)
{
    emscripten_run_script(script);
    if (sync) {
        Context *target = globalcontext_get_process_lock(global, sync_caller_pid);
        if (target) {
            mailbox_send_term_signal(target, TrapAnswerSignal, OK_ATOM);
            globalcontext_get_process_unlock(global, target);
        } // else: sender died
    }
}

static term nif_emscripten_run_script(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    int ok;
    char *str = interop_term_to_string(argv[0], &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    bool main_thread = false;
    bool async = false;
    if (argc == 2) {
        term main_thread_t = interop_kv_get_value_default(argv[1], ATOM_STR("\xB", "main_thread"), FALSE_ATOM, ctx->global);
        main_thread = main_thread_t == TRUE_ATOM;

        term async_t = interop_kv_get_value_default(argv[1], ATOM_STR("\x5", "async"), FALSE_ATOM, ctx->global);
        async = async_t == TRUE_ATOM;
        if (!main_thread && async) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }
    term ret = OK_ATOM;
    if (main_thread) {
        if (async) {
            // str will be freed as it's passed as satellite
            emscripten_dispatch_to_thread(emscripten_main_runtime_thread_id(), EM_FUNC_SIG_VIIII, do_run_script, str, ctx->global, str, false, 0);
        } else {
            // Trap caller waiting for completion
            context_update_flags(ctx, ~NoFlags, Trap);
            ret = term_invalid_term();
            // str will be freed as it's passed as satellite
            emscripten_dispatch_to_thread(emscripten_main_runtime_thread_id(), EM_FUNC_SIG_VIIII, do_run_script, str, ctx->global, str, true, ctx->process_id);
        }
    } else {
        emscripten_run_script(str);
        free(str);
    }
    return ret;
}

static term nif_emscripten_promise_resolve_reject(Context *ctx, int argc, term argv[], em_promise_result_t result)
{
    struct EmscriptenPlatformData *platform = ctx->global->platform_data;
    struct PromiseResource *promise_rsrc;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], platform->promise_resource_type, (void **) &promise_rsrc))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (promise_rsrc->resolved) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (argc == 0 || term_is_integer(argv[1])) {
        int value = argc > 0 ? term_to_int(argv[1]) : 0;
        emscripten_dispatch_to_thread(emscripten_main_runtime_thread_id(), EM_FUNC_SIG_VIII, sys_promise_resolve_int_and_destroy, NULL, promise_rsrc->promise, result, value);
    } else {
        int ok;
        char *str = interop_term_to_string(argv[1], &ok);
        if (UNLIKELY(!ok)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        // str will be freed as it's passed as satellite
        emscripten_dispatch_to_thread(emscripten_main_runtime_thread_id(), EM_FUNC_SIG_VIII, sys_promise_resolve_str_and_destroy, str, promise_rsrc->promise, result, str);
    }
    promise_rsrc->resolved = true;

    return OK_ATOM;
}

static term nif_emscripten_promise_resolve(Context *ctx, int argc, term argv[])
{
    return nif_emscripten_promise_resolve_reject(ctx, argc, argv, EM_PROMISE_FULFILL);
}

static term nif_emscripten_promise_reject(Context *ctx, int argc, term argv[])
{
    return nif_emscripten_promise_resolve_reject(ctx, argc, argv, EM_PROMISE_REJECT);
}

static const struct Nif atomvm_platform_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_platform
};
static const struct Nif atomvm_random_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_random
};
static const struct Nif emscripten_run_script_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_emscripten_run_script
};
static const struct Nif emscripten_promise_resolve_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_emscripten_promise_resolve
};
static const struct Nif emscripten_promise_reject_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_emscripten_promise_reject
};

static bool get_callback_target(Context *ctx, term t, const char **target, char **str)
{
    if (term_is_atom(t)) {
        *str = NULL;
        if (globalcontext_is_term_equal_to_atom_string(ctx->global, t, ATOM_STR("\x6", "window"))) {
            *target = EMSCRIPTEN_EVENT_TARGET_WINDOW;
        } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, t, ATOM_STR("\x8", "document"))) {
            *target = EMSCRIPTEN_EVENT_TARGET_DOCUMENT;
        } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, t, ATOM_STR("\x6", "screen"))) {
            *target = EMSCRIPTEN_EVENT_TARGET_SCREEN;
        } else {
            return false;
        }
    } else {
        int ok;
        *str = interop_term_to_string(t, &ok);
        if (!ok) {
            return false;
        }
        *target = *str;
    }
    return true;
}

static bool get_register_callback_parameters(Context *ctx, int argc, term argv[], const char **target, struct HTMLEventUserDataResource **resource, bool *use_capture)
{
    if (argc >= 1 && !term_is_atom(argv[1]) && !term_is_list(argv[1])) {
        return false;
    }
    bool use_capture_opt = false;
    bool prevent_default_opt = false;
    if (argc > 1 && term_is_atom(argv[1])) {
        use_capture_opt = argv[1] == TRUE_ATOM;
    } else if (argc > 1 && term_is_list(argv[1])) {
        use_capture_opt = interop_proplist_get_value_default(argv[1], globalcontext_make_atom(ctx->global, ATOM_STR("\xB", "use_capture")), FALSE_ATOM) == TRUE_ATOM;
        prevent_default_opt = interop_proplist_get_value_default(argv[1], globalcontext_make_atom(ctx->global, ATOM_STR("\xF", "prevent_default")), FALSE_ATOM) == TRUE_ATOM;
    }
    *use_capture = use_capture_opt;
    char *str;
    if (UNLIKELY(!get_callback_target(ctx, argv[0], target, &str))) {
        return false;
    }
    // Create a resource to hold user data
    size_t estimated_user_data_usage;
    if (argc > 2) {
        estimated_user_data_usage = memory_estimate_usage(argv[2]) + 1; // mso list
    } else {
        estimated_user_data_usage = 0;
    }
    size_t resource_size = sizeof(struct HTMLEventUserDataResource) + estimated_user_data_usage * sizeof(term);
    struct EmscriptenPlatformData *platform = ctx->global->platform_data;
    struct HTMLEventUserDataResource *htmlevent_user_data_resource = enif_alloc_resource(platform->htmlevent_user_data_resource_type, resource_size);
    htmlevent_user_data_resource->target_pid = ctx->process_id;
    // We don't need to keep resource now because caller will make a term using
    // enif_make_resource which increments ref count
    // Monitor process so we will unregister & decrement ref count if target dies.
    if (UNLIKELY(enif_monitor_process(erl_nif_env_from_context(ctx), htmlevent_user_data_resource, &ctx->process_id, NULL) != 0)) {
        // If we fail, caller will not make a term, so decrement resource count now to dispose it.
        enif_release_resource(htmlevent_user_data_resource);
        return false;
    }
    // Save target element to allow users to pass the resource for unregistration
    htmlevent_user_data_resource->target_element_str = str;
    htmlevent_user_data_resource->target_element = *target;
    htmlevent_user_data_resource->unregistered = false;
    htmlevent_user_data_resource->prevent_default = prevent_default_opt;
    if (argc > 2) {
        term *heap_end;
        htmlevent_user_data_resource->user_data = memory_copy_term_tree_to_storage(htmlevent_user_data_resource->storage, &heap_end, argv[2]);
    } else {
        htmlevent_user_data_resource->user_data = term_invalid_term();
    }
    *resource = htmlevent_user_data_resource;
    return true;
}

static term term_from_emscripten_result(EMSCRIPTEN_RESULT em_result, Context *ctx)
{
    if (em_result == EMSCRIPTEN_RESULT_SUCCESS) {
        return OK_ATOM;
    }
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result_tuple, 0, ERROR_ATOM);
    term reason;
    switch (em_result) {
        case EMSCRIPTEN_RESULT_NOT_SUPPORTED:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xD", "not_supported"));
            break;
        case EMSCRIPTEN_RESULT_FAILED_NOT_DEFERRED:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\x13", "failed_not_deferred"));
            break;
        case EMSCRIPTEN_RESULT_INVALID_TARGET:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xE", "invalid_target"));
            break;
        case EMSCRIPTEN_RESULT_UNKNOWN_TARGET:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xE", "unknown_target"));
            break;
        case EMSCRIPTEN_RESULT_FAILED:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "failed"));
            break;
        case EMSCRIPTEN_RESULT_NO_DATA:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "no_data"));
            break;
        case EMSCRIPTEN_RESULT_TIMED_OUT:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "timed_out"));
            break;
        default:
            reason = term_from_int(em_result);
    }
    term_put_tuple_element(result_tuple, 1, reason);
    return result_tuple;
}

static EM_BOOL html5api_key_callback(int eventType, const EmscriptenKeyboardEvent *event, void *user_data)
{
    struct RefcBinary *refc = refc_binary_from_data(user_data);
    GlobalContext *global = refc->resource_type->global;
    struct HTMLEventUserDataResource *resource = (struct HTMLEventUserDataResource *) user_data;

    size_t event_size = TUPLE_SIZE(2); // {EventType, Event}
    event_size += term_map_size_in_terms(14);
    event_size += FLOAT_SIZE; // timestamp
    event_size += term_boxed_integer_size(event->location);
    event_size += term_boxed_integer_size(event->charCode);
    event_size += term_boxed_integer_size(event->keyCode);
    event_size += term_boxed_integer_size(event->which);
    size_t key_len = strlen(event->key);
    event_size += TERM_BINARY_HEAP_SIZE(key_len);
    size_t code_len = strlen(event->code);
    event_size += TERM_BINARY_HEAP_SIZE(code_len);
    size_t char_value_len = strlen(event->charValue);
    event_size += TERM_BINARY_HEAP_SIZE(char_value_len);
    size_t locale_len = strlen(event->locale);
    event_size += TERM_BINARY_HEAP_SIZE(locale_len);

    Heap heap;
    // Heap will be destroyed when message is dequeued
    if (UNLIKELY(memory_init_heap(&heap, event_size) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    term event_term = term_alloc_tuple(2, &heap);
    term event_map = term_alloc_map(14, &heap);
    term event_type_term = UNDEFINED_ATOM;
    switch (eventType) {
        case EMSCRIPTEN_EVENT_KEYPRESS:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x8", "keypress"));
            break;
        case EMSCRIPTEN_EVENT_KEYDOWN:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x7", "keydown"));
            break;
        case EMSCRIPTEN_EVENT_KEYUP:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x5", "keyup"));
            break;
    }
    term_put_tuple_element(event_term, 0, event_type_term);
    term_put_tuple_element(event_term, 1, event_map);

    term_set_map_assoc(event_map, 0, globalcontext_make_atom(global, ATOM_STR("\x9", "timestamp")), term_from_float(event->timestamp, &heap));
    term_set_map_assoc(event_map, 1, globalcontext_make_atom(global, ATOM_STR("\x8", "location")), term_make_maybe_boxed_int64(event->location, &heap));
    term_set_map_assoc(event_map, 2, globalcontext_make_atom(global, ATOM_STR("\x8", "ctrl_key")), event->ctrlKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 3, globalcontext_make_atom(global, ATOM_STR("\x9", "shift_key")), event->shiftKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 4, globalcontext_make_atom(global, ATOM_STR("\x7", "alt_key")), event->altKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 5, globalcontext_make_atom(global, ATOM_STR("\x8", "meta_key")), event->metaKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 6, globalcontext_make_atom(global, ATOM_STR("\x6", "repeat")), event->repeat ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 7, globalcontext_make_atom(global, ATOM_STR("\x9", "char_code")), term_make_maybe_boxed_int64(event->charCode, &heap));
    term_set_map_assoc(event_map, 8, globalcontext_make_atom(global, ATOM_STR("\x8", "key_code")), term_make_maybe_boxed_int64(event->keyCode, &heap));
    term_set_map_assoc(event_map, 9, globalcontext_make_atom(global, ATOM_STR("\x5", "which")), term_make_maybe_boxed_int64(event->which, &heap));
    term_set_map_assoc(event_map, 10, globalcontext_make_atom(global, ATOM_STR("\x3", "key")), term_from_literal_binary(event->key, key_len, &heap, global));
    term_set_map_assoc(event_map, 11, globalcontext_make_atom(global, ATOM_STR("\x4", "code")), term_from_literal_binary(event->code, code_len, &heap, global));
    term_set_map_assoc(event_map, 12, globalcontext_make_atom(global, ATOM_STR("\xA", "char_value")), term_from_literal_binary(event->charValue, char_value_len, &heap, global));
    term_set_map_assoc(event_map, 13, globalcontext_make_atom(global, ATOM_STR("\x6", "locale")), term_from_literal_binary(event->locale, locale_len, &heap, global));

    sys_enqueue_emscripten_htmlevent_message(global, resource->target_pid, event_term, resource->user_data, heap.root);
    return resource->prevent_default;
}

static size_t mouse_event_map_elements_size(const EmscriptenMouseEvent *event)
{
    size_t event_size = FLOAT_SIZE; // timestamp
    event_size += term_boxed_integer_size(event->screenX);
    event_size += term_boxed_integer_size(event->screenY);
    event_size += term_boxed_integer_size(event->clientX);
    event_size += term_boxed_integer_size(event->clientY);
    event_size += term_boxed_integer_size(event->movementX);
    event_size += term_boxed_integer_size(event->movementY);
    event_size += term_boxed_integer_size(event->targetX);
    event_size += term_boxed_integer_size(event->targetY);
    event_size += term_boxed_integer_size(event->padding);
    return event_size;
}

static void enter_mouse_event_to_map(term event_map, const EmscriptenMouseEvent *event, GlobalContext *global, Heap *heap)
{
    term_set_map_assoc(event_map, 0, globalcontext_make_atom(global, ATOM_STR("\x9", "timestamp")), term_from_float(event->timestamp, heap));
    term_set_map_assoc(event_map, 1, globalcontext_make_atom(global, ATOM_STR("\x8", "screen_x")), term_make_maybe_boxed_int64(event->screenX, heap));
    term_set_map_assoc(event_map, 2, globalcontext_make_atom(global, ATOM_STR("\x8", "screen_y")), term_make_maybe_boxed_int64(event->screenY, heap));
    term_set_map_assoc(event_map, 3, globalcontext_make_atom(global, ATOM_STR("\x8", "client_x")), term_make_maybe_boxed_int64(event->clientX, heap));
    term_set_map_assoc(event_map, 4, globalcontext_make_atom(global, ATOM_STR("\x8", "client_y")), term_make_maybe_boxed_int64(event->clientY, heap));
    term_set_map_assoc(event_map, 5, globalcontext_make_atom(global, ATOM_STR("\x8", "ctrl_key")), event->ctrlKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 6, globalcontext_make_atom(global, ATOM_STR("\x9", "shift_key")), event->shiftKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 7, globalcontext_make_atom(global, ATOM_STR("\x7", "alt_key")), event->altKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 8, globalcontext_make_atom(global, ATOM_STR("\x8", "meta_key")), event->metaKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 9, globalcontext_make_atom(global, ATOM_STR("\x6", "button")), term_from_int(event->button));
    term_set_map_assoc(event_map, 10, globalcontext_make_atom(global, ATOM_STR("\x7", "buttons")), term_from_int(event->buttons));
    term_set_map_assoc(event_map, 11, globalcontext_make_atom(global, ATOM_STR("\xA", "movement_x")), term_make_maybe_boxed_int64(event->movementX, heap));
    term_set_map_assoc(event_map, 12, globalcontext_make_atom(global, ATOM_STR("\xA", "movement_y")), term_make_maybe_boxed_int64(event->movementY, heap));
    term_set_map_assoc(event_map, 13, globalcontext_make_atom(global, ATOM_STR("\x8", "target_x")), term_make_maybe_boxed_int64(event->targetX, heap));
    term_set_map_assoc(event_map, 14, globalcontext_make_atom(global, ATOM_STR("\x8", "target_y")), term_make_maybe_boxed_int64(event->targetY, heap));
    term_set_map_assoc(event_map, 15, globalcontext_make_atom(global, ATOM_STR("\x7", "padding")), term_make_maybe_boxed_int64(event->padding, heap));
}

static EM_BOOL html5api_mouse_callback(int eventType, const EmscriptenMouseEvent *event, void *user_data)
{
    struct RefcBinary *refc = refc_binary_from_data(user_data);
    GlobalContext *global = refc->resource_type->global;
    struct HTMLEventUserDataResource *resource = (struct HTMLEventUserDataResource *) user_data;

    size_t event_size = TUPLE_SIZE(2); // {EventType, Event}
    event_size += term_map_size_in_terms(16);
    event_size += mouse_event_map_elements_size(event);

    Heap heap;
    // Heap will be destroyed when message is dequeued
    if (UNLIKELY(memory_init_heap(&heap, event_size) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    term event_term = term_alloc_tuple(2, &heap);
    term event_map = term_alloc_map(16, &heap);
    term event_type_term = UNDEFINED_ATOM;
    switch (eventType) {
        case EMSCRIPTEN_EVENT_CLICK:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x5", "click"));
            break;
        case EMSCRIPTEN_EVENT_MOUSEDOWN:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x9", "mousedown"));
            break;
        case EMSCRIPTEN_EVENT_MOUSEUP:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x7", "mouseup"));
            break;
        case EMSCRIPTEN_EVENT_DBLCLICK:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x8", "dblclick"));
            break;
        case EMSCRIPTEN_EVENT_MOUSEMOVE:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x9", "mousemove"));
            break;
        case EMSCRIPTEN_EVENT_MOUSEENTER:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\xA", "mouseenter"));
            break;
        case EMSCRIPTEN_EVENT_MOUSELEAVE:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\xA", "mouseleave"));
            break;
        case EMSCRIPTEN_EVENT_MOUSEOVER:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x9", "mouseover"));
            break;
        case EMSCRIPTEN_EVENT_MOUSEOUT:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x8", "mouseout"));
            break;
    }
    term_put_tuple_element(event_term, 0, event_type_term);
    term_put_tuple_element(event_term, 1, event_map);

    enter_mouse_event_to_map(event_map, event, global, &heap);

    sys_enqueue_emscripten_htmlevent_message(global, resource->target_pid, event_term, resource->user_data, heap.root);
    return resource->prevent_default;
}

static EM_BOOL html5api_wheel_callback(int eventType, const EmscriptenWheelEvent *event, void *user_data)
{
    struct RefcBinary *refc = refc_binary_from_data(user_data);
    GlobalContext *global = refc->resource_type->global;
    struct HTMLEventUserDataResource *resource = (struct HTMLEventUserDataResource *) user_data;

    size_t event_size = TUPLE_SIZE(2); // {EventType, Event}
    event_size += term_map_size_in_terms(20);
    event_size += mouse_event_map_elements_size(&event->mouse);
    event_size += (3 * FLOAT_SIZE); // deltaX, deltaY, deltaZ
    event_size += term_boxed_integer_size(event->deltaMode);

    Heap heap;
    // Heap will be destroyed when message is dequeued
    if (UNLIKELY(memory_init_heap(&heap, event_size) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    term event_term = term_alloc_tuple(2, &heap);
    term event_map = term_alloc_map(20, &heap);
    term event_type_term = UNDEFINED_ATOM;
    switch (eventType) {
        case EMSCRIPTEN_EVENT_WHEEL:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x5", "wheel"));
            break;
    }
    term_put_tuple_element(event_term, 0, event_type_term);
    term_put_tuple_element(event_term, 1, event_map);

    enter_mouse_event_to_map(event_map, &event->mouse, global, &heap);

    term_set_map_assoc(event_map, 16, globalcontext_make_atom(global, ATOM_STR("\x7", "delta_x")), term_from_float(event->deltaX, &heap));
    term_set_map_assoc(event_map, 17, globalcontext_make_atom(global, ATOM_STR("\x7", "delta_y")), term_from_float(event->deltaY, &heap));
    term_set_map_assoc(event_map, 18, globalcontext_make_atom(global, ATOM_STR("\x7", "delta_z")), term_from_float(event->deltaZ, &heap));
    term_set_map_assoc(event_map, 19, globalcontext_make_atom(global, ATOM_STR("\xA", "delta_mode")), term_make_maybe_boxed_int64(event->deltaMode, &heap));

    sys_enqueue_emscripten_htmlevent_message(global, resource->target_pid, event_term, resource->user_data, heap.root);
    return resource->prevent_default;
}

static EM_BOOL html5api_ui_callback(int eventType, const EmscriptenUiEvent *event, void *user_data)
{
    struct RefcBinary *refc = refc_binary_from_data(user_data);
    GlobalContext *global = refc->resource_type->global;
    struct HTMLEventUserDataResource *resource = (struct HTMLEventUserDataResource *) user_data;

    size_t event_size = TUPLE_SIZE(2); // {EventType, Event}
    event_size += term_map_size_in_terms(9);
    event_size += term_boxed_integer_size(event->detail);

    Heap heap;
    // Heap will be destroyed when message is dequeued
    if (UNLIKELY(memory_init_heap(&heap, event_size) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    term event_term = term_alloc_tuple(2, &heap);
    term event_map = term_alloc_map(9, &heap);
    term event_type_term = UNDEFINED_ATOM;
    switch (eventType) {
        case EMSCRIPTEN_EVENT_RESIZE:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x6", "resize"));
            break;
        case EMSCRIPTEN_EVENT_SCROLL:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x6", "scroll"));
            break;
    }
    term_put_tuple_element(event_term, 0, event_type_term);
    term_put_tuple_element(event_term, 1, event_map);

    term_set_map_assoc(event_map, 0, globalcontext_make_atom(global, ATOM_STR("\x6", "detail")), term_make_maybe_boxed_int64(event->detail, &heap));
    term_set_map_assoc(event_map, 1, globalcontext_make_atom(global, ATOM_STR("\x1A", "document_body_client_width")), term_from_int(event->documentBodyClientWidth));
    term_set_map_assoc(event_map, 2, globalcontext_make_atom(global, ATOM_STR("\x1B", "document_body_client_height")), term_from_int(event->documentBodyClientHeight));
    term_set_map_assoc(event_map, 3, globalcontext_make_atom(global, ATOM_STR("\x12", "window_inner_width")), term_from_int(event->windowInnerWidth));
    term_set_map_assoc(event_map, 4, globalcontext_make_atom(global, ATOM_STR("\x13", "window_inner_height")), term_from_int(event->windowInnerHeight));
    term_set_map_assoc(event_map, 5, globalcontext_make_atom(global, ATOM_STR("\x12", "window_outer_width")), term_from_int(event->windowOuterWidth));
    term_set_map_assoc(event_map, 6, globalcontext_make_atom(global, ATOM_STR("\x13", "window_outer_height")), term_from_int(event->windowOuterHeight));
    term_set_map_assoc(event_map, 7, globalcontext_make_atom(global, ATOM_STR("\xA", "scroll_top")), term_from_int(event->scrollTop));
    term_set_map_assoc(event_map, 8, globalcontext_make_atom(global, ATOM_STR("\xB", "scroll_left")), term_from_int(event->scrollLeft));

    sys_enqueue_emscripten_htmlevent_message(global, resource->target_pid, event_term, resource->user_data, heap.root);
    return resource->prevent_default;
}

static EM_BOOL html5api_focus_callback(int eventType, const EmscriptenFocusEvent *event, void *user_data)
{
    struct RefcBinary *refc = refc_binary_from_data(user_data);
    GlobalContext *global = refc->resource_type->global;
    struct HTMLEventUserDataResource *resource = (struct HTMLEventUserDataResource *) user_data;

    size_t event_size = TUPLE_SIZE(2); // {EventType, Event}
    event_size += term_map_size_in_terms(2);
    size_t node_name_len = strlen(event->nodeName);
    event_size += TERM_BINARY_HEAP_SIZE(node_name_len);
    size_t id_len = strlen(event->id);
    event_size += TERM_BINARY_HEAP_SIZE(id_len);

    Heap heap;
    // Heap will be destroyed when message is dequeued
    if (UNLIKELY(memory_init_heap(&heap, event_size) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    term event_term = term_alloc_tuple(2, &heap);
    term event_map = term_alloc_map(2, &heap);
    term event_type_term = UNDEFINED_ATOM;
    switch (eventType) {
        case EMSCRIPTEN_EVENT_BLUR:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x4", "blur"));
            break;
        case EMSCRIPTEN_EVENT_FOCUS:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x5", "focus"));
            break;
        case EMSCRIPTEN_EVENT_FOCUSIN:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x7", "focusin"));
            break;
        case EMSCRIPTEN_EVENT_FOCUSOUT:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x8", "focusout"));
            break;
    }
    term_put_tuple_element(event_term, 0, event_type_term);
    term_put_tuple_element(event_term, 1, event_map);

    term_set_map_assoc(event_map, 0, globalcontext_make_atom(global, ATOM_STR("\x9", "node_name")), term_from_literal_binary(event->nodeName, node_name_len, &heap, global));
    term_set_map_assoc(event_map, 1, globalcontext_make_atom(global, ATOM_STR("\x2", "id")), term_from_literal_binary(event->id, id_len, &heap, global));

    sys_enqueue_emscripten_htmlevent_message(global, resource->target_pid, event_term, resource->user_data, heap.root);
    return resource->prevent_default;
}

static size_t touch_point_map_elements_size(const EmscriptenTouchPoint *touch)
{
    size_t event_size = term_boxed_integer_size(touch->identifier);
    event_size += term_boxed_integer_size(touch->screenX);
    event_size += term_boxed_integer_size(touch->screenY);
    event_size += term_boxed_integer_size(touch->clientX);
    event_size += term_boxed_integer_size(touch->clientY);
    event_size += term_boxed_integer_size(touch->pageX);
    event_size += term_boxed_integer_size(touch->pageY);
    event_size += term_boxed_integer_size(touch->targetX);
    event_size += term_boxed_integer_size(touch->targetY);
    return event_size;
}

static void enter_touch_point_to_map(term touch_map, const EmscriptenTouchPoint *touch, GlobalContext *global, Heap *heap)
{
    term_set_map_assoc(touch_map, 0, globalcontext_make_atom(global, ATOM_STR("\xA", "identifier")), term_make_maybe_boxed_int64(touch->identifier, heap));
    term_set_map_assoc(touch_map, 1, globalcontext_make_atom(global, ATOM_STR("\x8", "screen_x")), term_make_maybe_boxed_int64(touch->screenX, heap));
    term_set_map_assoc(touch_map, 2, globalcontext_make_atom(global, ATOM_STR("\x8", "screen_y")), term_make_maybe_boxed_int64(touch->screenY, heap));
    term_set_map_assoc(touch_map, 3, globalcontext_make_atom(global, ATOM_STR("\x8", "client_x")), term_make_maybe_boxed_int64(touch->clientX, heap));
    term_set_map_assoc(touch_map, 4, globalcontext_make_atom(global, ATOM_STR("\x8", "client_y")), term_make_maybe_boxed_int64(touch->clientY, heap));
    term_set_map_assoc(touch_map, 5, globalcontext_make_atom(global, ATOM_STR("\x6", "page_x")), term_make_maybe_boxed_int64(touch->pageX, heap));
    term_set_map_assoc(touch_map, 6, globalcontext_make_atom(global, ATOM_STR("\x6", "page_y")), term_make_maybe_boxed_int64(touch->pageY, heap));
    term_set_map_assoc(touch_map, 7, globalcontext_make_atom(global, ATOM_STR("\xA", "is_changed")), touch->isChanged ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(touch_map, 8, globalcontext_make_atom(global, ATOM_STR("\x9", "on_target")), touch->onTarget ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(touch_map, 9, globalcontext_make_atom(global, ATOM_STR("\x8", "target_x")), term_make_maybe_boxed_int64(touch->targetX, heap));
    term_set_map_assoc(touch_map, 10, globalcontext_make_atom(global, ATOM_STR("\x8", "target_y")), term_make_maybe_boxed_int64(touch->targetY, heap));
}

static EM_BOOL html5api_touch_callback(int eventType, const EmscriptenTouchEvent *event, void *user_data)
{
    struct RefcBinary *refc = refc_binary_from_data(user_data);
    GlobalContext *global = refc->resource_type->global;
    struct HTMLEventUserDataResource *resource = (struct HTMLEventUserDataResource *) user_data;

    size_t event_size = TUPLE_SIZE(2); // {EventType, Event}
    event_size += term_map_size_in_terms(6);
    event_size += FLOAT_SIZE; // timestamp
    for (int i = 0; i < event->numTouches; i++) {
        event_size += term_map_size_in_terms(11);
        event_size += touch_point_map_elements_size(&event->touches[i]);
        event_size += CONS_SIZE;
    }

    Heap heap;
    // Heap will be destroyed when message is dequeued
    if (UNLIKELY(memory_init_heap(&heap, event_size) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    term event_term = term_alloc_tuple(2, &heap);
    term event_map = term_alloc_map(6, &heap);
    term event_type_term = UNDEFINED_ATOM;
    switch (eventType) {
        case EMSCRIPTEN_EVENT_TOUCHSTART:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\xA", "touchstart"));
            break;
        case EMSCRIPTEN_EVENT_TOUCHEND:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x8", "touchend"));
            break;
        case EMSCRIPTEN_EVENT_TOUCHMOVE:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\x9", "touchmove"));
            break;
        case EMSCRIPTEN_EVENT_TOUCHCANCEL:
            event_type_term = globalcontext_make_atom(global, ATOM_STR("\xB", "touchcancel"));
            break;
    }
    term_put_tuple_element(event_term, 0, event_type_term);
    term_put_tuple_element(event_term, 1, event_map);

    term touches_list = term_nil();
    for (int i = event->numTouches - 1; i >= 0; i--) {
        term touch_map = term_alloc_map(11, &heap);
        enter_touch_point_to_map(touch_map, &event->touches[i], global, &heap);
        touches_list = term_list_prepend(touch_map, touches_list, &heap);
    }

    term_set_map_assoc(event_map, 0, globalcontext_make_atom(global, ATOM_STR("\x9", "timestamp")), term_from_float(event->timestamp, &heap));
    term_set_map_assoc(event_map, 1, globalcontext_make_atom(global, ATOM_STR("\x8", "ctrl_key")), event->ctrlKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 2, globalcontext_make_atom(global, ATOM_STR("\x9", "shift_key")), event->shiftKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 3, globalcontext_make_atom(global, ATOM_STR("\x7", "alt_key")), event->altKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 4, globalcontext_make_atom(global, ATOM_STR("\x8", "meta_key")), event->metaKey ? TRUE_ATOM : FALSE_ATOM);
    term_set_map_assoc(event_map, 5, globalcontext_make_atom(global, ATOM_STR("\x7", "touches")), touches_list);

    sys_enqueue_emscripten_htmlevent_message(global, resource->target_pid, event_term, resource->user_data, heap.root);
    return resource->prevent_default;
}
#define HTML5_REGISTER_CALLBACK(event_type, event_constant, callback)                                                                                                                    \
    static term nif_emscripten_register_##callback##_callback(Context *ctx, int argc, term argv[])                                                                                       \
    {                                                                                                                                                                                    \
        const char *target;                                                                                                                                                              \
        struct HTMLEventUserDataResource *resource;                                                                                                                                      \
        bool use_capture;                                                                                                                                                                \
        if (UNLIKELY(!get_register_callback_parameters(ctx, argc, argv, &target, &resource, &use_capture))) {                                                                            \
            RAISE_ERROR(BADARG_ATOM);                                                                                                                                                    \
        }                                                                                                                                                                                \
        resource->event = event_constant;                                                                                                                                                \
        EMSCRIPTEN_RESULT result = emscripten_set_##callback##_callback_on_thread(target, resource, use_capture, html5api_##event_type##_callback, emscripten_main_runtime_thread_id()); \
        if (result != EMSCRIPTEN_RESULT_SUCCESS && result != EMSCRIPTEN_RESULT_DEFERRED) {                                                                                               \
            return term_from_emscripten_result(result, ctx);                                                                                                                             \
        }                                                                                                                                                                                \
        term resource_term = enif_make_resource(erl_nif_env_from_context(ctx), resource);                                                                                                \
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(3), 1, &resource_term, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {                                                         \
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);                                                                                                                                             \
        }                                                                                                                                                                                \
        term result_tuple = term_alloc_tuple(result == EMSCRIPTEN_RESULT_SUCCESS ? 2 : 3, &ctx->heap);                                                                                   \
        term_put_tuple_element(result_tuple, 0, OK_ATOM);                                                                                                                                \
        term_put_tuple_element(result_tuple, 1, resource_term);                                                                                                                          \
        if (result == EMSCRIPTEN_RESULT_DEFERRED) {                                                                                                                                      \
            term_put_tuple_element(result_tuple, 2, globalcontext_make_atom(ctx->global, ATOM_STR("\x8", "deferred")));                                                                  \
        }                                                                                                                                                                                \
        return result_tuple;                                                                                                                                                             \
    }                                                                                                                                                                                    \
    static const struct Nif emscripten_register_##callback##_callback_nif = {                                                                                                            \
        .base.type = NIFFunctionType,                                                                                                                                                    \
        .nif_ptr = nif_emscripten_register_##callback##_callback                                                                                                                         \
    };                                                                                                                                                                                   \
    static term nif_emscripten_unregister_##callback##_callback(Context *ctx, int argc, term argv[])                                                                                     \
    {                                                                                                                                                                                    \
        UNUSED(argc);                                                                                                                                                                    \
        struct EmscriptenPlatformData *platform = ctx->global->platform_data;                                                                                                            \
        struct HTMLEventUserDataResource *resource;                                                                                                                                      \
        if (enif_get_resource(erl_nif_env_from_context(ctx), argv[0], platform->htmlevent_user_data_resource_type, (void **) &resource)) {                                               \
            if (!resource->unregistered) {                                                                                                                                               \
                sys_enqueue_emscripten_unregister_htmlevent_message(ctx->global, resource);                                                                                              \
            }                                                                                                                                                                            \
            return OK_ATOM;                                                                                                                                                              \
        }                                                                                                                                                                                \
        const char *target;                                                                                                                                                              \
        char *str;                                                                                                                                                                       \
        if (UNLIKELY(!get_callback_target(ctx, argv[0], &target, &str))) {                                                                                                               \
            RAISE_ERROR(BADARG_ATOM);                                                                                                                                                    \
        }                                                                                                                                                                                \
        EMSCRIPTEN_RESULT result = emscripten_set_##callback##_callback_on_thread(target, NULL, false, NULL, emscripten_main_runtime_thread_id());                                       \
        free(str);                                                                                                                                                                       \
        return term_from_emscripten_result(result, ctx);                                                                                                                                 \
    }                                                                                                                                                                                    \
    static const struct Nif emscripten_unregister_##callback##_callback_nif = {                                                                                                          \
        .base.type = NIFFunctionType,                                                                                                                                                    \
        .nif_ptr = nif_emscripten_unregister_##callback##_callback                                                                                                                       \
    }

HTML5_REGISTER_CALLBACK(key, EMSCRIPTEN_EVENT_KEYPRESS, keypress);
HTML5_REGISTER_CALLBACK(key, EMSCRIPTEN_EVENT_KEYDOWN, keydown);
HTML5_REGISTER_CALLBACK(key, EMSCRIPTEN_EVENT_KEYUP, keyup);
HTML5_REGISTER_CALLBACK(mouse, EMSCRIPTEN_EVENT_CLICK, click);
HTML5_REGISTER_CALLBACK(mouse, EMSCRIPTEN_EVENT_MOUSEDOWN, mousedown);
HTML5_REGISTER_CALLBACK(mouse, EMSCRIPTEN_EVENT_MOUSEUP, mouseup);
HTML5_REGISTER_CALLBACK(mouse, EMSCRIPTEN_EVENT_DBLCLICK, dblclick);
HTML5_REGISTER_CALLBACK(mouse, EMSCRIPTEN_EVENT_MOUSEMOVE, mousemove);
HTML5_REGISTER_CALLBACK(mouse, EMSCRIPTEN_EVENT_MOUSEENTER, mouseenter);
HTML5_REGISTER_CALLBACK(mouse, EMSCRIPTEN_EVENT_MOUSELEAVE, mouseleave);
HTML5_REGISTER_CALLBACK(mouse, EMSCRIPTEN_EVENT_MOUSEOVER, mouseover);
HTML5_REGISTER_CALLBACK(mouse, EMSCRIPTEN_EVENT_MOUSEOUT, mouseout);
HTML5_REGISTER_CALLBACK(wheel, EMSCRIPTEN_EVENT_WHEEL, wheel);
HTML5_REGISTER_CALLBACK(ui, EMSCRIPTEN_EVENT_RESIZE, resize);
HTML5_REGISTER_CALLBACK(ui, EMSCRIPTEN_EVENT_SCROLL, scroll);
HTML5_REGISTER_CALLBACK(focus, EMSCRIPTEN_EVENT_BLUR, blur);
HTML5_REGISTER_CALLBACK(focus, EMSCRIPTEN_EVENT_FOCUS, focus);
HTML5_REGISTER_CALLBACK(focus, EMSCRIPTEN_EVENT_FOCUSIN, focusin);
HTML5_REGISTER_CALLBACK(focus, EMSCRIPTEN_EVENT_FOCUSOUT, focusout);
HTML5_REGISTER_CALLBACK(touch, EMSCRIPTEN_EVENT_TOUCHSTART, touchstart);
HTML5_REGISTER_CALLBACK(touch, EMSCRIPTEN_EVENT_TOUCHEND, touchend);
HTML5_REGISTER_CALLBACK(touch, EMSCRIPTEN_EVENT_TOUCHMOVE, touchmove);
HTML5_REGISTER_CALLBACK(touch, EMSCRIPTEN_EVENT_TOUCHCANCEL, touchcancel);

#define HTML5_GET_REGISTER_CALLBACK(callback)                          \
    if (strcmp("register_" #callback "_callback/1", nifname) == 0) {   \
        return &emscripten_register_##callback##_callback_nif;         \
    }                                                                  \
    if (strcmp("register_" #callback "_callback/2", nifname) == 0) {   \
        return &emscripten_register_##callback##_callback_nif;         \
    }                                                                  \
    if (strcmp("register_" #callback "_callback/3", nifname) == 0) {   \
        return &emscripten_register_##callback##_callback_nif;         \
    }                                                                  \
    if (strcmp("unregister_" #callback "_callback/1", nifname) == 0) { \
        return &emscripten_unregister_##callback##_callback_nif;       \
    }

const struct Nif *platform_nifs_get_nif(const char *nifname)
{
    if (strcmp("atomvm:platform/0", nifname) == 0) {
        return &atomvm_platform_nif;
    }
    if (strcmp("atomvm:random/0", nifname) == 0) {
        return &atomvm_random_nif;
    }
    if (memcmp("emscripten:", nifname, strlen("emscripten:"))) {
        return NULL;
    }
    nifname += strlen("emscripten:");
    if (strcmp("run_script/1", nifname) == 0) {
        return &emscripten_run_script_nif;
    }
    if (strcmp("run_script/2", nifname) == 0) {
        return &emscripten_run_script_nif;
    }
    if (strcmp("promise_resolve/1", nifname) == 0) {
        return &emscripten_promise_resolve_nif;
    }
    if (strcmp("promise_resolve/2", nifname) == 0) {
        return &emscripten_promise_resolve_nif;
    }
    if (strcmp("promise_reject/1", nifname) == 0) {
        return &emscripten_promise_reject_nif;
    }
    if (strcmp("promise_reject/2", nifname) == 0) {
        return &emscripten_promise_reject_nif;
    }
    HTML5_GET_REGISTER_CALLBACK(keypress)
    HTML5_GET_REGISTER_CALLBACK(keydown)
    HTML5_GET_REGISTER_CALLBACK(keyup)
    HTML5_GET_REGISTER_CALLBACK(click);
    HTML5_GET_REGISTER_CALLBACK(mousedown);
    HTML5_GET_REGISTER_CALLBACK(mouseup);
    HTML5_GET_REGISTER_CALLBACK(dblclick);
    HTML5_GET_REGISTER_CALLBACK(mousemove);
    HTML5_GET_REGISTER_CALLBACK(mouseenter);
    HTML5_GET_REGISTER_CALLBACK(mouseleave);
    HTML5_GET_REGISTER_CALLBACK(mouseover);
    HTML5_GET_REGISTER_CALLBACK(mouseout);
    HTML5_GET_REGISTER_CALLBACK(wheel);
    HTML5_GET_REGISTER_CALLBACK(resize);
    HTML5_GET_REGISTER_CALLBACK(scroll);
    HTML5_GET_REGISTER_CALLBACK(blur);
    HTML5_GET_REGISTER_CALLBACK(focus);
    HTML5_GET_REGISTER_CALLBACK(focusin);
    HTML5_GET_REGISTER_CALLBACK(focusout);
    HTML5_GET_REGISTER_CALLBACK(touchstart);
    HTML5_GET_REGISTER_CALLBACK(touchend);
    HTML5_GET_REGISTER_CALLBACK(touchmove);
    HTML5_GET_REGISTER_CALLBACK(touchcancel);

    return NULL;
}
