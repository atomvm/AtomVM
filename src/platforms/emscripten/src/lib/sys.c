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

#include <sys.h>

#include <avmpack.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <iff.h>
#include <scheduler.h>

#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include <emscripten.h>
#include <emscripten/fetch.h>
#include <emscripten/promise.h>
#include <emscripten/threading.h>

#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <list.h>
#include <term.h>
#include <trace.h>

#include "emscripten_sys.h"
#include "platform_defaultatoms.h"

/**
 * @brief resolve a promise with an int value and destroy it
 * @details called on the main thread using `emscripten_dispatch_to_thread`
 * @param promise promise to resolve and destroy
 * @param result `EM_PROMISE_FULFILL` to resolve or `EM_PROMISE_REJECT` to reject
 * @param value value to resolve or reject to
 */
void sys_promise_resolve_int_and_destroy(em_promise_t promise, em_promise_result_t result, int value)
{
    if (result == EM_PROMISE_FULFILL) {
        EM_ASM({
            promiseMap.get($0).resolve($1);
        },
            promise, value);
    } else {
        EM_ASM({
            promiseMap.get($0).reject($1);
        },
            promise, value);
    }
    emscripten_promise_destroy(promise);
}

/**
 * @brief resolve a promise with a string value and destroy it
 * @details called on the main thread using `emscripten_dispatch_to_thread`
 * @param promise promise to resolve and destroy
 * @param result `EM_PROMISE_FULFILL` to resolve or `EM_PROMISE_REJECT` to reject
 * @param value value to resolve or reject to
 */
void sys_promise_resolve_str_and_destroy(em_promise_t promise, em_promise_result_t result, int value)
{
    if (result == EM_PROMISE_FULFILL) {
        EM_ASM({
            promiseMap.get($0).resolve(UTF8ToString($1));
        },
            promise, value);
    } else {
        EM_ASM({
            promiseMap.get($0).reject(UTF8ToString($1));
        },
            promise, value);
    }
    emscripten_promise_destroy(promise);
}

static void promise_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct PromiseResource *promise_rsrc = (struct PromiseResource *) obj;
    if (!promise_rsrc->resolved) {
        emscripten_dispatch_to_thread(emscripten_main_runtime_thread_id(), EM_FUNC_SIG_VIII, sys_promise_resolve_str_and_destroy, NULL, promise_rsrc->promise, EM_PROMISE_REJECT, "noproc");
        promise_rsrc->resolved = true;
    }
}

static void htmlevent_user_data_dtor(ErlNifEnv *caller_env, void *obj)
{
    struct HTMLEventUserDataResource *htmlevent_user_data_rsrc = (struct HTMLEventUserDataResource *) obj;
    if (!term_is_invalid_term(htmlevent_user_data_rsrc->user_data)) {
        memory_sweep_mso_list(htmlevent_user_data_rsrc->storage[STORAGE_MSO_LIST_INDEX], caller_env->global);
        htmlevent_user_data_rsrc->user_data = term_invalid_term();
    }
    free(htmlevent_user_data_rsrc->target_element_str);
    htmlevent_user_data_rsrc->target_element_str = NULL;
}

static void htmlevent_user_data_down(ErlNifEnv *caller_env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    UNUSED(pid);
    UNUSED(mon);

    struct HTMLEventUserDataResource *htmlevent_user_data_rsrc = (struct HTMLEventUserDataResource *) obj;
    if (!htmlevent_user_data_rsrc->unregistered) {
        sys_enqueue_emscripten_unregister_htmlevent_message(caller_env->global, htmlevent_user_data_rsrc);
    }
}

static const ErlNifResourceTypeInit promise_resource_type_init = {
    .members = 1,
    .dtor = promise_dtor,
};

static const ErlNifResourceTypeInit htmlevent_user_data_resource_type_init = {
    .members = 3,
    .dtor = htmlevent_user_data_dtor,
    .stop = NULL,
    .down = htmlevent_user_data_down,
};

void sys_init_platform(GlobalContext *glb)
{
    struct EmscriptenPlatformData *platform = malloc(sizeof(struct EmscriptenPlatformData));
    if (IS_NULL_PTR(platform)) {
        fprintf(stderr, "Cannot allocate platform data");
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_mutex_init(&platform->poll_mutex, NULL))) {
        fprintf(stderr, "Cannot initialize pthread_mutex");
        AVM_ABORT();
    }
    if (UNLIKELY(pthread_cond_init(&platform->poll_cond, NULL))) {
        fprintf(stderr, "Cannot initialize pthread_cond");
        AVM_ABORT();
    }
    list_init(&platform->messages);
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, glb);
    platform->promise_resource_type = enif_init_resource_type(&env, "promise", &promise_resource_type_init, ERL_NIF_RT_CREATE, NULL);
    if (IS_NULL_PTR(platform->promise_resource_type)) {
        fprintf(stderr, "Cannot initialize promise_resource_type");
        AVM_ABORT();
    }
    platform->htmlevent_user_data_resource_type = enif_init_resource_type(&env, "htmlevent_user_data", &htmlevent_user_data_resource_type_init, ERL_NIF_RT_CREATE, NULL);
    if (IS_NULL_PTR(platform->htmlevent_user_data_resource_type)) {
        fprintf(stderr, "Cannot initialize promise_resource_type");
        AVM_ABORT();
    }
    glb->platform_data = platform;
}

void sys_free_platform(GlobalContext *glb)
{
    struct EmscriptenPlatformData *platform = glb->platform_data;
    pthread_cond_destroy(&platform->poll_cond);
    pthread_mutex_destroy(&platform->poll_mutex);
    free(platform);
}

static void sys_enqueue_emscripten_message(GlobalContext *glb, struct EmscriptenMessageBase *message)
{
    struct EmscriptenPlatformData *platform = glb->platform_data;
    pthread_mutex_lock(&platform->poll_mutex);
    list_append(&platform->messages, &message->message_head);
    pthread_cond_signal(&platform->poll_cond);
    pthread_mutex_unlock(&platform->poll_mutex);
}

#ifndef AVM_NO_SMP
void sys_signal(GlobalContext *glb)
{
    struct EmscriptenMessageBase *message = malloc(sizeof(struct EmscriptenMessageBase));
    message->message_type = Signal;
    sys_enqueue_emscripten_message(glb, message);
}
#endif

void sys_enqueue_emscripten_cast_message(GlobalContext *glb, const char *target, const char *message_content)
{
    struct EmscriptenMessageCast *message = malloc(sizeof(struct EmscriptenMessageCast));
    if (IS_NULL_PTR(message)) {
        fprintf(stderr, "Cannot allocate message");
        AVM_ABORT();
    }
    message->base.message_type = Cast;
    message->target_name = strdup(target);
    if (IS_NULL_PTR(message->target_name)) {
        fprintf(stderr, "Cannot copy target name");
        AVM_ABORT();
    }
    message->message = strdup(message_content);
    if (IS_NULL_PTR(message->message)) {
        fprintf(stderr, "Cannot copy message content");
        AVM_ABORT();
    }
    sys_enqueue_emscripten_message(glb, &message->base);
}

em_promise_t sys_enqueue_emscripten_call_message(GlobalContext *glb, const char *target, const char *message_content)
{
    em_promise_t promise = emscripten_promise_create();
    struct EmscriptenMessageCall *message = malloc(sizeof(struct EmscriptenMessageCall));
    if (IS_NULL_PTR(message)) {
        fprintf(stderr, "Cannot allocate message");
        AVM_ABORT();
    }
    struct EmscriptenPlatformData *platform = glb->platform_data;
    struct PromiseResource *promise_rsrc = enif_alloc_resource(platform->promise_resource_type, sizeof(struct PromiseResource));
    enif_keep_resource(promise_rsrc);
    promise_rsrc->promise = promise;
    promise_rsrc->resolved = false;
    message->promise_rsrc = promise_rsrc;
    message->target_name = strdup(target);
    if (IS_NULL_PTR(message->target_name)) {
        fprintf(stderr, "Cannot copy target name");
        AVM_ABORT();
    }
    message->message = strdup(message_content);
    if (IS_NULL_PTR(message->message)) {
        fprintf(stderr, "Cannot copy message content");
        AVM_ABORT();
    }
    message->base.message_type = Call;
    sys_enqueue_emscripten_message(glb, &message->base);
    return promise;
}

void sys_enqueue_emscripten_htmlevent_message(GlobalContext *glb, int32_t target_pid, term message_term, term user_data, HeapFragment *heap)
{
    struct EmscriptenMessageHTMLEvent *message = malloc(sizeof(struct EmscriptenMessageHTMLEvent));
    if (IS_NULL_PTR(message)) {
        fprintf(stderr, "Cannot allocate message");
        AVM_ABORT();
    }
    message->base.message_type = HTMLEvent;
    message->target_pid = target_pid;
    message->message = message_term;
    message->user_data = user_data;
    message->message_heap = heap;
    sys_enqueue_emscripten_message(glb, &message->base);
}

/**
 * @brief Enqueue an unregister request
 * @details To make sure handler's data is not accessed after unregistration,
 * enqueue the request to unregister it.
 * @param glb the global context
 * @param rsrc resource object representing the handler
 */
void sys_enqueue_emscripten_unregister_htmlevent_message(GlobalContext *glb, struct HTMLEventUserDataResource *rsrc)
{
    struct EmscriptenMessageUnregisterHTMLEvent *message = malloc(sizeof(struct EmscriptenMessageUnregisterHTMLEvent));
    if (IS_NULL_PTR(message)) {
        fprintf(stderr, "Cannot allocate message");
        AVM_ABORT();
    }
    message->base.message_type = UnregisterHTMLEvent;
    message->rsrc = rsrc;
    sys_enqueue_emscripten_message(glb, &message->base);
}

/**
 * @brief Unregister HTML Event handler
 * @details Actual call to unregister handler must be done on main thread which
 * has `JSEvents.eventHandlers`
 * @param rsrc resource object representing the handler
 */
static void sys_unregister_htmlevent_handler(struct HTMLEventUserDataResource *rsrc)
{
    if (!rsrc->unregistered) {
        switch (rsrc->event) {
            case EMSCRIPTEN_EVENT_KEYPRESS:
                emscripten_set_keypress_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_KEYDOWN:
                emscripten_set_keydown_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_KEYUP:
                emscripten_set_keyup_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_CLICK:
                emscripten_set_click_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_MOUSEDOWN:
                emscripten_set_mousedown_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_MOUSEUP:
                emscripten_set_mouseup_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_DBLCLICK:
                emscripten_set_dblclick_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_MOUSEMOVE:
                emscripten_set_mousemove_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_WHEEL:
                emscripten_set_wheel_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_RESIZE:
                emscripten_set_resize_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_SCROLL:
                emscripten_set_scroll_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_BLUR:
                emscripten_set_blur_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_FOCUS:
                emscripten_set_focus_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_FOCUSIN:
                emscripten_set_focusin_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_FOCUSOUT:
                emscripten_set_focusout_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_DEVICEORIENTATION:
                emscripten_set_deviceorientation_callback_on_thread(NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_DEVICEMOTION:
                emscripten_set_devicemotion_callback_on_thread(NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_ORIENTATIONCHANGE:
                emscripten_set_orientationchange_callback_on_thread(NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_FULLSCREENCHANGE:
                emscripten_set_fullscreenchange_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_POINTERLOCKCHANGE:
                emscripten_set_pointerlockchange_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_VISIBILITYCHANGE:
                emscripten_set_visibilitychange_callback_on_thread(NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_TOUCHSTART:
                emscripten_set_touchstart_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_TOUCHEND:
                emscripten_set_touchend_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_TOUCHMOVE:
                emscripten_set_touchmove_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_TOUCHCANCEL:
                emscripten_set_touchcancel_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_GAMEPADCONNECTED:
                emscripten_set_gamepadconnected_callback_on_thread(NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_GAMEPADDISCONNECTED:
                emscripten_set_gamepaddisconnected_callback_on_thread(NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_BEFOREUNLOAD:
                emscripten_set_beforeunload_callback_on_thread(NULL, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_BATTERYCHARGINGCHANGE:
                emscripten_set_batterychargingchange_callback_on_thread(NULL, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_BATTERYLEVELCHANGE:
                emscripten_set_batterylevelchange_callback_on_thread(NULL, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_MOUSEENTER:
                emscripten_set_mouseenter_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_MOUSELEAVE:
                emscripten_set_mouseleave_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_MOUSEOVER:
                emscripten_set_mouseover_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_MOUSEOUT:
                emscripten_set_mouseout_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
            case EMSCRIPTEN_EVENT_POINTERLOCKERROR:
                emscripten_set_pointerlockerror_callback_on_thread(rsrc->target_element, NULL, false, NULL, emscripten_main_runtime_thread_id());
                break;
        }
        rsrc->unregistered = true;
        enif_release_resource(rsrc);
    }
}

static int sys_emscripten_get_target(GlobalContext *glb, const char *target_name)
{
    size_t target_name_size = strlen(target_name);
    char target_atom_str[target_name_size + 1];
    target_atom_str[0] = target_name_size;
    memcpy(target_atom_str + 1, target_name, target_name_size);
    term target_atom = globalcontext_existing_term_from_atom_string(glb, target_atom_str);
    if (UNLIKELY(!term_is_atom(target_atom))) {
        return -1;
    }
    int target_atom_index = term_to_atom_index(target_atom);
    return globalcontext_get_registered_process(glb, target_atom_index);
}

static void sys_emscripten_send_message(GlobalContext *glb, int target_pid, const char *message, struct PromiseResource *promise)
{
    // call:
    // {emscripten, {call, Promise, Message}}
    // cast:
    // {emscripten, {cast, Message}}
    size_t message_len = strlen(message);
    bool is_call = promise != NULL;
    Heap heap;
    if (UNLIKELY(memory_init_heap(&heap, TUPLE_SIZE(is_call ? 3 : 2) + TUPLE_SIZE(2) + (is_call ? TERM_BOXED_RESOURCE_SIZE : 0) + term_binary_heap_size(message_len)) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    term message_term = term_alloc_tuple(2, &heap);
    term_put_tuple_element(message_term, 0, EMSCRIPTEN_ATOM);
    term payload = term_alloc_tuple(is_call ? 3 : 2, &heap);
    term_put_tuple_element(message_term, 1, payload);
    term_put_tuple_element(payload, 0, globalcontext_make_atom(glb, is_call ? ATOM_STR("\x4", "call") : ATOM_STR("\x4", "cast")));
    if (is_call) {
        term promise_term = term_from_resource(promise, &heap);
        term_put_tuple_element(payload, 1, promise_term);
    }
    term bin = term_from_literal_binary(message, message_len, &heap, glb);
    term_put_tuple_element(payload, is_call ? 2 : 1, bin);
    globalcontext_send_message(glb, target_pid, message_term);
    memory_destroy_heap(&heap, glb);
}

static void sys_process_emscripten_message(GlobalContext *glb, struct EmscriptenMessageBase *message)
{
    UNUSED(glb);
    switch (message->message_type) {
        case Cast: {
            struct EmscriptenMessageCast *message_send = (struct EmscriptenMessageCast *) message;
            int local_pid = sys_emscripten_get_target(glb, message_send->target_name);
            if (local_pid > 0) {
                sys_emscripten_send_message(glb, local_pid, message_send->message, NULL);
            }
            free(message_send->target_name);
            free(message_send->message);
        } break;

        case Call: {
            struct EmscriptenMessageCall *message_async_call = (struct EmscriptenMessageCall *) message;
            int local_pid = sys_emscripten_get_target(glb, message_async_call->target_name);
            if (local_pid > 0) {
                sys_emscripten_send_message(glb, local_pid, message_async_call->message, message_async_call->promise_rsrc);
            } else {
                emscripten_dispatch_to_thread(emscripten_main_runtime_thread_id(), EM_FUNC_SIG_VIII, sys_promise_resolve_str_and_destroy, NULL, message_async_call->promise_rsrc->promise, EM_PROMISE_REJECT, "noproc");
                message_async_call->promise_rsrc->resolved = true;
            }
            enif_release_resource(message_async_call->promise_rsrc);
            free(message_async_call->target_name);
            free(message_async_call->message);
        } break;

        case HTMLEvent: {
            struct EmscriptenMessageHTMLEvent *message_html_event = (struct EmscriptenMessageHTMLEvent *) message;
            bool has_user_data = !term_is_invalid_term(message_html_event->user_data);
            BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(3), heap) // one too many if has_user_data is false
            term message_term = term_alloc_tuple(has_user_data ? 3 : 2, &heap);
            term_put_tuple_element(message_term, 0, EMSCRIPTEN_ATOM);
            term_put_tuple_element(message_term, 1, message_html_event->message);
            if (has_user_data) {
                term_put_tuple_element(message_term, 2, message_html_event->user_data);
            }
            globalcontext_send_message(glb, message_html_event->target_pid, message_term);
            END_WITH_STACK_HEAP(heap, glb)
            memory_sweep_mso_list(message_html_event->message_heap->mso_list, glb);
            memory_destroy_heap_fragment(message_html_event->message_heap);
        } break;

        case UnregisterHTMLEvent: {
            struct EmscriptenMessageUnregisterHTMLEvent *message_unregister = (struct EmscriptenMessageUnregisterHTMLEvent *) message;
            if (!message_unregister->rsrc->unregistered) {
                emscripten_dispatch_to_thread(emscripten_main_runtime_thread_id(), EM_FUNC_SIG_VI, sys_unregister_htmlevent_handler, NULL, message_unregister->rsrc);
            }
        } break;

        case Signal:
            break;
    }
}

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
    struct EmscriptenPlatformData *platform = glb->platform_data;
    if (timeout_ms > 0) {
        struct timespec abstime;
        sys_monotonic_time(&abstime);
        int timeout_secs_part = timeout_ms / 1000;
        int timeout_ms_part = timeout_ms - (timeout_secs_part * 1000);
        abstime.tv_nsec += timeout_ms_part * 1000000;
        if (abstime.tv_nsec > 1000000000) {
            timeout_secs_part += 1;
            abstime.tv_nsec -= 1000000000;
        }
        abstime.tv_sec += timeout_secs_part;
        pthread_mutex_lock(&platform->poll_mutex);
        if (list_is_empty(&platform->messages)) {
            pthread_cond_timedwait(&platform->poll_cond, &platform->poll_mutex, &abstime);
        }
    } else if (timeout_ms < 0) {
        pthread_mutex_lock(&platform->poll_mutex);
        if (list_is_empty(&platform->messages)) {
            pthread_cond_wait(&platform->poll_cond, &platform->poll_mutex);
        }
    } else {
        pthread_mutex_lock(&platform->poll_mutex);
    }

    if (!list_is_empty(&platform->messages)) {
        struct ListHead messages = platform->messages;
        messages.next->prev = &messages;
        messages.prev->next = &messages;
        list_init(&platform->messages);
        pthread_mutex_unlock(&platform->poll_mutex);

        do {
            struct EmscriptenMessageBase *message = GET_LIST_ENTRY(list_first(&messages), struct EmscriptenMessageBase, message_head);
            sys_process_emscripten_message(glb, message);
            list_remove(&message->message_head);
            free(message);
        } while (!list_is_empty(&messages));
    } else {
        pthread_mutex_unlock(&platform->poll_mutex);
    }
}

void sys_listener_destroy(struct ListHead *item)
{
    UNUSED(item);
}

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

void sys_time(struct timespec *t)
{
    if (UNLIKELY(clock_gettime(CLOCK_REALTIME, t))) {
        fprintf(stderr, "Failed clock_gettime.\n");
        AVM_ABORT();
    }
}

void sys_monotonic_time(struct timespec *t)
{
    if (UNLIKELY(clock_gettime(CLOCK_MONOTONIC, t))) {
        fprintf(stderr, "Failed clock_gettime.\n");
        AVM_ABORT();
    }
}

uint64_t sys_monotonic_time_u64()
{
    // 2^64/10^9/86400/365 around 585 years
    double now = emscripten_get_now() * 1000000.0;
    return (uint64_t) now;
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms)
{
    return ms * 1000000;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t t)
{
    return t / 1000000;
}

static emscripten_fetch_t *fetch_file(const char *url)
{
    emscripten_fetch_attr_t attr;
    emscripten_fetch_attr_init(&attr);
    strcpy(attr.requestMethod, "GET");
    attr.attributes = EMSCRIPTEN_FETCH_LOAD_TO_MEMORY | EMSCRIPTEN_FETCH_SYNCHRONOUS;
    emscripten_fetch_t *fetch = emscripten_fetch(&attr, url);
    if (fetch->status == 200) {
        return fetch;
    } else {
        printf("Downloading %s failed, HTTP failure status code: %d.\n", fetch->url, fetch->status);
        emscripten_fetch_close(fetch);
        return NULL;
    }
}

static void *load_or_fetch_file(const char *path, emscripten_fetch_t **fetch, size_t *size)
{
    int fd = open(path, O_RDONLY);
    if (fd >= 0) {
        off_t fsize = lseek(fd, 0, SEEK_END);
        lseek(fd, 0, SEEK_SET);
        void *data = malloc(fsize);
        if (IS_NULL_PTR(data)) {
            close(fd);
            return NULL;
        }

        size_t r = read(fd, data, fsize);
        if (UNLIKELY(r != fsize)) {
            free(data);
            close(fd);
            return NULL;
        }
        if (size) {
            *size = fsize;
        }
        return data;
    }
    if (UNLIKELY(errno != ENOENT)) {
        return NULL;
    }

    *fetch = fetch_file(path);
    if (IS_NULL_PTR(*fetch)) {
        return NULL;
    }
    if (size) {
        *size = (*fetch)->numBytes;
    }
    return (void *) (*fetch)->data;
}

enum OpenAVMResult sys_open_avm_from_file(
    GlobalContext *global, const char *path, struct AVMPackData **avm_data)
{
    TRACE("sys_open_avm_from_file: Going to open: %s\n", path);

    UNUSED(global);

    emscripten_fetch_t *fetch = NULL;
    void *data = load_or_fetch_file(path, &fetch, NULL);
    if (IS_NULL_PTR(data)) {
        return AVM_OPEN_CANNOT_OPEN;
    }

    struct ConstAVMPack *const_avm = malloc(sizeof(struct ConstAVMPack));
    if (IS_NULL_PTR(const_avm)) {
        if (fetch) {
            emscripten_fetch_close(fetch);
        } else {
            free(data);
        }
        return AVM_OPEN_FAILED_ALLOC;
    }
    avmpack_data_init(&const_avm->base, &const_avm_pack_info);
    const_avm->base.data = (const uint8_t *) data;

    *avm_data = &const_avm->base;
    return AVM_OPEN_OK;
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    TRACE("sys_open_avm_from_file: Going to open: %s\n", path);

    size_t size;
    emscripten_fetch_t *fetch = NULL;
    void *data = load_or_fetch_file(path, &fetch, &size);
    if (IS_NULL_PTR(data)) {
        return NULL;
    }

    if (UNLIKELY(!iff_is_valid_beam(data))) {
        fprintf(stderr, "%s is not a valid BEAM file.\n", path);
        if (fetch) {
            emscripten_fetch_close(fetch);
        } else {
            free(data);
        }
        return NULL;
    }
    Module *new_module = module_new_from_iff_binary(global, data, size);
    if (IS_NULL_PTR(new_module)) {
        if (fetch) {
            emscripten_fetch_close(fetch);
        } else {
            free(data);
        }
        return NULL;
    }
    new_module->module_platform_data = NULL;

    return new_module;
}

Module *sys_load_module(GlobalContext *global, const char *module_name)
{
    const void *beam_module = NULL;
    uint32_t beam_module_size = 0;

    struct ListHead *avmpack_data_list = synclist_rdlock(&global->avmpack_data);
    struct ListHead *item;
    LIST_FOR_EACH (item, avmpack_data_list) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        avmpack_data->in_use = true;
        if (avmpack_find_section_by_name(avmpack_data->data, module_name, &beam_module, &beam_module_size)) {
            break;
        }
    }
    synclist_unlock(&global->avmpack_data);

    if (IS_NULL_PTR(beam_module)) {
        fprintf(stderr, "Failed to open module: %s\n", module_name);
        return NULL;
    }

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    new_module->module_platform_data = NULL;

    return new_module;
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    UNUSED(glb);
    UNUSED(driver_name);
    UNUSED(opts);
    return NULL;
}

term sys_get_info(Context *ctx, term key)
{
    UNUSED(ctx);
    UNUSED(key);
    return UNDEFINED_ATOM;
}
