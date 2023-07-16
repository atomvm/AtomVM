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
#include <nifs.h>
#include <term.h>
#include <term_typedef.h>

#include <emscripten.h>
#include <emscripten/promise.h>
#include <emscripten/proxying.h>
#include <emscripten/threading.h>

//#define ENABLE_TRACE
#include <trace.h>

#include "emscripten_sys.h"
#include "platform_defaultatoms.h"
#include "platform_nifs.h"

static term nif_atomvm_platform(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    return EMSCRIPTEN_ATOM;
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
        fprintf(stderr, "bad arg, run_script");
        term_display(stderr, argv[0], ctx);
        fprintf(stderr, "\n");
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
        if (UNLIKELY(!ok || str == NULL)) {
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

const struct Nif *platform_nifs_get_nif(const char *nifname)
{
    if (strcmp("atomvm:platform/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &atomvm_platform_nif;
    }
    if (strcmp("emscripten:run_script/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &emscripten_run_script_nif;
    }
    if (strcmp("emscripten:run_script/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &emscripten_run_script_nif;
    }
    if (strcmp("emscripten:promise_resolve/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &emscripten_promise_resolve_nif;
    }
    if (strcmp("emscripten:promise_resolve/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &emscripten_promise_resolve_nif;
    }
    if (strcmp("emscripten:promise_reject/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &emscripten_promise_reject_nif;
    }
    if (strcmp("emscripten:promise_reject/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &emscripten_promise_reject_nif;
    }
    return NULL;
}
