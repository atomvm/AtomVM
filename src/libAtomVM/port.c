/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Fred Dushin <fred@dushin.net>
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

#include "port.h"
#include "context.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "mailbox.h"

term port_create_tuple2(Context *ctx, term a, term b)
{
    term terms[2];
    terms[0] = a;
    terms[1] = b;

    return port_create_tuple_n(ctx, 2, terms);
}

term port_create_tuple3(Context *ctx, term a, term b, term c)
{
    term terms[3];
    terms[0] = a;
    terms[1] = b;
    terms[2] = c;

    return port_create_tuple_n(ctx, 3, terms);
}

term port_create_tuple_n(Context *ctx, size_t num_terms, term *terms)
{
    term ret = term_alloc_tuple(num_terms, ctx);

    for (size_t i = 0; i < num_terms; ++i) {
        term_put_tuple_element(ret, i, terms[i]);
    }

    return ret;
}

term port_create_error_tuple(Context *ctx, term reason)
{
    return port_create_tuple2(ctx, ERROR_ATOM, reason);
}

term port_create_sys_error_tuple(Context *ctx, term syscall, int errno)
{
    term reason = port_create_tuple2(ctx, syscall, term_from_int32(errno));
    return port_create_error_tuple(ctx, reason);
}

term port_create_ok_tuple(Context *ctx, term t)
{
    return port_create_tuple2(ctx, OK_ATOM, t);
}

term port_create_reply(Context *ctx, term ref, term payload)
{
    return port_create_tuple2(ctx, ref, payload);
}

void port_send_message(GlobalContext *glb, term pid, term msg)
{
    int local_process_id = term_to_local_process_id(pid);
    globalcontext_send_message(glb, local_process_id, msg);
}

void port_send_message_nolock(GlobalContext *glb, term pid, term msg)
{
    int local_process_id = term_to_local_process_id(pid);
    globalcontext_send_message_nolock(glb, local_process_id, msg);
}

void port_ensure_available_with_roots(Context *ctx, size_t size, size_t num_roots, term *roots, enum MemoryShrinkMode shrink_mode)
{
    switch (memory_ensure_free_with_roots(ctx, size, num_roots, roots, shrink_mode)) {
        case MEMORY_GC_OK:
            break;
        case MEMORY_GC_ERROR_FAILED_ALLOCATION:
            // TODO Improve error handling
            fprintf(stderr, "Failed to allocate additional heap storage: [%s:%i]\n", __FILE__, __LINE__);
            AVM_ABORT();
        case MEMORY_GC_DENIED_ALLOCATION:
            // TODO Improve error handling
            fprintf(stderr, "Not permitted to allocate additional heap storage: [%s:%i]\n", __FILE__, __LINE__);
            AVM_ABORT();
    }
}

bool port_is_standard_port_command(term t)
{
    if (!term_is_tuple(t)) {
        return false;
    } else if (term_get_tuple_arity(t) != 3) {
        return false;
    } else {
        term pid = term_get_tuple_element(t, 0);
        term ref = term_get_tuple_element(t, 1);
        if (!term_is_pid(pid)) {
            return false;
        } else if (!term_is_reference(ref)) {
            return false;
        } else {
            return true;
        }
    }
}

term port_heap_create_tuple2(term **heap_ptr, term a, term b)
{
    term terms[2];
    terms[0] = a;
    terms[1] = b;

    return port_heap_create_tuple_n(heap_ptr, 2, terms);
}

term port_heap_create_tuple3(term **heap_ptr, term a, term b, term c)
{
    term terms[3];
    terms[0] = a;
    terms[1] = b;
    terms[2] = c;

    return port_heap_create_tuple_n(heap_ptr, 3, terms);
}

term port_heap_create_tuple_n(term **heap_ptr, size_t num_terms, term *terms)
{
    term ret = term_heap_alloc_tuple(num_terms, heap_ptr);

    for (size_t i = 0; i < num_terms; ++i) {
        term_put_tuple_element(ret, i, terms[i]);
    }

    return ret;
}

term port_heap_create_error_tuple(term **heap_ptr, term reason)
{
    return port_heap_create_tuple2(heap_ptr, ERROR_ATOM, reason);
}

term port_heap_create_sys_error_tuple(term **heap_ptr, term syscall, int errno)
{
    term reason = port_heap_create_tuple2(heap_ptr, syscall, term_from_int32(errno));
    return port_heap_create_error_tuple(heap_ptr, reason);
}

term port_heap_create_ok_tuple(term **heap_ptr, term t)
{
    return port_heap_create_tuple2(heap_ptr, OK_ATOM, t);
}

term port_heap_create_reply(term **heap_ptr, term ref, term payload)
{
    return port_heap_create_tuple2(heap_ptr, ref, payload);
}
