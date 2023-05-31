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

void port_ensure_available(Context *ctx, size_t size)
{
    if (context_avail_free_memory(ctx) < size) {
        switch (memory_ensure_free_opt(ctx, size, MEMORY_NO_GC)) {
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
}

int port_is_standard_port_command(term t)
{
    if (!term_is_tuple(t)) {
        return 0;
    } else if (term_get_tuple_arity(t) != 3) {
        return 0;
    } else {
        term pid = term_get_tuple_element(t, 0);
        term ref = term_get_tuple_element(t, 1);
        if (!term_is_pid(pid)) {
            return 0;
        } else if (!term_is_reference(ref)) {
            return 0;
        } else {
            return 1;
        }
    }
}

term port_heap_create_tuple2(Heap *heap, term a, term b)
{
    term terms[2];
    terms[0] = a;
    terms[1] = b;

    return port_heap_create_tuple_n(heap, 2, terms);
}

term port_heap_create_tuple3(Heap *heap, term a, term b, term c)
{
    term terms[3];
    terms[0] = a;
    terms[1] = b;
    terms[2] = c;

    return port_heap_create_tuple_n(heap, 3, terms);
}

term port_heap_create_tuple_n(Heap *heap, size_t num_terms, term *terms)
{
    term ret = term_alloc_tuple(num_terms, heap);

    for (size_t i = 0; i < num_terms; ++i) {
        term_put_tuple_element(ret, i, terms[i]);
    }

    return ret;
}

term port_heap_create_error_tuple(Heap *heap, term reason)
{
    return port_heap_create_tuple2(heap, ERROR_ATOM, reason);
}

term port_heap_create_sys_error_tuple(Heap *heap, term syscall, int errno)
{
    term reason = port_heap_create_tuple2(heap, syscall, term_from_int32(errno));
    return port_heap_create_error_tuple(heap, reason);
}

term port_heap_create_ok_tuple(Heap *heap, term t)
{
    return port_heap_create_tuple2(heap, OK_ATOM, t);
}

term port_heap_create_reply(Heap *heap, term ref, term payload)
{
    return port_heap_create_tuple2(heap, ref, payload);
}
