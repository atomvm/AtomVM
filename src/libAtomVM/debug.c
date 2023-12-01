/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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

#include "debug.h"

static COLD_FUNC void debug_display_type(term t, const Context *ctx)
{
    if (term_is_atom(t) || term_is_integer(t) || term_is_nil(t) || term_is_pid(t)) {
        term_display(stderr, t, ctx);
    } else if ((t & 0x3F) == 0) {
        fprintf(stderr, "tuple(%zu)", term_get_size_from_boxed_header(t));
    } else if (term_is_boxed(t)) {
        fprintf(stderr, "boxed(0x%lx)", (unsigned long) term_to_term_ptr(t));
    } else if ((t & 0x3) == 0x1) {
        fprintf(stderr, "list(0x%lx)", (unsigned long) term_to_term_ptr(t));
    } else if (term_is_catch_label(t)) {
        int module_index;
        int catch_label = term_to_catch_label_and_module(t, &module_index);
        fprintf(stderr, "catch label(%i:%i)", module_index, catch_label);
    } else if (term_is_cp(t)) {
        fprintf(stderr, "continuation pointer");
    } else {
        fprintf(stderr, "unknown");
    }
}

static COLD_FUNC void debug_dump_binary_mem(char *buf, term val, unsigned n)
{
    for (unsigned int i = 0; i < n; ++i) {
        int bit_i = val >> i & 0x1;
        buf[(n - 1) - i] = bit_i ? '1' : '0';
    }
    buf[n] = '\0';
}

static COLD_FUNC void debug_dump_term(Context *ctx, term *pos, const char *region, unsigned i)
{
    term t = *pos;
    // TODO use TERM_BITS instead
    char buf[32 + 1];
    debug_dump_binary_mem(buf, *pos, 32);
    fprintf(stderr, "DEBUG: %s 0x%p %3i: (%s)b 0x%09" TERM_X_FMT ": ", region, (void *) pos, i, buf, t);
    debug_display_type(t, ctx);
    fprintf(stderr, "\n");
}

COLD_FUNC void debug_dump_memory(Context *ctx, term *start, term *end, const char *region)
{
    unsigned long size = end - start;
    fprintf(stderr, "DEBUG:\n");
    fprintf(stderr, "DEBUG: %s start: 0x%lx\n", region, (unsigned long) start);
    fprintf(stderr, "DEBUG: %s end:   0x%lx\n", region, (unsigned long) end);
    fprintf(stderr, "DEBUG: %s size:  %li words\n", region, size);
    term *pos = start;
    for (unsigned i = 0; i < size; ++i) {
        debug_dump_term(ctx, pos, region, i);
        ++pos;
    }
    fprintf(stderr, "DEBUG:\n");
}

COLD_FUNC void debug_dump_context(Context *ctx)
{
    debug_dump_heap(ctx);
    debug_dump_stack(ctx);
    debug_dump_registers(ctx);
}

COLD_FUNC void debug_dump_heap(Context *ctx)
{
    debug_dump_memory(ctx, ctx->heap.heap_start, ctx->heap.heap_ptr, "heap");
}

COLD_FUNC void debug_dump_stack(Context *ctx)
{
    term *stack_base = context_stack_base(ctx);
    debug_dump_memory(ctx, ctx->e, stack_base, "stack");
}

COLD_FUNC void debug_dump_registers(Context *ctx)
{
    debug_dump_memory(ctx, ctx->x, ctx->x + 16, "register");
}

COLD_FUNC void debug_print_processes_list(struct ListHead *processes)
{
    Context *contexts = GET_LIST_ENTRY(processes, Context, processes_list_head);
    if (!contexts) {
        printf("No processes\n");
        return;
    }

    Context *context = contexts;
    printf("Processes list:\n");
    do {
        printf("%" PRIu32 ": %p\n", context->process_id, (void *) context);
        context = GET_LIST_ENTRY(context->processes_list_head.next, Context, processes_list_head);
    } while (context != contexts);
    printf("\n");
}

COLD_FUNC char reg_type_c(int reg_type)
{
    switch (reg_type) {
        case 2:
            return 'a';

        case 3:
            return 'x';

        case 4:
            return 'y';

        case 12:
            return 'y';

        default:
            return '?';
    }
}
