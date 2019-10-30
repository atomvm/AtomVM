/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include "debug.h"

static COLD_FUNC void debug_display_type(term t, const Context *ctx)
{
    if (term_is_atom(t) || term_is_integer(t) || term_is_nil(t) || term_is_pid(t)) {
        term_display(stderr, t, ctx);
    } else if ((t & 0x3F) == 0x24) {
        fprintf(stderr, "binary(%li)", (unsigned long) (t >> 6));
    } else if ((t & 0x3F) == 0) {
        fprintf(stderr, "tuple(%i)", term_get_size_from_boxed_header(t));
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
    for (unsigned int i = 0; i < n;  ++i) {
        int bit_i = val >> i & 0x1;
        buf[(n - 1) - i] = bit_i ? '1' : '0';
    }
    buf[n] = '\0';
}

static COLD_FUNC size_t debug_dump_term(Context *ctx, term *pos, const char *region, unsigned i)
{
    term t = *pos;
    unsigned long boxed_words = 0, boxed_words_i = 0;
    while (1) {
        // TODO use TERM_BITS instead
        size_t n = sizeof(term) * 8;
        char buf[n + 1];
        debug_dump_binary_mem(buf, *pos, n);
        const char* format_str =
        #if TERM_BYTES == 4
            "DEBUG: %s 0x%lx %3i: (%s)b 0x%08lx: ";
        #elif TERM_BYTES == 8
            "DEBUG: %s 0x%lx %3i: (%s)b 0x%016lx: ";
        #else
            #error
        #endif
        fprintf(stderr, format_str, region, (unsigned long) pos, i, buf, t);

        if (strncmp(region, "f", 1) == 0) {
            fprintf(stderr, "\n");
            return i + 1;
        } else if (boxed_words) {
            if (boxed_words_i > 1) {
                fprintf(stderr, "^^^\n");
                boxed_words_i--;
                pos++; i++;
            } else {
                fprintf(stderr, "^^^\n");
                return boxed_words + 1;
            }
        } else if (term_is_atom(t) || term_is_integer(t) || term_is_nil(t) || term_is_pid(t)) {
            term_display(stderr, t, ctx);
            fprintf(stderr, "\n");
            return i + 1;
        } else if ((t & 0x3F) == 0x24) {
            boxed_words = boxed_words_i = (unsigned long) (t >> 6);
            fprintf(stderr, "binary(%li)\n", boxed_words);
            pos++; i++;
        } else if ((t & 0x3F) == 0) {
            fprintf(stderr, "tuple(%i)\n", term_get_size_from_boxed_header(t));
            return i + 1;
        } else if (term_is_boxed(t)) {
            fprintf(stderr, "boxed(0x%lx)\n", (unsigned long) term_to_term_ptr(t));
            return i + 1;
        } else if ((t & 0x3) == 0x1) {
            fprintf(stderr, "list(0x%lx)\n", (unsigned long) term_to_term_ptr(t));
            return i + 1;
        } else if (term_is_catch_label(t)) {
            int module_index;
            int catch_label = term_to_catch_label_and_module(t, &module_index);
            fprintf(stderr, "catch label(%i:%i)\n", module_index, catch_label);
            return i + 1;
        } else if (term_is_cp(t)) {
            fprintf(stderr, "continuation pointer\n");
            return i + 1;
        } else {
            fprintf(stderr, "unknown\n");
            return i + 1;
        }
    }

    debug_display_type(t, ctx);
    return i + 1;
}

COLD_FUNC void debug_dump_memory(Context *ctx, term *start, term *end, const char *region)
{
    if (start <= end) {
        unsigned long size = end - start;
        fprintf(stderr, "DEBUG:\n");
        fprintf(stderr, "DEBUG: %s start: 0x%lx\n", region, (unsigned long) start);
        fprintf(stderr, "DEBUG: %s end:   0x%lx\n", region, (unsigned long) end);
        fprintf(stderr, "DEBUG: %s size:  %li words\n", region, size);
        term *pos = start;
        for (unsigned i = 0; i < size;) {
            i = debug_dump_term(ctx, pos, region, i);
            pos += i;
        }
    } else {
        fprintf(stderr, "DEBUG:\n");
        fprintf(stderr, "DEBUG: %s ERROR! start > end!!\n", region);
        fprintf(stderr, "DEBUG: %s start: 0x%lx\n", region, (unsigned long) start);
        fprintf(stderr, "DEBUG: %s end:   0x%lx\n", region, (unsigned long) end);
        abort();
    }
    fprintf(stderr, "DEBUG:\n");
}

COLD_FUNC void debug_dump_heap(Context *ctx)
{
    debug_dump_memory(ctx, ctx->heap_start, ctx->heap_ptr, "h");
}

COLD_FUNC void debug_dump_free(Context *ctx)
{
    debug_dump_memory(ctx, ctx->heap_ptr, ctx->e, "f");
}

COLD_FUNC void debug_dump_stack(Context *ctx)
{
    debug_dump_memory(ctx, ctx->e, ctx->stack_base, "s");
}

COLD_FUNC void debug_dump_registers(Context *ctx)
{
    debug_dump_memory(ctx, ctx->x, ctx->x + 16, "r");
}

COLD_FUNC void debug_dump_context(Context *ctx)
{
    debug_dump_heap(ctx);
    debug_dump_free(ctx);
    debug_dump_stack(ctx);
    debug_dump_registers(ctx);
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
        printf("%i: %p\n", context->process_id, (void *) context);
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
