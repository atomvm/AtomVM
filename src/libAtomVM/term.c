/*
 * This file is part of AtomVM.
 *
 * Copyright 2018,2019 Davide Bettio <davide@uninstall.it>
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

#include "term.h"

#include "atom.h"
#include "atom_table.h"
#include "context.h"
#include "interop.h"
#include "tempstack.h"

#include <ctype.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>

struct FprintfFun
{
    PrinterFun base;
    FILE *stream;
};

struct SnprintfFun
{
    PrinterFun base;
    int size;
    char *buf;
};

const term empty_tuple = 0;

int fprintf_printer(PrinterFun *fun, const char *fmt, ...)
{
    int ret;

    va_list args;
    va_start(args, fmt);

    FILE *stream = CONTAINER_OF(fun, struct FprintfFun, base)->stream;
    ret = vfprintf(stream, fmt, args);

    va_end(args);

    return ret;
}

int snprintf_printer(PrinterFun *fun, const char *fmt, ...)
{
    int ret;

    va_list args;
    va_start(args, fmt);

    struct SnprintfFun *snpf = CONTAINER_OF(fun, struct SnprintfFun, base);
    ret = vsnprintf(snpf->buf, snpf->size, fmt, args);
    snpf->buf += ret;
    snpf->size -= ret;

    va_end(args);

    return ret;
}

void term_display(FILE *fd, term t, const Context *ctx)
{
    term_fprint(fd, t, ctx->global);
}

int term_fprint(FILE *stream, term t, const GlobalContext *global)
{
    struct FprintfFun fprintf_fun = {
        .base = {
            .print = fprintf_printer
        },
        .stream = stream
    };

    return term_funprint(&fprintf_fun.base, t, global);
}

int term_snprint(char *buf, size_t size, term t, const GlobalContext *global)
{
    struct SnprintfFun snprintf_fun = {
        .base = {
            .print = snprintf_printer
        },
        .buf = buf,
        .size = size
    };

    return term_funprint(&snprintf_fun.base, t, global);
}

int term_funprint(PrinterFun *fun, term t, const GlobalContext *global)
{
    if (term_is_atom(t)) {
        int atom_index = term_to_atom_index(t);
        AtomString atom_string = atom_table_get_atom_string(global->atom_table, atom_index);
        return fun->print(fun, "%.*s", (int) atom_string_len(atom_string),
            (char *) atom_string_data(atom_string));

    } else if (term_is_integer(t)) {
        avm_int_t iv = term_to_int(t);
        return fun->print(fun, AVM_INT_FMT, iv);

    } else if (term_is_nil(t)) {
        return fun->print(fun, "[]");

    } else if (term_is_nonempty_list(t)) {
        int is_printable = 1;
        term list_item = t;
        while (term_is_nonempty_list(list_item)) {
            term head = term_get_list_head(list_item);
            is_printable = is_printable && term_is_uint8(head) && isprint(term_to_uint8(head));
            list_item = term_get_list_tail(list_item);
        }
        // improper lists are not printable
        if (!term_is_nil(list_item)) {
            is_printable = 0;
        }

        if (is_printable) {
            int ok;
            char *printable = interop_list_to_string(t, &ok);
            if (LIKELY(ok)) {
                int ret = fun->print(fun, "\"%s\"", printable);
                free(printable);
                return ret;
            } else {
                return fun->print(fun, "???");
            }

        } else {
            int ret = fun->print(fun, "[");
            if (UNLIKELY(ret < 0)) {
                return ret;
            }
            int display_separator = 0;
            while (term_is_nonempty_list(t)) {
                if (display_separator) {
                    ret += fun->print(fun, ",");
                } else {
                    display_separator = 1;
                }

                int printed = term_funprint(fun, term_get_list_head(t), global);
                if (UNLIKELY(printed < 0)) {
                    return printed;
                }
                ret += printed;
                t = term_get_list_tail(t);
            }
            if (!term_is_nil(t)) {
                int printed = fun->print(fun, "|");
                if (UNLIKELY(printed < 0)) {
                    return printed;
                }
                ret += printed;

                printed = term_funprint(fun, t, global);
                if (UNLIKELY(printed < 0)) {
                    return printed;
                }
                ret += printed;
            }
            int printed = fun->print(fun, "]");
            if (UNLIKELY(printed < 0)) {
                return printed;
            }
            ret += printed;
            return ret;
        }
    } else if (term_is_pid(t)) {
        return fun->print(fun, "<0.%" PRIu32 ".0>", term_to_local_process_id(t));

    } else if (term_is_function(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);
        Module *fun_module = (Module *) boxed_value[1];
        uint32_t fun_index = boxed_value[2];
        const char *format =
        #ifdef __clang__
                "#Fun<erl_eval.%lu.%llu>";
        #else
                "#Fun<erl_eval.%lu.%llu>";
        #endif
        return fun->print(fun, format, fun_index, (unsigned long) fun_module);

    } else if (term_is_tuple(t)) {
        int ret = fun->print(fun, "{");
        if (UNLIKELY(ret < 0)) {
            return ret;
        }

        int tuple_size = term_get_tuple_arity(t);
        for (int i = 0; i < tuple_size; i++) {
            if (i != 0) {
                int printed = fun->print(fun, ",");
                if (UNLIKELY(printed < 0)) {
                    return printed;
                }
                ret += printed;
            }
            int printed = term_funprint(fun, term_get_tuple_element(t, i), global);
            if (UNLIKELY(printed < 0)) {
                return printed;
            }
            ret += printed;
        }

        int printed = fun->print(fun, "}");
        if (UNLIKELY(printed < 0)) {
            return printed;
        }
        ret += printed;
        return ret;

    } else if (term_is_map(t)) {
        int ret = fun->print(fun, "#{");
        if (UNLIKELY(ret < 0)) {
            return ret;
        }

        int map_size = term_get_map_size(t);
        for (int i = 0; i < map_size; i++) {
            if (i != 0) {
                int printed = fun->print(fun, ",");
                if (UNLIKELY(printed < 0)) {
                    return printed;
                }
                ret += printed;
            }
            int printed = term_funprint(fun, term_get_map_key(t, i), global);
            if (UNLIKELY(printed < 0)) {
                return printed;
            }
            ret += printed;

            printed = fun->print(fun, "=>");
            if (UNLIKELY(printed < 0)) {
                return printed;
            }
            ret += printed;

            printed = term_funprint(fun, term_get_map_value(t, i), global);
            if (UNLIKELY(printed < 0)) {
                return printed;
            }
            ret += printed;
        }

        int printed = fun->print(fun, "}");
        if (UNLIKELY(printed < 0)) {
            return printed;
        }
        ret += printed;
        return ret;

    } else if (term_is_binary(t)) {
        int len = term_binary_size(t);
        const unsigned char *binary_data = (const unsigned char *) term_binary_data(t);

        int is_printable = 1;
        for (int i = 0; i < len; i++) {
            if (!isprint(binary_data[i])) {
                is_printable = 0;
                break;
            }
        }

        int ret = fun->print(fun, "<<");
        if (UNLIKELY(ret < 0)) {
            return ret;
        }

        if (is_printable) {
            int printed = fun->print(fun, "\"%.*s\"", len, binary_data);
            if (UNLIKELY(printed < 0)) {
                return printed;
            }
            ret += printed;

        } else {
            int display_separator = 0;
            for (int i = 0; i < len; i++) {
                if (display_separator) {
                    int printed = fun->print(fun, ",");
                    if (UNLIKELY(printed < 0)) {
                        return printed;
                    }
                    ret += printed;
                } else {
                    display_separator = 1;
                }

                uint8_t c = (uint8_t) binary_data[i];
                int printed = fun->print(fun, "%i", (int) c);
                if (UNLIKELY(printed < 0)) {
                    return printed;
                }
                ret += printed;
            }
        }
        int printed = fun->print(fun, ">>");
        if (UNLIKELY(printed < 0)) {
            return printed;
        }
        ret += printed;
        return ret;

    } else if (term_is_reference(t)) {
        const char *format =
#ifdef __clang__
        "#Ref<0.0.0.%llu>";
#else
        "#Ref<0.0.0.%lu>";
#endif
        return fun->print(fun, format, term_to_ref_ticks(t));

    } else if (term_is_boxed_integer(t)) {
        int size = term_boxed_size(t);
        switch (size) {
            case 1:
                return fun->print(fun, AVM_INT_FMT, term_unbox_int(t));

#if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2:
                return fun->print(fun, AVM_INT64_FMT, term_unbox_int64(t));
#endif
            default:
                AVM_ABORT();
        }

    } else if (term_is_float(t)) {
        avm_float_t f = term_to_float(t);
        return fun->print(fun, AVM_FLOAT_FMT, f);

    } else {
        return fun->print(fun, "Unknown term type: %" TERM_U_FMT, t);
    }
}

static int term_type_to_index(term t)
{
    if (term_is_invalid_term(t)) {
        return 0;

    } else if (term_is_any_integer(t)) {
        return 1;

    } else if (term_is_float(t)) {
        return 2;

    } else if (term_is_atom(t)) {
        return 3;

    } else if (term_is_reference(t)) {
        return 4;

    } else if (term_is_function(t)) {
        return 5;

    } else if (term_is_pid(t)) {
        return 6;

    } else if (term_is_tuple(t)) {
        return 7;

    } else if (term_is_nil(t)) {
        return 8;

    } else if (term_is_nonempty_list(t)) {
        return 9;

    } else if (term_is_binary(t)) {
        return 10;

    } else if (term_is_map(t)) {
        return 11;

    } else {
        AVM_ABORT();
    }
}

#define BEGIN_MAP_KEY TERM_RESERVED_MARKER(1)
#define END_MAP_KEY TERM_RESERVED_MARKER(0)

#define CMP_POP_AND_CONTINUE()                                                                     \
    other = temp_stack_pop(&temp_stack);                                                           \
    if (other == BEGIN_MAP_KEY) {                                                                  \
        map_key_nesting++;                                                                       \
        other = temp_stack_pop(&temp_stack);                                                       \
    } else if (other == END_MAP_KEY) {                                                             \
        map_key_nesting--;                                                                       \
        other = temp_stack_pop(&temp_stack);                                                       \
    }                                                                                              \
    t = temp_stack_pop(&temp_stack);

TermCompareResult term_compare(term t, term other, TermCompareOpts opts, GlobalContext *global)
{
    struct TempStack temp_stack;
    if (UNLIKELY(temp_stack_init(&temp_stack) != TempStackOk)) {
        return TermCompareMemoryAllocFail;
    }

    if (UNLIKELY(temp_stack_push(&temp_stack, t) != TempStackOk)) {
        return TermCompareMemoryAllocFail;
    }
    if (UNLIKELY(temp_stack_push(&temp_stack, other) != TempStackOk)) {
        return TermCompareMemoryAllocFail;
    }

    TermCompareResult result = TermEquals;
    int map_key_nesting = 0;

    while (!temp_stack_is_empty(&temp_stack)) {
        if (t == other) {
            CMP_POP_AND_CONTINUE();

        } else if (term_is_integer(t) && term_is_integer(other)) {
            avm_int_t t_int = term_to_int(t);
            avm_int_t other_int = term_to_int(other);
            //They cannot be equal
            result = (t_int > other_int) ? TermGreaterThan : TermLessThan;
            break;

        } else if (term_is_reference(t) && term_is_reference(other)) {
            int64_t t_ticks = term_to_ref_ticks(t);
            int64_t other_ticks = term_to_ref_ticks(other);
            if (t_ticks == other_ticks) {
                CMP_POP_AND_CONTINUE();
            } else {
                result = (t_ticks > other_ticks) ? TermGreaterThan : TermLessThan;
                break;
            }

        } else if (term_is_nonempty_list(t) && term_is_nonempty_list(other)) {
            term t_tail = term_get_list_tail(t);
            term other_tail = term_get_list_tail(other);
            // invalid term is used as a term lower than any other
            // so "a" < "ab" -> true can be implemented.
            if (term_is_nil(t_tail)) {
                t_tail = term_invalid_term();
            }
            if (term_is_nil(other_tail)) {
                other_tail = term_invalid_term();
            }
            if (UNLIKELY(temp_stack_push(&temp_stack, t_tail) != TempStackOk)) {
                return TermCompareMemoryAllocFail;
            }
            if (UNLIKELY(temp_stack_push(&temp_stack, other_tail) != TempStackOk)) {
                return TermCompareMemoryAllocFail;
            }
            t = term_get_list_head(t);
            other = term_get_list_head(other);

        } else if (term_is_tuple(t) && term_is_tuple(other)) {
            int tuple_size = term_get_tuple_arity(t);
            int other_tuple_size = term_get_tuple_arity(other);

            if (tuple_size != other_tuple_size) {
                result = (tuple_size > other_tuple_size) ? TermGreaterThan : TermLessThan;
                break;
            }

            if (tuple_size > 0) {
                for (int i = tuple_size - 1; i >= 1; i--) {
                    if (UNLIKELY(temp_stack_push(&temp_stack, term_get_tuple_element(t, i))
                            != TempStackOk)) {
                        return TermCompareMemoryAllocFail;
                    }
                    if (UNLIKELY(temp_stack_push(&temp_stack, term_get_tuple_element(other, i))
                            != TempStackOk)) {
                        return TermCompareMemoryAllocFail;
                    }
                }
                t = term_get_tuple_element(t, 0);
                other = term_get_tuple_element(other, 0);

            } else {
                CMP_POP_AND_CONTINUE();
            }

        } else if (term_is_binary(t) && term_is_binary(other)) {
            int t_size = term_binary_size(t);
            int other_size = term_binary_size(other);

            const char *t_data = term_binary_data(t);
            const char *other_data = term_binary_data(other);

            int cmp_size = (t_size > other_size) ? other_size : t_size;

            int memcmp_result = memcmp(t_data, other_data, cmp_size);
            if (memcmp_result == 0) {
                if (t_size == other_size) {
                    CMP_POP_AND_CONTINUE();
                } else {
                    result = (t_size > other_size) ? TermGreaterThan : TermLessThan;
                    break;
                }
            } else {
                result = (memcmp_result > 0) ? TermGreaterThan : TermLessThan;
                break;
            }

        } else if (term_is_map(t) && term_is_map(other)) {
            int t_size = term_get_map_size(t);
            int other_size = term_get_map_size(other);

            if (t_size != other_size) {
                result = (t_size > other_size) ? TermGreaterThan : TermLessThan;
                break;
            }
            if (t_size > 0) {
                for (int i = t_size - 1; i >= 1; i--) {
                    if (UNLIKELY(temp_stack_push(&temp_stack, term_get_map_value(t, i))
                            != TempStackOk)) {
                        return TermCompareMemoryAllocFail;
                    }
                    if (UNLIKELY(temp_stack_push(&temp_stack, term_get_map_value(other, i))
                            != TempStackOk)) {
                        return TermCompareMemoryAllocFail;
                    }
                    if (UNLIKELY(
                            temp_stack_push(&temp_stack, END_MAP_KEY) != TempStackOk)) {
                        return TermCompareMemoryAllocFail;
                    }
                    if (UNLIKELY(
                            temp_stack_push(&temp_stack, term_get_map_key(t, i)) != TempStackOk)) {
                        return TermCompareMemoryAllocFail;
                    }
                    if (UNLIKELY(temp_stack_push(&temp_stack, term_get_map_key(other, i))
                            != TempStackOk)) {
                        return TermCompareMemoryAllocFail;
                    }
                    if (UNLIKELY(
                            temp_stack_push(&temp_stack, BEGIN_MAP_KEY) != TempStackOk)) {
                        return TermCompareMemoryAllocFail;
                    }
                }
                if (UNLIKELY(temp_stack_push(&temp_stack, term_get_map_value(t, 0)) != TempStackOk)) {
                    return TermCompareMemoryAllocFail;
                }
                if (UNLIKELY(temp_stack_push(&temp_stack, term_get_map_value(other, 0)) != TempStackOk)) {
                    return TermCompareMemoryAllocFail;
                }
                map_key_nesting++;
                if (UNLIKELY(temp_stack_push(&temp_stack, END_MAP_KEY) != TempStackOk)) {
                    return TermCompareMemoryAllocFail;
                }
                t = term_get_map_key(t, 0);
                other = term_get_map_key(other, 0);

            } else {
                CMP_POP_AND_CONTINUE();
            }

        } else if (term_is_any_integer(t) && term_is_any_integer(other)) {
            avm_int64_t t_int = term_maybe_unbox_int64(t);
            avm_int64_t other_int = term_maybe_unbox_int64(other);
            if (t_int == other_int) {
                CMP_POP_AND_CONTINUE();
            } else {
                result = (t_int > other_int) ? TermGreaterThan : TermLessThan;
                break;
            }

        } else if (term_is_float(t) && term_is_float(other)) {
            avm_float_t t_float = term_to_float(t);
            avm_float_t other_float = term_to_float(other);
            if (t_float == other_float) {
                CMP_POP_AND_CONTINUE();
            } else {
                result = (t_float > other_float) ? TermGreaterThan : TermLessThan;
                break;
            }

        } else if (term_is_number(t) && term_is_number(other)
            && ((opts & TermCompareExact) != TermCompareExact) && (map_key_nesting == 0)) {
            avm_float_t t_float = term_conv_to_float(t);
            avm_float_t other_float = term_conv_to_float(other);
            if (t_float == other_float) {
                CMP_POP_AND_CONTINUE();
            } else {
                result = (t_float > other_float) ? TermGreaterThan : TermLessThan;
                break;
            }

        } else if (term_is_atom(t) && term_is_atom(other)) {
            int t_atom_index = term_to_atom_index(t);
            int other_atom_index = term_to_atom_index(other);

            // it cannot be equal since we check for term equality as first thing
            // so let's ignore 0
            int atom_cmp_result = atom_table_cmp_using_atom_index(
                global->atom_table, t_atom_index, other_atom_index);
            result = (atom_cmp_result > 0) ? TermGreaterThan : TermLessThan;
            break;

        } else if (term_is_pid(t) && term_is_pid(other)) {
            //TODO: handle ports
            result = (t > other) ? TermGreaterThan : TermLessThan;
            break;

        } else {
            result = (term_type_to_index(t) > term_type_to_index(other)) ? TermGreaterThan : TermLessThan;
            break;
        }
    }

    temp_stack_destroy(&temp_stack);

    return result;
}

term term_alloc_refc_binary(size_t size, bool is_const, Heap *heap, GlobalContext *glb)
{
    term *boxed_value = memory_heap_alloc(heap, TERM_BOXED_REFC_BINARY_SIZE);
    boxed_value[0] = ((TERM_BOXED_REFC_BINARY_SIZE - 1) << 6) | TERM_BOXED_REFC_BINARY;
    boxed_value[1] = (term) size;
    boxed_value[2] = (term) is_const ? RefcBinaryIsConst : RefcNoFlags;
    term ret = ((term) boxed_value) | TERM_BOXED_VALUE_TAG;
    if (is_const) {
        boxed_value[3] = (term) NULL;
        // TODO Consider making const refc binaries 4 words instead of 6
        boxed_value[4] = term_nil(); // mso_list is not used
        boxed_value[5] = term_nil(); // for const binaries
    } else {
        struct RefcBinary *refc = refc_binary_create_refc(size);
        if (IS_NULL_PTR(refc)) {
            // TODO propagate error to callers of this function, e.g., as an invalid term
            fprintf(stderr, "memory_create_refc_binary: Unable to allocate %zu bytes for refc_binary.\n", size);
            AVM_ABORT();
        }
        boxed_value[3] = (term) refc;
        heap->root->mso_list = term_list_init_prepend(boxed_value + 4, ret, heap->root->mso_list);
        synclist_append(&glb->refc_binaries, &refc->head);
    }
    return ret;
}

static term find_binary(term binary_or_state)
{
    term t = binary_or_state;
    while (term_is_match_state(t) || term_is_sub_binary(t)) {
        if (term_is_match_state(t)) {
            t = term_get_match_state_binary(t);
        } else { // term_is_sub_binary
            t = term_get_sub_binary_ref(t);
        }
    }
    return t;
}

term term_alloc_sub_binary(term binary_or_state, size_t offset, size_t len, Heap *heap)
{
    term *boxed = memory_heap_alloc(heap, TERM_BOXED_SUB_BINARY_SIZE);
    term binary = find_binary(binary_or_state);

    boxed[0] = ((TERM_BOXED_SUB_BINARY_SIZE - 1) << 6) | TERM_BOXED_SUB_BINARY;
    boxed[1] = (term) len;
    boxed[2] = (term) offset;
    boxed[3] = binary;

    return ((term) boxed) | TERM_BOXED_VALUE_TAG;
}

term term_get_map_assoc(term map, term key, GlobalContext *glb)
{
    int pos = term_find_map_pos(map, key, glb);
    if (pos == TERM_MAP_NOT_FOUND) {
        return term_invalid_term();
    } else if (UNLIKELY(pos == TERM_MAP_MEMORY_ALLOC_FAIL)) {
        // TODO: do not AVM_ABORT, return out of memory error
        AVM_ABORT();
    }
    return term_get_map_value(map, pos);
}
