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
#include "defaultatoms.h"
#include "interop.h"
#include "intn.h"
#include "module.h"
#include "tempstack.h"
#include "utils.h"

#include <ctype.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

enum TermTypeIndex {
    TERM_TYPE_INDEX_INVALID = 0,
    TERM_TYPE_INDEX_INTEGER = 1,
    TERM_TYPE_INDEX_FLOAT = 2,
    TERM_TYPE_INDEX_ATOM = 3,
    TERM_TYPE_INDEX_REFERENCE = 4,
    TERM_TYPE_INDEX_FUNCTION = 5,
    TERM_TYPE_INDEX_PORT = 6,
    TERM_TYPE_INDEX_PID = 7,
    TERM_TYPE_INDEX_TUPLE = 8,
    TERM_TYPE_INDEX_NIL = 9,
    TERM_TYPE_INDEX_NON_EMPTY_LIST = 10,
    TERM_TYPE_INDEX_BINARY = 11,
    TERM_TYPE_INDEX_MAP = 12,
};

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
        atom_index_t atom_index = term_to_atom_index(t);
        size_t atom_len;
        const uint8_t *atom_data = atom_table_get_atom_string(global->atom_table, atom_index, &atom_len);
        return fun->print(fun, "%.*s", (int) atom_len, atom_data);

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
    } else if (term_is_local_pid(t)) {
        int32_t process_id = term_to_local_process_id(t);
        return fun->print(fun, "<0.%" PRIu32 ".0>", process_id);

    } else if (term_is_external_pid(t)) {
        uint32_t node_atom_index = term_to_atom_index(term_get_external_node(t));
        uint32_t number = term_get_external_pid_process_id(t);
        uint32_t serial = term_get_external_pid_serial(t);
        // creation is not printed
        return fun->print(fun, "<%" PRIu32 ".%" PRIu32 ".%" PRIu32 ">", node_atom_index, number, serial);

    } else if (term_is_local_port(t)) {
        // Update also PORT_AS_CSTRING_LEN when changing this format string
        int32_t process_id = term_to_local_process_id(t);
        return fun->print(fun, "#Port<0.%" PRIu32 ">", process_id);

    } else if (term_is_external_port(t)) {
        // Update also PORT_AS_CSTRING_LEN when changing this format string
        uint32_t node_atom_index = term_to_atom_index(term_get_external_node(t));
        uint64_t number = term_get_external_port_number(t);
        // creation is not printed
        return fun->print(fun, "#Port<%" PRIu32 ".%" PRIu64 ">", node_atom_index, number);

    } else if (term_is_function(t)) {
        const term *boxed_value = term_to_const_term_ptr(t);

        if (term_is_external_fun(t)) {
            term module_atom = boxed_value[1];
            term function_atom = boxed_value[2];
            int arity = term_to_int(boxed_value[3]);

            atom_index_t module_atom_index = term_to_atom_index(module_atom);
            size_t module_name_len;
            const uint8_t *module_name = atom_table_get_atom_string(global->atom_table, module_atom_index, &module_name_len);

            atom_index_t function_atom_index = term_to_atom_index(function_atom);
            size_t function_name_len;
            const uint8_t *function_name = atom_table_get_atom_string(global->atom_table, function_atom_index, &function_name_len);

            // fun m:f/a
            int ret = fun->print(fun, "fun %.*s:%.*s/%i", (int) module_name_len, module_name,
                (int) function_name_len, function_name, arity);
            return ret;

        } else {
            Module *fun_module = (Module *) boxed_value[1];

            term module_name_atom = module_get_name(fun_module);
            atom_index_t module_atom_index = term_to_atom_index(module_name_atom);
            size_t module_name_len;
            const uint8_t *module_name = atom_table_get_atom_string(global->atom_table, module_atom_index, &module_name_len);
            uint32_t fun_index = term_to_int32(boxed_value[2]);
            uint32_t arity, old_index, old_uniq;
            module_get_fun_arity_old_index_uniq(fun_module, fun_index, &arity, &old_index, &old_uniq);
            int ret = fun->print(fun, "#Fun<%.*s.%" PRIu32 ".%" PRIu32 ">", (int) module_name_len,
                module_name, old_index, old_uniq);
            return ret;
        }

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

    } else if (term_is_local_reference(t)) {
        uint64_t ref_ticks = term_to_ref_ticks(t);

        // Update also REF_AS_CSTRING_LEN when changing this format string
        return fun->print(fun, "#Ref<0.%" PRIu32 ".%" PRIu32 ">", (uint32_t) (ref_ticks >> 32), (uint32_t) ref_ticks);

    } else if (term_is_external_reference(t)) {
        // Update also REF_AS_CSTRING_LEN when changing this format string
        uint32_t node_atom_index = term_to_atom_index(term_get_external_node(t));
        uint32_t len = term_get_external_reference_len(t);
        const uint32_t *data = term_get_external_reference_words(t);
        // creation is not printed
        int ret = fun->print(fun, "#Ref<%" PRIu32, node_atom_index);
        for (int i = len - 1; i >= 0; i--) {
            ret += fun->print(fun, ".%" PRIu32, data[i]);
        }
        ret += fun->print(fun, ">");
        return ret;

    } else if (term_is_boxed_integer(t)) {
        int size = term_boxed_size(t);
        switch (size) {
            case 1:
                return fun->print(fun, AVM_INT_FMT, term_unbox_int(t));

#if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2:
                return fun->print(fun, AVM_INT64_FMT, term_unbox_int64(t));
#endif
            default: {
                const intn_digit_t *intn_data;
                size_t intn_data_len;
                intn_integer_sign_t sign;
                term_to_bigint(t, &intn_data, &intn_data_len, &sign);
                size_t unused_s_len;
                char *s = intn_to_string(intn_data, intn_data_len, sign, 10, &unused_s_len);
                if (IS_NULL_PTR(s)) {
                    return -1;
                }
                int print_res = fun->print(fun, "%s", s);
                free(s);
                return print_res;
            }
        }

    } else if (term_is_float(t)) {
        avm_float_t f = term_to_float(t);
        return fun->print(fun, AVM_FLOAT_FMT, f);

    } else {
        return fun->print(fun, "Unknown term type: %" TERM_U_FMT, t);
    }
}

static enum TermTypeIndex term_type_to_index(term t)
{
    switch (t & TERM_PRIMARY_MASK) {
        case TERM_PRIMARY_CP:
            // invalid term
            return TERM_TYPE_INDEX_INVALID;
        case TERM_PRIMARY_LIST:
            return TERM_TYPE_INDEX_NON_EMPTY_LIST;
        case TERM_PRIMARY_BOXED: {
            const term *boxed_value = term_to_const_term_ptr(t);

            switch (boxed_value[0] & TERM_BOXED_TAG_MASK) {
                case TERM_BOXED_TUPLE:
                    return TERM_TYPE_INDEX_TUPLE;
                case TERM_BOXED_POSITIVE_INTEGER:
                    return TERM_TYPE_INDEX_INTEGER;
                case TERM_BOXED_NEGATIVE_INTEGER:
                    return TERM_TYPE_INDEX_INTEGER;
                case TERM_BOXED_REF:
                    return TERM_TYPE_INDEX_REFERENCE;
                case TERM_BOXED_FUN:
                    return TERM_TYPE_INDEX_FUNCTION;
                case TERM_BOXED_FLOAT:
                    return TERM_TYPE_INDEX_FLOAT;
                case TERM_BOXED_REFC_BINARY:
                case TERM_BOXED_HEAP_BINARY:
                case TERM_BOXED_SUB_BINARY:
                    return TERM_TYPE_INDEX_BINARY;
                case TERM_BOXED_MAP:
                    return TERM_TYPE_INDEX_MAP;
                case TERM_BOXED_EXTERNAL_PID:
                    return TERM_TYPE_INDEX_PID;
                case TERM_BOXED_EXTERNAL_PORT:
                    return TERM_TYPE_INDEX_PORT;
                case TERM_BOXED_EXTERNAL_REF:
                    return TERM_TYPE_INDEX_REFERENCE;
                default:
                    UNREACHABLE();
            }
        }
        break;
        case TERM_PRIMARY_IMMED:
            switch (t & TERM_IMMED_TAG_MASK) {
                case TERM_PID_TAG:
                    return TERM_TYPE_INDEX_PID;
                case TERM_PORT_TAG:
                    return TERM_TYPE_INDEX_PORT;
                case TERM_IMMED2_TAG: {
                    switch (t & TERM_IMMED2_TAG_MASK) {
                        case TERM_IMMED2_ATOM:
                            return TERM_TYPE_INDEX_ATOM;
                        case TERM_NIL:
                            return TERM_TYPE_INDEX_NIL;
                        default:
                            UNREACHABLE();
                    }
                }
                case TERM_INTEGER_TAG:
                    return TERM_TYPE_INDEX_INTEGER;
                default:
                    UNREACHABLE();
            }
        default:
            UNREACHABLE();
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

        } else {
            enum TermTypeIndex type_t = term_type_to_index(t);
            enum TermTypeIndex type_other = term_type_to_index(other);
            if (type_t == type_other) {
                switch (type_t) {
                    case TERM_TYPE_INDEX_INTEGER: {
                        if (term_is_integer(t) && term_is_integer(other)) {
                            avm_int_t t_int = term_to_int(t);
                            avm_int_t other_int = term_to_int(other);
                            // They cannot be equal
                            result = (t_int > other_int) ? TermGreaterThan : TermLessThan;
                            goto unequal;
                        }

                        term_integer_sign_t t_sign;
                        size_t t_size;
                        if (term_is_boxed(t)) {
                            t_sign = term_boxed_integer_sign(t);
                            t_size = term_boxed_size(t);
                        } else {
                            t_sign = term_integer_sign_from_int(term_to_int(t));
                            t_size = 0;
                        }
                        term_integer_sign_t other_sign;
                        size_t other_size;
                        if (term_is_boxed(other)) {
                            other_sign = term_boxed_integer_sign(other);
                            other_size = term_boxed_size(other);
                        } else {
                            other_sign = term_integer_sign_from_int(term_to_int(other));
                            other_size = 0;
                        }

                        _Static_assert(
                            TermPositiveInteger < TermNegativeInteger, "Unexpected sign definition in term.h");
                        if (t_sign < other_sign) {
                            result = TermGreaterThan;
                            goto unequal;
                        } else if (t_sign > other_sign) {
                            result = TermLessThan;
                            goto unequal;
                        }

                        TermCompareResult more_digits_result;
                        TermCompareResult less_digits_result;
                        if (t_sign == TermPositiveInteger) {
                            more_digits_result = TermGreaterThan;
                            less_digits_result = TermLessThan;
                        } else {
                            more_digits_result = TermLessThan;
                            less_digits_result = TermGreaterThan;
                        }

                        if (t_size == other_size) {
                            const term *t_ptr = term_to_const_term_ptr(t);
                            const term *other_ptr = term_to_const_term_ptr(other);
                            if (t_size == 1) {
                                if (t_ptr[1] != other_ptr[1]) {
                                    result = (t_ptr[1] > other_ptr[1]) ? TermGreaterThan : TermLessThan;
                                    goto unequal;
                                }
#if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                            } else if (t_size == 2) {
                                avm_int64_t t64 = term_unbox_int64(t);
                                avm_int64_t other64 = term_unbox_int64(other);
                                if (t64 != other64) {
                                    result = (t64 > other64) ? TermGreaterThan : TermLessThan;
                                    goto unequal;
                                }
#endif
                            } else {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
                                // on 64-bit big endian systems, term size is 64 bit, so a term
                                // contains 2 intn_digit_t
                                // however inside a big integer digits are in "little endian" order
                                // so comparison cannot be directly done in 64-bit chunks
                                intn_digit_t *t_digits = (intn_digit_t *) t_ptr;
                                intn_digit_t *other_digits = (intn_digit_t *) other_ptr;
                                size_t digits_per_term = (sizeof(term) / sizeof(intn_digit_t));
                                size_t digit_count = (1 + t_size) * digits_per_term;
                                // t_digits[0] ... t_digits[digits_per_term - 1] is the boxed header
                                for (size_t i = digit_count - 1; i >= digits_per_term; i--) {
                                    if (t_digits[i] != other_digits[i]) {
                                        result = (t_digits[i] > other_digits[i]) ? more_digits_result
                                                                                 : less_digits_result;
                                        goto unequal;
                                    }
                                }
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
                                for (size_t i = t_size; i >= 1; i--) {
                                    if (t_ptr[i] != other_ptr[i]) {
                                        result = (t_ptr[i] > other_ptr[i]) ? more_digits_result
                                                                           : less_digits_result;
                                        goto unequal;
                                    }
                                }
#else
#error "Unsupported endianness"
#endif
                            }
                            CMP_POP_AND_CONTINUE();
                            break;

                        } else if (t_size > other_size) {
                            result = more_digits_result;
                            goto unequal;
                        } else {
                            result = less_digits_result;
                            goto unequal;
                        }
                    }

                    case TERM_TYPE_INDEX_REFERENCE: {
                        if (!term_is_external(t) && !term_is_external(other)) {
                            int64_t t_ticks = term_to_ref_ticks(t);
                            int64_t other_ticks = term_to_ref_ticks(other);
                            if (t_ticks == other_ticks) {
                                CMP_POP_AND_CONTINUE();
                                break;
                            } else {
                                result = (t_ticks > other_ticks) ? TermGreaterThan : TermLessThan;
                                goto unequal;
                            }
                        } else {
                            term node = term_is_external(t) ? term_get_external_node(t) : NONODE_AT_NOHOST_ATOM;
                            term other_node = term_is_external(other) ? term_get_external_node(other) : NONODE_AT_NOHOST_ATOM;
                            if (node == other_node) {
                                uint32_t creation = term_is_external(t) ? term_get_external_node_creation(t) : 0;
                                uint32_t other_creation = term_is_external(other) ? term_get_external_node_creation(other) : 0;
                                if (creation == other_creation) {
                                    uint32_t len = term_is_external(t) ? term_get_external_reference_len(t) : 2;
                                    uint32_t other_len = term_is_external(other) ? term_get_external_reference_len(other) : 2;
                                    if (len == other_len) {
                                        const uint32_t *data;
                                        const uint32_t *other_data;
                                        uint32_t local_data[2];
                                        if (term_is_external(t)) {
                                            data = term_get_external_reference_words(t);
                                        } else {
                                            uint64_t ref_ticks = term_to_ref_ticks(t);
                                            local_data[0] = ref_ticks >> 32;
                                            local_data[1] = (uint32_t) ref_ticks;
                                            data = local_data;
                                        }
                                        if (term_is_external(other)) {
                                            other_data = term_get_external_reference_words(other);
                                        } else {
                                            uint64_t ref_ticks = term_to_ref_ticks(other);
                                            local_data[0] = ref_ticks >> 32;
                                            local_data[1] = (uint32_t) ref_ticks;
                                            other_data = local_data;
                                        }
                                        // Comparison is done in reverse order
                                        for (int i = len - 1; i >= 0; i--) {
                                            if (data[i] != other_data[i]) {
                                                result = (data[i] > other_data[i]) ? TermGreaterThan : TermLessThan;
                                                goto unequal;
                                            }
                                        }
                                        if (result != TermEquals) {
                                            goto unequal;
                                        }
                                        CMP_POP_AND_CONTINUE();
                                        break;
                                    } else {
                                        result = (len > other_len) ? TermGreaterThan : TermLessThan;
                                        goto unequal;
                                    }
                                } else {
                                    result = (creation > other_creation) ? TermGreaterThan : TermLessThan;
                                    goto unequal;
                                }
                            } else {
                                t = node;
                                other = other_node;
                                break;
                            }
                        }
                    }
                    case TERM_TYPE_INDEX_NON_EMPTY_LIST: {
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
                        break;
                    }
                    case TERM_TYPE_INDEX_TUPLE: {
                        int tuple_size = term_get_tuple_arity(t);
                        int other_tuple_size = term_get_tuple_arity(other);

                        if (tuple_size != other_tuple_size) {
                            result = (tuple_size > other_tuple_size) ? TermGreaterThan : TermLessThan;
                            goto unequal;
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
                        break;
                    }
                    case TERM_TYPE_INDEX_BINARY: {
                        int t_size = term_binary_size(t);
                        int other_size = term_binary_size(other);

                        const char *t_data = term_binary_data(t);
                        const char *other_data = term_binary_data(other);

                        int cmp_size = (t_size > other_size) ? other_size : t_size;

                        int memcmp_result = memcmp(t_data, other_data, cmp_size);
                        if (memcmp_result == 0) {
                            if (t_size == other_size) {
                                CMP_POP_AND_CONTINUE();
                                break;
                            } else {
                                result = (t_size > other_size) ? TermGreaterThan : TermLessThan;
                                goto unequal;
                            }
                        } else {
                            result = (memcmp_result > 0) ? TermGreaterThan : TermLessThan;
                            goto unequal;
                        }
                    }
                    case TERM_TYPE_INDEX_MAP: {
                        int t_size = term_get_map_size(t);
                        int other_size = term_get_map_size(other);

                        if (t_size != other_size) {
                            result = (t_size > other_size) ? TermGreaterThan : TermLessThan;
                            goto unequal;
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
                            break;
                        }
                    }
                    break;
                    case TERM_TYPE_INDEX_FLOAT: {
                        avm_float_t t_float = term_to_float(t);
                        avm_float_t other_float = term_to_float(other);
                        if (t_float == other_float) {
                            CMP_POP_AND_CONTINUE();
                            break;
                        } else {
                            result = (t_float > other_float) ? TermGreaterThan : TermLessThan;
                            goto unequal;
                        }
                    }
                    break;
                    case TERM_TYPE_INDEX_ATOM: {
                        int t_atom_index = term_to_atom_index(t);
                        int other_atom_index = term_to_atom_index(other);

                        // it cannot be equal since we check for term equality as first thing
                        // so let's ignore 0
                        int atom_cmp_result = atom_table_cmp_using_atom_index(
                            global->atom_table, t_atom_index, other_atom_index);
                        result = (atom_cmp_result > 0) ? TermGreaterThan : TermLessThan;
                        goto unequal;
                    }
                    case TERM_TYPE_INDEX_PID: {
                        uint32_t process_id = term_is_external(t) ? term_get_external_pid_process_id(t) : (uint32_t) term_to_local_process_id(t);
                        uint32_t other_process_id = term_is_external(other) ? term_get_external_pid_process_id(other) : (uint32_t) term_to_local_process_id(other);
                        if (process_id == other_process_id) {
                            uint32_t serial = term_is_external(t) ? term_get_external_pid_serial(t) : 0;
                            uint32_t other_serial = term_is_external(other) ? term_get_external_pid_serial(other) : 0;
                            if (serial == other_serial) {
                                term node = term_is_external(t) ? term_get_external_node(t) : NONODE_AT_NOHOST_ATOM;
                                term other_node = term_is_external(other) ? term_get_external_node(other) : NONODE_AT_NOHOST_ATOM;
                                if (node == other_node) {
                                    uint32_t creation = term_is_external(t) ? term_get_external_node_creation(t) : 0;
                                    uint32_t other_creation = term_is_external(other) ? term_get_external_node_creation(other) : 0;
                                    if (creation == other_creation) {
                                        CMP_POP_AND_CONTINUE();
                                        break;
                                    } else {
                                        result = (creation > other_creation) ? TermGreaterThan : TermLessThan;
                                        goto unequal;
                                    }
                                } else {
                                    t = node;
                                    other = other_node;
                                    break;
                                }
                            } else {
                                result = (serial > other_serial) ? TermGreaterThan : TermLessThan;
                                goto unequal;
                            }
                        } else {
                            result = (process_id > other_process_id) ? TermGreaterThan : TermLessThan;
                            goto unequal;
                        }
                    }
                    case TERM_TYPE_INDEX_PORT: {
                        term node = term_is_external(t) ? term_get_external_node(t) : NONODE_AT_NOHOST_ATOM;
                        term other_node = term_is_external(other) ? term_get_external_node(other) : NONODE_AT_NOHOST_ATOM;
                        if (node == other_node) {
                            uint32_t creation = term_is_external(t) ? term_get_external_node_creation(t) : 0;
                            uint32_t other_creation = term_is_external(other) ? term_get_external_node_creation(other) : 0;
                            if (creation == other_creation) {
                                uint64_t port_number = term_is_external(t) ? term_get_external_port_number(t) : (uint64_t) term_to_local_process_id(t);
                                uint64_t other_port_number = term_is_external(other) ? term_get_external_port_number(other) : (uint64_t) term_to_local_process_id(other);
                                if (port_number == other_port_number) {
                                    CMP_POP_AND_CONTINUE();
                                    break;
                                } else {
                                    result = (port_number > other_port_number) ? TermGreaterThan : TermLessThan;
                                    goto unequal;
                                }
                            } else {
                                result = (creation > other_creation) ? TermGreaterThan : TermLessThan;
                                goto unequal;
                            }
                        } else {
                            t = node;
                            other = other_node;
                            break;
                        }
                    }
                    default:
                        UNREACHABLE();
                }
            } else if ((((type_t == TERM_TYPE_INDEX_FLOAT && type_other == TERM_TYPE_INDEX_INTEGER)
                || (type_t == TERM_TYPE_INDEX_INTEGER && type_other == TERM_TYPE_INDEX_FLOAT)))
                && ((opts & TermCompareExact) != TermCompareExact) && (map_key_nesting == 0)) {
                avm_float_t t_float = term_conv_to_float(t);
                avm_float_t other_float = term_conv_to_float(other);
                if (t_float == other_float) {
                    CMP_POP_AND_CONTINUE();
                } else {
                    result = (t_float > other_float) ? TermGreaterThan : TermLessThan;
                    break;
                }
            } else {
                result = (type_t > type_other) ? TermGreaterThan : TermLessThan;
                break;
            }
        }
    }

unequal:
    temp_stack_destroy(&temp_stack);

    return result;
}

void term_get_function_mfa(term fun, term *m, term *f, term *a)
{
    TERM_DEBUG_ASSERT(term_is_fun(fun));

    const term *boxed_value = term_to_const_term_ptr(fun);
    if (term_is_external_fun(fun)) {
        if (m != NULL) {
            *m = boxed_value[1];
        }
        if (f != NULL) {
            *f = boxed_value[2];
        }
        if (a != NULL) {
            *a = boxed_value[3];
        }
        return;
    }

    Module *module = (Module *) boxed_value[1];
    if (m != NULL) {
        *m = module_get_name(module);
    }
    if (f != NULL) {
        uint32_t fun_index = term_to_int32(boxed_value[2]);

        uint32_t label, arity, n_freeze;
        module_get_fun(module, fun_index, &label, &arity, &n_freeze);

        atom_index_t fun_name;
        bool has_local_name = module_get_function_from_label(module, label, &fun_name, (int *) &arity);

        *f = has_local_name ? term_from_atom_index(fun_name) : term_nil();
    }
    if (a != NULL) {
        uint32_t fun_index = term_to_int32(boxed_value[2]);

        uint32_t label, arity, n_freeze;
        module_get_fun(module, fun_index, &label, &arity, &n_freeze);
        TERM_DEBUG_ASSERT(arity <= 255);

        *a = term_from_int11((int16_t) arity);
    }
}

term term_alloc_refc_binary(size_t size, bool is_const, Heap *heap, GlobalContext *glb)
{
    term *boxed_value = memory_heap_alloc(heap, TERM_BOXED_REFC_BINARY_SIZE);
    boxed_value[0] = ((TERM_BOXED_REFC_BINARY_SIZE - 1) << 6) | TERM_BOXED_REFC_BINARY;
    boxed_value[1] = (term) size;
    boxed_value[2] = (term) is_const ? RefcBinaryIsConst : RefcNoFlags;
    term ret = ((term) boxed_value) | TERM_PRIMARY_BOXED;
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
        refc->ref_count = 1; // added to mso list, increment ref count
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

    return ((term) boxed) | TERM_PRIMARY_BOXED;
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

avm_float_t term_conv_to_float(term t)
{
    if (term_is_float(t)) {
        return term_to_float(t);
    } else if (term_is_integer(t)) {
        return term_to_int(t);
    } else if (term_is_boxed_integer(t)) {
        size_t boxed_size = term_boxed_size(t);
        switch (boxed_size) {
            case 0:
                UNREACHABLE();
            case 1:
                return term_unbox_int(t);
#if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2:
                return term_unbox_int64(t);
#endif
            default: {
                const intn_digit_t *num;
                size_t num_len;
                intn_integer_sign_t num_sign;
                term_to_bigint(t, &num, &num_len, &num_sign);

                return intn_to_double(num, num_len, num_sign);
            }
        }
    } else {
        UNREACHABLE();
    }
}
