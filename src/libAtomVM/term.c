/***************************************************************************
 *   Copyright 2018,2019 by Davide Bettio <davide@uninstall.it>            *
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

#include "term.h"

#include "atom.h"
#include "context.h"
#include "interop.h"
#include "valueshashtable.h"
#include "tempstack.h"

#include <ctype.h>
#include <stdio.h>
#include <inttypes.h>

const term empty_tuple = 0;

void term_display(FILE *fd, term t, const Context *ctx)
{
    if (term_is_atom(t)) {
        int atom_index = term_to_atom_index(t);
            AtomString atom_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table, atom_index, (unsigned long) NULL);
            fprintf(fd, "%.*s", (int) atom_string_len(atom_string), (char *) atom_string_data(atom_string));

    } else if (term_is_integer(t)) {
        avm_int_t iv = term_to_int(t);
        fprintf(fd, AVM_INT_FMT, iv);

    } else if (term_is_nil(t)) {
        fprintf(fd, "[]");

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
                fprintf(fd, "\"%s\"", printable);
                free(printable);
            } else {
                fprintf(fd, "???");
            }

        } else {
            fputc('[', fd);
            int display_separator = 0;
            while (term_is_nonempty_list(t)) {
                if (display_separator) {
                    fputc(',', fd);
                } else {
                    display_separator = 1;
                }

                term_display(fd, term_get_list_head(t), ctx);
                t = term_get_list_tail(t);
            }
            if (!term_is_nil(t)) {
                fputc('|', fd);
                term_display(fd, t, ctx);
            }
            fputc(']', fd);
        }
    } else if (term_is_pid(t)) {
        fprintf(fd, "<0.%i.0>", term_to_local_process_id(t));

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
        fprintf(fd, format, fun_index, (unsigned long) fun_module);

    } else if (term_is_tuple(t)) {
        fputc('{', fd);

        int tuple_size = term_get_tuple_arity(t);
        for (int i = 0; i < tuple_size; i++) {
            if (i != 0) {
                fputc(',', fd);
            }
            term_display(fd, term_get_tuple_element(t, i), ctx);
        }

        fputc('}', fd);

    } else if (term_is_binary(t)) {
        int len = term_binary_size(t);
        const char *binary_data = term_binary_data(t);

        int is_printable = 1;
        for (int i = 0; i < len; i++) {
            if (!isprint(binary_data[i])) {
                is_printable = 0;
                break;
            }
        }

        fprintf(fd, "<<");
        if (is_printable) {
            fprintf(fd, "\"%.*s\"", len, binary_data);

        } else {
            int display_separator = 0;
            for (int i = 0; i < len; i++) {
                if (display_separator) {
                    fputc(',', fd);
                } else {
                    display_separator = 1;
                }

                uint8_t c = (uint8_t) binary_data[i];
                fprintf(fd, "%i", (int) c);
            }
        }
        fprintf(fd, ">>");

    } else if (term_is_reference(t)) {
        const char *format = "#Ref<0.0.0." AVM_UINT64_FMT ">";
        fprintf(fd, format, term_to_ref_ticks(t));

    } else if (term_is_boxed_integer(t)) {
        int size = term_boxed_size(t);
        switch (size) {
            case 1:
                fprintf(fd, AVM_INT_FMT, term_unbox_int(t));
                break;

#if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2:
                fprintf(fd, AVM_INT64_FMT, term_unbox_int64(t));
                break;
#endif

            default:
                abort();
        }

#ifndef AVM_NO_FP
    } else if (term_is_float(t)) {
        avm_float_t f = term_to_float(t);
        fprintf(fd, AVM_FLOAT_FMT, f);
#endif

    } else {
        fprintf(fd, "Unknown term type: 0x%lx", (unsigned long) t);
    }
}

static int term_type_to_index(term t)
{
    if (term_is_invalid_term(t)) {
        return 0;

    } else if (term_is_number(t)) {
        return 1;

    } else if (term_is_atom(t)) {
        return 2;

    } else if (term_is_reference(t)) {
        return 3;

    } else if (term_is_function(t)) {
        return 4;

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

    } else {
        abort();
    }
}

int term_compare(term t, term other, Context *ctx)
{
    struct TempStack temp_stack;
    temp_stack_init(&temp_stack);

    temp_stack_push(&temp_stack, t);
    temp_stack_push(&temp_stack, other);

    int result = 0;

    while (!temp_stack_is_empty(&temp_stack)) {
        if (t == other) {
            other = temp_stack_pop(&temp_stack);
            t = temp_stack_pop(&temp_stack);

        } else if (term_is_integer(t) && term_is_integer(other)) {
            avm_int_t t_int = term_to_int(t);
            avm_int_t other_int = term_to_int(other);
            //They cannot be equal
            result = (t_int > other_int) ? 1 : -1;
            break;

        } else if (term_is_reference(t) && term_is_reference(other)) {
            int64_t t_ticks = term_to_ref_ticks(t);
            int64_t other_ticks = term_to_ref_ticks(other);
            if (t_ticks == other_ticks) {
                other = temp_stack_pop(&temp_stack);
                t = temp_stack_pop(&temp_stack);
            } else {
                result = (t_ticks > other_ticks) ? 1 : -1;
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
            temp_stack_push(&temp_stack, t_tail);
            temp_stack_push(&temp_stack, other_tail);
            t = term_get_list_head(t);
            other = term_get_list_head(other);

        } else if (term_is_tuple(t) && term_is_tuple(other)) {
            int tuple_size = term_get_tuple_arity(t);
            int other_tuple_size = term_get_tuple_arity(other);

            if (tuple_size != other_tuple_size) {
                result = (tuple_size > other_tuple_size) ? 1 : -1;
                break;
            }

            if (tuple_size > 0) {
                for (int i = 1; i < tuple_size; i++) {
                    temp_stack_push(&temp_stack, term_get_tuple_element(t, i));
                    temp_stack_push(&temp_stack, term_get_tuple_element(other, i));
                }
                t = term_get_tuple_element(t, 0);
                other = term_get_tuple_element(other, 0);

            } else {
                other = temp_stack_pop(&temp_stack);
                t = temp_stack_pop(&temp_stack);
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
                    other = temp_stack_pop(&temp_stack);
                    t = temp_stack_pop(&temp_stack);
                } else {
                    result = (t_size > other_size) ? 1 : -1;
                    break;
                }
            } else {
                result = (memcmp_result > 0) ? 1 : -1;
                break;
            }

        } else if (term_is_any_integer(t) && term_is_any_integer(other)) {
            avm_int64_t t_int = term_maybe_unbox_int64(t);
            avm_int64_t other_int = term_maybe_unbox_int64(other);
            if (t_int == other_int) {
                other = temp_stack_pop(&temp_stack);
                t = temp_stack_pop(&temp_stack);
            } else {
                result = (t_int > other_int) ? 1 : -1;
                break;
            }

#ifndef AVM_NO_FP
        } else if (term_is_number(t) && term_is_number(other)) {
            avm_float_t t_float = term_conv_to_float(t);
            avm_float_t other_float = term_conv_to_float(other);
            if (t_float == other_float) {
                other = temp_stack_pop(&temp_stack);
                t = temp_stack_pop(&temp_stack);
            } else {
                result = (t_float > other_float) ? 1 : -1;
                break;
            }
#endif

        } else if (term_is_atom(t) && term_is_atom(other)) {
            int t_atom_index = term_to_atom_index(t);
            AtomString t_atom_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table,
                    t_atom_index, (unsigned long) NULL);

            int t_atom_len = atom_string_len(t_atom_string);
            const char *t_atom_data = (const char *) atom_string_data(t_atom_string);

            int other_atom_index = term_to_atom_index(other);
            AtomString other_atom_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table,
                    other_atom_index, (unsigned long) NULL);

            int other_atom_len = atom_string_len(other_atom_string);
            const char *other_atom_data = (const char *) atom_string_data(other_atom_string);

            int cmp_size = (t_atom_len > other_atom_len) ? other_atom_len : t_atom_len;

            int memcmp_result = memcmp(t_atom_data, other_atom_data, cmp_size);
            if (memcmp_result == 0) {
                result = (t_atom_len > other_atom_len) ? 1 : -1;
                break;
            } else {
                result = memcmp_result > 0 ? 1 : -1;
                break;
            }

        } else if (term_is_pid(t) && term_is_pid(other)) {
            //TODO: handle ports
            result = (t > other) ? 1 : -1;
            break;

        } else {
            result = (term_type_to_index(t) > term_type_to_index(other)) ? 1 : -1;
            break;
        }
    }

    temp_stack_destory(&temp_stack);

    return result;
}
