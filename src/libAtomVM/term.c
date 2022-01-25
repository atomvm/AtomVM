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
#include "context.h"
#include "interop.h"
#include "tempstack.h"
#include "valueshashtable.h"

#include <ctype.h>
#include <inttypes.h>
#include <stdio.h>

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

    } else if (term_is_map(t)) {
        fprintf(fd, "#{");

        int map_size = term_get_map_size(t);
        for (int i = 0; i < map_size; i++) {
            if (i != 0) {
                fputc(',', fd);
            }
            term_display(fd, term_get_map_key(t, i), ctx);
            fprintf(fd, "=>");
            term_display(fd, term_get_map_value(t, i), ctx);
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
        const char *format =
#ifdef __clang__
        "#Ref<0.0.0.%llu>";
#else
        "#Ref<0.0.0.%lu>";
#endif
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
        fprintf(fd, "Unknown term type: %li", t);
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

    } else if (term_is_map(t)) {
        return 11;

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

        } else if (term_is_map(t) && term_is_map(other)) {
            int t_size = term_get_map_size(t);
            int other_size = term_get_map_size(other);

            if (t_size != other_size) {
                result = (t_size > other_size) ? 1 : -1;
                break;
            }
            if (t_size > 0) {
                for (int i = 1; i < t_size; i++) {
                    temp_stack_push(&temp_stack, term_get_map_value(t, i));
                    temp_stack_push(&temp_stack, term_get_map_value(other, i));
                    temp_stack_push(&temp_stack, term_get_map_key(t, i));
                    temp_stack_push(&temp_stack, term_get_map_key(other, i));
                }
                t = term_get_map_key(t, 0);
                other = term_get_map_key(other, 0);

            } else {
                other = temp_stack_pop(&temp_stack);
                t = temp_stack_pop(&temp_stack);
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

term term_alloc_refc_binary(Context *ctx, size_t size, bool is_const)
{
    term *boxed_value = memory_heap_alloc(ctx, TERM_BOXED_REFC_BINARY_SIZE);
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
            abort();
        }
        boxed_value[3] = (term) refc;
        ctx->mso_list = term_list_init_prepend(boxed_value + 4, ret, ctx->mso_list);
        list_append(&ctx->global->refc_binaries, (struct ListHead *) refc);
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

term term_alloc_sub_binary(term binary_or_state, size_t offset, size_t len, Context *ctx)
{
    term *boxed = memory_heap_alloc(ctx, TERM_BOXED_SUB_BINARY_SIZE);
    term binary = find_binary(binary_or_state);

    boxed[0] = ((TERM_BOXED_SUB_BINARY_SIZE - 1) << 6) | TERM_BOXED_SUB_BINARY;
    boxed[1] = (term) len;
    boxed[2] = (term) offset;
    boxed[3] = binary;

    return ((term) boxed) | TERM_BOXED_VALUE_TAG;
}
