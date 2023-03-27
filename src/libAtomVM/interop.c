/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
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

#include "interop.h"

#include "defaultatoms.h"
#include "tempstack.h"
#include "valueshashtable.h"

char *interop_term_to_string(term t, int *ok)
{
    if (term_is_list(t)) {
        return interop_list_to_string(t, ok);

    } else if (term_is_binary(t)) {
        char *str = interop_binary_to_string(t);
        *ok = str != NULL;
        return str;

    } else {
        // TODO: implement also for other types?
        *ok = 0;
        return NULL;
    }
}

char *interop_binary_to_string(term binary)
{
    int len = term_binary_size(binary);

    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }
    memcpy(str, term_binary_data(binary), len);

    str[len] = 0;

    return str;
}

char *interop_list_to_string(term list, int *ok)
{
    int proper;
    int len = term_list_length(list, &proper);
    if (UNLIKELY(!proper)) {
        *ok = 0;
        return NULL;
    }

    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }

    term t = list;
    for (int i = 0; i < len; i++) {
        term byte_value_term = term_get_list_head(t);
        if (UNLIKELY(!term_is_integer(byte_value_term))) {
            *ok = 0;
            free(str);
            return NULL;
        }

        if (UNLIKELY(!term_is_uint8(byte_value_term))) {
            *ok = 0;
            free(str);
            return NULL;
        }
        uint8_t byte_value = term_to_uint8(byte_value_term);

        str[i] = (char) byte_value;
        t = term_get_list_tail(t);
    }
    str[len] = 0;

    *ok = 1;
    return str;
}

char *interop_atom_to_string(Context *ctx, term atom)
{
    int atom_index = term_to_atom_index(atom);
    AtomString atom_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table, atom_index, (unsigned long) NULL);
    int len = atom_string_len(atom_string);

    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }
    atom_string_to_c(atom_string, str, len + 1);

    return str;
}

term interop_proplist_get_value(term list, term key)
{
    return interop_proplist_get_value_default(list, key, term_nil());
}

term interop_proplist_get_value_default(term list, term key, term default_value)
{
    term t = list;

    while (!term_is_nil(t)) {
        term *t_ptr = term_get_list_ptr(t);

        term head = t_ptr[1];
        if (term_is_tuple(head) && term_get_tuple_arity(head) == 2 && term_get_tuple_element(head, 0) == key) {
            return term_get_tuple_element(head, 1);

        } else if (term_is_atom(head)) {
            if (head == key) {
                return TRUE_ATOM;
            }
        }

        t = *t_ptr;
    }

    return default_value;
}

InteropFunctionResult interop_iolist_size(term t, size_t *size)
{
    if (term_is_binary(t)) {
        *size = term_binary_size(t);
        return InteropOk;
    }

    if (UNLIKELY(!term_is_list(t))) {
        return InteropBadArg;
    }

    unsigned long acc = 0;

    struct TempStack temp_stack;
    if (UNLIKELY(temp_stack_init(&temp_stack) != TempStackOk)) {
        return InteropMemoryAllocFail;
    }

    if (UNLIKELY(temp_stack_push(&temp_stack, t) != TempStackOk)) {
        temp_stack_destory(&temp_stack);
        return InteropMemoryAllocFail;
    }

    while (!temp_stack_is_empty(&temp_stack)) {
        if (term_is_integer(t)) {
            acc++;
            t = temp_stack_pop(&temp_stack);

        } else if (term_is_nil(t)) {
            t = temp_stack_pop(&temp_stack);

        } else if (term_is_nonempty_list(t)) {
            if (UNLIKELY(temp_stack_push(&temp_stack, term_get_list_tail(t)) != TempStackOk)) {
                temp_stack_destory(&temp_stack);
                return InteropMemoryAllocFail;
            }
            t = term_get_list_head(t);

        } else if (term_is_binary(t)) {
            acc += term_binary_size(t);
            t = temp_stack_pop(&temp_stack);

        } else {
            temp_stack_destory(&temp_stack);
            return InteropBadArg;
        }
    }

    temp_stack_destory(&temp_stack);

    *size = acc;
    return InteropOk;
}

InteropFunctionResult interop_write_iolist(term t, char *p)
{
    if (term_is_binary(t)) {
        int len = term_binary_size(t);
        memcpy(p, term_binary_data(t), len);
        return InteropOk;
    }

    struct TempStack temp_stack;
    if (UNLIKELY(temp_stack_init(&temp_stack) != TempStackOk)) {
        return InteropMemoryAllocFail;
    }

    if (UNLIKELY(temp_stack_push(&temp_stack, t) != TempStackOk)) {
        temp_stack_destory(&temp_stack);
        return InteropMemoryAllocFail;
    }

    while (!temp_stack_is_empty(&temp_stack)) {
        if (term_is_integer(t)) {
            *p = term_to_int(t);
            p++;
            t = temp_stack_pop(&temp_stack);

        } else if (term_is_nil(t)) {
            t = temp_stack_pop(&temp_stack);

        } else if (term_is_nonempty_list(t)) {
            if (UNLIKELY(temp_stack_push(&temp_stack, term_get_list_tail(t)) != TempStackOk)) {
                temp_stack_destory(&temp_stack);
                return InteropMemoryAllocFail;
            }
            t = term_get_list_head(t);

        } else if (term_is_binary(t)) {
            int len = term_binary_size(t);
            memcpy(p, term_binary_data(t), len);
            p += len;
            t = temp_stack_pop(&temp_stack);

        } else {
            temp_stack_destory(&temp_stack);
            return InteropBadArg;
        }
    }

    temp_stack_destory(&temp_stack);
    return InteropOk;
}

term interop_map_get_value(Context *ctx, term map, term key)
{
    return interop_map_get_value_default(ctx, map, key, term_nil());
}

term interop_map_get_value_default(Context *ctx, term map, term key, term default_value)
{
    int pos = term_find_map_pos(map, key, ctx->global);
    if (pos == TERM_MAP_NOT_FOUND) {
        return default_value;
    } else if (UNLIKELY(pos == TERM_MAP_MEMORY_ALLOC_FAIL)) {
        // TODO: do not crash, handle out of memory
        AVM_ABORT();
    } else {
        return term_get_map_value(map, pos);
    }
}

int interop_atom_term_select_int(const AtomStringIntPair *table, term atom, GlobalContext *global)
{
    int i;
    for (i = 0; table[i].as_val != NULL; i++) {
        if (globalcontext_is_term_equal_to_atom_string(global, atom, table[i].as_val)) {
            return table[i].i_val;
        }
    }
    return table[i].i_val;
}

term interop_kv_get_value_default(term kv, AtomString key, term default_value, GlobalContext *glb)
{
    term key_term = globalcontext_existing_term_from_atom_string(glb, key);
    if (term_is_invalid_term(key_term)) {
        return default_value;
    }

    if (term_is_nonempty_list(kv)) {
        return interop_proplist_get_value_default(kv, key_term, default_value);
    } else if (term_is_map(kv)) {
        return interop_proplist_get_value_default(kv, key_term, default_value);
    } else {
        return default_value;
    }
}
