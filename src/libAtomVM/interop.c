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

#include "atom_table.h"
#include "bitstring.h"
#include "defaultatoms.h"
#include "tempstack.h"
#include "term.h"
#include "term_typedef.h"
#include "valueshashtable.h"
#include <stdint.h>

char *interop_term_to_string(term t, int *ok)
{
    if (term_is_list(t)) {
        return interop_iolist_to_string(t, ok);

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

char *interop_iolist_to_string(term list, int *ok)
{
    size_t len;
    switch (interop_iolist_size(list, &len)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            *ok = 0;
            return NULL;
        case InteropBadArg:
            *ok = 0;
            return NULL;
    }

    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }

    switch (interop_write_iolist(list, str)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            free(str);
            *ok = 0;
            return NULL;
        case InteropBadArg:
            free(str);
            *ok = 0;
            return NULL;
    }

    str[len] = 0;

    *ok = 1;
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
    GlobalContext *glb = ctx->global;

    int atom_index = term_to_atom_index(atom);

    size_t len;
    atom_ref_t atom_ref = atom_table_get_atom_ptr_and_len(glb->atom_table, atom_index, &len);

    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }
    atom_table_write_cstring(glb->atom_table, atom_ref, len + 1, str);

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

inline InteropFunctionResult interop_chardata_fold(term t, interop_chardata_fold_fun fold_fun, interop_chardata_rest_fun rest_fun, void *accum)
{
    if (term_is_binary(t)) {
        return fold_fun(t, accum);
    }

    if (UNLIKELY(!term_is_list(t))) {
        return InteropBadArg;
    }

    struct TempStack temp_stack;
    if (UNLIKELY(temp_stack_init(&temp_stack) != TempStackOk)) {
        return InteropMemoryAllocFail;
    }

    if (UNLIKELY(temp_stack_push(&temp_stack, t) != TempStackOk)) {
        temp_stack_destroy(&temp_stack);
        return InteropMemoryAllocFail;
    }

    while (!temp_stack_is_empty(&temp_stack)) {
        if (term_is_integer(t) || term_is_binary(t)) {
            InteropFunctionResult result = fold_fun(t, accum);
            if (UNLIKELY(result != InteropOk)) {
                if (rest_fun) {
                    // we don't pass failed element, fold_fun handles it
                    t = temp_stack_pop(&temp_stack);
                    while (!temp_stack_is_empty(&temp_stack)) {
                        rest_fun(t, accum);
                        t = temp_stack_pop(&temp_stack);
                    }
                }
                // we don't process last element either which is the original list
                temp_stack_destroy(&temp_stack);
                return result;
            } else {
                t = temp_stack_pop(&temp_stack);
            }

        } else if (term_is_nil(t)) {
            t = temp_stack_pop(&temp_stack);

        } else if (term_is_nonempty_list(t)) {
            if (UNLIKELY(temp_stack_push(&temp_stack, term_get_list_tail(t)) != TempStackOk)) {
                temp_stack_destroy(&temp_stack);
                return InteropMemoryAllocFail;
            }
            t = term_get_list_head(t);

        } else {
            if (rest_fun) {
                while (!temp_stack_is_empty(&temp_stack)) {
                    rest_fun(t, accum);
                    t = temp_stack_pop(&temp_stack);
                }
                // we don't process last element which was the passed term
            }
            temp_stack_destroy(&temp_stack);
            return InteropBadArg;
        }
    }

    temp_stack_destroy(&temp_stack);

    return InteropOk;
}

static inline InteropFunctionResult size_fold_fun(term t, void *accum)
{
    size_t *size = (size_t *) accum;
    if (term_is_integer(t)) {
        *size += 1;
    } else /* term_is_binary(t) */ {
        *size += term_binary_size(t);
    }
    return InteropOk;
}

InteropFunctionResult interop_iolist_size(term t, size_t *size)
{
    *size = 0;
    return interop_chardata_fold(t, size_fold_fun, NULL, size);
}

static inline InteropFunctionResult write_string_fold_fun(term t, void *accum)
{
    char **p = (char **) accum;
    if (term_is_integer(t)) {
        **p = term_to_int(t);
        (*p)++;
    } else /* term_is_binary(t) */ {
        int len = term_binary_size(t);
        memcpy(*p, term_binary_data(t), len);
        *p += len;
    }
    return InteropOk;
}

InteropFunctionResult interop_write_iolist(term t, char *p)
{
    return interop_chardata_fold(t, write_string_fold_fun, NULL, (void *) &p);
}

static enum UnicodeConversionResult interop_binary_conversion(term t, uint8_t *output, size_t *output_len, size_t *rest_crsr, enum CharDataEncoding in_encoding, enum CharDataEncoding out_encoding)
{
    size_t len = term_binary_size(t);
    if (in_encoding == Latin1Encoding && out_encoding == Latin1Encoding) {
        if (output) {
            memcpy(output, term_binary_data(t), len);
        }
        *output_len = len;
        return UnicodeOk;
    }
    size_t result = 0;
    size_t input_index;
    const uint8_t *input = (const uint8_t *) term_binary_data(t);
    if (in_encoding == Latin1Encoding) {
        for (input_index = 0; input_index < len; input_index++) {
            if (out_encoding == UTF8Encoding) {
                size_t char_size;
                if (UNLIKELY(!bitstring_utf8_encode(input[input_index], output, &char_size))) {
                    *rest_crsr = input_index;
                    *output_len = result;
                    return UnicodeError;
                }
                result += char_size;
                if (output) {
                    output += char_size;
                }
            } else {
                // UCS4Native
                result += sizeof(uint32_t);
                if (output) {
                    *((uint32_t *) output) = input[input_index];
                    output += sizeof(uint32_t);
                }
            }
        }
        *output_len = result;
        return UnicodeOk;
    }
    input_index = 0;
    while (input_index < len) {
        size_t char_size;
        uint32_t c;
        enum UnicodeTransformDecodeResult decode_result = bitstring_utf8_decode(input + input_index, len - input_index, &c, &char_size);
        if (UNLIKELY(decode_result != UnicodeTransformDecodeSuccess)) {
            *rest_crsr = input_index;
            *output_len = result;
            return decode_result == UnicodeTransformDecodeIncomplete ? UnicodeIncompleteTransform : UnicodeError;
        }
        switch (out_encoding) {
            case Latin1Encoding: {
                if (c > 255) {
                    *rest_crsr = input_index;
                    *output_len = result;
                    return UnicodeError;
                }
                if (output) {
                    *output++ = c;
                }
                result++;
            } break;
            case UTF8Encoding: {
                if (output) {
                    memcpy(output, input + input_index, char_size);
                    output += char_size;
                }
                result += char_size;
            } break;
            case UCS4NativeEncoding: {
                if (output) {
                    *((uint32_t *) output) = c;
                    output += sizeof(uint32_t);
                }
                result += sizeof(uint32_t);
            } break;
        }
        input_index += char_size;
    }
    *output_len = result;
    return UnicodeOk;
}

struct CharDataToBytesSizeAcc
{
    enum CharDataEncoding in_encoding;
    enum CharDataEncoding out_encoding;
    size_t size;
    size_t rest_size;
    bool badarg;
    bool incomplete_transform;
};

static InteropFunctionResult chardata_to_bytes_size_fold_fun(term t, void *accum)
{
    struct CharDataToBytesSizeAcc *acc = (struct CharDataToBytesSizeAcc *) accum;
    if (term_is_binary(t)) {
        size_t bin_size;
        size_t rest_crsr;
        enum UnicodeConversionResult conv_result = interop_binary_conversion(t, NULL, &bin_size, &rest_crsr, acc->in_encoding, acc->out_encoding);
        acc->size += bin_size;
        if (UNLIKELY(conv_result != UnicodeOk)) {
            acc->rest_size = term_sub_binary_heap_size(t, term_binary_size(t) - rest_crsr);
            acc->incomplete_transform = conv_result == UnicodeIncompleteTransform;
            return InteropBadArg;
        }
    } else /* term_is_integer(t) */ {
        avm_int_t c = term_to_int(t);
        if (c < 0 || c > UNICODE_CHAR_MAX) {
            return InteropBadArg;
        }
        switch (acc->out_encoding) {
            case Latin1Encoding: {
                if (c > 255) {
                    return InteropBadArg;
                }
                acc->size++;
            } break;
            case UTF8Encoding: {
                size_t char_size;
                if (UNLIKELY(!bitstring_utf8_encode((uint32_t) c, NULL, &char_size))) {
                    return InteropBadArg;
                }
                acc->size += char_size;
            } break;
            case UCS4NativeEncoding: {
                acc->size += sizeof(uint32_t);
            } break;
        }
    }
    return InteropOk;
}

static void chardata_to_bytes_size_rest_fun(term t, void *accum)
{
    struct CharDataToBytesSizeAcc *acc = (struct CharDataToBytesSizeAcc *) accum;
    if (!term_is_binary(t) && !term_is_integer(t) && !term_is_list(t)) {
        acc->badarg = true;
    }
    if (!acc->badarg) {
        if (!term_is_nil(t)) {
            acc->incomplete_transform = false;
        }
        acc->rest_size += CONS_SIZE;
    }
}

enum UnicodeConversionResult interop_chardata_to_bytes_size(term t, size_t *size, size_t *rest_size, enum CharDataEncoding in_encoding, enum CharDataEncoding out_encoding)
{
    struct CharDataToBytesSizeAcc acc = {
        .in_encoding = in_encoding,
        .out_encoding = out_encoding,
        .size = 0,
        .rest_size = 0,
        .badarg = false,
        .incomplete_transform = false
    };
    InteropFunctionResult res = interop_chardata_fold(t, chardata_to_bytes_size_fold_fun, chardata_to_bytes_size_rest_fun, &acc);
    if (UNLIKELY(res == InteropMemoryAllocFail)) {
        return UnicodeMemoryAllocFail;
    }
    if (acc.badarg) {
        return UnicodeBadArg;
    }
    *size = acc.size;
    if (rest_size) {
        *rest_size = acc.rest_size;
    }
    if (acc.incomplete_transform) {
        return UnicodeIncompleteTransform;
    }
    return res == InteropOk ? UnicodeOk : UnicodeError;
}

struct CharDataToBytesAcc
{
    enum CharDataEncoding in_encoding;
    enum CharDataEncoding out_encoding;
    uint8_t *output;
    term *rest;
    Heap *heap;
    bool badarg;
    bool incomplete_transform;
};

static InteropFunctionResult chardata_to_bytes_fold_fun(term t, void *accum)
{
    struct CharDataToBytesAcc *acc = (struct CharDataToBytesAcc *) accum;
    if (term_is_binary(t)) {
        size_t bin_size;
        size_t rest_crsr;
        enum UnicodeConversionResult conv_result = interop_binary_conversion(t, acc->output, &bin_size, &rest_crsr, acc->in_encoding, acc->out_encoding);
        acc->output += bin_size;
        if (UNLIKELY(conv_result != UnicodeOk)) {
            if (acc->rest) {
                *acc->rest = term_alloc_sub_binary(t, rest_crsr, term_binary_size(t) - rest_crsr, acc->heap);
            }
            if (conv_result == UnicodeIncompleteTransform) {
                acc->incomplete_transform = true;
            }
            return InteropBadArg;
        }
    } else /* term_is_integer(t) */ {
        avm_int_t c = term_to_int(t);
        if (c < 0 || c > UNICODE_CHAR_MAX) {
            if (acc->rest) {
                *acc->rest = t;
            }
            return InteropBadArg;
        }
        switch (acc->out_encoding) {
            case Latin1Encoding: {
                if (c > 255) {
                    if (acc->rest) {
                        *acc->rest = t;
                    }
                    return InteropBadArg;
                }
                *acc->output++ = (uint8_t) c;
            } break;
            case UTF8Encoding: {
                size_t char_size;
                if (UNLIKELY(!bitstring_utf8_encode((uint32_t) c, acc->output, &char_size))) {
                    if (acc->rest) {
                        *acc->rest = t;
                    }
                    return InteropBadArg;
                }
                acc->output += char_size;
            } break;
            case UCS4NativeEncoding: {
                // Special case: if input encoding is latin1, only accept characters < 256.
                if (acc->in_encoding == Latin1Encoding && c > 255) {
                    if (acc->rest) {
                        *acc->rest = t;
                    }
                    return InteropBadArg;
                }
                *((uint32_t *) acc->output) = (uint32_t) c;
                acc->output += sizeof(uint32_t);
            } break;
        }
    }
    return InteropOk;
}

static void chardata_to_bytes_rest_fun(term t, void *accum)
{
    struct CharDataToBytesAcc *acc = (struct CharDataToBytesAcc *) accum;
    if (!term_is_binary(t) && !term_is_integer(t) && !term_is_list(t)) {
        acc->badarg = true;
    }
    if (!acc->badarg) {
        if (!term_is_nil(t)) {
            acc->incomplete_transform = false;
        }
        if (acc->rest) {
            *acc->rest = term_list_prepend(*acc->rest, t, acc->heap);
        }
    }
}

enum UnicodeConversionResult interop_chardata_to_bytes(term t, uint8_t *output, term *rest, enum CharDataEncoding in_encoding, enum CharDataEncoding out_encoding, Heap *heap)
{
    struct CharDataToBytesAcc acc = {
        .in_encoding = in_encoding,
        .out_encoding = out_encoding,
        .output = output,
        .rest = rest,
        .heap = heap,
        .badarg = false,
        .incomplete_transform = false
    };
    InteropFunctionResult res = interop_chardata_fold(t, chardata_to_bytes_fold_fun, chardata_to_bytes_rest_fun, &acc);
    if (UNLIKELY(res == InteropMemoryAllocFail)) {
        return UnicodeMemoryAllocFail;
    }
    if (acc.badarg) {
        return UnicodeBadArg;
    }
    if (acc.incomplete_transform) {
        return UnicodeIncompleteTransform;
    }
    return res == InteropOk ? UnicodeOk : UnicodeError;
}

term interop_map_get_value(GlobalContext *glb, term map, term key)
{
    return interop_map_get_value_default(glb, map, key, term_nil());
}

term interop_map_get_value_default(GlobalContext *glb, term map, term key, term default_value)
{
    int pos = term_find_map_pos(map, key, glb);
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
        return interop_map_get_value_default(glb, kv, key_term, default_value);
    } else {
        return default_value;
    }
}

term interop_atom_term_select_atom(const AtomStringIntPair *table, int value, GlobalContext *global)
{
    for (int i = 0; table[i].as_val != NULL; i++) {
        if (value == table[i].i_val) {
            int global_atom_index = globalcontext_insert_atom(global, table[i].as_val);
            return term_from_atom_index(global_atom_index);
        }
    }
    return term_invalid_term();
}

term interop_chars_to_list(const char *chars, size_t len, Heap *heap)
{
    term ret = term_nil();
    for (int i = (int) len - 1; i >= 0; --i) {
        term c_term = term_from_int(chars[i]);
        ret = term_list_prepend(c_term, ret, heap);
    }
    return ret;
}
