/*
 * This file is part of AtomVM.
 *
 * Copyright 2018-2023 Davide Bettio <davide@uninstall.it>
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

#ifndef _TEMPSTACK_H_
#define _TEMPSTACK_H_

#include "term_typedef.h"
#include "utils.h"

typedef enum
{
    TempStackOk = 0,
    TempStackFailedAlloc = 1
} TempStackResult;

struct TempStack
{
    term *stack_end;
    term *stack_pos;
    int size;
};

NO_DISCARD static inline TempStackResult temp_stack_init(struct TempStack *temp_stack)
{
    temp_stack->size = 8;
    temp_stack->stack_end = ((term *) malloc(temp_stack->size * sizeof(term))) + temp_stack->size;
    if (IS_NULL_PTR(temp_stack->stack_end)) {
        return TempStackFailedAlloc;
    }
    temp_stack->stack_pos = temp_stack->stack_end;

    return TempStackOk;
}

static inline void temp_stack_destroy(struct TempStack *temp_stack)
{
    free(temp_stack->stack_end - temp_stack->size);
}

NO_DISCARD static TempStackResult temp_stack_grow(struct TempStack *temp_stack)
{
    int old_used_size = temp_stack->stack_end - temp_stack->stack_pos;
    int new_size = temp_stack->size * 2;
    term *new_stack_end = ((term *) malloc(new_size * sizeof(term))) + new_size;
    if (IS_NULL_PTR(new_stack_end)) {
        return TempStackFailedAlloc;
    }
    term *new_stack_pos = new_stack_end - old_used_size;
    memcpy(new_stack_pos, temp_stack->stack_pos, old_used_size * sizeof(term));

    free(temp_stack->stack_end - temp_stack->size);
    temp_stack->stack_end = new_stack_end;
    temp_stack->stack_pos = new_stack_pos;
    temp_stack->size = new_size;

    return TempStackOk;
}

static inline int temp_stack_is_empty(const struct TempStack *temp_stack)
{
    return temp_stack->stack_end == temp_stack->stack_pos;
}

NO_DISCARD static inline TempStackResult temp_stack_push(struct TempStack *temp_stack, term value)
{
    if (temp_stack->stack_end - temp_stack->stack_pos == temp_stack->size - 1) {
        TempStackResult ret = temp_stack_grow(temp_stack);
        if (UNLIKELY(ret != TempStackOk)) {
            return ret;
        }
    }

    temp_stack->stack_pos--;
    *temp_stack->stack_pos = value;

    return TempStackOk;
}

static inline term temp_stack_pop(struct TempStack *temp_stack)
{
    term value = *temp_stack->stack_pos;
    temp_stack->stack_pos++;

    return value;
}

#endif
