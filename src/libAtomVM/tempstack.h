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

#include <stdbool.h>

#include "term_typedef.h"
#include "utils.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum
{
    TempStackOk = 0,
    TempStackFailedAlloc = 1
} TempStackResult;

#define MIN_STACK_SIZE 8

struct TempStack
{
    term *stack_start;
    term *stack_pos;
    term *stack_end;
    term min_stack[MIN_STACK_SIZE];
};

NO_DISCARD static inline TempStackResult temp_stack_init(struct TempStack *temp_stack)
{
    temp_stack->stack_start = temp_stack->min_stack;
    temp_stack->stack_end = temp_stack->stack_start + MIN_STACK_SIZE;
    temp_stack->stack_pos = temp_stack->stack_end;

    return TempStackOk;
}

static inline void temp_stack_destroy(struct TempStack *temp_stack)
{
    if (temp_stack->stack_start != temp_stack->min_stack) {
        free(temp_stack->stack_start);
    }
}

NO_DISCARD static TempStackResult temp_stack_grow(struct TempStack *temp_stack)
{
    size_t old_used_size = temp_stack->stack_end - temp_stack->stack_start;
    size_t new_size = old_used_size * 2;
    term *new_stack_start = ((term *) malloc(new_size * sizeof(term)));
    if (IS_NULL_PTR(new_stack_start)) {
        return TempStackFailedAlloc;
    }
    term *new_stack_end = new_stack_start + new_size;
    term *new_stack_pos = new_stack_end;
    if (temp_stack->stack_start != temp_stack->min_stack) {
        memcpy((term *) new_stack_start + new_size - old_used_size, (const term *) temp_stack->stack_start, old_used_size * sizeof(term));
        free(temp_stack->stack_start);
        new_stack_pos -= old_used_size;
    }

    temp_stack->stack_end = new_stack_end;
    temp_stack->stack_pos = new_stack_pos;
    temp_stack->stack_start = new_stack_start;

    return TempStackOk;
}

static inline bool temp_stack_is_empty(const struct TempStack *temp_stack)
{
    return temp_stack->stack_pos == temp_stack->min_stack + MIN_STACK_SIZE;
}

NO_DISCARD static inline TempStackResult temp_stack_push(struct TempStack *temp_stack, term value)
{
    if (temp_stack->stack_pos == temp_stack->stack_start) {
        TempStackResult ret = temp_stack_grow(temp_stack);
        if (UNLIKELY(ret != TempStackOk)) {
            return ret;
        }
    } else if (temp_stack->stack_pos == temp_stack->min_stack) {
        // We reached the end of min_stack but we already had an allocated buffer
        temp_stack->stack_pos = temp_stack->stack_end;
    }

    temp_stack->stack_pos--;
    *temp_stack->stack_pos = value;

    return TempStackOk;
}

static inline term temp_stack_pop(struct TempStack *temp_stack)
{
    term value = *temp_stack->stack_pos;
    temp_stack->stack_pos++;

    if (temp_stack->stack_pos == temp_stack->stack_end && temp_stack->stack_end != temp_stack->min_stack + MIN_STACK_SIZE) {
        // Transition to C-stack based buffer
        temp_stack->stack_pos = temp_stack->min_stack;
    }

    return value;
}

#ifdef __cplusplus
}
#endif

#endif
