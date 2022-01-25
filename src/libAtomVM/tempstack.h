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

#ifndef _TEMPSTACK_H_
#define _TEMPSTACK_H_

struct TempStack
{
    term *stack_end;
    term *stack_pos;
    int size;
};

static inline void temp_stack_init(struct TempStack *temp_stack)
{
    temp_stack->size = 8;
    temp_stack->stack_end = ((term *) malloc(temp_stack->size * sizeof(term))) + temp_stack->size;
    temp_stack->stack_pos = temp_stack->stack_end;
}

static inline void temp_stack_destory(struct TempStack *temp_stack)
{
    free(temp_stack->stack_end - temp_stack->size);
}

static void temp_stack_grow(struct TempStack *temp_stack)
{
    int old_used_size = temp_stack->stack_end - temp_stack->stack_pos;
    int new_size = temp_stack->size * 2;
    term *new_stack_end = ((term *) malloc(new_size * sizeof(term))) + new_size;
    term *new_stack_pos = new_stack_end - old_used_size;
    memcpy(new_stack_pos, temp_stack->stack_pos, old_used_size * sizeof(term));

    free(temp_stack->stack_end - temp_stack->size);
    temp_stack->stack_end = new_stack_end;
    temp_stack->stack_pos = new_stack_pos;
    temp_stack->size = new_size;
}

static inline int temp_stack_is_empty(const struct TempStack *temp_stack)
{
    return temp_stack->stack_end == temp_stack->stack_pos;
}

static inline void temp_stack_push(struct TempStack *temp_stack, term value)
{
    if (temp_stack->stack_end - temp_stack->stack_pos == temp_stack->size - 1) {
        temp_stack_grow(temp_stack);
    }

    temp_stack->stack_pos--;
    *temp_stack->stack_pos = value;
}

static inline term temp_stack_pop(struct TempStack *temp_stack)
{
    term value = *temp_stack->stack_pos;
    temp_stack->stack_pos++;

    return value;
}

#endif
