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

#ifndef _LIST_H_
#define _LIST_H_

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief gets a pointer to the struct that contains a certain list head
 *
 * @details This macro should be used to retrieve a pointer to the struct that is containing the given ListHead.
 */
#define GET_LIST_ENTRY(list_item, type, list_head_member) \
    ((type *) (((char *) (list_item)) - ((unsigned long) &((type *) 0)->list_head_member)))

#define LIST_FOR_EACH(item, head) \
    for (item = (head)->next; item != (head); item = item->next)

#define MUTABLE_LIST_FOR_EACH(item, tmp, head) \
    for (item = (head)->next, tmp = item->next; item != (head); item = tmp, tmp = item->next)

/*
 * @brief a struct requires a ListHead member to be used with linked list manipulation functions.
 *
 * @detail Each struct that is going to be used as part of a linked list should have at least one ListHead,
 * each head can be used for a different linked list.
 */
struct ListHead
{
    struct ListHead *next;
    struct ListHead *prev;
};

static inline void list_insert(struct ListHead *new_item, struct ListHead *prev_head, struct ListHead *next_head)
{
    new_item->prev = prev_head;
    new_item->next = next_head;
    next_head->prev = new_item;
    prev_head->next = new_item;
}

static inline void list_append(struct ListHead *head, struct ListHead *new_item)
{
    list_insert(new_item, head->prev, head);
}

static inline void list_prepend(struct ListHead *head, struct ListHead *new_item)
{
    list_insert(new_item, head, head->next);
}

static inline void list_remove(struct ListHead *remove_item)
{
    remove_item->prev->next = remove_item->next;
    remove_item->next->prev = remove_item->prev;
}

static inline void list_init(struct ListHead *list_item)
{
    list_item->prev = list_item;
    list_item->next = list_item;
}

static inline bool list_is_empty(struct ListHead *list_item)
{
    return (list_item->next == list_item) && (list_item->prev == list_item);
}

static inline struct ListHead *list_first(struct ListHead *head)
{
    return head->next;
}

static inline struct ListHead *list_last(struct ListHead *head)
{
    return head->prev;
}

#ifdef __cplusplus
}
#endif

#endif
