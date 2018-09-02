/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
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

#ifndef _LIST_H_
#define _LIST_H_

#include "linkedlist.h"

#define LIST_FOR_EACH(item, head) \
    for (item = (head)->next; item != (head); item = item->next)

#define MUTABLE_LIST_FOR_EACH(item, tmp, head) \
    for (item = (head)->next, tmp = item->next; item != (head); item = tmp, tmp = item->next)

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

static inline int list_is_empty(struct ListHead *list_item)
{
    return (list_item->next == list_item) && (list_item->prev == list_item);
}

static inline struct ListHead *list_first(struct ListHead *head)
{
    return head->next;
}

static inline struct ListHead *list_last(struct ListHead *head)
{
    return head->next;
}

#endif
