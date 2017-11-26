/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
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

#ifndef _LINKEDLIST_H_
#define _LINKEDLIST_H_

#include <stdlib.h>

struct ListHead
{
    struct ListHead *next;
    struct ListHead *prev;
};

#define LIST_ENTRY(list_item, type, list_head_member) \
    ((type *) (((char *) (list_item)) - ((unsigned long) &((type *) 0)->list_head_member)))

static inline void linkedlist_insert(struct ListHead *new_item, struct ListHead *prev_head, struct ListHead *next_head)
{
    new_item->prev = prev_head;
    new_item->next = next_head;
    next_head->prev = new_item;
    prev_head->next = new_item;
}

static inline void linkedlist_remove(struct ListHead **list, struct ListHead *remove_item)
{
    if (remove_item->next == remove_item->prev) {
        *list = NULL;
        return;
    }

    remove_item->prev->next = remove_item->next;
    remove_item->next->prev = remove_item->prev;

    if (*list == remove_item) {
        *list = remove_item->next;
    }
}

static inline void linkedlist_append(struct ListHead **list, struct ListHead *new_item)
{
    if (*list == NULL) {
        linkedlist_insert(new_item, new_item, new_item);
        *list = new_item;
    } else {
        linkedlist_insert(new_item, (*list)->prev, *list);
    }
}

static inline void linkedlist_prepend(struct ListHead **list, struct ListHead *new_item)
{
    if (*list == NULL) {
        linkedlist_insert(new_item, new_item, new_item);
    } else {
        linkedlist_insert(new_item, (*list)->prev, *list);
    }
    *list = new_item;
}

#endif
