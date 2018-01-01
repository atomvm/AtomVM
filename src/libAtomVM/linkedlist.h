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

/**
 * @file linkedlist.h
 * @brief Linked list manipulation functions
 *
 * @details This header implements manipulation functions for doubly linked circular linked lists.
 */

#ifndef _LINKEDLIST_H_
#define _LINKEDLIST_H_

#include <stdlib.h>

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

/**
 * @brief gets a pointer to the struct that contains a certain list head
 *
 * @details This macro should be used to retrieve a pointer to the struct that is containing the given ListHead.
 */
#define GET_LIST_ENTRY(list_item, type, list_head_member) \
    ((type *) (((char *) (list_item)) - ((unsigned long) &((type *) 0)->list_head_member)))

/**
 * @brief Inserts a linked list head between two linked list heads
 *
 * @details It inserts a linked list head between prev_head and next_head.
 * @param new_item the linked list head that will be inserted to the linked list
 * @param prev_head the linked list head that comes before the element that is going to be inserted
 * @param next_head the linked list head that comes after the element that is going to be inserted
 */
static inline void linkedlist_insert(struct ListHead *new_item, struct ListHead *prev_head, struct ListHead *next_head)
{
    new_item->prev = prev_head;
    new_item->next = next_head;
    next_head->prev = new_item;
    prev_head->next = new_item;
}

/**
 * @brief Removes a linked list item from a linked list
 *
 * @details It removes a linked list head from the list pointed by list.
 * @param list a pointer to the linked list pointer that we want to remove the item from, it will be set to NULL if no items are left
 * @param remove_item the item that is going to be removed
 */
static inline void linkedlist_remove(struct ListHead **list, struct ListHead *remove_item)
{
    if (remove_item->next == remove_item) {
        *list = NULL;
        return;
    }

    remove_item->prev->next = remove_item->next;
    remove_item->next->prev = remove_item->prev;

    if (*list == remove_item) {
        *list = remove_item->next;
    }
}

/**
 * @brief Appends a list item to a linked list
 *
 * @details It appends a list item head to a linked list and it initializes linked list pointer if empty.
 * @param list a pointer to the linked list pointer that the head is going to be append, it will be set to new_item if it is the first one
 * @param new_item the item that is going to be appended to the end of the list
 */
static inline void linkedlist_append(struct ListHead **list, struct ListHead *new_item)
{
    if (*list == NULL) {
        linkedlist_insert(new_item, new_item, new_item);
        *list = new_item;
    } else {
        linkedlist_insert(new_item, (*list)->prev, *list);
    }
}

/**
 * @brief Prepends a list item to a linked list
 *
 * @details It prepends a list item head to a linked list and it updates the pointer to the list.
 * @param list a pointer to the linked list
 * @param new_item the list head that is going to be prepended to the list
 */
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
