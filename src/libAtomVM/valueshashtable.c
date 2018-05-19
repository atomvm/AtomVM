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

#include "valueshashtable.h"

#include "utils.h"

#include <stdlib.h>

#define DEFAULT_SIZE 8

struct HNode
{
    struct HNode *next;
    unsigned long key;
    unsigned long value;
};

struct ValuesHashTable *valueshashtable_new()
{
    struct ValuesHashTable *htable = malloc(sizeof(struct ValuesHashTable));
    if (IS_NULL_PTR(htable)) {
        return NULL;
    }
    htable->buckets = calloc(DEFAULT_SIZE, sizeof(struct HNode *));
    if (IS_NULL_PTR(htable->buckets)) {
        free(htable);
        return NULL;
    }

    htable->count = 0;
    htable->capacity = DEFAULT_SIZE;

    return htable;
}

int valueshashtable_insert(struct ValuesHashTable *hash_table, unsigned long key, unsigned long value)
{
    long index = key % hash_table->capacity;

    struct HNode *node = hash_table->buckets[index];
    if (node) {
        while (1) {
            if (node->key == key) {
                node->value = value;
                return 1;
            }

            if (node->next) {
                node = node->next;
            } else {
                break;
            }
        }
    }

    struct HNode *new_node = malloc(sizeof(struct HNode));
    if (IS_NULL_PTR(new_node)) {
        return 0;
    }
    new_node->next = NULL;
    new_node->key = key;
    new_node->value = value;

    if (node) {
        node->next = new_node;
    } else {
        hash_table->buckets[index] = new_node;
    }

    hash_table->count++;
    return 1;
}

unsigned long valueshashtable_get_value(const struct ValuesHashTable *hash_table, unsigned long key, unsigned long default_value)
{
    long index = key % hash_table->capacity;

    const struct HNode *node = hash_table->buckets[index];
    while (node) {
        if (node->key == key) {
            return node->value;
        }

        node = node->next;
    }

    return default_value;
}

int valueshashtable_has_key(const struct ValuesHashTable *hash_table, unsigned long key)
{
    long index = key % hash_table->capacity;

    const struct HNode *node = hash_table->buckets[index];
    while (node) {
        if (node->key == key) {
            return 1;
        }

        node = node->next;
    }

    return 0;
}
