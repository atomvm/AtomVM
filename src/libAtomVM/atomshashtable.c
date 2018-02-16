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

#include "atomshashtable.h"

#include <stdlib.h>
#include <string.h>

#define DEFAULT_SIZE 8

struct HNode
{
    struct HNode *next;
    AtomString *key;
    unsigned long value;
};

static unsigned long sdbm_hash(const unsigned char *str, int len)
{
    unsigned long hash = 0;
    int c;

    for (int i = 0; i < len; i++) {
        c = *str++;
        hash = c + (hash << 6) + (hash << 16) - hash;
    }

    return hash;
}

struct AtomsHashTable *atomshashtable_new()
{
    struct AtomsHashTable *htable = malloc(sizeof(struct AtomsHashTable));
    if (!htable) {
        return NULL;
    }
    htable->buckets = calloc(DEFAULT_SIZE, sizeof(struct HNode *));
    if (!htable->buckets) {
        free(htable);
        return NULL;
    }

    htable->count = 0;
    htable->capacity = DEFAULT_SIZE;

    return htable;
}

int atomshashtable_insert(struct AtomsHashTable *hash_table, AtomString string, unsigned long value)
{
    int alen = atom_string_len(string);

    unsigned long hash = sdbm_hash(string, alen);
    long index = hash % hash_table->capacity;

    struct HNode *node = hash_table->buckets[index];
    if (node) {
        while (1) {
            if (atom_are_equals(string, node->key)) {
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

    AtomString *astring = malloc(alen + 1);
    if (!astring) {
        return 0;
    }

    struct HNode *new_node = malloc(sizeof(struct HNode *));
    if (!new_node) {
        free(astring);
        return 0;
    }
    memcpy(astring, string, alen + 1);
    new_node->next = NULL;
    new_node->key = astring;
    new_node->value = value;

    if (node) {
        node->next = new_node;
    } else {
        hash_table->buckets[index] = new_node;
    }

    hash_table->count++;
    return 1;
}

unsigned long atomshashtable_get_value(const struct AtomsHashTable *hash_table, const AtomString string, unsigned long default_value)
{
    unsigned long hash = sdbm_hash(string, atom_string_len(string));
    long index = hash % hash_table->capacity;

    const struct HNode *node = hash_table->buckets[index];
    while (node) {
        if (atom_are_equals(string, node->key)) {
            return node->value;
        }

        node = node->next;
    }

    return default_value;
}

int atomshashtable_has_key(const struct AtomsHashTable *hash_table, const AtomString string)
{
    unsigned long hash = sdbm_hash(string, atom_string_len(string));
    long index = hash % hash_table->capacity;

    const struct HNode *node = hash_table->buckets[index];
    while (node) {
        if (atom_are_equals(string, node->key)) {
            return 1;
        }

        node = node->next;
    }

    return 0;
}
