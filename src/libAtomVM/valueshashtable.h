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

#ifndef _VALUESHASHTABLE_H_
#define _VALUESHASHTABLE_H_

struct ValuesHashTable
{
    int capacity;
    int count;
    struct HNode **buckets;
};

struct ValuesHashTable *valueshashtable_new();
int valueshashtable_insert(struct ValuesHashTable *hash_table, unsigned long key, unsigned long value);
unsigned long valueshashtable_get_value(const struct ValuesHashTable *hash_table, unsigned long key, unsigned long default_value);
int valueshashtable_has_key(const struct ValuesHashTable *hash_table, unsigned long key);

#endif
