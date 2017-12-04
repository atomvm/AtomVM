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

#ifndef _ATOM_H_
#define _ATOM_H_

#include <stdint.h>

typedef const void * AtomString;

extern AtomString local_atom_string(uint8_t *table_data, int atom_index);
extern void atom_string_to_c(AtomString atom_string, char *buf, int bufsize);
extern int atom_are_equals(AtomString a, AtomString b);

static inline int atom_string_len(AtomString atom_str)
{
    return *((const uint8_t *) atom_str);
}

static inline const void *atom_string_data(AtomString atom_str)
{
    return ((const uint8_t *) atom_str) + 1;
}

#endif
