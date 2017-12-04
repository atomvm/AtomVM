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

#include "atom.h"

#include <stdlib.h>
#include <string.h>

#include "utils.h"

AtomString local_atom_string(uint8_t *table_data, int atom_index)
{
    int atoms_count = READ_32_ALIGNED(table_data + 8);
    const char *current_atom = (const char *) table_data + 12;

    if (atom_index > atoms_count) {
        abort();
    }

    const char *atom = NULL;
    for (int i = 1; i <= atom_index; i++) {
        int atom_len = *current_atom;
        atom = current_atom;

        current_atom += atom_len + 1;
    }

    return (AtomString) atom;
}

void atom_string_to_c(AtomString atom_string, char *buf, int bufsize)
{
    int atom_len = *((uint8_t *) atom_string);

    if (bufsize < atom_len) {
        atom_len = bufsize - 1;
    }
    memcpy(buf, ((uint8_t *) atom_string) + 1, atom_len);
    buf[atom_len] = '\0';
}

int atom_are_equals(AtomString a, AtomString b)
{
    int atom_len_a = *((const uint8_t *) a);
    int atom_len_b = *((const uint8_t *) b);

    if (atom_len_a != atom_len_b) {
        return 0;
    }

    if (!memcmp((uint8_t *) a + 1, (uint8_t *) b + 1, atom_len_a)) {
        return 1;

    } else {
        return 0;
    }
}
