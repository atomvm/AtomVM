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
#include <stdio.h>

#include "utils.h"

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

void atom_write_mfa(char *buf, size_t buf_size, AtomString module, AtomString function, int arity)
{

    unsigned int arity_num_digits = 0;
    // Simple calculation because we don't expect arities greater than 255
    if (arity < 10) {
        arity_num_digits = 1;
    } else if (arity < 100) {
        arity_num_digits = 2;
    } else {
        arity_num_digits = 3;
    }

    unsigned int module_name_len = atom_string_len(module);
    memcpy(buf, atom_string_data(module), module_name_len);

    buf[module_name_len] = ':';

    unsigned int function_name_len = atom_string_len(function);
    if (UNLIKELY((module_name_len + function_name_len + 3 + arity_num_digits) > buf_size))
    {
        fprintf(stderr, "Insufficient room to write mfa.\n");
        abort();
    }
    memcpy(buf + module_name_len + 1, atom_string_data(function), function_name_len);

    // Calculate the offset so we can get the pointer to the following unfilled position in the buffer
    unsigned int offset = sizeof(char) * (module_name_len + function_name_len + 1);
    char *arity_offset_start = buf + offset;
    snprintf(arity_offset_start, arity_num_digits + 2, "/%u", arity);
}
