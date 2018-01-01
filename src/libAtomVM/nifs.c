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

#include "nifs.h"

#include "utils.h"

#include <stdio.h>
#include <string.h>

#define MAX_NIF_NAME_LEN 32

const struct Nif *nifs_get(AtomString module, AtomString function, int arity)
{
    char nifname[MAX_NIF_NAME_LEN];

    int module_name_len = atom_string_len(module);
    memcpy(nifname, atom_string_data(module), module_name_len);

    nifname[module_name_len] = ':';

    int function_name_len = atom_string_len(function);
    memcpy(nifname + module_name_len + 1, atom_string_data(function), function_name_len);

    nifname[module_name_len + function_name_len + 1] = '\\';
    nifname[module_name_len + function_name_len + 2] = '0' + arity;
    nifname[module_name_len + function_name_len + 3] = 0;

    return NULL;
}
