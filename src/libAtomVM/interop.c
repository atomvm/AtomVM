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

#include "interop.h"

char *interop_term_to_string(term t)
{
    if (term_is_nonempty_list(t)) {
        return interop_list_to_string(t);

    } else if (term_is_binary(t)) {
        return interop_binary_to_string(t);

    } else {
        //TODO: implement also for other types?
        return NULL;
    }
}

char *interop_binary_to_string(term binary)
{
    int len = term_binary_size(binary);

    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }
    memcpy(str, term_binary_data(binary), len);

    str[len] = 0;

    return str;
}


char *interop_list_to_string(term list)
{
    int len = 0;

    term t = list;

    while (!term_is_nil(t)) {
        len++;
        term *t_ptr = term_get_list_ptr(t);
        t = *t_ptr;
    }

    t = list;
    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }

    for (int i = 0; i < len; i++) {
        term *t_ptr = term_get_list_ptr(t);
        str[i] = (char) term_to_int32(t_ptr[1]);
        t = *t_ptr;
    }
    str[len] = 0;

    return str;
}

term interop_proplist_get_value(term list, term key)
{
    term t = list;

    while (!term_is_nil(t)) {
        term *t_ptr = term_get_list_ptr(t);

        term head = t_ptr[1];
        if (term_get_tuple_element(head, 0) == key) {
            return term_get_tuple_element(head, 1);
        }

        t = *t_ptr;
    }

    return term_nil();
}
