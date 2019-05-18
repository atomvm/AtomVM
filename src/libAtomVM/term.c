/***************************************************************************
 *   Copyright 2018,2019 by Davide Bettio <davide@uninstall.it>            *
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

#include "term.h"

#include "atom.h"
#include "context.h"
#include "interop.h"
#include "valueshashtable.h"

#include <ctype.h>
#include <stdio.h>

void term_display(FILE *fd, term t, const Context *ctx)
{
    if (term_is_atom(t)) {
        int atom_index = term_to_atom_index(t);
            AtomString atom_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table, atom_index, (unsigned long) NULL);
            fprintf(fd, "%.*s", (int) atom_string_len(atom_string), (char *) atom_string_data(atom_string));

    } else if (term_is_integer(t)) {
        long iv = term_to_int32(t);
        fprintf(fd, "%li", iv);

    } else if (term_is_nil(t)) {
        fprintf(fd, "[]");

    } else if (term_is_nonempty_list(t)) {
        int is_printable = 1;
        term list_item = t;
        while (!term_is_nil(list_item)) {
            term head = term_get_list_head(list_item);
            if (!term_is_integer(head) || !isprint(term_to_int32(head))) {
                is_printable = 0;
            }
            list_item = term_get_list_tail(list_item);
        }

        if (is_printable) {
            int ok;
            char *printable = interop_list_to_string(t, &ok);
            if (LIKELY(ok)) {
                fprintf(fd, "\"%s\"", printable);
                free(printable);
            } else {
                fprintf(fd, "???");
            }

        } else {
            fputc('[', fd);
            int display_separator = 0;
            while (!term_is_nil(t)) {
                if (display_separator) {
                    fputc(',', fd);
                } else {
                    display_separator = 1;
                }

                term_display(fd, term_get_list_head(t), ctx);
                t = term_get_list_tail(t);
            }
            fputc(']', fd);
        }
    } else if (term_is_pid(t)) {
        fprintf(fd, "<0.%i.0>", term_to_local_process_id(t));

    } else if (term_is_tuple(t)) {
        fputc('{', fd);

        int tuple_size = term_get_tuple_arity(t);
        for (int i = 0; i < tuple_size; i++) {
            if (i != 0) {
                fputc(',', fd);
            }
            term_display(fd, term_get_tuple_element(t, i), ctx);
        }

        fputc('}', fd);

    } else if (term_is_binary(t)) {
        int len = term_binary_size(t);
        const char *binary_data = term_binary_data(t);

        int is_printable = 1;
        for (int i = 0; i < len; i++) {
            if (!isprint(binary_data[i])) {
                is_printable = 0;
                break;
            }
        }

        fprintf(fd, "<<");
        if (is_printable) {
            fprintf(fd, "\"%.*s\"", len, binary_data);

        } else {
            int display_separator = 0;
            for (int i = 0; i < len; i++) {
                if (display_separator) {
                    fputc(',', fd);
                } else {
                    display_separator = 1;
                }

                fprintf(fd, "%i", binary_data[i]);
            }
        }
        fprintf(fd, ">>");

    } else if (term_is_reference(t)) {
        const char *format =
#ifdef __clang__
        "#Ref<0.0.0.%llu>";
#else
        "#Ref<0.0.0.%lu>";
#endif
        fprintf(fd, format, term_to_ref_ticks(t));
    } else {
        fprintf(fd, "Unknown term type: %li", t);
    }
}
