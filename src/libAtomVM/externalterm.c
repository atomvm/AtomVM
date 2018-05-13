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

#include "externalterm.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "utils.h"

#define EXTERNAL_TERM_TAG 131
#define STRING_EXT 107
#define BINARY_EXT 109

term externalterm_to_term(const void *external_term, Context *ctx)
{
    uint8_t *external_term_buf = (uint8_t *) external_term;

    if (UNLIKELY(external_term_buf[0] != EXTERNAL_TERM_TAG)) {
        fprintf(stderr, "External term format not supported\n");
        abort();
    }

    switch (external_term_buf[1]) {
        case STRING_EXT: {
            uint16_t string_size = READ_16_UNALIGNED(external_term_buf + 2);
            return term_from_string(external_term_buf + 4, string_size, ctx);
        }

        case BINARY_EXT: {
            uint32_t binary_size = READ_32_UNALIGNED(external_term_buf + 2);
            return term_from_literal_binary(external_term_buf + 6, binary_size, ctx);
        }

        default:
            fprintf(stderr, "Unknown term type: %i\n", (int) external_term_buf[1]);
            abort();
    }
}
