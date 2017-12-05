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

#include "debug.h"

void debug_dump_stack(Context *ctx)
{
    fprintf(stderr, "\n");
    for (unsigned int i = 0; i < ctx->stack_size; i++) {
        fprintf(stderr, "DEBUG: stack: (%i) %lx\n", i, ctx->stack[i]);
    }
    fprintf(stderr, "DEBUG: \n");
    fprintf(stderr, "DEBUG: e: %li\n", ctx->e - ctx->stack);
}

char reg_type_c(int reg_type)
{
    switch (reg_type) {
        case 2:
            return 'a';

        case 3:
            return 'x';

        case 4:
            return 'y';

        default:
            return '?';
    }
}
