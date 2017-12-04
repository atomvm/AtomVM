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

#ifndef _MODULE_H_
#define _MODULE_H_

#include <stdint.h>

#include "Context.h"

typedef void (*BifImpl)();
typedef void (*BifImpl0)(Context *ctx, int reg);
typedef void (*BifImpl2)(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2, int reg);

typedef struct
{
    char magic[4];
    uint32_t size;
    uint32_t info_size;
    uint32_t version;
    uint32_t opcode_max;
    uint32_t labels;
    uint32_t functions_count;

    uint8_t code[1];
} __attribute__((packed)) CodeChunk;

struct Module
{
    CodeChunk *code;
    void *export_table;
    void *atom_table;

    BifImpl *imported_bifs;
    void *local_labels;

    void **labels;
};

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

extern void module_build_imported_functions_table(Module *this_module, uint8_t *table_data, uint8_t *atom_tab);
extern void module_destroy(Module *module);
extern void module_add_label(Module *mod, int index, void *ptr);
extern Module *module_new_from_iff_binary(void *iff_binary, unsigned long size);

#endif
