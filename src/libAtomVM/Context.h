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

#ifndef _CONTEXT_H_
#define _CONTEXT_H_

#include "linkedlist.h"
#include "globalcontext.h"
#include "Term.h"

#define DEFAULT_STACK_SIZE 32

struct Module;

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

struct Context
{
    struct ListHead processes_list_head;

    struct ListHead processes_table_head;
    int32_t process_id;

    term x[16];

    term *stack;
    unsigned long stack_size;
    term *e;

    unsigned long cp;

    unsigned long saved_ip;

    struct ListHead *mailbox;

    GlobalContext *global;
};

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

extern Context *context_new(GlobalContext *glb);
extern int context_execute_loop(Context *ctx, Module *mod, uint8_t *beam_file);

#endif
