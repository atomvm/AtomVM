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

#include "Context.h"

#include "globalcontext.h"

#define IMPL_EXECUTE_LOOP
#include "opcodesswitch.h"
#undef IMPL_EXECUTE_LOOP

Context *context_new(GlobalContext *glb)
{
    Context *ctx = malloc(sizeof(Context));
    ctx->cp = (unsigned long) -1;

    ctx->stack = (term *) calloc(DEFAULT_STACK_SIZE, sizeof(term));
    ctx->stack_size = DEFAULT_STACK_SIZE;
    ctx->e = ctx->stack + ctx->stack_size;

    linkedlist_append(&glb->ready_processes, &ctx->processes_list_head);

    ctx->mailbox = NULL;

    ctx->global = glb;

    ctx->process_id = globalcontext_get_new_process_id(glb);
    linkedlist_append(&glb->processes_table, &ctx->processes_table_head);

    return ctx;
}

void context_destroy(Context *ctx)
{
    linkedlist_remove(&ctx->global->processes_table, &ctx->processes_table_head);

    free(ctx->stack);
    free(ctx);
}
