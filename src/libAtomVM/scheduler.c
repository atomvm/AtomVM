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
#include "scheduler.h"
#include "sys.h"

#include "time.h"

static void scheduler_timeout_callback(void *data);
static void scheduler_execute_native_handlers(GlobalContext *global);

Context *scheduler_wait(GlobalContext *global, Context *c, int timeout)
{
    #ifdef DEBUG_PRINT_READY_PROCESSES
        debug_print_processes_list(global->ready_processes);
    #endif
    scheduler_make_waiting(global, c);

    if (timeout != -1) {
        EventListener *listener = malloc(sizeof(EventListener));
        linkedlist_append(&global->listeners, &listener->listeners_list_head);
        listener->fd = -1;

        listener->expires = 1;
        sys_set_timestamp_from_relative_to_abs(&listener->expiral_timestamp, timeout);
        listener->data = c;
        listener->handler = scheduler_timeout_callback;
    }

    scheduler_execute_native_handlers(global);

    //TODO: it would be better to check also events here
    if (!global->ready_processes) {
        if (!global->listeners) {
            fprintf(stderr, "Application hang detected. Aborting.");
            abort();
        }

        sys_waitevents(global->listeners);
    }

    struct ListHead *next_ready = global->ready_processes;
    linkedlist_remove(&global->ready_processes, next_ready);
    linkedlist_append(&global->ready_processes, next_ready);

    return GET_LIST_ENTRY(next_ready, Context, processes_list_head);
}

void scheduler_make_ready(GlobalContext *global, Context *c)
{
    linkedlist_remove(&global->waiting_processes, &c->processes_list_head);
    linkedlist_append(&global->ready_processes, &c->processes_list_head);
}

void scheduler_make_waiting(GlobalContext *global, Context *c)
{
    linkedlist_remove(&global->ready_processes, &c->processes_list_head);
    linkedlist_append(&global->waiting_processes, &c->processes_list_head);
}

static void scheduler_timeout_callback(void *data)
{
    EventListener *listener = (EventListener *) data;
    Context *c = (Context *) listener->data;
    linkedlist_remove(&c->global->listeners, &listener->listeners_list_head);
    scheduler_make_ready(c->global, c);

    free(listener);
}

static void scheduler_execute_native_handlers(GlobalContext *global)
{
    Context *contexts = GET_LIST_ENTRY(global->ready_processes, Context, processes_list_head);
    if (!contexts) {
        return;
    }

    Context *context = contexts;
    Context *next_context;
    do {
        next_context = GET_LIST_ENTRY(context->processes_list_head.next, Context, processes_list_head);

        if (context->native_handler) {
            context->native_handler(context);
            scheduler_make_waiting(global, context);
        }

        context = next_context;
    } while (context != contexts);
}

int schudule_processes_count(GlobalContext *global)
{
    if (!global->processes_table) {
        return 0;
    }

    int count = 0;

    Context *contexts = GET_LIST_ENTRY(global->processes_table, Context, processes_list_head);
    Context *context = contexts;
    do {
        context = GET_LIST_ENTRY(context->processes_list_head.next, Context, processes_list_head);
        count++;
    } while (context != contexts);

    return count;
}
