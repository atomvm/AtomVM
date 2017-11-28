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

#include "scheduler.h"
#include "sys.h"

#include "time.h"

static void scheduler_timeout_callback(void *data);

Context *scheduler_wait(GlobalContext *global, Context *c, int timeout)
{
    linkedlist_remove(&global->ready_processes, &c->processes_list_head);
    linkedlist_append(&global->waiting_processes, &c->processes_list_head);

    if (timeout != -1) {
        EventListener *listener = malloc(sizeof(EventListener));
        linkedlist_append(&global->listeners, &listener->listeners_list_head);
        listener->fd = -1;

        listener->expires = 1;
        sys_set_timestamp_from_relative_to_abs(&listener->expiral_timestamp, timeout);
        listener->data = c;
        listener->handler = scheduler_timeout_callback;
    }

    if (!global->listeners) {
        fprintf(stderr, "Application hang detected. Aborting.");
        abort();
    }

    //TODO: it would be better to check also events here
    if (!global->ready_processes) {
        sys_waitevents(global->listeners);
    }

    struct ListHead *next_ready = global->ready_processes;
    linkedlist_remove(&global->ready_processes, next_ready);
    linkedlist_append(&global->ready_processes, next_ready);

    return LIST_ENTRY(next_ready, Context, processes_list_head);
}

void scheduler_make_ready(GlobalContext *global, Context *c)
{
    linkedlist_remove(&global->waiting_processes, &c->processes_list_head);
    linkedlist_append(&global->ready_processes, &c->processes_list_head);
}

static void scheduler_timeout_callback(void *data)
{
    EventListener *listener = (EventListener *) data;
    Context *c = (Context *) listener->data;
    linkedlist_remove(&c->global->listeners, &listener->listeners_list_head);
    scheduler_make_ready(c->global, c);

    free(listener);
}
