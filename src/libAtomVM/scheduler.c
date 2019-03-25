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
#include "list.h"
#include "scheduler.h"
#include "sys.h"
#include "utils.h"

#include "time.h"

static void scheduler_timeout_callback(void *data);
static void scheduler_execute_native_handlers(GlobalContext *global);
static Context *scheduler_get_expired_before(const GlobalContext *global, const struct timespec *before_timestamp);
static int scheduler_find_min_timeout(const GlobalContext *global, struct timespec *found_timeout);
static inline int before_than(const struct timespec *a, const struct timespec *b);
static int make_ready_expired_contexts(GlobalContext *global);

Context *scheduler_wait(GlobalContext *global, Context *c)
{
    #ifdef DEBUG_PRINT_READY_PROCESSES
        debug_print_processes_list(global->ready_processes);
    #endif
    scheduler_make_waiting(global, c);

    sys_platform_periodic_tasks();

    do {
        struct timespec next_timeout;
        next_timeout.tv_sec = global->next_timeout_at.tv_sec;
        next_timeout.tv_nsec = global->next_timeout_at.tv_nsec;

        if  (next_timeout.tv_sec | next_timeout.tv_nsec) {
            struct timespec now_timestamp;
            sys_set_timestamp_from_relative_to_abs(&now_timestamp, 0);

            if (before_than(&next_timeout, &now_timestamp)) {

                Context *expired_ctx = scheduler_get_expired_before(global, &now_timestamp);
                if (UNLIKELY(!expired_ctx)) {
                    fprintf(stderr, "Timeout without any expired context, aborting.\n");
                    abort();
                }
                scheduler_make_ready(global, expired_ctx);
                if (!scheduler_find_min_timeout(global, &global->next_timeout_at)) {
                    global->next_timeout_at.tv_sec = 0;
                    global->next_timeout_at.tv_nsec = 0;
                }

            } else if (list_is_empty(&global->ready_processes)) {

                EventListener *listener = malloc(sizeof(EventListener));
                if (IS_NULL_PTR(listener)) {
                    fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                    abort();
                }
                linkedlist_append(&global->listeners, &listener->listeners_list_head);
                listener->fd = -1;

                listener->expires = 1;
                listener->expiral_timestamp.tv_sec = next_timeout.tv_sec;
                listener->expiral_timestamp.tv_nsec = next_timeout.tv_nsec;
                listener->one_shot = 1;
                listener->data = global;
                listener->handler = scheduler_timeout_callback;

                sys_waitevents(global);
            }
        } else if (list_is_empty(&global->ready_processes)) {
            if (LIKELY(global->listeners)) {
                sys_waitevents(global);
            } else {
                fprintf(stderr, "Hang detected\n");
                abort();
            }
        }

        scheduler_execute_native_handlers(global);
    } while (list_is_empty(&global->ready_processes));

    struct ListHead *next_ready = list_first(&global->ready_processes);
    list_remove(next_ready);
    list_append(&global->ready_processes, next_ready);

    return GET_LIST_ENTRY(next_ready, Context, processes_list_head);
}

Context *scheduler_next(GlobalContext *global, Context *c)
{
    sys_platform_periodic_tasks();

    UNUSED(global);
    c->reductions += DEFAULT_REDUCTIONS_AMOUNT;

    if (global->next_timeout_at.tv_sec | global->next_timeout_at.tv_nsec) {
        make_ready_expired_contexts(global);
    }

    //TODO: improve scheduling here
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH(item, tmp, &global->ready_processes) {
        Context *next_context = GET_LIST_ENTRY(item, Context, processes_list_head);
        if (!next_context->native_handler && (next_context != c)) {
            return next_context;
        }
    }

    return c;
}

void scheduler_make_ready(GlobalContext *global, Context *c)
{
    list_remove(&c->processes_list_head);
    list_append(&global->ready_processes, &c->processes_list_head);
}

void scheduler_make_waiting(GlobalContext *global, Context *c)
{
    list_remove(&c->processes_list_head);
    list_append(&global->waiting_processes, &c->processes_list_head);
}

void scheduler_terminate(Context *c)
{
    list_remove(&c->processes_list_head);
    if (!c->leader) {
        context_destroy(c);
    }
}

static int make_ready_expired_contexts(GlobalContext *global)
{
    struct timespec now_timestamp;
    sys_set_timestamp_from_relative_to_abs(&now_timestamp, 2);

    Context *expired_ctx = scheduler_get_expired_before(global, &now_timestamp);
    if (!expired_ctx) {
        return 0;
    }

    scheduler_make_ready(global, expired_ctx);

    if (!scheduler_find_min_timeout(global, &global->next_timeout_at)) {
        global->next_timeout_at.tv_sec = 0;
        global->next_timeout_at.tv_nsec = 0;
    }

    return 1;
}

void scheduler_set_timeout(Context *ctx, uint32_t timeout)
{
    GlobalContext *glb = ctx->global;

    struct timespec new_timeout;
    sys_set_timestamp_from_relative_to_abs(&new_timeout, timeout);
    ctx->timeout_at.tv_sec = new_timeout.tv_sec;
    ctx->timeout_at.tv_nsec = new_timeout.tv_nsec;

    struct timespec next_timeout;
    next_timeout.tv_sec = glb->next_timeout_at.tv_sec;
    next_timeout.tv_nsec = glb->next_timeout_at.tv_nsec;

    if ((next_timeout.tv_sec == 0) && (next_timeout.tv_nsec == 0)) {
        glb->next_timeout_at.tv_nsec = new_timeout.tv_nsec;
        glb->next_timeout_at.tv_sec = new_timeout.tv_sec;

    } else if (before_than(&new_timeout, &next_timeout)) {
        glb->next_timeout_at.tv_sec = new_timeout.tv_sec;
        glb->next_timeout_at.tv_nsec = new_timeout.tv_nsec;
    }
}

int scheduler_is_timeout_expired(const Context *ctx)
{
    struct timespec now_timestamp;
    sys_set_timestamp_from_relative_to_abs(&now_timestamp, 0);
    return before_than(&ctx->timeout_at, &now_timestamp);
}

static void scheduler_timeout_callback(void *data)
{
    EventListener *listener = (EventListener *) data;
    GlobalContext *global = (GlobalContext *) listener->data;
    linkedlist_remove(&global->listeners, &listener->listeners_list_head);

    make_ready_expired_contexts(global);
}

static inline int before_than(const struct timespec *a, const struct timespec *b)
{
    return (a->tv_sec < b->tv_sec) ||
        ((a->tv_sec == b->tv_sec) && (a->tv_nsec < b->tv_nsec));
}

static Context *scheduler_get_expired_before(const GlobalContext *global, const struct timespec *before_timestamp)
{
    struct timespec min;
    Context *min_context = NULL;

    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH(item, tmp, &global->waiting_processes) {
        Context *context = GET_LIST_ENTRY(item, Context, processes_list_head);

        if (context->timeout_at.tv_sec | context->timeout_at.tv_nsec) {
            if (!min_context || before_than(&context->timeout_at, &min)) {
                min.tv_sec = context->timeout_at.tv_sec;
                min.tv_nsec = context->timeout_at.tv_nsec;
                min_context = context;
            }
        }
    }

    if (min_context && before_than(&min_context->timeout_at, before_timestamp)) {
        return min_context;
    } else {
        return NULL;
    }
}

static int scheduler_find_min_timeout(const GlobalContext *global, struct timespec *found_timeout)
{
    struct timespec min;
    int found_first = 0;

    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH(item, tmp, &global->waiting_processes) {
        Context *context = GET_LIST_ENTRY(item, Context, processes_list_head);

        if (context->timeout_at.tv_sec | context->timeout_at.tv_nsec) {
            if (!found_first || before_than(&context->timeout_at, &min)) {
                min.tv_sec = context->timeout_at.tv_sec;
                min.tv_nsec = context->timeout_at.tv_nsec;
                found_first = 1;
            }
        }

    }

    if (found_first) {
        found_timeout->tv_sec = min.tv_sec;
        found_timeout->tv_nsec = min.tv_nsec;
    }

    return found_first;
}

static void scheduler_execute_native_handlers(GlobalContext *global)
{
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH(item, tmp, &global->ready_processes) {
        Context *context = GET_LIST_ENTRY(item, Context, processes_list_head);

        if (context->native_handler) {
            context->native_handler(context);
            scheduler_make_waiting(global, context);
        }
    }
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
