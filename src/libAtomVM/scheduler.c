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
#include "debug.h"
#include "list.h"
#include "sys.h"
#include "utils.h"

static void scheduler_execute_native_handlers(GlobalContext *global);

static void update_timer_wheel(GlobalContext *global)
{
    struct TimerWheel *tw = global->timer_wheel;
    uint32_t last_seen_millis = global->last_seen_millis;

    if (timer_wheel_is_empty(tw)) {
        sys_stop_millis_timer();
        return;
    }

    uint32_t millis_now = sys_millis();

    if (millis_now < last_seen_millis) {
        for (uint32_t i = last_seen_millis; i < UINT32_MAX; i++) {
            timer_wheel_tick(tw);
        }
    }
    for (uint32_t i = last_seen_millis; i < millis_now; i++) {
        timer_wheel_tick(tw);
    }
    global->last_seen_millis = millis_now;
}

Context *scheduler_wait(GlobalContext *global, Context *c)
{
    #ifdef DEBUG_PRINT_READY_PROCESSES
        debug_print_processes_list(global->ready_processes);
    #endif
    scheduler_make_waiting(global, c);

    return scheduler_do_wait(global);
}

Context *scheduler_do_wait(GlobalContext *global)
{
    do {
        update_timer_wheel(global);
        sys_consume_pending_events(global);
        scheduler_execute_native_handlers(global);

        update_timer_wheel(global);
        if (list_is_empty(&global->ready_processes)) {
            sys_sleep(global);
        }
    } while (list_is_empty(&global->ready_processes));

    struct ListHead *next_ready = list_first(&global->ready_processes);
    list_remove(next_ready);
    list_append(&global->ready_processes, next_ready);

    return GET_LIST_ENTRY(next_ready, Context, processes_list_head);
}

static inline void scheduler_execute_native_handler(GlobalContext *global, Context *c)
{
    scheduler_make_waiting(global, c);
    // context might terminate itself
    // so call to native_handler must be the last action here.
    c->native_handler(c);
}

Context *scheduler_next(GlobalContext *global, Context *c)
{
    c->reductions += DEFAULT_REDUCTIONS_AMOUNT;

    update_timer_wheel(global);

    sys_consume_pending_events(global);

    //TODO: improve scheduling here
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, &global->ready_processes) {
        Context *next_context = GET_LIST_ENTRY(item, Context, processes_list_head);
        if (next_context->native_handler) {
            scheduler_execute_native_handler(global, next_context);

        } else if (!next_context->native_handler && (next_context != c)) {
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

static void scheduler_timeout_callback(struct TimerWheelItem *it)
{
    timer_wheel_item_init(it, NULL, 0);
    Context *ctx = GET_LIST_ENTRY(it, Context, timer_wheel_head);
    ctx->flags = (ctx->flags | WaitingTimeoutExpired) & ~WaitingTimeout;
    scheduler_make_ready(ctx->global, ctx);
}

void scheduler_set_timeout(Context *ctx, uint32_t timeout)
{
    GlobalContext *glb = ctx->global;

    ctx->flags |= WaitingTimeout;

    struct TimerWheel *tw = glb->timer_wheel;

    if (timer_wheel_is_empty(tw)) {
        sys_start_millis_timer();
    }

    struct TimerWheelItem *twi = &ctx->timer_wheel_head;
    if (UNLIKELY(twi->callback)) {
        abort();
    }

    uint64_t expiry = timer_wheel_expiry_to_monotonic(tw, timeout);
    timer_wheel_item_init(twi, scheduler_timeout_callback, expiry);

    timer_wheel_insert(tw, twi);
}

void scheduler_cancel_timeout(Context *ctx)
{
    GlobalContext *glb = ctx->global;

    ctx->flags &= ~(WaitingTimeout | WaitingTimeoutExpired);

    struct TimerWheel *tw = glb->timer_wheel;
    struct TimerWheelItem *twi = &ctx->timer_wheel_head;
    if (twi->callback) {
        timer_wheel_remove(tw, twi);
        timer_wheel_item_init(twi, NULL, 0);
    }
}

static void scheduler_execute_native_handlers(GlobalContext *global)
{
    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH (item, tmp, &global->ready_processes) {
        Context *context = GET_LIST_ENTRY(item, Context, processes_list_head);

        if (context->native_handler) {
            scheduler_execute_native_handler(global, context);
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
