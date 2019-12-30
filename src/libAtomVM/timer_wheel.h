/***************************************************************************
 *   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 *
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

#ifndef _TIMER_WHEEL_H_
#define _TIMER_WHEEL_H_

#include <stdbool.h>
#include <stdint.h>

#include "list.h"

struct TimerWheelItem;
typedef void (timer_wheel_callback_t)(struct TimerWheelItem *);

struct TimerWheel
{
    struct ListHead *slots;
    int slots_count;
    int timers;
    uint64_t monotonic_time;
};

struct TimerWheelItem
{
    uint64_t expiry_time;
    struct ListHead head;
    timer_wheel_callback_t *callback;
};

struct TimerWheel *timer_wheel_new(int slots_count);
void timer_wheel_tick(struct TimerWheel *tw);

static inline void timer_wheel_insert(struct TimerWheel *tw, struct TimerWheelItem *item)
{
    uint64_t expiry_time = item->expiry_time;
    int slot = expiry_time % tw->slots_count;

    tw->timers++;

    list_append(&tw->slots[slot], &item->head);
}

static inline void timer_wheel_remove(struct TimerWheel *tw, struct TimerWheelItem *item)
{
    tw->timers--;

    list_remove(&item->head);
}

static inline bool timer_wheel_is_empty(const struct TimerWheel *tw)
{
    return tw->timers == 0;
}

static inline int timer_wheel_timers_count(const struct TimerWheel *tw)
{
    return tw->timers;
}

static inline void timer_wheel_item_init(struct TimerWheelItem *it, timer_wheel_callback_t cb, uint64_t expiry)
{
    it->expiry_time = expiry;
    it->callback = cb;
}

static inline uint64_t timer_wheel_expiry_to_monotonic(const struct TimerWheel *tw, uint32_t expiry)
{
    return tw->monotonic_time + expiry;
}

#endif
