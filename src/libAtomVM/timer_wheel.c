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

#include "timer_wheel.h"

struct TimerWheel *timer_wheel_new(int slots_count)
{
    struct TimerWheel *tw = malloc(sizeof(struct TimerWheel));
    tw->slots = malloc(sizeof(struct ListHead) * slots_count);
    for (int i = 0; i < slots_count; i++) {
        list_init(&tw->slots[i]);
    }
    tw->slots_count = slots_count;
    tw->timers = 0;
    tw->monotonic_time = 0;

    return tw;
}

void timer_wheel_tick(struct TimerWheel *tw)
{
    tw->monotonic_time++;
    uint64_t monotonic_time = tw->monotonic_time;
    int pos = tw->monotonic_time % tw->slots_count;

    struct ListHead *item;
    struct ListHead *tmp;
    MUTABLE_LIST_FOR_EACH(item, tmp, &tw->slots[pos]) {
        struct TimerWheelItem *ti = GET_LIST_ENTRY(item, struct TimerWheelItem, head);
        if (ti->expiry_time <= monotonic_time) {
            tw->timers--;
            list_remove(item);
            ti->callback(ti);
        }
    }
}
