/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Davide Bettio <davide@uninstall.it>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifndef _TIMER_WHEEL_H_
#define _TIMER_WHEEL_H_

#include <stdbool.h>
#include <stdint.h>

#include "list.h"

struct TimerWheelItem;
typedef void(timer_wheel_callback_t)(struct TimerWheelItem *);

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
