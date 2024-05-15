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

#ifndef _TIMER_LIST_H_
#define _TIMER_LIST_H_

#include <stdbool.h>
#include <stdint.h>

#include "list.h"

#ifdef __cplusplus
extern "C" {
#endif

struct TimerListItem;
typedef void(timer_list_callback_t)(struct TimerListItem *);

struct TimerList
{
    struct ListHead head;
    int timers;
    uint64_t next_timer;
};

struct TimerListItem
{
    uint64_t expiry_time;
    struct ListHead head;
};

static inline void timer_list_init(struct TimerList *tw)
{
    list_init(&tw->head);
    tw->timers = 0;
    tw->next_timer = 0;
}

static inline void timer_list_insert(struct TimerList *tw, struct TimerListItem *item)
{
    uint64_t expiry_time = item->expiry_time;

    if (tw->timers == 0 || expiry_time < tw->next_timer) {
        tw->next_timer = expiry_time;
    }

    tw->timers++;

    list_append(&tw->head, &item->head);
}

static inline void timer_list_remove(struct TimerList *tw, struct TimerListItem *item)
{
    if (item->head.next != &item->head) {
        tw->timers--;
        list_remove(&item->head);
        list_init(&item->head);
    }
}

static inline bool timer_list_is_empty(const struct TimerList *tw)
{
    return tw->timers == 0;
}

static inline int timer_list_timers_count(const struct TimerList *tw)
{
    return tw->timers;
}

static inline void timer_list_item_init(struct TimerListItem *it, uint64_t expiry)
{
    list_init(&it->head);
    it->expiry_time = expiry;
}

/**
 * @brief process the timer wheel, calling cb for every item that should be
 * fired (for which `expiry_time` <= `now`).
 *
 * @details The current algorithm is very basic. Under heavy load, two
 * processes scheduled for different timers might be triggered (same seems to
 * be true with BEAM). The only optimization is the next time is saved so the
 * function doesn't run every timer on every call.
 *
 * @param tw the timer wheel
 * @param now the current monotonic date
 * @param cb the callback
 */
void timer_list_next(struct TimerList *tw, uint64_t now, timer_list_callback_t cb);

#ifdef __cplusplus
}
#endif

#endif
