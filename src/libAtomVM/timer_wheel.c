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

#include "timer_wheel.h"
#include "utils.h"

struct TimerWheel *timer_wheel_new(int slots_count)
{
    struct TimerWheel *tw = malloc(sizeof(struct TimerWheel));
    if (IS_NULL_PTR(tw)) {
        return NULL;
    }
    tw->slots = malloc(sizeof(struct ListHead) * slots_count);
    if (IS_NULL_PTR(tw->slots)) {
        free(tw);
        return NULL;
    }

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
    MUTABLE_LIST_FOR_EACH (item, tmp, &tw->slots[pos]) {
        struct TimerWheelItem *ti = GET_LIST_ENTRY(item, struct TimerWheelItem, head);
        if (ti->expiry_time <= monotonic_time) {
            tw->timers--;
            list_remove(item);
            ti->callback(ti);
        }
    }
}
