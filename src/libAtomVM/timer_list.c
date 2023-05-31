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

#include "timer_list.h"

void timer_list_next(struct TimerList *tw, uint64_t now, timer_list_callback_t cb)
{
    if (tw->timers == 0 || now < tw->next_timer) {
        return;
    }
    struct ListHead *item;
    struct ListHead *tmp;
    uint64_t next_timer = 0;
    MUTABLE_LIST_FOR_EACH (item, tmp, &tw->head) {
        struct TimerListItem *ti = GET_LIST_ENTRY(item, struct TimerListItem, head);
        if (ti->expiry_time <= now) {
            tw->timers--;
            list_remove(item);
            list_init(item);
            cb(ti);
        } else if (next_timer == 0 || ti->expiry_time < next_timer) {
            next_timer = ti->expiry_time;
        }
    }
    tw->next_timer = next_timer;
}
