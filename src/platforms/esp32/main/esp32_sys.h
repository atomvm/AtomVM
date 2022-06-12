/*
 * This file is part of AtomVM.
 *
 * Copyright 2019-2022 Davide Bettio <davide@uninstall.it>
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

#ifndef _ESP32_SYS_H_
#define _ESP32_SYS_H_

#include "freertos/FreeRTOS.h"
#include <freertos/queue.h>

#include <time.h>

#define EVENT_DESCRIPTORS_COUNT 16

typedef struct EventListener EventListener;

typedef void (*event_handler_t)(EventListener *listener);

struct EventListener
{
    struct ListHead listeners_list_head;

    event_handler_t handler;
    void *data;
    void *sender;
};

struct ESP32PlatformData
{
    struct ListHead listeners;
    struct ListHead sockets_list_head;
};

extern QueueSetHandle_t event_set;
extern xQueueHandle event_queue;
void esp32_sys_queue_init();

void sys_event_listener_init(EventListener *listener, void *sender, event_handler_t handler, void *data);

void socket_init(Context *ctx, term opts);

#endif
