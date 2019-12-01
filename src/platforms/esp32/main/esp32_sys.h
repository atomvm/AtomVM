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

#ifndef _ESP32_SYS_H_
#define _ESP32_SYS_H_

#include "freertos/FreeRTOS.h"
#include <freertos/queue.h>

#include <time.h>

#define EVENT_DESCRIPTORS_COUNT 16

typedef struct EventListener EventListener;

typedef void (*event_handler_t)(EventListener *listener);

struct EventListener {
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

extern xQueueHandle event_queue;
void esp32_sys_queue_init();

void sys_event_listener_init(EventListener *listener, void *sender, event_handler_t handler, void *data);

void socket_init(Context *ctx, term opts);

#endif
