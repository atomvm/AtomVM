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

#include "sys.h"

#include "avmpack.h"
#include "scheduler.h"
#include "globalcontext.h"
#include "socket.h"
#include "gpio_driver.h"
#include "network.h"

#include "freertos/FreeRTOS.h"
#include "esp_system.h"
#include "esp_event.h"
#include "esp_event_loop.h"
#include <limits.h>
#include <stdint.h>
#include <posix/sys/socket.h>

static inline void sys_clock_gettime(struct timespec *t)
{
    TickType_t ticks = xTaskGetTickCount();
    t->tv_sec = (ticks * portTICK_PERIOD_MS) / 1000;
    t->tv_nsec = ((ticks * portTICK_PERIOD_MS) % 1000) * 1000000;
}

static int32_t timespec_diff_to_ms(struct timespec *timespec1, struct timespec *timespec2)
{
    return (timespec1->tv_sec - timespec2->tv_sec) * 1000 + (timespec1->tv_nsec - timespec2->tv_nsec) / 1000000;
}

extern void sys_waitevents(GlobalContext *glb)
{
    struct ListHead *listeners_list = glb->listeners;
    struct timespec now;
    sys_clock_gettime(&now);

    EventListener *listeners = GET_LIST_ENTRY(listeners_list, EventListener, listeners_list_head);

    int min_timeout = INT_MAX;
    int count = 0;

    //
    // First: Find maximum allowed sleep time, and count file descriptor listeners,
    // the max FD, and set the read FD set
    //
    EventListener *listener = listeners;
    int max_fd = 0;
    fd_set read_fds;
    FD_ZERO(&read_fds);
    do {
        if (listener->expires) {
            int wait_ms = timespec_diff_to_ms(&listener->expiral_timestamp, &now);
            if (wait_ms <= 0) {
                min_timeout = 0;
            } else if (min_timeout > wait_ms) {
                min_timeout = wait_ms;
            }
        }
        if (listener->fd >= 0) {
            count++;
            FD_SET(listener->fd, &read_fds);
            if (listener->fd >= max_fd) {
                max_fd = listener->fd;
            }
        }
        listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
    } while (listener != listeners);

    if (count > 0) {
        //
        // Is anyone ready to read?
        //
        struct timeval select_timeout;
        select_timeout.tv_sec = 0; // min_timeout / 1000;
        select_timeout.tv_usec = 10000; // (min_timeout % 1000) * 1000000;
        select(max_fd + 1, &read_fds, NULL, NULL, &select_timeout);
        //
        // If so, call the handler
        //
        listener = listeners;
        do {
            EventListener *next_listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
            if (listener->fd >= 0) {
                if (FD_ISSET(listener->fd, &read_fds)) {
                    listener->handler(listener);
                }
            }
            listener = next_listener;
            listeners = GET_LIST_ENTRY(glb->listeners, EventListener, listeners_list_head);
        } while (listener != listeners);
    } else {
        vTaskDelay(min_timeout / portTICK_PERIOD_MS);
    }

    //second: execute handlers for expiered timers
    if (min_timeout != INT_MAX) {
        listener = listeners;
        sys_clock_gettime(&now);
        do {
            EventListener *next_listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
            if (listener->expires) {
                int wait_ms = timespec_diff_to_ms(&listener->expiral_timestamp, &now);
                if (wait_ms <= 0) {
                    //it is completely safe to free a listener in the callback, we are going to not use it after this call
                    listener->handler(listener);
                }
            }

            listener = next_listener;
            listeners = GET_LIST_ENTRY(glb->listeners, EventListener, listeners_list_head);
        } while (listener != listeners);
    }
}

extern void sys_set_timestamp_from_relative_to_abs(struct timespec *t, int32_t millis)
{
    sys_clock_gettime(t);
    t->tv_sec += millis / 1000;
    t->tv_nsec += (millis % 1000) * 1000000;
}

void sys_time(struct timespec *t)
{
    struct timeval tv;
    if (UNLIKELY(gettimeofday(&tv, NULL))) {
        fprintf(stderr, "Failed gettimeofday.\n");
        abort();
    }

    t->tv_sec = tv.tv_sec;
    t->tv_nsec = tv.tv_usec * 1000;
}

Module *sys_load_module(GlobalContext *global, const char *module_name)
{
    const void *beam_module = NULL;
    uint32_t beam_module_size = 0;

    if (!(global->avmpack_data && avmpack_find_section_by_name(global->avmpack_data, module_name, &beam_module, &beam_module_size))) {
        fprintf(stderr, "Failed to open module: %s\n", module_name);
        return NULL;
    }

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    new_module->module_platform_data = NULL;

    return new_module;
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    Context *new_ctx = context_new(glb);

    if (!strcmp(driver_name, "socket")) {
        socket_init(new_ctx, opts);
    } else if (!strcmp(driver_name, "network")) {
        network_init(new_ctx, opts);
    } else if (!strcmp(driver_name, "gpio")) {
        gpiodriver_init(new_ctx);
    } else {
        context_destroy(new_ctx);
        return NULL;
    }

    return new_ctx;
}

