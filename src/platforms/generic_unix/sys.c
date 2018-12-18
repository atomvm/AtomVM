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
#include "iff.h"
#include "mapped_file.h"
#include "scheduler.h"
#include "socket.h"
#include "gpio_driver.h"
#include "network_driver.h"
#include "utils.h"

#include <limits.h>
#include <poll.h>
#include <stdint.h>
#include <time.h>
#include <unistd.h>

//#define ENABLE_TRACE

#ifndef TRACE
    #ifdef ENABLE_TRACE
        #define TRACE printf
    #else
        #define TRACE(...)
    #endif
#endif

static int32_t timespec_diff_to_ms(struct timespec *timespec1, struct timespec *timespec2);

extern void sys_waitevents(struct ListHead *listeners_list)
{
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);

    EventListener *listeners = GET_LIST_ENTRY(listeners_list, EventListener, listeners_list_head);
    EventListener *last_listener = GET_LIST_ENTRY(listeners_list->prev, EventListener, listeners_list_head);

    int min_timeout = INT_MAX;
    int count = 0;

    //first: find maximum allowed sleep time, and count file descriptor listeners
    EventListener *listener = listeners;
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
        }

        listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
    } while (listener != listeners);

    //second: use either poll or nanosleep
    if (count > 0) {
        struct pollfd *fds = calloc(count, sizeof(struct pollfd));
        if (IS_NULL_PTR(fds)) {
            fprintf(stderr, "Cannot allocate memory for pollfd, aborting.\n");
            abort();
        }
        int poll_fd_index = 0;

        //build pollfd array
        EventListener *listener = listeners;
        do {
            if (listener->fd >= 0) {
                fds[poll_fd_index].fd = listener->fd;
                fds[poll_fd_index].events = POLLIN;
                fds[poll_fd_index].revents = 0;
            }

            poll_fd_index++;

            listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
        } while (listener != listeners);

        poll(fds, poll_fd_index, min_timeout);

        //check which event happened
        listener = listeners;
        do {
            EventListener *next_listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
            for (int i = 0; i < poll_fd_index; i++) {
                if ((fds[i].fd == listener->fd) && (fds[i].revents & fds[i].events)) {
                    //it is completely safe to free a listener in the callback, we are going to not use it after this call
                    listener->handler(listener);
                }
            }

            listener = next_listener;
        } while (listener != listeners);

        free(fds);

    //just need to wait for a certain timespan
    } else {
        struct timespec t;
        t.tv_sec = min_timeout / 1000;
        t.tv_nsec = (min_timeout % 1000) * 1000000;

        struct timespec rem;
        int nanosleep_result = nanosleep(&t, &rem);
        while (nanosleep_result == -1) {
            nanosleep_result = nanosleep(&rem, &rem);
        }
    }

    //third: execute handlers for expiered timers
    if (min_timeout != INT_MAX) {
        listener = listeners;
        clock_gettime(CLOCK_MONOTONIC, &now);
        do {
            EventListener *next_listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
            if (listener->expires) {
                int wait_ms = timespec_diff_to_ms(&listener->expiral_timestamp, &now);
                if (wait_ms <= 0) {
                    //it is completely safe to free a listener in the callback, we are going to not use it after this call
                    listener->handler(listener);
                    //TODO check if one shot
                }
            }

            listener = next_listener;
        } while (listener != last_listener);
    }
}

extern void sys_set_timestamp_from_relative_to_abs(struct timespec *t, int32_t millis)
{
    if (UNLIKELY(clock_gettime(CLOCK_MONOTONIC, t))) {
        fprintf(stderr, "Failed clock_gettime.\n");
        abort();
    }
    t->tv_sec += millis / 1000;
    t->tv_nsec += (millis % 1000) * 1000000;
}

void sys_time(struct timespec *t)
{
    if (UNLIKELY(clock_gettime(CLOCK_REALTIME, t))) {
        fprintf(stderr, "Failed clock_gettime.\n");
        abort();
    }
}

Module *sys_load_module(GlobalContext *global, const char *module_name)
{
    TRACE("sys_load_module: Going to load: %s\n", module_name);

    const void *beam_module = NULL;
    uint32_t beam_module_size = 0;

    MappedFile *beam_file = NULL;
    if (!(global->avmpack_data && avmpack_find_section_by_name(global->avmpack_data, module_name, &beam_module, &beam_module_size))) {
        beam_file = mapped_file_open_beam(module_name);
        if (UNLIKELY(!iff_is_valid_beam(beam_file->mapped))) {
            fprintf(stderr, "%s is not a valid BEAM file.\n", module_name);
        }
        beam_module = beam_file->mapped;
        beam_module_size = beam_file->size;
    }

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    if (IS_NULL_PTR(new_module)) {
        return NULL;
    }
    new_module->module_platform_data = beam_file;

    return new_module;
}

static int32_t timespec_diff_to_ms(struct timespec *timespec1, struct timespec *timespec2)
{
    return (timespec1->tv_sec - timespec2->tv_sec) * 1000 + (timespec1->tv_nsec - timespec2->tv_nsec) / 1000000;
}

void sys_platform_periodic_tasks()
{
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    Context *new_ctx = context_new(glb);
    
    if (!strcmp(driver_name, "socket")) {
        socket_init(new_ctx, opts);
    } else if (!strcmp(driver_name, "network")) {
        networkdriver_init(new_ctx);
    } else if (!strcmp(driver_name, "gpio")) {
        gpiodriver_init(new_ctx);
    } else {
        context_destroy(new_ctx);
        return NULL;
    }
    
    return new_ctx;
}
