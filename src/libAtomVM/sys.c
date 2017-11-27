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

#include "scheduler.h"

#include <limits.h>
#include <poll.h>
#include <stdint.h>
#include <time.h>
#include <unistd.h>

static int32_t timespec_diff_to_ms(struct timespec *timespec1, struct timespec *timespec2);

extern void sys_waitevents(struct ListHead *listeners_list)
{
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);

    EventListener *listeners = LIST_ENTRY(listeners_list, EventListener, listeners_list_head);

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

        listener = LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
    } while (listener != listeners);

    //second: use either poll or nanosleep
    if (count > 0) {
        struct pollfd *fds = calloc(count, sizeof(struct pollfd));
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

            listener = LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
        } while (listener != listeners);

        poll(fds, poll_fd_index, min_timeout);

        //check which event happened
        listener = listeners;
        do {
            EventListener *next_listener = LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
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
            EventListener *next_listener = LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
            if (listener->expires) {
                int wait_ms = timespec_diff_to_ms(&listener->expiral_timestamp, &now);
                if (wait_ms <= 0) {
                    //it is completely safe to free a listener in the callback, we are going to not use it after this call
                    listener->handler(listener);
                }
            }

            listener = next_listener;
        } while (listener != listeners);
    }
}

extern void sys_set_timestamp_from_relative_to_abs(struct timespec *t, int32_t millis)
{
    clock_gettime(CLOCK_MONOTONIC, t);
    t->tv_sec += millis / 1000;
    t->tv_nsec += (millis % 1000) * 1000000;
}

static int32_t timespec_diff_to_ms(struct timespec *timespec1, struct timespec *timespec2)
{
    return (timespec1->tv_sec - timespec2->tv_sec) * 1000 + (timespec1->tv_nsec - timespec2->tv_nsec) / 1000000;
}
