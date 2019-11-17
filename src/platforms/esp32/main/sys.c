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
#include "esp32_sys.h"

#include "avmpack.h"
#include "i2cdriver.h"
#include "scheduler.h"
#include "globalcontext.h"
#include "socket.h"
#include "gpio_driver.h"
#include "spidriver.h"
#include "network.h"
#include "defaultatoms.h"

#include "freertos/FreeRTOS.h"
#include "esp_system.h"
#include "esp_event.h"
#include "esp_event_loop.h"
#include <limits.h>
#include <stdint.h>
#include <posix/sys/socket.h>

#define EVENT_QUEUE_LEN 16

static const char *const esp_free_heap_size_atom = "\x14" "esp32_free_heap_size";
static const char *const esp_chip_info_atom = "\xF" "esp32_chip_info";
static const char *const esp_idf_version_atom = "\xF" "esp_idf_version";
static const char *const esp32_atom = "\x5" "esp32";

xQueueHandle event_queue = NULL;

void *event_descriptors[EVENT_DESCRIPTORS_COUNT];

int open_event_descriptor(void *ptr)
{
    for (int i = 0; i < EVENT_DESCRIPTORS_COUNT; i++) {
        if (!event_descriptors[i]) {
            event_descriptors[i] = ptr;
            return i;
        }
    }

    fprintf(stderr, "exausted descriptors\n");

    return -1;
}

void close_event_descriptor(int index)
{
    if (UNLIKELY(index >= EVENT_DESCRIPTORS_COUNT)) {
        fprintf(stderr, "tried to close invalid event descriptor\n");
        abort();
    }

    event_descriptors[index] = NULL;
}

int find_event_descriptor(void *ptr)
{
    for (int i = 0; i < EVENT_DESCRIPTORS_COUNT; i++) {
        if (event_descriptors[i] == ptr) {
            return i;
        }
    }

    fprintf(stderr, "warning: event descriptor not found\n");

    return -1;
}

// for debugging only
void print_event_descriptors()
{
    for (int i = 0; i < EVENT_DESCRIPTORS_COUNT; i++) {
        if (event_descriptors[i] != NULL) {
            fprintf(stderr, "\tevent_descriptor[%i]: 0x%lx\n", i, (unsigned long) event_descriptors[i]);
        }
    }
}

void *get_event_ptr(int i)
{
    if (i < 0 || EVENT_DESCRIPTORS_COUNT <= i) {
        fprintf(stderr, "fatal: event descriptor index out of range: %i\n", i);
        abort();
    }
    return event_descriptors[i];
}

void esp32_sys_queue_init()
{
    event_queue = xQueueCreate(EVENT_QUEUE_LEN, sizeof(uint32_t));
}

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

static void receive_events(GlobalContext *glb, TickType_t wait_ticks)
{
    int event_descriptor;
    if (xQueueReceive(event_queue, &event_descriptor, wait_ticks) == pdTRUE) {
        struct ListHead *listeners_list = glb->listeners;
        EventListener *listeners = GET_LIST_ENTRY(listeners_list, EventListener, listeners_list_head);
        EventListener *last_listener = GET_LIST_ENTRY(listeners_list->prev, EventListener, listeners_list_head);
        EventListener *listener = listeners;

        if (!listener) {
            fprintf(stderr, "warning: no listeners.\n");
            return;
        }

        size_t n = linkedlist_length(glb->listeners);
        for (size_t i = 0;  i < n;  ++i) {
            EventListener *next_listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
            if (listener->fd == event_descriptor) {
                listener->handler(listener);
            }
            listener = next_listener;
        }
    }
}

void sys_waitevents(GlobalContext *glb)
{
    struct ListHead *listeners_list = glb->listeners;
    struct timespec now;
    sys_clock_gettime(&now);

    EventListener *listeners = GET_LIST_ENTRY(listeners_list, EventListener, listeners_list_head);

    int min_timeout = INT_MAX;

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
        listener = GET_LIST_ENTRY(listener->listeners_list_head.next, EventListener, listeners_list_head);
    } while (listener != listeners);

    TickType_t ticks_to_wait = min_timeout / portTICK_PERIOD_MS;

    receive_events(glb, ticks_to_wait);

    listeners = GET_LIST_ENTRY(glb->listeners, EventListener, listeners_list_head);
    EventListener *last_listener = GET_LIST_ENTRY(listeners_list->prev, EventListener, listeners_list_head);

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
        } while (listeners != NULL && listener != listeners);
    }
}

void sys_consume_pending_events(GlobalContext *glb)
{
    receive_events(glb, 0);
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

void sys_start_millis_timer()
{
}

void sys_stop_millis_timer()
{
}

uint32_t sys_millis()
{
    TickType_t ticks = xTaskGetTickCount();
    return ticks * portTICK_PERIOD_MS;
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
    } else if (!strcmp(driver_name, "spi")) {
        spidriver_init(new_ctx, opts);
    } else if (!strcmp(driver_name, "i2c")) {
        i2cdriver_init(new_ctx, opts);
    } else {
        context_destroy(new_ctx);
        return NULL;
    }

    return new_ctx;
}

term sys_get_info(Context *ctx, term key)
{
    if (key == context_make_atom(ctx, esp_free_heap_size_atom)) {
        return term_from_int32(esp_get_free_heap_size());
    }
    if (key == context_make_atom(ctx, esp_chip_info_atom)) {
        esp_chip_info_t info;
        esp_chip_info(&info);
        if (memory_ensure_free(ctx, 5) != MEMORY_GC_OK) {
            return OUT_OF_MEMORY_ATOM;
        }
        term ret = term_alloc_tuple(4, ctx);
        term_put_tuple_element(ret, 0, context_make_atom(ctx, esp32_atom));
        term_put_tuple_element(ret, 1, term_from_int32(info.features));
        term_put_tuple_element(ret, 2, term_from_int32(info.cores));
        term_put_tuple_element(ret, 3, term_from_int32(info.revision));
        return ret;
    }
    if (key == context_make_atom(ctx, esp_idf_version_atom)) {
        const char *str = esp_get_idf_version();
        size_t n = strlen(str);
        if (memory_ensure_free(ctx, 2 * n) != MEMORY_GC_OK) {
            return OUT_OF_MEMORY_ATOM;
        }
        return term_from_string((const uint8_t *) str, n, ctx);
    }
    return UNDEFINED_ATOM;
}
