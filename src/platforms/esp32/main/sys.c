/*
 * This file is part of AtomVM.
 *
 * Copyright 2017-2022 Davide Bettio <davide@uninstall.it>
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

#include "sys.h"
#include "esp32_sys.h"

#include "avmpack.h"
#include "component_ports.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "scheduler.h"

#include "trace.h"

#include "esp_event.h"
#include "esp_event_loop.h"
#include "esp_heap_caps.h"
#include "esp_system.h"
#include "freertos/FreeRTOS.h"
#include <limits.h>
#include <stdint.h>
#include <sys/socket.h>

#define EVENT_QUEUE_LEN 16

static const char *const esp_free_heap_size_atom = "\x14" "esp32_free_heap_size";
static const char *const esp_largest_free_block_atom = "\x18" "esp32_largest_free_block";
static const char *const esp_get_minimum_free_size_atom = "\x17" "esp32_minimum_free_size";
static const char *const esp_chip_info_atom = "\xF" "esp32_chip_info";
static const char *const esp_idf_version_atom = "\xF" "esp_idf_version";
static const char *const esp32_atom = "\x5" "esp32";

xQueueHandle event_queue = NULL;
QueueSetHandle_t event_set = NULL;

void esp32_sys_queue_init()
{
    event_set = xQueueCreateSet(EVENT_QUEUE_LEN * 4);
    event_queue = xQueueCreate(EVENT_QUEUE_LEN, sizeof(void *));
    xQueueAddToSet(event_queue, event_set);
}

static inline void sys_clock_gettime(struct timespec *t)
{
    TickType_t ticks = xTaskGetTickCount();
    t->tv_sec = (ticks * portTICK_PERIOD_MS) / 1000;
    t->tv_nsec = ((ticks * portTICK_PERIOD_MS) % 1000) * 1000000;
}

static void receive_events(GlobalContext *glb, TickType_t wait_ticks)
{
    struct ESP32PlatformData *platform = glb->platform_data;

    void *sender = NULL;
    QueueSetMemberHandle_t event_source;
    while ((event_source = xQueueSelectFromSet(event_set, wait_ticks))) {
        if (UNLIKELY(list_is_empty(&platform->listeners))) {
            fprintf(stderr, "warning: no listeners.\n");
            return;
        }

        if (event_source == event_queue) {
            if (xQueueReceive(event_queue, &sender, wait_ticks) == pdFALSE) {
                continue;
            }
        } else {
            sender = event_source;
        }

        struct ListHead *listener_lh;
        LIST_FOR_EACH (listener_lh, &platform->listeners) {
            EventListener *listener = GET_LIST_ENTRY(listener_lh, EventListener, listeners_list_head);
            if (listener->sender == sender) {
                TRACE("sys: handler found for: %p\n", (void *) sender);
                listener->handler(listener);
                TRACE("sys: handler executed\n");
                return;
            }
        }

        TRACE("sys: handler not found for: %p\n", (void *) sender);
    }
}

void sys_consume_pending_events(GlobalContext *glb)
{
    // delay 1 tick in order to allow this task to respond to the IDF task watchdog timer
    vTaskDelay(1);
    receive_events(glb, 0);
}

void sys_event_listener_init(EventListener *listener, void *sender, event_handler_t handler, void *data)
{
    list_init(&listener->listeners_list_head);
    listener->sender = sender;
    listener->handler = handler;
    listener->data = data;
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

void sys_init_platform(GlobalContext *glb)
{
    struct ESP32PlatformData *platform = malloc(sizeof(struct ESP32PlatformData));
    list_init(&platform->listeners);
    glb->platform_data = platform;
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

    struct ListHead *item;
    LIST_FOR_EACH (item, &global->avmpack_data) {
        struct AVMPackData *avmpack_data = (struct AVMPackData *) item;
        if (avmpack_find_section_by_name(avmpack_data->data, module_name, &beam_module, &beam_module_size)) {
            break;
        }
    }

    if (IS_NULL_PTR(beam_module)) {
        fprintf(stderr, "Failed to open module: %s\n", module_name);
        return NULL;
    }

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    new_module->module_platform_data = NULL;

    return new_module;
}

// This function allows to use AtomVM as a component on ESP32 and customize it
__attribute__((weak)) Context *sys_create_port_fallback(const char *driver_name, GlobalContext *global, term opts)
{
    UNUSED(global);
    UNUSED(opts);

    fprintf(stderr, "Failed to load port \"%s\".  Ensure the port is configured properly in the build.\n", driver_name);

    return NULL;
}

Context *sys_create_port(GlobalContext *glb, const char *port_name, term opts)
{
    Context *new_ctx = component_ports_create_port(port_name, glb, opts);
    if (IS_NULL_PTR(new_ctx)) {
        return sys_create_port_fallback(port_name, glb, opts);
    }
    return new_ctx;
}

term sys_get_info(Context *ctx, term key)
{
    if (key == context_make_atom(ctx, esp_free_heap_size_atom)) {
        return term_from_int32(esp_get_free_heap_size());
    }
    if (key == context_make_atom(ctx, esp_largest_free_block_atom)) {
        return term_from_int32(heap_caps_get_largest_free_block(MALLOC_CAP_DEFAULT));
    }
    if (key == context_make_atom(ctx, esp_get_minimum_free_size_atom)) {
        return term_from_int32(esp_get_minimum_free_heap_size());
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

void sys_sleep(GlobalContext *glb)
{
    UNUSED(glb);

    vTaskDelay(1);
}
