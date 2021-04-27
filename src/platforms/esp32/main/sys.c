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
#include "defaultatoms.h"
#include "globalcontext.h"
#include "gpio_driver.h"
#include "i2cdriver.h"
#include "network.h"
#include "scheduler.h"
#include "spidriver.h"
#include "uart_driver.h"

#include "trace.h"

#include "esp_event.h"
#include "esp_event_loop.h"
#include "esp_system.h"
#include "freertos/FreeRTOS.h"
#include <limits.h>
#include <sys/socket.h>
#include <stdint.h>

#define EVENT_QUEUE_LEN 16

static const char *const esp_free_heap_size_atom = "\x14" "esp32_free_heap_size";
static const char *const esp_chip_info_atom = "\xF" "esp32_chip_info";
static const char *const esp_idf_version_atom = "\xF" "esp_idf_version";
static const char *const esp32_atom = "\x5" "esp32";

xQueueHandle event_queue = NULL;

void esp32_sys_queue_init()
{
    event_queue = xQueueCreate(EVENT_QUEUE_LEN, sizeof(void *));
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
    while (xQueueReceive(event_queue, &sender, wait_ticks) == pdTRUE) {

        if (UNLIKELY(list_is_empty(&platform->listeners))) {
            fprintf(stderr, "warning: no listeners.\n");
            return;
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
__attribute__((weak)) Context *sys_create_port_fallback(Context *new_ctx, const char *driver_name, term opts)
{
    UNUSED(driver_name);
    UNUSED(opts);

    context_destroy(new_ctx);
    return NULL;
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
    } else if (!strcmp(driver_name, "uart")) {
        uart_driver_init(new_ctx, opts);
    } else {
        return sys_create_port_fallback(new_ctx, driver_name, opts);
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

void sys_sleep(GlobalContext *glb)
{
    UNUSED(glb);

    vTaskDelay(1);
}
