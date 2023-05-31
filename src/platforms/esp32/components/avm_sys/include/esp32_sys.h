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

#define REGISTER_PORT_DRIVER(NAME, INIT_CB, CREATE_CB)                \
    struct PortDriverDef NAME##_port_driver_def = {                   \
        .port_driver_name = #NAME,                                    \
        .port_driver_init_cb = INIT_CB,                               \
        .port_driver_create_port_cb = CREATE_CB                       \
    };                                                                \
                                                                      \
    struct PortDriverDefListItem NAME##_port_driver_def_list_item = { \
        .def = &NAME##_port_driver_def                                \
    };                                                                \
                                                                      \
    __attribute__((constructor)) void NAME##register_port_driver()    \
    {                                                                 \
        NAME##_port_driver_def_list_item.next = port_driver_list;     \
        port_driver_list = &NAME##_port_driver_def_list_item;         \
    }

#define REGISTER_NIF_COLLECTION(NAME, INIT_CB, RESOLVE_NIF_CB)              \
    struct NifCollectionDef NAME##_nif_collection_def = {                   \
        .nif_collection_init_cb = INIT_CB,                                  \
        .nif_collection_resove_nif_cb = RESOLVE_NIF_CB                      \
    };                                                                      \
                                                                            \
    struct NifCollectionDefListItem NAME##_nif_collection_def_list_item = { \
        .def = &NAME##_nif_collection_def                                   \
    };                                                                      \
                                                                            \
    __attribute__((constructor)) void NAME##register_nif_collection()       \
    {                                                                       \
        NAME##_nif_collection_def_list_item.next = nif_collection_list;     \
        nif_collection_list = &NAME##_nif_collection_def_list_item;         \
    }

#define EVENT_DESCRIPTORS_COUNT 16

typedef struct EventListener EventListener;

typedef EventListener *(*event_handler_t)(GlobalContext *glb, EventListener *listener);

struct EventListener
{
    struct ListHead listeners_list_head;

    event_handler_t handler;
    void *sender;
};

struct ESP32PlatformData
{
    struct SyncList listeners;
    struct SyncList sockets;
    struct ListHead ready_connections;
};

typedef void (*port_driver_init_t)(GlobalContext *global);
typedef Context *(*port_driver_create_port_t)(GlobalContext *global, term opts);

struct PortDriverDef
{
    const char *port_driver_name;
    const port_driver_init_t port_driver_init_cb;
    const port_driver_create_port_t port_driver_create_port_cb;
};

struct PortDriverDefListItem
{
    struct PortDriverDefListItem *next;
    const struct PortDriverDef *const def;
};

typedef void (*nif_collection_init_t)(GlobalContext *global);
typedef const struct Nif *(*nif_collection_resolve_nif_t)(const char *name);

struct NifCollectionDef
{
    const nif_collection_init_t nif_collection_init_cb;
    const nif_collection_resolve_nif_t nif_collection_resove_nif_cb;
};

struct NifCollectionDefListItem
{
    struct NifCollectionDefListItem *next;
    const struct NifCollectionDef *const def;
};

extern struct PortDriverDefListItem *port_driver_list;
extern struct NifCollectionDefListItem *nif_collection_list;

extern QueueSetHandle_t event_set;
extern xQueueHandle event_queue;
void esp32_sys_queue_init();

void sys_event_listener_init(EventListener *listener, void *sender, event_handler_t handler, void *data);

void socket_init(Context *ctx, term opts);

void port_driver_init_all(GlobalContext *global);
void nif_collection_init_all(GlobalContext *global);
const struct Nif *nif_collection_resolve_nif(const char *name);

#endif
