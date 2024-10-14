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

#include "esp_pthread.h"
#include "freertos/FreeRTOS.h"
#include <esp_partition.h>
#include <freertos/queue.h>

#if ESP_IDF_VERSION_MAJOR >= 5
#include <spi_flash_mmap.h>
#endif

#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>

#include <sys/poll.h>
#include <stdbool.h>
#include <time.h>

#ifndef AVM_NO_SMP
#include "smp.h"
#endif

#include "sys.h"
#include "portnifloader.h"

#define EVENT_DESCRIPTORS_COUNT 16

typedef void *listener_event_t;

struct EventListener
{
    struct ListHead listeners_list_head;
    event_handler_t handler;
    listener_event_t sender;
};

struct ESP32PlatformData
{
    pthread_t select_thread;
    bool select_thread_exit;
    bool eventfd_registered;
    int signal_fd;
    int ATOMIC select_events_poll_count;
    struct pollfd *fds;

    // socket_driver
    EventListener *socket_listener;
    struct SyncList sockets;
    struct ListHead ready_connections;

#ifndef AVM_NO_SMP
    Mutex *entropy_mutex;
#endif
    mbedtls_entropy_context entropy_ctx;
    bool entropy_is_initialized;

#ifndef AVM_NO_SMP
    Mutex *random_mutex;
#endif
    mbedtls_ctr_drbg_context random_ctx;
    bool random_is_initialized;

#ifdef CONFIG_AVM_ENABLE_STORAGE_NIFS
    ErlNifResourceType *mounted_fs_resource_type;
#endif
};

extern QueueSetHandle_t event_set;
extern QueueHandle_t event_queue;
void esp32_sys_queue_init();

void socket_init(Context *ctx, term opts);

term esp_err_to_term(GlobalContext *glb, esp_err_t status);

const void *esp32_sys_mmap_partition(
    const char *partition_name, spi_flash_mmap_handle_t *handle, int *size);

#endif
