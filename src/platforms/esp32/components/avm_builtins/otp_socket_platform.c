/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 by Fred Dushin <fred@dushin.net>
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

#include <otp_socket_platform.h>

#include <context.h>
#include <esp_log.h>
#include <freertos/FreeRTOS.h>
#include <freertos/queue.h>
#include <freertos/task.h>
#include <globalcontext.h>
#include <list.h>
#include <sys/socket.h>

// #define ENABLE_TRACE
#include <trace.h>

#define SELECT_QUEUE_SIZE 32
#define SELECT_TASK_STACK 5000
#define SELECT_TASK_SLEEP_MS 50
#define SELECT_TASK_TIMEOUT_MS 0

static xQueueHandle select_queue;

enum SelectTaskOperation
{
    op_ADD,
    op_REMOVE
};

struct SelectTaskMessage
{
    enum SelectTaskOperation op;
    Context *ctx;
    int fd;
};

struct FDListElement
{
    struct ListHead head;
    Context *ctx;
    int fd;
};

//
// select RTOS task
//

static void update_fds(struct ListHead *fds, struct SelectTaskMessage *msg)
{
    switch (msg->op) {
        case op_ADD: {
            TRACE("Got op_ADD message for 0x%p,%i\n", msg->ctx, msg->fd);
            struct FDListElement *elt = malloc(sizeof(struct FDListElement));
            elt->ctx = msg->ctx;
            elt->fd = msg->fd;
            list_append(fds, (struct ListHead *) elt);
            break;
        }
        case op_REMOVE: {
            struct ListHead *item, *tmp;
            TRACE("Got op_REMOVE message for 0x%p,%i\n", msg->ctx, msg->fd);
            MUTABLE_LIST_FOR_EACH (item, tmp, fds) {
                struct FDListElement *elt = (struct FDListElement *) item;
                if (elt->fd == msg->fd) {
                    TRACE("Removing fd=%i\n", elt->fd);
                    list_remove(item);
                    free(item);
                }
            }
            break;
        }
        default:
            break;
    }
}

static void maybe_select(struct ListHead *fds)
{
    if (!list_is_empty(fds)) {

        fd_set read_fds;
        FD_ZERO(&read_fds);

        struct ListHead *item;
        int fd_max = 0;
        LIST_FOR_EACH (item, fds) {
            struct FDListElement *elt = (struct FDListElement *) item;
            FD_SET(elt->fd, &read_fds);
            if (elt->fd > fd_max) {
                fd_max = elt->fd;
            }
        }

        struct timeval tv;
        tv.tv_sec = 0;
        tv.tv_usec = SELECT_TASK_TIMEOUT_MS * 1000;
        int res = select(fd_max + 1, &read_fds, NULL, NULL, &tv);

        if (res == -1) {
            ESP_LOGW(TAG, "Error calling select.");
        } else if (res == 0) {
            // No socket fds are ready
        } else {
            LIST_FOR_EACH (item, fds) {
                struct FDListElement *elt = (struct FDListElement *) item;
                if (FD_ISSET(elt->fd, &read_fds)) {
                    TRACE("socket 0x%p,%i ready for read\n", elt->ctx, elt->fd);
                    BaseType_t err = xQueueSendToBack(event_queue, &elt->ctx, 0);
                    if (err != pdPASS) {
                        ESP_LOGW(TAG, "Unable to push context onto event queue.  err=%i", err);
                    } else {
                        TRACE("Pushed context 0x%p,%i onto event_queue\n", elt->ctx, elt->fd);
                    }
                }
            }
        }
    }
}

void select_task(void *args)
{
    TRACE("Entered select_task\n");
    xQueueHandle select_queue = (xQueueHandle) args;

    struct ListHead fds;
    list_init(&fds);

    for (;;) {
        struct SelectTaskMessage msg;
        BaseType_t res = xQueueReceive(select_queue, &msg, 0);
        if (res == pdPASS) {
            update_fds(&fds, &msg);
#ifdef ENABLE_TRACE
            TRACE("[");
            struct ListHead *item;
            LIST_FOR_EACH (item, &fds) {
                struct FDListElement *elt = (struct FDListElement *) item;
                TRACE("%i,", elt->fd);
            }
            TRACE("]\n");
#endif
        } else if (res == errQUEUE_EMPTY) {
            // queue is empty
        }

        maybe_select(&fds);

        vTaskDelay(SELECT_TASK_SLEEP_MS / portTICK_PERIOD_MS);
    }
    vTaskDelete(NULL);
}

//
// entrypoints
//

void otp_socket_init(GlobalContext *global)
{
    select_queue = xQueueCreate(SELECT_QUEUE_SIZE, sizeof(struct SelectTaskMessage));
    if (IS_NULL_PTR(select_queue)) {
        ESP_LOGE(TAG, "Failed to initialize select_queue.");
        AVM_ABORT();
    } else {
        BaseType_t res = xTaskCreate(select_task, "select_task", SELECT_TASK_STACK, select_queue, 1, NULL);
        if (res != pdPASS) {
            ESP_LOGE(TAG, "Failed to initialize select_task.  Error: %i", res);
            vQueueDelete(select_queue);
            AVM_ABORT();
        }
    }
    ESP_LOGI(TAG, "Initialized AtomVM socket.");
}

//
// platform-specific function implementations
//

void otp_socket_platform_notify_add(Context *ctx, int fd)
{
    struct SelectTaskMessage msg = {
        .op = op_ADD,
        .fd = fd,
        .ctx = ctx
    };
    TRACE("Pushing context 0x%p,fd=%i onto select_queue\n", ctx, fd);
    BaseType_t res = xQueueSendToBack(select_queue, &msg, 0);
    if (res != pdPASS) {
        ESP_LOGW(TAG, "Unable to push fd %i to select queue.", fd);
    }
}

void otp_socket_platform_notify_remove(Context *ctx, int fd)
{
    struct SelectTaskMessage select_msg = {
        .op = op_REMOVE,
        .fd = fd,
        .ctx = ctx
    };
    TRACE("Pushing REMOVE msg for 0x%p,%i onto select_queue\n", ctx, socket_data->fd);
    BaseType_t res = xQueueSendToBack(select_queue, &select_msg, 0);
    if (res != pdPASS) {
        ESP_LOGW(TAG, "Unable to push remove to select_queue.");
    }
}

struct ListHead *otp_socket_platform_get_listeners(void *platform_data)
{
    return &((struct ESP32PlatformData *) platform_data)->listeners;
}

void otp_socket_platform_set_listener(EventListener *listener, void *sender, int fd)
{
    listener->sender = sender;
}

bool otp_socket_platform_supports_peek()
{
    return true;
}

#include <sdkconfig.h>
#ifdef CONFIG_AVM_ENABLE_OTP_SOCKET_PORT_DRIVER

Context *otp_socket_create_port(GlobalContext *global, term opts);

REGISTER_PORT_DRIVER(otp_socket, otp_socket_init, otp_socket_create_port)
#endif
