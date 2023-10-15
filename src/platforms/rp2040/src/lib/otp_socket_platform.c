/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 by Fred Dushin <fred@dushin.net>
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

#ifdef LIB_PICO_CYW43_ARCH

#include <nifs.h>
#include <otp_socket.h>
#include <rp2040_sys.h>

struct OTPSocketNIFGlobalData
{
    GlobalContext *global;
    queue_t queue;
};

static struct OTPSocketNIFGlobalData OTPSocketGlobalData;

static EventListener *otp_socket_event_handler(GlobalContext *glb, EventListener *listener)
{
    UNUSED(glb);

    struct LWIPEvent event;
    while (queue_try_remove(listener->queue, &event)) {
        event.handler(&event);
    }
    return listener;
}

static void otp_socket_nif_init(GlobalContext *global)
{
    OTPSocketGlobalData.global = global;
    queue_init(&OTPSocketGlobalData.queue, sizeof(struct LWIPEvent), EVENT_QUEUE_LEN);

    EventListener *network_listener = malloc(sizeof(EventListener));

    network_listener->handler = otp_socket_event_handler;
    network_listener->queue = &OTPSocketGlobalData.queue;
    sys_register_listener(global, network_listener);
}

static void otp_socket_nif_destroy(GlobalContext *global)
{
    sys_unregister_listener_from_event(global, &OTPSocketGlobalData.queue);
}

void otp_socket_lwip_enqueue(struct LWIPEvent *event)
{
    sys_try_post_listener_event_from_isr(OTPSocketGlobalData.global, &OTPSocketGlobalData.queue, event);
}

REGISTER_NIF_COLLECTION(otp_socket, otp_socket_nif_init, otp_socket_nif_destroy, otp_socket_nif_get_nif)

#endif
