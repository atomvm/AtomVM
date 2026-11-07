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
#include <rp2_sys.h>

struct OTPSocketNIFGlobalData
{
    GlobalContext *global;
    queue_t queue;
};

static struct OTPSocketNIFGlobalData OTPSocketGlobalData;

void otp_socket_lwip_poll(void)
{
    struct LWIPEvent event;
    while (queue_try_remove(&OTPSocketGlobalData.queue, &event)) {
        event.handler(&event);
    }
    otp_socket_lwip_process_pending();
}

bool otp_socket_lwip_has_events(void)
{
    return !queue_is_empty(&OTPSocketGlobalData.queue) || otp_socket_lwip_has_pending();
}

void otp_socket_lwip_signal(void)
{
#ifndef AVM_NO_SMP
    sys_signal(OTPSocketGlobalData.global);
#endif
}

static void otp_socket_nif_init(GlobalContext *global)
{
    OTPSocketGlobalData.global = global;
    queue_init(&OTPSocketGlobalData.queue, sizeof(struct LWIPEvent), EVENT_QUEUE_LEN);
}

static void otp_socket_nif_destroy(GlobalContext *global)
{
    UNUSED(global);
    do {
        otp_socket_lwip_poll();
    } while (otp_socket_lwip_has_events());
    // Producers are still active until sys_free_platform deinitializes CYW43.
    // Keep the queue alive through that boundary, as with listener queues.
}

bool otp_socket_lwip_enqueue(struct LWIPEvent *event)
{
    if (!queue_try_add(&OTPSocketGlobalData.queue, event)) {
        return false;
    }
    otp_socket_lwip_signal();
    return true;
}

REGISTER_NIF_COLLECTION(otp_socket, otp_socket_nif_init, otp_socket_nif_destroy, otp_socket_nif_get_nif)

#endif
