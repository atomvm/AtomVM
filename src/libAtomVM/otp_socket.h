/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Fred Dushin <fred@dushin.net>
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

#ifndef _OTP_SOCKET_H_
#define _OTP_SOCKET_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <globalcontext.h>
#include <nifs.h>
#include <otp_socket_platform.h>

#if !defined(OTP_SOCKET_BSD) && !defined(OTP_SOCKET_LWIP)
#if HAVE_SOCKET && HAVE_SELECT
#define OTP_SOCKET_BSD 1
#elif HAVE_LWIP_RAW
#define OTP_SOCKET_LWIP 1
#else
#error OTP Socket requires BSD Socket or lwIP
#endif
#endif

const struct Nif *otp_socket_nif_get_nif(const char *nifname);
void otp_socket_init(GlobalContext *global);

#if OTP_SOCKET_LWIP
struct LWIPEvent
{
    void (*handler)(struct LWIPEvent *event);
    union
    {
        struct
        {
            struct SocketResource *rsrc_obj;
            struct tcp_pcb *newpcb;
        } accept_make_resource;
        struct
        {
            struct tcp_pcb *tpcb;
            struct pbuf *buf;
            err_t err;
        } tcp_recv;
        struct
        {
            struct SocketResource *rsrc_obj;
            struct pbuf *buf;
            uint32_t addr;
            uint16_t port;
        } udp_recv;
        struct
        {
            GlobalContext *global;
            int32_t target_pid;
        } trap_answer_ok;
        struct
        {
            GlobalContext *global;
            int32_t target_pid;
        } trap_answer_closed;
        struct
        {
            struct SocketResource *rsrc_obj;
        } finalize_close;
    };
};

/**
 * @brief Enqueue an event to be processed in task context
 * @param event the event to enqueue
 * @details This function must be implemented in otp_socket_platform.c.
 * Platforms using lwIP implementation may have lwIP callbacks coming
 * from ISR and in such case need to implement a queue mechanism to process
 * them in task context. GlobalContext is not always available in lwIP callbacks
 * so platforms using a queue need a global variable.
 * If lwIP callbacks are not called from ISR, calling handler with the event is
 * sufficient.
 * @end
 */
void otp_socket_lwip_enqueue(struct LWIPEvent *event);

#endif

#ifdef __cplusplus
}
#endif

#endif
