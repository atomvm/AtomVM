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

enum SocketErrors
{
    SocketClosed = 0,
    SocketWouldBlock = -1,
    SocketOtherError = -2
};

struct SocketResource;

const struct Nif *otp_socket_nif_get_nif(const char *nifname);
void otp_socket_init(GlobalContext *global);

/**
 * @brief Get the resource object associated with a socket term.
 *
 * @param socket_term   the term with the socket
 * @param otp_socket    on output, the socket resource
 * @return true in case of success
 */
bool term_to_otp_socket(term socket_term, struct SocketResource **otp_socket, Context *ctx);

/**
 * @brief Determine if a term is a socket term.
 *
 * @param socket_term   the term to test
 * @return true if it is a term
 */
bool term_is_otp_socket(term socket_term);

/**
 * @brief Send data to a socket (without blocking)
 *
 * @param otp_socket    the socket resource
 * @param buf           buffer to send
 * @param len           number of bytes
 * @param dest          destination address or invalid term for sendto/send
 * @return the number of written bytes or a value from SocketErrors
 */
ssize_t socket_send(struct SocketResource *socket, const uint8_t *buf, size_t len, term dest);

/**
 * @brief Read data from a socket.
 *
 * @param otp_socket    the socket resource
 * @param buf           buffer to store data
 * @param len           number of bytes
 * @param flags         flags passed to recvfrom
 * @param from          filled with origin address using recvfrom (can be NULL)
 * @param heap          heap to build the origin address term (can be NULL if from is NULL)
 * @return the number of read bytes or a value from SocketErrors
 */
ssize_t socket_recv(struct SocketResource *socket, uint8_t *buf, size_t len, int flags, term *from, Heap *heap);

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
        } tcp_accept;
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
        // Used by otp_net
        struct
        {
            void *callback_arg;
            bool success;
            uint32_t addr;
        } dns_gethostbyname;
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
