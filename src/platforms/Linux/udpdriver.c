/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
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

#include "udpdriver.h"

#include <string.h>

#include "atom.h"
#include "context.h"
#include "debug.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "utils.h"
#include "term.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/udp.h>

#include "trace.h"

struct UDPDriverData
{
    int sockfd;
};

static void consume_udpdriver_mailbox(Context *ctx);
static uint32_t tuple_to_addr(term addr_tuple);

static const char *const ok_a = "\x2" "ok";
static const char *const error_a = "\x5" "error";

static const char *const send_a = "\x4" "send";

static inline term term_from_atom_string(GlobalContext *glb, AtomString string)
{
    int global_atom_index = globalcontext_insert_atom(glb, string);
    return term_from_atom_index(global_atom_index);
}

void udpdriver_init(Context *ctx)
{
    struct UDPDriverData *udp_driver_context = calloc(1, sizeof(struct UDPDriverData));

    ctx->native_handler = consume_udpdriver_mailbox;
    ctx->platform_data = udp_driver_context;

    udp_driver_context->sockfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
}

static void consume_udpdriver_mailbox(Context *ctx)
{
    if (UNLIKELY(ctx->native_handler != consume_udpdriver_mailbox)) {
        abort();
    }

    struct UDPDriverData *udp_driver_context = (struct UDPDriverData *) ctx->platform_data;

    GlobalContext *glb = ctx->global;

    Message *message = mailbox_dequeue(ctx);

    term ret;

    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);

    UNUSED(ref);

    if (cmd == term_from_atom_string(glb, send_a)) {
        term ipaddr_term = term_get_tuple_element(msg, 3);
        term dest_port_term = term_get_tuple_element(msg, 4);
        term buffer_term = term_get_tuple_element(msg, 5);

        struct sockaddr_in addr;
        memset(&addr, 0, sizeof(struct sockaddr_in));
        addr.sin_family = AF_INET;
        addr.sin_addr.s_addr = htonl(tuple_to_addr(ipaddr_term));
        addr.sin_port = htons(term_to_int32(dest_port_term));

        char *buffer = interop_list_to_string(buffer_term);
        int buf_len = strlen(buffer);

        TRACE("send: data with len: %i, to: %s, port: %i\n", buf_len, inet_ntoa(addr.sin_addr.s_addr), term_to_int32(dest_port_term));

        int sent_data = sendto(udp_driver_context->sockfd, buffer, buf_len, 0, (struct sockaddr *) &addr, sizeof(addr));
        UNUSED(sent_data);

        ret = term_from_atom_string(glb, ok_a);

    } else {
        TRACE("udpdriver: unrecognized command\n");
        ret = term_from_atom_string(glb, error_a);
    }

    free(message);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);
    mailbox_send(target, ret);
}

static uint32_t tuple_to_addr(term addr_tuple)
{
    return ((term_to_int32(term_get_tuple_element(addr_tuple, 0)) & 0xFF) << 24)
            | ((term_to_int32(term_get_tuple_element(addr_tuple, 1)) & 0xFF) << 16)
            | ((term_to_int32(term_get_tuple_element(addr_tuple, 2)) & 0xFF) << 8)
            | (term_to_int32(term_get_tuple_element(addr_tuple, 3)) & 0xFF);
}
