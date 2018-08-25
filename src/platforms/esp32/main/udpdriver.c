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

#include <string.h>

#include "atom.h"
#include "context.h"
#include "debug.h"
#include "globalcontext.h"
#include "mailbox.h"
#include "utils.h"
#include "term.h"

#include <esp_log.h>

#include <lwip/inet.h>

#include <sys/socket.h>

#ifndef TRACE
    #ifdef ENABLE_TRACE
        #define TRACE printf
    #else
        #define TRACE(...)
    #endif
#endif

void consume_udpdriver_mailbox(Context *ctx);
static uint32_t tuple_to_addr(term addr_tuple);

static const char *const ok_a = "\x2" "ok";
static const char *const error_a = "\x5" "error";

static const char *const send_a = "\x4" "send";

static inline term term_from_atom_string(GlobalContext *glb, AtomString string)
{
    int global_atom_index = globalcontext_insert_atom(glb, string);
    return term_from_atom_index(global_atom_index);
}

static char *list_to_string(term list)
{
    int len = 0;

    term t = list;

    while (!term_is_nil(t)) {
        len++;
        term *t_ptr = term_get_list_ptr(t);
        t = *t_ptr;
    }

    t = list;
    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }

    for (int i = 0; i < len; i++) {
        term *t_ptr = term_get_list_ptr(t);
        str[i] = (char) term_to_int32(t_ptr[1]);
        t = *t_ptr;
    }
    str[len] = 0;

    return str;
}

void consume_udpdriver_mailbox(Context *ctx)
{
    GlobalContext *glb = ctx->global;

    Message *message = mailbox_dequeue(ctx);

    term ret;

    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);

    if (cmd == term_from_atom_string(glb, send_a)) {
        term ipaddr_term = term_get_tuple_element(msg, 3);
        term dest_port_term = term_get_tuple_element(msg, 4);
        term buffer_term = term_get_tuple_element(msg, 5);

        int sockfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);

        struct sockaddr_in addr;
        memset(&addr, 0, sizeof(struct sockaddr_in));
        addr.sin_family = AF_INET;
        addr.sin_addr.s_addr = htonl(tuple_to_addr(ipaddr_term));
        addr.sin_port = htons(term_to_int32(dest_port_term));

        char *buffer = list_to_string(buffer_term);
        int buf_len = strlen(buffer);

        TRACE("send: data with len: %i, to: %s, port: %i\n", buf_len, inet_ntoa(addr.sin_addr.s_addr), term_to_int32(dest_port_term));

        int sent_data = sendto(sockfd, buffer, buf_len, 0, (struct sockaddr *) &addr, sizeof(addr));

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
