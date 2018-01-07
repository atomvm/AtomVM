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

#include <string.h>

#include "driver/gpio.h"

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "debug.h"
#include "globalcontext.h"
#include "mailbox.h"
#include "module.h"
#include "utils.h"
#include "term.h"

#ifndef TRACE
    #ifdef ENABLE_TRACE
        #define TRACE printf
    #else
        #define TRACE(...)
    #endif
#endif

static void consume_gpio_mailbox(Context *ctx);

native_handler platform_open_port(const char *driver_name)
{
    if (!strcmp(driver_name, "gpio")) {
        return consume_gpio_mailbox;
    } else {
        return NULL;
    }
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

    for (int i = 0; i < len; i++) {
        term *t_ptr = term_get_list_ptr(t);
        str[i] = (char) term_to_int32(t_ptr[1]);
        t = *t_ptr;
    }
    str[len] = 0;

    return str;
}

static void consume_gpio_mailbox(Context *ctx)
{
    term ret;

    term msg = mailbox_receive(ctx);
    term pid = term_get_tuple_element(msg, 0);
    term cmd = term_get_tuple_element(msg, 1);

    char *str = list_to_string(cmd);
    if (!strcmp(str, "set_level")) {
        int32_t gpio_num = term_to_int32(term_get_tuple_element(msg, 2));
        int32_t level = term_to_int32(term_get_tuple_element(msg, 3));
        gpio_set_level(gpio_num, level != 0);
        TRACE("gpio: set_level: %i %i\n", gpio_num, level != 0);
        ret = term_nil(); //TODO ok here
    } else if (!strcmp(str, "set_direction")) {
        int32_t gpio_num = term_to_int32(term_get_tuple_element(msg, 2));
        char *direction = list_to_string(term_get_tuple_element(msg, 3));
        if (!strcmp("input", direction)) {
            gpio_set_direction(gpio_num, GPIO_MODE_INPUT);
            TRACE("gpio: set_direction: %i INPUT\n", gpio_num);
            ret = term_nil(); //TODO ok here
        } else if (!strcmp("output", direction))  {
            gpio_set_direction(gpio_num, GPIO_MODE_OUTPUT);
            TRACE("gpio: set_direction: %i OUTPUT\n", gpio_num);
            ret = term_nil(); //TODO ok here
        } else {
            TRACE("gpio: %s is not a direction\n", direction);
            ret = term_nil(); //TODO error here
        }
        free(direction);
    } else {
        TRACE("gpio: %s is not a call\n", str);
        ret = term_nil(); //TODO error here
    }
    free(str);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);
    mailbox_send(target, ret);
}
